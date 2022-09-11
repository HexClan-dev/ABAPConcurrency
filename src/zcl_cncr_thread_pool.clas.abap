CLASS zcl_cncr_thread_pool DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ENUM thread_status,
             running,
             waiting,
             stopped,
             failed,
           END OF ENUM thread_status.

    TYPES: BEGIN OF lty_s_server_grp,
             thread_group TYPE rzllitab-classname,
             applserver   TYPE rzllitab-applserver,
             free_pbt_wps TYPE i,
             max_pbt_wps  TYPE i,
           END OF lty_s_server_grp.

    TYPES: BEGIN OF lty_s_runnable,
             que_nr       TYPE i,
             thread_name  TYPE zif_cncr_runnable=>ty_char32,
             thread_group TYPE rzllitab-classname,
             runnable     TYPE REF TO zif_cncr_runnable,
             status       TYPE thread_status,
             messages     TYPE bapiret2_t,
           END OF lty_s_runnable.

    TYPES: lty_t_runnable TYPE SORTED TABLE OF lty_s_runnable WITH UNIQUE KEY que_nr.

    METHODS: constructor
      IMPORTING
        iv_max_processes TYPE i OPTIONAL.

    METHODS add_thread
      IMPORTING
        iv_thread_name TYPE zif_cncr_runnable=>ty_char32 OPTIONAL
        io_runnable    TYPE REF TO zif_cncr_runnable.

    METHODS execute.

    METHODS at_end_process FOR EVENT at_end_thread OF zcl_cncr_async_task
      IMPORTING
        eo_runnable
        et_bapiret.

    METHODS at_end_process_failed FOR EVENT at_end_thread_failed OF zcl_cncr_async_task
      IMPORTING
        et_bapiret.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS: get_server_group
      IMPORTING
                iv_max_processes       TYPE i OPTIONAL
      RETURNING VALUE(rs_server_group) TYPE lty_s_server_grp.

    DATA: ms_server_group  TYPE lty_s_server_grp.
    DATA: mt_runnable      TYPE lty_t_runnable.
    DATA: mv_thread_count  TYPE i VALUE 0.
    DATA: mv_max_thread    TYPE i VALUE 0.


ENDCLASS.



CLASS zcl_cncr_thread_pool IMPLEMENTATION.

  METHOD constructor.
    " Get best server group available
    me->ms_server_group = me->get_server_group( iv_max_processes ).
    me->mv_max_thread   = iv_max_processes.

  ENDMETHOD.

  METHOD get_server_group.

    DATA: lt_rzllitab      TYPE TABLE OF rzllitab.
    DATA: ls_srv_group     TYPE lty_s_server_grp.

    SELECT * FROM rzllitab
        WHERE grouptype = 'S'
        INTO CORRESPONDING FIELDS OF TABLE @lt_rzllitab.

    LOOP AT lt_rzllitab ASSIGNING FIELD-SYMBOL(<ls_rzllitab>).
      CLEAR: ls_srv_group.

      CALL FUNCTION 'SPBT_INITIALIZE'
        EXPORTING
          group_name         = <ls_rzllitab>-classname
        IMPORTING
          max_pbt_wps        = ls_srv_group-max_pbt_wps
          free_pbt_wps       = ls_srv_group-free_pbt_wps
        EXCEPTIONS
          invalid_group_name = 1
          OTHERS             = 7.

      IF sy-subrc = 0.
        ls_srv_group-applserver = <ls_rzllitab>-applserver.
        ls_srv_group-thread_group  = <ls_rzllitab>-classname.

        IF ls_srv_group-free_pbt_wps >= rs_server_group-free_pbt_wps AND
           ( ls_srv_group-max_pbt_wps >= iv_max_processes OR
             ls_srv_group-free_pbt_wps >= iv_max_processes OR
             iv_max_processes IS INITIAL ).
          rs_server_group = ls_srv_group.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " If you are requesting more parallel tasks than possible
    " then the minimum is assigned
    IF rs_server_group IS INITIAL.
      rs_server_group = ls_srv_group.
    ENDIF.

  ENDMETHOD.

  METHOD add_thread.

    DATA: ls_runnable LIKE LINE OF me->mt_runnable.
    TRY.
        DATA(lv_task_id) = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH  cx_uuid_error.
        " If this fails use some radim
        lv_task_id = 'TASK' && me->mv_thread_count.
    ENDTRY.

    IF iv_thread_name IS SUPPLIED.
      ls_runnable-thread_name = iv_thread_name.
    ELSE.
      ls_runnable-thread_name = lv_task_id.
    ENDIF.

    ls_runnable-thread_group = me->ms_server_group-thread_group.
    ls_runnable-que_nr = me->mv_thread_count.
    ls_runnable-runnable = io_runnable.
    ls_runnable-status   = waiting.
    APPEND ls_runnable TO me->mt_runnable.

    me->mv_thread_count = me->mv_thread_count + 1.
  ENDMETHOD.

  METHOD execute.

    LOOP AT me->mt_runnable
        ASSIGNING FIELD-SYMBOL(<ls_runnable>).


      IF me->mv_thread_count >= me->ms_server_group-free_pbt_wps OR
         me->mv_thread_count >= me->mv_max_thread.
        WAIT UNTIL me->mv_thread_count < me->mv_max_thread OR
                   me->mv_thread_count < me->ms_server_group-free_pbt_wps.
        CONTINUE.
      ENDIF.

      TRY.
          DATA(lo_runnable) = NEW zcl_cncr_async_task(
              io_runnable = <ls_runnable>-runnable
          ).

          " Register the Event handler
          SET HANDLER me->at_end_process        FOR lo_runnable.
          SET HANDLER me->at_end_process_failed FOR lo_runnable.


          DATA(lv_is_running) = lo_runnable->run( iv_task_name = <ls_runnable>-thread_name iv_group = <ls_runnable>-thread_group ).

          IF lv_is_running = abap_true.
            <ls_runnable>-status = running.
          ELSE.
            <ls_runnable>-status = failed.
          ENDIF.

        CATCH cx_uuid_error INTO DATA(lx_uuid_error).
          APPEND VALUE #( message = lx_uuid_error->err_text type = 'E' ) TO <ls_runnable>-messages.

        CATCH zcx_cncr_exception INTO DATA(lx_error).
          APPEND lx_error->get_bapireturn( ) TO <ls_runnable>-messages.

      ENDTRY.
    ENDLOOP.


  ENDMETHOD.


  METHOD at_end_process.
    " This is ending the process
*    me->mv_is_running = abap_false.
*
*    APPEND LINES OF et_bapiret TO me->mt_messages.
*
*    " Measure the execution time of the execution
*    me->end_execution_time( ).
*
*    IF eo_runnable IS NOT INITIAL.
*      " Update the Runnable instance
*      me->mo_runnable = eo_runnable.
*    ENDIF.
  ENDMETHOD.

  METHOD at_end_process_failed.
*    " This is ending the process
*    me->mv_is_running = abap_false.
*
*    APPEND LINES OF et_bapiret TO me->mt_messages.
*
*    " Measure the execution time of the execution
*    me->end_execution_time( ).
  ENDMETHOD.

ENDCLASS.

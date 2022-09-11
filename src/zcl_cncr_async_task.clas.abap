CLASS zcl_cncr_async_task DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        io_runnable TYPE REF TO zif_cncr_runnable.

    METHODS at_end_process
      IMPORTING !p_task        TYPE clike
      RETURNING VALUE(ro_self) TYPE REF TO zif_cncr_runnable
      RAISING   zcx_cncr_exception.

    METHODS: run
      IMPORTING
                iv_task_name         TYPE zif_cncr_runnable=>ty_char32 DEFAULT 'TASK1'
                iv_group             TYPE rzllitab-classname OPTIONAL
      RETURNING VALUE(rv_is_running) TYPE abap_bool
      RAISING   zcx_cncr_exception.

    EVENTS: at_end_thread EXPORTING
        VALUE(et_bapiret)  TYPE        bapiret2_t          OPTIONAL
        VALUE(eo_runnable) TYPE REF TO zif_cncr_runnable OPTIONAL.

    EVENTS: at_end_thread_failed EXPORTING
        VALUE(et_bapiret)  TYPE        bapiret2_t          OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS get_process_info
      IMPORTING
                iv_group               TYPE rzllitab-classname
      EXPORTING
                VALUE(ev_max_pbt_wps)  TYPE i
                VALUE(ev_free_pbt_wps) TYPE i
      RAISING   zcx_cncr_exception.

    METHODS run_rfc_with_group
      IMPORTING
                io_serialized        TYPE string
                iv_task_name         TYPE zif_cncr_runnable=>ty_char32 DEFAULT 'TASK1'
                iv_group             TYPE rzllitab-classname DEFAULT 'parallel_generators'
      RETURNING VALUE(rv_is_running) TYPE abap_bool
      RAISING   zcx_cncr_exception.

    METHODS run_rfc
      IMPORTING
                io_serialized        TYPE string
                iv_task_name         TYPE zif_cncr_runnable=>ty_char32 DEFAULT 'TASK1'
      RETURNING VALUE(rv_is_running) TYPE abap_bool
      RAISING   zcx_cncr_exception.

    DATA: mo_runnable   TYPE REF TO zif_cncr_runnable.

ENDCLASS.



CLASS zcl_cncr_async_task IMPLEMENTATION.

  METHOD constructor.
    me->mo_runnable  = io_runnable.

  ENDMETHOD.


  METHOD run.

    DATA(lo_serialized) = zcl_cncr_thread=>serialize( io_runnable = me->mo_runnable ).

    IF iv_group IS INITIAL.
      rv_is_running =  me->run_rfc( io_serialized = lo_serialized   iv_task_name  = iv_task_name ).
    ELSE.
      rv_is_running =  me->run_rfc_with_group( io_serialized = lo_serialized  iv_task_name  = iv_task_name iv_group = iv_group ).
    ENDIF.

    IF rv_is_running = abap_true. EXIT. ENDIF.

  ENDMETHOD.

  METHOD run_rfc_with_group.


    CALL FUNCTION 'ZCNCR_TASK_ASYNC'
      STARTING NEW TASK iv_task_name
      DESTINATION IN GROUP iv_group
      CALLING at_end_process ON END OF TASK
      EXPORTING
        io_serialized = io_serialized
      EXCEPTIONS
        OTHERS        = 1.

    IF sy-subrc = 0.
      rv_is_running = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD run_rfc.
    TRY.
        CALL FUNCTION 'ZCNCR_TASK_ASYNC'
          STARTING NEW TASK iv_task_name
          CALLING at_end_process ON END OF TASK
          EXPORTING
            io_serialized = io_serialized
          EXCEPTIONS
            OTHERS        = 1.

        IF sy-subrc = 0.
          rv_is_running = abap_true.
        ENDIF.

      CATCH zcx_cncr_exception INTO DATA(lx_error).
        RAISE EVENT at_end_thread_failed
            EXPORTING
                et_bapiret = VALUE bapiret2_t( ( lx_error->get_bapireturn( ) ) ) .
        rv_is_running = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD get_process_info.

    CALL FUNCTION 'SPBT_INITIALIZE'
      EXPORTING
        group_name         = iv_group
      IMPORTING
        max_pbt_wps        = ev_max_pbt_wps
        free_pbt_wps       = ev_free_pbt_wps
      EXCEPTIONS
        invalid_group_name = 1
        OTHERS             = 7.

    IF sy-subrc <> 0.
      " Error while Serializing Class &1
*      MESSAGE e010(zcncr_main) INTO zcx_cncr_exception=>mv_msg_text WITH iv_group.
*      zcx_cncr_exception=>s_raise( ).
    ENDIF.

  ENDMETHOD.

  METHOD at_end_process.

    DATA: lo_serialized TYPE string.
    DATA: lt_bapiret TYPE bapiret2_t,
          ls_bapiret TYPE bapiret2.

    RECEIVE RESULTS FROM FUNCTION 'ZCNCR_TASK_ASYNC'
      IMPORTING
        eo_serialized   = lo_serialized
        es_message      = ls_bapiret
      EXCEPTIONS
        error                 = 1
        system_failure        = 2
        communication_failure = 3
        OTHERS = 4.

    APPEND ls_bapiret TO lt_bapiret.

    IF sy-subrc <> 0 OR lo_serialized IS INITIAL.
      MESSAGE e020(zcncr_main) INTO zcx_cncr_exception=>mv_msg_text.
      " Convert the message into bapiret structure
      APPEND zcx_cncr_exception=>get_bapiret2( ) TO lt_bapiret.
      RAISE EVENT at_end_thread_failed EXPORTING et_bapiret = lt_bapiret.
      RETURN.
    ENDIF.

    " Update the runnable instance
    me->mo_runnable = zcl_cncr_thread=>deserialize( io_serialized = lo_serialized ).
    RAISE EVENT at_end_thread EXPORTING eo_runnable = me->mo_runnable.
  ENDMETHOD.

ENDCLASS.

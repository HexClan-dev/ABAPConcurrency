CLASS zcl_cncr_thread DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS: gc_default_group TYPE rzllitab-classname VALUE 'parallel_generators'.

    INTERFACES: zif_cncr_runnable.

    METHODS constructor
      IMPORTING
        io_runnable TYPE REF TO zif_cncr_runnable OPTIONAL.

    METHODS start FINAL
      RETURNING VALUE(rs_errors) TYPE bapiret2
      RAISING   zcx_cncr_exception.

    METHODS wait FINAL
      IMPORTING
        iv_seconds TYPE i OPTIONAL.

    METHODS kill_process FINAL
      RETURNING VALUE(rv_is_stopped) TYPE abap_bool.

    METHODS get_result  FINAL
      RETURNING VALUE(ro_runnable) TYPE REF TO zif_cncr_runnable.

    METHODS at_end_process  FINAL FOR EVENT at_end_thread OF zcl_cncr_async_task
      IMPORTING
        eo_runnable
        et_bapiret.

    METHODS at_end_process_failed  FINAL FOR EVENT at_end_thread_failed OF zcl_cncr_async_task
      IMPORTING
        et_bapiret.

    METHODS: get_execution_time  FINAL
      RETURNING VALUE(rv_execution_time) TYPE i
      RAISING   zcx_cncr_exception.

    CLASS-METHODS serialize
      IMPORTING io_runnable          TYPE REF TO zif_cncr_runnable
      RETURNING VALUE(rv_serialized) TYPE string
      RAISING   zcx_cncr_exception.

    CLASS-METHODS deserialize
      IMPORTING io_serialized      TYPE string
      RETURNING VALUE(ro_runnable) TYPE REF TO zif_cncr_runnable
      RAISING   zcx_cncr_exception.

    DATA: mv_is_running TYPE abap_bool VALUE abap_false.
    DATA: mt_messages TYPE TABLE OF bapiret2 READ-ONLY.


  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mo_runnable TYPE REF TO zif_cncr_runnable.
    DATA: mv_execution_time TYPE i.

    DATA: mo_exec_time  TYPE REF TO if_abap_runtime,
          mv_start_time TYPE i,
          mv_end_time   TYPE i.


    METHODS start_execution_time.

    METHODS end_execution_time.

ENDCLASS.



CLASS zcl_cncr_thread IMPLEMENTATION.

  METHOD constructor.
    IF io_runnable IS SUPPLIED.
      me->mo_runnable = io_runnable.
    ELSE.
      me->mo_runnable = me.
    ENDIF.

    " Create ABAP runtime time measurement
    me->mo_exec_time = cl_abap_runtime=>create_hr_timer( ).
  ENDMETHOD.

  METHOD start.
    TRY.
        DATA(lo_runnable) = NEW zcl_cncr_async_task(
            io_runnable = me->mo_runnable
        ).

        " Register the Event handler
        SET HANDLER me->at_end_process        FOR lo_runnable.
        SET HANDLER me->at_end_process_failed FOR lo_runnable.

        DATA(lv_task_id) = cl_system_uuid=>create_uuid_c32_static( ).

        me->start_execution_time( ).
        me->mv_is_running = lo_runnable->run( iv_task_name = lv_task_id ).

      CATCH cx_uuid_error INTO DATA(lx_uuid_error).
        APPEND VALUE #( message = lx_uuid_error->err_text type = 'E' ) TO me->mt_messages.

      CATCH zcx_cncr_exception INTO DATA(lx_error).
        APPEND lx_error->get_bapireturn( ) TO me->mt_messages.

    ENDTRY.

  ENDMETHOD.


  METHOD kill_process.
    " TODO -> implement process killing with shared memory


  ENDMETHOD.


  METHOD serialize.
    CALL TRANSFORMATION id SOURCE oref = io_runnable
                           RESULT XML rv_serialized.

    IF sy-subrc <> 0.
      DATA(lv_class_name) = cl_abap_classdescr=>get_class_name( io_runnable ).

      " Error while Serializing Class &1
      MESSAGE e001(zcncr_main) INTO zcx_cncr_exception=>mv_msg_text WITH lv_class_name.
      zcx_cncr_exception=>s_raise( ).
    ENDIF.
  ENDMETHOD.

  METHOD deserialize.
    CALL TRANSFORMATION id SOURCE XML io_serialized RESULT oref = ro_runnable.

    IF sy-subrc <> 0.
      DATA(lv_class_name) = cl_abap_classdescr=>get_class_name( ro_runnable ).

      " Error while deserializing Class &1
      MESSAGE e002(zcncr_main) INTO zcx_cncr_exception=>mv_msg_text WITH lv_class_name.
      zcx_cncr_exception=>s_raise( ).
    ENDIF.
  ENDMETHOD.

  METHOD at_end_process.
    " This is ending the process
    me->mv_is_running = abap_false.

    APPEND LINES OF et_bapiret TO me->mt_messages.

    " Measure the execution time of the execution
    me->end_execution_time( ).

    IF eo_runnable IS NOT INITIAL.
      " Update the Runnable instance
      me->mo_runnable = eo_runnable.
    ENDIF.
  ENDMETHOD.

  METHOD at_end_process_failed.
    " This is ending the process
    me->mv_is_running = abap_false.

    APPEND LINES OF et_bapiret TO me->mt_messages.

    " Measure the execution time of the execution
    me->end_execution_time( ).
  ENDMETHOD.


  METHOD get_execution_time.
*    IF mo_exec_time IS NOT BOUND.
*      MESSAGE e015(zcncr_main) INTO zcx_cncr_exception=>mv_msg_text.
*      zcx_cncr_exception=>s_raise( ).
*    ENDIF.

    rv_execution_time = me->mv_end_time - me->mv_start_time.
  ENDMETHOD.

  METHOD wait.
    IF iv_seconds IS SUPPLIED.
      WAIT UNTIL me->mv_is_running = abap_false UP TO iv_seconds SECONDS.
    ELSE.
      WAIT UNTIL me->mv_is_running = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD start_execution_time.
    me->mv_start_time = me->mo_exec_time->get_runtime( ).
  ENDMETHOD.

  METHOD end_execution_time.
    me->mv_end_time = me->mo_exec_time->get_runtime( ).
  ENDMETHOD.


  METHOD get_result.
    IF me->mv_is_running = abap_true.
      me->wait( ).
    ENDIF.

    ro_runnable = me->mo_runnable.
  ENDMETHOD.

  METHOD zif_cncr_runnable~run.
    DATA(lv_class_name) = cl_abap_classdescr=>get_class_name( me ).

    MESSAGE e016(zcncr_main) WITH lv_class_name INTO zcx_cncr_exception=>mv_msg_text.
    zcx_cncr_exception=>s_raise( ).
  ENDMETHOD.

ENDCLASS.

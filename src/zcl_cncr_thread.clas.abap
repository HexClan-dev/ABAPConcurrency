CLASS zcl_cncr_thread DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS: gc_default_group TYPE rzllitab-classname VALUE 'parallel_generators'.

    METHODS constructor
      IMPORTING
        io_runnable TYPE REF TO zif_cncr_runnable.

    METHODS start
      RETURNING VALUE(rs_errors) TYPE bapiret2
      RAISING   zcx_cncr_exception.

    METHODS wait
      IMPORTING
        iv_seconds TYPE i OPTIONAL.

    METHODS kill_process
      RETURNING VALUE(rv_is_stopped) TYPE abap_bool.


    METHODS get_result
      RETURNING VALUE(ro_runnable) TYPE REF TO zif_cncr_runnable.


    METHODS at_end_process FOR EVENT at_end_thread OF zcl_cncr_async_task
      IMPORTING
        eo_runnable
        es_bapiret.

    CLASS-METHODS serialize
      IMPORTING io_runnable          TYPE REF TO zif_cncr_runnable
      RETURNING VALUE(rv_serialized) TYPE string
      RAISING   zcx_cncr_exception.

    CLASS-METHODS deserialize
      IMPORTING io_serialized      TYPE string
      RETURNING VALUE(ro_runnable) TYPE REF TO zif_cncr_runnable
      RAISING   zcx_cncr_exception.

    DATA: mv_is_running TYPE abap_bool VALUE abap_false.
    DATA: ms_bapiret TYPE bapiret2.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mo_runnable TYPE REF TO zif_cncr_runnable.


ENDCLASS.



CLASS zcl_cncr_thread IMPLEMENTATION.

  METHOD constructor.
    me->mo_runnable = io_runnable.
  ENDMETHOD.

  METHOD start.
    TRY.
        DATA(lo_runnable) = NEW zcl_cncr_async_task( io_runnable = me->mo_runnable ).

        " Register the Event handler
        SET HANDLER me->at_end_process FOR lo_runnable.

        DATA(lv_task_id) = cl_system_uuid=>create_uuid_c32_static( ).

        me->mv_is_running = lo_runnable->run( iv_task_name = lv_task_id ).

      CATCH cx_uuid_error INTO DATA(lx_uuid_error).
        rs_errors = VALUE #( message = lx_uuid_error->err_text type = 'E' ).

      CATCH zcx_cncr_exception INTO DATA(lx_error).
        "Raise ZCX_CNCR_* Error
        rs_errors = lx_error->get_bapireturn( ).
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
    me->ms_bapiret = es_bapiret.

    IF eo_runnable IS NOT INITIAL.
      " Update the Runnable instance
      me->mo_runnable = eo_runnable.
    ENDIF.
  ENDMETHOD.

  METHOD wait.
    IF iv_seconds IS SUPPLIED.
      WAIT UNTIL me->mv_is_running = abap_false UP TO iv_seconds SECONDS.
    ELSE.
      WAIT UNTIL me->mv_is_running = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD get_result.
    IF me->mv_is_running = abap_true.
      me->wait( ).
    ENDIF.

    ro_runnable = me->mo_runnable.
  ENDMETHOD.

ENDCLASS.

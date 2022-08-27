CLASS zcl_cncr_thread DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        io_runnable TYPE REF TO zif_cncr_runnable.

    METHODS start.


    CLASS-METHODS serialize
      IMPORTING io_runnable          TYPE REF TO zif_cncr_runnable
      RETURNING VALUE(rv_serialized) TYPE string
      RAISING   zcx_cncr_exception.

    CLASS-METHODS deserialize
      IMPORTING io_serialized      TYPE string
      RETURNING VALUE(ro_runnable) TYPE REF TO zif_cncr_runnable
      RAISING   zcx_cncr_exception.


  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mo_runnable TYPE REF TO zif_cncr_runnable.

ENDCLASS.



CLASS zcl_cncr_thread IMPLEMENTATION.

  METHOD constructor.
    me->mo_runnable = io_runnable.
  ENDMETHOD.

  METHOD start.

    me->mo_runnable->run( ).

  ENDMETHOD.

  METHOD serialize.

    CALL TRANSFORMATION id SOURCE oref = io_runnable
                           RESULT XML rv_serialized.

    IF sy-subrc <> 0.
      " TODO -> add a message
      zcx_cncr_exception=>s_raise( ).
    ENDIF.

  ENDMETHOD.

  METHOD deserialize.

    CALL TRANSFORMATION id SOURCE XML io_serialized RESULT oref = ro_runnable.

    IF sy-subrc <> 0.
      " TODO -> add a message
      zcx_cncr_exception=>s_raise( ).
    ENDIF.

  ENDMETHOD.


ENDCLASS.

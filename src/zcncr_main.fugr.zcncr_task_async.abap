FUNCTION ZCNCR_TASK_ASYNC.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IO_SERIALIZED) TYPE  STRING
*"  EXPORTING
*"     VALUE(EO_SERIALIZED) TYPE  STRING
*"     VALUE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------
  TRY.

      DATA: lo_runnable TYPE REF TO zif_cncr_runnable.

      lo_runnable = zcl_cncr_thread=>deserialize( io_serialized = io_serialized ).

      lo_runnable->run( ).

      " Return the serialized objects
      eo_serialized = zcl_cncr_thread=>serialize( io_runnable = lo_runnable ).
    CATCH zcx_cncr_exception INTO DATA(lx_exc).
      es_message = lx_exc->get_bapireturn( ).
  ENDTRY.

ENDFUNCTION.

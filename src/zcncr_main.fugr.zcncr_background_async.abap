FUNCTION ZCNCR_BACKGROUND_ASYNC.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IO_SERIALIZED) TYPE  STRING
*"----------------------------------------------------------------------
  TRY.

      DATA: lo_runnable TYPE REF TO zif_cncr_runnable.
      lo_runnable = zcl_cncr_thread=>deserialize( io_serialized = io_serialized ).
      lo_runnable->run( ).

    CATCH zcx_cncr_exception INTO DATA(lx_exc).
  ENDTRY.

ENDFUNCTION.

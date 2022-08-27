CLASS zcl_cncr_async_task DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES: zif_cncr_thread.

    METHODS constructor
      IMPORTING
        io_runnable TYPE REF TO zif_cncr_runnable.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mo_runnable TYPE REF TO zif_cncr_runnable.

ENDCLASS.



CLASS zcl_cncr_async_task IMPLEMENTATION.

  METHOD constructor.
    me->mo_runnable  = io_runnable.

  ENDMETHOD.


  METHOD zif_cncr_thread~run.


    call function 'ZCNCR_TASK_ASYNC'
      STARTING NEW TASK ''
      EXPORTING
        iv_param1 = ''
      .


  ENDMETHOD.


  METHOD zif_cncr_thread~stop.



  ENDMETHOD.


ENDCLASS.

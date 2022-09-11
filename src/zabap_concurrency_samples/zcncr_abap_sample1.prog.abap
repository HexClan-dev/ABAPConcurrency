*&---------------------------------------------------------------------*
*& Report zcncr_abap_sample1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcncr_abap_sample1.



CLASS lcl_main DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: zif_cncr_runnable.


    METHODS: calculate_fibonaci
      IMPORTING
                VALUE(iv_index)       TYPE  i
      RETURNING VALUE(rv_fibo_number) TYPE  i.

    DATA: mv_value TYPE i.
    DATA: mv_fibonaci TYPE i VALUE 35.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD zif_cncr_runnable~run.

    mv_value = me->calculate_fibonaci(
      EXPORTING
        iv_index       = mv_fibonaci
    ).

  ENDMETHOD.

  METHOD calculate_fibonaci.

    IF iv_index < 0. "exception handling
      RETURN.
    ELSE. "calculate the result
      DATA(index_substracted1) = iv_index - 1.
      DATA(index_substracted2) = iv_index - 2.

      IF iv_index LE 2.
        rv_fibo_number = 1.
      ELSE.
        DATA(result1) = 0.
        result1 = me->calculate_fibonaci(
              EXPORTING
                iv_index       = index_substracted1
        ).

        DATA(result2) = 0.
        result2 = me->calculate_fibonaci(
             EXPORTING
               iv_index       = index_substracted2
        ).

        rv_fibo_number = result1 + result2. "return
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.



START-OF-SELECTION.

  DATA(lo_exec_time) = cl_abap_runtime=>create_hr_timer( ).
  DATA(lv_start_time) = lo_exec_time->get_runtime( ).

  DATA(lo_runn1) = NEW lcl_main( ).
  lo_runn1->mv_fibonaci = 35.
  DATA(lo_thread) = NEW zcl_cncr_thread( lo_runn1 ).

  DATA(lo_runn2) = NEW lcl_main( ).
  lo_runn2->mv_fibonaci = 36.
  DATA(lo_thread2) = NEW zcl_cncr_thread( lo_runn2 ).

  DATA(lo_runn3) = NEW lcl_main( ).
  lo_runn3->mv_fibonaci = 37.
  DATA(lo_thread3) = NEW zcl_cncr_thread( lo_runn3 ).

  lo_thread->start( ).
  lo_thread2->start( ).
  lo_thread3->start( ).

  lo_runn1 ?= lo_thread->get_result( ).
  WRITE: 'The process ended successfully!'.
  WRITE: /, 'Fibonaci sequence 1 is ',
            lo_runn1->mv_value ,
            '  Execution Time : ',
            lo_thread->get_execution_time( ).

  lo_runn2 ?= lo_thread2->get_result( ).
  WRITE: /, 'Fibonaci sequence 2 is ',
            lo_runn2->mv_value ,
            '  Execution Time : ',
            lo_thread2->get_execution_time( ).

  lo_runn3 ?= lo_thread3->get_result( ).
  WRITE: /, 'Fibonaci sequence 3 is ',
            lo_runn3->mv_value ,
            '  Execution Time : ',
            lo_thread3->get_execution_time( ).

  DATA(lv_exec_time) = lo_exec_time->get_runtime( ) - lv_start_time.


  DATA(lv_thread_exec_time) = lo_thread->get_execution_time( ) + lo_thread2->get_execution_time( ) + lo_thread3->get_execution_time( ).
  WRITE: /, 'Total Sum Thread Execution Time : ',
            lv_thread_exec_time .

  WRITE: /, 'Execution time : ',
            lv_exec_time.

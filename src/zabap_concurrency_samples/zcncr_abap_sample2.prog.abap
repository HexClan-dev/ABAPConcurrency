*&---------------------------------------------------------------------*
*& Report zcncr_abap_sample2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcncr_abap_sample2.


CLASS lcl_main DEFINITION INHERITING FROM zcl_cncr_thread CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: calculate_fibonaci
      IMPORTING
                VALUE(iv_index)       TYPE  i
      RETURNING VALUE(rv_fibo_number) TYPE  i.

    METHODS: zif_cncr_runnable~run REDEFINITION.

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

  TRY.
      DATA: lo_result_main TYPE REF TO lcl_main.
      DATA(lo_exec_time) = cl_abap_runtime=>create_hr_timer( ).
      DATA(lv_start_time) = lo_exec_time->get_runtime( ).

      DATA(lo_runn1) = NEW lcl_main( ).
      lo_runn1->mv_fibonaci = 35.

      DATA(lo_runn2) = NEW lcl_main( ).
      lo_runn2->mv_fibonaci = 36.

      DATA(lo_runn3) = NEW lcl_main( ).
      lo_runn3->mv_fibonaci = 37.


      lo_runn1->start( ).
      lo_runn2->start( ).
      lo_runn3->start( ).


      lo_result_main ?= lo_runn1->get_result( ).
      WRITE: 'The process ended successfully!'.
      WRITE: /, 'Fibonaci sequence 1 is ',
                lo_result_main->mv_value ,
                '  Execution Time : ',
                lo_runn1->get_execution_time( ).

      lo_result_main ?= lo_runn2->get_result( ).
      WRITE: /, 'Fibonaci sequence 2 is ',
                lo_result_main->mv_value ,
                '  Execution Time : ',
                lo_runn2->get_execution_time( ).

      lo_result_main ?= lo_runn3->get_result( ).
      WRITE: /, 'Fibonaci sequence 3 is ',
                lo_result_main->mv_value ,
                '  Execution Time : ',
                lo_runn3->get_execution_time( ).

      DATA(lv_exec_time) = lo_exec_time->get_runtime( ) - lv_start_time.

*  lo_runn4 ?= lo_thread4->get_result( ).
*  WRITE: /, 'Fibonaci sequence 4 is ', lo_runn4->mv_value.
      DATA(lv_thread_exec_time) = lo_runn1->get_execution_time( ) + lo_runn2->get_execution_time( ) + lo_runn3->get_execution_time( ).
      WRITE: /, 'Total Sum Thread Execution Time : ',
                lv_thread_exec_time .

      WRITE: /, 'Execution time : ',
                lv_exec_time.


    CATCH zcx_cncr_exception INTO DATA(lx_error).
      WRITE : / ,' An error ocurred : ' ,lx_error->get_text( ).
  ENDTRY.

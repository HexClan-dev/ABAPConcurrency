*&---------------------------------------------------------------------*
*& Report zcncr_no_abap_sample1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcncr_no_abap_sample1.

CLASS lcl_main DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS: run.

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

  METHOD run.

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

  DATA(lo_runn2) = NEW lcl_main( ).
  lo_runn2->mv_fibonaci = 36.

  DATA(lo_runn3) = NEW lcl_main( ).
  lo_runn3->mv_fibonaci = 37.

*  DATA(lo_runn4) = NEW lcl_main( ).
*  lo_runn4->mv_fibonaci = 37.

  DATA(lv_t1) = lo_exec_time->get_runtime( ).
  lo_runn1->run( ).

  DATA(lv_t2) = lo_exec_time->get_runtime( ).
  lo_runn2->run( ).

  DATA(lv_t3) = lo_exec_time->get_runtime( ).
  lo_runn3->run( ).
  DATA(lv_t4) = lo_exec_time->get_runtime( ).

  DATA(lv_exec_time) = lv_t4 - lv_start_time.


  DATA(lv_exec_t1) = lv_t2 - lv_t1.
  DATA(lv_exec_t2) = lv_t3 - lv_t2.
  DATA(lv_exec_t3) = lv_t4 - lv_t3.



  WRITE: 'The process ended successfully!'.
  WRITE: /, 'Fibonaci sequence 1 is ', lo_runn1->mv_value, ' Execution Time : ', lv_exec_t1.

  WRITE: /, 'Fibonaci sequence 2 is ', lo_runn2->mv_value, ' Execution Time : ', lv_exec_t2.

  WRITE: /, 'Fibonaci sequence 3 is ', lo_runn3->mv_value, ' Execution Time : ', lv_exec_t3.

*  WRITE: /, 'Fibonaci sequence 4 is ', lo_runn4->mv_value.
  DATA(lv_total_exec_time) = lv_exec_t1 + lv_exec_t2 + lv_exec_t3.
  WRITE: /, 'Total Sum Execution Time : ', lv_total_exec_time.

  WRITE: /, 'Execution time : ', lv_exec_time.

*&---------------------------------------------------------------------*
*& Report zcncr_abap_sample3
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcncr_abap_sample3.



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

  DATA(lo_thread_pool) = NEW zcl_cncr_thread_pool( 3 ).

  DATA(lo_test_f1) = NEW lcl_main( ).
  lo_test_f1->mv_value = 40.
  DATA(lo_test_f2) = NEW lcl_main( ).
  lo_test_f2->mv_value = 36.
  DATA(lo_test_f3) = NEW lcl_main( ).
  lo_test_f3->mv_value = 40.

  DATA(lv_thread_name1) = lo_thread_pool->add_thread(
      io_runnable    = lo_test_f1
  ).

  lo_thread_pool->add_thread(
      io_runnable    = lo_test_f2
  ).

  lo_thread_pool->add_thread(
      iv_thread_name = 'Thread3'
      io_runnable    = lo_test_f3
  ).

  lo_thread_pool->execute( ).

*  lo_thread_pool->wait_for_all( ).
  lo_thread_pool->wait_for(
    EXPORTING
      iv_thread_name = lv_thread_name1
      iv_status      = zcl_cncr_thread_pool=>finished
  ).

*  BREAK-POINT.
*  cl_demo_output=>display_data( lo_thread_pool->mt_runnable[] ).

INTERFACE zif_cncr_thread
  PUBLIC .

  METHODS run.

  METHODS stop
    RETURNING VALUE(rv_is_stopped) TYPE abap_bool.

*  METHODS get_data.

ENDINTERFACE.

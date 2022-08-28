INTERFACE zif_cncr_runnable
  PUBLIC .

  TYPES: ty_char32 TYPE c LENGTH 32 .


  "Will be used to serialize the Thread Classes
  INTERFACES: if_serializable_object.

  METHODS: run
    RETURNING VALUE(ro_runnable) TYPE REF TO zif_cncr_runnable
    RAISING   zcx_cncr_exception.


*  METHODS kill_process
*    RETURNING VALUE(rv_is_stopped) TYPE abap_bool.


ENDINTERFACE.

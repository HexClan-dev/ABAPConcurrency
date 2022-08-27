INTERFACE zif_cncr_runnable
  PUBLIC .

  "Will be used to serialize the Thread Classes
  INTERFACES: zif_abapgit_data_serializer.

  METHODS: run
    RETURNING VALUE(ro_runnable) TYPE REF TO zif_cncr_runnable.


ENDINTERFACE.

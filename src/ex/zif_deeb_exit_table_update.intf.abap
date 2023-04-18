interface ZIF_DEEB_EXIT_TABLE_UPDATE
  public .


  methods MODIFY_INCOMING_RECORD
    importing
      !IT_FIELDS type DDFIELDS
    exporting
      !EV_SKIP type ABAP_BOOL
      !EV_MESSAGE type STRING
    changing
      !CS_DATA type DATA
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
endinterface.

interface ZIF_DEEB_UT_TABLE_UPDATE
  public .


  methods UPDATE
    importing
      !IS_PARAMS type ZDEEB_S_TABLE_UPDATE_PAR
      !IT_TABLE type TABLE optional
      !IR_TABLE type ref to DATA optional
    exporting
      !EV_MESSAGE type STRING
      !EV_MODIFIED_LINES type I
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods GET_TABLE_FROM_JSON
    importing
      !IV_TABLE type STRING
      !IV_JSON type STRING
    exporting
      !EV_ERROR type STRING
    returning
      value(RR_TABLE) type ref to DATA .
  methods GET_UTIL
    returning
      value(RR_UTIL) type ref to ZIF_DEEB_UT_DDIC_UTIL .
  methods GET_EXIT
    returning
      value(RR_EXIT) type ref to ZIF_DEEB_EXIT_TABLE_UPDATE .
  methods IS_EXIT_AVAILABLE
    returning
      value(RV_EXIT) type ABAP_BOOL .
  methods INIT_EXIT
    importing
      !IV_TYPE type DATA
    exporting
      !EV_ERROR type STRING
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
endinterface.

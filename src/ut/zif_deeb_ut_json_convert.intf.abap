interface ZIF_DEEB_UT_JSON_CONVERT
  public .


  methods FROM_JSON
    importing
      !IV_JSON type STRING
    changing
      !CA_DATA type DATA .
endinterface.

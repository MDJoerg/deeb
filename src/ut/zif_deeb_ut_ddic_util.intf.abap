interface ZIF_DEEB_UT_DDIC_UTIL
  public .


  methods GET_DDIC_FIELDS_FROM_STRUC
    importing
      !IS_DATA type DATA
      !IV_LANG type LANGUAGE default SY-LANGU
      !IV_WITH_SUB type ABAP_BOOL default ABAP_FALSE
    returning
      value(RT_FIELDS) type DDFIELDS .
  methods GET_FIELDS_FROM_FROM_STRUC
    importing
      !IS_DATA type DATA
      !IV_LANG type LANGUAGE default SY-LANGU
      !IV_WITH_SUB type ABAP_BOOL default ABAP_FALSE
      !IV_ONLY_KEYS type ABAP_BOOL default ABAP_FALSE
      !IV_WITH_CLIENT type ABAP_BOOL default ABAP_FALSE
      !IT_IGNORE type STRING_TABLE optional
      !IV_DATA_TYPES type STRING optional
      !IV_WILDCARD type STRING optional
    returning
      value(RT_FIELDS) type STRING_TABLE .
  methods IS_STRING_IN_TAB
    importing
      !IV_CHECK type STRING
      !IT_DATA type STRING_TABLE
    returning
      value(RV_FOUND) type ABAP_BOOL .
  methods APPEND_WHERE_FROM_VALUE
    importing
      !IV_FIELDNAME type STRING
      !IV_OPERAND type STRING default '='
      !IV_COMBINE type STRING default 'AND'
      !IV_ENCLOSE type ABAP_BOOL default ABAP_TRUE
      !IV_VALUE type DATA
      !IT_CURRENT type STRING_TABLE
    returning
      value(RT_WHERE) type STRING_TABLE .
  methods APPEND_WHERE_FROM_STRUC_FIELD
    importing
      !IV_FIELDNAME type STRING
      !IV_OPERAND type STRING default '='
      !IV_COMBINE type STRING default 'AND'
      !IV_STRUC_NAME type STRING optional
      !IV_MASK type ABAP_BOOL default ABAP_TRUE
      !IT_CURRENT type STRING_TABLE
    returning
      value(RT_WHERE) type STRING_TABLE .
  methods IS_STRUC_EQUAL
    importing
      !IS_DATA1 type DATA
      !IS_DATA2 type DATA
      !IT_IGNORE type STRING_TABLE
    returning
      value(RV_EQUAL) type ABAP_BOOL .
  methods IS_TABLE_RECORD_MODIFIED
    importing
      !IV_TABLE type STRING
      !IS_DATA type DATA
      !IV_CHECK_TIMESTAMP type ABAP_BOOL
      !IV_WHERE type STRING optional
    returning
      value(RV_MODIFIED) type ABAP_BOOL .
  methods IS_BOOLEAN_TRUE
    importing
      !IV_CHECK type DATA
    returning
      value(RV_BOOL_TRUE) type ABAP_BOOL .
endinterface.

interface ZIF_DEEB_WS_BL_BSP
  public .


  interfaces ZIF_DEEB_UT_BSP_SRV .

  aliases M_BSP_NAVIGATION
    for ZIF_DEEB_UT_BSP_SRV~M_BSP_NAVIGATION .
  aliases M_BSP_PAGE
    for ZIF_DEEB_UT_BSP_SRV~M_BSP_PAGE .
  aliases PING
    for ZIF_DEEB_UT_BSP_SRV~PING .

  constants C_TABLE_FIELD_TIMESTAMP type STRING value 'REC_TIMESTAMP' ##NO_TEXT.

  methods SQL_SELECT .
  methods TABLE_UPDATE .
endinterface.

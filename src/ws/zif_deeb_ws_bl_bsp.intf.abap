INTERFACE zif_deeb_ws_bl_bsp
  PUBLIC .


  INTERFACES zif_deeb_ut_bsp_srv .

  ALIASES m_bsp_navigation
    FOR zif_deeb_ut_bsp_srv~m_bsp_navigation .
  ALIASES m_bsp_page
    FOR zif_deeb_ut_bsp_srv~m_bsp_page .
  ALIASES ping
    FOR zif_deeb_ut_bsp_srv~ping .

  METHODS sql_select .

  METHODS table_update.

ENDINTERFACE.

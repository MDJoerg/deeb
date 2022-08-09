class ZCL_DEEB_WS_BL definition
  public
  create public .

public section.

  interfaces ZIF_DEEB_WS_BL_BSP .

  class-data GV_BL_CLASS type SEOCLSNAME value 'ZCL_DEEB_WS_BL' ##NO_TEXT.
  class-data GV_ENCODING type ABAP_ENCOD .

  methods CONVERT_STRING_TO_XSTRING
    importing
      !IV_STRING type STRING
    returning
      value(RV_XSTRING) type XSTRING .
  methods SET_RESPONSE_STATUS
    importing
      !IV_CODE type I
      !IV_TEXT type STRING
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods SET_RESPONSE_STRING
    importing
      !IV_CONTENT type STRING
      !IV_CONTENT_TYPE type STRING
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods SET_RESPONSE_XSTRING
    importing
      !IV_XCONTENT type XSTRING
      !IV_CONTENT_TYPE type STRING
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  class-methods CREATE_BL_BSP
    importing
      !IR_BSP_PAGE type ref to CL_BSP_PAGE_BASE
      !IR_BSP_NAVIGATION type ref to CL_BSP_NAVIGATION
    returning
      value(RR_INSTANCE) type ref to ZIF_DEEB_WS_BL_BSP .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DEEB_WS_BL IMPLEMENTATION.


  METHOD convert_string_to_xstring.
    CALL FUNCTION 'ECATT_CONV_STRING_TO_XSTRING'
      EXPORTING
        im_string   = iv_string
        im_encoding = gv_encoding
      IMPORTING
        ex_xstring  = rv_xstring.
  ENDMETHOD.


  METHOD create_bl_bsp.

* ------ create bl instance and set context
* user-exit: modify implementation via implicite enhancement and overwrite mv_bl_class

    CREATE OBJECT rr_instance TYPE (gv_bl_class).
    rr_instance->m_bsp_navigation = ir_bsp_navigation.
    rr_instance->m_bsp_page       = ir_bsp_page.

  ENDMETHOD.


  METHOD set_response_status.

* ------- check context
    IF zif_deeb_ws_bl_bsp~m_bsp_page IS INITIAL.
      RETURN.
    ENDIF.


* ----- get response and set status
    DATA(lr_response) = zif_deeb_ws_bl_bsp~m_bsp_page->if_bsp_page~get_response( ).
    CALL METHOD lr_response->set_status
      EXPORTING
        code   = iv_code
        reason = iv_text.
    rv_success = abap_true.
  ENDMETHOD.


  METHOD set_response_string.

* ------- check context
    IF zif_deeb_ws_bl_bsp~m_bsp_page IS INITIAL.
      RETURN.
    ENDIF.


* ----- get response and set status
    DATA: lv_xstring TYPE xstring.

* ----- map to binary
    DATA(lv_xcontent) = convert_string_to_xstring( iv_content ).

* ----- set as binary
    rv_success = set_response_xstring(
                     iv_xcontent     = lv_xcontent
                     iv_content_type = iv_content_type
    ).
  ENDMETHOD.


  METHOD set_response_xstring.

* ------- check context
    IF zif_deeb_ws_bl_bsp~m_bsp_page IS INITIAL.
      RETURN.
    ELSE.
      DATA(lr_response) = zif_deeb_ws_bl_bsp~m_bsp_page->if_bsp_page~get_response( ).
    ENDIF.

* ----- local data
    DATA: lv_xsize   TYPE i.
    DATA: lv_size    TYPE string.

* ----- map to binary
    lv_xsize   = xstrlen( iv_xcontent ).
    lv_size    = lv_xsize.
    CONDENSE lv_size.

* ----- set the response
    lr_response->set_data( iv_xcontent ).
    lr_response->set_header_field( name = 'Content-Length' value = lv_size ).
    lr_response->set_header_field( name = 'Content-Type'   value = iv_content_type ).

* ----- finally true
    rv_success = abap_true.
  ENDMETHOD.


  METHOD zif_deeb_ws_bl_bsp~ping.
    set_response_status(
        iv_code    = 200
        iv_text    = |OK|
    ).
    set_response_string(
        iv_content      = |DEEP WebService is alive (connected to { sy-sysid }/{ sy-mandt })|
        iv_content_type = 'text/text'
    ).
  ENDMETHOD.
ENDCLASS.

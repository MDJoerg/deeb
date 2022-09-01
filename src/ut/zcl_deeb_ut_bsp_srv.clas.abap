class ZCL_DEEB_UT_BSP_SRV definition
  public
  create public .

public section.

  interfaces ZIF_DEEB_UT_BSP_SRV .

  class-data GV_ENCODING type ABAP_ENCOD .

  methods CONVERT_STRING_TO_XSTRING
    importing
      !IV_STRING type STRING
    returning
      value(RV_XSTRING) type XSTRING .
  methods GET_REQUEST
    returning
      value(RR_REQUEST) type ref to IF_HTTP_REQUEST .
  methods GET_RESPONSE
    returning
      value(RR_RESPONSE) type ref to IF_HTTP_RESPONSE .
  methods SET_RESPONSE_BAD_REQUEST
    importing
      !IV_REASON type DATA
      !IV_FILL_PAYLOAD type ABAP_BOOL default ABAP_TRUE
      !IV_PAYLOAD type STRING optional .
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
  class-methods GET_VERSION
    returning
      value(RV_VERSION) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DEEB_UT_BSP_SRV IMPLEMENTATION.


  METHOD CONVERT_STRING_TO_XSTRING.
    CALL FUNCTION 'ECATT_CONV_STRING_TO_XSTRING'
      EXPORTING
        im_string   = iv_string
        im_encoding = gv_encoding
      IMPORTING
        ex_xstring  = rv_xstring.
  ENDMETHOD.


  METHOD GET_REQUEST.
* ------- check context
    IF ZIF_DEEB_UT_BSP_SRV~m_bsp_page IS INITIAL.
      RETURN.
    ELSE.
      rr_request = ZIF_DEEB_UT_BSP_SRV~m_bsp_page->if_bsp_page~get_request( ).
    ENDIF.
  ENDMETHOD.


  METHOD GET_RESPONSE.
* ------- check context
    IF ZIF_DEEB_UT_BSP_SRV~m_bsp_page IS INITIAL.
      RETURN.
    ELSE.
      rr_response = ZIF_DEEB_UT_BSP_SRV~m_bsp_page->if_bsp_page~get_response( ).
    ENDIF.
  ENDMETHOD.


  METHOD GET_VERSION.
    rv_version = zif_deeb_c=>version.
  ENDMETHOD.


  METHOD SET_RESPONSE_BAD_REQUEST.
* ------ prepare answer
    DATA(lv_text) = |Bad Request|.
    IF iv_reason IS NOT INITIAL.
      lv_text = lv_text && | - { iv_reason }|.
    ENDIF.

* ------ set http code/text
    set_response_status(
        iv_code    = 400
        iv_text    = lv_text
    ).

* ------ set content
    IF iv_fill_payload = abap_true.
      DATA(lv_payload) = iv_payload.
      IF lv_payload IS INITIAL.
        lv_payload = lv_text.
      ENDIF.

      set_response_string(
          iv_content      = lv_payload
          iv_content_type = 'text/text'
      ).
    ENDIF.
  ENDMETHOD.


  METHOD SET_RESPONSE_STATUS.

* ------- check context
    IF ZIF_DEEB_UT_BSP_SRV~m_bsp_page IS INITIAL.
      RETURN.
    ENDIF.


* ----- get response and set status
    DATA(lr_response) = ZIF_DEEB_UT_BSP_SRV~m_bsp_page->if_bsp_page~get_response( ).
    CALL METHOD lr_response->set_status
      EXPORTING
        code   = iv_code
        reason = iv_text.
    rv_success = abap_true.
  ENDMETHOD.


  METHOD SET_RESPONSE_STRING.

* ------- check context
    IF ZIF_DEEB_UT_BSP_SRV~m_bsp_page IS INITIAL.
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


  METHOD SET_RESPONSE_XSTRING.

* ------- check context
    IF ZIF_DEEB_UT_BSP_SRV~m_bsp_page IS INITIAL.
      RETURN.
    ELSE.
      DATA(lr_response) = ZIF_DEEB_UT_BSP_SRV~m_bsp_page->if_bsp_page~get_response( ).
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


  method ZIF_DEEB_UT_BSP_SRV~PING.
    set_response_status(
        iv_code    = 200
        iv_text    = |OK|
    ).
    set_response_string(
        iv_content      = |DEEP WebService is alive (connected to SAP system { sy-sysid }/{ sy-mandt }, backend release { ZIF_DEEB_C=>release })|
        iv_content_type = 'text/text'
    ).
  endmethod.
ENDCLASS.

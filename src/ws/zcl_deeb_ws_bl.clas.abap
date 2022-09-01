class ZCL_DEEB_WS_BL definition
  public
  inheriting from ZCL_DEEB_UT_BSP_SRV
  create public .

public section.

  interfaces ZIF_DEEB_WS_BL_BSP .

  class-data GV_BL_CLASS type SEOCLSNAME value 'ZCL_DEEB_WS_BL' ##NO_TEXT.
  class-data GV_MAX_RECORDS type I value 10000 ##NO_TEXT.

  methods EXECUTE_SQL
    importing
      !IV_SQL type STRING
      !IV_MAX type I
    exporting
      !ER_DATA type ref to DATA
      !EV_TEXT type STRING
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods IS_SQL_STATEMENT_ALLOWED
    importing
      !IV_SQL type STRING
    returning
      value(RV_ALLOWED) type ABAP_BOOL .
  class-methods CREATE_BL_BSP
    importing
      !IR_BSP_PAGE type ref to CL_BSP_PAGE_BASE
      !IR_BSP_NAVIGATION type ref to CL_BSP_NAVIGATION
    returning
      value(RR_INSTANCE) type ref to ZIF_DEEB_WS_BL_BSP .
protected section.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DEEB_WS_BL IMPLEMENTATION.


  METHOD create_bl_bsp.

* ------ create bl instance and set context
* user-exit: modify implementation via implicite enhancement and overwrite mv_bl_class

    CREATE OBJECT rr_instance TYPE (gv_bl_class).
    rr_instance->m_bsp_navigation = ir_bsp_navigation.
    rr_instance->m_bsp_page       = ir_bsp_page.

  ENDMETHOD.


  METHOD execute_sql.

* ------------------- check incoming statement
    IF is_sql_statement_allowed( iv_sql ) EQ abap_false.
      ev_text = |invalid SQL statement|.
      RETURN.
    ENDIF.


* ------------------- dynamic sql select
    TRY.
        DATA(lo_stmt) = NEW cl_sql_statement( ).
        DATA(lo_res) = lo_stmt->execute_query( iv_sql ).

        DATA(lt_meta) = lo_res->get_metadata( ).
        LOOP AT lt_meta ASSIGNING FIELD-SYMBOL(<ls_meta>).
          IF <ls_meta>-column_name EQ 'COUNT(*)'.
            <ls_meta>-column_name = 'COUNT'.
          ELSE.
            REPLACE ALL OCCURRENCES OF '(' IN <ls_meta>-column_name WITH '_'.
            REPLACE ALL OCCURRENCES OF ')' IN <ls_meta>-column_name WITH '_'.
            REPLACE ALL OCCURRENCES OF '*' IN <ls_meta>-column_name WITH 'ALL'.
          ENDIF.
          DATA(lv_last) = strlen( <ls_meta>-column_name ) - 1.
          IF <ls_meta>-column_name+lv_last(1) = '_'.
            <ls_meta>-column_name = <ls_meta>-column_name(lv_last).
          ENDIF.
        ENDLOOP.

        DATA lr_struc TYPE REF TO cl_abap_structdescr.
        DATA(lr_sref) = lo_res->get_struct_ref(
          EXPORTING
            md_tab                     = lt_meta
*        string_only                = ABAP_FALSE    " ABAP_TRUE means that char column is taken as string column
*        p_strict                   = ABAP_TRUE    " Typerzeugung mit ABAP-OO Regeln? (s.  CL_ABAP_STRUCT _DESCR)
        ).

        FIELD-SYMBOLS: <wa> TYPE data.
        FIELD-SYMBOLS: <tab> TYPE table.

        ASSIGN lr_sref->* TO <wa>.
        CREATE DATA er_data LIKE TABLE OF <wa>.
        ASSIGN er_data->* TO <tab>.

        lo_res->set_param_table( er_data ).
        lo_res->next_package( iv_max ).
        lo_res->close( ).

        rv_success = abap_true.

      CATCH cx_root INTO DATA(lx).
        ev_text = lx->get_text( ).
    ENDTRY.


  ENDMETHOD.


  METHOD is_sql_statement_allowed.

* -------- check empty
    IF iv_sql IS INITIAL.
      RETURN.
    ENDIF.

* -------- check select in statement
    DATA(lv_upper) = to_upper( iv_sql ).
    IF NOT lv_upper CS 'SELECT'.
      RETURN.
    ENDIF.

* -------- finally success
    rv_allowed = abap_true.

  ENDMETHOD.


  METHOD zif_deeb_ws_bl_bsp~sql_select.

* ------- local data
    DATA(lv_text) = ||.
    DATA lr_data TYPE REF TO data.
    FIELD-SYMBOLS: <tab> TYPE table.



    TRY.
* -------- get request payload and check content
        DATA(lv_json) = get_request( )->get_cdata( ).
        IF lv_json IS INITIAL OR lv_json(1) NE '{'.
          set_response_bad_request( |Missing JSON payload| ).
          RETURN.
        ENDIF.

* --------- unpack json
        DATA ls_params TYPE zdeeb_ws_s_sql_select.
        cl_fdt_json=>json_to_data(
          EXPORTING
            iv_json = lv_json
*    IMPORTING
*      ev_meta =
          CHANGING
            ca_data = ls_params
        ).
        IF ls_params IS INITIAL
          OR ls_params-sql IS INITIAL.
          set_response_bad_request( |Invalid JSON payload| ).
          RETURN.
        ENDIF.

* --------- prepare
        DATA(lv_max) = ls_params-max_records.
        IF lv_max <= 0.
          lv_max = gv_max_records.
        ENDIF.


* --------- process
        IF execute_sql(
          EXPORTING
            iv_sql     = ls_params-sql
            iv_max     = lv_max
          IMPORTING
           er_data    = lr_data
           ev_text    = lv_text
        ) EQ abap_false
          OR lr_data IS INITIAL.
          set_response_bad_request( lv_text ).
          RETURN.
        ENDIF.


* ---------- convert to json
        ASSIGN lr_data->* TO <tab>.
        DATA(lv_json_table) = cl_fdt_json=>data_to_json( ia_data = <tab> ).



* --------- set response
        set_response_string(
            iv_content      = lv_json_table
            iv_content_type = 'text/json'
        ).

        set_response_status(
            iv_code    = 200
            iv_text    = lv_json
        ).


* -------- catch exceptions
      CATCH cx_root INTO DATA(lx_ex).
        lv_text = lx_ex->get_text( ).
        set_response_status(
            iv_code    = 500
            iv_text    = lv_text
        ).
        set_response_string(
            iv_content      = lv_text
            iv_content_type = 'text/text'
        ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.

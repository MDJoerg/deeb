class ZCL_DEEB_WS_BL definition
  public
  inheriting from ZCL_DEEB_UT_BSP_SRV
  create public .

public section.

  interfaces ZIF_DEEB_WS_BL_BSP .

  class-data GV_BL_CLASS type SEOCLSNAME value 'ZCL_DEEB_WS_BL' ##NO_TEXT.
  class-data GV_MAX_RECORDS type I value 10000 ##NO_TEXT.
  class-data GV_CHECK_SE16 type ABAP_BOOL value ABAP_TRUE ##NO_TEXT.

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

* -------- check transaction auth
    IF gv_check_se16 EQ abap_true.
      AUTHORITY-CHECK OBJECT 'S_TCODE'
       ID 'TCD' FIELD 'SE16'.
      DATA(lv_se16) = sy-subrc.

      AUTHORITY-CHECK OBJECT 'S_TCODE'
       ID 'TCD' FIELD 'SE16N'.
      DATA(lv_se16n) = sy-subrc.

      IF NOT ( lv_se16 = 0 OR lv_se16n = 0 ).
        RETURN.
      ENDIF.
    ENDIF.


* -------- check select in statement
    DATA(lv_upper) = to_upper( iv_sql ).
    IF NOT lv_upper CS 'SELECT'.
      RETURN.
    ENDIF.

* -------- filter required tables from select
    DATA lv_next_is_table TYPE abap_bool.
    DATA lt_tables TYPE TABLE OF tabname.

    SPLIT iv_sql AT ' ' INTO TABLE DATA(lt_parts).

    LOOP AT lt_parts ASSIGNING FIELD-SYMBOL(<lv_part>).
      DATA(lv_part) = to_upper( <lv_part> ).
      CONDENSE lv_part.
      IF lv_part EQ 'FROM'
        OR lv_part EQ 'JOIN'.
        lv_next_is_table = abap_true.
      ELSEIF lv_next_is_table EQ abap_true.
        APPEND lv_part TO lt_tables.
        lv_next_is_table = abap_false.
      ENDIF.
    ENDLOOP.


* ------- check tables extracted
    IF lt_tables[] IS INITIAL.
      RETURN. " unknown select
    ENDIF.

* ------- check authorization for table
    LOOP AT lt_tables ASSIGNING FIELD-SYMBOL(<lv_tabname>).
      AUTHORITY-CHECK OBJECT 'S_TABU_NAM'
       ID 'ACTVT' FIELD '03' " display
       ID 'TABLE' FIELD <lv_tabname>.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDLOOP.


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


  METHOD zif_deeb_ws_bl_bsp~table_update.

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

* --------- get params
        DATA lv_timestamp       TYPE timestampl.
        DATA lv_delete          TYPE abap_bool VALUE abap_true.
        DATA lv_with_timestamp  TYPE abap_bool VALUE abap_true.

* --------- unpack json
        DATA ls_params TYPE zdeeb_ws_s_table_update.
        cl_fdt_json=>json_to_data(
          EXPORTING
            iv_json = lv_json
          CHANGING
            ca_data = ls_params
        ).
        IF ls_params IS INITIAL
          OR ls_params-table IS INITIAL
          OR ls_params-data IS INITIAL.
          set_response_bad_request( |Invalid JSON payload| ).
          RETURN.
        ENDIF.

* --------- create table
        CREATE DATA lr_data TYPE TABLE OF (ls_params-table).
        IF lr_data IS INITIAL.
          set_response_bad_request( |Invalid table name { ls_params-table }| ).
          RETURN.
        ELSE.
          ASSIGN lr_data->* TO <tab>.
        ENDIF.


* ---------- transform
        cl_fdt_json=>json_to_data(
          EXPORTING
            iv_json = ls_params-data
          CHANGING
            ca_data = <tab>
        ).

        IF <tab>[] IS INITIAL.
          set_response_bad_request( |Invalid data for table { ls_params-table }| ).
          RETURN.
        ENDIF.


* --------- prepare and check timestamp
        DATA(lv_lin) = lines( <tab> ).
        IF ls_params-with_timestamp EQ abap_true.
          IF ls_params-timestamp IS INITIAL.
            GET TIME STAMP FIELD ls_params-timestamp.
          ENDIF.

          LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<wa>).
            ASSIGN COMPONENT 'TIMESTAMP' OF STRUCTURE <wa> TO FIELD-SYMBOL(<ts>).
            IF <ts> IS NOT ASSIGNED.
              set_response_bad_request( |table { ls_params-table } has no timestamp field| ).
              RETURN.
            ELSE.
              <ts> = ls_params-timestamp.
            ENDIF.
          ENDLOOP.
        ENDIF.


* ----------- modify database
        IF ls_params-delete EQ abap_true.
          DELETE FROM (ls_params-table).
        ENDIF.

        MODIFY (ls_params-table) FROM TABLE <tab>.
        COMMIT WORK.


* --------- set response
        set_response_string(
            iv_content      = |{ lv_lin } lines of table { ls_params-table } updated|
            iv_content_type = 'text/json'
        ).

        set_response_status(
            iv_code    = 200
            iv_text    = 'OK'
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

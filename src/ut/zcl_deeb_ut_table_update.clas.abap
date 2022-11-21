class ZCL_DEEB_UT_TABLE_UPDATE definition
  public
  create public .

public section.

  interfaces ZIF_DEEB_UT_TABLE_UPDATE .

  aliases GET_UTIL
    for ZIF_DEEB_UT_TABLE_UPDATE~GET_UTIL .
protected section.

  data MS_PARAMS type ZDEEB_S_TABLE_UPDATE_PAR .
  data MR_TABLE type ref to DATA .
  data MR_WA type ref to DATA .
  data MR_UTIL type ref to ZIF_DEEB_UT_DDIC_UTIL .

  methods IS_RECORD_MODIFIED
    importing
      !IS_DATA type DATA
      !IV_CHECK_TIMESTAMP type ABAP_BOOL
      !IV_TABLE type STRING
    returning
      value(RV_MODIFIED) type ABAP_BOOL .
private section.
ENDCLASS.



CLASS ZCL_DEEB_UT_TABLE_UPDATE IMPLEMENTATION.


  METHOD zif_deeb_ut_table_update~get_table_from_json.

    DATA lr_data TYPE REF TO data.
    FIELD-SYMBOLS: <tab> TYPE table.


    TRY.
* --------- create table
        CREATE DATA lr_data TYPE TABLE OF (iv_table).
        IF lr_data IS INITIAL.
          ev_error = |Invalid table name { iv_table }|.
          RETURN.
        ELSE.
          ASSIGN lr_data->* TO <tab>.
        ENDIF.


* ---------- transform
        DATA(lr_json) = zcl_deeb_factory=>create_json_converter( ).

        lr_json->from_json(
          EXPORTING
            iv_json = iv_json
          CHANGING
            ca_data = <tab>
        ).

        rr_table = lr_data.

      CATCH cx_root INTO DATA(lx_exc).
        ev_error = lx_exc->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_deeb_ut_table_update~update.

* --------- local data definition
    FIELD-SYMBOLS: <tab> TYPE table.

* --------- init and check
    IF it_table[] IS INITIAL AND ir_table IS INITIAL.
      ev_message = |no table data given|.
      RETURN.
    ENDIF.

    IF ir_table IS NOT INITIAL.
      mr_table = ir_table.
    ELSE.
      mr_table = REF #(  it_table ).
    ENDIF.

    ASSIGN mr_table->* TO <tab>.
    DATA(lv_lin) = lines( <tab> ).
    IF lv_lin = 0.
      ev_message = |no table data found|.
      RETURN.
    ENDIF.



* ------------ prepare, create workarea and check for fields
    ms_params = is_params.

    CREATE DATA mr_wa LIKE LINE OF <tab>.
    ASSIGN mr_wa->* TO FIELD-SYMBOL(<wa>).
    DATA(lv_special_fields) = abap_false.


* --------- prepare and check timestamp
    DATA(lv_special_timestamp) = abap_false.
    ASSIGN COMPONENT zif_deeb_ws_bl_bsp=>c_table_field_timestamp OF STRUCTURE <wa> TO FIELD-SYMBOL(<lv_timestamp>).
    IF <lv_timestamp> IS ASSIGNED.
      lv_special_fields = abap_true.
      lv_special_timestamp = abap_true.
      IF ms_params-timestamp IS INITIAL.
        GET TIME STAMP FIELD ms_params-timestamp.
      ENDIF.
    ELSE.
      IF ms_params-with_timestamp EQ abap_true.
        ev_message = |table { ms_params-table } has no timestamp field|.
        RETURN.
      ENDIF.
    ENDIF.



* ----------- loop and prepare data
    IF lv_special_fields EQ abap_true
      OR ms_params-duplicates_allowed EQ abap_false.
      LOOP AT <tab> ASSIGNING <wa>.

*       set timestamp
        IF lv_special_timestamp EQ abap_true.
          ASSIGN COMPONENT zif_deeb_ws_bl_bsp=>c_table_field_timestamp OF STRUCTURE <wa> TO <lv_timestamp>.
          <lv_timestamp> = ms_params-timestamp.
        ENDIF.

*       check duplicates
        IF ms_params-duplicates_allowed EQ abap_false
         AND is_record_modified(
          iv_table = CONV string( ms_params-table )
          is_data = <wa>
          iv_check_timestamp = lv_special_timestamp
        ) EQ abap_false.
          DELETE <tab>.
        ENDIF.

      ENDLOOP.
    ENDIF.



* ----------- modify database
    DATA(lv_commit) = abap_false.
    IF ms_params-delete EQ abap_true.
      DELETE FROM (ms_params-table) WHERE (ms_params-where).
      lv_commit = abap_true.
    ENDIF.

    ev_modified_lines = lines( <tab> ).
    IF ev_modified_lines > 0.
      MODIFY (ms_params-table) FROM TABLE <tab>.
      lv_commit = abap_true.
    ENDIF.


    IF lv_commit EQ abap_true.
      COMMIT WORK.
    ENDIF.

    rv_success = abap_true.


  ENDMETHOD.


  METHOD is_record_modified.

    rv_modified = get_util( )->is_table_record_modified(
        iv_table           = CONV string( ms_params-table )
        is_data            = is_data
        iv_check_timestamp = iv_check_timestamp
        iv_where           = ms_params-where
    ).

  ENDMETHOD.


  METHOD zif_deeb_ut_table_update~get_util.
    IF mr_util IS INITIAL.
      mr_util = zcl_deeb_factory=>create_ddic_util( ).
    ENDIF.
    rr_util = mr_util.
  ENDMETHOD.
ENDCLASS.

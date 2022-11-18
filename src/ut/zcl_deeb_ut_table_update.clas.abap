class ZCL_DEEB_UT_TABLE_UPDATE definition
  public
  create public .

public section.

  interfaces ZIF_DEEB_UT_TABLE_UPDATE .
protected section.
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
    DATA lr_data TYPE REF TO data.

* --------- init and check
    IF it_table[] IS INITIAL AND ir_table IS INITIAL.
      ev_message = |no table data found|.
      RETURN.
    ENDIF.

    IF ir_table IS NOT INITIAL.
      lr_data = ir_table.
    ELSE.
      lr_data = REF #(  it_table ).
    ENDIF.

    ASSIGN lr_data->* TO <tab>.
    DATA(lv_lin) = lines( <tab> ).
    DATA(ls_params) = is_params.


* ------------ create workarea and check for fields
    DATA: lr_wa TYPE REF TO data.
    CREATE DATA lr_wa LIKE LINE OF <tab>.
    ASSIGN lr_wa->* TO FIELD-SYMBOL(<wa>).
    DATA(lv_special_fields) = abap_false.


* --------- prepare and check timestamp
    DATA(lv_special_timestamp) = abap_false.
    ASSIGN COMPONENT zif_deeb_ws_bl_bsp=>c_table_field_timestamp OF STRUCTURE <wa> TO FIELD-SYMBOL(<lv_timestamp>).
    IF <lv_timestamp> IS ASSIGNED.
      lv_special_fields = abap_true.
      lv_special_timestamp = abap_true.
      IF ls_params-timestamp IS INITIAL.
        GET TIME STAMP FIELD ls_params-timestamp.
      ENDIF.
    ELSE.
      IF ls_params-with_timestamp EQ abap_true.
        ev_message = |table { ls_params-table } has no timestamp field|.
        RETURN.
      ENDIF.
    ENDIF.



* ----------- loop and prepare data
    IF lv_special_fields EQ abap_true.
      LOOP AT <tab> ASSIGNING <wa>.
*           set timestamp
        IF lv_special_timestamp EQ abap_true.
          ASSIGN COMPONENT zif_deeb_ws_bl_bsp=>c_table_field_timestamp OF STRUCTURE <wa> TO <lv_timestamp>.
          <lv_timestamp> = ls_params-timestamp.
        ENDIF.
      ENDLOOP.
    ENDIF.

* ----------- modify database
    IF ls_params-delete EQ abap_true.
      DELETE FROM (ls_params-table).
    ENDIF.

    MODIFY (ls_params-table) FROM TABLE <tab>.
    COMMIT WORK.
    ev_modified_lines = lv_lin.
    rv_success = abap_true.


  ENDMETHOD.
ENDCLASS.

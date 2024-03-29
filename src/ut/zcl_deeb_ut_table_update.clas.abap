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
  data MR_EXIT type ref to ZIF_DEEB_EXIT_TABLE_UPDATE .
  data MV_EXIT_CHECKED type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.

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
    DATA(lv_special_fields) = abap_false.
    DATA: lv_exit_error TYPE abap_bool.
    DATA: lv_exit_skip TYPE abap_bool.
    DATA: lv_message TYPE string.


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

    IF is_params-exit_class IS NOT INITIAL.
      IF zif_deeb_ut_table_update~init_exit( is_params-exit_class ) EQ abap_false.
        ev_message = |invalid exit class - { is_params-exit_class }|.
        RETURN.
      ELSE.
        lv_special_fields = abap_true.
      ENDIF.
    ENDIF.


* ------------ prepare, create workarea and check for fields
    ms_params = is_params.

    CREATE DATA mr_wa LIKE LINE OF <tab>.
    ASSIGN mr_wa->* TO FIELD-SYMBOL(<wa>).

    DATA(lt_fields) = get_util( )->get_ddic_fields_from_struc( <wa> ).


* --------- prepare and check timestamp
    DATA(lv_special_timestamp) = abap_false.

    IF ms_params-no_check_cell_value = abap_false.
      lv_special_fields = abap_true.
    ENDIF.

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
        DATA(lv_tabix) = sy-tabix.

*       call exit first
        IF mr_exit IS NOT INITIAL.
          TRY.
              IF mr_exit->modify_incoming_record(
                  EXPORTING
                     it_fields = lt_fields
                  IMPORTING
                     ev_skip   = lv_exit_skip
                     ev_message = lv_message
                  CHANGING
                     cs_data   = <wa>
            ) EQ abap_false.
                ev_message = |processing stopped by exit { is_params-exit_class } - { lv_message } - { lv_tabix }/{ lv_lin }|.
                RETURN.
              ELSE.
                IF lv_exit_skip EQ abap_true.
                  DELETE <tab>.
                  CONTINUE.
                ENDIF.
              ENDIF.
            CATCH cx_root INTO DATA(lx_exc).
              lv_message = lx_exc->get_text( ).
              ev_message = |Error calling exit { is_params-exit_class } - { lv_message } - { lv_tabix }/{ lv_lin }|.
              RETURN.
          ENDTRY.

        ENDIF.


*       set timestamp
        IF lv_special_timestamp EQ abap_true.
          ASSIGN COMPONENT zif_deeb_ws_bl_bsp=>c_table_field_timestamp OF STRUCTURE <wa> TO <lv_timestamp>.
          <lv_timestamp> = ms_params-timestamp.
        ENDIF.

*       check empty
        IF ms_params-no_check_cell_value EQ abap_false.
          LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>)
            WHERE datatype = 'CHAR'.
            ASSIGN COMPONENT <ls_field>-fieldname OF STRUCTURE <wa> TO FIELD-SYMBOL(<lv_field>).
            IF <lv_field> IS ASSIGNED AND <lv_field> = 'NaN'.
              CLEAR <lv_field>.
              UNASSIGN <lv_field>.
            ENDIF.
          ENDLOOP.
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


  method ZIF_DEEB_UT_TABLE_UPDATE~GET_EXIT.
    rr_exit = mr_exit.
  endmethod.


  METHOD zif_deeb_ut_table_update~init_exit.
    TRY.
        mr_exit ?= zcl_deeb_factory=>create_instance( iv_type ).
        IF mr_exit IS NOT INITIAL.
          rv_success = abap_true.
        ENDIF.
      CATCH cx_root INTO DATA(lx_exc).
        ev_error = lx_exc->get_text( ).
    ENDTRY.
  ENDMETHOD.


  method ZIF_DEEB_UT_TABLE_UPDATE~IS_EXIT_AVAILABLE.
    if mr_exit is NOT INITIAL.
      rv_exit = abap_true.
    endif.
  endmethod.
ENDCLASS.

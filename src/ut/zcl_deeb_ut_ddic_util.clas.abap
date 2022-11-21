class ZCL_DEEB_UT_DDIC_UTIL definition
  public
  create public .

public section.

  interfaces ZIF_DEEB_UT_DDIC_UTIL .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DEEB_UT_DDIC_UTIL IMPLEMENTATION.


  METHOD zif_deeb_ut_ddic_util~append_where_from_struc_field.

    rt_where = it_current.
    DATA(lv_line) = ||.

    IF iv_combine IS NOT INITIAL
      AND rt_where[] IS NOT INITIAL.
      lv_line = |{ iv_combine } |.
    ENDIF.

    DATA(lv_value) = |{ iv_struc_name }-{ iv_fieldname }|.
    IF iv_mask EQ abap_true.
      lv_value = |@{ lv_value }|.
    ENDIF.

    lv_line = lv_line && |{ iv_fieldname } { iv_operand } { lv_value }|.
    APPEND lv_line TO rt_where.

  ENDMETHOD.


  METHOD zif_deeb_ut_ddic_util~append_where_from_value.
    rt_where = it_current.
    DATA(lv_line) = ||.

    IF iv_combine IS NOT INITIAL
      AND rt_where[] IS NOT INITIAL.
      lv_line = |{ iv_combine } |.
    ENDIF.

    DATA(lv_value) = |{ iv_value }|.
    IF iv_enclose EQ abap_true.
      lv_value = |'{ lv_value }'|.
    ENDIF.

    lv_line = lv_line && |{ iv_fieldname } { iv_operand } { lv_value }|.
    APPEND lv_line TO rt_where.

  ENDMETHOD.


  METHOD zif_deeb_ut_ddic_util~get_ddic_fields_from_struc.
    TRY.
        DATA(lr_struc) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( p_data = is_data ) ).
        lr_struc->get_ddic_field_list(
          EXPORTING
            p_langu                  = iv_lang         " Current language
            p_including_substructres = iv_with_sub       " List Also for Substructures
          RECEIVING
            p_field_list             = rt_fields                 " List of Dictionary Descriptions for the Components
          EXCEPTIONS
            not_found                = 1                " Type could not be found
            no_ddic_type             = 2                " Typ is not a dictionary type
            OTHERS                   = 3
        ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_deeb_ut_ddic_util~get_fields_from_from_struc.

* ------ get fields
    DATA(lt_fields) = zif_deeb_ut_ddic_util~get_ddic_fields_from_struc(
      is_data = is_data
      iv_lang = iv_lang
      iv_with_sub = iv_with_sub
    ).
    IF lt_fields[] IS INITIAL.
      RETURN.
    ENDIF.


* ------ loop and filter
    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
      DATA(lv_filtered) = abap_false.

      IF iv_only_keys EQ abap_true
        AND <ls_field>-keyflag EQ abap_false.
        lv_filtered = abap_true.
      ENDIF.

      IF iv_with_client EQ abap_false
        AND <ls_field>-datatype = 'CLNT'.
        lv_filtered = abap_true.
      ENDIF.

      IF it_ignore[] IS NOT INITIAL
        AND zif_deeb_ut_ddic_util~is_string_in_tab(
          iv_check = CONV string( <ls_field>-fieldname )
          it_data = it_ignore
        ) EQ abap_true.
        lv_filtered = abap_true.
      ENDIF.

      IF iv_wildcard IS NOT INITIAL
        AND NOT <ls_field>-fieldname CP iv_wildcard.
        lv_filtered = abap_true.
      ENDIF.

      IF iv_data_types IS NOT INITIAL
        AND NOT iv_data_types CS <ls_field>-datatype.
        lv_filtered = abap_true.
      ENDIF.


      IF lv_filtered EQ abap_false.
        APPEND <ls_field>-fieldname TO rt_fields.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_deeb_ut_ddic_util~is_string_in_tab.
    CHECK it_data[] IS NOT INITIAL.
    READ TABLE it_data WITH TABLE KEY table_line = iv_check TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      rv_found = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD zif_deeb_ut_ddic_util~is_struc_equal.

    IF is_data1 = is_data2.
      rv_equal = abap_true.
    ELSE.
      IF it_ignore[] IS NOT INITIAL.
        DATA(lt_fields) = zif_deeb_ut_ddic_util~get_fields_from_from_struc(
                            is_data        = is_data1
*                            iv_lang        = SY-LANGU
*                            iv_with_sub    = abap_false
*                            iv_only_keys   = abap_false
*                            iv_with_client = abap_false
                            it_ignore      = it_ignore
*                            iv_data_types  =
*                            iv_wildcard    =
                          ).
        rv_equal = abap_true.
        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<lv_fieldname>).
          ASSIGN COMPONENT <lv_fieldname> OF STRUCTURE is_data1 TO FIELD-SYMBOL(<lv_value1>).
          ASSIGN COMPONENT <lv_fieldname> OF STRUCTURE is_data2 TO FIELD-SYMBOL(<lv_value2>).
          IF <lv_value1> IS ASSIGNED AND <lv_value2> IS ASSIGNED.
            IF <lv_value1> NE <lv_value2>.
              CLEAR rv_equal.
              RETURN.
            ENDIF.
            UNASSIGN: <lv_value1>, <lv_value2>.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD zif_deeb_ut_ddic_util~is_table_record_modified.
* --------- get key fields
    DATA lt_ignore TYPE string_table.
    APPEND zif_deeb_c=>c_table_field_timestamp TO lt_ignore.

    DATA(lt_keys) = zif_deeb_ut_ddic_util~get_fields_from_from_struc(
      EXPORTING
        is_data        = is_data
*      iv_with_sub    = abap_false
        iv_only_keys   = abap_true
*      iv_with_client = abap_false
        it_ignore      = lt_ignore                  " Table of Strings
*      iv_data_types  =
*      iv_wildcard    =
    ).

    IF lt_keys[] IS INITIAL.
      " workaround -
      rv_modified = abap_true.
      RETURN.
    ENDIF.

* -------- prepare data for old
    DATA lr_old TYPE REF TO data.
    FIELD-SYMBOLS: <old> TYPE data.

    CREATE DATA lr_old TYPE (iv_table).
    ASSIGN lr_old->* TO <old>.


* -------- prepare where clause
    DATA lt_where TYPE string_table.

    IF iv_where IS NOT INITIAL.
      APPEND iv_where TO lt_where.
    ENDIF.

    LOOP AT lt_keys ASSIGNING FIELD-SYMBOL(<lv_key_field>).
      lt_where = zif_deeb_ut_ddic_util~append_where_from_struc_field(
                             iv_fieldname  = <lv_key_field>
                             iv_operand    = '='
                             iv_combine    = 'AND'
                             iv_struc_name = 'IS_DATA'
                             iv_mask       = abap_false
                             it_current    = lt_where
                           ).
    ENDLOOP.




* ---------- select without timestamp
    IF iv_check_timestamp EQ abap_false.
      SELECT SINGLE *
        FROM (iv_table)
        INTO CORRESPONDING FIELDS OF <old>
       WHERE (lt_where).

      IF sy-subrc NE 0.
        rv_modified = abap_true.
        RETURN.
      ENDIF.

    ELSE.
* ---------- select with timestamp
      lt_where = zif_deeb_ut_ddic_util~append_where_from_struc_field(
                             iv_fieldname  = zif_deeb_c=>c_table_field_timestamp
                             iv_operand    = '<='
                             iv_combine    = 'AND'
                             iv_struc_name = 'IS_DATA'
                             iv_mask       = abap_false
                             it_current    = lt_where
                           ).
      DATA(lv_orderby) = |{ zif_deeb_c=>c_table_field_timestamp } DESCENDING|.
      SELECT *
        FROM (iv_table)
        INTO CORRESPONDING FIELDS OF <old>
        UP TO 1 ROWS
        WHERE (lt_where)
        ORDER BY (lv_orderby).
      ENDSELECT.

    ENDIF.

* ----------- check old
    IF <old> IS INITIAL.
      rv_modified = abap_true.
      RETURN.
    ELSE.
      IF zif_deeb_ut_ddic_util~is_struc_equal(
           is_data1  = is_data
           is_data2  = <old>
           it_ignore = lt_ignore
         ) EQ abap_false.
        rv_modified = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_deeb_ut_ddic_util~is_boolean_true.
    IF iv_check IS NOT INITIAL
      AND strlen( iv_check ) > 0.
      DATA(lv_string) = CONV string( iv_check ).
      IF lv_string(1) CA 'xXtT1'.
        rv_bool_true = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

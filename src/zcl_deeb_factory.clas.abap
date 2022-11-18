class ZCL_DEEB_FACTORY definition
  public
  create public .

public section.

  interfaces ZIF_DEEB_C .

  class-methods CREATE_JSON_CONVERTER
    returning
      value(RR_INSTANCE) type ref to ZIF_DEEB_UT_JSON_CONVERT .
  class-methods CREATE_TABLE_UPDATER
    returning
      value(RR_INSTANCE) type ref to ZIF_DEEB_UT_TABLE_UPDATE .
  class-methods CREATE_INSTANCE
    importing
      !IV_TYPE type STRING
    exporting
      !EV_ERROR type STRING
    returning
      value(RR_INSTANCE) type ref to OBJECT .
protected section.

  class-data GV_LAST_CREATE_ERROR type STRING .
private section.
ENDCLASS.



CLASS ZCL_DEEB_FACTORY IMPLEMENTATION.


  METHOD create_instance.

    DATA lv_type(30).

    CLEAR gv_last_create_error.
    IF iv_type IS NOT INITIAL.

      lv_type = iv_type.

*     next releases: map to other type per customizing


*     check interface to default implementation (same name except prefix)
      IF lv_type CP 'ZIF*'.
        lv_type+1(2) = 'CL'.
      ENDIF.

*     create object
      TRY.
          CREATE OBJECT rr_instance TYPE (lv_type).
        CATCH cx_root INTO DATA(lx_exc).
          ev_error = lx_exc->get_text( ).
          gv_last_create_error = ev_error.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  method CREATE_JSON_CONVERTER.
    rr_instance ?= create_instance( 'ZIF_DEEB_UT_JSON_CONVERT' ).
  endmethod.


  METHOD create_table_updater.
    rr_instance ?= create_instance( 'ZIF_DEEB_UT_TABLE_UPDATE' ).
  ENDMETHOD.
ENDCLASS.

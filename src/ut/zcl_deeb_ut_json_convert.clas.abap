class ZCL_DEEB_UT_JSON_CONVERT definition
  public
  create public .

public section.

  interfaces ZIF_DEEB_UT_JSON_CONVERT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DEEB_UT_JSON_CONVERT IMPLEMENTATION.


  METHOD zif_deeb_ut_json_convert~from_json.

    cl_fdt_json=>json_to_data(
      EXPORTING
        iv_json = iv_json
      CHANGING
        ca_data = ca_data
    ).

  ENDMETHOD.
ENDCLASS.

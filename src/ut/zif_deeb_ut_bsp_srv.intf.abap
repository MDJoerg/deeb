interface ZIF_DEEB_UT_BSP_SRV
  public .


  data M_BSP_PAGE type ref to CL_BSP_PAGE_BASE .
  data M_BSP_NAVIGATION type ref to CL_BSP_NAVIGATION .

  methods PING .
endinterface.

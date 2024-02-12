CLASS ltcl_domain_factory DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS from_data_element FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_domain_factory IMPLEMENTATION.
  METHOD from_data_element.
    DATA create_user_name TYPE ernam.

    DATA(domain_factory) = zcl_aboc_ddic_domain_factory=>from_data_element( create_user_name ).
    cl_abap_unit_assert=>assert_bound( act = domain_factory ).
  ENDMETHOD.
ENDCLASS.

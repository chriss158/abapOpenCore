CLASS ltcl_domain DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS get_domain_values            FOR TESTING RAISING cx_static_check.
    METHODS get_dom_value_text           FOR TESTING RAISING cx_static_check.
    METHODS get_dom_value_text_as_string FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_domain IMPLEMENTATION.
  METHOD get_domain_values.
    TRY.
        DATA(domain) = NEW zcl_aboc_ddic_domain( 'AUART' ).
        DATA(domain_values) = domain->get_domain_values( ).
      CATCH cx_ddic_type_not_found.

    ENDTRY.

    cl_abap_unit_assert=>assert_not_initial( act = domain_values ).
  ENDMETHOD.

  METHOD get_dom_value_text.
    TRY.
        DATA(domain) = NEW zcl_aboc_ddic_domain( 'AUART' ).
        DATA(domain_value_text) = domain->get_dom_value_text( 'TA' ).
      CATCH cx_ddic_type_not_found.

    ENDTRY.

    cl_abap_unit_assert=>assert_bound( act = domain_value_text ).
  ENDMETHOD.

  METHOD get_dom_value_text_as_string.
    TRY.
        DATA(domain) = NEW zcl_aboc_ddic_domain( 'AUART' ).
        DATA(domain_value_text_as_string) = domain->get_dom_value_text_as_string( 'TA' ).
      CATCH cx_ddic_type_not_found.

    ENDTRY.

    cl_abap_unit_assert=>assert_not_initial( act = domain_value_text_as_string ).
  ENDMETHOD.
ENDCLASS.

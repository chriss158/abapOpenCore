CLASS zcl_aboc_ddic_domain_factory DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS from_data_element IMPORTING data_element  TYPE any
                                    RETURNING VALUE(result) TYPE REF TO zcl_aboc_ddic_domain.
ENDCLASS.


CLASS zcl_aboc_ddic_domain_factory IMPLEMENTATION.
  METHOD from_data_element.
    DATA(data_descr) = cl_abap_datadescr=>describe_by_data( data_element ).
    DATA(ddic_header) = data_descr->get_ddic_header( ).
    result = NEW zcl_aboc_ddic_domain( ddic_header-refname ).
  ENDMETHOD.
ENDCLASS.

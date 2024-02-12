CLASS zcl_aboc_ddic_domain DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor                  IMPORTING domain_name   TYPE domname
                                         RAISING   cx_ddic_type_not_found.

    METHODS get_dom_value_text           IMPORTING value         TYPE any
                                         RETURNING VALUE(result) TYPE REF TO data.

    METHODS get_dom_value_text_as_string IMPORTING value         TYPE any
                                         RETURNING VALUE(result) TYPE string.

    METHODS get_domain_values            RETURNING VALUE(result) TYPE REF TO data.

  PRIVATE SECTION.
    DATA domain_name TYPE domname.

    METHODS get_key_value_names_for_table IMPORTING table           TYPE tabname
                                          EXPORTING key_fieldname   TYPE fieldname
                                                    value_fieldname TYPE fieldname.
ENDCLASS.


CLASS zcl_aboc_ddic_domain IMPLEMENTATION.
  METHOD constructor.
    me->domain_name = domain_name.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA type_descr TYPE REF TO cl_abap_typedescr.
    cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = me->domain_name
                                         RECEIVING  p_descr_ref    = type_descr
                                         EXCEPTIONS type_not_found = 1
                                                    OTHERS         = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW cx_ddic_type_not_found( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_domain_values.
    DATA(components) = VALUE cl_abap_structdescr=>component_table( ).
    DATA struct_desc TYPE REF TO cl_abap_structdescr.
    DATA table_desc  TYPE REF TO cl_abap_tabledescr.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    DATA dd07v TYPE STANDARD TABLE OF dd07v.
    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING  domname        = me->domain_name
                 text           = abap_true
                 langu          = sy-langu
      TABLES     dd07v_tab      = dd07v
      EXCEPTIONS wrong_textflag = 1
                 OTHERS         = 2.
    IF sy-subrc = 0 AND dd07v IS NOT INITIAL.

      APPEND VALUE abap_componentdescr( name = 'VALUE'
                                        type = CAST #( cl_abap_datadescr=>describe_by_name( p_name = 'DOMVALUE_L' ) ) ) TO components.
      APPEND VALUE abap_componentdescr( name = 'TEXT'
                                        type = CAST #( cl_abap_datadescr=>describe_by_name( p_name = 'VAL_TEXT' ) ) ) TO components.

      struct_desc = cl_abap_structdescr=>create( components ).

      table_desc = cl_abap_tabledescr=>create( p_line_type  = struct_desc
                                               p_table_kind = cl_abap_tabledescr=>tablekind_std
                                               p_unique     = abap_false ).

      DATA struct TYPE REF TO data.
      CREATE DATA struct TYPE HANDLE struct_desc.
      CREATE DATA result TYPE HANDLE table_desc.

      ASSIGN struct->* TO FIELD-SYMBOL(<table_struct>).
      ASSIGN result->* TO <table>.

      LOOP AT dd07v ASSIGNING FIELD-SYMBOL(<dd07v>).
        ASSIGN COMPONENT 'VALUE' OF STRUCTURE <table_struct> TO FIELD-SYMBOL(<table_struct_value>).
        ASSIGN COMPONENT 'TEXT' OF STRUCTURE <table_struct> TO FIELD-SYMBOL(<table_struct_text>).

        IF <dd07v>-domvalue_h IS INITIAL.
          <table_struct_value> = <dd07v>-domvalue_l.
        ELSE.
          <table_struct_value> = |{ <dd07v>-domvalue_l } - { <dd07v>-domvalue_h }|.
        ENDIF.

        <table_struct_text> = <dd07v>-ddtext.

        APPEND <table_struct> TO <table>.
      ENDLOOP.
    ELSE.

      SELECT SINGLE * FROM dd01l
        INTO @DATA(dd01l)
        WHERE domname  = @me->domain_name
          AND as4local = 'A'
          AND as4vers  = ''  ##WARN_OK.

      IF sy-subrc = 0.

        DATA texttable TYPE dd08v-tabname.
        CALL FUNCTION 'DDUT_TEXTTABLE_GET'
          ##FM_SUBRC_OK
          EXPORTING  tabname   = dd01l-entitytab
          IMPORTING  texttable = texttable
          EXCEPTIONS OTHERS    = 1.

        IF sy-subrc = 0.
          DATA(selecttable) = COND #( WHEN texttable IS NOT INITIAL AND ( dd01l-entitytab CP 'Z*' OR texttable NP 'Z*' ) THEN texttable ELSE dd01l-entitytab ).

          DATA dd03l TYPE STANDARD TABLE OF dd03l.
          get_key_value_names_for_table( EXPORTING table           = selecttable
                                         IMPORTING key_fieldname   = DATA(key_fieldname)
                                                   value_fieldname = DATA(value_fieldname) ).

          IF key_fieldname IS NOT INITIAL.
            SELECT fieldname, position, rollname
              FROM dd03l
              WHERE tabname   = @selecttable
                AND as4local  = 'A'
                AND as4vers   = ''
                AND keyflag   = ''
                AND fieldname = @value_fieldname
              ORDER BY position
              INTO CORRESPONDING FIELDS OF TABLE @dd03l
              UP TO 1 ROWS.
          ENDIF.
          IF dd03l IS INITIAL.
            SELECT fieldname, position, rollname
              FROM dd03l
              WHERE tabname  = @selecttable
                AND as4local = 'A'
                AND as4vers  = ''
                AND keyflag  = ''
              ORDER BY position
              INTO CORRESPONDING FIELDS OF TABLE @dd03l
              UP TO 1 ROWS.
          ENDIF.
          IF sy-subrc = 0.
            DATA(position) = dd03l[ 1 ]-position.
            DATA(textfield) = dd03l[ 1 ]-fieldname.
            DATA(textrollname) = dd03l[ 1 ]-rollname.
            IF key_fieldname IS INITIAL OR value_fieldname IS INITIAL.
              position -= 1.
              SELECT SINGLE fieldname, rollname
                INTO ( @DATA(keyfield), @DATA(keyrollname) )
                FROM dd03l
                WHERE tabname  = @selecttable
                  AND as4local = 'A'
                  AND as4vers  = ''
                  AND position = @position  ##WARN_OK.
            ELSE.
              SELECT SINGLE fieldname, rollname
                INTO ( @keyfield, @keyrollname )
                FROM dd03l
                WHERE tabname   = @selecttable
                  AND as4local  = 'A'
                  AND as4vers   = ''
                  AND fieldname = @key_fieldname.
            ENDIF.

            APPEND VALUE abap_componentdescr( name = 'VALUE'
                                              type = CAST #( cl_abap_datadescr=>describe_by_name( p_name = keyrollname ) ) ) TO components.
            APPEND VALUE abap_componentdescr( name = 'TEXT'
                                              type = CAST #( cl_abap_datadescr=>describe_by_name( p_name = textrollname ) ) ) TO components.

            struct_desc = cl_abap_structdescr=>create( components ).

            table_desc = cl_abap_tabledescr=>create( p_line_type  = struct_desc
                                                     p_table_kind = cl_abap_tabledescr=>tablekind_std
                                                     p_unique     = abap_false ).

            CREATE DATA result TYPE HANDLE table_desc.

            ASSIGN result->* TO <table>.

            DATA(fields) = |{ keyfield }, { textfield }|.
            TRY.
                SELECT (fields) INTO TABLE @<table>
                  FROM (selecttable)
                  WHERE spras = @sy-langu.
              CATCH cx_sy_dynamic_osql_semantics INTO DATA(error).
            ENDTRY.

            IF error IS BOUND.
              TRY.
                  SELECT (fields) INTO TABLE @<table>
                    FROM (selecttable).
                CATCH cx_sy_dynamic_osql_semantics INTO error.
              ENDTRY.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_key_value_names_for_table.
    CASE table.
      WHEN 'KNA1'.
        key_fieldname = 'KUNNR'.
        value_fieldname = 'NAME1'.
    ENDCASE.
  ENDMETHOD.

  METHOD get_dom_value_text.
    DATA(domain_values) = get_domain_values( ).

    IF domain_values IS NOT BOUND.
      RETURN.
    ENDIF.

    FIELD-SYMBOLS <domain_values> TYPE STANDARD TABLE.
    ASSIGN domain_values->* TO <domain_values>.

    IF <domain_values> IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT <domain_values> ASSIGNING FIELD-SYMBOL(<dom_value>).
      ASSIGN COMPONENT 'VALUE' OF STRUCTURE <dom_value> TO FIELD-SYMBOL(<value>).
      ASSIGN COMPONENT 'TEXT' OF STRUCTURE <dom_value> TO FIELD-SYMBOL(<text>).

      IF <value> = value.
        DATA text_descr TYPE REF TO cl_abap_datadescr.
        text_descr ?= cl_abap_datadescr=>describe_by_data( <text> ).

        CREATE DATA result TYPE HANDLE text_descr.

        ASSIGN result->* TO FIELD-SYMBOL(<result_text>).

        <result_text> = <text>.
        RETURN.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD get_dom_value_text_as_string.
    DATA(domain_value_text_ref) = get_dom_value_text( value ).
    ASSIGN domain_value_text_ref->* TO FIELD-SYMBOL(<domain_value>).
    IF sy-subrc = 0.
      result = <domain_value>.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

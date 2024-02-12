*&---------------------------------------------------------------------*
*& Report zsd_analyze_exec_log
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaboc_analyze_exec_log LINE-SIZE 220.

DATA zaboc_exec_log TYPE zaboc_exec_log.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
  SELECT-OPTIONS so_tcode FOR zaboc_exec_log-tcode.
  SELECT-OPTIONS so_prog FOR zaboc_exec_log-exec_program.
  SELECT-OPTIONS so_date FOR zaboc_exec_log-last_execution_date.
  SELECT-OPTIONS so_time FOR zaboc_exec_log-last_execution_time.
  SELECT-OPTIONS so_numex FOR zaboc_exec_log-number_executions.
SELECTION-SCREEN END OF BLOCK b01.

CLASS main_report DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.

    "! Entry point
    "! @parameter tcode_so | Corresponding importing parameter for the selection parameter
    "! @parameter prog_so |
    "! @parameter date_so |
    "! @parameter time_so |
    "! @parameter num_exec_so |
    CLASS-METHODS start IMPORTING tcode_so    TYPE STANDARD TABLE
                                  prog_so     TYPE STANDARD TABLE
                                  date_so     TYPE STANDARD TABLE
                                  time_so     TYPE STANDARD TABLE
                                  num_exec_so TYPE STANDARD TABLE.

  PRIVATE SECTION.
    CLASS-DATA main TYPE REF TO main_report.

    DATA output_data TYPE zaboc_t_exec_log.

    "! Select data
    "!
    "! @parameter tcode_so |
    "! @parameter prog_so |
    "! @parameter date_so |
    "! @parameter time_so |
    "! @parameter num_exec_so |
    METHODS select_data IMPORTING tcode_so    TYPE STANDARD TABLE
                                  prog_so     TYPE STANDARD TABLE
                                  date_so     TYPE STANDARD TABLE
                                  time_so     TYPE STANDARD TABLE
                                  num_exec_so TYPE STANDARD TABLE.

    "! Display the data as ALV
    METHODS display.

    METHODS set_column_text    IMPORTING salv        TYPE REF TO cl_salv_table
                                         column_name TYPE lvc_fname
                                         text        TYPE string.

    METHODS set_column_visible IMPORTING salv        TYPE REF TO cl_salv_table
                                         column_name TYPE lvc_fname
                                         visible     TYPE abap_bool.
ENDCLASS.


CLASS main_report IMPLEMENTATION.
  METHOD class_constructor.
    main = NEW main_report( ).
  ENDMETHOD.

  METHOD start.
    main->select_data( tcode_so    = tcode_so
                       prog_so     = prog_so
                       date_so     = date_so
                       time_so     = time_so
                       num_exec_so = num_exec_so ).

    main->display( ).
  ENDMETHOD.

  METHOD select_data.
    " TODO: variable is assigned but never used (ABAP cleaner)
    output_data = zcl_wld_exec_logger=>get_exec_log( tcode_so               = tcode_so
                                                     prog_so                = prog_so
                                                     last_execution_date_so = date_so
                                                     last_execution_time_so = time_so
                                                     number_executions_so   = num_exec_so ).
  ENDMETHOD.

  METHOD display.
    DATA salv TYPE REF TO cl_salv_table.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = salv
                                CHANGING  t_table      = output_data ).
      CATCH cx_salv_msg INTO DATA(salv_msg_error).
        MESSAGE ID salv_msg_error->msgid TYPE salv_msg_error->msgty NUMBER salv_msg_error->msgno
                WITH salv_msg_error->msgv1 salv_msg_error->msgv2 salv_msg_error->msgv3 salv_msg_error->msgv4.
    ENDTRY.

    salv->get_columns( )->set_optimize( abap_true ).
    salv->get_functions( )->set_all( abap_true ).

    set_column_text( salv        = salv
                     column_name = 'LAST_EXECUTION_TIME'
                     text        = CONV string( 'Letze Ausführungszeit'(t01) ) ).
    set_column_text( salv        = salv
                     column_name = 'LAST_EXECUTION_DATE'
                     text        = CONV string( 'Letzes Ausführungsdatum'(t02) ) ).
    set_column_text( salv        = salv
                     column_name = 'EXECUTION_DATA'
                     text        = CONV string( TEXT-h04 ) ).

    set_column_visible( salv        = salv
                        column_name = 'MANDT'
                        visible     = abap_false ).

    salv->display( ).
  ENDMETHOD.

  METHOD set_column_text.
    TRY.
        salv->get_columns( )->get_column( column_name )->set_long_text( CONV scrtext_l( text ) ).
        salv->get_columns( )->get_column( column_name )->set_medium_text( CONV SCRTEXT_m( text ) ).
        salv->get_columns( )->get_column( column_name )->set_short_text( CONV SCRTEXT_s( text ) ).
      CATCH cx_salv_not_found ##NO_HANDLER.
        " Nothing todo. Ignore error
    ENDTRY.
  ENDMETHOD.

  METHOD set_column_visible.
    TRY.
        salv->get_columns( )->get_column( column_name )->set_visible( visible ).
      CATCH cx_salv_not_found ##NO_HANDLER.
        " Nothing todo. Ignore error
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  main_report=>start( tcode_so    = so_tcode[]
                      prog_so     = so_prog[]
                      date_so     = so_date[]
                      time_so     = so_time[]
                      num_exec_so = so_numex[] ).

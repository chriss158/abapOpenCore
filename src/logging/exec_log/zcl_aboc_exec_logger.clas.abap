CLASS zcl_aboc_exec_logger DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS log          IMPORTING tcode                  TYPE tcode          DEFAULT sy-tcode
                                         program                TYPE programm       DEFAULT sy-cprog
                                         execution_data         TYPE char02         OPTIONAL.

    CLASS-METHODS get_exec_log IMPORTING tcode_so               TYPE STANDARD TABLE OPTIONAL
                                         prog_so                TYPE STANDARD TABLE OPTIONAL
                                         last_execution_date_so TYPE STANDARD TABLE OPTIONAL
                                         last_execution_time_so TYPE STANDARD TABLE OPTIONAL
                                         number_executions_so   TYPE STANDARD TABLE OPTIONAL
                               RETURNING VALUE(result)          TYPE zaboc_t_exec_log.

ENDCLASS.


CLASS zcl_aboc_exec_logger IMPLEMENTATION.
  METHOD log.
    SELECT SINGLE number_executions
      FROM zaboc_exec_log
      INTO @DATA(number_executions)
      WHERE tcode          = @tcode
        AND exec_program   = @program
        AND execution_data = @execution_data.

    number_executions += 1.

    GET TIME.

    MODIFY zaboc_exec_log FROM @( VALUE #( tcode               = tcode
                                          exec_program        = program
                                          execution_data      = execution_data
                                          last_execution_date = sy-datum
                                          last_execution_time = sy-uzeit
                                          number_executions   = number_executions ) ).
    COMMIT WORK AND WAIT.
  ENDMETHOD.

  METHOD get_exec_log.
    SELECT * FROM zaboc_exec_log
      INTO TABLE result
      WHERE exec_program        IN prog_so
        AND tcode               IN tcode_so
        AND last_execution_date IN last_execution_date_so
        AND last_execution_time IN last_execution_time_so
        AND number_executions   IN number_executions_so.
  ENDMETHOD.
ENDCLASS.

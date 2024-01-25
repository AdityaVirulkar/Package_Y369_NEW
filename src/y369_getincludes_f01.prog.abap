*&---------------------------------------------------------------------*
*&  Include           Y369_GETINCLUDES_F01                             *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  f_get_filename
*&---------------------------------------------------------------------*
*       Select file path to download output to
*----------------------------------------------------------------------*
FORM f_get_filename .
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = p_fpath2.
ENDFORM.                    " f_get_filename

*&--------------------------------------------------------------------*
*&      Form  f_check_namspc
*&--------------------------------------------------------------------*
*       Check if Customer Namespace is valid
*---------------------------------------------------------------------*
FORM f_check_namspc.

* Begin of Change by <Vandana Sharma> on <23-Feb-2009> <AB1K900402>
  DATA: l_cust_len TYPE i.
  IF NOT s_namspc[] IS INITIAL.
    LOOP AT s_namspc.
      CLEAR l_cust_len.
      l_cust_len = STRLEN( s_namspc-low ) - 1.
      IF s_namspc-low(1) NE '/' OR s_namspc-low+l_cust_len(1) NE '/'.
* Begin of change by <Saurabh Luthra> <AB1K900398>
*        MESSAGE e000 WITH 'Not a valid namespace.'.
        MESSAGE e000 WITH text-101.
* End of change by <Saurabh Luthra> <AB1K900398>
      ENDIF.
    ENDLOOP.
  ENDIF.
* End of Change by <Vandana Sharma> on <23-Feb-2009> <AB1K900402>
ENDFORM.                    "f_check_namspc

*&--------------------------------------------------------------------*
*&      Form  f_edit_namespace_string
*&--------------------------------------------------------------------*
*       Append '%' to customer namespaces for future data selections
*---------------------------------------------------------------------*
FORM f_edit_namespace_string.
  LOOP AT s_namspc.
    CONCATENATE s_namspc-low '%' INTO s_namspc-low.
    MODIFY s_namspc.
  ENDLOOP.
ENDFORM.                    "f_edit_namespace_string

*&---------------------------------------------------------------------*
*&      Form  f_get_prog
*&---------------------------------------------------------------------*
*      Get the Program names
*----------------------------------------------------------------------*
FORM f_get_prog .

  SELECT obj_name
          FROM tadir
          INTO TABLE t_prog
          WHERE object EQ 'FUGR' AND
               ( obj_name LIKE 'Z%' OR
                 obj_name LIKE 'Y%' ) AND
                 author NE 'SAP'.
* Begin of change by <Saurabh Luthra> <AB1K900398>
* Begin of Change by <Vandana Sharma> on <23-Feb-2009> <AB1K900402>
  IF NOT s_namspc[] IS INITIAL.
    LOOP AT s_namspc.
      SELECT obj_name FROM tadir
        APPENDING TABLE t_prog
        WHERE object EQ 'FUGR'
        AND   obj_name LIKE s_namspc-low
        AND   author NE 'SAP'.
    ENDLOOP.
  ENDIF.
* End of change by <Saurabh Luthra> <AB1K900398>
* End of Change by <Vandana Sharma> on <23-Feb-2009> <AB1K900402>

  LOOP AT t_prog.
    CONCATENATE 'SAPL' t_prog-prog INTO t_prog-prog.
    MODIFY t_prog.
  ENDLOOP.

  SELECT obj_name
       FROM   tadir
       APPENDING TABLE t_prog
       WHERE object EQ 'PROG' AND
             ( obj_name LIKE 'Z%' OR
               obj_name LIKE 'Y%' OR
               obj_name LIKE 'SAPMZ%' OR
               obj_name LIKE 'SAPMY%' OR
               obj_name LIKE 'MZ%' OR
               obj_name LIKE 'MY%' OR
               obj_name LIKE 'SAPDZ%' OR
               obj_name LIKE 'SAPDY%' OR
               obj_name LIKE 'DY%' OR
               obj_name LIKE 'DZ%' OR
               obj_name LIKE 'MP9%' OR
               obj_name LIKE 'SAPFZ%' OR
               obj_name LIKE 'SAPFY%' OR
               obj_name LIKE 'FY%' OR
               obj_name LIKE 'FZ%' OR
               obj_name LIKE 'SAPUZ%' OR
               obj_name LIKE 'SAPUY%' OR
               obj_name LIKE 'UZ%' OR
               obj_name LIKE 'UY%' )
                AND  author NE 'SAP'.

* Begin of Change by <Vandana Sharma> on <23-Feb-2009> <AB1K900402>
  IF NOT s_namspc[] IS INITIAL.
    LOOP AT s_namspc.
* Begin of change by <Saurabh Luthra> <AB1K900398>
*      SELECT obj_name FROM tadir
*        APPENDING TABLE t_prog
*        WHERE obj_name LIKE s_namspc-low.
      SELECT obj_name FROM tadir
        APPENDING TABLE t_prog
        WHERE object EQ 'PROG'
        AND   obj_name LIKE s_namspc-low
        AND   author NE 'SAP'.
* End of change by <Saurabh Luthra> <AB1K900398>
    ENDLOOP.
  ENDIF.
* End of Change by <Vandana Sharma> on <23-Feb-2009> <AB1K900402>

ENDFORM.                    " f_get_prog

*&---------------------------------------------------------------------*
*&      Form  f_get_incl
*&---------------------------------------------------------------------*
*       Get the Includes
*----------------------------------------------------------------------*
FORM f_get_incl .

  LOOP AT t_prog.
* Show the gui status bar to avoid short dumps
    MOVE sy-tabix TO l_tabix.
    CONCATENATE 'Processing done for'
                  l_tabix
                  'Programs'
                  INTO l_text
                  SEPARATED BY space.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text   = l_text
      EXCEPTIONS
        OTHERS = 1.
    IF sy-batch = 'X'.
      MESSAGE i000 WITH l_text.
    ENDIF.

    REFRESH: t_inc.
    CALL FUNCTION 'GET_INCLUDETAB'
      EXPORTING
        progname = t_prog-prog
      TABLES
        incltab  = t_inc.
*    IF sy-subrc = 0.
    LOOP AT t_inc INTO wa_inc.
      wa_final-prg = t_prog-prog.
      wa_final-include = wa_inc-name.
      APPEND wa_final TO t_final.
    ENDLOOP.
*    ENDIF.
  ENDLOOP.

ENDFORM.                    " f_get_incl

*&---------------------------------------------------------------------*
*&      Form  f_download_results
*&---------------------------------------------------------------------*
*       download the results to flat file
*----------------------------------------------------------------------*

FORM f_download_results USING p_p_fpath2 TYPE any.

  CALL FUNCTION 'WS_DOWNLOAD'
    EXPORTING
      filename                = p_p_fpath2
      filetype                = 'ASC'
    TABLES
      data_tab                = t_final
    EXCEPTIONS
      file_open_error         = 1
      file_write_error        = 2
      invalid_filesize        = 3
      invalid_type            = 4
      no_batch                = 5
      unknown_error           = 6
      invalid_table_width     = 7
      gui_refuse_filetransfer = 8
      customer_error          = 9
      no_authority            = 10
      OTHERS                  = 11.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " f_download_results

*&---------------------------------------------------------------------*
*&      Form  f_display_report
*&---------------------------------------------------------------------*
*       Display the report with a message
*----------------------------------------------------------------------*
FORM f_display_report .
  SKIP.
  WRITE :/03 'File downloaded to path:',
          30 p_fpath2.
ENDFORM.                    " f_display_report

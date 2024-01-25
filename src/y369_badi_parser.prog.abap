*&---------------------------------------------------------------------*
*& Report  Y369_BADI_PARSER                                            *
*&                                                                     *
*&---------------------------------------------------------------------*
*& Prepared by Ajitabh for upgrade                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  y369_badi_parser LINE-SIZE 100.

TYPE-POOLS : slis.

*----------------------------------------------------------------------*
TABLES:
*----------------------------------------------------------------------*
  tstc.      "SAP Transaction Codes

*----------------------------------------------------------------------*
TYPES:
*----------------------------------------------------------------------*
  BEGIN OF t_tadir,
    pgmid    LIKE tadir-pgmid,    "Program ID in Requests and Tasks
    object   LIKE tadir-object,   "Object Name in Object Directory
    obj_name LIKE tadir-obj_name, "Object Name in Object Directory
    devclass TYPE tadir-devclass,
    text TYPE sxs_attrt-text,     "Badi text
    imp_name TYPE sxc_exit-imp_name,       "Implementation
  END OF t_tadir,


  BEGIN OF t_tstc,
    tcode TYPE tstc-tcode,
    pgmna TYPE tstc-pgmna,
    ttext TYPE tstct-ttext,
    devclass TYPE tadir-devclass,
  END OF t_tstc,

******** For Badi's ************************
  BEGIN OF t_final_badi,
     tcode TYPE tstc-tcode,          "TCODE
     ttext TYPE tstct-ttext,         "TCODE text
     obj_name TYPE tadir-obj_name,   "Badi Name
     text TYPE sxs_attrt-text,       "Badi text
     imp_name TYPE sxc_exit-imp_name,"Implementation
  END OF t_final_badi.

*----------------------------------------------------------------------*
data:
*----------------------------------------------------------------------*
  i_badi        TYPE STANDARD TABLE OF t_tadir WITH header line,
  wa1_badi      TYPE t_tadir,
  i_badi_final  TYPE STANDARD TABLE OF t_tadir WITH header line,
  i_tstc TYPE TABLE OF t_tstc WITH header line,
  i_final_badi TYPE TABLE OF t_final_badi WITH header line,
  wa_final_badi TYPE t_final_badi,
  v_devclass    LIKE tadir-devclass,
  v_text(60)    TYPE c,
  v_ttext(36)   TYPE c,

***** For ALV output********
  t_fieldcat       TYPE slis_t_fieldcat_alv,
  l_repid          LIKE sy-repid,
  wa_fieldcat     TYPE slis_fieldcat_alv,
  t_layout         TYPE slis_layout_alv,
  w_callback_subroutine TYPE slis_formname,

    BEGIN OF t_final_alv_badi occurs 0,
     tcode LIKE tstc-tcode,          "TCODE
     ttext LIKE tstct-ttext,         "TCODE text
     obj_name LIKE tadir-obj_name,   "Badi Name
     text LIKE sxs_attrt-text,       "Badi text
     imp_name LIKE sxc_exit-imp_name,"Implementation
  END OF t_final_alv_badi.


CONSTANTS: c_x       TYPE c VALUE 'X',
           c_r3tr(4) TYPE c VALUE 'R3TR',
           c_prog(4) TYPE c VALUE 'PROG',
           c_sxsd(4) TYPE c VALUE 'SXSD',
           c_user_command(12) TYPE c VALUE 'USER COMMAND'.

************************************************************************
*SELECTION SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS:
  s_tcode FOR tstc-tcode .
SELECTION-SCREEN END OF BLOCK b1.

************************************************************************
START-OF-SELECTION.
************************************************************************

* Find TCODE Program Name
  SELECT  *
    FROM tstc INTO CORRESPONDING FIELDS OF TABLE i_tstc
   WHERE ( tcode IN s_tcode ).

* Getting the description for the TCODE
  LOOP AT i_tstc.
    SELECT SINGLE ttext FROM tstct INTO v_ttext
   WHERE ( sprsl = sy-langu ) AND
         ( tcode = i_tstc-tcode  ).

    i_tstc-ttext = v_ttext.
    MODIFY i_tstc.
  ENDLOOP.

  CLEAR i_tstc.

* Select Development Class
  LOOP AT i_tstc.
    CLEAR v_devclass.
    SELECT SINGLE devclass
      FROM tadir INTO (v_devclass)
     WHERE ( pgmid    = c_r3tr     ) AND
           ( object   = c_prog     ) AND
           ( obj_name = i_tstc-pgmna ).

    i_tstc-devclass = v_devclass.
    MODIFY i_tstc.
  ENDLOOP.

  l_repid = sy-repid.
  w_callback_subroutine = c_user_command.


*Code for badi
  PERFORM code_for_badi.
* Populate fieldcat for BADI's
  PERFORM sub_populate_field_cat.
*Modify fieldcat for BADI's
  PERFORM sub_modify_field_cat.
*ALV output for the BADI's
  PERFORM display_alv_report.


*&---------------------------------------------------------------------*
*&      Form  sub_populate_field_cat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_populate_field_cat.

  CONSTANTS: c_tabnam_for_alv_fm TYPE slis_tabname VALUE
  'T_FINAL_ALV_BADI'.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = l_repid
      i_internal_tabname     = c_tabnam_for_alv_fm
      i_inclname             = l_repid
    CHANGING
      ct_fieldcat            = t_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " sub_populate_field_cat

*&---------------------------------------------------------------------*
*&      Form  sub_modify_field_cat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_modify_field_cat .

  LOOP AT t_fieldcat INTO wa_fieldcat.

    CASE wa_fieldcat-fieldname.


      WHEN 'TCODE'.

        wa_fieldcat-seltext_l = 'TRANSACTION CODE'.
        wa_fieldcat-seltext_m = 'TRX CODE'.
        wa_fieldcat-seltext_s = 'TCODE'.
        wa_fieldcat-outputlen =  20.

      WHEN 'TTEXT'.

        wa_fieldcat-seltext_l = 'TRANSATION DESCRP'.
        wa_fieldcat-seltext_m = 'TRX DESCRP'.
        wa_fieldcat-seltext_s = 'TCODE DESC'.
        wa_fieldcat-outputlen =  20.

      WHEN 'OBJ_NAME'.

        wa_fieldcat-seltext_l = 'BADI NAME'.
        wa_fieldcat-seltext_m = 'BADI NAM'.
        wa_fieldcat-seltext_s = 'BADI NA'.
        wa_fieldcat-outputlen =  25.
        wa_fieldcat-hotspot   = 'X'.

      WHEN 'TEXT'.

        wa_fieldcat-seltext_l = 'BADI DESC'.
        wa_fieldcat-seltext_m = 'BADI DESC'.
        wa_fieldcat-seltext_s = 'BADI DESC'.
        wa_fieldcat-outputlen =  30.

      WHEN 'IMP_NAME'.

        wa_fieldcat-seltext_l = 'IMPLEMENTATION NAME'.
        wa_fieldcat-seltext_m = 'IMP NAME'.
        wa_fieldcat-seltext_s = 'IMP NAM'.
        wa_fieldcat-outputlen =  15.

    ENDCASE.
    MODIFY t_fieldcat FROM wa_fieldcat INDEX sy-tabix.

  ENDLOOP.
ENDFORM.                    " sub_modify_field_cat

*&---------------------------------------------------------------------*
*&      Form  display_alv_report
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv_report .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = l_repid
      i_callback_user_command = w_callback_subroutine
*      i_callback_top_of_page = 'TOP-OF-PAGE'
      i_grid_title           = text-001
      is_layout              = t_layout
      it_fieldcat            = t_fieldcat
      i_save                 = c_x
    TABLES
      t_outtab               = i_final_badi
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " display_alv_report


*&---------------------------------------------------------------------*
*&      Form  code_for_badi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM code_for_badi .

* Select BADI entries
  SELECT pgmid
         object
         obj_name
         devclass
    FROM tadir
    INTO CORRESPONDING FIELDS OF TABLE i_badi
    FOR ALL ENTRIES IN i_tstc
   WHERE ( pgmid    = c_r3tr     ) AND
         ( object   = c_sxsd     ) AND
         ( devclass = i_tstc-devclass ).

  LOOP AT i_badi.
    CLEAR v_text.
    SELECT SINGLE text
      FROM sxs_attrt
      INTO v_text
     WHERE ( sprsl      = sy-langu        ) AND
           ( exit_name  = i_badi-obj_name ).

    i_badi-text = v_text.
    MODIFY i_badi.
  ENDLOOP.


  LOOP AT i_badi INTO wa1_badi.
    SELECT imp_name FROM sxc_exit
     INTO CORRESPONDING FIELDS OF wa1_badi
      WHERE ( exit_name = wa1_badi-obj_name ).

      APPEND wa1_badi TO i_badi_final.
    ENDSELECT.
  ENDLOOP.


  LOOP AT i_badi_final.
    READ TABLE i_tstc WITH KEY devclass = i_badi_final-devclass.

    wa_final_badi-tcode     = i_tstc-tcode.
    wa_final_badi-ttext     = i_tstc-ttext.
    wa_final_badi-obj_name  = i_badi_final-obj_name.
    wa_final_badi-text      = i_badi_final-text.
    wa_final_badi-imp_name  = i_badi_final-imp_name.

    APPEND wa_final_badi TO i_final_badi.
  ENDLOOP.

ENDFORM.                    " code_for_badi

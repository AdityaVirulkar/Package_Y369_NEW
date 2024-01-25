*&---------------------------------------------------------------------*
*&  Include           Y369_UCCHECKER_F01                               *
*&---------------------------------------------------------------------*

*&--------------------------------------------------------------------*
*&      Form  f_check_namspc
*&--------------------------------------------------------------------*
*       Check if Customer Namespace is valid
*---------------------------------------------------------------------*
FORM f_check_namspc.

  DATA: l_cust_len TYPE i.
  IF NOT s_namspc[] IS INITIAL.
    LOOP AT s_namspc.
      CLEAR l_cust_len.
      l_cust_len = STRLEN( s_namspc-low ) - 1.
      IF s_namspc-low(1) NE '/' OR s_namspc-low+l_cust_len(1) NE '/'.
        MESSAGE e001 WITH text-101.
      ENDIF.
    ENDLOOP.
  ENDIF.
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

  IF NOT s_namspc[] IS INITIAL.
    LOOP AT s_namspc.
      SELECT obj_name AS low
        FROM tadir
        APPENDING CORRESPONDING FIELDS OF TABLE s_progr
        WHERE object EQ 'PROG'
        AND   obj_name LIKE s_namspc-low
        AND   author NE 'SAP'.
    ENDLOOP.
  ENDIF.
ENDFORM.                    "f_edit_namespace_string

*&---------------------------------------------------------------------*
*&      Form  edit_prog_name
*&---------------------------------------------------------------------*
*       Edit Progname
*----------------------------------------------------------------------*
FORM edit_prog_name .

  LOOP AT s_progr.
    CONCATENATE 'I' 'EQ' s_progr-low INTO s_progr.
    MODIFY s_progr.
  ENDLOOP.

ENDFORM.                    " edit_prog_name

*&---------------------------------------------------------------------
*
*&      Form  selezione_programmi
*&---------------------------------------------------------------------

FORM selezione_programmi.

  DATA: va_devclass LIKE tadir-devclass,                    "RF21032001
         va_pgmid LIKE tadir-pgmid,                         "RF21032001
         va_object LIKE tadir-object,                       "RF21032001
         va_obj_name LIKE tadir-obj_name.                   "RF21032001

  DATA: BEGIN OF i_tadir_tb OCCURS   0,
          tabname  LIKE dd02l-tabname,
        END OF i_tadir_tb.

  DATA: BEGIN OF i_tadir_dt OCCURS   0,
          rollname  LIKE dd04l-rollname,
        END OF i_tadir_dt.

  DATA: BEGIN OF i_tadir_dm OCCURS   0,
          domname  LIKE dd01l-domname,
        END OF i_tadir_dm.

  DATA: fg_tfdir.

  PERFORM include_to_be_excluded USING: '<SCREEN>',
                                        'DB__SSEL',
                                        '%_C*',
                                        '<ICON>',
                                        '>ICON<',
                                        '<SYMBOL>',
                                        '>SYMBOL<'.

* Begin of Change <Saurabh Luthra> <AB1K900522>
*  SELECT *  FROM   trdir INTO TABLE i_trdir
*           WHERE name       IN  s_progr
*           AND   cnam       IN  s_cnam
*           AND   cdat       IN  s_cdat
*           AND   unam       IN  s_unam
*           AND   udat       IN  s_udat
*           AND   subc       IN  s_subc.

  REFRESH i_prog.
  CLEAR: s_progr, wa_prog.

  LOOP AT s_progr.
    wa_prog-prog = s_progr-low.
    APPEND wa_prog TO i_prog.
    CLEAR: s_progr, wa_prog.
  ENDLOOP.

  IF NOT i_prog[] IS INITIAL.

    SELECT name subc
    FROM trdir
    INTO TABLE i_trdir
    FOR ALL ENTRIES IN i_prog
    WHERE name  = i_prog-prog
      AND cnam IN  s_cnam
      AND cdat IN  s_cdat
      AND unam IN  s_unam
      AND udat IN  s_udat
      AND subc IN  s_subc.
  ENDIF.
* End of Change <Saurabh Luthra> <AB1K900522>

  CLEAR tfdir.
  SELECT SINGLE * FROM  tfdir
         WHERE  funcname  = 'TR_TRANSFORM_TRDIR_TO_TADIR'.
  IF sy-subrc = 0. fg_tfdir = 'X'. ENDIF.

  DELETE i_trdir WHERE subc = 'T'.

  LOOP AT i_trdir.

    IF NOT fg_tfdir IS INITIAL.

* Selezione dei prog in base alla classe di sviluppo.
      CALL FUNCTION 'TR_TRANSFORM_TRDIR_TO_TADIR'
        EXPORTING
          iv_trdir_name       = i_trdir-name
        IMPORTING
          es_tadir_keys       = i_tadir
        EXCEPTIONS
          invalid_name_syntax = 1
          OTHERS              = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ELSE.
      i_tadir-pgmid     = 'R3TR'.
      i_tadir-object    = 'PROG'.
      i_tadir-obj_name  = i_trdir-name.
    ENDIF.

    CLEAR tadir.
    SELECT SINGLE * FROM  tadir                      "#EC CI_SEL_NESTED
           WHERE  pgmid     = i_tadir-pgmid
           AND    object    = i_tadir-object
           AND    obj_name  = i_tadir-obj_name.

    IF NOT  tadir-devclass  IN s_devcla.
      DELETE i_trdir.
      CONTINUE.
    ELSE.                                                   "RF21032001
      i_trdir-devclass = tadir-devclass.                    "RF21032001
    ENDIF.

    CLEAR: i_tot, i_include.
    IF i_trdir-subc <> 'I'. ADD 1 TO vn_cont_tot. ENDIF.

    IF i_trdir-subc = '1'
       OR i_trdir-subc = 'M'
       OR i_trdir-subc = 'F'
       OR i_trdir-subc = 'S' .

* Determino include dei programmi selezionati
      CLEAR: i_appoggio.
      REFRESH: i_appoggio.
      CALL FUNCTION 'GET_INCLUDES'
        EXPORTING
          progname = i_trdir-name
        TABLES
          incltab  = i_appoggio.

      DELETE i_appoggio WHERE incl IN r_incl.
* modifica doit
      DELETE i_appoggio WHERE incl CS '='.
      DELETE i_appoggio WHERE incl CA '<>'.
* fine modifica doit
      MOVE-CORRESPONDING i_trdir TO i_tot.
      i_tot-masterlang = tadir-masterlang.
      APPEND i_tot.
      LOOP  AT i_appoggio.
        CLEAR trdir.
        READ TABLE i_trdir WITH KEY name = i_appoggio-incl.
        IF sy-subrc <> 0.
          SELECT SINGLE * FROM trdir                 "#EC CI_SEL_NESTED
                 WHERE  name     = i_appoggio.
          CHECK sy-subrc = 0.
          CLEAR: va_devclass,                               "RF21032001
                 va_pgmid, va_object, va_obj_name.          "RF21032001
          va_pgmid = 'R3TR'.                                "RF21032001
          va_object    = 'PROG'.                            "RF21032001
          va_obj_name  = trdir-name.                        "RF21032001
          CLEAR tadir.                                      "RF21032001
          SELECT SINGLE * FROM  tadir                       "RF21032001
                                                     "#EC CI_SEL_NESTED
            WHERE  pgmid     = va_pgmid                     "RF21032001
            AND    object    = va_object                    "RF21032001
            AND    obj_name  = va_obj_name.                 "RF21032001
          va_devclass = tadir-devclass.                     "RF21032001
        ELSE.
          trdir = i_trdir.
          va_devclass = i_trdir-devclass.                   "RF21032001
        ENDIF.
        MOVE-CORRESPONDING trdir TO i_tot.
        i_tot-devclass = va_devclass.                       "RF21032001
        i_tot-masterlang = tadir-masterlang.
        APPEND i_tot.
        i_include-prog = i_trdir-name.
        i_include-incl = trdir-name.
        i_include-masterlang = tadir-masterlang.
        APPEND i_include.
      ENDLOOP.
    ELSE.
      i_tot = i_trdir.
      APPEND i_tot.
    ENDIF.

  ENDLOOP.

* Elimino programmi con subc = T
  DELETE i_tot WHERE subc = 'T'.

  SORT i_tot.
  DELETE ADJACENT DUPLICATES FROM i_tot.



* Carico tabelle interne per determinazione indici secondari
  SELECT sqltab indexname FROM  dd12l  INTO TABLE i_dd12l
         WHERE   as4local    = 'A'
         AND    as4vers     = '0000' .                  "#EC CI_NOFIRST

  SELECT  sqltab indexname fieldname FROM  dd17s  INTO TABLE i_dd17s
         FOR ALL ENTRIES IN i_dd12l
         WHERE  sqltab      = i_dd12l-sqltab
         AND    indexname   = i_dd12l-indexname
         AND    as4local    = 'A'
         AND    as4vers     = '0000' .

  REFRESH:  i_dd12l, i_trdir.
  DELETE i_dd17s WHERE fieldname = 'MANDT'.

  SELECT * FROM  dd02l INTO TABLE i_dd02l_view
           WHERE  tabclass  = 'VIEW'.                   "#EC CI_NOFIELD


  SELECT obj_name FROM  tadir  INTO TABLE i_tadir_dm
         WHERE  pgmid       = 'R3TR'
         AND    object      = 'DOMA'
         AND    devclass   IN s_devcla.               "#EC CI_SGLSELECT

* modifica doit
  DELETE i_tadir_dm WHERE domname(1) NE 'Z' AND
                          domname(1) NE 'Y'.
* fine modifica doit
  SELECT obj_name FROM  tadir  INTO TABLE i_tadir_dt
         WHERE  pgmid       = 'R3TR'
         AND    object      = 'DTEL'
         AND    devclass   IN s_devcla.               "#EC CI_SGLSELECT

* modifica doit
  DELETE i_tadir_dt WHERE rollname(1) NE 'Z' AND
                          rollname(1) NE 'Y'.
* fine modifica doit

*
  SELECT obj_name FROM  tadir  INTO TABLE i_tadir_tb
         WHERE  pgmid       = 'R3TR'
         AND    object      = 'TABL'
         AND    devclass   IN s_devcla.               "#EC CI_SGLSELECT

* modifica doit
  DELETE i_tadir_tb WHERE tabname(1) NE 'Z' AND
                          tabname(1) NE 'Y'.
* fine modifica doit



  SELECT prog dnum FROM  d020s INTO TABLE  i_d020s
        FOR ALL ENTRIES IN i_tot
        WHERE  prog        = i_tot-name
          AND  type       NE 'S'.


  CALL FUNCTION 'DB_COMMIT'
    EXCEPTIONS
      OTHERS = 1.
  CHECK sy-subrc EQ 0.

ENDFORM.                    " selezione_programmi

*&---------------------------------------------------------------------
*
*&      Form  include_to_be_excluded
*&---------------------------------------------------------------------

FORM include_to_be_excluded USING name TYPE any.

  r_incl-sign = 'I'.
  r_incl-low = name.
  IF name = '%_C*'.
    r_incl-option = 'CP'.
  ELSE.
    r_incl-option = 'EQ'.
  ENDIF.
  APPEND r_incl.

ENDFORM.                    " include_to_be_excluded

*&---------------------------------------------------------------------
*
*&      Form  Elabora_programmi
*&---------------------------------------------------------------------
FORM elabora_programmi.

*Selezione Titolo Programma
  SELECT name subc
  FROM trdir
  INTO TABLE tb_trdir
  FOR ALL ENTRIES IN i_tot
  WHERE name = i_tot-name.

  SORT tb_trdir BY name.

*Selezion Tipo Programma
  SELECT name text
  FROM trdirt
  INTO TABLE tb_trdirt
  FOR ALL ENTRIES IN i_tot
  WHERE name = i_tot-name.

  SORT tb_trdirt BY name.

* Loop su tutti i programmi selezionati + relative include
  LOOP AT i_tot.
    REFRESH t_code.
    READ REPORT i_tot-name INTO t_code.

    LOOP AT t_code.

      LOOP AT tb_istruzioni.
        IF t_code-line CP tb_istruzioni-tipo.
          IF NOT t_code-line(1) = ca_star.
            ADD 1 TO tb_istruzioni-conta.
            va_check = ca_x.
            MODIFY tb_istruzioni.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDLOOP.
*Dopo aver controllato il codice di un programma.
    IF va_check = ca_x.
      PERFORM carica_tb_stampa.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " Elabora_programmi

*&---------------------------------------------------------------------*
*&      Form  f_delete_programs
*&---------------------------------------------------------------------*

FORM f_delete_programs.

  i_totappogg[] = i_tot[].

*Valorizzazione della tabella di appoggio dei soli programmi da
*analizzare
  DELETE i_totappogg WHERE name IN s_progr.

  LOOP AT i_totappogg.
    CLEAR i_tot.
    READ TABLE i_tot WITH KEY name = i_totappogg-name.
*se nella i_tot sono presenti programmi non richiesti vengono cancellati
    IF sy-subrc = 0.
      DELETE i_tot WHERE name = i_totappogg-name.
    ENDIF.
  ENDLOOP.

  DELETE i_tot WHERE name = sy-repid.

ENDFORM.                    " f_delete_programs

*&---------------------------------------------------------------------*
*&      Form  carica_tb_stampa
*&---------------------------------------------------------------------*

FORM carica_tb_stampa.

  PERFORM build_report.

ENDFORM.                    " carica_tb_stampa

*&---------------------------------------------------------------------*
*&      Form  reset_tb_istruzioni
*&---------------------------------------------------------------------*
FORM reset_tb_istruzioni.
  LOOP AT tb_istruzioni WHERE conta NE 0.
    tb_istruzioni-conta = 0.
    MODIFY tb_istruzioni.
  ENDLOOP.
  CLEAR va_check.
ENDFORM.                    " reset_tb_istruzioni

*&---------------------------------------------------------------------*
*&      Form  build_dyn_itab
*&---------------------------------------------------------------------*

FORM build_dyn_itab.

  DATA: new_table TYPE REF TO data,
        new_line  TYPE REF TO data,
        wa_it_fldcat TYPE lvc_s_fcat.

*CREAZIONE CAMPI FISSI
*Colonna per Nome Programma
  CLEAR wa_it_fldcat.
  wa_it_fldcat-fieldname = text-001.
  wa_it_fldcat-datatype = 'C'.
  wa_it_fldcat-intlen = 40.
  wa_it_fldcat-outputlen = 40.
  APPEND wa_it_fldcat TO it_fldcat .

*Colonna per Descrizione Programma
  CLEAR wa_it_fldcat.
  wa_it_fldcat-fieldname = text-002.
  wa_it_fldcat-datatype = 'C'.
  wa_it_fldcat-intlen = 60.
  wa_it_fldcat-outputlen = 60.
  APPEND wa_it_fldcat TO it_fldcat .

*Colonna per Nome Programma
  CLEAR wa_it_fldcat.
  wa_it_fldcat-fieldname = text-003.
  wa_it_fldcat-datatype = 'C'.
  wa_it_fldcat-intlen = 3.
  APPEND wa_it_fldcat TO it_fldcat .

*Colonna per Campo Check Programma
  CLEAR wa_it_fldcat.
  wa_it_fldcat-fieldname = text-004.
  wa_it_fldcat-datatype = 'C'.
  wa_it_fldcat-intlen = 3.
  APPEND wa_it_fldcat TO it_fldcat .

*Colonne relative alle istruzioni da controllare
  LOOP AT tb_istruzioni.
    ADD 1 TO va_col.
    CLEAR wa_it_fldcat.
    wa_it_fldcat-fieldname = tb_istruzioni-tipo.
    wa_it_fldcat-datatype = 'C'.
    wa_it_fldcat-intlen = 10.
    APPEND wa_it_fldcat TO it_fldcat .
  ENDLOOP.

*Carico in va_text le intestazioni della tabella per il salvataggio del
*file
  CLEAR wa_it_fldcat.
  LOOP AT it_fldcat INTO wa_it_fldcat.
    CONCATENATE va_text wa_it_fldcat-fieldname ';' INTO va_text.
  ENDLOOP.

* Creazione tabella interna dinamica
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = it_fldcat
    IMPORTING
      ep_table        = new_table.

  ASSIGN new_table->* TO <dyn_table>.

* Creatzione work area dinamica
  CREATE DATA new_line LIKE LINE OF <dyn_table>.
  ASSIGN new_line->* TO <dyn_wa>.

ENDFORM.                    " build_dyn_itab

*&---------------------------------------------------------------------*
*&      Form  build_report
*&---------------------------------------------------------------------*

FORM build_report.

  DATA: fieldvalue(60) TYPE c.
  FIELD-SYMBOLS: <fs1> TYPE ANY.


  CLEAR va_col.
*Caricamento dati Fissi in Tabella
*Nome Programma
  ADD 1 TO va_col.
  fieldvalue = i_tot-name..
  CONDENSE   fieldvalue NO-GAPS.
  ASSIGN COMPONENT  va_col OF STRUCTURE <dyn_wa> TO <fs1>.
  <fs1> =  fieldvalue.


*Titolo Programma
  READ TABLE tb_trdirt WITH KEY name = i_tot-name BINARY SEARCH.
  IF sy-subrc = 0.
    fieldvalue = tb_trdirt-text.
  ENDIF.
  ADD 1 TO va_col.
  ASSIGN COMPONENT  va_col OF STRUCTURE <dyn_wa> TO <fs1>.
  <fs1> =  fieldvalue.

*Tipo Programma
  READ TABLE tb_trdir WITH KEY name = i_tot-name BINARY SEARCH.
  IF sy-subrc = 0.
    fieldvalue = tb_trdir-subc.
  ENDIF.
  ADD 1 TO va_col.
  CONDENSE   fieldvalue NO-GAPS.
  ASSIGN COMPONENT  va_col OF STRUCTURE <dyn_wa> TO <fs1>.
  <fs1> =  fieldvalue.

*Check
  ADD 1 TO va_col.
  fieldvalue = va_check.
  CONDENSE   fieldvalue NO-GAPS.
  ASSIGN COMPONENT  va_col OF STRUCTURE <dyn_wa> TO <fs1>.
  <fs1> =  fieldvalue.


  va_col = 5.

*Colonne relative alle istruzioni da controllare
  LOOP AT tb_istruzioni.
    fieldvalue = tb_istruzioni-conta.
    CONDENSE fieldvalue NO-GAPS.
    ASSIGN COMPONENT  va_col OF STRUCTURE <dyn_wa> TO <fs1>.
    <fs1> =  fieldvalue.
    ADD 1 TO va_col.
  ENDLOOP.

* Append to the dynamic internal table
  APPEND <dyn_wa> TO <dyn_table>.

  PERFORM reset_tb_istruzioni.

ENDFORM.                    " build_report

*&---------------------------------------------------------------------*
*&      Form  stampa_risultati
*&---------------------------------------------------------------------*
*       text

FORM stampa_risultati.
  DATA: wa_cat LIKE LINE OF alv_fldcat.


  PERFORM layout_alv.

  CLEAR wa_cat.
  wa_cat-fieldname = text-001.
  wa_cat-seltext_s = text-001.
  wa_cat-outputlen = '40'.
  APPEND wa_cat TO alv_fldcat.

  CLEAR wa_cat.
  wa_cat-fieldname = text-002.
  wa_cat-seltext_s = text-002.
  wa_cat-outputlen = '50'.
  APPEND wa_cat TO alv_fldcat.

  CLEAR wa_cat.
  wa_cat-fieldname = text-003.
  wa_cat-seltext_s = text-003.
  wa_cat-outputlen = '8'.
  APPEND wa_cat TO alv_fldcat.

  CLEAR wa_cat.
  wa_cat-fieldname = text-004.
  wa_cat-seltext_s = text-004.
  wa_cat-outputlen = '5'.
  APPEND wa_cat TO alv_fldcat.

  va_col = 1.
  LOOP AT tb_istruzioni.
    CLEAR wa_cat.
    wa_cat-fieldname = tb_istruzioni-tipo.
    wa_cat-seltext_l = tb_istruzioni-tipo.
    wa_cat-outputlen = '20'.
    APPEND wa_cat TO alv_fldcat.
    ADD 1 TO va_col.
  ENDLOOP.


* Call ABAP List Viewer (ALV)

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat = alv_fldcat
      is_layout   = st_layout
    TABLES
      t_outtab    = <dyn_table>.


ENDFORM.                    " stampa_risultati


*&---------------------------------------------------------------------*
*&      Form  layout_alv
*&---------------------------------------------------------------------*
FORM layout_alv.

  CLEAR st_layout.
  st_layout-colwidth_optimize = ca_x.
  st_layout-zebra = ca_x.

ENDFORM.                    " layout_alv

*&---------------------------------------------------------------------*
*&      Form  carico_istruzioni
*&---------------------------------------------------------------------*

FORM carico_istruzioni.

*Carico Tb_Istruzioni con le Istruzioni Critiche da Controllare
  tb_istruzioni-tipo = '*SET*PARAMETER*'.
  APPEND tb_istruzioni.
  tb_istruzioni-tipo = '*GET*PARAMETER*'.
  APPEND tb_istruzioni.
  tb_istruzioni-tipo = '*TRANSLATE*CODEPAGE*'.
  APPEND tb_istruzioni.
  tb_istruzioni-tipo = '*TRANSLATE*NUMBER*FORM*'.
  APPEND tb_istruzioni.
  tb_istruzioni-tipo = '*DESCRIBE*DISTANCE*'.
  APPEND tb_istruzioni.
  tb_istruzioni-tipo = '*DESCRIBE*LENGTH*'.
  APPEND tb_istruzioni.
  tb_istruzioni-tipo = '*GET*BIT*'.
  APPEND tb_istruzioni.
  tb_istruzioni-tipo = '*SET*BIT*'.
  APPEND tb_istruzioni.
  tb_istruzioni-tipo = '*ADD*THEN*UNTIL*'.
  APPEND tb_istruzioni.
  tb_istruzioni-tipo = '*ADD*FROM*GIVING*'.
  APPEND tb_istruzioni.
  tb_istruzioni-tipo = '*VARYING*'.
  APPEND tb_istruzioni.
  tb_istruzioni-tipo = '*INSERT*REPORT*'.
  APPEND tb_istruzioni.
  tb_istruzioni-tipo = '*OPEN*TEXT*MODE*'.
  APPEND tb_istruzioni.
  tb_istruzioni-tipo = '*STRLEN*'.
  APPEND tb_istruzioni.
  tb_istruzioni-tipo = '*WS_UPLOAD*'.
  APPEND tb_istruzioni.
  tb_istruzioni-tipo = '*WS_DOWNLOAD*'.
  APPEND tb_istruzioni.
  tb_istruzioni-tipo = '*UPLOAD*'.
  APPEND tb_istruzioni.
  tb_istruzioni-tipo = '*DOWNLOAD*'.
  APPEND tb_istruzioni.
  tb_istruzioni-tipo = '*0D0A*'.
  APPEND tb_istruzioni.
  tb_istruzioni-tipo = '*CALL*FUNCTION*DESTINATION*'.
  APPEND tb_istruzioni.


  DESCRIBE TABLE tb_istruzioni LINES vn_colonne.

ENDFORM.                    " carico_istruzioni

*&---------------------------------------------------------------------*
*&      Form  salva_dati
*&---------------------------------------------------------------------*

FORM salva_dati.

  va_file = p_file.

*Converto i dati della tabella <dyn_table> in apposito formato caricare
*i dati su file CSV
  CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
    EXPORTING
      i_field_seperator    = ';'
    TABLES
      i_tab_sap_data       = <dyn_table>
    CHANGING
      i_tab_converted_data = itab1
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.

*Carico in Itab2 le intestazioni e i dati presenti in Itab1
  APPEND va_text TO itab2.
  LOOP AT itab1 INTO va_text .
    APPEND va_text TO itab2.
  ENDLOOP.

  IF NOT p_file IS INITIAL.

*Apertura file di output
    va_dir = p_file.
* Begin of Change <Saurabh Luthra> <AB1K900413>
*    OPEN DATASET va_dir FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    OPEN DATASET va_dir FOR OUTPUT IN TEXT MODE.
* End of Change <Saurabh Luthra> <AB1K900413>
    IF sy-subrc NE 0.
      MESSAGE e208 WITH text-e05.
    ENDIF.

*Carico le strutture dati nel file
    LOOP AT itab2 INTO va_text.
      TRANSFER va_text TO va_dir.
    ENDLOOP.


* Chiusura file di output
    CLOSE DATASET va_dir.
  ELSE.
    MESSAGE i208 WITH text-e02.
  ENDIF.

ENDFORM.                    " salva_dati

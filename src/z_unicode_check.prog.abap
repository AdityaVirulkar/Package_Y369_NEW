****************************************************************
* Accenture GBS Milan                                          *
* Nome programma       : Z_UNICODE_CHECK                       *
* Data creazione       : 07/03/2006                            *
* CR:                  :                                       *
* Descrizione breve    : Report controllo programmi UNICODE    *
* compatibili.                                                 *
****************************************************************
* Edited by Saurabh Luthra on April 14, 2008                   *
*                                                              *
* Summary of changes,                                          *
* - Changed t_code size to accomodate large width programs     *
* - Added Y*, Z* defaults to Program Name field                *
****************************************************************
* Edited by Saurabh Luthra on February 27, 2009                *
* CR# AB1K900413                                               *
* Summary of changes,                                          *
* - Removed ENCODING DEFAULT addition on OPEN DATASET          *
****************************************************************
* Edited by Saurabh Luthra on March 4, 2009                    *
* CR# AB1K900415                                               *
* Summary of changes,                                          *
* - Removed Y*, Z* initialization on S_PROG                    *
* - Added TADIR PROGs to initialization of S_PROGR             *
* - Included Customer Namespace functionality                  *
* - Divided Program into Includes                              *

* Edited by Saurabh Luthra on August 19, 2009                  *
* CR# AB1K900522                                               *
* Summary of changes,                                          *
* - Used internal table for s_progr                            *
* - Used field names in place of select * for trdir            *
****************************************************************


REPORT z_unicode_check
           MESSAGE-ID 00
           LINE-SIZE  255
           LINE-COUNT 65
           NO STANDARD PAGE HEADING.

INCLUDE y369_ucchecker_top.

* [ SELECT-OPTIONS ]

SELECT-OPTIONS: s_prog   FOR trdir-name,
                s_devcla FOR tadir-devclass,
                s_cnam   FOR trdir-cnam,
                s_cdat   FOR trdir-cdat,
                s_unam   FOR trdir-unam,
                s_udat   FOR trdir-udat,
                s_subc   FOR trdir-subc.

SELECTION-SCREEN SKIP 1.
* Begin of Change by Saurabh Luthra - March 4, 2009
*SELECT-OPTIONS: s_progr FOR trdir-name.
SELECT-OPTIONS: s_progr FOR tadir-obj_name.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: s_namspc FOR v_namespace NO INTERVALS.

* End of Change by Saurabh Luthra - March 4, 2009
SELECTION-SCREEN SKIP 1.
PARAMETER: p_file LIKE rlgrap-filename.   "File logico

* Begin of Change by Saurabh Luthra - April 14, 2008
INCLUDE y369_ucchecker_f01.

* [ INITIALIZATION EVENT ]
INITIALIZATION.

* Begin of Change by Saurabh Luthra - March 4, 2009
*  s_prog-sign = 'I'.
*  s_prog-option = 'CP'.
*  s_prog-low = 'Y*'.
*  CLEAR s_prog-high.
*  APPEND s_prog.
*  CLEAR s_prog.
*
*  s_prog-sign = 'I'.
*  s_prog-option = 'CP'.
*  s_prog-low = 'Z*'.
*  CLEAR s_prog-high.
*  APPEND s_prog.
*  CLEAR s_prog.

  SELECT obj_name
      AS low
    FROM tadir
    INTO CORRESPONDING FIELDS OF TABLE s_progr
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


* [ AT SELECTION-SCREEN EVENT ]

AT SELECTION-SCREEN ON s_namspc.
  PERFORM f_check_namspc.

* End of Change by Saurabh Luthra - March 4, 2009
* End of Change by Saurabh Luthra - April 14, 2008

* [ START-OF-SELECTION EVENT ]

START-OF-SELECTION.

* Begin of Change by Saurabh Luthra - March 4, 2009
* Edit customer namespace and append to S_PROGR
  PERFORM f_edit_namespace_string.
* End of Change by Saurabh Luthra - March 4, 2009

  PERFORM carico_istruzioni.

* build the dynamic internal table
  PERFORM build_dyn_itab.

* Begin of Change by Vandana Sharma - March 10, 2009
  PERFORM edit_prog_name.
* End of Change by Vandana Sharma - March 10, 2009

  PERFORM selezione_programmi.

  CLEAR i_tot.

  IF NOT s_progr[] IS INITIAL.
    PERFORM f_delete_programs.
  ENDIF.

  PERFORM elabora_programmi.

*Salva i dati su file
  PERFORM salva_dati.

*Stampa dei risultati su ALV
  PERFORM stampa_risultati.

*  end of selection.

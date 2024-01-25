*&---------------------------------------------------------------------*
*& Report  Y369_GETINCLUDES                                            *
*&                                                                     *
*&---------------------------------------------------------------------*
* MODIFICATION HISTORY
*&---------------------------------------------------------------------*
* Modified by      : Vandana Sharma
* Modification Date: 13.02.2009
* Correction No    : AB1K900402
* Description      : 1. Changed selection criteria and added namespace
*                       functionality.
*                    2. Cleaned up the source code. Divided program into
*                       includes.
*&---------------------------------------------------------------------*
* Modified by      : Saurabh Luthra
* Modification Date: 19.02.2009
* Correction No    : AB1K900398
* Description      : 1. Corrected program to accept namespace FUGRs.
*                    2. Program was accepting all namespace items into
*                       PROG internal table. Corrected program to accept
*                       namespace PROGs only.
*                    3. Changed namespace error message
*&---------------------------------------------------------------------*


REPORT  y369_getincludes
        MESSAGE-ID su.

INCLUDE y369_getincludes_top.
INCLUDE y369_getincludes_f01.

AT SELECTION-SCREEN ON s_namspc.
  PERFORM f_check_namspc.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fpath2.
  PERFORM f_get_filename.

START-OF-SELECTION.

* Edit customer namespace
  PERFORM f_edit_namespace_string.
* Get Program name
  PERFORM f_get_prog.
* Get includes
  PERFORM f_get_incl.
* Download the results to file
  PERFORM f_download_results USING p_fpath2.

END-OF-SELECTION.

* Display the report
  PERFORM f_display_report.

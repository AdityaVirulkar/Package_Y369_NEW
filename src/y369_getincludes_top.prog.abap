*&---------------------------------------------------------------------*
*&  Include           Y369_GETINCLUDES_TOP                             *
*&---------------------------------------------------------------------*

* Types
TYPES: BEGIN OF ty_prog,
         prog LIKE sy-repid,
       END OF ty_prog,

       BEGIN OF ty_inc,
         name LIKE sy-repid,
       END OF ty_inc,

       BEGIN OF ty_final,
         prg     LIKE trdir-name,
         include LIKE sy-repid,
       END OF ty_final.

* Internal Tables
DATA: t_prog  TYPE ty_prog OCCURS 0 WITH HEADER LINE,
      t_inc   TYPE ty_inc OCCURS 0,
      t_final TYPE ty_final OCCURS 0,
* Work Areas
      wa_inc   TYPE ty_inc,
      wa_final TYPE ty_final,
* Variables
      l_tabix(6)  TYPE c,
      l_text(120) TYPE c,
      v_namespace(20) TYPE c.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_namspc FOR v_namespace NO INTERVALS.
SELECTION-SCREEN SKIP 1.
PARAMETERS: p_fpath2 TYPE  rlgrap-filename.
SELECTION-SCREEN END OF BLOCK b1.

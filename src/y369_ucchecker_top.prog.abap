*&---------------------------------------------------------------------*
*&  Include           Y369_UCCHECKER_TOP                               *
*&---------------------------------------------------------------------*

* [ TABLES ]
TABLES: tadir,
        tfdir,
        trdir.

* [ INTERNAL TABLES ]

DATA: BEGIN  OF i_tadir OCCURS 100.
INCLUDE  STRUCTURE  tadir.
DATA: END OF i_tadir.

* Begin of Change <Saurabh Luthra> <AB1K900522>
*DATA: BEGIN OF i_trdir  OCCURS 100.
*INCLUDE  STRUCTURE  trdir.
*DATA: devclass LIKE tadir-devclass.
*DATA: END OF i_trdir.

DATA: BEGIN OF i_trdir  OCCURS 100,
       name like trdir-name,
       subc like trdir-subc,
       devclass LIKE tadir-devclass,
END OF i_trdir.

* End of Change <Saurabh Luthra> <AB1K900522>


* Tabella contenente tutti i prog e relative include.
DATA: BEGIN OF i_tot OCCURS 0.
        INCLUDE STRUCTURE i_trdir.
DATA: masterlang LIKE tadir-masterlang,
      END OF i_tot.


DATA: i_totappogg LIKE i_tot OCCURS 0 WITH HEADER LINE.


DATA: BEGIN OF tb_istruzioni OCCURS 0,
          tipo(100) TYPE c,
          conta TYPE i,
      END OF tb_istruzioni.


DATA: BEGIN OF tb_trdir OCCURS 0,
        name TYPE trdir-name,
        subc TYPE trdir-subc,
      END OF tb_trdir.

DATA: BEGIN OF tb_trdirt OCCURS 0,
        name TYPE trdir-name,
        text(70) TYPE c,
      END OF tb_trdirt.

DATA: BEGIN OF t_code OCCURS 0,
* Begin of Change by Saurabh Luthra - April 14, 2008
*            line(150),
            line(400),
* End of Change by Saurabh Luthra - April 14, 2008
      END   OF t_code.

DATA: BEGIN OF i_totale,
       prog LIKE syst-repid,
       incl LIKE syst-repid,
       masterlang LIKE tadir-masterlang,
       hrdcd1 TYPE c,
       hrdcd2 TYPE c.
DATA: END OF i_totale.

* Tabella che associa prog. alle  relative include
DATA: i_include LIKE i_totale OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF  i_appoggio OCCURS 10,
            incl LIKE syst-repid,
      END  OF i_appoggio.

DATA: BEGIN OF st_dd12l,
       sqltab LIKE dd12l-sqltab,
       indexname LIKE dd12l-indexname,
      END OF st_dd12l.

DATA: i_dd12l LIKE STANDARD TABLE OF st_dd12l.

DATA: BEGIN OF i_dd17s OCCURS 0,
       sqltab LIKE dd17s-sqltab,
       indexname LIKE dd17s-indexname,
       fieldname LIKE dd17s-fieldname,
      END OF i_dd17s.

DATA: i_dd02l_view LIKE dd02l OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF i_d020s OCCURS 0,
        prog LIKE d020s-prog,
        dnum LIKE d020s-dnum,
      END OF i_d020s.

DATA: BEGIN OF tb_intest OCCURS 0,
        label(60),
      END OF tb_intest.

* Begin of Change <Saurabh Luthra> <AB1K900522>
TYPES: BEGIN OF t_prog,
        prog LIKE trdir-name,
      END OF t_prog.

DATA: i_prog  TYPE STANDARD TABLE OF t_prog INITIAL SIZE 0,
      wa_prog TYPE t_prog.
* End of Change <Saurabh Luthra> <AB1K900522>

* [ ALV ]
TYPE-POOLS: slis,
            truxs.

DATA: st_layout TYPE slis_layout_alv,        "STRUTTURA PER LAYOUT
      alv_fldcat TYPE slis_t_fieldcat_alv,
      it_fldcat TYPE lvc_t_fcat.

* [ VARIABLES ]
DATA: vn_colonne  TYPE i,
      va_col      TYPE i,
      va_check(1) TYPE c,
      vn_cont_tot TYPE i,
      v_namespace(20) TYPE c,
      itab1 TYPE truxs_t_text_data,
      itab2 TYPE truxs_t_text_data,
      va_file TYPE string,
      va_text(4096) TYPE c,
      va_dir(4096)    TYPE c.

* [FIELD SYMBOLS ]
FIELD-SYMBOLS: <dyn_table> TYPE STANDARD TABLE,
               <dyn_wa>.

* [ RANGES ]
RANGES: r_incl  FOR syst-repid.

* [ CONSTANTS ]
CONSTANTS:
     ca_x(1) VALUE 'X',
     ca_star(1) VALUE '*'.

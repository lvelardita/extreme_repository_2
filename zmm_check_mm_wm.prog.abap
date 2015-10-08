*---------------------------------------------------------------------*
* Progetto    :
*---------------------------------------------------------------------*
* Titolo      : Controllo stock MM-WM
*---------------------------------------------------------------------*
* Creato da   : Programmatore Paolo
* Creato il   : xx.xx.xxxxx
* Descrizione :
*----------------------------------------------------------------------*
* Storico Modifiche
*----------------------------------------------------------------------*
* Id Mod.|     Autore     |  Data  | Descrizione                       |
*----------------------------------------------------------------------*
*        |                |        |                                   |
*        |                |        |                                   |
*        |                |        |                                   |
*----------------------------------------------------------------------*

REPORT zmm_check_mm_wm NO STANDARD PAGE HEADING line-size 150.

TABLES: mara,
        marc,
        mard,
        mbew,
        t001w,
        t001l,
        t320.

* Tabella per il caricamento dei dati
DATA: BEGIN OF t_file OCCURS 0,
        campo(200) TYPE c,
      END   OF t_file.


DATA: BEGIN OF t_file_wm OCCURS 0,
        campo(200) TYPE c,
      END   OF t_file_wm.


* Tabella per l'elaborazione dei dati
DATA: BEGIN OF t_input OCCURS 0,
          matnr(18) TYPE c,
          desc(40)  TYPE c,
          lgort(4)  TYPE c,
          meins(3)  TYPE c,
          charg(20) TYPE c,
          quant(18) TYPE c,
          tpstk(1)  TYPE c,
          produ(8)  TYPE c,
          werks(4)  TYPE c,
          scade(8)  TYPE c,
          dmbtr(18)  TYPE c,
     END   OF t_input.

* Tabella per l'elaborazione dei dati
DATA: BEGIN OF t_input_ctr OCCURS 0,
          matnr(18) TYPE c,
          werks(4)  TYPE c,
          lgort(4)  TYPE c,
          quant     TYPE mbew-lbkum,
       END   OF t_input_ctr.

* Tabella per l'elaborazione dei dati
DATA: BEGIN OF t_input_ctr_wm OCCURS 0,
          matnr(18) TYPE c,
          werks(4)  TYPE c,
          lgort(4)  TYPE c,
          quant     TYPE mbew-lbkum,
       END   OF t_input_ctr_wm.


*---------------------------------------------------------
*     Tabella per l'elaborazione dei dati WM
DATA: BEGIN OF t_input_wm OCCURS 0,
  werks(4)  TYPE c,
  lgort(4)  TYPE c,
  lgnum(3)  TYPE c,
  lenum(20) TYPE c,
  matnr(18) TYPE c,
  charg(20) TYPE c,
  quant(18) TYPE c,
  tipom(3)  TYPE c,
  ubi(10)   TYPE c,
  tpstk(1)  TYPE c,
  palle(2)  TYPE c,
END   OF t_input_wm.


* Tabella con i dati per il batch-input
DATA: BEGIN OF t_elab OCCURS 0,
        tpstk(1),
        werks      TYPE mchb-werks,
        lgort      TYPE mchb-lgort,
        matnr      TYPE mara-matnr,
        charg      TYPE mcha-charg,
        quant(18)  TYPE c,
        meins(3)   TYPE c,
        produ      TYPE d,
        scade      TYPE d,
        dmbtr(18)  TYPE c,
        xchpf      TYPE mara-xchpf,
        cderr      TYPE c,
        error(50)  TYPE c,
      END   OF t_elab.

DATA: g_sw_err      TYPE c,
      g_salvato     TYPE c,
      g_rest(200)   TYPE c,
      g_file        TYPE string,
      g_num_rec     TYPE i,
      g_bwart       TYPE mseg-bwart,
      g_budat(8)    TYPE c,
      g_dmbtr(15)   TYPE c,
      g_produ(8)    TYPE c,
      g_scade(8)    TYPE c,
      g_conta_righe TYPE i,
      g_max_pos     TYPE i  VALUE 300,
      g_intens      TYPE c  VALUE '1',
      g_len         TYPE i,
      g_charg(10)   TYPE c,
      g_errore      TYPE c,
      g_prctr       TYPE marc-prctr,
      g_tot_qta_mm  TYPE mseg-menge,
      g_tot_qta_wm  TYPE mseg-menge,
      g_diff        TYPE mseg-menge.

CONSTANTS: c_mask(20) VALUE ',*.*  ,*.*.'.

FIELD-SYMBOLS: <f1> TYPE any.

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-t01.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS: s_matnr FOR  mara-matnr,
                s_werks FOR  t001w-werks,
                s_lgort FOR  t001l-lgort.
SELECTION-SCREEN SKIP.

PARAMETERS: p_file LIKE rlgrap-filename
                   DEFAULT 'C:\Temp\Stock.txt' OBLIGATORY.

PARAMETERS: p_file_w LIKE rlgrap-filename
                   DEFAULT 'C:\Temp\Stock_wm.txt' OBLIGATORY.


PARAMETERS: p_ricod AS CHECKBOX.
SELECTION-SCREEN: SKIP.

SELECTION-SCREEN END OF BLOCK a1.

INCLUDE zbdcrecx1_bi.

*&---------------------------------------------------------------------*
*&      AT SELECTION SCREEN
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 EQ 'MAI'.
      screen-input       = '0'.
      screen-intensified = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  PERFORM filename_get    USING p_file.
  PERFORM filename_get_wm USING p_file_w.

*&---------------------------------------------------------------------*
*&      Form  FILENAME_GET
*&---------------------------------------------------------------------*

FORM filename_get USING l_filename.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = ' '
      def_path         = l_filename
      mask             = c_mask
      mode             = '0'
      title            = ' '
    IMPORTING
      filename         = l_filename
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.
  IF sy-subrc NE 0.
    MESSAGE i398(00) WITH 'Inserire nome file'(e00) ' ' ' ' ' '.
  ENDIF.
ENDFORM.                    " FILENAME_GET

*&---------------------------------------------------------------------*
*&      Form  filename_get_wm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_FILENAME text
*----------------------------------------------------------------------*
FORM filename_get_wm USING l_filename.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = ' '
      def_path         = l_filename
      mask             = c_mask
      mode             = '0'
      title            = ' '
    IMPORTING
      filename         = l_filename
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.
  IF sy-subrc NE 0.
    MESSAGE i398(00) WITH 'Inserire nome file'(e00) ' ' ' ' ' '.
  ENDIF.
ENDFORM.                    " FILENAME_GET


*-----------------------------------------------------------------------
*       TOP OF PAGE
*-----------------------------------------------------------------------

TOP-OF-PAGE.
  WRITE: /50 'Controllo quantita'(h00),
         150 ' '.
  SKIP.
  FORMAT COLOR COL_HEADING.
  WRITE:   / sy-uline,
          /1 sy-vline,
           2 'Div.'(h04),
           6 sy-vline,
           7 'Mag.'(h05),
          11 sy-vline,
          12 'Materiale'(h02),
          30 sy-vline,
          31 'Partita'(h03),
          41 sy-vline,
          49 'Quantità'(h08),
          58 sy-vline,
          59 'UM'(h09),
          62 sy-vline,
          63 'T'(h06),
          64 sy-vline,
          65 'Dt. prod.'(h07),
          75 sy-vline,
          76 'Dt. scad.'(h11),
          86 sy-vline,
          87 'Importo'(h13),
         105 sy-vline,
         106 'X'(h01),
         107 sy-vline,
         108 'Note'(h12),
         150 sy-vline.
  WRITE / sy-uline.
  FORMAT COLOR OFF.

*-----------------------------------------------------------------------
*       START OF SELECTION
*-----------------------------------------------------------------------

START-OF-SELECTION.

*---------------------- Leggi file stock MM
  PERFORM leggi_file.
  IF g_sw_err IS INITIAL.
    PERFORM splitta_file.
  ENDIF.

*---------------------- Leggi file stock WM
  PERFORM leggi_file_w.
  IF g_sw_err IS INITIAL.
    PERFORM splitta_file_wm.
  ENDIF.

  IF g_sw_err IS INITIAL.
    PERFORM elabora.
*    PERFORM stampa_lista.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  LEGGI_FILE
*&---------------------------------------------------------------------*

FORM leggi_file.
  g_file = p_file.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename = g_file
      filetype = 'ASC'
    TABLES
      data_tab = t_file
    EXCEPTIONS
      OTHERS   = 1.
  IF sy-subrc NE 0.
    MESSAGE i398(00) WITH 'Impossibile leggere il file'(i01)
                          g_file ' ' ' '.
    g_sw_err = 'X'.
  ENDIF.
ENDFORM.                    " LEGGI_FILE

*&---------------------------------------------------------------------*
*&      Form  leggi_file_w
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM leggi_file_w.
  g_file = p_file_w.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename = g_file
      filetype = 'ASC'
    TABLES
      data_tab = t_file_wm
    EXCEPTIONS
      OTHERS   = 1.
  IF sy-subrc NE 0.
    MESSAGE i398(00) WITH 'Impossibile leggere il file'(i01)
                          g_file ' ' ' '.
    g_sw_err = 'X'.
  ENDIF.
ENDFORM.                    " LEGGI_FILE


*&---------------------------------------------------------------------*
*&      Form  SPLITTA_FILE
*&---------------------------------------------------------------------*
FORM splitta_file.
  LOOP AT t_file.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE t_input TO <f1>.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
      SPLIT t_file-campo AT ';' INTO <f1> g_rest.
      t_file-campo = g_rest.
    ENDDO.
    APPEND t_input.
  ENDLOOP.
ENDFORM.                    " SPLITTA_FILE

*&---------------------------------------------------------------------*
*&      Form  splitta_file_wm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM splitta_file_wm.
  LOOP AT t_file_wm.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE t_input_wm TO <f1>.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
      SPLIT t_file_wm-campo AT ';' INTO <f1> g_rest.
      t_file_wm-campo = g_rest.
    ENDDO.
    APPEND t_input_wm.
  ENDLOOP.
ENDFORM.                    " SPLITTA_FILE

*&---------------------------------------------------------------------*
*&      Form  ELABORA
*&---------------------------------------------------------------------*
FORM elabora.

  CLEAR g_errore.
  CLEAR g_tot_qta_mm.
  CLEAR g_tot_qta_wm.

  SORT t_input.

*---------------------------- Leggo tab. stock MM
  LOOP AT t_input.

*---------------------------- Cerca se magazzino con WM.
    SELECT SINGLE * FROM t320 WHERE
                    werks EQ t_input-werks AND
                    lgort EQ t_input-lgort.
    IF sy-subrc EQ 0.

      TRANSLATE t_input-quant USING ',.'.
      g_tot_qta_mm = g_tot_qta_mm + t_input-quant.

      MOVE-CORRESPONDING t_input TO t_input_ctr.
      COLLECT t_input_ctr.

    ENDIF.

  ENDLOOP.

  SORT    t_input_ctr.

*---------------------------- Leggo tab. stock WM
  LOOP AT t_input_wm.

    TRANSLATE t_input_wm-quant USING ',.'.
    g_tot_qta_wm = g_tot_qta_wm + t_input_wm-quant.

    MOVE-CORRESPONDING t_input_wm TO t_input_ctr_wm.
    COLLECT t_input_ctr_wm.

  ENDLOOP.

  SORT    t_input_ctr_wm.

* ----------------------------- Inizio controllo Logico-Fisico
  LOOP AT t_input_ctr.

    READ TABLE t_input_ctr_wm WITH KEY
               matnr = t_input_ctr-matnr
               werks = t_input_ctr-werks
               lgort = t_input_ctr-lgort.

    IF sy-subrc NE 0.
      WRITE: /1 sy-vline,
              5 'Stock MM senza stock WM', t_input_ctr-matnr, t_input_ctr-werks, t_input_ctr-lgort.
    ELSE.

      IF t_input_ctr-quant NE t_input_ctr_wm-quant.
        PERFORM stampa_lista.
      ENDIF.

    ENDIF.

  ENDLOOP.

* ----------------------------- Inizio controllo Fisico-Logico
  SORT    t_input_ctr_wm.
  SORT    t_input_ctr.

  LOOP AT t_input_ctr_wm.

    READ TABLE t_input_ctr WITH KEY
               matnr = t_input_ctr_wm-matnr
               werks = t_input_ctr_wm-werks
               lgort = t_input_ctr_wm-lgort.

    IF sy-subrc NE 0.
      WRITE: /1 sy-vline,
              5 'Stock WM senza stock MM', t_input_ctr_wm-matnr, t_input_ctr_wm-werks, t_input_ctr_wm-lgort.
        ENDIF.

  ENDLOOP.


g_diff = g_tot_qta_mm - g_tot_qta_wm.
WRITE: /1 sy-vline,
              5 'Stock totale MM', g_tot_qta_mm,
             45 'Stock totale WM', g_tot_qta_wm,
             85 'Differenza', g_diff.


ENDFORM.                    " ELABORA

**&---------------------------------------------------------------------*
*&      Form  STAMPA_LISTA
*&---------------------------------------------------------------------*

FORM stampa_lista.

***  NEW-PAGE LINE-SIZE 150.

  SET PF-STATUS 'STATO_1'.
  SET TITLEBAR 'TITOLO_1'.

  g_intens = 1.
  FORMAT COLOR COL_POSITIVE.

  PERFORM enhance_read.
  WRITE:  /1 sy-vline,
           2 t_input_ctr-matnr,
          22 sy-vline,
          23 t_input_ctr-werks,
          27 sy-vline,
          28 t_input_ctr-lgort,
          33 sy-vline,
          34 t_input_ctr-quant,
          54 sy-vline,
          55 t_input_ctr_wm-matnr,
          73 sy-vline,
          74 t_input_ctr_wm-werks,
          79 sy-vline,
          80 t_input_ctr_wm-lgort,
          85 sy-vline,
          86 t_input_ctr_wm-quant,
         120 sy-vline.

  FORMAT COLOR OFF.

*  WRITE / sy-uline.
ENDFORM.                    " STAMPA_LISTA

*&---------------------------------------------------------------------*
*&      Form  ENHANCE_READ
*&--------------------------------------------------------------------*

FORM enhance_read.
  IF g_intens EQ '1'.
    FORMAT INTENSIFIED.
    g_intens = '0'.
  ELSEIF g_intens EQ '0'.
    FORMAT INTENSIFIED OFF.
    g_intens = '1'.
  ENDIF.
ENDFORM.                    " ENHANCE_READ

*

*
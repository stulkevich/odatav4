*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 28.02.2025 at 19:45:00
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZV_ODATAV4_ENT..................................*
TABLES: ZV_ODATAV4_ENT, *ZV_ODATAV4_ENT. "view work areas
CONTROLS: TCTRL_ZV_ODATAV4_ENT
TYPE TABLEVIEW USING SCREEN '0510'.
DATA: BEGIN OF STATUS_ZV_ODATAV4_ENT. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZV_ODATAV4_ENT.
* Table for entries selected to show on screen
DATA: BEGIN OF ZV_ODATAV4_ENT_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZV_ODATAV4_ENT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZV_ODATAV4_ENT_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZV_ODATAV4_ENT_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZV_ODATAV4_ENT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZV_ODATAV4_ENT_TOTAL.

*...processing: ZV_ODATAV4_NPROP................................*
TABLES: ZV_ODATAV4_NPROP, *ZV_ODATAV4_NPROP. "view work areas
CONTROLS: TCTRL_ZV_ODATAV4_NPROP
TYPE TABLEVIEW USING SCREEN '0520'.
DATA: BEGIN OF STATUS_ZV_ODATAV4_NPROP. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZV_ODATAV4_NPROP.
* Table for entries selected to show on screen
DATA: BEGIN OF ZV_ODATAV4_NPROP_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZV_ODATAV4_NPROP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZV_ODATAV4_NPROP_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZV_ODATAV4_NPROP_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZV_ODATAV4_NPROP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZV_ODATAV4_NPROP_TOTAL.

*...processing: ZV_V4_MSRV......................................*
TABLES: ZV_V4_MSRV, *ZV_V4_MSRV. "view work areas
CONTROLS: TCTRL_ZV_V4_MSRV
TYPE TABLEVIEW USING SCREEN '0500'.
DATA: BEGIN OF STATUS_ZV_V4_MSRV. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZV_V4_MSRV.
* Table for entries selected to show on screen
DATA: BEGIN OF ZV_V4_MSRV_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZV_V4_MSRV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZV_V4_MSRV_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZV_V4_MSRV_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZV_V4_MSRV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZV_V4_MSRV_TOTAL.

*.........table declarations:.................................*
TABLES: /IWBEP/I_V4_MSRT               .
TABLES: /IWBEP/I_V4_MSRV               .
TABLES: ZODATAV4_ENTITY                .
TABLES: ZODATAV4_NPROP                 .

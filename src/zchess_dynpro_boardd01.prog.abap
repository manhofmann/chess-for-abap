*&---------------------------------------------------------------------*
*& Include          ZCHESS_DYNPRO_BOARDD01
*&---------------------------------------------------------------------*
CLASS lcl_dynpro_board DEFINITION
  INHERITING FROM zcl_chess_dynpro_board
  CREATE PUBLIC FINAL.

  PUBLIC SECTION.

    METHODS call_screen REDEFINITION.

ENDCLASS.

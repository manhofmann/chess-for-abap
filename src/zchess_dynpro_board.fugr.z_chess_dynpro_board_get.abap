FUNCTION z_chess_dynpro_board_get .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IO_GUI_BOARD) TYPE REF TO  ZCL_CHESS_GUI_BOARD
*"  EXPORTING
*"     REFERENCE(EO_DYNPRO_BOARD) TYPE REF TO  ZCL_CHESS_DYNPRO_BOARD
*"----------------------------------------------------------------------

  go_dynpro_board = NEW lcl_dynpro_board(
                          io_gui_board     = io_gui_board
                          ir_dynpro_fields = REF #( zchess_gui_dynpro_fields ) ).

  eo_dynpro_board = go_dynpro_board.

ENDFUNCTION.

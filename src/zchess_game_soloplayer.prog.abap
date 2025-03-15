REPORT zchess_game_soloplayer.

START-OF-SELECTION.

  TRY.
      zcl_chess_dynpro_board=>display( NEW zcl_chess_gui_board_soloplayer( ) ).
    CATCH zcx_chess_exception INTO DATA(lx_exception).
      MESSAGE lx_exception TYPE 'E'.
  ENDTRY.

REPORT zchess_game_multiplayer.

START-OF-SELECTION.

  TRY.
      zcl_chess_dynpro_board=>display( NEW zcl_chess_gui_board_remote( ) ).
    CATCH zcx_chess_exception INTO DATA(lx_exception).
      MESSAGE lx_exception TYPE 'E'.
  ENDTRY.

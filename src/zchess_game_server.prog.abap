REPORT zchess_game_server.

START-OF-SELECTION.

  TRY.
      NEW zcl_chess_amc_server( )->do_loop( ).
    CATCH zcx_chess_exception INTO DATA(lx_exception).
      MESSAGE lx_exception TYPE 'E'.
  ENDTRY.

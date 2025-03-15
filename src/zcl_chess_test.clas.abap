class ZCL_CHESS_TEST definition
  public
  abstract
  create public
  for testing
  duration medium
  risk level harmless .

public section.

  methods SETUP_GAME
    returning
      value(RO_RESULT) type ref to ZCL_CHESS_GAME
    raising
      ZCX_CHESS_EXCEPTION .
protected section.

  data MO_BOARD type ref to ZIF_CHESS_BOARD .
private section.
ENDCLASS.



CLASS ZCL_CHESS_TEST IMPLEMENTATION.


  METHOD setup_game.

    me->mo_board = NEW zcl_chess_board( ).

    me->mo_board->initialize( NEW zcl_chess_board_setup_classic( ) ).

    ro_result = NEW zcl_chess_game(
                          io_board = me->mo_board
                          io_black = NEW zcl_chess_player( zif_chess_piece=>cs_color-black )
                          io_white = NEW zcl_chess_player( zif_chess_piece=>cs_color-white ) ).

  ENDMETHOD.
ENDCLASS.

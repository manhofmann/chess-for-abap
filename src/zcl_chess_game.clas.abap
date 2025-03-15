class ZCL_CHESS_GAME definition
  public
  create public .

public section.

  data MO_CURRENT_PLAYER type ref to ZIF_CHESS_PLAYER read-only .
  data MO_GAME_RESULT type ref to ZCL_CHESS_MOVE_STATE_INFO read-only .
  data MO_CLOCK type ref to ZCL_CHESS_CLOCK .

  methods PLAY_MOVE
    importing
      !IO_MOVE type ref to ZIF_CHESS_MOVE
    returning
      value(RO_RESULT) type ref to ZCL_CHESS_MOVE_STATE_INFO
    raising
      ZCX_CHESS_EXCEPTION .
  methods PLAY_MOVE_CURRENT_PLAYER
    returning
      value(RO_RESULT) type ref to ZCL_CHESS_MOVE_STATE_INFO
    raising
      ZCX_CHESS_EXCEPTION .
  methods UNDO_MOVE
    raising
      ZCX_CHESS_EXCEPTION .
  methods CONSTRUCTOR
    importing
      !IO_BOARD type ref to ZIF_CHESS_BOARD
      !IO_WHITE type ref to ZIF_CHESS_PLAYER
      !IO_BLACK type ref to ZIF_CHESS_PLAYER
    raising
      ZCX_CHESS_EXCEPTION .
protected section.

  methods SWAP_PLAYERS .
private section.

  data MO_BOARD type ref to ZIF_CHESS_BOARD .
  data MO_WHITE type ref to ZIF_CHESS_PLAYER .
  data MO_BLACK type ref to ZIF_CHESS_PLAYER .
  data MO_MOVE_STATE_CHECK type ref to ZCL_CHESS_MOVE_STATE_CHECKER .
ENDCLASS.



CLASS ZCL_CHESS_GAME IMPLEMENTATION.


  METHOD constructor.

    me->mo_board = io_board.
    me->mo_white = io_white.
    me->mo_black = io_black.

    " white starts
    me->mo_current_player = io_white.

    " order of checks are important!
    me->mo_move_state_check = NEW #( VALUE #( ( NEW zcl_chess_move_stalem_checkm( )   )
                                              ( NEW zcl_chess_move_50_moves_rule( )   )
                                              ( NEW zcl_chess_move_insuff_material( ) )
                                              ( NEW zcl_chess_move_threefold_rep( ) )
                                              ( NEW zcl_chess_move_check( )           )
                                   ) ).

    me->mo_clock = NEW zcl_chess_clock( io_board ).

  ENDMETHOD.


  METHOD play_move.

    IF me->mo_game_result IS BOUND AND me->mo_game_result->is_game_ending_move( ).
      " new moves are not possible anymore -> start a new game
      ro_result = me->mo_game_result.
      RETURN.
    ENDIF.

    ro_result = NEW #( ).

    IF io_move IS NOT BOUND.
      " 025	No move found. Please play a move!
      MESSAGE e025 INTO DATA(dummy).
      ro_result->set_message_from_sy( ).
      RETURN.
    ENDIF.

    DATA(lo_piece) = me->mo_board->get_piece( io_move->mo_from ).

    IF lo_piece IS NOT BOUND.
      " 026	Only pieces can be moved. Please move a piece!
      MESSAGE e026 INTO dummy.
      ro_result->set_message_from_sy( ).
      RETURN.
    ENDIF.

    IF io_move->mv_type = zif_chess_move=>cs_type-resignation.
      ro_result->set_code( zif_chess_move_state=>cs_move_state_code-resignation ).
      " 034	&1 player resigned
      MESSAGE e034 WITH zcl_chess_piece_utility=>get_color_name( me->mo_current_player->mv_color ) INTO dummy.
      ro_result->set_message_from_sy( ).
      me->mo_game_result = ro_result.
      io_move->set_moved_at( ).
      RETURN.
    ENDIF.

    " only current player can move pieces of his color
    IF lo_piece->mv_color <> me->mo_current_player->mv_color.
      " 027	Only pieces of color &1 can be moved!
      MESSAGE e027 WITH zcl_chess_piece_utility=>get_color_name( me->mo_current_player->mv_color ) INTO dummy.
      ro_result->set_message_from_sy( ).
      RETURN.
    ENDIF.

    IF NOT me->mo_current_player->mo_move_handler->handle_move(
         io_board    = me->mo_board
         io_move     = io_move ).
      " 028	This move was not valid. Please try again!
      MESSAGE e028 INTO dummy.
      ro_result->set_message_from_sy( ).
      RETURN.
    ENDIF.

    ro_result = me->mo_move_state_check->zif_chess_move_state~get_state(
                    io_board  = me->mo_board
                    io_player = me->mo_current_player ).

    IF ro_result->is_game_ending_move( ).
      io_move->set_moved_at( ).
      me->mo_game_result = ro_result.
      RETURN.
    ENDIF.

    IF NOT ro_result->is_legal_move( ).
      mo_board->undo_move( ).
      " same player must do another (legal) move
      RETURN.
    ENDIF.

    io_move->set_moved_at( ).

    me->swap_players( ).

  ENDMETHOD.


  METHOD play_move_current_player.

    IF me->mo_game_result IS BOUND AND me->mo_game_result->is_game_ending_move( ).
      " new moves are not possible anymore -> start a new game
      ro_result = me->mo_game_result.
      RETURN.
    ENDIF.

    ro_result = NEW #( ).

    DATA(lo_move) = me->mo_current_player->get_input( me->mo_board ).

    ro_result = me->play_move( lo_move ).

  ENDMETHOD.


  METHOD swap_players.

    CASE me->mo_current_player->mv_color.
      WHEN zif_chess_piece=>cs_color-white.
        me->mo_current_player = me->mo_black.
      WHEN zif_chess_piece=>cs_color-black.
        me->mo_current_player = me->mo_white.
    ENDCASE.

  ENDMETHOD.


  METHOD undo_move.

    me->mo_board->undo_move( ).

    me->swap_players( ).

  ENDMETHOD.
ENDCLASS.

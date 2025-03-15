class ZCL_CHESS_MOVE_STALEM_CHECKM definition
  public
  final
  create public .

public section.

  interfaces ZIF_CHESS_MOVE_STATE .

  methods CONSTRUCTOR
    raising
      ZCX_CHESS_EXCEPTION .
protected section.
private section.

  data MO_BOARD_UTILITY type ref to ZCL_CHESS_BOARD_UTILITY .
ENDCLASS.



CLASS ZCL_CHESS_MOVE_STALEM_CHECKM IMPLEMENTATION.


  METHOD constructor.
    me->mo_board_utility = NEW zcl_chess_board_utility( ).
  ENDMETHOD.


  METHOD zif_chess_move_state~get_state.

    ro_result = NEW #( ).

    DATA(lv_opponent_color) = zcl_chess_piece_utility=>get_opposite_color( io_player->mv_color ).

    DATA(lt_legal_moves) = me->mo_board_utility->get_legal_moves(
                             iv_color    = lv_opponent_color
                             io_board    = io_board ).

    IF lt_legal_moves IS NOT INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_king_square) = io_board->get_king_square( lv_opponent_color ).

    DATA(lv_color_name) = zcl_chess_piece_utility=>get_color_name( lv_opponent_color ).

    IF io_board->is_square_threatened(
                           io_square          = lo_king_square
                           iv_defending_color = lv_opponent_color ).

      " no legal moves and king is in check -> check mate
      ro_result->set_code( zif_chess_move_state=>cs_move_state_code-checkmate ).
      " 029	&1 King has been checkmated! The game ended
      MESSAGE e029 WITH lv_color_name INTO DATA(dummy).
      ro_result->set_message_from_sy( ).
    ELSE.
      " no legal moves and king is not in check -> stale mate
      ro_result->set_code( zif_chess_move_state=>cs_move_state_code-stalemate ).
      " 030	&1 King has been stalemated! The game ended
      MESSAGE e030 WITH lv_color_name INTO dummy.
      ro_result->set_message_from_sy( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

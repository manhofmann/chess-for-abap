class ZCL_CHESS_MOVE_CHECK definition
  public
  final
  create public .

public section.

  interfaces ZIF_CHESS_MOVE_STATE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CHESS_MOVE_CHECK IMPLEMENTATION.


  METHOD zif_chess_move_state~get_state.

    ro_result = NEW #( ).

    DATA(lo_king_square) = io_board->get_king_square( io_player->mv_color ).

    IF io_board->is_square_threatened(
                           io_square          = lo_king_square
                           iv_defending_color = io_player->mv_color ).

      " king is in check
      ro_result->set_code( zif_chess_move_state=>cs_move_state_code-check ).
      " 031	&1 King is in check!
      MESSAGE e031 WITH zcl_chess_piece_utility=>get_color_name( io_player->mv_color ) INTO DATA(dummy).
      ro_result->set_message_from_sy( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

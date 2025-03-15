class ZCL_CHESS_MOVE_HANDLER_ENPASSA definition
  public
  inheriting from ZCL_CHESS_MOVE_HANDLER
  final
  create public .

public section.

  methods ZIF_CHESS_MOVE_HANDLER~HANDLE_MOVE
    redefinition .
protected section.
private section.

  methods GET_EN_PASSANT_SQUARE
    importing
      !IO_MOVE type ref to ZIF_CHESS_MOVE
    returning
      value(RO_RESULT) type ref to ZIF_CHESS_SQUARE .
  methods IS_EN_PASSANT_MOVE
    importing
      !IO_MOVE type ref to ZIF_CHESS_MOVE
      !IO_BOARD type ref to ZIF_CHESS_BOARD_READ
    returning
      value(RV_RESULT) type BOOLE_D
    raising
      ZCX_CHESS_EXCEPTION .
ENDCLASS.



CLASS ZCL_CHESS_MOVE_HANDLER_ENPASSA IMPLEMENTATION.


  METHOD get_en_passant_square.

    ro_result = NEW zcl_chess_square(
                     iv_x  = io_move->mo_to->mv_x
                     iv_y  = io_move->mo_from->mv_y ).

  ENDMETHOD.


  METHOD is_en_passant_move.

    DATA lv_direction     TYPE int2.

    IF NOT io_board->is_inside_board( io_move->mo_from ) OR
       NOT io_board->is_inside_board( io_move->mo_to ).
      RETURN.
    ENDIF.

    DATA(lo_pawn) = io_board->get_piece( io_move->mo_from ).

    IF lo_pawn IS NOT BOUND.
      RETURN.
    ENDIF.

    IF lo_pawn->mv_type <> zif_chess_piece=>cs_type-pawn.
      RETURN.
    ENDIF.

    CASE lo_pawn->mv_color.
      WHEN zif_chess_piece=>cs_color-white.

        " white can only capture to rank #6
        IF io_move->mo_to->mv_y <> 6.
          RETURN.
        ENDIF.

        " white pawns can only move 'up' the board (= rank increases - Y axis)
        lv_direction = 1.

      WHEN zif_chess_piece=>cs_color-black.

        " white can only capture to rank #3
        IF io_move->mo_to->mv_y <> 3.
          RETURN.
        ENDIF.

        lv_direction = -1.

    ENDCASE.

    DATA(lv_x_distance) = io_move->get_x_distance( ).
    DATA(lv_y_distance) = io_move->get_y_distance( ).

    IF lv_y_distance <> lv_direction OR abs( lv_x_distance ) <> 1 OR NOT io_board->is_square_empty( io_move->mo_to ).
      RETURN.
    ENDIF.

    DATA(lo_enemy_pawn_square) = me->get_en_passant_square( io_move ).

    IF NOT io_board->is_inside_board( lo_enemy_pawn_square ).
      RETURN.
    ENDIF.

    DATA(lo_enemy_pawn) = io_board->get_piece( lo_enemy_pawn_square ).

    IF lo_enemy_pawn IS NOT BOUND.
      RETURN.
    ENDIF.

    IF lo_enemy_pawn->mv_type <> zif_chess_piece=>cs_type-pawn.
      RETURN.
    ENDIF.

    IF lo_enemy_pawn->mv_color = lo_pawn->mv_color.
      RETURN.
    ENDIF.

    IF lo_enemy_pawn->mv_move_amount <> 1.
      RETURN.
    ENDIF.

    READ TABLE io_board->mt_piece_moves
      INTO DATA(ls_last_move)
      INDEX lines( io_board->mt_piece_moves ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " en passant can only be done after the enemy pawn has moved
    " -> check if last move is really this enemy pawn:
    IF ls_last_move-piece <> lo_enemy_pawn.
      RETURN.
    ENDIF.

    rv_result = abap_true.

  ENDMETHOD.


  METHOD zif_chess_move_handler~handle_move.

    IF me->is_en_passant_move(
         io_move  = io_move
         io_board = io_board ).

      DATA(lo_square) = me->get_en_passant_square( io_move ).

      io_board->capture_piece(
          io_move   = io_move
          io_square = lo_square ).

      io_board->clear_square( lo_square ).

      io_move->set_move_type( zif_chess_move=>cs_type-en_passant ).

      " call method MOVE_PIECE after special moves, to be able to raise event properly -> e.g. update UI
      io_board->move_piece( io_move ).

      rv_result = abap_true.

    ELSE.
      rv_result = me->handle_next(
                    io_board    = io_board
                    io_move     = io_move ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

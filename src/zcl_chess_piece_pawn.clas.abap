class ZCL_CHESS_PIECE_PAWN definition
  public
  inheriting from ZCL_CHESS_PIECE
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_COLOR type ZCHESS_PIECE_COLOR .

  methods ZIF_CHESS_PIECE~GET_ALL_POSSIBLE_MOVES
    redefinition .
  methods ZIF_CHESS_PIECE~IS_MOVE_VALID
    redefinition .
  methods ZIF_CHESS_PIECE~GET_PIECE_VALUE
    redefinition .
protected section.
private section.

  methods GET_ENEMY_DIRECTION
    returning
      value(RV_RESULT) type INT2 .
ENDCLASS.



CLASS ZCL_CHESS_PIECE_PAWN IMPLEMENTATION.


  METHOD constructor.

    super->constructor( iv_color ).

    me->zif_chess_piece~mv_type = zif_chess_piece=>cs_type-pawn.

  ENDMETHOD.


  METHOD get_enemy_direction.

    IF me->zif_chess_piece~mv_color = zif_chess_piece=>cs_color-white.
      " white pawns can only move 'up' the board (= rank increases - Y axis)
      rv_result = 1.
    ELSE.
      rv_result = -1.
    ENDIF.

  ENDMETHOD.


  METHOD zif_chess_piece~get_all_possible_moves.

    DATA lv_x TYPE int2.

    DATA(lv_direction) = me->get_enemy_direction( ).

    IF NOT me->zif_chess_piece~has_been_moved( ).

      INSERT NEW zcl_chess_move(
                  io_from = io_square
                  io_to   = NEW zcl_chess_square(
                                  iv_x = io_square->mv_x
                                  iv_y = io_square->mv_y + 2 * lv_direction ) ) INTO TABLE rt_result.

    ENDIF.

    DO 3 TIMES.

      lv_x = io_square->mv_x + ( sy-index - 2 ).

      IF NOT zcl_chess_square_utility=>is_inside_board(
                        iv_x = lv_x
                        iv_y = io_square->mv_y ).
        CONTINUE.
      ENDIF.

      DATA(lo_to) = NEW zcl_chess_square(
                          iv_x = CONV #( lv_x )
                          iv_y = io_square->mv_y + lv_direction ).

      INSERT NEW zcl_chess_move(
                  io_from = io_square
                  io_to   = lo_to ) INTO TABLE rt_result.

    ENDDO.

  ENDMETHOD.


  METHOD zif_chess_piece~get_piece_value.
    rv_result = zif_chess_piece=>cs_value-pawn.
  ENDMETHOD.


  METHOD zif_chess_piece~is_move_valid.

    IF NOT super->zif_chess_piece~is_move_valid(
         io_move  = io_move
         io_board = io_board ).
      RETURN.
    ENDIF.

    DATA(lv_x_distance) = io_move->get_x_distance( ).
    DATA(lv_y_distance) = io_move->get_y_distance( ).

    DATA(lv_direction) = me->get_enemy_direction( ).

    " normal pawn move: one square in 'direction of enemy':
    IF lv_x_distance = 0 AND lv_y_distance = lv_direction AND io_board->is_square_empty( io_move->mo_to ).
      rv_result = abap_true.
      RETURN.
    ENDIF.

    " first move of a pawn can be two squares:
    IF lv_x_distance = 0 AND lv_y_distance = 2 * lv_direction AND io_board->is_square_empty( io_move->mo_to ) AND NOT me->zif_chess_piece~has_been_moved( ).

      " check if square in front of pawn is empty:
      DATA(lo_square) = NEW zcl_chess_square(
                              iv_x = io_move->mo_from->mv_x
                              iv_y = io_move->mo_from->mv_y + lv_direction ).

      IF io_board->is_square_empty( lo_square ).
        rv_result = abap_true.
        RETURN.
      ENDIF.

    ENDIF.

    " normal capture move:
    IF abs( lv_x_distance ) = 1 AND lv_y_distance = lv_direction AND NOT io_board->is_square_empty( io_move->mo_to ).
      rv_result = abap_true.
      RETURN.
    ENDIF.

    rv_result = abap_false.

  ENDMETHOD.
ENDCLASS.

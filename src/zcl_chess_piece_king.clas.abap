class ZCL_CHESS_PIECE_KING definition
  public
  inheriting from ZCL_CHESS_PIECE
  final
  create public .

public section.

  class-data GT_MOVES_TABLE type TTY_MOVES_TABLE read-only .

  class-methods CLASS_CONSTRUCTOR .
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
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_CHESS_PIECE_KING IMPLEMENTATION.


  METHOD class_constructor.

    " a king can move like a rook and bishop (but only one square)
    INSERT LINES OF zcl_chess_piece_bishop=>gt_moves_table INTO TABLE gt_moves_table.
    INSERT LINES OF zcl_chess_piece_rook=>gt_moves_table   INTO TABLE gt_moves_table.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( iv_color ).

    me->zif_chess_piece~mv_type = zif_chess_piece=>cs_type-king.

  ENDMETHOD.


  METHOD zif_chess_piece~get_all_possible_moves.

    DATA lo_move TYPE REF TO zif_chess_move.

    IF NOT me->zif_chess_piece~has_been_moved( ).

      " two possible castling moves:
      " king side
      lo_move = NEW zcl_chess_move(
                      io_from = io_square
                      io_to   = NEW zcl_chess_square(
                                      iv_x = 8
                                      iv_y = io_square->mv_y ) ).

      lo_move->set_move_type( zif_chess_move=>cs_type-castling ).
      INSERT lo_move INTO TABLE rt_result.

      " queen side
      lo_move = NEW zcl_chess_move(
                      io_from = io_square
                      io_to   = NEW zcl_chess_square(
                                      iv_x = 1
                                      iv_y = io_square->mv_y ) ).

      lo_move->set_move_type( zif_chess_move=>cs_type-castling ).
      INSERT lo_move INTO TABLE rt_result.

      " we do not check if rook has been moved, this is done later ...

    ENDIF.

    rt_result = me->calculate_possible_moves(
                  io_square      = io_square
                  iv_times       = 1
                  it_moves_table = gt_moves_table ).

  ENDMETHOD.


  METHOD zif_chess_piece~get_piece_value.
    rv_result = zif_chess_piece=>cs_value-king.
  ENDMETHOD.


  METHOD zif_chess_piece~is_move_valid.

    DATA lv_x_distance    TYPE int2.
    DATA lv_y_distance    TYPE int2.

    IF NOT super->zif_chess_piece~is_move_valid(
         io_move  = io_move
         io_board = io_board ).
      RETURN.
    ENDIF.

    lv_x_distance = io_move->mo_to->mv_x - io_move->mo_from->mv_x.
    lv_y_distance = io_move->mo_to->mv_y - io_move->mo_from->mv_y.

    IF abs( lv_x_distance ) <= 1 AND abs( lv_y_distance ) <= 1.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

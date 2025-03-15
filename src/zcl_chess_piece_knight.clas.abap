class ZCL_CHESS_PIECE_KNIGHT definition
  public
  inheriting from ZCL_CHESS_PIECE
  final
  create public .

public section.

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
    CLASS-DATA gt_moves_table TYPE tty_moves_table.

ENDCLASS.



CLASS ZCL_CHESS_PIECE_KNIGHT IMPLEMENTATION.


  METHOD class_constructor.

    gt_moves_table  = VALUE #( ( x =  2  y =  1 )
                               ( x =  1  y =  2 )
                               ( x = -1  y =  2 )
                               ( x = -2  y =  1 )
                               ( x = -2  y = -1 )
                               ( x = -1  y = -2 )
                               ( x =  1  y = -2 )
                               ( x =  2  y = -1 ) ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( iv_color ).

    me->zif_chess_piece~mv_type = zif_chess_piece=>cs_type-knight.

  ENDMETHOD.


  METHOD zif_chess_piece~get_all_possible_moves.

    rt_result = me->calculate_possible_moves(
                  io_square      = io_square
                  iv_times       = 1
                  it_moves_table = gt_moves_table ).

  ENDMETHOD.


  METHOD zif_chess_piece~get_piece_value.
    rv_result = zif_chess_piece=>cs_value-knight.
  ENDMETHOD.


  METHOD zif_chess_piece~is_move_valid.

    DATA lv_x_distance  TYPE int2.
    DATA lv_y_distance  TYPE int2.

    IF NOT super->zif_chess_piece~is_move_valid(
         io_move  = io_move
         io_board = io_board ).
      RETURN.
    ENDIF.

    lv_x_distance = io_move->mo_to->mv_x - io_move->mo_from->mv_x.
    lv_y_distance = io_move->mo_to->mv_y - io_move->mo_from->mv_y.

    rv_result = xsdbool( abs( lv_x_distance * lv_y_distance ) = 2 ).

  ENDMETHOD.
ENDCLASS.

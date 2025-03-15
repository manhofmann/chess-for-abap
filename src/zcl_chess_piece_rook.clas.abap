class ZCL_CHESS_PIECE_ROOK definition
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



CLASS ZCL_CHESS_PIECE_ROOK IMPLEMENTATION.


  METHOD class_constructor.

    " all possible horizontal/vertical moves:
    gt_moves_table  = VALUE #( ( x =  0  y =  1 )
                               ( x =  1  y =  0 )
                               ( x = -1  y =  0 )
                               ( x =  0  y = -1 ) ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( iv_color ).

    me->zif_chess_piece~mv_type = zif_chess_piece=>cs_type-rook.

  ENDMETHOD.


  METHOD zif_chess_piece~get_all_possible_moves.

    rt_result = me->calculate_possible_moves(
                  io_square      = io_square
                  iv_times       = 7 " rook can move max 7 squares
                  it_moves_table = gt_moves_table ).

  ENDMETHOD.


  METHOD zif_chess_piece~get_piece_value.
    rv_result = zif_chess_piece=>cs_value-rook.
  ENDMETHOD.


  METHOD zif_chess_piece~is_move_valid.

    IF NOT super->zif_chess_piece~is_move_valid(
         io_move  = io_move
         io_board = io_board ).
      RETURN.
    ENDIF.

    rv_result = me->is_horizon_vertical_move_valid(
                  io_move  = io_move
                  io_board = io_board ).

  ENDMETHOD.
ENDCLASS.

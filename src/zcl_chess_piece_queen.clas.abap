class ZCL_CHESS_PIECE_QUEEN definition
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
    CLASS-DATA gt_moves_table TYPE tty_moves_table.
ENDCLASS.



CLASS ZCL_CHESS_PIECE_QUEEN IMPLEMENTATION.


  METHOD constructor.

    super->constructor( iv_color ).

    me->zif_chess_piece~mv_type = zif_chess_piece=>cs_type-queen.

  ENDMETHOD.


  METHOD zif_chess_piece~get_all_possible_moves.

    rt_result = me->calculate_possible_moves(
                  io_square      = io_square
                  iv_times       = 7 " queen can move up to seven squares
                  it_moves_table = zcl_chess_piece_king=>gt_moves_table ).

  ENDMETHOD.


  METHOD zif_chess_piece~get_piece_value.
    rv_result = zif_chess_piece=>cs_value-queen.
  ENDMETHOD.


  METHOD zif_chess_piece~is_move_valid.

    IF NOT super->zif_chess_piece~is_move_valid(
         io_move  = io_move
         io_board = io_board ).
      RETURN.
    ENDIF.

    IF me->is_horizon_vertical_move_valid(
                  io_move  = io_move
                  io_board = io_board ).
      rv_result = abap_true.
      RETURN.
    ENDIF.

    IF me->is_diagonal_move_valid(
                  io_move  = io_move
                  io_board = io_board ).
      rv_result = abap_true.
      RETURN.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

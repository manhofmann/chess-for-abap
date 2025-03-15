class ZCL_CHESS_MOVE definition
  public
  final
  create public .

public section.

  interfaces IF_SERIALIZABLE_OBJECT .
  interfaces ZIF_CHESS_MOVE .

  methods CONSTRUCTOR
    importing
      !IO_FROM type ref to ZIF_CHESS_SQUARE
      !IO_TO type ref to ZIF_CHESS_SQUARE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CHESS_MOVE IMPLEMENTATION.


  METHOD constructor.
    me->zif_chess_move~mo_from = io_from.
    me->zif_chess_move~mo_to   = io_to.
  ENDMETHOD.


  METHOD zif_chess_move~get_x_direction.

    DATA(lv_x_distance) = me->zif_chess_move~get_x_distance( ).

    rv_result = lv_x_distance / abs( lv_x_distance ).

  ENDMETHOD.


  METHOD zif_chess_move~get_x_distance.
    rv_result = me->zif_chess_move~mo_to->mv_x - me->zif_chess_move~mo_from->mv_x.
  ENDMETHOD.


  METHOD zif_chess_move~get_y_direction.

    DATA(lv_y_distance) = me->zif_chess_move~get_y_distance( ).

    rv_result = lv_y_distance / abs( lv_y_distance ).

  ENDMETHOD.


  METHOD zif_chess_move~get_y_distance.
    rv_result = me->zif_chess_move~mo_to->mv_y - me->zif_chess_move~mo_from->mv_y.
  ENDMETHOD.


  METHOD zif_chess_move~set_capture_piece.

    IF me->zif_chess_move~mo_captured_piece IS BOUND AND
       me->zif_chess_move~mo_captured_piece->mv_type <> io_piece->mv_type.
      " 014	Captured piece in move must not be changed
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e014.
    ENDIF.

    IF me->zif_chess_move~mo_captured_square IS BOUND AND
       me->zif_chess_move~mo_captured_square->mv_name <> io_square->mv_name.
      " 047	Capture sqaure in move can only be set once
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e047.
    ENDIF.

    " we need to store where the piece was captured as well to undo the move
    " (en passant captured square is not where the attacker will land)
    me->zif_chess_move~mo_captured_square = io_square.
    me->zif_chess_move~mo_captured_piece  = io_piece.

  ENDMETHOD.


  METHOD zif_chess_move~set_moved_at.
    GET TIME STAMP FIELD me->zif_chess_move~mv_moved_at.
  ENDMETHOD.


  METHOD zif_chess_move~set_move_type.

    IF me->zif_chess_move~mv_type IS NOT INITIAL AND
       me->zif_chess_move~mv_type <> iv_type.
      " 016	Move type can only be set once
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e016.
    ENDIF.

    me->zif_chess_move~mv_type = iv_type.

  ENDMETHOD.


  METHOD zif_chess_move~set_notation.
    me->zif_chess_move~mv_notation = iv_notation.
  ENDMETHOD.


  METHOD zif_chess_move~set_promotion_piece.

    IF me->zif_chess_move~mo_promoted_piece IS BOUND AND
       me->zif_chess_move~mo_promoted_piece->mv_type <> io_piece->mv_type.
      " 015	Promoted piece in move must not change its type
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e015.
    ENDIF.

    me->zif_chess_move~mo_promoted_piece = io_piece.

  ENDMETHOD.
ENDCLASS.

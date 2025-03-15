class ZCL_CHESS_MOVE_LIST definition
  public
  final
  create public .

public section.

  methods GET
    importing
      !IO_BOARD type ref to ZIF_CHESS_BOARD_READ
    returning
      value(RT_RESULT) type ZCHESS_TT_MOVE_LIST_ALV
    raising
      ZCX_CHESS_EXCEPTION .
protected section.
private section.

  methods GET_MOVE_NOTATION
    importing
      !IS_PIECE_MOVE type ZCHESS_PIECE_MOVE
      !IO_BOARD type ref to ZIF_CHESS_BOARD_READ
    returning
      value(RV_RESULT) type ZCHESS_MOVE_NOTATION .
ENDCLASS.



CLASS ZCL_CHESS_MOVE_LIST IMPLEMENTATION.


  METHOD get.

    DATA lv_turn_number               TYPE zchess_move_list_turn_number VALUE 1.
    DATA ls_previous                  TYPE zchess_piece_move.
    FIELD-SYMBOLS <ls_move_list>      TYPE zchess_move_list_alv.
    FIELD-SYMBOLS <lv_move_notation>  TYPE zchess_move_notation.

    LOOP AT io_board->mt_piece_moves ASSIGNING FIELD-SYMBOL(<ls_piece_move>).

      DATA(lv_castling_skip) = xsdbool( <ls_piece_move>-move->mv_type     = zif_chess_move=>cs_type-castling AND
                                        <ls_piece_move>-piece->mv_color   = ls_previous-piece->mv_color      AND
                                        <ls_piece_move>-piece->mv_type    = zif_chess_piece=>cs_type-rook ).

      IF lv_castling_skip = abap_true.
        " do not show rook castling move
        CONTINUE.
      ENDIF.

      CASE <ls_piece_move>-piece->mv_color.
        WHEN zif_chess_piece=>cs_color-white.

          INSERT VALUE #( turn_number = lv_turn_number ) INTO TABLE rt_result ASSIGNING <ls_move_list>.

          ASSIGN COMPONENT 'WHITE_MOVE' OF STRUCTURE <ls_move_list> TO <lv_move_notation>.

        WHEN zif_chess_piece=>cs_color-black.

          ASSIGN COMPONENT 'BLACK_MOVE' OF STRUCTURE <ls_move_list> TO <lv_move_notation>.

          lv_turn_number = lv_turn_number + 1.

      ENDCASE.


      IF <ls_piece_move>-move->mv_notation IS INITIAL.
        " only calculate latest move which has notation not set
        " because we need the current board state to be able
        " to properly set e.g. check or disambiguating moves
        <lv_move_notation> = me->get_move_notation(
                               is_piece_move = <ls_piece_move>
                               io_board      = io_board ).
        <ls_piece_move>-move->set_notation( <lv_move_notation> ).
      ELSE.
        " if notation is already set we just copy it
        <lv_move_notation> = <ls_piece_move>-move->mv_notation.
      ENDIF.

      " remember previous move, because castling is technically two moves
      ls_previous = <ls_piece_move>.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_move_notation.

    " TODO: Disambiguating moves, check, checkmate

    CASE is_piece_move-piece->mv_type.
      WHEN zif_chess_piece=>cs_type-pawn.
        rv_result = to_lower( is_piece_move-move->mo_to->mv_name ).
      WHEN OTHERS.
        rv_result = is_piece_move-piece->mv_type.
    ENDCASE.

    CASE is_piece_move-piece->mv_type.
      WHEN zif_chess_piece=>cs_type-pawn.

        IF is_piece_move-move->mo_captured_piece IS BOUND.
          rv_result = |{ to_lower( is_piece_move-move->mo_from->mv_name(1) ) }x{ to_lower( is_piece_move-move->mo_to->mv_name ) }|.
        ENDIF.

        IF is_piece_move-move->mv_type = zif_chess_move=>cs_type-promotion.
          rv_result = |{ rv_result }={ is_piece_move-move->mo_promoted_piece->mv_type }|.
        ENDIF.

        IF is_piece_move-move->mv_type = zif_chess_move=>cs_type-en_passant.
          rv_result = |{ rv_result } e.p.|.
        ENDIF.

      WHEN OTHERS.

        IF is_piece_move-move->mo_captured_piece IS BOUND.
          rv_result = |{ rv_result }x|.
        ENDIF.

        rv_result = |{ rv_result }{ to_lower( is_piece_move-move->mo_to->mv_name ) }|.

    ENDCASE.

    IF is_piece_move-move->mv_type = zif_chess_move=>cs_type-castling.

      IF is_piece_move-move->get_x_distance( ) > 0.
        " castling king side
        rv_result = 'O-O'.
      ELSE.
        " castling queen side
        rv_result = 'O-O-O'.
      ENDIF.

    ENDIF.

    " TODO: checkmate
    DATA(lv_opponent_color) = zcl_chess_piece_utility=>get_opposite_color( is_piece_move-piece->mv_color ).

    DATA(lo_king_square) = io_board->get_king_square( lv_opponent_color ).

    IF io_board->is_square_threatened(
         io_square          = lo_king_square
         iv_defending_color = lv_opponent_color ).
      rv_result = |{ rv_result }+|.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

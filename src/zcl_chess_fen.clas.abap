class ZCL_CHESS_FEN definition
  public
  final
  create public .

public section.

  methods GET_NOTATION
    importing
      !IO_BOARD type ref to ZIF_CHESS_BOARD_READ
      !IO_CURRENT_PLAYER type ref to ZIF_CHESS_PLAYER
    returning
      value(RV_RESULT) type ZCHESS_FORSYTH_EDWARDS_NOTATIO
    raising
      ZCX_CHESS_EXCEPTION .
protected section.
private section.

  methods GET_CASTLING_NOTATION
    importing
      !IO_BOARD type ref to ZIF_CHESS_BOARD_READ
    returning
      value(RV_RESULT) type ZCHESS_FORSYTH_EDWARDS_NOTATIO
    raising
      ZCX_CHESS_EXCEPTION .
  methods GET_CASTLING_NOTATION_COLOR
    importing
      !IO_BOARD type ref to ZIF_CHESS_BOARD_READ
      !IV_COLOR type ZCHESS_PIECE_COLOR
    returning
      value(RV_RESULT) type ZCHESS_FORSYTH_EDWARDS_NOTATIO
    raising
      ZCX_CHESS_EXCEPTION .
  methods GET_EN_PASSANT_NOTATION
    importing
      !IO_BOARD type ref to ZIF_CHESS_BOARD_READ
    returning
      value(RV_RESULT) type ZCHESS_FORSYTH_EDWARDS_NOTATIO
    raising
      ZCX_CHESS_EXCEPTION .
  methods GET_FULL_MOVE_NOTATION
    importing
      !IO_BOARD type ref to ZIF_CHESS_BOARD_READ
    returning
      value(RV_RESULT) type ZCHESS_FORSYTH_EDWARDS_NOTATIO
    raising
      ZCX_CHESS_EXCEPTION .
  methods GET_HALF_MOVE_NOTATION
    importing
      !IO_BOARD type ref to ZIF_CHESS_BOARD_READ
    returning
      value(RV_RESULT) type ZCHESS_FORSYTH_EDWARDS_NOTATIO
    raising
      ZCX_CHESS_EXCEPTION .
  methods GET_PIECE_NOTATION
    importing
      !IO_PIECE type ref to ZIF_CHESS_PIECE
    returning
      value(RV_RESULT) type ZCHESS_FORSYTH_EDWARDS_NOTATIO
    raising
      ZCX_CHESS_EXCEPTION .
  methods GET_PIECE_PLACEMENT
    importing
      !IO_BOARD type ref to ZIF_CHESS_BOARD_READ
    returning
      value(RV_RESULT) type ZCHESS_FORSYTH_EDWARDS_NOTATIO
    raising
      ZCX_CHESS_EXCEPTION .
  methods GET_PIECE_PLACEMENT_RANK
    importing
      !IO_BOARD type ref to ZIF_CHESS_BOARD_READ
      !IV_RANK type ZCHESS_RANK_NUMBER
    returning
      value(RV_RESULT) type ZCHESS_FORSYTH_EDWARDS_NOTATIO
    raising
      ZCX_CHESS_EXCEPTION .
ENDCLASS.



CLASS ZCL_CHESS_FEN IMPLEMENTATION.


  METHOD get_castling_notation.

    DATA(lv_white_castling) = me->get_castling_notation_color(
                                io_board = io_board
                                iv_color = zif_chess_piece=>cs_color-white ).

    DATA(lv_black_castling) = me->get_castling_notation_color(
                                io_board = io_board
                                iv_color = zif_chess_piece=>cs_color-black ).

    IF lv_white_castling = '-' AND
       lv_black_castling = '-'.
      rv_result = '-'.
      RETURN.
    ENDIF.

    IF lv_white_castling <> '-'.
      rv_result = |{ rv_result }{ lv_white_castling }|.
    ENDIF.

    IF lv_black_castling <> '-'.
      rv_result = |{ rv_result }{ lv_black_castling }|.
    ENDIF.

  ENDMETHOD.


  METHOD get_castling_notation_color.

    DATA lv_rank TYPE zchess_rank_number.

    DATA(lo_king_square) = io_board->get_king_square( iv_color ).

    DATA(lo_king) = io_board->get_piece( lo_king_square ).

    ASSERT lo_king IS BOUND.

    IF lo_king->has_been_moved( ).
      rv_result = '-'. " no castling possible
      RETURN.
    ENDIF.

    IF iv_color = zif_chess_piece=>cs_color-white.
      lv_rank = 1.
    ELSE.
      lv_rank = 8.
    ENDIF.

    DATA(lo_king_side) = zcl_chess_square_factory=>create_square_by_name( |H{ lv_rank STYLE = SIMPLE }| ).

    DATA(lo_king_side_rook) = io_board->get_piece( lo_king_side ).

    IF lo_king_side_rook IS BOUND AND NOT lo_king_side_rook->has_been_moved( ).
      rv_result = |{ rv_result }K|. " king side castling is possible
    ENDIF.

    DATA(lo_queen_side) = zcl_chess_square_factory=>create_square_by_name( |A{ lv_rank STYLE = SIMPLE }| ).

    DATA(lo_queen_side_rook) = io_board->get_piece( lo_queen_side ).

    IF lo_queen_side_rook IS BOUND AND NOT lo_queen_side_rook->has_been_moved( ).
      rv_result = |{ rv_result }Q|. " queen side castling is possible
    ENDIF.

    IF iv_color = zif_chess_piece=>cs_color-white.
      rv_result = to_upper( rv_result ).
    ELSE.
      rv_result = to_lower( rv_result ).
    ENDIF.

  ENDMETHOD.


  METHOD get_en_passant_notation.

    " default/fallback
    rv_result = '-'.

    " read last move
    READ TABLE io_board->mt_piece_moves
      INTO DATA(ls_piece_move)
      INDEX lines( io_board->mt_piece_moves ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF ls_piece_move-piece->mv_type <> zif_chess_piece=>cs_type-pawn.
      RETURN.
    ENDIF.

    IF abs( ls_piece_move-move->get_y_distance( ) ) < 2.
      RETURN.
    ENDIF.

    " pawn moved two square and it is now a possible target of 'en passant' capture
    " -> add square 'behind' pawn in algebraic notation (= arithmetic mean of y coordinates from move )

    DATA(lo_square) = NEW zcl_chess_square(
                            iv_x = ls_piece_move-move->mo_from->mv_x
                            iv_y = ( ls_piece_move-move->mo_from->mv_y + ls_piece_move-move->mo_to->mv_y ) / 2 ).

    rv_result = to_lower( lo_square->zif_chess_square~mv_name ).

  ENDMETHOD.


  METHOD get_full_move_notation.

    DATA lv_full_moves TYPE int2 VALUE 1. " full moves start at one

    " this methods returns the half moves since last pawn move or capture

    " read table backwards
    LOOP AT io_board->mt_piece_moves
      ASSIGNING FIELD-SYMBOL(<ls_piece_moves>)
      WHERE piece->mv_color = zif_chess_piece=>cs_color-black.

      IF <ls_piece_moves>-move->mv_type  = zif_chess_move=>cs_type-castling AND
         <ls_piece_moves>-piece->mv_type = zif_chess_piece=>cs_type-rook.

        " castling are actually two entries in table PIECE_MOVES:
        " 1) king
        " 2) rook
        " we ignore for half moves the 'rook castling' move
        CONTINUE.
      ENDIF.

      lv_full_moves = lv_full_moves + 1.

    ENDLOOP.

    rv_result = |{ lv_full_moves STYLE = SIMPLE }|.

  ENDMETHOD.


  METHOD get_half_move_notation.

    DATA lv_half_moves TYPE int2.

    " this methods returns the half moves since last pawn move or capture

    " read table backwards
    DO lines( io_board->mt_piece_moves ) TIMES.

      DATA(lv_index) = lines( io_board->mt_piece_moves ) - sy-index + 1.

      READ TABLE io_board->mt_piece_moves
        ASSIGNING FIELD-SYMBOL(<ls_piece_moves>)
        INDEX lv_index.

      IF <ls_piece_moves>-piece->mv_type = zif_chess_piece=>cs_type-pawn.
        EXIT.
      ENDIF.

      IF <ls_piece_moves>-move->mo_captured_piece IS BOUND.
        EXIT.
      ENDIF.

      IF <ls_piece_moves>-move->mv_type  = zif_chess_move=>cs_type-castling AND
         <ls_piece_moves>-piece->mv_type = zif_chess_piece=>cs_type-rook.

        " castling are actually two entries in table PIECE_MOVES:
        " 1) king
        " 2) rook
        " we ignore for half moves the 'rook castling' move
        CONTINUE.
      ENDIF.

      lv_half_moves = lv_half_moves + 1.

    ENDDO.

    rv_result = |{ lv_half_moves STYLE = SIMPLE }|.

  ENDMETHOD.


  METHOD get_notation.

    rv_result = me->get_piece_placement( io_board ).

    rv_result = |{ rv_result } { to_lower( io_current_player->mv_color ) }|.
    rv_result = |{ rv_result } { me->get_castling_notation( io_board ) }|.
    rv_result = |{ rv_result } { me->get_en_passant_notation( io_board ) }|.
    rv_result = |{ rv_result } { me->get_half_move_notation( io_board ) }|.
    rv_result = |{ rv_result } { me->get_full_move_notation( io_board ) }|.

  ENDMETHOD.


  METHOD get_piece_notation.

    IF io_piece->mv_color = zif_chess_piece=>cs_color-white.
      rv_result = to_upper( io_piece->mv_type ).
    ELSE.
      rv_result = to_lower( io_piece->mv_type ).
    ENDIF.

  ENDMETHOD.


  METHOD get_piece_placement.

    DO zif_chess_board=>cv_number_ranks TIMES.

      " notation starts by rank 8: we count down from 8 to 1
      DATA(lv_rank) = zif_chess_board=>cv_number_ranks - sy-index + 1.

      DATA(lv_piece_placement_rank) = me->get_piece_placement_rank(
                                        io_board = io_board
                                        iv_rank  = CONV #( lv_rank ) ).

      rv_result = |{ rv_result }{ lv_piece_placement_rank }|.

      " first rank does not need /
      IF lv_rank <> 1.
        rv_result = |{ rv_result }/|.
      ENDIF.

    ENDDO.

  ENDMETHOD.


  METHOD get_piece_placement_rank.

    DATA lv_empty_squares TYPE int1.

    DO zif_chess_board=>cv_number_files TIMES.

      DATA(lv_file) = sy-index.

      DATA(lo_square) = NEW zcl_chess_square(
                              iv_y = iv_rank
                              iv_x = CONV #( lv_file ) ).

      DATA(lo_piece) = io_board->get_piece( lo_square ).

      IF lo_piece IS BOUND.

        IF lv_empty_squares IS NOT INITIAL.
          rv_result = |{ rv_result }{ lv_empty_squares STYLE = SIMPLE }|.
        ENDIF.

        lv_empty_squares = 0.

        rv_result = |{ rv_result }{ me->get_piece_notation( lo_piece ) }|.

      ELSE.
        lv_empty_squares = lv_empty_squares + 1.
      ENDIF.

    ENDDO.

    IF lv_empty_squares IS NOT INITIAL.
      rv_result = |{ rv_result }{ lv_empty_squares STYLE = SIMPLE }|.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

class ZCL_CHESS_BOARD_UTILITY definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    raising
      ZCX_CHESS_EXCEPTION .
  methods GET_LEGAL_MOVES
    importing
      !IV_COLOR type ZCHESS_PIECE_COLOR
      !IV_WITH_PROMOTION_MOVES type BOOLE_D default ABAP_FALSE
      !IO_BOARD type ref to ZIF_CHESS_BOARD_READ
    returning
      value(RT_RESULT) type ZCHESS_TT_MOVE
    raising
      ZCX_CHESS_EXCEPTION .
  methods GET_LEGAL_MOVES_WITHOUT_COPY
    importing
      !IV_COLOR type ZCHESS_PIECE_COLOR
      !IV_WITH_PROMOTION_MOVES type BOOLE_D default ABAP_FALSE
      !IO_BOARD type ref to ZIF_CHESS_BOARD
    returning
      value(RT_RESULT) type ZCHESS_TT_MOVE
    raising
      ZCX_CHESS_EXCEPTION .
protected section.
private section.

  data MO_MOVE_HANDLER type ref to ZIF_CHESS_MOVE_HANDLER .
  data MO_PIECE_FACTORY type ref to ZCL_CHESS_PIECE_FACTORY .

  methods INSERT_ALL_PROMOTION_MOVES
    importing
      !IO_MOVE type ref to ZIF_CHESS_MOVE
    returning
      value(RT_RESULT) type ZCHESS_TT_MOVE
    raising
      ZCX_CHESS_EXCEPTION .
ENDCLASS.



CLASS ZCL_CHESS_BOARD_UTILITY IMPLEMENTATION.


  METHOD constructor.
    me->mo_move_handler   = mo_move_handler = zcl_chess_move_handler=>create_move_handler( ).
    me->mo_piece_factory  = NEW #( ).
  ENDMETHOD.


  METHOD get_legal_moves.

    DATA(lo_board_copy) = io_board->create_copy( ).

    rt_result = me->get_legal_moves_without_copy(
                  iv_color                = iv_color
                  iv_with_promotion_moves = iv_with_promotion_moves
                  io_board                = lo_board_copy ).

  ENDMETHOD.


  METHOD get_legal_moves_without_copy.

    " check all pieces:
    LOOP AT io_board->mt_pieces_on_squares
      INTO DATA(lo_piece)
      WHERE table_line IS BOUND
      AND   table_line->mv_color = iv_color.

      DATA(lv_index) = sy-tabix.

      READ TABLE io_board->mt_squares
        INTO DATA(lo_square)
        INDEX lv_index.

      ASSERT sy-subrc = 0.

      DATA(lt_moves) = lo_piece->get_all_possible_moves( lo_square ).

      LOOP AT lt_moves INTO DATA(lo_move).

        lo_move->set_promotion_piece( me->mo_piece_factory->create_piece(
                                        iv_color = iv_color
                                        iv_type  = zif_chess_piece=>cs_type-queen ) ).

        IF me->mo_move_handler->handle_move(
             io_board    = io_board
             io_move     = lo_move ).

          DATA(lo_king_square) = io_board->get_king_square( iv_color ).

          IF NOT io_board->is_square_threatened(
               io_square          = lo_king_square
               iv_defending_color = iv_color ).

            IF iv_with_promotion_moves = abap_true.
              INSERT LINES OF me->insert_all_promotion_moves( lo_move ) INTO TABLE rt_result.
            ELSE.
              INSERT lo_move INTO TABLE rt_result.
            ENDIF.

          ENDIF.

          io_board->undo_move( ).

        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD insert_all_promotion_moves.

    INSERT io_move INTO TABLE rt_result.

    IF io_move->mv_type <> zif_chess_move=>cs_type-promotion.
      RETURN.
    ENDIF.

    " add all possible other promotion pieces: rook, knight, bishop
    " queen should be set already in IO_MOVE

    DATA(lo_move) = NEW zcl_chess_move(
                          io_from = io_move->mo_from
                          io_to   = io_move->mo_to ).

    lo_move->zif_chess_move~set_promotion_piece( me->mo_piece_factory->create_piece(
                                            iv_color = io_move->mo_promoted_piece->mv_color
                                            iv_type  = zif_chess_piece=>cs_type-rook ) ).

    INSERT lo_move INTO TABLE rt_result.

    lo_move = NEW zcl_chess_move(
                    io_from = io_move->mo_from
                    io_to   = io_move->mo_to ).

    lo_move->zif_chess_move~set_promotion_piece( me->mo_piece_factory->create_piece(
                                            iv_color = io_move->mo_promoted_piece->mv_color
                                            iv_type  = zif_chess_piece=>cs_type-knight ) ).

    INSERT lo_move INTO TABLE rt_result.

    lo_move = NEW zcl_chess_move(
                    io_from = io_move->mo_from
                    io_to   = io_move->mo_to ).

    lo_move->zif_chess_move~set_promotion_piece( me->mo_piece_factory->create_piece(
                                            iv_color = io_move->mo_promoted_piece->mv_color
                                            iv_type  = zif_chess_piece=>cs_type-bishop ) ).

    INSERT lo_move INTO TABLE rt_result.

  ENDMETHOD.
ENDCLASS.

class ZCL_CHESS_BOARD definition
  public
  create public .

public section.

  interfaces ZIF_CHESS_BOARD_READ .
  interfaces ZIF_CHESS_BOARD .
protected section.

  methods GET_TABLE_INDEX
    importing
      !IO_SQUARE type ref to ZIF_CHESS_SQUARE
    returning
      value(RV_RESULT) type SYST_INDEX .
private section.
ENDCLASS.



CLASS ZCL_CHESS_BOARD IMPLEMENTATION.


  METHOD get_table_index.
    rv_result = zif_chess_board=>cv_number_ranks * ( io_square->mv_y - 1 ) + io_square->mv_x.
  ENDMETHOD.


  METHOD zif_chess_board_read~create_copy.

    DATA lv_xstring TYPE xstring.

    TRY.

        CALL TRANSFORMATION id
          SOURCE object = me
          RESULT XML lv_xstring.

        CALL TRANSFORMATION id
          SOURCE XML lv_xstring
          RESULT object = ro_result.

      CATCH cx_transformation_error INTO DATA(lx_exception).
        " 036	Error during CALL TRANSFORMATION
        RAISE EXCEPTION TYPE zcx_chess_exception
          MESSAGE e036
          EXPORTING
            previous = lx_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_chess_board_read~get_king_square.

    LOOP AT me->zif_chess_board_read~mt_pieces_on_squares
      TRANSPORTING NO FIELDS
      WHERE table_line IS BOUND
      AND   table_line->mv_color = iv_color
      AND   table_line->mv_type  = zif_chess_piece=>cs_type-king.

      DATA(lv_index) = sy-tabix.
      EXIT.

    ENDLOOP.

    IF sy-subrc <> 0.
      " 020	King of color &1 is missing on board
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e020 WITH iv_color.
    ENDIF.

    TRY.
        ro_result = me->zif_chess_board_read~mt_squares[ lv_index ].
      CATCH cx_sy_itab_line_not_found.
        " 021	Invalid index &1. Square can not be found
        RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e021 WITH |{ lv_index STYLE = SIMPLE }|.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_chess_board_read~get_piece.

    TRY.
        ro_result = me->zif_chess_board~mt_pieces_on_squares[ me->get_table_index( io_square ) ].
      CATCH cx_sy_itab_line_not_found.
        " 002	Invalid square &1. Piece not found
        RAISE EXCEPTION TYPE zcx_chess_exception
          MESSAGE e002 WITH io_square->mv_name.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_chess_board_read~is_inside_board.

    rv_result = zcl_chess_square_utility=>is_inside_board(
                  iv_x = io_square->mv_x
                  iv_y = io_square->mv_y ).

  ENDMETHOD.


  METHOD zif_chess_board_read~is_move_valid.

    DATA(lo_piece) = me->zif_chess_board_read~get_piece( io_move->mo_from ).

    IF lo_piece IS NOT BOUND.
      RETURN.
    ENDIF.

    rv_result = lo_piece->is_move_valid(
                  io_move  = io_move
                  io_board = me ).

  ENDMETHOD.


  METHOD zif_chess_board_read~is_square_empty.
    rv_result = xsdbool( me->zif_chess_board~get_piece( io_square ) IS NOT BOUND ).
  ENDMETHOD.


  METHOD zif_chess_board_read~is_square_threatened.

    " check all enemy pieces:
    LOOP AT me->zif_chess_board_read~mt_pieces_on_squares
      INTO DATA(lo_enemy_pieces)
      WHERE table_line IS BOUND
      AND   table_line->mv_color = zcl_chess_piece_utility=>get_opposite_color( iv_defending_color ).

      DATA(lv_tabix) = sy-tabix.

      " get square of enemy piece by index
      READ TABLE me->zif_chess_board_read~mt_squares
        INTO DATA(lo_enemy_square)
        INDEX lv_tabix.

      ASSERT sy-subrc = 0.

      DATA(lo_move) = NEW zcl_chess_move(
                            io_from = lo_enemy_square
                            io_to   = io_square ).

      IF me->zif_chess_board_read~is_move_valid( lo_move ).
        rv_result = abap_true.
        RETURN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_chess_board~capture_piece.

    DATA(lo_piece) = me->zif_chess_board~get_piece( io_square ).

    IF lo_piece IS NOT BOUND.
      " 003	Cannot capture empty square &1
      RAISE EXCEPTION TYPE zcx_chess_exception
        MESSAGE e003 WITH io_square->mv_name.
    ENDIF.

    io_move->set_capture_piece(
        io_piece  = lo_piece
        io_square = io_square ).

  ENDMETHOD.


  METHOD zif_chess_board~clear_square.
    TRY.
        CLEAR me->zif_chess_board~mt_pieces_on_squares[ me->get_table_index( io_square ) ].
      CATCH cx_sy_itab_line_not_found.
        " 002	Invalid square &1. Piece not found
        RAISE EXCEPTION TYPE zcx_chess_exception
          MESSAGE e002 WITH io_square->mv_name.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_chess_board~initialize.

    DATA lv_index TYPE sy-index VALUE 1.

    DATA(lt_setup) = io_setup->get_config( ).

    IF lines( lt_setup ) <> zif_chess_board=>cv_number_ranks.
      " 001	Wrong number of ranks: &1 (Expected &2)
      RAISE EXCEPTION TYPE zcx_chess_exception
        MESSAGE e001 WITH |{ lines( lt_setup ) STYLE = SIMPLE }| |{ zif_chess_board=>cv_number_ranks STYLE = SIMPLE }|.
    ENDIF.

    CLEAR me->zif_chess_board~mt_pieces_on_squares.

    me->zif_chess_board~mt_squares = zcl_chess_square_factory=>get_squares(
                                       iv_number_ranks  = zif_chess_board=>cv_number_ranks
                                       iv_number_files  = zif_chess_board=>cv_number_files ).

    DATA(lo_piece_factory) = NEW zcl_chess_piece_factory( ).

    DO lines( me->zif_chess_board~mt_squares ) TIMES.
      INSERT INITIAL LINE INTO TABLE me->zif_chess_board~mt_pieces_on_squares.
    ENDDO.

    LOOP AT lt_setup INTO DATA(lv_setup).

      DO.

        DATA(lv_piece_data) = lv_setup(2).

        IF lv_piece_data IS INITIAL.
          EXIT.
        ENDIF.

        IF lv_piece_data <> zif_chess_board_setup=>cv_empty_square.

          READ TABLE me->zif_chess_board~mt_squares
            INTO DATA(lo_square)
            INDEX lv_index.

          ASSERT sy-subrc = 0.

          DATA(lo_piece) = lo_piece_factory->create_piece(
                            iv_color = lv_piece_data(1)
                            iv_type  = lv_piece_data+1(1) ).

          me->zif_chess_board~set_piece(
              io_square = lo_square
              io_piece  = lo_piece ).

        ENDIF.

        SHIFT lv_setup LEFT BY 2 PLACES.
        lv_index = lv_index + 1.

      ENDDO.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_chess_board~move_piece.

    DATA(lo_piece) = me->zif_chess_board~get_piece( io_move->mo_from ).

    IF lo_piece IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(lo_capture) = me->zif_chess_board~get_piece( io_move->mo_to ).

    IF lo_capture IS BOUND.
      me->zif_chess_board~capture_piece(
              io_square = io_move->mo_to
              io_move   = io_move ).
    ENDIF.

    me->zif_chess_board~clear_square( io_move->mo_from ).

    INSERT VALUE #( piece = lo_piece
                    move  = io_move ) INTO TABLE me->zif_chess_board~mt_piece_moves.

    lo_piece->got_moved( ).

    CASE io_move->mv_type.
      WHEN zif_chess_move=>cs_type-promotion.

        IF io_move->mo_promoted_piece IS NOT BOUND.
          " 049	Promoted piece is empty in promotion move
          RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e049.
        ENDIF.

        me->zif_chess_board~set_piece(
            io_square = io_move->mo_to
            io_piece  = io_move->mo_promoted_piece ).

      WHEN OTHERS.
        me->zif_chess_board~set_piece(
            io_square = io_move->mo_to
            io_piece  = lo_piece ).
    ENDCASE.

  ENDMETHOD.


  METHOD zif_chess_board~set_piece.

    TRY.
        me->zif_chess_board~mt_pieces_on_squares[ me->get_table_index( io_square ) ] = io_piece.
      CATCH cx_sy_itab_line_not_found.
        " 002	Invalid square &1. Piece not found
        RAISE EXCEPTION TYPE zcx_chess_exception
          MESSAGE e002 WITH io_square->mv_name.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_chess_board~undo_move.

    DATA(lv_last_line) = lines( me->zif_chess_board~mt_piece_moves ).

    READ TABLE me->zif_chess_board~mt_piece_moves
      INTO DATA(ls_last_move)
      INDEX lv_last_line.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ls_last_move-piece->got_moved( -1 ).

    me->zif_chess_board~set_piece(
        io_square = ls_last_move-move->mo_from
        io_piece  = ls_last_move-piece ).

    me->zif_chess_board~clear_square( ls_last_move-move->mo_to ).

    IF ls_last_move-move->mo_captured_piece IS BOUND.
      me->zif_chess_board~set_piece(
          io_square = ls_last_move-move->mo_captured_square
          io_piece  = ls_last_move-move->mo_captured_piece ).
    ENDIF.

    DELETE me->zif_chess_board~mt_piece_moves INDEX lv_last_line.

    IF ls_last_move-move->mv_type  = zif_chess_move=>cs_type-castling AND
       ls_last_move-piece->mv_type = zif_chess_piece=>cs_type-rook.

      " do another undo move, because castling 'counts' as two moves:
      " first king gets moved and then rook.
      me->zif_chess_board~undo_move( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

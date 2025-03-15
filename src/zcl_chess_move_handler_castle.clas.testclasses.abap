CLASS lcl_test_castling DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM zcl_chess_test.
  .

  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO zcl_chess_game.  "class under test

    METHODS setup
      RAISING zcx_chess_exception.

    METHODS test_castling_white FOR TESTING
      RAISING zcx_chess_exception.

    METHODS test_castling_check FOR TESTING
      RAISING zcx_chess_exception.

    METHODS test_castling_not_empty FOR TESTING
      RAISING zcx_chess_exception.

ENDCLASS.

CLASS lcl_test_castling IMPLEMENTATION.

  METHOD setup.
    me->f_cut = me->setup_game( ).
  ENDMETHOD.

  METHOD test_castling_white.

    DATA lt_moves TYPE STANDARD TABLE OF REF TO zif_chess_move WITH EMPTY KEY.

    " this tests castling for white

    " white knight move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'G1'
             iv_to   = 'F3' ) INTO TABLE lt_moves.

    " black pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E7'
             iv_to   = 'E6' ) INTO TABLE lt_moves.

    " white pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'G2'
             iv_to   = 'G3' ) INTO TABLE lt_moves.

    " black pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E6'
             iv_to   = 'E5' ) INTO TABLE lt_moves.

    " white bishop move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'F1'
             iv_to   = 'H3' ) INTO TABLE lt_moves.

    " black pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E5'
             iv_to   = 'E4' ) INTO TABLE lt_moves.

    " white king castling
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E1'
             iv_to   = 'H1' ) INTO TABLE lt_moves.

    LOOP AT lt_moves INTO DATA(lo_move).

      DATA(lv_tabix) = sy-tabix.

      DATA(lo_result) = me->f_cut->play_move( lo_move ).

      cl_abap_unit_assert=>assert_equals(
          act = lo_result->mv_code
          exp = zif_chess_move_state=>cs_move_state_code-normal ).

      cl_abap_unit_assert=>assert_differs(
          act = lo_result->ms_message-msgty
          exp = 'E' ).

    ENDLOOP.

    cl_abap_unit_assert=>assert_equals(
        act = lo_result->is_game_ending_move( )
        exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
        act = lo_move->mv_type
        exp = zif_chess_move=>cs_type-castling ).

  ENDMETHOD.

  METHOD test_castling_check.

    DATA lt_moves TYPE STANDARD TABLE OF REF TO zif_chess_move WITH EMPTY KEY.

    " this tests that castling is not possible if king would be in check after castling

    " white pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'G2'
             iv_to   = 'G3' ) INTO TABLE lt_moves.

    " black pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'D7'
             iv_to   = 'D6' ) INTO TABLE lt_moves.

    " white knight move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'G1'
             iv_to   = 'F3' ) INTO TABLE lt_moves.

    " black pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E7'
             iv_to   = 'E5' ) INTO TABLE lt_moves.

    " white bishop move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'F1'
             iv_to   = 'H3' ) INTO TABLE lt_moves.

    " black bishop (capure) move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'C8'
             iv_to   = 'H3' ) INTO TABLE lt_moves.

    " white king castling should not be possible
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E1'
             iv_to   = 'H1' ) INTO TABLE lt_moves.

    LOOP AT lt_moves INTO DATA(lo_move).

      DATA(lv_tabix) = sy-tabix.

      DATA(lo_result) = me->f_cut->play_move( lo_move ).

      IF lv_tabix <> lines( lt_moves ).

        cl_abap_unit_assert=>assert_equals(
            act = lo_result->mv_code
            exp = zif_chess_move_state=>cs_move_state_code-normal ).

        cl_abap_unit_assert=>assert_differs(
            act = lo_result->ms_message-msgty
            exp = 'E' ).

      ELSE.

        cl_abap_unit_assert=>assert_equals(
            act = lo_result->ms_message-msgty
            exp = 'E' ).

      ENDIF.

    ENDLOOP.

    cl_abap_unit_assert=>assert_equals(
        act = lo_result->is_game_ending_move( )
        exp = abap_false ).

    cl_abap_unit_assert=>assert_differs(
        act = lo_move->mv_type
        exp = zif_chess_move=>cs_type-castling ).

  ENDMETHOD.

  METHOD test_castling_not_empty.

    DATA lt_moves TYPE STANDARD TABLE OF REF TO zif_chess_move WITH EMPTY KEY.

    " this tests that castling is not possible because space between king and rook is not empty

    " white pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'G2'
             iv_to   = 'G3' ) INTO TABLE lt_moves.

    " black pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'D7'
             iv_to   = 'D6' ) INTO TABLE lt_moves.

    " white bishop move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'F1'
             iv_to   = 'H3' ) INTO TABLE lt_moves.

    " black pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E7'
             iv_to   = 'E6' ) INTO TABLE lt_moves.

    " white king castling should not be possible
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E1'
             iv_to   = 'H1' ) INTO TABLE lt_moves.

    LOOP AT lt_moves INTO DATA(lo_move).

      DATA(lv_tabix) = sy-tabix.

      DATA(lo_result) = me->f_cut->play_move( lo_move ).

      IF lv_tabix <> lines( lt_moves ).

        cl_abap_unit_assert=>assert_equals(
            act = lo_result->mv_code
            exp = zif_chess_move_state=>cs_move_state_code-normal ).

        cl_abap_unit_assert=>assert_differs(
            act = lo_result->ms_message-msgty
            exp = 'E' ).

      ELSE.

        cl_abap_unit_assert=>assert_equals(
            act = lo_result->ms_message-msgty
            exp = 'E' ).

      ENDIF.

    ENDLOOP.

    cl_abap_unit_assert=>assert_equals(
        act = lo_result->is_game_ending_move( )
        exp = abap_false ).

    cl_abap_unit_assert=>assert_differs(
        act = lo_move->mv_type
        exp = zif_chess_move=>cs_type-castling ).


  ENDMETHOD.

ENDCLASS.

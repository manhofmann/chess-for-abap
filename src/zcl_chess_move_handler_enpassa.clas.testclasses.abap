CLASS lcl_test_en_passant DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM zcl_chess_test.
  .

  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO zcl_chess_game.  "class under test

    METHODS setup
      RAISING zcx_chess_exception.

    METHODS test_en_passant_white FOR TESTING
      RAISING zcx_chess_exception.

    METHODS test_en_passant_black FOR TESTING
      RAISING zcx_chess_exception.

    METHODS test_en_passant_moved_twice FOR TESTING
      RAISING zcx_chess_exception.

    METHODS test_en_passant_no_pawn FOR TESTING
      RAISING zcx_chess_exception.

    METHODS test_en_passant_not_prev_move FOR TESTING
      RAISING zcx_chess_exception.

ENDCLASS.

CLASS lcl_test_en_passant IMPLEMENTATION.

  METHOD setup.
    me->f_cut = me->setup_game( ).
  ENDMETHOD.

  METHOD test_en_passant_white.

    DATA lt_moves TYPE STANDARD TABLE OF REF TO zif_chess_move WITH EMPTY KEY.

    " this tests en passant for white

    " white pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E2'
             iv_to   = 'E4' ) INTO TABLE lt_moves.

    " black pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'A7'
             iv_to   = 'A5' ) INTO TABLE lt_moves.

    " white pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E4'
             iv_to   = 'E5' ) INTO TABLE lt_moves.

    " black pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'D7'
             iv_to   = 'D5' ) INTO TABLE lt_moves.

    " en passant
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E5'
             iv_to   = 'D6' ) INTO TABLE lt_moves.

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
        exp = zif_chess_move=>cs_type-en_passant ).

  ENDMETHOD.

  METHOD test_en_passant_black.

    DATA lt_moves TYPE STANDARD TABLE OF REF TO zif_chess_move WITH EMPTY KEY.

    " this tests en passant for black

    " white pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'A2'
             iv_to   = 'A4' ) INTO TABLE lt_moves.

    " black pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'D7'
             iv_to   = 'D5' ) INTO TABLE lt_moves.

    " white pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'A4'
             iv_to   = 'A5' ) INTO TABLE lt_moves.

    " black pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'D5'
             iv_to   = 'D4' ) INTO TABLE lt_moves.

    " white pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E2'
             iv_to   = 'E4' ) INTO TABLE lt_moves.

    " en passant
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'D4'
             iv_to   = 'E3' ) INTO TABLE lt_moves.

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
        exp = zif_chess_move=>cs_type-en_passant ).

  ENDMETHOD.


  METHOD test_en_passant_moved_twice.

    DATA lt_moves TYPE STANDARD TABLE OF REF TO zif_chess_move WITH EMPTY KEY.

    " white pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E2'
             iv_to   = 'E4' ) INTO TABLE lt_moves.

    " black pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'D7'
             iv_to   = 'D6' ) INTO TABLE lt_moves.

    " white pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E4'
             iv_to   = 'E5' ) INTO TABLE lt_moves.

    " black pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'D6'
             iv_to   = 'D5' ) INTO TABLE lt_moves.

    " this 'en passant' should not be possible as black pawn moved twice already
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E5'
             iv_to   = 'D6' ) INTO TABLE lt_moves.

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
        exp = zif_chess_move=>cs_type-en_passant ).

  ENDMETHOD.

  METHOD test_en_passant_no_pawn.

    DATA lt_moves TYPE STANDARD TABLE OF REF TO zif_chess_move WITH EMPTY KEY.

    " white pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E2'
             iv_to   = 'E4' ) INTO TABLE lt_moves.

    " black knight move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'G8'
             iv_to   = 'F6' ) INTO TABLE lt_moves.

    " white pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E4'
             iv_to   = 'E5' ) INTO TABLE lt_moves.

    " black knight move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'F6'
             iv_to   = 'D5' ) INTO TABLE lt_moves.

    " this 'en passant' should not be possible as capturing piece is not a pawn
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E5'
             iv_to   = 'D6' ) INTO TABLE lt_moves.

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
        exp = zif_chess_move=>cs_type-en_passant ).

  ENDMETHOD.

  METHOD test_en_passant_not_prev_move.

    DATA lt_moves TYPE STANDARD TABLE OF REF TO zif_chess_move WITH EMPTY KEY.

    " white pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E2'
             iv_to   = 'E4' ) INTO TABLE lt_moves.

    " black pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'D7'
             iv_to   = 'D5' ) INTO TABLE lt_moves.

    " white pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E4'
             iv_to   = 'E5' ) INTO TABLE lt_moves.

    " black pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'C7'
             iv_to   = 'C6' ) INTO TABLE lt_moves.

    " this 'en passant' should not be possible because previous move was another piece
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E5'
             iv_to   = 'D6' ) INTO TABLE lt_moves.

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
        exp = zif_chess_move=>cs_type-en_passant ).


  ENDMETHOD.

ENDCLASS.

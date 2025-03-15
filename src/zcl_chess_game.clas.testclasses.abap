CLASS lcl_test_fools_mate DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM zcl_chess_test.
  .

  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO zcl_chess_game.  "class under test

    METHODS setup
      RAISING zcx_chess_exception.

    METHODS test_fools_mate FOR TESTING
      RAISING zcx_chess_exception.

ENDCLASS.       "lcl_Test_Fools_Mate

CLASS lcl_test_fools_mate IMPLEMENTATION.

  METHOD setup.
    me->f_cut = me->setup_game( ).
  ENDMETHOD.

  METHOD test_fools_mate.

    DATA lt_moves TYPE STANDARD TABLE OF REF TO zif_chess_move WITH EMPTY KEY.

    " white pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'F2'
             iv_to   = 'F3' ) INTO TABLE lt_moves.

    " black pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E7'
             iv_to   = 'E5' ) INTO TABLE lt_moves.

    " white pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'G2'
             iv_to   = 'G4' ) INTO TABLE lt_moves.

    " checkmate by black queen
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'D8'
             iv_to   = 'H4' ) INTO TABLE lt_moves.

    LOOP AT lt_moves INTO DATA(lo_move).

      DATA(lv_tabix) = sy-tabix.

      DATA(lo_result) = me->f_cut->play_move( lo_move ).

      IF lv_tabix <> lines( lt_moves ).
        cl_abap_unit_assert=>assert_equals(
            act = lo_result->mv_code
            exp = zif_chess_move_state=>cs_move_state_code-normal ).
      ENDIF.

    ENDLOOP.

    cl_abap_unit_assert=>assert_equals(
        act = lo_result->mv_code
        exp = zif_chess_move_state=>cs_move_state_code-checkmate ).

    cl_abap_unit_assert=>assert_equals(
        act = lo_result->is_game_ending_move( )
        exp = abap_true ).

  ENDMETHOD.

ENDCLASS.


CLASS lcl_test_stale_mate DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM zcl_chess_test.
  .

  " stale mate by Sam Loyd:
  " 1.e3 a5 2.Qh5 Ra6 3.Qxa5 h5 4.Qxc7 Rah6 5.h4 f6 6.Qxd7+ Kf7 7.Qxb7 Qd3 8.Qxb8 Qh7 9.Qxc8 Kg6 10.Qe6

  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO zcl_chess_game.  "class under test

    METHODS setup
      RAISING zcx_chess_exception.

    METHODS test_fools_mate FOR TESTING
      RAISING zcx_chess_exception.

ENDCLASS.       "lcl_Test_Fools_Mate

CLASS lcl_test_stale_mate IMPLEMENTATION.

  METHOD setup.
    me->f_cut = me->setup_game( ).
  ENDMETHOD.

  METHOD test_fools_mate.

    DATA lt_moves TYPE STANDARD TABLE OF REF TO zif_chess_move WITH EMPTY KEY.

    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E2'
             iv_to   = 'E3' ) INTO TABLE lt_moves.

    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'A7'
             iv_to   = 'A5' ) INTO TABLE lt_moves.

    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'D1'
             iv_to   = 'H5' ) INTO TABLE lt_moves.

    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'A8'
             iv_to   = 'A6' ) INTO TABLE lt_moves.

    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'H5'
             iv_to   = 'A5' ) INTO TABLE lt_moves.

    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'H7'
             iv_to   = 'H5' ) INTO TABLE lt_moves.

    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'A5'
             iv_to   = 'C7' ) INTO TABLE lt_moves.

    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'A6'
             iv_to   = 'H6' ) INTO TABLE lt_moves.

    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'H2'
             iv_to   = 'H4' ) INTO TABLE lt_moves.

    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'F7'
             iv_to   = 'F6' ) INTO TABLE lt_moves.

    " check by white queen:
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'C7'
             iv_to   = 'D7' ) INTO TABLE lt_moves.

    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E8'
             iv_to   = 'F7' ) INTO TABLE lt_moves.

    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'D7'
             iv_to   = 'B7' ) INTO TABLE lt_moves.

    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'D8'
             iv_to   = 'D3' ) INTO TABLE lt_moves.

    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'B7'
             iv_to   = 'B8' ) INTO TABLE lt_moves.

    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'D3'
             iv_to   = 'H7' ) INTO TABLE lt_moves.

    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'B8'
             iv_to   = 'C8' ) INTO TABLE lt_moves.

    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'F7'
             iv_to   = 'G6' ) INTO TABLE lt_moves.

    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'C8'
             iv_to   = 'E6' ) INTO TABLE lt_moves.

    LOOP AT lt_moves INTO DATA(lo_move).

      DATA(lv_tabix) = sy-tabix.

      DATA(lo_result) = me->f_cut->play_move( lo_move ).

      IF lv_tabix <> lines( lt_moves ).
        cl_abap_unit_assert=>assert_equals(
            act = lo_result->mv_code
            exp = zif_chess_move_state=>cs_move_state_code-normal ).
      ENDIF.

    ENDLOOP.

    cl_abap_unit_assert=>assert_equals(
        act = lo_result->mv_code
        exp = zif_chess_move_state=>cs_move_state_code-stalemate ).

    cl_abap_unit_assert=>assert_equals(
        act = lo_result->is_game_ending_move( )
        exp = abap_true ).

  ENDMETHOD.

ENDCLASS.

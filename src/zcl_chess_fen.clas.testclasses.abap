CLASS lcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM zcl_chess_test.

  PRIVATE SECTION.
    DATA f_cut TYPE REF TO zcl_chess_fen.
    DATA f_game TYPE REF TO zcl_chess_game.

    METHODS setup
      RAISING zcx_chess_exception.

    METHODS test FOR TESTING
      RAISING zcx_chess_exception.

    METHODS test_pawn_move FOR TESTING
      RAISING zcx_chess_exception.

    METHODS test_full_move FOR TESTING
      RAISING zcx_chess_exception.

    METHODS test_no_castling_white FOR TESTING
      RAISING zcx_chess_exception.

    METHODS test_only_kinghts FOR TESTING
      RAISING zcx_chess_exception.

    METHODS test_white_castling FOR TESTING
      RAISING zcx_chess_exception.

ENDCLASS.       "lcl_Test_Fools_Mate

CLASS lcl_test IMPLEMENTATION.

  METHOD setup.
    me->f_game = me->setup_game( ).
    me->f_cut  = NEW #( ).
  ENDMETHOD.

  METHOD test.

    DATA(lv_notation) = me->f_cut->get_notation(
                          io_board          = me->mo_board
                          io_current_player = me->f_game->mo_current_player ).

    cl_abap_unit_assert=>assert_equals(
        act = lv_notation
        exp = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1' ).

  ENDMETHOD.

  METHOD test_pawn_move.

    DATA lt_moves TYPE STANDARD TABLE OF REF TO zif_chess_move WITH EMPTY KEY.

    " white pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E2'
             iv_to   = 'E4' ) INTO TABLE lt_moves.

    LOOP AT lt_moves INTO DATA(lo_move).

      DATA(lv_tabix) = sy-tabix.

      DATA(lo_result) = me->f_game->play_move( lo_move ).

      IF lv_tabix <> lines( lt_moves ).
        cl_abap_unit_assert=>assert_equals(
            act = lo_result->mv_code
            exp = zif_chess_move_state=>cs_move_state_code-normal ).
      ENDIF.

    ENDLOOP.

    DATA(lv_notation) = me->f_cut->get_notation(
                          io_board          = me->mo_board
                          io_current_player = me->f_game->mo_current_player ).

    cl_abap_unit_assert=>assert_equals(
        act = lv_notation
        exp = 'rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1' ).

  ENDMETHOD.

  METHOD test_full_move.

    DATA lt_moves TYPE STANDARD TABLE OF REF TO zif_chess_move WITH EMPTY KEY.

    " white pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E2'
             iv_to   = 'E4' ) INTO TABLE lt_moves.

    " black pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E7'
             iv_to   = 'E5' ) INTO TABLE lt_moves.

    LOOP AT lt_moves INTO DATA(lo_move).

      DATA(lv_tabix) = sy-tabix.

      DATA(lo_result) = me->f_game->play_move( lo_move ).

      IF lv_tabix <> lines( lt_moves ).
        cl_abap_unit_assert=>assert_equals(
            act = lo_result->mv_code
            exp = zif_chess_move_state=>cs_move_state_code-normal ).
      ENDIF.

    ENDLOOP.

    DATA(lv_notation) = me->f_cut->get_notation(
                          io_board          = me->mo_board
                          io_current_player = me->f_game->mo_current_player ).

    cl_abap_unit_assert=>assert_equals(
        act = lv_notation
        exp = 'rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2' ).

  ENDMETHOD.

  METHOD test_no_castling_white.

    DATA lt_moves TYPE STANDARD TABLE OF REF TO zif_chess_move WITH EMPTY KEY.

    " white pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E2'
             iv_to   = 'E4' ) INTO TABLE lt_moves.

    " black pawn move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E7'
             iv_to   = 'E5' ) INTO TABLE lt_moves.

    " white king move
    INSERT zcl_chess_move_factory=>create_move_from_square_names(
             iv_from = 'E1'
             iv_to   = 'E2' ) INTO TABLE lt_moves.

    LOOP AT lt_moves INTO DATA(lo_move).

      DATA(lv_tabix) = sy-tabix.

      DATA(lo_result) = me->f_game->play_move( lo_move ).

      IF lv_tabix <> lines( lt_moves ).
        cl_abap_unit_assert=>assert_equals(
            act = lo_result->mv_code
            exp = zif_chess_move_state=>cs_move_state_code-normal ).
      ENDIF.

    ENDLOOP.

    DATA(lv_notation) = me->f_cut->get_notation(
                          io_board          = me->mo_board
                          io_current_player = me->f_game->mo_current_player ).

    cl_abap_unit_assert=>assert_equals(
        act = lv_notation
        exp = 'rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPPKPPP/RNBQ1BNR b kq - 1 2' ).

  ENDMETHOD.

  METHOD test_only_kinghts.

    DATA lt_moves TYPE STANDARD TABLE OF REF TO zif_chess_move WITH EMPTY KEY.

    DO 10 TIMES.

      " white knight forward
      INSERT zcl_chess_move_factory=>create_move_from_square_names(
               iv_from = 'G1'
               iv_to   = 'F3' ) INTO TABLE lt_moves.

      " black knight forward
      INSERT zcl_chess_move_factory=>create_move_from_square_names(
               iv_from = 'G8'
               iv_to   = 'F6' ) INTO TABLE lt_moves.

      " white knight back
      INSERT zcl_chess_move_factory=>create_move_from_square_names(
               iv_from = 'F3'
               iv_to   = 'G1' ) INTO TABLE lt_moves.

      " black knight back
      INSERT zcl_chess_move_factory=>create_move_from_square_names(
               iv_from = 'F6'
               iv_to   = 'G8' ) INTO TABLE lt_moves.

    ENDDO.

    LOOP AT lt_moves INTO DATA(lo_move).

      DATA(lv_tabix) = sy-tabix.

      DATA(lo_result) = me->f_game->play_move( lo_move ).

      IF lv_tabix <> lines( lt_moves ).
        cl_abap_unit_assert=>assert_equals(
            act = lo_result->mv_code
            exp = zif_chess_move_state=>cs_move_state_code-normal ).
      ENDIF.

    ENDLOOP.

    DATA(lv_notation) = me->f_cut->get_notation(
                          io_board          = me->mo_board
                          io_current_player = me->f_game->mo_current_player ).

    cl_abap_unit_assert=>assert_equals(
        act = lv_notation
        exp = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 40 21' ).

  ENDMETHOD.

  METHOD test_white_castling.

    DATA lt_moves TYPE STANDARD TABLE OF REF TO zif_chess_move WITH EMPTY KEY.

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

      DATA(lo_result) = me->f_game->play_move( lo_move ).

      IF lv_tabix <> lines( lt_moves ).
        cl_abap_unit_assert=>assert_equals(
            act = lo_result->mv_code
            exp = zif_chess_move_state=>cs_move_state_code-normal ).
      ENDIF.

    ENDLOOP.

    DATA(lv_notation) = me->f_cut->get_notation(
                          io_board          = me->mo_board
                          io_current_player = me->f_game->mo_current_player ).

    cl_abap_unit_assert=>assert_equals(
        act = lv_notation
        exp = 'rnbqkbnr/pppp1ppp/8/8/4p3/5NPB/PPPPPP1P/RNBQ1RK1 b kq - 1 4' ).

  ENDMETHOD.


ENDCLASS.

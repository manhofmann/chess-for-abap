class ZCL_CHESS_MOVE_HANDLER_CASTLE definition
  public
  inheriting from ZCL_CHESS_MOVE_HANDLER
  final
  create public .

public section.

  methods ZIF_CHESS_MOVE_HANDLER~HANDLE_MOVE
    redefinition .
  PROTECTED SECTION.
private section.

  methods GET_CORNER_SQUARE
    importing
      !IO_MOVE type ref to ZIF_CHESS_MOVE
    returning
      value(RO_RESULT) type ref to ZIF_CHESS_SQUARE
    raising
      ZCX_CHESS_EXCEPTION .
  methods IS_CASTLING_MOVE
    importing
      !IO_MOVE type ref to ZIF_CHESS_MOVE
      !IO_BOARD type ref to ZIF_CHESS_BOARD_READ
    returning
      value(RV_RESULT) type BOOLE_D
    raising
      ZCX_CHESS_EXCEPTION .
ENDCLASS.



CLASS ZCL_CHESS_MOVE_HANDLER_CASTLE IMPLEMENTATION.


  METHOD get_corner_square.

    DATA lv_x_coordinate  TYPE zchess_file_number.
    DATA lv_direction     TYPE int2.

    DATA(lv_x_distance) = io_move->get_x_distance( ).

    lv_direction = lv_x_distance / abs( lv_x_distance ).

    IF lv_direction > 0.
      lv_x_coordinate = 8.
    ELSE.
      lv_x_coordinate = 1.
    ENDIF.

    ro_result = NEW zcl_chess_square(
                      iv_x = lv_x_coordinate
                      iv_y = io_move->mo_from->mv_y ).

  ENDMETHOD.


  METHOD is_castling_move.

    CONSTANTS cv_king_moves TYPE int1 VALUE 2.

    DATA lv_x_coordinate          TYPE zchess_file_number.
    DATA lv_check_empty_squares   TYPE zchess_file_number.

    IF NOT io_board->is_inside_board( io_move->mo_from ) OR
       NOT io_board->is_inside_board( io_move->mo_to ).
      RETURN.
    ENDIF.

    DATA(lo_king) = io_board->get_piece( io_move->mo_from ).

    IF lo_king IS NOT BOUND.
      RETURN.
    ENDIF.

    IF lo_king->mv_type <> zif_chess_piece=>cs_type-king.
      RETURN.
    ENDIF.

    IF lo_king->mv_move_amount > 0.
      RETURN.
    ENDIF.

    DATA(lv_x_distance) = io_move->get_x_distance( ).

    " castling is allowed via dragging king to rank A,B,C,G or H
    " which means king moves more than one square:
    IF abs( lv_x_distance ) <= 1.
      RETURN.
    ENDIF.

    DATA(lv_direction) = io_move->get_x_direction( ).

    IF lv_direction > 0.
      lv_x_coordinate = 8.
    ELSE.
      lv_x_coordinate = 1.
    ENDIF.

    DATA(lo_corner) = me->get_corner_square( io_move ).

    DATA(lo_rook) = io_board->get_piece( lo_corner ).

    IF lo_rook IS NOT BOUND.
      RETURN.
    ENDIF.

    IF lo_rook->mv_type <> zif_chess_piece=>cs_type-rook.
      RETURN.
    ENDIF.

    IF lo_rook->mv_move_amount > 0.
      RETURN.
    ENDIF.

    IF lo_rook->mv_color <> lo_king->mv_color.
      RETURN.
    ENDIF.

    " we are able to castle now, but we have to met two conditions:
    " 1) we cannot castle out of, through, or into check!
    " 2) no pieces between king and rook

    " check if king would castle if currently in check:
    IF io_board->is_square_threatened(
         io_square          = io_move->mo_from
         iv_defending_color = lo_king->mv_color ).
      RETURN.
    ENDIF.

    IF lv_direction > 0.
      lv_check_empty_squares = 2.
    ELSE.
      lv_check_empty_squares = 3.
    ENDIF.

    DO lv_check_empty_squares TIMES.

      DATA(lv_index) = sy-index.

      lv_x_coordinate = io_move->mo_from->mv_x + lv_direction * lv_index.

      DATA(lo_square) = NEW zcl_chess_square(
                              iv_x = lv_x_coordinate
                              iv_y = io_move->mo_from->mv_y ).

      IF NOT io_board->is_square_empty( lo_square ).
        RETURN.
      ENDIF.

      IF lv_index <= cv_king_moves.

        IF io_board->is_square_threatened(
             io_square          = lo_square
             iv_defending_color = lo_king->mv_color ).
          RETURN.
        ENDIF.

      ENDIF.

    ENDDO.

    rv_result = abap_true.

  ENDMETHOD.


  METHOD zif_chess_move_handler~handle_move.

    DATA lo_rook_move TYPE REF TO zif_chess_move.
    DATA lo_king_move TYPE REF TO zif_chess_move.

    IF me->is_castling_move(
         io_board    = io_board
         io_move     = io_move ).

      DATA(lo_corner) = me->get_corner_square( io_move ).

      IF io_move->get_x_direction( ) > 0.

        lo_king_move = NEW zcl_chess_move(
                              io_from = io_move->mo_from
                              io_to   = NEW zcl_chess_square(
                                                iv_x = 7
                                                iv_y = io_move->mo_from->mv_y ) ).

        lo_rook_move = NEW zcl_chess_move(
                              io_from = lo_corner
                              io_to   = NEW zcl_chess_square(
                                                iv_x = 6
                                                iv_y = io_move->mo_from->mv_y ) ).

      ELSE.

        lo_king_move = NEW zcl_chess_move(
                              io_from = io_move->mo_from
                              io_to   = NEW zcl_chess_square(
                                                iv_x = 3
                                                iv_y = io_move->mo_from->mv_y ) ).

        lo_rook_move = NEW zcl_chess_move(
                              io_from = lo_corner
                              io_to   = NEW zcl_chess_square(
                                                iv_x = 4
                                                iv_y = io_move->mo_from->mv_y ) ).

      ENDIF.

      lo_king_move->set_move_type( zif_chess_move=>cs_type-castling ).
      lo_rook_move->set_move_type( zif_chess_move=>cs_type-castling ).
      io_move->set_move_type( zif_chess_move=>cs_type-castling ).

      io_board->move_piece( lo_king_move ).
      io_board->move_piece( lo_rook_move ).

      rv_result = abap_true.

    ELSE.
      rv_result = me->handle_next(
                    io_board    = io_board
                    io_move     = io_move ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

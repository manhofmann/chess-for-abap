class ZCL_CHESS_PIECE definition
  public
  abstract
  create public .

public section.

  interfaces IF_SERIALIZABLE_OBJECT .
  interfaces ZIF_CHESS_PIECE
      abstract methods GET_ALL_POSSIBLE_MOVES
                       GET_PIECE_VALUE .

  types:
    BEGIN OF ty_moves_table,
        x TYPE int2,
        y TYPE int2,
      END OF ty_moves_table .
  types:
    tty_moves_table TYPE STANDARD TABLE OF ty_moves_table WITH EMPTY KEY .

  methods CONSTRUCTOR
    importing
      !IV_COLOR type ZCHESS_PIECE_COLOR .
protected section.

  methods IS_DIAGONAL_MOVE_VALID
    importing
      !IO_MOVE type ref to ZIF_CHESS_MOVE
      !IO_BOARD type ref to ZIF_CHESS_BOARD_READ
    returning
      value(RV_RESULT) type BOOLE_D
    raising
      ZCX_CHESS_EXCEPTION .
  methods IS_HORIZON_VERTICAL_MOVE_VALID
    importing
      !IO_MOVE type ref to ZIF_CHESS_MOVE
      !IO_BOARD type ref to ZIF_CHESS_BOARD_READ
    returning
      value(RV_RESULT) type BOOLE_D
    raising
      ZCX_CHESS_EXCEPTION .
  methods CALCULATE_POSSIBLE_MOVES
    importing
      !IO_SQUARE type ref to ZIF_CHESS_SQUARE
      !IV_TIMES type INT1
      !IT_MOVES_TABLE type TTY_MOVES_TABLE
    returning
      value(RT_RESULT) type ZCHESS_TT_MOVE
    raising
      ZCX_CHESS_EXCEPTION .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CHESS_PIECE IMPLEMENTATION.


  METHOD calculate_possible_moves.

    DATA lv_x TYPE int2.
    DATA lv_y TYPE int2.

    LOOP AT it_moves_table ASSIGNING FIELD-SYMBOL(<ls_move>).

      DO iv_times TIMES.

        DATA(lv_index) = sy-index.

        lv_x = io_square->mv_x + lv_index * <ls_move>-x.
        lv_y = io_square->mv_y + lv_index * <ls_move>-y.

        IF NOT zcl_chess_square_utility=>is_inside_board(
                          iv_x = lv_x
                          iv_y = lv_y ).
          CONTINUE.
        ENDIF.

        DATA(lo_to) = NEW zcl_chess_square(
                            iv_x = CONV #( lv_x )
                            iv_y = CONV #( lv_y ) ).

        INSERT NEW zcl_chess_move(
                    io_from = io_square
                    io_to   = lo_to ) INTO TABLE rt_result.

      ENDDO.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
    me->zif_chess_piece~mv_color = iv_color.
    me->zif_chess_piece~mv_move_amount = 0.
  ENDMETHOD.


  METHOD is_diagonal_move_valid.

    DATA lv_x_distance    TYPE int2.
    DATA lv_y_distance    TYPE int2.
    DATA lv_x_direction   TYPE int2.
    DATA lv_y_direction   TYPE int2.
    DATA lv_x_coordinate  TYPE int2.
    DATA lv_y_coordinate  TYPE int2.

    lv_x_distance = io_move->mo_to->mv_x - io_move->mo_from->mv_x.
    lv_y_distance = io_move->mo_to->mv_y - io_move->mo_from->mv_y.

    IF abs( lv_y_distance ) <> abs( lv_x_distance ) OR lv_x_distance IS INITIAL.
      RETURN.
    ENDIF.

    rv_result = abap_true.

    " normalize; direction should be either 1 or -1 now:
    lv_x_direction = lv_x_distance / abs( lv_x_distance ).
    lv_y_direction = lv_y_distance / abs( lv_y_distance ).

    " test every square in diagonal direction
    DO abs( lv_x_distance ) TIMES.

      DATA(lv_index) = sy-index.

      lv_x_coordinate = io_move->mo_from->mv_x + lv_index * lv_x_direction.
      lv_y_coordinate = io_move->mo_from->mv_y + lv_index * lv_y_direction.

      DATA(lo_square) = NEW zcl_chess_square(
                              iv_x = CONV #( lv_x_coordinate )
                              iv_y = CONV #( lv_y_coordinate ) ).

      IF NOT io_board->is_square_empty( lo_square ).

        rv_result = abap_false.

        " check if any piece is already between start and end square
        IF lv_index < abs( lv_x_distance ).
          RETURN.
        ENDIF.

      ENDIF.

    ENDDO.

    DATA(lo_piece) = io_board->get_piece( lo_square ).

    IF lo_piece IS NOT BOUND.
      RETURN.
    ENDIF.

    " capture move
    IF rv_result = abap_false AND NOT lo_piece->is_ally( me ).
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD is_horizon_vertical_move_valid.

    DATA lv_x_distance    TYPE int2.
    DATA lv_y_distance    TYPE int2.
    DATA lv_x_direction   TYPE int2.
    DATA lv_y_direction   TYPE int2.
    DATA lv_x_coordinate  TYPE int2.
    DATA lv_y_coordinate  TYPE int2.

    lv_x_distance = io_move->mo_to->mv_x - io_move->mo_from->mv_x.
    lv_y_distance = io_move->mo_to->mv_y - io_move->mo_from->mv_y.

    " normalize; direction should be either 1 or -1 now:
    lv_x_direction = lv_x_distance / abs( lv_x_distance ).
    lv_y_direction = lv_y_distance / abs( lv_y_distance ).

    IF abs( lv_x_direction ) + abs( lv_y_direction ) <> 1.
      RETURN.
    ENDIF.

    rv_result = abap_true.

    DO abs( lv_x_distance ) TIMES.

      DATA(lv_index) = sy-index.

      lv_x_coordinate = io_move->mo_from->mv_x + lv_index * lv_x_direction.
      lv_y_coordinate = io_move->mo_from->mv_y.

      DATA(lo_square) = NEW zcl_chess_square(
                              iv_x = CONV #( lv_x_coordinate )
                              iv_y = CONV #( lv_y_coordinate ) ).

      IF NOT io_board->is_square_empty( lo_square ).

        rv_result = abap_false.

        " check if any piece is already between start and end square
        IF lv_index < abs( lv_x_distance ).
          RETURN.
        ENDIF.

      ENDIF.

    ENDDO.

    DO abs( lv_y_distance ) TIMES.

      lv_index = sy-index.

      lv_x_coordinate = io_move->mo_from->mv_x.
      lv_y_coordinate = io_move->mo_from->mv_y + lv_index * lv_y_direction.

      lo_square = NEW zcl_chess_square(
                        iv_x = CONV #( lv_x_coordinate )
                        iv_y = CONV #( lv_y_coordinate ) ).

      IF NOT io_board->is_square_empty( lo_square ).

        rv_result = abap_false.

        " check if any piece is already between start and end square
        IF lv_index < abs( lv_y_distance ).
          RETURN.
        ENDIF.

      ENDIF.

    ENDDO.

    DATA(lo_piece) = io_board->get_piece( lo_square ).

    IF lo_piece IS NOT BOUND.
      RETURN.
    ENDIF.

    " capture move
    IF rv_result = abap_false AND NOT lo_piece->is_ally( me ).
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_chess_piece~got_moved.
    me->zif_chess_piece~mv_move_amount = me->zif_chess_piece~mv_move_amount + iv_times.
  ENDMETHOD.


  METHOD zif_chess_piece~has_been_moved.
    rv_result = xsdbool( me->zif_chess_piece~mv_move_amount > 0 ).
  ENDMETHOD.


  METHOD zif_chess_piece~is_ally.

    IF io_piece IS NOT BOUND.
      RETURN.
    ENDIF.

    rv_result = xsdbool( me->zif_chess_piece~mv_color = io_piece->mv_color ).

  ENDMETHOD.


  METHOD zif_chess_piece~is_move_valid.

    " sanity check if moves are inside the board itself:
    IF NOT io_board->is_inside_board( io_move->mo_from ) OR
       NOT io_board->is_inside_board( io_move->mo_to ).
      RETURN.
    ENDIF.

    " an empty square cannot be moved:
    IF io_board->is_square_empty( io_move->mo_from ).
      RETURN.
    ENDIF.

    " source and target square have to be different:
    IF io_move->mo_from->mv_name = io_move->mo_to->mv_name.
      RETURN.
    ENDIF.

    " sanity check if it is current piece/correct move
    ASSERT me = io_board->get_piece( io_move->mo_from ).

    DATA(lo_piece) = io_board->get_piece( io_move->mo_to ).

    " disable friendly fire
    IF me->zif_chess_piece~is_ally( lo_piece ).
      RETURN.
    ENDIF.

    rv_result = abap_true.

  ENDMETHOD.
ENDCLASS.

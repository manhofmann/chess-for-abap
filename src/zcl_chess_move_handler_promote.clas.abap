class ZCL_CHESS_MOVE_HANDLER_PROMOTE definition
  public
  inheriting from ZCL_CHESS_MOVE_HANDLER
  final
  create public .

public section.

  methods ZIF_CHESS_MOVE_HANDLER~HANDLE_MOVE
    redefinition .
protected section.
private section.

  methods GET_PROMOTION_DECISION_PIECE
    importing
      !IV_COLOR type ZCHESS_PIECE_COLOR
    returning
      value(RO_RESULT) type ref to ZIF_CHESS_PIECE
    raising
      ZCX_CHESS_EXCEPTION .
  methods IS_PROMOTION_MOVE
    importing
      !IO_MOVE type ref to ZIF_CHESS_MOVE
      !IO_BOARD type ref to ZIF_CHESS_BOARD_READ
    returning
      value(RV_RESULT) type BOOLE_D
    raising
      ZCX_CHESS_EXCEPTION .
ENDCLASS.



CLASS ZCL_CHESS_MOVE_HANDLER_PROMOTE IMPLEMENTATION.


  METHOD GET_PROMOTION_DECISION_PIECE.

    DATA lv_answer      TYPE c.
    DATA lt_selection   TYPE STANDARD TABLE OF spopli WITH EMPTY KEY.
    DATA lv_piece_type  TYPE zchess_piece_type.

    DO.

      lt_selection = VALUE #( ( selflag   = abap_true
                                varoption = 'Queen' )
                              ( varoption = 'Rook' )
                              ( varoption = 'Bishop' )
                              ( varoption = 'Knight' ) ).

      CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
        EXPORTING
          textline1          = 'Select promotion'
          titel              = 'Pawn will promote'
        IMPORTING
          answer             = lv_answer
        TABLES
          t_spopli           = lt_selection
        EXCEPTIONS
          not_enough_answers = 1
          too_much_answers   = 2
          too_much_marks     = 3
          OTHERS             = 4.

      DATA(lv_subrc) = sy-subrc.

      IF sy-subrc <> 0.
        " 013	Error in function module '&1'. SUBRC=&2
        RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e013 WITH 'POPUP_TO_DECIDE_LIST' |{ lv_subrc STYLE = SIMPLE }|.
      ENDIF.

      IF lv_answer <> 'A'.
        EXIT.
      ENDIF.

    ENDDO.

    CASE lv_answer.
      WHEN '1'.
        lv_piece_type = zif_chess_piece=>cs_type-queen.
      WHEN '2'.
        lv_piece_type = zif_chess_piece=>cs_type-rook.
      WHEN '3'.
        lv_piece_type = zif_chess_piece=>cs_type-bishop.
      WHEN '4'.
        lv_piece_type = zif_chess_piece=>cs_type-knight.
      WHEN OTHERS.
        " 018	Invalid answer to promote piece
        RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e018.
    ENDCASE.

    ro_result = NEW zcl_chess_piece_factory( )->create_piece(
                                               iv_color = iv_color
                                               iv_type  = lv_piece_type ).

  ENDMETHOD.


  METHOD is_promotion_move.

    DATA lv_rank_edge TYPE zchess_rank_number.

    DATA(lo_pawn) = io_board->get_piece( io_move->mo_from ).

    IF lo_pawn IS NOT BOUND.
      RETURN.
    ENDIF.

    IF lo_pawn->mv_type <> zif_chess_piece=>cs_type-pawn.
      RETURN.
    ENDIF.

    IF NOT io_board->is_move_valid( io_move ).
      RETURN.
    ENDIF.

    CASE lo_pawn->mv_color.
      WHEN zif_chess_piece=>cs_color-white.
        lv_rank_edge = 8.
      WHEN zif_chess_piece=>cs_color-black.
        lv_rank_edge = 1.
    ENDCASE.

    rv_result = xsdbool( io_move->mo_to->mv_y = lv_rank_edge ).

  ENDMETHOD.


  METHOD zif_chess_move_handler~handle_move.

    DATA lo_promote TYPE REF TO zif_chess_piece.

    IF me->is_promotion_move(
         io_board    = io_board
         io_move     = io_move ).

      DATA(lo_piece) = io_board->get_piece( io_move->mo_from ).

      IF io_move->mo_promoted_piece IS NOT BOUND.
        " if promotion piece is not already set in move, we have to ask for it
        lo_promote = me->get_promotion_decision_piece( lo_piece->mv_color ).
      ELSE.
        " if promotion piece is already set in move, we take it
        lo_promote = io_move->mo_promoted_piece.
      ENDIF.

      io_move->set_promotion_piece( lo_promote ).

      io_move->set_move_type( zif_chess_move=>cs_type-promotion ).

      io_board->move_piece( io_move ).

      rv_result = abap_true.

    ELSE.
      rv_result = me->handle_next(
                    io_board    = io_board
                    io_move     = io_move ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

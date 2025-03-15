class ZCL_CHESS_MOVE_HANDLER definition
  public
  abstract
  create public .

public section.

  interfaces ZIF_CHESS_MOVE_HANDLER
      abstract methods HANDLE_MOVE .

  class-methods CREATE_MOVE_HANDLER
    importing
      !IV_REGULAR type BOOLE_D default ABAP_TRUE
      !IV_PROMOTE type BOOLE_D default ABAP_TRUE
      !IV_CASTLING type BOOLE_D default ABAP_TRUE
      !IV_EN_PASSANT type BOOLE_D default ABAP_TRUE
      !IV_REMOTE type BOOLE_D default ABAP_FALSE
      !IO_CLIENT type ref to ZCL_CHESS_AMC_CLIENT optional
    returning
      value(RO_RESULT) type ref to ZIF_CHESS_MOVE_HANDLER
    raising
      ZCX_CHESS_EXCEPTION .
protected section.

  data MO_NEXT_HANDLER type ref to ZIF_CHESS_MOVE_HANDLER .

  methods HANDLE_NEXT
    importing
      !IO_MOVE type ref to ZIF_CHESS_MOVE
      !IO_BOARD type ref to ZIF_CHESS_BOARD
    returning
      value(RV_RESULT) type BOOLE_D
    raising
      ZCX_CHESS_EXCEPTION .
private section.
ENDCLASS.



CLASS ZCL_CHESS_MOVE_HANDLER IMPLEMENTATION.


  METHOD create_move_handler.

    DATA lo_previous TYPE REF TO zif_chess_move_handler.

    DATA lt_handler TYPE STANDARD TABLE OF REF TO zif_chess_move_handler WITH EMPTY KEY.

    IF iv_remote = abap_true.
      INSERT NEW zcl_chess_move_handler_remote( io_client ) INTO TABLE lt_handler.
    ENDIF.

    IF iv_castling = abap_true.
      INSERT NEW zcl_chess_move_handler_castle( ) INTO TABLE lt_handler.
    ENDIF.

    IF iv_promote = abap_true.
      INSERT NEW zcl_chess_move_handler_promote( ) INTO TABLE lt_handler.
    ENDIF.

    IF iv_en_passant = abap_true.
      INSERT NEW zcl_chess_move_handler_enpassa( ) INTO TABLE lt_handler.
    ENDIF.

    IF iv_regular = abap_true.
      INSERT NEW zcl_chess_move_handler_regular( ) INTO TABLE lt_handler.
    ENDIF.

    LOOP AT lt_handler INTO DATA(lo_handler).

      IF ro_result IS NOT BOUND.
        ro_result = lo_handler.
      ELSE.
        lo_previous->set_next( lo_handler ).
      ENDIF.

      lo_previous = lo_handler.

    ENDLOOP.

  ENDMETHOD.


  METHOD handle_next.

    IF me->mo_next_handler IS BOUND.
      rv_result = me->mo_next_handler->handle_move(
                    io_board    = io_board
                    io_move     = io_move ).

    ELSE.
      rv_result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD zif_chess_move_handler~set_next.

    IF io_handler IS NOT BOUND.
      " 017	Move handler instance must not be empty
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e017.
    ENDIF.

    me->mo_next_handler = io_handler.

  ENDMETHOD.
ENDCLASS.

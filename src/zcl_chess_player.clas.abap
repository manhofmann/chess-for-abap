class ZCL_CHESS_PLAYER definition
  public
  create public .

public section.

  interfaces ZIF_CHESS_PLAYER .

  methods CONSTRUCTOR
    importing
      !IV_COLOR type ZCHESS_PIECE_COLOR
    raising
      ZCX_CHESS_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CHESS_PLAYER IMPLEMENTATION.


  METHOD constructor.

    me->zif_chess_player~mv_color = iv_color.

    " different players might have different move handlers, e.g. it can be used to
    " send a move via AMC
    me->zif_chess_player~mo_move_handler = zcl_chess_move_handler=>create_move_handler( ).

    CASE iv_color.
      WHEN zif_chess_piece=>cs_color-white.
        me->zif_chess_player~mv_player_name = TEXT-001.
      WHEN zif_chess_piece=>cs_color-black.
        me->zif_chess_player~mv_player_name = TEXT-002.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_chess_player~get_input.
    " implement in subclass
    RETURN.
  ENDMETHOD.


  METHOD zif_chess_player~get_resign_move.

    DATA lo_to    TYPE REF TO zif_chess_square.

    " get my king
    DATA(lo_from) = io_board->get_king_square( me->zif_chess_player~mv_color ).

    CASE me->zif_chess_player~mv_color.
      WHEN zif_chess_piece=>cs_color-white.
        " move white king to black square in the middle of the board
        lo_to = zcl_chess_square_factory=>create_square_by_name( 'D4' ).
      WHEN zif_chess_piece=>cs_color-black.
        " move black king to white square in the middle of the board
        lo_to = zcl_chess_square_factory=>create_square_by_name( 'D5' ).
    ENDCASE.

    ro_result = NEW zcl_chess_move(
                      io_from = lo_from
                      io_to   = lo_to ).

    ro_result->set_move_type( zif_chess_move=>cs_type-resignation ).

  ENDMETHOD.


  METHOD zif_chess_player~set_player_name.
    me->zif_chess_player~mv_player_name = iv_name.
  ENDMETHOD.
ENDCLASS.

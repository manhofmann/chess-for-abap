class ZCL_CHESS_MOVE_HANDLER_REGULAR definition
  public
  inheriting from ZCL_CHESS_MOVE_HANDLER
  final
  create public .

public section.

  methods ZIF_CHESS_MOVE_HANDLER~HANDLE_MOVE
    redefinition .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CHESS_MOVE_HANDLER_REGULAR IMPLEMENTATION.


  METHOD zif_chess_move_handler~handle_move.

    IF io_board->is_move_valid( io_move ).

      DATA(lo_from_piece) = io_board->get_piece( io_move->mo_from ).
      DATA(lo_to_piece)   = io_board->get_piece( io_move->mo_to ).

      " disable friendly fire:
      IF lo_from_piece->is_ally( lo_to_piece ).
        RETURN.
      ENDIF.

      io_move->set_move_type( zif_chess_move=>cs_type-regular ).

      io_board->move_piece( io_move ).

      rv_result = abap_true.

    ELSE.
      rv_result = me->handle_next(
                    io_board    = io_board
                    io_move     = io_move ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

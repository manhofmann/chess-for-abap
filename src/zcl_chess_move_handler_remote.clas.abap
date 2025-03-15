class ZCL_CHESS_MOVE_HANDLER_REMOTE definition
  public
  inheriting from ZCL_CHESS_MOVE_HANDLER
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_CLIENT type ref to ZCL_CHESS_AMC_CLIENT .

  methods ZIF_CHESS_MOVE_HANDLER~HANDLE_MOVE
    redefinition .
  PROTECTED SECTION.
private section.

  data MO_CLIENT type ref to ZCL_CHESS_AMC_CLIENT .
ENDCLASS.



CLASS ZCL_CHESS_MOVE_HANDLER_REMOTE IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    me->mo_client = io_client.

  ENDMETHOD.


  METHOD zif_chess_move_handler~handle_move.

    rv_result = me->handle_next(
                  io_board    = io_board
                  io_move     = io_move ).

    IF rv_result = abap_true.
      me->mo_client->send_move( io_move ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

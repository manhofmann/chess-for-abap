class ZCL_CHESS_PLAYER_GUI_REMOTE definition
  public
  inheriting from ZCL_CHESS_PLAYER_GUI
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_COLOR type ZCHESS_PIECE_COLOR
      !IO_CLIENT type ref to ZCL_CHESS_AMC_CLIENT
    raising
      ZCX_CHESS_EXCEPTION .

  methods ZIF_CHESS_PLAYER~GET_INPUT
    redefinition .
protected section.
private section.

  data MO_CLIENT type ref to ZCL_CHESS_AMC_CLIENT .
ENDCLASS.



CLASS ZCL_CHESS_PLAYER_GUI_REMOTE IMPLEMENTATION.


  METHOD constructor.

    super->constructor( iv_color ).

    me->mo_client = io_client.

  ENDMETHOD.


  METHOD zif_chess_player~get_input.

    " 032	Waiting for move of opponent ...
    MESSAGE s032 INTO DATA(dummy).
    cl_progress_indicator=>progress_indicate(
        i_msgid              = 'ZCHESS_MESSAGES'
        i_msgno              = '032'
        i_output_immediately = abap_true ).

    ro_result = me->mo_client->receive_opponent_move( ).

  ENDMETHOD.
ENDCLASS.

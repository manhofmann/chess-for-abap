interface ZIF_CHESS_PLAYER
  public .


  interfaces IF_SERIALIZABLE_OBJECT .

  data MV_COLOR type ZCHESS_PIECE_COLOR read-only .
  data MO_MOVE_HANDLER type ref to ZIF_CHESS_MOVE_HANDLER read-only .
  data MV_PLAYER_NAME type ZCHESS_PLAYER_NAME read-only .

  methods GET_INPUT
    importing
      !IO_BOARD type ref to ZIF_CHESS_BOARD_READ
    returning
      value(RO_RESULT) type ref to ZIF_CHESS_MOVE
    raising
      ZCX_CHESS_EXCEPTION .
  methods GET_RESIGN_MOVE
    importing
      !IO_BOARD type ref to ZIF_CHESS_BOARD_READ
    returning
      value(RO_RESULT) type ref to ZIF_CHESS_MOVE
    raising
      ZCX_CHESS_EXCEPTION .
  methods SET_PLAYER_NAME
    importing
      !IV_NAME type ZCHESS_PLAYER_NAME .
endinterface.

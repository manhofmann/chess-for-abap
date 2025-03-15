interface ZIF_CHESS_MOVE
  public .


  interfaces IF_SERIALIZABLE_OBJECT .

  constants:
    BEGIN OF cs_type,
      regular     TYPE zchess_move_type VALUE 'R',
      castling    TYPE zchess_move_type VALUE 'C',
      en_passant  TYPE zchess_move_type VALUE 'E',
      promotion   TYPE zchess_move_type VALUE 'P',
      resignation TYPE zchess_move_type VALUE 'X',
    END OF cs_type .
  data MO_FROM type ref to ZIF_CHESS_SQUARE read-only .
  data MO_TO type ref to ZIF_CHESS_SQUARE read-only .
  data MO_CAPTURED_PIECE type ref to ZIF_CHESS_PIECE read-only .
  data MO_PROMOTED_PIECE type ref to ZIF_CHESS_PIECE read-only .
  data MO_CAPTURED_SQUARE type ref to ZIF_CHESS_SQUARE read-only .
  data MV_TYPE type ZCHESS_MOVE_TYPE read-only .
  data MV_MOVED_AT type ZCHESS_MOVED_AT read-only .
  data MV_NOTATION type ZCHESS_MOVE_NOTATION read-only .

  methods GET_X_DIRECTION
    returning
      value(RV_RESULT) type INT2 .
  methods GET_Y_DIRECTION
    returning
      value(RV_RESULT) type INT2 .
  methods GET_X_DISTANCE
    returning
      value(RV_RESULT) type INT2 .
  methods GET_Y_DISTANCE
    returning
      value(RV_RESULT) type INT2 .
  methods SET_PROMOTION_PIECE
    importing
      !IO_PIECE type ref to ZIF_CHESS_PIECE
    raising
      ZCX_CHESS_EXCEPTION .
  methods SET_CAPTURE_PIECE
    importing
      !IO_PIECE type ref to ZIF_CHESS_PIECE
      !IO_SQUARE type ref to ZIF_CHESS_SQUARE
    raising
      ZCX_CHESS_EXCEPTION .
  methods SET_MOVE_TYPE
    importing
      !IV_TYPE type ZCHESS_MOVE_TYPE
    raising
      ZCX_CHESS_EXCEPTION .
  methods SET_MOVED_AT .
  methods SET_NOTATION
    importing
      !IV_NOTATION type ZCHESS_MOVE_NOTATION .
endinterface.

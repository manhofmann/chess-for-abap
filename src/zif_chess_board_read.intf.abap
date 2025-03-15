interface ZIF_CHESS_BOARD_READ
  public .


  interfaces IF_SERIALIZABLE_OBJECT .

  data MT_SQUARES type ZCHESS_TT_CHESS_SQUARE read-only .
  data MT_PIECES_ON_SQUARES type ZCHESS_TT_CHESS_PIECE read-only .
  data MT_PIECE_MOVES type ZCHESS_TT_PIECE_MOVE read-only .
  constants CV_NUMBER_FILES type INT1 value 8 ##NO_TEXT.
  constants CV_NUMBER_RANKS type INT1 value 8 ##NO_TEXT.

  methods GET_KING_SQUARE
    importing
      !IV_COLOR type ZCHESS_PIECE_COLOR
    returning
      value(RO_RESULT) type ref to ZIF_CHESS_SQUARE
    raising
      ZCX_CHESS_EXCEPTION .
  methods GET_PIECE
    importing
      !IO_SQUARE type ref to ZIF_CHESS_SQUARE
    returning
      value(RO_RESULT) type ref to ZIF_CHESS_PIECE
    raising
      ZCX_CHESS_EXCEPTION .
  methods IS_SQUARE_EMPTY
    importing
      !IO_SQUARE type ref to ZIF_CHESS_SQUARE
    returning
      value(RV_RESULT) type BOOLE_D
    raising
      ZCX_CHESS_EXCEPTION .
  methods IS_SQUARE_THREATENED
    importing
      !IO_SQUARE type ref to ZIF_CHESS_SQUARE
      !IV_DEFENDING_COLOR type ZCHESS_PIECE_COLOR
    returning
      value(RV_RESULT) type BOOLE_D
    raising
      ZCX_CHESS_EXCEPTION .
  methods IS_INSIDE_BOARD
    importing
      !IO_SQUARE type ref to ZIF_CHESS_SQUARE
    returning
      value(RV_RESULT) type BOOLE_D
    raising
      ZCX_CHESS_EXCEPTION .
  methods IS_MOVE_VALID
    importing
      !IO_MOVE type ref to ZIF_CHESS_MOVE
    returning
      value(RV_RESULT) type BOOLE_D
    raising
      ZCX_CHESS_EXCEPTION .
  methods CREATE_COPY
    returning
      value(RO_RESULT) type ref to ZIF_CHESS_BOARD
    raising
      ZCX_CHESS_EXCEPTION .
endinterface.

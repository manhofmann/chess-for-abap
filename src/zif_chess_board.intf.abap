interface ZIF_CHESS_BOARD
  public .


  interfaces ZIF_CHESS_BOARD_READ .

  aliases CV_NUMBER_FILES
    for ZIF_CHESS_BOARD_READ~CV_NUMBER_FILES .
  aliases CV_NUMBER_RANKS
    for ZIF_CHESS_BOARD_READ~CV_NUMBER_RANKS .
  aliases MT_PIECES_ON_SQUARES
    for ZIF_CHESS_BOARD_READ~MT_PIECES_ON_SQUARES .
  aliases MT_PIECE_MOVES
    for ZIF_CHESS_BOARD_READ~MT_PIECE_MOVES .
  aliases MT_SQUARES
    for ZIF_CHESS_BOARD_READ~MT_SQUARES .
  aliases GET_KING_SQUARE
    for ZIF_CHESS_BOARD_READ~GET_KING_SQUARE .
  aliases GET_PIECE
    for ZIF_CHESS_BOARD_READ~GET_PIECE .
  aliases IS_INSIDE_BOARD
    for ZIF_CHESS_BOARD_READ~IS_INSIDE_BOARD .
  aliases IS_MOVE_VALID
    for ZIF_CHESS_BOARD_READ~IS_MOVE_VALID .
  aliases IS_SQUARE_EMPTY
    for ZIF_CHESS_BOARD_READ~IS_SQUARE_EMPTY .
  aliases IS_SQUARE_THREATENED
    for ZIF_CHESS_BOARD_READ~IS_SQUARE_THREATENED .

  methods INITIALIZE
    importing
      !IO_SETUP type ref to ZIF_CHESS_BOARD_SETUP
    raising
      ZCX_CHESS_EXCEPTION .
  methods CLEAR_SQUARE
    importing
      !IO_SQUARE type ref to ZIF_CHESS_SQUARE
    raising
      ZCX_CHESS_EXCEPTION .
  methods SET_PIECE
    importing
      !IO_SQUARE type ref to ZIF_CHESS_SQUARE
      !IO_PIECE type ref to ZIF_CHESS_PIECE
    raising
      ZCX_CHESS_EXCEPTION .
  methods MOVE_PIECE
    importing
      !IO_MOVE type ref to ZIF_CHESS_MOVE
    raising
      ZCX_CHESS_EXCEPTION .
  methods UNDO_MOVE
    raising
      ZCX_CHESS_EXCEPTION .
  methods CAPTURE_PIECE
    importing
      !IO_MOVE type ref to ZIF_CHESS_MOVE
      !IO_SQUARE type ref to ZIF_CHESS_SQUARE
    raising
      ZCX_CHESS_EXCEPTION .
endinterface.

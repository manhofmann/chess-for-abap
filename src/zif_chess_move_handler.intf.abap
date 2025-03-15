interface ZIF_CHESS_MOVE_HANDLER
  public .


  methods SET_NEXT
    importing
      !IO_HANDLER type ref to ZIF_CHESS_MOVE_HANDLER
    raising
      ZCX_CHESS_EXCEPTION .
  methods HANDLE_MOVE
    importing
      !IO_MOVE type ref to ZIF_CHESS_MOVE
      !IO_BOARD type ref to ZIF_CHESS_BOARD
    returning
      value(RV_RESULT) type BOOLE_D
    raising
      ZCX_CHESS_EXCEPTION .
endinterface.

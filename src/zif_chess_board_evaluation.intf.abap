interface ZIF_CHESS_BOARD_EVALUATION
  public .


  methods EVALUATE
    importing
      !IO_BOARD type ref to ZIF_CHESS_BOARD_READ
    returning
      value(RV_RESULT) type ZCHESS_BOARD_EVALULATION_VALUE
    raising
      ZCX_CHESS_EXCEPTION .
endinterface.

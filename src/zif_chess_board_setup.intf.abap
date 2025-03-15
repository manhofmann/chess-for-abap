interface ZIF_CHESS_BOARD_SETUP
  public .


  constants CV_EMPTY_SQUARE type CHAR2 value '..' ##NO_TEXT.

  methods GET_CONFIG
    returning
      value(RT_RESULT) type ZCHESS_TT_BOARD_SETUP_ROW .
endinterface.

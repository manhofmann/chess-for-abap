interface ZIF_CHESS_SQUARE
  public .


  interfaces IF_SERIALIZABLE_OBJECT .

  data MV_X type ZCHESS_FILE_NUMBER read-only .
  data MV_Y type ZCHESS_RANK_NUMBER read-only .
  data MV_NAME type ZCHESS_SQUARE_NAME read-only .

  events MOVE
    exporting
      value(IO_MOVE) type ref to ZIF_CHESS_MOVE .

  methods IS_LIGHT
    returning
      value(RV_RESULT) type BOOLE_D .
  methods ONDRAG
    for event ONDRAG of CL_GUI_PICTURE
    importing
      !DRAGDROPOBJ .
  methods ONDROP default fail
    for event ONDROP of CL_GUI_PICTURE
    importing
      !DRAGDROPOBJ .
endinterface.

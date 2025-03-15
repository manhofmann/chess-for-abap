class ZCL_CHESS_SQUARE_UTILITY definition
  public
  final
  create public .

public section.

  class-methods TO_SQUARE_COORDINATES
    importing
      !IV_NAME type ZCHESS_SQUARE_NAME
    exporting
      !EV_Y type ZCHESS_RANK_NUMBER
      !EV_X type ZCHESS_FILE_NUMBER .
  class-methods TO_SQUARE_NAME
    importing
      !IV_X type ZCHESS_FILE_NUMBER
      !IV_Y type ZCHESS_RANK_NUMBER
    returning
      value(RV_RESULT) type ZCHESS_SQUARE_NAME .
  class-methods IS_INSIDE_BOARD
    importing
      !IV_X type ANY
      !IV_Y type ANY
    returning
      value(RV_RESULT) type BOOLE_D
    raising
      ZCX_CHESS_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CHESS_SQUARE_UTILITY IMPLEMENTATION.


  METHOD is_inside_board.

    IF iv_x <= zif_chess_board=>cv_number_ranks  AND
       iv_x >= 1                                 AND
       iv_y <= zif_chess_board=>cv_number_files  AND
       iv_y >= 1.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD to_square_coordinates.
    ev_x = cl_abap_conv_out_ce=>uccpi( iv_name(1) ) - 64.
    ev_y = iv_name+1(1).
  ENDMETHOD.


  METHOD to_square_name.
    rv_result(1)    = cl_abap_conv_in_ce=>uccpi( uccp = iv_x + 64 ).
    rv_result+1(1)  = |{ iv_y STYLE = SIMPLE }|.
  ENDMETHOD.
ENDCLASS.

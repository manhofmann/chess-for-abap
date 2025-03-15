class ZCL_CHESS_SQUARE_FACTORY definition
  public
  final
  create public .

public section.

  class-methods GET_SQUARES
    importing
      !IV_NUMBER_RANKS type INT1 default 8
      !IV_NUMBER_FILES type INT1 default 8
    returning
      value(RT_RESULT) type ZCHESS_TT_CHESS_SQUARE .
  class-methods CREATE_SQUARE_BY_NAME
    importing
      !IV_NAME type ZCHESS_SQUARE_NAME
    returning
      value(RO_RESULT) type ref to ZIF_CHESS_SQUARE
    raising
      ZCX_CHESS_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CHESS_SQUARE_FACTORY IMPLEMENTATION.


  METHOD create_square_by_name.

    zcl_chess_square_utility=>to_square_coordinates(
      EXPORTING
        iv_name = iv_name
      IMPORTING
        ev_y    = DATA(lv_y)
        ev_x    = DATA(lv_x) ).

    ro_result = NEW zcl_chess_square(
                      iv_x = lv_x
                      iv_y = lv_y ).

  ENDMETHOD.


  METHOD get_squares.

    DATA lv_x TYPE zchess_file_number.
    DATA lv_y TYPE zchess_rank_number.

    DO iv_number_ranks TIMES.

      lv_y = sy-index.

      DO iv_number_files TIMES.

        lv_x = sy-index.

        INSERT NEW zcl_chess_square( iv_x = lv_x
                                     iv_y = lv_y ) INTO TABLE rt_result.

      ENDDO.

    ENDDO.

    " RT_RESULT is a table which contains this squares:
    " A1, B1, C1, ..., A2, B2, ...

  ENDMETHOD.
ENDCLASS.

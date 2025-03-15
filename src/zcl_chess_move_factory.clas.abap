class ZCL_CHESS_MOVE_FACTORY definition
  public
  final
  create public .

public section.

  class-methods CREATE_MOVE_FROM_SQUARE_NAMES
    importing
      !IV_FROM type ZCHESS_SQUARE_NAME
      !IV_TO type ZCHESS_SQUARE_NAME
    returning
      value(RO_RESULT) type ref to ZIF_CHESS_MOVE
    raising
      ZCX_CHESS_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CHESS_MOVE_FACTORY IMPLEMENTATION.


  METHOD create_move_from_square_names.
    ro_result = NEW zcl_chess_move(
                      io_from = zcl_chess_square_factory=>create_square_by_name( iv_from )
                      io_to   = zcl_chess_square_factory=>create_square_by_name( iv_to ) ).
  ENDMETHOD.
ENDCLASS.

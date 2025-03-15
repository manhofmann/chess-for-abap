class ZCL_CHESS_SQUARE definition
  public
  final
  create public .

public section.

  interfaces ZIF_CHESS_SQUARE .

  methods CONSTRUCTOR
    importing
      !IV_X type ZCHESS_FILE_NUMBER
      !IV_Y type ZCHESS_RANK_NUMBER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CHESS_SQUARE IMPLEMENTATION.


  METHOD constructor.

    me->zif_chess_square~mv_x = iv_x.
    me->zif_chess_square~mv_y = iv_y.

    me->zif_chess_square~mv_name = zcl_chess_square_utility=>to_square_name(
                                     iv_x = iv_x
                                     iv_y = iv_y ).

  ENDMETHOD.


  METHOD zif_chess_square~is_light.
    rv_result = xsdbool( ( me->zif_chess_square~mv_x + me->zif_chess_square~mv_y ) MOD 2 = 1 ).
  ENDMETHOD.


  METHOD zif_chess_square~ondrag.
    dragdropobj->object = me.
  ENDMETHOD.


  METHOD zif_chess_square~ondrop.

    DATA(lo_move) = NEW zcl_chess_move(
                          io_from = CAST #( dragdropobj->object )
                          io_to   = me ).

    RAISE EVENT zif_chess_square~move
      EXPORTING
        io_move = lo_move.

  ENDMETHOD.
ENDCLASS.

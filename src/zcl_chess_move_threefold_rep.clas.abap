class ZCL_CHESS_MOVE_THREEFOLD_REP definition
  public
  final
  create public .

public section.

  interfaces ZIF_CHESS_MOVE_STATE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CHESS_MOVE_THREEFOLD_REP IMPLEMENTATION.


  METHOD zif_chess_move_state~get_state.

    ro_result = NEW #( ).

    " TODO

  ENDMETHOD.
ENDCLASS.

class ZCL_CHESS_MOVE_INSUFF_MATERIAL definition
  public
  final
  create public .

public section.

  interfaces ZIF_CHESS_MOVE_STATE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CHESS_MOVE_INSUFF_MATERIAL IMPLEMENTATION.


  METHOD zif_chess_move_state~get_state.

    ro_result = NEW #( ).

    " TODO

  ENDMETHOD.
ENDCLASS.

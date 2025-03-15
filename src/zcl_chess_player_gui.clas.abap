class ZCL_CHESS_PLAYER_GUI definition
  public
  inheriting from ZCL_CHESS_PLAYER
  create public .

public section.

  data MO_NEXT_MOVE type ref to ZIF_CHESS_MOVE .

  methods ZIF_CHESS_PLAYER~GET_INPUT
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CHESS_PLAYER_GUI IMPLEMENTATION.


  METHOD zif_chess_player~get_input.
    ro_result = me->mo_next_move.
  ENDMETHOD.
ENDCLASS.

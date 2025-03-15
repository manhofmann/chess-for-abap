class ZCL_CHESS_MOVE_STATE_CHECKER definition
  public
  final
  create public .

public section.

  interfaces ZIF_CHESS_MOVE_STATE .

  methods CONSTRUCTOR
    importing
      !IT_MOVE_STATE type ZCHESS_TT_CHESS_MOVE_STATE .
protected section.
private section.

  data MT_MOVE_STATE type ZCHESS_TT_CHESS_MOVE_STATE .
ENDCLASS.



CLASS ZCL_CHESS_MOVE_STATE_CHECKER IMPLEMENTATION.


  METHOD constructor.
    me->mt_move_state = it_move_state.
  ENDMETHOD.


  METHOD zif_chess_move_state~get_state.

    ro_result = NEW #( ).

    LOOP AT me->mt_move_state INTO DATA(lo_move_state).

      DATA(lo_state) = lo_move_state->get_state(
                           io_player  = io_player
                           io_board   = io_board ).

      " check higher priority
      IF lo_state->mv_code > ro_result->mv_code.
        ro_result = lo_state.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

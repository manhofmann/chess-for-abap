class ZCL_CHESS_GUI_BOARD_MULTIPLAY definition
  public
  inheriting from ZCL_CHESS_GUI_BOARD
  abstract
  create public .

public section.

  methods HANDLE_USER_COMMAND
    redefinition .
  methods UPDATE
    redefinition .
protected section.

  methods DO_OPPONENT_MOVE
    raising
      ZCX_CHESS_EXCEPTION .
  methods GET_LOCAL_MULTIPLAYER
    returning
      value(RO_RESULT) type ref to ZCL_CHESS_PLAYER_GUI_MULTIPLAY
    raising
      ZCX_CHESS_EXCEPTION .
  methods TRIGGER_OPPONENT_MOVE_OK_CODE
    raising
      ZCX_CHESS_EXCEPTION .

  methods GET_DRAGDOP_OBJECT
    redefinition .
  methods RESIGN
    redefinition .
  methods UNDO
    redefinition .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CHESS_GUI_BOARD_MULTIPLAY IMPLEMENTATION.


  METHOD do_opponent_move.

    DATA(lo_local_player) = me->get_local_multiplayer( ).

    IF lo_local_player->zif_chess_player~mv_color <> me->mo_game->mo_current_player->mv_color.
      me->trigger_opponent_move_ok_code( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_dragdop_object.

    " default/fallback
    ro_result = me->mo_only_drop.

    DATA(lo_local_player) = me->get_local_multiplayer( ).

    " in multiplayer mode only local player may move pieces and only if its local player's turn
    IF io_piece IS BOUND AND
       io_piece->mv_color                         = lo_local_player->zif_chess_player~mv_color AND
       lo_local_player->zif_chess_player~mv_color = me->mo_game->mo_current_player->mv_color.

      ro_result = me->mo_drag_and_drop.

    ENDIF.

  ENDMETHOD.


  METHOD get_local_multiplayer.

    " now try to find out if we are white or black player:
    TRY.
        ro_result ?= me->mo_white.
      CATCH cx_sy_move_cast_error.
        TRY.
            ro_result ?= me->mo_black.
          CATCH cx_sy_move_cast_error.
            " 035	Neither white nor black player are local player instances
            RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e035(zchess_messages).
        ENDTRY.
    ENDTRY.

  ENDMETHOD.


  METHOD handle_user_command.

    CASE iv_ucomm.
      WHEN cs_ok_code-opponent_move.

        IF me->mo_game->mo_game_result IS BOUND.
          RETURN.
        ENDIF.

        me->play_move( ).

      WHEN OTHERS.
        super->handle_user_command( iv_ucomm ).
    ENDCASE.

  ENDMETHOD.


  METHOD resign.

    IF me->mo_game->mo_game_result IS BOUND.
      RETURN.
    ENDIF.

    DATA(lo_local_player) = me->get_local_multiplayer( ).

    IF me->mo_game->mo_current_player->mv_color <> lo_local_player->zif_chess_player~mv_color.
      " 051	You can only resign for yourself
      MESSAGE s051.
      RETURN.
    ENDIF.

    rv_result = super->resign( ).

  ENDMETHOD.


  METHOD trigger_opponent_move_ok_code.

    IF me->mo_game->mo_game_result IS BOUND.
      RETURN.
    ENDIF.

    me->set_function_code( cs_ok_code-opponent_move ).

  ENDMETHOD.


  METHOD undo.
    " 050	Undo move not possible
    MESSAGE s050.
  ENDMETHOD.


  METHOD update.

    super->update( iv_screen ).

    me->do_opponent_move( ).

  ENDMETHOD.
ENDCLASS.

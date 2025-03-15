class ZCL_CHESS_MOVE_STATE_INFO definition
  public
  final
  create public .

public section.

  interfaces IF_SERIALIZABLE_OBJECT .

  data MV_CODE type ZCHESS_MOVE_STATE_INFO_CODE read-only value ZIF_CHESS_MOVE_STATE=>CS_MOVE_STATE_CODE-NORMAL ##NO_TEXT.
  data MS_MESSAGE type SYMSG read-only .

  methods SET_CODE
    importing
      !IV_CODE type ZCHESS_MOVE_STATE_INFO_CODE .
  methods SET_MESSAGE
    importing
      !IS_MESSAGE type SYMSG .
  methods IS_GAME_ENDING_MOVE
    returning
      value(RV_RESULT) type BOOLE_D .
  methods IS_LEGAL_MOVE
    returning
      value(RV_RESULT) type BOOLE_D .
  methods SET_MESSAGE_FROM_SY .
  methods GET_MESSAGE_TEXT
    returning
      value(RV_RESULT) type BAPI_MSG .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CHESS_MOVE_STATE_INFO IMPLEMENTATION.


  METHOD get_message_text.

    MESSAGE ID me->ms_message-msgid
            TYPE me->ms_message-msgty
            NUMBER me->ms_message-msgno
            WITH me->ms_message-msgv1
                 me->ms_message-msgv2
                 me->ms_message-msgv3
                 me->ms_message-msgv4
            INTO rv_result.

  ENDMETHOD.


  METHOD is_game_ending_move.

    " game can continue:
*0  Normal
*10	Check

    " game ending states:
*20	Draw (by insufficient material)
*21	Draw (by 50 moves rule)
*40	Stalemate
*50	Checkmate

    rv_result = xsdbool( me->mv_code >= zif_chess_move_state=>cs_move_state_code-draw_ins_material ).

  ENDMETHOD.


  METHOD is_legal_move.

*0  Normal
*10	Check
*20	Draw (by insufficient material)
*21	Draw (by 50 moves rule)
*40	Stalemate
*50	Checkmate

    rv_result = xsdbool( me->mv_code < zif_chess_move_state=>cs_move_state_code-check ).

  ENDMETHOD.


  METHOD set_code.
    me->mv_code = iv_code.
  ENDMETHOD.


  METHOD set_message.
    me->ms_message = is_message.
  ENDMETHOD.


  METHOD set_message_from_sy.
    me->set_message( CORRESPONDING #( sy ) ).
  ENDMETHOD.
ENDCLASS.

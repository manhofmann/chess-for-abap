class ZCL_CHESS_AMC_CLIENT definition
  public
  inheriting from ZCL_CHESS_AMC
  final
  create public .

public section.

  methods RECEIVE_OPPONENT_MOVE
    returning
      value(RO_RESULT) type ref to ZIF_CHESS_MOVE
    raising
      ZCX_CHESS_EXCEPTION .
  methods WAIT_OPPONENT_JOINED
    returning
      value(RO_RESULT) type ref to ZIF_CHESS_PLAYER
    raising
      ZCX_CHESS_EXCEPTION .
  methods SEND_MOVE
    importing
      !IO_MOVE type ref to ZIF_CHESS_MOVE
    raising
      ZCX_CHESS_EXCEPTION .
  methods CONSTRUCTOR
    raising
      ZCX_CHESS_EXCEPTION .
  methods REGISTER_MULTIPLAYER
    importing
      !IO_PLAYER type ref to ZCL_CHESS_PLAYER_GUI_LOCAL
    exporting
      !EV_PLAYER_COLOR type ZCHESS_PIECE_COLOR
      !EO_OPPONENT type ref to ZIF_CHESS_PLAYER
    raising
      ZCX_CHESS_EXCEPTION .
protected section.
private section.

  data MO_PLAYER type ref to ZCL_CHESS_PLAYER_GUI_LOCAL .
ENDCLASS.



CLASS ZCL_CHESS_AMC_CLIENT IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    TRY.
        " lister to client messages
        cl_amc_channel_manager=>create_message_consumer(
            i_application_id = cv_application_id
            i_channel_id     = cs_channel-client
            )->start_message_delivery( i_receiver = me ).

        me->mo_producer ?= cl_amc_channel_manager=>create_message_producer(
                            i_application_id  = cv_application_id
                            i_channel_id      = cs_channel-server ).

      CATCH cx_amc_error INTO DATA(lx_exception).
        " 039	AMC Error &1
        RAISE EXCEPTION TYPE zcx_chess_exception
          MESSAGE e039 WITH lx_exception->get_text( )
          EXPORTING
            previous = lx_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD receive_opponent_move.

    me->wait_for_message(
      EXPORTING
        iv_name   = cs_pcp_field-opponent_move
      IMPORTING
        et_fields = DATA(lt_fields)
        ev_binary = DATA(lv_binary) ).

    " check if it is for correct player:
    IF NOT line_exists( lt_fields[ name   = cs_pcp_field-opponent_move
                                   value  = me->mo_player->mv_player_id ] ).
      ro_result = me->receive_opponent_move( ).
      RETURN.
    ENDIF.

    ro_result ?= me->deserialize_object( lv_binary ).

  ENDMETHOD.


  METHOD register_multiplayer.

    CLEAR ev_player_color.
    CLEAR eo_opponent.

    IF me->mo_player IS BOUND.
      " 045	Player has already been registered for multiplayer game
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e045.
    ENDIF.

    TRY.

        me->mo_player = io_player.

        DATA(lo_message) = cl_ac_message_type_pcp=>create( ).

        lo_message->set_field(
            i_name  = cs_pcp_field-register_multiplayer
            i_value = |{ me->mo_player->mv_player_id }| ).

        lo_message->set_field(
            i_name  = cs_pcp_field-method_name
            i_value = 'NEW_PLAYER' ).

        lo_message->set_binary( me->serialize_object( io_player ) ).

        me->mo_producer->send( lo_message ).

      CATCH cx_static_check INTO DATA(lx_exception).
        " 040	AMC Error during sending event &1
        RAISE EXCEPTION TYPE zcx_chess_exception
          MESSAGE e040 WITH 'REGISTER_MULTIPLAYER'
          EXPORTING
            previous = lx_exception.
    ENDTRY.

    me->wait_for_message(
      EXPORTING
        iv_name   = cs_pcp_field-multiplayer_registered
      IMPORTING
        et_fields = DATA(lt_fields)
        ev_binary = DATA(lv_binary) ).

    READ TABLE lt_fields
     ASSIGNING FIELD-SYMBOL(<ls_registered>)
     WITH KEY name = cs_pcp_field-multiplayer_registered.

    IF sy-subrc <> 0.
      " 041	AMC Error during received. Expected field &1 not set
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e041 WITH cs_pcp_field-multiplayer_registered.
    ENDIF.

    ev_player_color = <ls_registered>-value.

    IF lv_binary IS NOT INITIAL.
      eo_opponent ?= me->deserialize_object( lv_binary ).
    ENDIF.

  ENDMETHOD.


  METHOD send_move.

    TRY.

        DATA(lo_message) = cl_ac_message_type_pcp=>create( ).

        lo_message->set_field(
            i_name  = cs_pcp_field-method_name
            i_value = 'MOVE_RECEIVED' ).

        lo_message->set_field(
            i_name  = cs_pcp_field-player_id
            i_value = |{ me->mo_player->mv_player_id }| ).

        lo_message->set_binary( me->serialize_object( io_move ) ).

        me->mo_producer->send( lo_message ).

      CATCH cx_static_check INTO DATA(lx_exception).
        " 040	AMC Error during sending event &1
        RAISE EXCEPTION TYPE zcx_chess_exception
          MESSAGE e040 WITH 'SEND_MOVE'
          EXPORTING
            previous = lx_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD wait_opponent_joined.

    me->wait_for_message(
      EXPORTING
        iv_name   = cs_pcp_field-opponent_joined
      IMPORTING
        et_fields = DATA(lt_fields)
        ev_binary = DATA(lv_binary) ).

    " check if it is for correct player:
    IF NOT line_exists( lt_fields[ name   = cs_pcp_field-opponent_joined
                                   value  = me->mo_player->mv_player_id ] ).
      ro_result = me->wait_opponent_joined( ).
      RETURN.
    ENDIF.

    ro_result ?= me->deserialize_object( lv_binary ).

  ENDMETHOD.
ENDCLASS.

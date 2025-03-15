class ZCL_CHESS_AMC_SERVER definition
  public
  inheriting from ZCL_CHESS_AMC
  final
  create public .

public section.

  methods DO_LOOP
    raising
      ZCX_CHESS_EXCEPTION .
  methods CONSTRUCTOR
    raising
      ZCX_CHESS_EXCEPTION .
protected section.

  methods SEND_PLAYER_JOINED
    importing
      !IO_PLAYER type ref to ZCL_CHESS_PLAYER_GUI_LOCAL
      !IV_OPPONENT_ID type SYSUUID_C32
    raising
      ZCX_CHESS_EXCEPTION .
  methods SET_OPPONENT_PLAYER
    importing
      !IO_PLAYER type ref to ZCL_CHESS_PLAYER_GUI_LOCAL
    exporting
      !EV_PLAYER_COLOR type ZCHESS_PIECE_COLOR
      !EO_OPPONENT type ref to ZCL_CHESS_PLAYER_GUI_LOCAL
    changing
      !CS_GAME type ZCHESS_GAME_SERVER
    raising
      ZCX_CHESS_EXCEPTION .
  methods ADD_PLAYER_TO_GAME
    importing
      !IO_PLAYER type ref to ZCL_CHESS_PLAYER_GUI_LOCAL
    exporting
      !EV_PLAYER_COLOR type ZCHESS_PIECE_COLOR
      !EO_OPPONENT type ref to ZCL_CHESS_PLAYER_GUI_LOCAL
    raising
      ZCX_CHESS_EXCEPTION .
  methods DO_LOOP_SINGLE
    raising
      ZCX_CHESS_EXCEPTION .
  methods MOVE_RECEIVED
    importing
      !IT_FIELDS type PCP_FIELDS
      !IV_BINARY type XSTRING
    raising
      ZCX_CHESS_EXCEPTION .
  methods NEW_PLAYER
    importing
      !IT_FIELDS type PCP_FIELDS
      !IV_BINARY type XSTRING
    raising
      ZCX_CHESS_EXCEPTION .
private section.

*    TYPES:
*      BEGIN OF ty_game,
*        white_player TYPE sysuuid_c32,
*        black_player TYPE sysuuid_c32,
*        game         TYPE REF TO zcl_chess_game,
*      END OF ty_game.
*
*    TYPES: tty_game TYPE STANDARD TABLE OF ty_game WITH EMPTY KEY.
  data MT_GAME type ZCHESS_TT_GAME_SERVER .
  data MV_GAME_ID type INT4 value 1 ##NO_TEXT.
ENDCLASS.



CLASS ZCL_CHESS_AMC_SERVER IMPLEMENTATION.


  METHOD add_player_to_game.

    CLEAR ev_player_color.
    CLEAR eo_opponent.

    " search for game where either player is not yet set (= opponent of first player is missing)
    LOOP AT me->mt_game
      ASSIGNING FIELD-SYMBOL(<ls_game>)
      WHERE ( white_player IS BOUND AND
              black_player IS NOT BOUND )
      OR    ( white_player IS NOT BOUND AND
              black_player IS BOUND )
      AND   game->mo_game_result IS NOT BOUND.

      set_opponent_player(
        EXPORTING
          io_player       = io_player
        IMPORTING
          ev_player_color = ev_player_color
          eo_opponent     = eo_opponent
        CHANGING
          cs_game         = <ls_game> ).

      io_player->set_player_color( ev_player_color ).

      RETURN.

    ENDLOOP.

    IF sy-subrc <> 0.

      INSERT VALUE #( id = me->mv_game_id ) INTO TABLE me->mt_game ASSIGNING <ls_game>.

      me->mv_game_id = me->mv_game_id + 1.

      DATA(lo_random) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( ) min = 0 max = 1 ).

      CASE lo_random->get_next( ).
        WHEN 0.
          <ls_game>-white_player  = io_player.
          ev_player_color         = zif_chess_piece=>cs_color-white.
        WHEN 1.
          <ls_game>-black_player  = io_player.
          ev_player_color         = zif_chess_piece=>cs_color-black.
        WHEN OTHERS.
          " 042	Invalid random result. Expected either 0 or 1
          RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e042.
      ENDCASE.

      io_player->set_player_color( ev_player_color ).

      IF <ls_game>-game IS NOT BOUND.

        DATA(lo_board) = NEW zcl_chess_board( ).

        lo_board->zif_chess_board~initialize( NEW zcl_chess_board_setup_classic( ) ).

        <ls_game>-game = NEW zcl_chess_game(
                              io_board = lo_board
                              " non-gui players
                              io_black = NEW zcl_chess_player( zif_chess_piece=>cs_color-black )
                              io_white = NEW zcl_chess_player( zif_chess_piece=>cs_color-white ) ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    TRY.
        " lister to server messages
        cl_amc_channel_manager=>create_message_consumer(
            i_application_id = cv_application_id
            i_channel_id     = cs_channel-server
            )->start_message_delivery( i_receiver = me ).

        me->mo_producer ?= cl_amc_channel_manager=>create_message_producer(
                            i_application_id  = cv_application_id
                            i_channel_id      = cs_channel-client ).

      CATCH cx_amc_error INTO DATA(lx_exception).
        " 039	AMC Error &1
        RAISE EXCEPTION TYPE zcx_chess_exception
          MESSAGE e039 WITH lx_exception->get_text( )
          EXPORTING
            previous = lx_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD do_loop.

    " allow only one instance/loop running
    CALL FUNCTION 'ENQUEUE_EZCHESS_SERVER'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      " 033	Game server is already running
      MESSAGE s033(zchess_messages).
      RETURN.
    ENDIF.

    DO.
      me->do_loop_single( ).
    ENDDO.

  ENDMETHOD.


  METHOD do_loop_single.

    me->wait_for_message(
      EXPORTING
        iv_name   = cs_pcp_field-method_name
      IMPORTING
        et_fields = DATA(lt_fields)
        ev_binary = DATA(lv_binary) ).

    READ TABLE lt_fields
      ASSIGNING FIELD-SYMBOL(<ls_field>)
      WITH KEY name = cs_pcp_field-method_name.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL METHOD me->(<ls_field>-value)
      EXPORTING
        it_fields = lt_fields
        iv_binary = lv_binary.

  ENDMETHOD.


  METHOD move_received.

    DATA lv_opponent_id TYPE sysuuid_c32.

    READ TABLE it_fields
      ASSIGNING FIELD-SYMBOL(<ls_player_id>)
      WITH KEY name = cs_pcp_field-player_id.

    IF sy-subrc <> 0.
      " 041	AMC Error during received. Expected field &1 not set
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e041 WITH cs_pcp_field-player_id.
    ENDIF.

    LOOP AT me->mt_game
      ASSIGNING FIELD-SYMBOL(<ls_game>)
      WHERE ( white_player IS BOUND AND black_player IS BOUND )
      AND ( white_player->mv_player_id = <ls_player_id>-value OR
            black_player->mv_player_id = <ls_player_id>-value ).

      IF <ls_game>-white_player->mv_player_id = <ls_player_id>-value.
        lv_opponent_id = <ls_game>-black_player->mv_player_id.
      ELSE.
        lv_opponent_id = <ls_game>-white_player->mv_player_id.
      ENDIF.

      EXIT.
    ENDLOOP.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lo_state) = <ls_game>-game->play_move( CAST #( me->deserialize_object( iv_binary ) ) ).

    IF lo_state->is_game_ending_move( ).
      DELETE me->mt_game WHERE id = <ls_game>-id.
    ENDIF.

    TRY.

        DATA(lo_message) = cl_ac_message_type_pcp=>create( ).

        lo_message->set_field(
            i_name  = cs_pcp_field-opponent_move
            i_value = |{ lv_opponent_id }| ).

        lo_message->set_binary( iv_binary ).

        me->mo_producer->send( lo_message ).

      CATCH cx_static_check INTO DATA(lx_exception).
        " 040	AMC Error during sending event &1
        RAISE EXCEPTION TYPE zcx_chess_exception
          MESSAGE e040 WITH 'MOVE_RECEIVED'
          EXPORTING
            previous = lx_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD new_player.

    DATA lo_player TYPE REF TO zcl_chess_player_gui_local.

    READ TABLE it_fields
      ASSIGNING FIELD-SYMBOL(<ls_register>)
      WITH KEY name = cs_pcp_field-register_multiplayer.

    IF sy-subrc <> 0.
      " 041	AMC Error during received. Expected field &1 not set
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e041 WITH cs_pcp_field-register_multiplayer.
    ENDIF.

    lo_player = CAST #( me->deserialize_object( iv_binary ) ).

    me->add_player_to_game(
      EXPORTING
        io_player       = lo_player
      IMPORTING
        ev_player_color = DATA(lv_player_color)
        eo_opponent     = DATA(lo_opponent) ).

    TRY.

        DATA(lo_message) = cl_ac_message_type_pcp=>create( ).

        lo_message->set_field(
            i_name  = cs_pcp_field-multiplayer_registered
            i_value = |{ lv_player_color }| ).

        IF lo_opponent IS BOUND.
          " opponent is only set/bound for the second player who joins a game
          lo_message->set_binary( me->serialize_object( lo_opponent ) ).
        ENDIF.

        me->mo_producer->send( lo_message ).

      CATCH cx_static_check INTO DATA(lx_exception).
        " 040	AMC Error during sending event &1
        RAISE EXCEPTION TYPE zcx_chess_exception
          MESSAGE e040 WITH 'NEW_PLAYER'
          EXPORTING
            previous = lx_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD send_player_joined.

    TRY.

        DATA(lo_message) = cl_ac_message_type_pcp=>create( ).

        lo_message->set_field(
            i_name  = cs_pcp_field-opponent_joined
            i_value = |{ iv_opponent_id }| ).

        lo_message->set_binary( me->serialize_object( io_player ) ).

        me->mo_producer->send( lo_message ).

      CATCH cx_static_check INTO DATA(lx_exception).
        " 040	AMC Error during sending event &1
        RAISE EXCEPTION TYPE zcx_chess_exception
          MESSAGE e040 WITH 'MOVE_RECEIVED'
          EXPORTING
            previous = lx_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD set_opponent_player.

    CLEAR ev_player_color.
    CLEAR eo_opponent.

    IF cs_game-white_player IS NOT BOUND AND
       cs_game-black_player IS BOUND.

      cs_game-white_player  = io_player.
      ev_player_color       = zif_chess_piece=>cs_color-white.
      eo_opponent           = cs_game-black_player.

    ELSEIF cs_game-white_player IS BOUND AND
           cs_game-black_player IS NOT BOUND.

      cs_game-black_player  = io_player.
      ev_player_color       = zif_chess_piece=>cs_color-black.
      eo_opponent           = cs_game-white_player.

    ELSE.
      " 046	Either white or black player should be set
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e046.
    ENDIF.

    ASSERT cs_game-white_player IS BOUND AND
           cs_game-black_player IS BOUND.

    ASSERT cs_game-white_player->zif_chess_player~mv_color <> cs_game-black_player->zif_chess_player~mv_color.

    me->send_player_joined(
        io_player      = io_player
        iv_opponent_id = eo_opponent->mv_player_id ).

  ENDMETHOD.
ENDCLASS.

class ZCL_CHESS_GUI_BOARD_REMOTE definition
  public
  inheriting from ZCL_CHESS_GUI_BOARD_MULTIPLAY
  create public .

public section.

  methods CONSTRUCTOR
    raising
      ZCX_CHESS_EXCEPTION .

  methods HANDLE_USER_COMMAND
    redefinition .
  methods INITIALIZE
    redefinition .
protected section.

  data MO_AMC_CLIENT type ref to ZCL_CHESS_AMC_CLIENT .
  data MO_REMOTE_LOCAL_PLAYER type ref to ZIF_CHESS_PLAYER .

  methods GET_REMOTE_LOCAL_PLAYER
    returning
      value(RO_RESULT) type ref to ZIF_CHESS_PLAYER
    raising
      ZCX_CHESS_EXCEPTION .
  methods TRIGGER_OPPONENT_JOIN_OK_CODE
    raising
      ZCX_CHESS_EXCEPTION .

  methods DO_OPPONENT_MOVE
    redefinition .
  methods RESIGN
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_CHESS_GUI_BOARD_REMOTE IMPLEMENTATION.


  METHOD constructor.

    DATA lo_white         TYPE REF TO zcl_chess_player_gui.
    DATA lo_black         TYPE REF TO zcl_chess_player_gui.
    DATA lt_enq           TYPE STANDARD TABLE OF seqg3 WITH EMPTY KEY.
    DATA lo_amc_client    TYPE REF TO zcl_chess_amc_client.

    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        gname                 = 'ZCHESS_GAME_SERVER'
        guname                = space " game server can be started by any user
      TABLES
        enq                   = lt_enq
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.

    IF sy-subrc <> 0 OR lt_enq IS INITIAL.
      " 043	Game server is not running. Please start it
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e043(zchess_messages).
    ENDIF.

    lo_amc_client = NEW #( ).

    DATA(lo_local_player) = NEW zcl_chess_player_gui_local( lo_amc_client ).

    lo_amc_client->register_multiplayer(
      EXPORTING
        io_player       = lo_local_player
      IMPORTING
        ev_player_color = DATA(lv_player_color)
        eo_opponent     = DATA(lo_remote_local_player) ).

    lo_local_player->set_player_color( lv_player_color ).

    CASE lv_player_color.
      WHEN zif_chess_piece=>cs_color-white.

        lo_white ?= lo_local_player.

        lo_black = NEW zcl_chess_player_gui_remote(
                          iv_color  = zif_chess_piece=>cs_color-black
                          io_client = lo_amc_client ).

      WHEN zif_chess_piece=>cs_color-black.

        lo_black ?= lo_local_player.

        lo_white = NEW zcl_chess_player_gui_remote(
                          iv_color  = zif_chess_piece=>cs_color-white
                          io_client = lo_amc_client ).

      WHEN OTHERS.
        " 019	Invalid color
        RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e019.
    ENDCASE.

    super->constructor(
        io_white  = lo_white
        io_black  = lo_black ).

    me->mo_amc_client           = lo_amc_client.
    me->mo_remote_local_player  = lo_remote_local_player.

  ENDMETHOD.


  METHOD do_opponent_move.

    DATA(lo_local_player) = me->get_local_multiplayer( ).

    " this method is redefined because we can only trigger opponent move
    " if opponent player has joined already
    IF lo_local_player->zif_chess_player~mv_color <> me->mo_game->mo_current_player->mv_color AND
       me->mo_remote_local_player IS BOUND.
      me->trigger_opponent_move_ok_code( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_remote_local_player.

    IF me->mo_remote_local_player IS BOUND.
      ro_result = me->mo_remote_local_player .
      RETURN.
    ENDIF.

    " 044	Wait for opponent to join the game ...
    MESSAGE s044 INTO DATA(dummy).
    cl_progress_indicator=>progress_indicate(
        i_msgid              = 'ZCHESS_MESSAGES'
        i_msgno              = '044'
        i_output_immediately = abap_true ).

    me->mo_remote_local_player  = me->mo_amc_client->wait_opponent_joined( ).

    ro_result = me->mo_remote_local_player .

  ENDMETHOD.


  METHOD handle_user_command.

    CASE iv_ucomm.
      WHEN cs_ok_code-opponent_join.

        DATA(lo_local_player) = get_local_multiplayer( ).

        DATA(lo_opponent) = me->get_remote_local_player( ).

        CASE lo_local_player->zif_chess_player~mv_color.
          WHEN zif_chess_piece=>cs_color-white.
            me->mr_dynpro_fields->black_player_name = lo_opponent->mv_player_name.
          WHEN zif_chess_piece=>cs_color-black.
            me->mr_dynpro_fields->white_player_name = lo_opponent->mv_player_name.
        ENDCASE.

        IF lo_local_player->zif_chess_player~mv_color = zif_chess_piece=>cs_color-black.
          " we are black player -> wait for whites first move
          me->trigger_opponent_move_ok_code( ).
        ENDIF.

      WHEN OTHERS.
        super->handle_user_command( iv_ucomm ).
    ENDCASE.

  ENDMETHOD.


  METHOD initialize.

    DATA(lo_local_player) = get_local_multiplayer( ).

    super->initialize(
        iv_perspective   = lo_local_player->zif_chess_player~mv_color
        ir_dynpro_fields = ir_dynpro_fields ).

*    IF lo_local_player->mo_opponent IS BOUND.
    IF me->mo_remote_local_player IS BOUND.

      CASE lo_local_player->zif_chess_player~mv_color.
        WHEN zif_chess_piece=>cs_color-white.
          me->mr_dynpro_fields->black_player_name = me->mo_remote_local_player->mv_player_name.
        WHEN zif_chess_piece=>cs_color-black.
          me->mr_dynpro_fields->white_player_name = me->mo_remote_local_player->mv_player_name.
      ENDCASE.

    ELSE.
      " opponent has not yet joined the game -> show board and wait
      me->trigger_opponent_join_ok_code( ).
    ENDIF.

  ENDMETHOD.


  METHOD resign.

    IF me->mo_game->mo_game_result IS BOUND.
      RETURN.
    ENDIF.

    rv_result = super->resign( ).

    IF rv_result = abap_true.

      DATA(lo_local_player) = me->get_local_multiplayer( ).

      DATA(lo_resign_move) = lo_local_player->zif_chess_player~get_resign_move( me->mo_board ).

      me->mo_amc_client->send_move( lo_resign_move ).

    ENDIF.

  ENDMETHOD.


  METHOD TRIGGER_OPPONENT_JOIN_OK_CODE.
    me->set_function_code( cs_ok_code-opponent_join ).
  ENDMETHOD.
ENDCLASS.

CLASS zcl_chess_gui_board DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS icon .

    CONSTANTS:
      BEGIN OF cs_ok_code,
        back          TYPE syucomm VALUE 'BACK',
        exit          TYPE syucomm VALUE 'EXIT',
        cancel        TYPE syucomm VALUE 'CANCEL',
        undo          TYPE syucomm VALUE 'UNDO',
        update        TYPE syucomm VALUE 'UPDATE',
        opponent_move TYPE syucomm VALUE 'OPPONENT_MOVE',
        opponent_join TYPE syucomm VALUE 'OPPONENT_JOIN',
        resign        TYPE syucomm VALUE 'RESIGN',
      END OF cs_ok_code .
    CONSTANTS:
      BEGIN OF cs_screen,
        screen_9000 TYPE scradnum VALUE '9000',
        screen_4000 TYPE scradnum VALUE '4000', " board white perspective
        screen_4001 TYPE scradnum VALUE '4001', " board black perspective
      END OF cs_screen .

    METHODS set_dynpro_fields_reference
      IMPORTING
        !ir_dynpro_fields TYPE REF TO zchess_gui_dynpro_fields .
    METHODS handle_user_command
      IMPORTING
        !iv_ucomm TYPE syucomm
      RAISING
        zcx_chess_exception .
    METHODS constructor
      IMPORTING
        !io_white TYPE REF TO zcl_chess_player_gui
        !io_black TYPE REF TO zcl_chess_player_gui
      RAISING
        zcx_chess_exception .
    METHODS display .
    METHODS initialize
      IMPORTING
        !iv_perspective   TYPE zchess_piece_color DEFAULT zif_chess_piece=>cs_color-white
        !ir_dynpro_fields TYPE REF TO zchess_gui_dynpro_fields
      RAISING
        zcx_chess_exception .
    METHODS update
      IMPORTING
        !iv_screen TYPE syst_dynnr DEFAULT sy-dynnr
      RAISING
        zcx_chess_exception .
protected section.

  data MO_WHITE type ref to ZCL_CHESS_PLAYER_GUI .
  data MO_BLACK type ref to ZCL_CHESS_PLAYER_GUI .
  data MO_GAME type ref to ZCL_CHESS_GAME .
  data MO_BOARD type ref to ZIF_CHESS_BOARD .
  data MO_DRAG_AND_DROP type ref to CL_DRAGDROP .
  data MO_ONLY_DROP type ref to CL_DRAGDROP .
  data MR_DYNPRO_FIELDS type ref to ZCHESS_GUI_DYNPRO_FIELDS .

  methods GET_DRAGDOP_OBJECT
    importing
      !IO_PIECE type ref to ZIF_CHESS_PIECE
    returning
      value(RO_RESULT) type ref to CL_DRAGDROP
    raising
      ZCX_CHESS_EXCEPTION .
  methods HANDLER_TIMER
    for event FINISHED of CL_GUI_TIMER .
  methods PLAY_MOVE
    raising
      ZCX_CHESS_EXCEPTION .
  methods SET_FUNCTION_CODE
    importing
      !IV_CODE type SYUCOMM
    raising
      ZCX_CHESS_EXCEPTION .
  methods MOVE_LIST_TOOLBAR
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_INTERACTIVE .
  methods SET_DRAG_DROP_OBJECT
    importing
      !IO_PICTURE type ref to CL_GUI_PICTURE
      !IO_PIECE type ref to ZIF_CHESS_PIECE
    raising
      ZCX_CHESS_EXCEPTION .
  methods UPDATE_SCREEN_400X
    importing
      !IV_SCREEN type SYST_DYNNR
    raising
      ZCX_CHESS_EXCEPTION .
  methods UPDATE_SCREEN_9000
    raising
      ZCX_CHESS_EXCEPTION .
  methods ADD_SQUARE
    importing
      !IO_SQUARE type ref to ZIF_CHESS_SQUARE
      !IO_PIECE type ref to ZIF_CHESS_PIECE
    raising
      ZCX_CHESS_EXCEPTION .
  methods FREE .
  methods UNDO
    raising
      ZCX_CHESS_EXCEPTION .
  methods RESIGN
    returning
      value(RV_RESULT) type BOOLE_D
    raising
      ZCX_CHESS_EXCEPTION .
  methods INITIALIZE_MOVE_LIST
    raising
      ZCX_CHESS_EXCEPTION .
  methods INITIALIZE_SQUARES
    raising
      ZCX_CHESS_EXCEPTION .
  methods UPDATE_SQUARE
    importing
      !IO_SQUARE type ref to ZIF_CHESS_SQUARE
    raising
      ZCX_CHESS_EXCEPTION .
  methods ON_MOVE
    for event MOVE of ZIF_CHESS_SQUARE
    importing
      !IO_MOVE .
  PRIVATE SECTION.

    DATA mt_board TYPE zchess_tt_gui_board_square .
    DATA mo_picture_buffer TYPE REF TO zcl_chess_piece_pic_buffer .
    DATA mt_move_list TYPE zchess_tt_move_list_alv .
    DATA mo_custom_move_list TYPE REF TO cl_gui_custom_container .
    DATA mo_alv_move_list TYPE REF TO cl_gui_alv_grid .
    DATA mo_timer TYPE REF TO cl_gui_timer .
ENDCLASS.



CLASS ZCL_CHESS_GUI_BOARD IMPLEMENTATION.


  METHOD add_square.

    DATA ls_square TYPE zchess_gui_board_square.

    ls_square-name = io_square->mv_name.

    CREATE OBJECT ls_square-container
      EXPORTING
        container_name              = io_square->mv_name
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    DATA(lv_subrc) = sy-subrc.

    IF lv_subrc <> 0.
      " 004	Error creating gui custom container. SUBRC=&1
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e004 WITH |{ lv_subrc STYLE = SIMPLE }|.
    ENDIF.

    ls_square-picture = NEW #( io_square = io_square
                               io_parent = ls_square-container ).

    DATA(lv_url) = me->mo_picture_buffer->get_url(
                   io_piece  = io_piece
                   io_square = io_square ).

    ls_square-picture->mo_picture->load_picture_from_url_async(
      EXPORTING
        url    = lv_url
      EXCEPTIONS
        error  = 1
        OTHERS = 2 ).

    IF sy-subrc <> 0.
      " 006	Error loading picture async
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e006.
    ENDIF.

    me->set_drag_drop_object(
        io_piece    = io_piece
        io_picture  = ls_square-picture->mo_picture ).

    INSERT ls_square INTO TABLE me->mt_board.

  ENDMETHOD.


  METHOD constructor.

    me->mo_picture_buffer = NEW #( ).

    me->mo_white = io_white.
    me->mo_black = io_black.

    " square with piece can be dragged and dropped
    me->mo_drag_and_drop = NEW #( ).
    me->mo_drag_and_drop->add(
      EXPORTING
        flavor          = 'Image'
        dragsrc         = abap_true
        droptarget      = abap_true
        effect          = cl_dragdrop=>move
      EXCEPTIONS
        already_defined = 1
        obj_invalid     = 2
        OTHERS          = 3  ).

    IF sy-subrc <> 0.
      " 009	Error adding dragdrop behavior
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e009.
    ENDIF.

    " empty square can only be dropped
    me->mo_only_drop = NEW #( ).
    me->mo_only_drop->add(
      EXPORTING
        flavor          = 'Image'
        dragsrc         = abap_false
        droptarget      = abap_true
        effect          = cl_dragdrop=>move
      EXCEPTIONS
        already_defined = 1
        obj_invalid     = 2
        OTHERS          = 3  ).

    IF sy-subrc <> 0.
      " 009	Error adding dragdrop behavior
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e009.
    ENDIF.

  ENDMETHOD.


  METHOD display.
    " TODO delete me
    RETURN.
  ENDMETHOD.


  METHOD free.

    LOOP AT me->mt_board INTO DATA(ls_board).

      ls_board-container->free(
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3 ).

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDLOOP.

    me->mo_custom_move_list->free(
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD get_dragdop_object.

    " default/fallback
    ro_result = me->mo_only_drop.

    IF io_piece IS BOUND AND
       io_piece->mv_color = me->mo_game->mo_current_player->mv_color.

      ro_result = me->mo_drag_and_drop.

    ENDIF.

  ENDMETHOD.


  METHOD handler_timer.

    IF me->mo_game->mo_current_player->mv_color = zif_chess_piece=>cs_color-white.
      me->mr_dynpro_fields->white_clock = me->mo_game->mo_clock->get_thinking_time( me->mo_game->mo_current_player->mv_color ).
    ELSE.
      me->mr_dynpro_fields->black_clock = me->mo_game->mo_clock->get_thinking_time( me->mo_game->mo_current_player->mv_color ).
    ENDIF.

    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = cs_ok_code-update
      EXCEPTIONS
        function_not_supported = 1
        OTHERS                 = 2.

    me->mo_timer->run( EXCEPTIONS OTHERS = 0 ).

  ENDMETHOD.


  METHOD handle_user_command.

    CASE iv_ucomm.
      WHEN zcl_chess_gui_board=>cs_ok_code-back OR
           zcl_chess_gui_board=>cs_ok_code-exit OR
           zcl_chess_gui_board=>cs_ok_code-cancel.
        me->resign( ).
        me->free( ).
        LEAVE TO SCREEN 0.
      WHEN zcl_chess_gui_board=>cs_ok_code-resign.
        me->resign( ).
      WHEN zcl_chess_gui_board=>cs_ok_code-undo.
        me->undo( ).
    ENDCASE.

  ENDMETHOD.


  METHOD initialize.

    me->mr_dynpro_fields = ir_dynpro_fields.

    me->mr_dynpro_fields->white_player_name = me->mo_white->zif_chess_player~mv_player_name.
    me->mr_dynpro_fields->black_player_name = me->mo_black->zif_chess_player~mv_player_name.

    CASE iv_perspective.
      WHEN zif_chess_piece=>cs_color-white.
        me->mr_dynpro_fields->board_subscreen = cs_screen-screen_4000.
      WHEN zif_chess_piece=>cs_color-black.
        me->mr_dynpro_fields->board_subscreen = cs_screen-screen_4001.
    ENDCASE.

    me->mo_board = NEW zcl_chess_board( ).

    me->mo_board->initialize( NEW zcl_chess_board_setup_classic( ) ).

    " game should be created before GUI is being initialized because dragdrop
    " depends on game state
    me->mo_game = NEW zcl_chess_game(
                          io_board = me->mo_board
                          io_black = me->mo_black
                          io_white = me->mo_white ).

    me->initialize_squares( ).
    me->initialize_move_list( ).

    " start the clock
    me->mo_game->mo_clock->start( ).

  ENDMETHOD.


  METHOD initialize_move_list.

    CREATE OBJECT me->mo_custom_move_list
      EXPORTING
        container_name              = 'MOVE_LIST'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    DATA(lv_subrc) = sy-subrc.

    IF lv_subrc <> 0.
      " 004	Error creating gui custom container. SUBRC=&1
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e004 WITH |{ lv_subrc STYLE = SIMPLE }|.
    ENDIF.

    CREATE OBJECT me->mo_alv_move_list
      EXPORTING
        i_parent          = me->mo_custom_move_list
      EXCEPTIONS
        error_cntl_create = 1                " Error when creating the control
        error_cntl_init   = 2                " Error While Initializing Control
        error_cntl_link   = 3                " Error While Linking Control
        error_dp_create   = 4                " Error While Creating DataProvider Control
        OTHERS            = 5.

    lv_subrc = sy-subrc.

    IF lv_subrc <> 0.
      " 023	Error creating gui alv grid. SUBRC=&1
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e023 WITH |{ lv_subrc STYLE = SIMPLE }|.
    ENDIF.

    me->mo_alv_move_list->set_table_for_first_display(
      EXPORTING
        i_bypassing_buffer            = abap_true
        i_structure_name              = 'ZCHESS_MOVE_LIST_ALV'
        is_layout                     = VALUE #( cwidth_opt = abap_true )
        it_toolbar_excluding          = VALUE #( ( cl_gui_alv_grid=>mc_fc_excl_all ) )
      CHANGING
        it_outtab                     = me->mt_move_list
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4 ).

    lv_subrc = sy-subrc.

    IF lv_subrc <> 0.
      " 024	Error calling method &1->&2. SUBRC=&3
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e024 WITH 'CL_GUI_ALV_GRID' 'SET_TABLE_FOR_FIRST_DISPLAY' |{ lv_subrc STYLE = SIMPLE }|.
    ENDIF.

    SET HANDLER me->move_list_toolbar FOR me->mo_alv_move_list.

  ENDMETHOD.


  METHOD initialize_squares.

    LOOP AT me->mo_board->mt_squares INTO DATA(lo_square).

      DATA(lo_piece) = me->mo_board->get_piece( lo_square ).

      me->add_square(
          io_square = lo_square
          io_piece  = lo_piece ).

      SET HANDLER me->on_move FOR lo_square.

    ENDLOOP.

  ENDMETHOD.


  METHOD move_list_toolbar.
    CLEAR e_object->mt_btnmnu.
    CLEAR e_object->mt_toolbar.
  ENDMETHOD.


  METHOD on_move.

    TRY.

        CASE me->mo_game->mo_current_player->mv_color.
          WHEN zif_chess_piece=>cs_color-white.
            me->mo_white->mo_next_move = io_move.
          WHEN zif_chess_piece=>cs_color-black.
            me->mo_black->mo_next_move = io_move.
        ENDCASE.

        me->play_move( ).

        me->set_function_code( cs_ok_code-update ).

      CATCH zcx_chess_exception INTO DATA(lx_exception).
        " dump
        MESSAGE lx_exception TYPE 'X'.
    ENDTRY.

  ENDMETHOD.


  METHOD play_move.

    CLEAR me->mr_dynpro_fields->message.

    DATA(lo_state) = me->mo_game->play_move_current_player( ).

    IF lo_state->ms_message IS NOT INITIAL.
      me->mr_dynpro_fields->message = lo_state->get_message_text( ).
    ENDIF.

    " TODO state processing

    CLEAR me->mo_white->mo_next_move.
    CLEAR me->mo_black->mo_next_move.

  ENDMETHOD.


  METHOD resign.

    IF me->mo_game->mo_game_result IS BOUND.
      RETURN.
    ENDIF.

    DATA(lo_resign_move) = me->mo_game->mo_current_player->get_resign_move( me->mo_board ).

    me->on_move( lo_resign_move ).

    rv_result = abap_true. " we assume resign move succeeded ...

  ENDMETHOD.


  METHOD set_drag_drop_object.

    DATA(lo_dragdrop) = me->get_dragdop_object( io_piece ).

    io_picture->set_dragdrop_picture(
      EXPORTING
        dragdrop = lo_dragdrop
      EXCEPTIONS
        error    = 1
        OTHERS   = 2 ).

    IF sy-subrc <> 0.
      " 010	Error setting dragdrop object to picture/control
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e010.
    ENDIF.

    io_picture->set_dragdrop_control(
      EXPORTING
        dragdrop = lo_dragdrop
      EXCEPTIONS
        error    = 1
        OTHERS   = 2 ).

    IF sy-subrc <> 0.
      " 010	Error setting dragdrop object to picture/control
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e010.
    ENDIF.

  ENDMETHOD.


  METHOD set_dynpro_fields_reference.
    me->mr_dynpro_fields = ir_dynpro_fields.
  ENDMETHOD.


  METHOD set_function_code.

    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = iv_code
      EXCEPTIONS
        function_not_supported = 1
        OTHERS                 = 2.

    DATA(lv_subrc) = sy-subrc.

    IF sy-subrc <> 0.
      " 013	Error in function module '&1'. SUBRC=&2
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e013(zchess_messages) WITH 'SAPGUI_SET_FUNCTIONCODE' |{ lv_subrc STYLE = SIMPLE }|.
    ENDIF.

  ENDMETHOD.


  METHOD undo.
    me->mo_game->undo_move( ).
  ENDMETHOD.


  METHOD update.

    CASE iv_screen.
      WHEN cs_screen-screen_9000.
        me->update_screen_9000( ).
      WHEN cs_screen-screen_4000 OR cs_screen-screen_4001.
        me->update_screen_400x( iv_screen ).
    ENDCASE.

  ENDMETHOD.


  METHOD update_screen_400x.

    DATA(lv_white_active) = xsdbool( me->mo_game->mo_current_player->mv_color = zif_chess_piece=>cs_color-white ).

    IF lv_white_active = abap_true.
      me->mr_dynpro_fields->white_icon = icon_release.
      me->mr_dynpro_fields->black_icon = icon_space.
    ELSE.
      me->mr_dynpro_fields->white_icon = icon_space.
      me->mr_dynpro_fields->black_icon = icon_release.
    ENDIF.

    LOOP AT SCREEN INTO DATA(ls_screen).

      CASE ls_screen-name.
        WHEN 'ZCHESS_GUI_DYNPRO_FIELDS-WHITE_PLAYER_NAME'.

          IF lv_white_active = abap_true.
            ls_screen-intensified = '1'.
          ELSE.
            ls_screen-intensified = '0'.
          ENDIF.

        WHEN 'ZCHESS_GUI_DYNPRO_FIELDS-BLACK_PLAYER_NAME'.

          IF lv_white_active = abap_false.
            ls_screen-intensified = '1'.
          ELSE.
            ls_screen-intensified = '0'.
          ENDIF.

        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

      MODIFY SCREEN FROM ls_screen.

    ENDLOOP.

  ENDMETHOD.


  METHOD update_screen_9000.

    LOOP AT me->mo_board->mt_squares INTO DATA(lo_square).
      " TODO optimize to only update necessary squares
      " ALSO reminder: special moves like en passant exist!
      me->update_square( lo_square ).
    ENDLOOP.

    me->mt_move_list = NEW zcl_chess_move_list( )->get( me->mo_board ).

    me->mo_alv_move_list->refresh_table_display(
      EXPORTING
        i_soft_refresh = abap_true
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2 ).

    DATA(lv_subrc) = sy-subrc.

    IF lv_subrc <> 0.
      " 024	Error calling method &1->&2. SUBRC=&3
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e024 WITH 'CL_GUI_ALV_GRID' 'REFRESH_TABLE_DISPLAY' |{ lv_subrc STYLE = SIMPLE }|.
    ENDIF.

  ENDMETHOD.


  METHOD update_square.

    DATA(lo_piece) = me->mo_board->get_piece( io_square ).

    READ TABLE me->mt_board
      ASSIGNING FIELD-SYMBOL(<ls_board>)
      WITH KEY name = io_square->mv_name.

    IF sy-subrc <> 0.
      " 011	Square &1 not found
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e011 WITH io_square->mv_name.
    ENDIF.

    DATA(lv_url) = me->mo_picture_buffer->get_url(
                     io_piece  = lo_piece
                     io_square = io_square ).

    <ls_board>-picture->load_picture( lv_url ).

    me->set_drag_drop_object(
        io_piece    = lo_piece
        io_picture  = <ls_board>-picture->mo_picture ).

  ENDMETHOD.
ENDCLASS.

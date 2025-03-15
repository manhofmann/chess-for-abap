class ZCL_CHESS_PLAYER_GUI_LOCAL definition
  public
  inheriting from ZCL_CHESS_PLAYER_GUI_MULTIPLAY
  final
  create public .

public section.

  data MV_PLAYER_ID type SYSUUID_C32 read-only .

  methods SET_PLAYER_COLOR
    importing
      !IV_COLOR type ZCHESS_PIECE_COLOR .
  methods CONSTRUCTOR
    importing
      !IO_CLIENT type ref to ZCL_CHESS_AMC_CLIENT
    raising
      ZCX_CHESS_EXCEPTION .
protected section.
private section.

  data MO_CLIENT type ref to ZCL_CHESS_AMC_CLIENT .
ENDCLASS.



CLASS ZCL_CHESS_PLAYER_GUI_LOCAL IMPLEMENTATION.


  METHOD constructor.

    DATA ls_address TYPE addr3_val.

    super->constructor( space ).

    me->mo_client = io_client.

    me->zif_chess_player~mo_move_handler = zcl_chess_move_handler_remote=>create_move_handler(
                                             iv_remote     = abap_true
                                             io_client     = io_client ).

    CALL FUNCTION 'SUSR_USER_ADDRESS_READ'
      EXPORTING
        user_name              = sy-uname
      IMPORTING
        user_address           = ls_address
      EXCEPTIONS
        user_address_not_found = 1
        OTHERS                 = 2.

    IF sy-subrc <> 0.
      CLEAR ls_address.
    ENDIF.

    me->zif_chess_player~mv_player_name = |{ sy-uname } - { ls_address-name_first } { ls_address-name_last }|.

    TRY.
        me->mv_player_id = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_uuid_error INTO DATA(lx_uuid_exception).
        " 048	UUID could not be generated
        RAISE EXCEPTION TYPE zcx_chess_exception
          MESSAGE e048
          EXPORTING
            previous = lx_uuid_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD set_player_color.
    " this method is needed because we do not know beforehand the color
    " this will be decided by game server class randomly
    me->zif_chess_player~mv_color = iv_color.
  ENDMETHOD.
ENDCLASS.

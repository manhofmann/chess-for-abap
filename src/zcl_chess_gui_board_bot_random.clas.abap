class ZCL_CHESS_GUI_BOARD_BOT_RANDOM definition
  public
  inheriting from ZCL_CHESS_GUI_BOARD_MULTIPLAY
  create public .

public section.

  methods CONSTRUCTOR
    raising
      ZCX_CHESS_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CHESS_GUI_BOARD_BOT_RANDOM IMPLEMENTATION.


  METHOD constructor.

    super->constructor(
        io_white  = NEW zcl_chess_player_gui_multiplay( zif_chess_piece=>cs_color-white )
        io_black  = NEW zcl_chess_player_bot_random( zif_chess_piece=>cs_color-black ) ).

  ENDMETHOD.
ENDCLASS.

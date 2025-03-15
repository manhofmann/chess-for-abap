class ZCL_CHESS_GAME_SIMULATION definition
  public
  inheriting from ZCL_CHESS_GAME
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_BOARD type ref to ZIF_CHESS_BOARD
      !IO_WHITE type ref to ZIF_CHESS_PLAYER
      !IO_BLACK type ref to ZIF_CHESS_PLAYER
      !IV_CURRENT_PLAYER type ZCHESS_PIECE_COLOR
    raising
      ZCX_CHESS_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CHESS_GAME_SIMULATION IMPLEMENTATION.


  METHOD constructor.

    super->constructor(
        io_board = io_board
        io_white = io_white
        io_black = io_black ).

    " change starting player in simulation to be able to start from any (board) position
    CASE iv_current_player.
      WHEN zif_chess_piece=>cs_color-white.
        me->mo_current_player = io_white.
      WHEN zif_chess_piece=>cs_color-black.
        me->mo_current_player = io_black.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.

INTERFACE zif_chess_move_state
  PUBLIC .


  CONSTANTS:
    BEGIN OF cs_move_state_code,
      normal               TYPE zchess_move_state_info_code VALUE 0,
      check                TYPE zchess_move_state_info_code VALUE 10,
      draw_ins_material    TYPE zchess_move_state_info_code VALUE 20,
      draw_50_moves        TYPE zchess_move_state_info_code VALUE 21,
      threefold_repetition TYPE zchess_move_state_info_code VALUE 22,
      stalemate            TYPE zchess_move_state_info_code VALUE 40,
      checkmate            TYPE zchess_move_state_info_code VALUE 50,
      resignation          TYPE zchess_move_state_info_code VALUE 60,
    END OF cs_move_state_code .

  METHODS get_state
    IMPORTING
      !io_board        TYPE REF TO zif_chess_board
      !io_player       TYPE REF TO zif_chess_player
    RETURNING
      VALUE(ro_result) TYPE REF TO zcl_chess_move_state_info
    RAISING
      zcx_chess_exception .
ENDINTERFACE.

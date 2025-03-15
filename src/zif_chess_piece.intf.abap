INTERFACE zif_chess_piece
  PUBLIC .


  INTERFACES if_serializable_object .

  DATA mv_color TYPE zchess_piece_color READ-ONLY .
  CONSTANTS:
    BEGIN OF cs_type,
      king   TYPE zchess_piece_type VALUE 'K',
      queen  TYPE zchess_piece_type VALUE 'Q',
      rook   TYPE zchess_piece_type VALUE 'R',
      knight TYPE zchess_piece_type VALUE 'N',
      bishop TYPE zchess_piece_type VALUE 'B',
      pawn   TYPE zchess_piece_type VALUE 'P',
    END OF cs_type .


  CONSTANTS:
    BEGIN OF cs_value,
      king   TYPE zchess_piece_value VALUE 60000,
      queen  TYPE zchess_piece_value VALUE 900,
      rook   TYPE zchess_piece_value VALUE 500,
      knight TYPE zchess_piece_value VALUE 300,
      bishop TYPE zchess_piece_value VALUE 320,
      pawn   TYPE zchess_piece_value VALUE 100,
    END OF cs_value .

  CONSTANTS:
    BEGIN OF cs_color,
      white TYPE zchess_piece_color VALUE 'W',
      black TYPE zchess_piece_color VALUE 'B',
    END OF cs_color .
  DATA mv_move_amount TYPE int1 READ-ONLY .
  DATA mv_type TYPE zchess_piece_type READ-ONLY .

  METHODS has_been_moved
    RETURNING
      VALUE(rv_result) TYPE boole_d .
  METHODS got_moved
    IMPORTING
      !iv_times TYPE int2 DEFAULT 1 .
  METHODS is_move_valid
    IMPORTING
      !io_move         TYPE REF TO zif_chess_move
      !io_board        TYPE REF TO zif_chess_board_read
    RETURNING
      VALUE(rv_result) TYPE boole_d
    RAISING
      zcx_chess_exception .
  METHODS is_ally
    IMPORTING
      !io_piece        TYPE REF TO zif_chess_piece
    RETURNING
      VALUE(rv_result) TYPE boole_d
    RAISING
      zcx_chess_exception .
  METHODS get_all_possible_moves
    IMPORTING
      !io_square       TYPE REF TO zif_chess_square
    RETURNING
      VALUE(rt_result) TYPE zchess_tt_move
    RAISING
      zcx_chess_exception .
  METHODS get_piece_value
    RETURNING
      VALUE(rv_result) TYPE zchess_piece_value .
ENDINTERFACE.

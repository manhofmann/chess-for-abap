class ZCL_CHESS_BOARD_SETUP_CLASSIC definition
  public
  final
  create public .

public section.

  interfaces ZIF_CHESS_BOARD_SETUP .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CHESS_BOARD_SETUP_CLASSIC IMPLEMENTATION.


  METHOD zif_chess_board_setup~get_config.

    " first line equals first rank (1) (we see black perspectives)
    " each line consists of 16 (8x2) characters:
    " e.g. WR means white rook
    " a (double) dot means empty square
    rt_result = VALUE #( ( 'WRWNWBWQWKWBWNWR' )
                         ( 'WPWPWPWPWPWPWPWP' )
                         ( '................' )
                         ( '................' )
                         ( '................' )
                         ( '................' )
                         ( 'BPBPBPBPBPBPBPBP' )
                         ( 'BRBNBBBQBKBBBNBR' ) ).

  ENDMETHOD.
ENDCLASS.

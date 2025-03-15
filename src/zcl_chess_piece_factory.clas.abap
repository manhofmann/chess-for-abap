class ZCL_CHESS_PIECE_FACTORY definition
  public
  final
  create public .

public section.

  methods CREATE_PIECE
    importing
      !IV_COLOR type ZCHESS_PIECE_COLOR
      !IV_TYPE type ZCHESS_PIECE_TYPE
    returning
      value(RO_RESULT) type ref to ZIF_CHESS_PIECE
    raising
      ZCX_CHESS_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CHESS_PIECE_FACTORY IMPLEMENTATION.


  METHOD create_piece.

    CASE iv_type.
      WHEN zif_chess_piece=>cs_type-king.
        ro_result = NEW zcl_chess_piece_king( iv_color ).
      WHEN zif_chess_piece=>cs_type-queen.
        ro_result = NEW zcl_chess_piece_queen( iv_color ).
      WHEN zif_chess_piece=>cs_type-rook.
        ro_result = NEW zcl_chess_piece_rook( iv_color ).
      WHEN zif_chess_piece=>cs_type-knight.
        ro_result = NEW zcl_chess_piece_knight( iv_color ).
      WHEN zif_chess_piece=>cs_type-bishop.
        ro_result = NEW zcl_chess_piece_bishop( iv_color ).
      WHEN zif_chess_piece=>cs_type-pawn.
        ro_result = NEW zcl_chess_piece_pawn( iv_color ).
      WHEN OTHERS.
        " 022	Invalid piece type &1
        RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e022 WITH iv_type.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.

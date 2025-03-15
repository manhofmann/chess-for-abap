class ZCL_CHESS_PIECE_UTILITY definition
  public
  final
  create public .

public section.

  class-methods GET_COLOR_NAME
    importing
      !IV_COLOR type ZCHESS_PIECE_COLOR
    returning
      value(RV_RESULT) type TEXT20
    raising
      ZCX_CHESS_EXCEPTION .
  class-methods GET_OPPOSITE_COLOR
    importing
      !IV_COLOR type ZCHESS_PIECE_COLOR
    returning
      value(RV_RESULT) type ZCHESS_PIECE_COLOR
    raising
      ZCX_CHESS_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CHESS_PIECE_UTILITY IMPLEMENTATION.


  METHOD get_color_name.

    CASE iv_color.
      WHEN zif_chess_piece=>cs_color-white.
        rv_result = TEXT-001.
      WHEN zif_chess_piece=>cs_color-black.
        rv_result = TEXT-002.
      WHEN OTHERS.
        " 019	Invalid color
        RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e019.
    ENDCASE.

  ENDMETHOD.


  METHOD get_opposite_color.

    CASE iv_color.
      WHEN zif_chess_piece=>cs_color-black.
        rv_result = zif_chess_piece=>cs_color-white.
      WHEN zif_chess_piece=>cs_color-white.
        rv_result = zif_chess_piece=>cs_color-black.
      WHEN OTHERS.
        " 019	Invalid color
        RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e019.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.

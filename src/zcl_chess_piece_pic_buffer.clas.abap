class ZCL_CHESS_PIECE_PIC_BUFFER definition
  public
  final
  create public .

public section.

  methods GET_URL
    importing
      !IO_PIECE type ref to ZIF_CHESS_PIECE
      !IO_SQUARE type ref to ZIF_CHESS_SQUARE
    returning
      value(RV_RESULT) type CNDP_URL
    raising
      ZCX_CHESS_EXCEPTION .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_buffer,
        object       TYPE w3objid,
        url          TYPE cndp_url,
      END OF ty_buffer .
  types:
    tty_buffer TYPE STANDARD TABLE OF ty_buffer WITH EMPTY KEY .

  data MT_BUFFER type TTY_BUFFER .

  methods PUBLIC_WWW_URL
    importing
      !IV_OBJECT type W3OBJID
    returning
      value(RV_RESULT) type CNDP_URL
    raising
      ZCX_CHESS_EXCEPTION .
  methods GET_WWW_OBJECT_NAME
    importing
      !IO_PIECE type ref to ZIF_CHESS_PIECE
      !IO_SQUARE type ref to ZIF_CHESS_SQUARE
    returning
      value(RV_RESULT) type W3OBJID
    raising
      ZCX_CHESS_EXCEPTION .
ENDCLASS.



CLASS ZCL_CHESS_PIECE_PIC_BUFFER IMPLEMENTATION.


  METHOD get_url.

    DATA ls_buffer       TYPE ty_buffer.

    ls_buffer-object = me->get_www_object_name(
                             io_piece  = io_piece
                             io_square = io_square ).

    READ TABLE me->mt_buffer
      INTO ls_buffer
      WITH KEY object = ls_buffer-object.

    IF sy-subrc <> 0.

      ls_buffer-url = me->public_www_url( ls_buffer-object ).

      INSERT ls_buffer INTO TABLE me->mt_buffer.

    ENDIF.

    rv_result = ls_buffer-url.

  ENDMETHOD.


  METHOD get_www_object_name.

    IF io_piece IS NOT BOUND.

      " add empty square (square without a piece on it)
      IF io_square->is_light( ).
        rv_result = 'ZCHESS_PIECE_SQUARE-LIGHT'.
      ELSE.
        rv_result = 'ZCHESS_PIECE_SQUARE-DARK'.
      ENDIF.

    ELSE.

      rv_result = 'ZCHESS_PIECE_'.

      CASE io_piece->mv_color.
        WHEN zif_chess_piece=>cs_color-white.
          rv_result = |{ rv_result }WHITE-|.
        WHEN zif_chess_piece=>cs_color-black.
          rv_result = |{ rv_result }BLACK-|.
        WHEN OTHERS.
          " 019	Invalid color
          RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e019.
      ENDCASE.

      CASE io_piece->mv_type.
        WHEN zif_chess_piece=>cs_type-king.
          rv_result = |{ rv_result }KING-|.
        WHEN zif_chess_piece=>cs_type-queen.
          rv_result = |{ rv_result }QUEEN-|.
        WHEN zif_chess_piece=>cs_type-rook.
          rv_result = |{ rv_result }ROOK-|.
        WHEN zif_chess_piece=>cs_type-bishop.
          rv_result = |{ rv_result }BISHOP-|.
        WHEN zif_chess_piece=>cs_type-knight.
          rv_result = |{ rv_result }KNIGHT-|.
        WHEN zif_chess_piece=>cs_type-pawn.
          rv_result = |{ rv_result }PAWN-|.
        WHEN OTHERS.
          " 022	Invalid piece type &1
          RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e022 WITH io_piece->mv_type.
      ENDCASE.

      IF io_square->is_light( ).
        rv_result = |{ rv_result }LIGHT|.
      ELSE.
        rv_result = |{ rv_result }DARK|.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD public_www_url.

    CALL FUNCTION 'DP_PUBLISH_WWW_URL'
      EXPORTING
        objid                 = iv_object
        "  lifetime needs to be set to ALL, because we have own buffer: pictures are reused
        lifetime              = cndp_lifetime_all
      IMPORTING
        url                   = rv_result
      EXCEPTIONS
        dp_invalid_parameters = 1
        no_object             = 2
        dp_error_publish      = 3
        OTHERS                = 4.

    DATA(lv_subrc) = sy-subrc.

    IF sy-subrc <> 0.
      " 013	Error in function module '&1'. SUBRC=&2
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e013 WITH 'DP_PUBLISH_WWW_URL' |{ lv_subrc STYLE = SIMPLE }|.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

class ZCL_CHESS_PLAYER_BOT_RANDOM definition
  public
  inheriting from ZCL_CHESS_PLAYER_GUI
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_COLOR type ZCHESS_PIECE_COLOR
    raising
      ZCX_CHESS_EXCEPTION .

  methods ZIF_CHESS_PLAYER~GET_INPUT
    redefinition .
protected section.
private section.

  data MO_BOARD_UTILITY type ref to ZCL_CHESS_BOARD_UTILITY .
  data MV_RANDOM_SEED type INT4 .
ENDCLASS.



CLASS ZCL_CHESS_PLAYER_BOT_RANDOM IMPLEMENTATION.


  METHOD constructor.

    super->constructor( iv_color ).

    me->mo_board_utility = NEW #( ).

    me->zif_chess_player~mv_player_name = 'Bot (random moves)'.

    me->mv_random_seed = cl_abap_random=>seed( ).

  ENDMETHOD.


  METHOD zif_chess_player~get_input.

    DATA(lt_legal_moves) = me->mo_board_utility->get_legal_moves(
                            iv_color                = me->zif_chess_player~mv_color
                            io_board                = CAST #( io_board )
                            iv_with_promotion_moves = abap_true ).

    IF lt_legal_moves IS INITIAL.
      " 052	Legal moves are empty
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e052.
    ENDIF.

    DATA(lo_random) = cl_abap_random_int=>create(
                            seed  = me->mv_random_seed
                            min   = 1
                            max   = lines( lt_legal_moves ) ).

    DATA(lv_index) = lo_random->get_next( ).

    READ TABLE lt_legal_moves INTO ro_result INDEX lv_index.

  ENDMETHOD.
ENDCLASS.

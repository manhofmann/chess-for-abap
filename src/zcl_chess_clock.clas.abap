class ZCL_CHESS_CLOCK definition
  public
  final
  create public .

public section.

  data MV_STARTED_AT type ZCHESS_CLOCK_STARTED_AT .

  methods CONSTRUCTOR
    importing
      !IO_BOARD type ref to ZIF_CHESS_BOARD_READ .
  methods GET_THINKING_TIME
    importing
      !IV_COLOR type ZCHESS_PIECE_COLOR
    returning
      value(RV_RESULT) type INT4 .
  methods START .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_move_duration,
        color    TYPE zchess_piece_color,
        duration TYPE tzntstmpl,
      END OF ty_move_duration .
  types:
    tty_move_duration TYPE STANDARD TABLE OF ty_move_duration WITH EMPTY KEY .

  data MO_BOARD type ref to ZIF_CHESS_BOARD_READ .

  methods GET_MOVE_DURATION
    importing
      !IV_COLOR type ZCHESS_PIECE_COLOR
    returning
      value(RT_RESULT) type TTY_MOVE_DURATION .
ENDCLASS.



CLASS ZCL_CHESS_CLOCK IMPLEMENTATION.


  METHOD constructor.
    me->mo_board = io_board.
  ENDMETHOD.


  METHOD get_move_duration.

    DATA lv_previous_moved_at TYPE zchess_moved_at.
    DATA lv_current           TYPE zchess_moved_at.
    DATA ls_result            TYPE ty_move_duration.

    lv_previous_moved_at = me->mv_started_at.

    LOOP AT me->mo_board->mt_piece_moves INTO DATA(ls_piece_move).

      ls_result-color     = ls_piece_move-piece->mv_color.
      ls_result-duration  = cl_abap_tstmp=>subtract(
                              tstmp1 = ls_piece_move-move->mv_moved_at
                              tstmp2 = lv_previous_moved_at ).

      INSERT ls_result INTO TABLE rt_result.

      lv_previous_moved_at = ls_piece_move-move->mv_moved_at.

    ENDLOOP.

    " if last move is from other player it means IV_COLOR has to move currently
    " add current timestamp as last move to have 'current thinking time'
    IF ls_result-color <> iv_color.

      ls_result-color     = iv_color.
      GET TIME STAMP FIELD lv_current.
      ls_result-duration  = cl_abap_tstmp=>subtract(
                              tstmp1 = lv_current
                              tstmp2 = lv_previous_moved_at ).

      INSERT ls_result INTO TABLE rt_result.

    ENDIF.

  ENDMETHOD.


  METHOD get_thinking_time.

    DATA lv_duration_seconds TYPE int4.
    DATA lv_timestamp_trunacted TYPE rstimestmp.
    DATA lv_timestamp_fraction  TYPE tzntstmpl.

    DATA(lt_move_duration) = me->get_move_duration( iv_color ).

    LOOP AT lt_move_duration
      ASSIGNING FIELD-SYMBOL(<ls_move_duration>)
      WHERE color = iv_color.

*      lv_timestamp_trunacted = trunc( <ls_move_duration>-duration ).

*      lv_timestamp_fraction = lv_timestamp_fraction + frac( <ls_move_duration>-duration ).

*      CALL FUNCTION 'RSSM_CONVERT_TIMESTAMP2DAYSEC'
*        EXPORTING
*          timestamp                  = lv_timestamp_trunacted
*        IMPORTING
*          sec                        = lv_duration_seconds
*        EXCEPTIONS
*          wrong_type                 = 1
*          wrong_number_of_parameters = 2
*          OTHERS                     = 3.
*
*      IF sy-subrc <> 0.
*        RAISE EXCEPTION TYPE zcx_chess_exception. " TODO
*      ENDIF.

      rv_result = rv_result + <ls_move_duration>-duration.

    ENDLOOP.

*    lv_timestamp_trunacted = trunc( lv_timestamp_fraction ).
*
*    IF lv_timestamp_trunacted > 0.
*
*      CALL FUNCTION 'RSSM_CONVERT_TIMESTAMP2DAYSEC'
*        EXPORTING
*          timestamp                  = lv_timestamp_trunacted
*        IMPORTING
*          sec                        = lv_duration_seconds
*        EXCEPTIONS
*          wrong_type                 = 1
*          wrong_number_of_parameters = 2
*          OTHERS                     = 3.
*
*      IF sy-subrc <> 0.
*        RAISE EXCEPTION TYPE zcx_chess_exception. " TODO
*      ENDIF.
*
*      rv_result = rv_result + lv_duration_seconds.
*
*    ENDIF.

  ENDMETHOD.


  METHOD start.

    IF me->mv_started_at IS NOT INITIAL.
      RETURN.
    ENDIF.

    GET TIME STAMP FIELD me->mv_started_at.

  ENDMETHOD.
ENDCLASS.

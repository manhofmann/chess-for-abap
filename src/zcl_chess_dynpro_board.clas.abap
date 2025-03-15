class ZCL_CHESS_DYNPRO_BOARD definition
  public
  abstract
  create public .

public section.

  data MO_GUI_BOARD type ref to ZCL_CHESS_GUI_BOARD read-only .
  data MR_DYNPRO_FIELDS type ref to ZCHESS_GUI_DYNPRO_FIELDS read-only .

  class-methods DISPLAY
    importing
      !IO_GUI_BOARD type ref to ZCL_CHESS_GUI_BOARD
    raising
      ZCX_CHESS_EXCEPTION .
  methods CONSTRUCTOR
    importing
      !IO_GUI_BOARD type ref to ZCL_CHESS_GUI_BOARD
      !IR_DYNPRO_FIELDS type ref to ZCHESS_GUI_DYNPRO_FIELDS .
  methods PAI
    importing
      !IV_UCOMM type SYUCOMM .
  methods CALL_SCREEN
  abstract .
  methods PBO
    importing
      !IV_SCREEN type SYST_DYNNR default SY-DYNNR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CHESS_DYNPRO_BOARD IMPLEMENTATION.


  METHOD constructor.

    me->mo_gui_board      = io_gui_board.
    me->mr_dynpro_fields  = ir_dynpro_fields.

  ENDMETHOD.


  METHOD display.

    DATA lo_dynpro_board TYPE REF TO zcl_chess_dynpro_board.

    CALL FUNCTION 'Z_CHESS_DYNPRO_BOARD_GET'
      EXPORTING
        io_gui_board    = io_gui_board
      IMPORTING
        eo_dynpro_board = lo_dynpro_board.

    io_gui_board->initialize( ir_dynpro_fields = lo_dynpro_board->mr_dynpro_fields ).

    lo_dynpro_board->call_screen( ).

  ENDMETHOD.


  METHOD pai.

    TRY.
        me->mo_gui_board->handle_user_command( iv_ucomm ).
      CATCH zcx_chess_exception INTO DATA(lx_exception).
        MESSAGE lx_exception TYPE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD pbo.

    TRY.
        me->mo_gui_board->update( iv_screen ).
      CATCH zcx_chess_exception INTO DATA(lx_exception).
        MESSAGE lx_exception TYPE 'E'.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.

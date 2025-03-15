class ZCL_CHESS_SQUARE_PICTURE definition
  public
  final
  create public .

public section.

  data MO_PICTURE type ref to CL_GUI_PICTURE read-only .

  methods LOAD_PICTURE
    importing
      !IV_URL type CNDP_URL
    raising
      ZCX_CHESS_EXCEPTION .
  methods CONSTRUCTOR
    importing
      !IO_SQUARE type ref to ZIF_CHESS_SQUARE
      !IO_PARENT type ref to CL_GUI_CUSTOM_CONTAINER
    raising
      ZCX_CHESS_EXCEPTION .
protected section.
private section.

  data MO_SQUARE type ref to ZIF_CHESS_SQUARE .
  data MV_URL type CNDP_URL .
ENDCLASS.



CLASS ZCL_CHESS_SQUARE_PICTURE IMPLEMENTATION.


  METHOD constructor.

    CREATE OBJECT me->mo_picture
      EXPORTING
        parent = io_parent
        name   = CONV #( io_square->mv_name )
      EXCEPTIONS
        error  = 1
        OTHERS = 2.

    DATA(lv_subrc) = sy-subrc.

    IF lv_subrc <> 0.
      " 005 Error creating gui picture. SUBRC=&1
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e005 WITH |{ lv_subrc STYLE = SIMPLE }|.
    ENDIF.

*    DATA(lv_url) = me->mo_picture_buffer->get_url(
*                     io_piece  = io_piece
*                     io_square = io_square ).
*
*    me->mo_picture->load_picture_from_url_async(
*      EXPORTING
*        url    = lv_url
*      EXCEPTIONS
*        error  = 1
*        OTHERS = 2 ).
*
*    IF sy-subrc <> 0.
*      " 006  Error loading picture async
*      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e006.
*    ENDIF.

    me->mo_picture->set_display_mode(
      EXPORTING
        display_mode = cl_gui_picture=>display_mode_stretch
      EXCEPTIONS
        error        = 1
        OTHERS       = 2 ).

    IF sy-subrc <> 0.
      " 007	Error setting display mode
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e007.
    ENDIF.

    me->mo_picture->set_registered_events(
      EXPORTING
        events                    = VALUE #( )
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3
        OTHERS                    = 4 ).

    IF sy-subrc <> 0.
      " 008	Error setting events
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e008.
    ENDIF.

    SET HANDLER io_square->ondrag FOR me->mo_picture.
    SET HANDLER io_square->ondrop FOR me->mo_picture.

  ENDMETHOD.


  METHOD load_picture.

    IF iv_url <> me->mv_url.

      me->mo_picture->load_picture_from_url_async(
        EXPORTING
          url    = iv_url
        EXCEPTIONS
          error  = 1
          OTHERS = 2 ).

      IF sy-subrc <> 0.
        " 012	Error loading picture
        RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e012.
      ENDIF.

      me->mv_url = iv_url.

    ENDIF.

  ENDMETHOD.
ENDCLASS.

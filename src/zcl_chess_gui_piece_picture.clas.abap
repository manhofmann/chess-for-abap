class ZCL_CHESS_GUI_PIECE_PICTURE definition
  public
  inheriting from CL_GUI_PICTURE
  final
  create private .

public section.

  methods LOAD_PICTURE
    importing
      !IV_NAME type W3OBJID
    raising
      ZCX_CHESS_EXCEPTION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CHESS_GUI_PIECE_PICTURE IMPLEMENTATION.


  METHOD load_picture.

    DATA lv_url TYPE cndp_url.

    CALL FUNCTION 'DP_PUBLISH_WWW_URL'
      EXPORTING
        objid                 = iv_name
        lifetime              = cndp_lifetime_transaction
      IMPORTING
        url                   = lv_url
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

    me->load_picture_from_url(
      EXPORTING
        url    = lv_url
      EXCEPTIONS
        error  = 1
        OTHERS = 2 ).

    IF sy-subrc <> 0.
      " 012	Error loading picture
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e012.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

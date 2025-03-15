CLASS zcl_chess_amc DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_amc_message_receiver .
    INTERFACES if_amc_message_receiver_pcp .

    CONSTANTS:
      BEGIN OF cs_pcp_field,
        register_multiplayer   TYPE string VALUE 'REGISTER_MULTIPLAYER',
        multiplayer_registered TYPE string VALUE 'MULTIPLAYER_REGISTERED',
        player_id              TYPE string VALUE 'PLAYER_ID',
        opponent_move          TYPE string VALUE 'OPPONENT_MOVE',
        opponent_joined        TYPE string VALUE 'OPPONENT_JOINED',
        method_name            TYPE string VALUE 'METHOD_NAME',
        player_name            TYPE string VALUE 'PLAYER_NAME',
      END OF cs_pcp_field .
    CONSTANTS:
      BEGIN OF cs_channel,
        client TYPE amc_channel_id VALUE '/client',
        server TYPE amc_channel_id VALUE '/server',
      END OF cs_channel .
    CONSTANTS cv_application_id TYPE amc_application_id VALUE 'ZCHESS_MULTIPLAYER' ##NO_TEXT.
    CONSTANTS cv_wait_time_in_seconds TYPE int4 VALUE 600 ##NO_TEXT.
protected section.

  data MO_RECEIVED_MESSAGE type ref to IF_AC_MESSAGE_TYPE_PCP .
  data MO_PRODUCER type ref to IF_AMC_MESSAGE_PRODUCER_PCP .

  methods DESERIALIZE_OBJECT
    importing
      !IV_BINARY type XSTRING
    returning
      value(RO_RESULT) type ref to OBJECT
    raising
      ZCX_CHESS_EXCEPTION .
  methods SERIALIZE_OBJECT
    importing
      !IO_OBJECT type ref to OBJECT
    returning
      value(RV_RESULT) type XSTRING
    raising
      ZCX_CHESS_EXCEPTION .
  methods WAIT_FOR_MESSAGE
    importing
      !IV_NAME type STRING optional
    exporting
      !ET_FIELDS type PCP_FIELDS
      !EV_BINARY type XSTRING
    raising
      ZCX_CHESS_EXCEPTION .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CHESS_AMC IMPLEMENTATION.


  METHOD deserialize_object.

    TRY.
        CALL TRANSFORMATION id
          SOURCE XML iv_binary
          RESULT object = ro_result.
      CATCH cx_transformation_error INTO DATA(lx_exception).
        " 036	Error during CALL TRANSFORMATION
        RAISE EXCEPTION TYPE zcx_chess_exception
          MESSAGE e036
          EXPORTING
            previous = lx_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD if_amc_message_receiver_pcp~receive.
    me->mo_received_message = i_message.
  ENDMETHOD.


  METHOD serialize_object.

    TRY.
        CALL TRANSFORMATION id
          SOURCE object = io_object
          RESULT XML rv_result.
      CATCH cx_transformation_error INTO DATA(lx_exception).
        " 036	Error during CALL TRANSFORMATION
        RAISE EXCEPTION TYPE zcx_chess_exception
          MESSAGE e036
          EXPORTING
            previous = lx_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD wait_for_message.

    CLEAR me->mo_received_message.
    CLEAR et_fields.
    CLEAR ev_binary.

    WAIT FOR MESSAGING CHANNELS
         UNTIL me->mo_received_message IS BOUND
         UP TO cv_wait_time_in_seconds SECONDS.

    IF sy-subrc <> 0.
      " 037	AMC Error - Wait time of &1 seconds exceeded
      RAISE EXCEPTION TYPE zcx_chess_exception MESSAGE e037 WITH |{ cv_wait_time_in_seconds STYLE = SIMPLE }|.
    ENDIF.

    TRY.

        me->mo_received_message->get_fields(
          CHANGING
            c_fields = et_fields ).

        ev_binary = me->mo_received_message->get_binary( ).

      CATCH cx_ac_message_type_pcp_error INTO DATA(lx_exception).
        " 038	AMC PCP Error
        RAISE EXCEPTION TYPE zcx_chess_exception
          MESSAGE e038
          EXPORTING
            previous = lx_exception.
    ENDTRY.

    IF iv_name IS NOT SUPPLIED.
      " this means we listen to any message
      RETURN.
    ENDIF.

    IF NOT line_exists( et_fields[ name = iv_name ] ).
      me->wait_for_message(
        EXPORTING
          iv_name   = iv_name
        IMPORTING
          et_fields = et_fields
          ev_binary = ev_binary ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

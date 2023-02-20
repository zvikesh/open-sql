CLASS zvks_cl_osql_inline DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    METHODS main
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS gc_carrid_aa TYPE string VALUE 'AA' ##NO_TEXT.

    METHODS param_demo
      EXPORTING
        ev_variable TYPE string.


ENDCLASS.



CLASS ZVKS_CL_OSQL_INLINE IMPLEMENTATION.


  METHOD main.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Inline Structure in SELECT
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    SELECT SINGLE
      FROM ZVKSR_FlightDetails AS flight
    FIELDS flight~AirlineID,
           flight~ConnectionID,
           flight~FlightDate,
           flight~SeatPrice,
           flight~CurrencyCode,
           flight~PlaneTypeID,
           flight~SeatsMax,
           flight~SeatsOccupied
     WHERE flight~AirlineID EQ @me->gc_carrid_aa                      "AA
      INTO @DATA(ls_flight_aa).                                          "<<< Inline Declaration
    IF sy-subrc IS NOT INITIAL.
      CLEAR ls_flight_aa.
    ELSE.
      out->write( |ls_flight_aa{ cl_abap_char_utilities=>newline }| ).
      out->write( ls_flight_aa ).
    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Inline Table in SELECT
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    SELECT
      FROM ZVKSR_FlightDetails AS flight
    FIELDS flight~AirlineID,
           flight~ConnectionID,
           flight~FlightDate,
           flight~SeatPrice,
           flight~CurrencyCode,
           flight~PlaneTypeID,
           flight~SeatsMax,
           flight~SeatsOccupied
     WHERE flight~AirlineID EQ @me->gc_carrid_aa                      "AA
      INTO TABLE @DATA(lt_flight).                                    "<<< Inline Declaration
    IF sy-subrc IS NOT INITIAL.
      CLEAR lt_flight.
    ELSE.
      out->write( |lt_flight{ cl_abap_char_utilities=>newline }| ).
      out->write( lt_flight ).
    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Work Area
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    LOOP AT lt_flight INTO DATA(ls_flight).
      out->write( |ls_flight{ cl_abap_char_utilities=>newline }| ).
      out->write( ls_flight ).
      EXIT.
    ENDLOOP.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Field Symbol
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    LOOP AT lt_flight ASSIGNING FIELD-SYMBOL(<lfs_flight>).
      out->write( |<lfs_sflight>{ cl_abap_char_utilities=>newline }| ).
      out->write( ls_flight ).
      EXIT.
    ENDLOOP.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Reference Object
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    LOOP AT lt_flight REFERENCE INTO DATA(lr_flight).
      out->write( |lr_slfight{ cl_abap_char_utilities=>newline }| ).
      out->write( lr_flight ).
      EXIT.
    ENDLOOP.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Fixed Length Character
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(lv_char16) = 'This is a string'.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " String Variable
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(lv_string) = |This is a string.|.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Integer Variable
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(lv_integer) = 10.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Structure
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(ls_structure) = VALUE /dmo/flight( carrier_id    = 'AA'
                                            connection_id = 017
                                            flight_date   = cl_abap_context_info=>get_system_date( ) ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Internal Table
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(lt_itab) = VALUE /dmo/t_flight( ( carrier_id    = 'AA'
                                           connection_id = 017
                                           flight_date   = cl_abap_context_info=>get_system_date( ) )
                                         ( carrier_id    = 'LH'
                                           connection_id = 017
                                           flight_date   = cl_abap_context_info=>get_system_date( ) ) ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Local Object
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(lo_http_destination) = cl_http_destination_provider=>create_by_url( 'www.odata_srv' ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Method Importing Parameter
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    me->param_demo(
      IMPORTING
        ev_variable = DATA(lv_variable) "*** CAN NOT BE DONW WITH FUNCTION MODULE ***"
    ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Application of inline structure or table using VALUE operator
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    DATA(lt_flight_db) = VALUE /dmo/t_flight( ).

    APPEND LINES OF VALUE /dmo/t_flight( ( carrier_id    = 'AA'
                                           connection_id = 017
                                           flight_date   = cl_abap_context_info=>get_system_date( )  )
                                         ( carrier_id    = 'LH'
                                           connection_id = 017
                                           flight_date   = cl_abap_context_info=>get_system_date( ) ) ) TO lt_flight_db.

    APPEND VALUE /dmo/flight( carrier_id    = 'AA'
                              connection_id = 017
                              flight_date   = cl_abap_context_info=>get_system_date( ) ) TO lt_flight_db.

    APPEND VALUE #( carrier_id    = 'AA'
                    connection_id = 017
                    flight_date   = cl_abap_context_info=>get_system_date( ) ) TO lt_flight_db.

    DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_http_destination ) .
    DATA(lo_http_request) = lo_http_client->get_http_request( ).

    lo_http_request->set_header_fields( VALUE #( ( name = 'Content-Type' value = 'application/json' )
                                                 ( name = 'Accept'       value = 'application/json' )
                                                 ( name = 'APIKey'       value = '----'             ) ) ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Immutable Variables
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    FINAL(lc_user_id) = cl_abap_context_info=>get_user_technical_name( ).

*    lc_user_id = 'NEWID'.  "Syntax Error

    "Use Case - Declare ConstanTs using Inline Declaration
    SELECT SINGLE
      FROM ZVKSR_Airline
    FIELDS AirlineID,
           AirlineName
    WHERE AirlineID EQ 'AA'
      INTO ( @FINAL(lc_AirlineID), @FINAL(lc_AirlineName) ).
    IF sy-subrc IS NOT INITIAL.
      CLEAR lt_flight.
    ELSE.
      out->write( |lc_AirlineID: { lc_AirlineID }| ).
      out->write( |lc_AirlineName: { lc_AirlineName }| ).
    ENDIF.

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    me->main( out ).
  ENDMETHOD.


  METHOD param_demo.

  ENDMETHOD.
ENDCLASS.

CLASS zvks_cl_osql_select DEFINITION
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
    CONSTANTS gc_carrid_lh TYPE string VALUE 'LH' ##NO_TEXT.
    CONSTANTS gc_carrid_sq TYPE string VALUE 'SQ' ##NO_TEXT.

    METHODS sql_select_new_syntax
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_literals
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_joins
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_union
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_itab
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_subqueries
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_case
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_cast
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_bltin_numeric_fn
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_bltin_string_fn
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_bltin_coalesce_fn
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_bltin_date_fn
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_bltin_time_fn
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_bltin_utcl_fn
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_bltin_tstmpl_fn
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_bltin_timezone_fn
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_bltin_fn_with_where
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_aggr_and_group_by
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_grouping_sets
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_cds_path
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_cds_param
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_curr_conv
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_hierachies
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_offset
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_window_exp
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS sql_select_nulls_last
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

ENDCLASS.



CLASS ZVKS_CL_OSQL_SELECT IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    me->main( out ).
  ENDMETHOD.


  METHOD main.

*    me->sql_select_new_syntax( out ).
*    me->sql_select_literals( out ).
*    me->sql_select_joins( out ).
*    me->sql_select_union( out ).
*    me->sql_select_itab( out ).
*    me->sql_select_subqueries( out ).
*    me->sql_select_case( out ).
*    me->sql_select_cast( out ).
*    me->sql_select_bltin_numeric_fn( out ).
*    me->sql_select_bltin_string_fn( out ).
*    me->sql_select_bltin_coalesce_fn( out ).
*    me->sql_select_bltin_date_fn( out ).
*    me->sql_select_bltin_time_fn( out ).
*    me->sql_select_bltin_utcl_fn( out ).
*    me->sql_select_bltin_tstmpl_fn( out ).
*    me->sql_select_bltin_timezone_fn( out ).
*    me->sql_select_bltin_fn_with_where( out ).
*    me->sql_select_aggr_and_group_by( out ).

    "*** WIP ***
*    me->sql_select_grouping_sets( out ).
*    me->sql_select_cds_path( out ).
*    me->sql_select_cds_param( out ).
*    me->sql_select_curr_conv( out ).
*    METHODS sql_select_hierachies.
*    METHODS sql_select_offset.
*    METHODS sql_select_window_exp.
*    METHODS sql_select_nulls_last.
*    https://blogs.sap.com/2023/02/17/special-abap-sql-expression-null/

  ENDMETHOD.


  METHOD sql_select_bltin_fn_with_where.

    "Built-In Functions
    SELECT
      FROM ZVKSR_Airline AS airline
    FIELDS airline~AirlineID,
           airline~AirlineName
    WHERE upper( concat( airline~AirlineID, airline~AirlineName ) ) LIKE '%BRITISH%'
          "like_regexpr( pcre  = @regex,
          "              value = text,
          "              case_sensitive = ' ' ) = 1
    INTO TABLE @DATA(lt_airline).
    IF sy-subrc IS NOT INITIAL.
      CLEAR lt_airline.
    ELSE.
      out->write( 'Where with upper.' ).
      out->write( lt_airline ).
    ENDIF.

    SELECT
      FROM ZVKSR_FlightDetails AS flight
    FIELDS flight~AirlineID,
           flight~ConnectionID,
           flight~SeatsMax,
           flight~SeatsOccupied
     WHERE ( flight~SeatsMax - flight~SeatsOccupied ) < 10
     "*** Use SQL Console to correct: AND abs( flight~SeatsMax ) < 200 ***"
     AND abs( flight~SeatsMax ) = 10
    INTO TABLE @DATA(lt_flight).
    IF sy-subrc IS NOT INITIAL.
      CLEAR lt_flight.
    ELSE.
      out->write( 'Where with expression.' ).
      out->write( lt_flight ).
    ENDIF.

  ENDMETHOD.


  METHOD sql_select_bltin_date_fn.

    "Reference: https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/abensql_date_func.htm

    INSERT zvkst_demo_exp FROM @( VALUE #( id    = 'X'
                                           dats1 = sy-datum
                                           dats2 = sy-datum + 100 ) ).

    SELECT SINGLE
      FROM zvkst_demo_exp
    FIELDS dats1,
           dats2,
           dats_is_valid(     dats1        ) AS dats_is_valid,
           extract_year(      dats1        ) AS extract_year,
           extract_month(     dats1        ) AS extract_month,
           extract_day(       dats1        ) AS extract_day,
           dayname(           dats1        ) AS dayname,
           monthname(         dats1        ) AS monthname,
           weekday(           dats1        ) AS weekday,
           days_between(      dats1,dats2 )  AS days_between,
           add_days(          dats1,100 )    AS add_days,
           add_months(        dats1,-1 )     AS add_months,
           "Conversion
           dats_to_datn(      dats = dats1 ) AS dats_to_datn
     WHERE id EQ @abap_true
     INTO @DATA(ls_dats_fn).
    IF sy-subrc IS NOT INITIAL.
      CLEAR ls_dats_fn.
    ELSE.
      out->write( 'Built-in Date Functions' ).
      out->write( ls_dats_fn ).
    ENDIF.

    DELETE FROM zvkst_demo_exp.

    INSERT zvkst_demo_exp FROM @( VALUE #( id    = 'X'
                                           datn1 = sy-datum
                                           datn2 = sy-datum + 100 ) ).

    SELECT SINGLE
      FROM zvkst_demo_exp
    FIELDS datn1,
           datn2,
           "There is no datn_is_valid function.
           "Though we have is_valid( ) but it is generic.
           extract_year(      datn1        )                                      AS extract_year,
           extract_month(     datn1        )                                      AS extract_month,
           extract_day(       datn1        )                                      AS extract_day,
           dayname(           datn1        )                                      AS dayname,
           monthname(         datn1        )                                      AS monthname,
           weekday(           datn1        )                                      AS weekday,
           days_between(      datn1,datn2  )                                      AS days_between,
           add_days(          datn1,100    )                                      AS add_days,
           add_months(        datn1,-1     )                                      AS add_months
           "Conversion
*           dats_from_datn( datn    = datn1,
*                           on_null = @sql_dats_from_datn=>c_on_null-set_to_null ) AS dats_from_datn
     WHERE id EQ @abap_true
     INTO @DATA(ls_datn_fn).
    IF sy-subrc IS NOT INITIAL.
      CLEAR ls_datn_fn.
    ELSE.
      out->write( 'Built-in Date Functions' ).
      out->write( ls_datn_fn ).
    ENDIF.

    DELETE FROM zvkst_demo_exp.

  ENDMETHOD.


  METHOD sql_select_nulls_last.
    "Nulls Last
*    INSERT demo_sflight_agg FROM TABLE @( VALUE #(
*      ( carrid = 'AA' connid = '1354'
*        seatsocc = '20' )
*      ( carrid = 'AA' connid = '0407' fldate = '17012019'
*        seatsocc = '20' )
*      ( carrid = 'AA' connid = '0407' fldate = '20012019'
*        seatsocc = '0' )
*      ( carrid = 'AA' connid = '0408' fldate = '16022019'
*        seatsocc = '0' )
*      ( carrid = 'AZ' connid = '0017' fldate = '16022019'
*        seatsocc = '10' )
*      ( carrid = 'AZ' connid = '0064' fldate = '16032019'
*        seatsocc = '0' )
*      ( carrid = 'AL' connid = '1984' fldate = '20012020'
*        seatsocc = '10' )
*      ( carrid = 'AL' connid = '1984' fldate = '25012020'
*        seatsocc = '11' )
*      ( carrid = 'AL' connid = '1984' fldate = '17042019'
*        seatsocc = '7' )
*      ( carrid = 'AL' connid = '1984' fldate = '01052019'
*        seatsocc = '20' )
*      ( carrid = 'AH' connid = '0400' fldate = '16072019'
*        seatsocc = '0' ) ) ).
*
*    SELECT FROM demo_sflight_agg AS a
*             LEFT OUTER JOIN demo_sflight_agg AS b
*               ON a~carrid = b~carrid AND
*                  a~connid = b~connid AND
*                  a~fldate = b~fldate AND
*                  a~connid LIKE '04%'
*           FIELDS a~carrid,
*                  a~fldate AS a_fldate,
*                  a~connid,
*                  a~seatsocc,
*                  b~carrid AS b_carrid,
*                  b~fldate AS b_fldate,
*                  b~connid AS b_connid,
*                  b~seatsocc AS b_seatsocc
*           WHERE a~carrid LIKE 'A%' AND
*                 a~fldate LIKE '%' AND
*                 a~connid LIKE '0%'
*           ORDER BY b~carrid NULLS LAST
*           INTO TABLE @DATA(result).
*
*    DELETE FROM demo_sflight_agg.
  ENDMETHOD.


  METHOD sql_select_bltin_time_fn.

    "Reference: https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/abensql_time_func.htm

    INSERT zvkst_demo_exp FROM @( VALUE #( id    = 'X'
                                           tims1 = sy-timlo ) ).

    SELECT SINGLE
      FROM zvkst_demo_exp
    FIELDS tims1 AS time1,
           tims_is_valid(  tims1 ) AS tims_is_valid,
           extract_hour(   tims1 ) AS extract_hour,
           extract_minute( tims1 ) AS extract_minute,
           extract_second( tims1 ) AS extract_second
           "Conversion
*           tims_to_timn( tims     = tims1,
*                         on_error = @sql_tims_to_timn=>c_on_error-set_to_null ) AS tims_to_timn
     WHERE id EQ @abap_true
    INTO @DATA(ls_tims_fn).
    IF sy-subrc IS NOT INITIAL.
      CLEAR ls_tims_fn.
    ELSE.
      out->write( 'Built-in Time Functions' ).
      out->write( ls_tims_fn ).
    ENDIF.

    DELETE FROM zvkst_demo_exp.

    INSERT zvkst_demo_exp FROM @( VALUE #( id    = 'X'
                                           timn1 = sy-timlo ) ).

    SELECT SINGLE
      FROM zvkst_demo_exp
    FIELDS timn1,
           "There is no timn_is_valid function.
           "Though we have is_valid( ) but it is generic.
           extract_hour(   timn1 ) AS extract_hour,
           extract_minute( timn1 ) AS extract_minute,
           extract_second( timn1 ) AS extract_second
*           tims_from_timn( timn    = timn1,
*                           on_null = @sql_tims_from_timn=>c_on_null-set_to_null ) AS tims_from_timn
     WHERE id EQ @abap_true
    INTO @DATA(ls_timn_fn).
    IF sy-subrc IS NOT INITIAL.
      CLEAR ls_timn_fn.
    ELSE.
      out->write( 'Built-in Time Functions' ).
      out->write( ls_timn_fn ).
    ENDIF.

    DELETE FROM zvkst_demo_exp.

  ENDMETHOD.


  METHOD sql_select_cds_param.

*    SELECT *
*           FROM demo_cds_param_view_entity(
*             p_distance_l = @from_distance,
*             p_distance_u = @to_distance,
*             p_unit       = @unit )
*           ORDER BY carrid, connid
*           INTO TABLE @DATA(result).
*    cl_demo_output=>display( result ).
  ENDMETHOD.


  METHOD sql_select_cds_path.

    "Path expressions in ABAP SQL
* SELECT scarr~carrname,
*           \_spfli[ (*) ]-connid AS connid,
*           \_spfli[ (*) ]\_sflight[ (*) ]-fldate AS fldate,
*           \_spfli[ (*) ]\_sairport-name AS name
*           FROM demo_cds_assoc_scarr AS scarr
*           WHERE scarr~carrid = @carrid
*           ORDER BY carrname, connid, fldate
*           INTO TABLE @DATA(result1).
*
* SELECT DISTINCT carrname
*       FROM demo_cds_assoc_sairport_tz( tz = @( to_upper( tz ) ) )          "Upper
*            \_spfli
*            \_scarr[ currcode = @( CONV s_currcode( to_upper( currc ) ) ) ] "Upper
*            AS scarr
*       ORDER BY carrname
*       INTO TABLE @DATA(result_new).

  ENDMETHOD.


  METHOD sql_select_itab.

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
     WHERE ( flight~AirlineID EQ @me->gc_carrid_aa OR                 "AA
             flight~AirlineID EQ @me->gc_carrid_lh )                  "LH
      INTO TABLE @DATA(lt_flight).
    IF sy-subrc IS NOT INITIAL.
      CLEAR lt_flight.
    ELSE.
      out->write( 'Select from Database Table' ).
      out->write( lt_flight ).
    ENDIF.

    "SELECT form Internal Table LT_FLIGHT
    SELECT
      FROM @lt_flight AS flight                                       "Internal Table
    FIELDS flight~AirlineID,
           flight~ConnectionID,
           flight~FlightDate,
           flight~SeatPrice,
           flight~CurrencyCode,
           flight~PlaneTypeID,
           flight~SeatsMax,
           flight~SeatsOccupied
     WHERE ( flight~AirlineID EQ @me->gc_carrid_aa )                  "AA
      INTO TABLE @DATA(lt_flight_lh).
    IF sy-subrc IS NOT INITIAL.
      CLEAR lt_flight_lh.
    ELSE.
      out->write( 'Select from Internal Table' ).
      out->write( lt_flight_lh ).
    ENDIF.

    "JOIN to Internal Table LT_FLIGHT
    SELECT
      FROM ZVKSR_ConnectingRoutes AS routes                           "Database Table
      LEFT OUTER JOIN @lt_flight AS flight                            "Internal Table
                   ON flight~AirlineID    EQ routes~AirlineID
                  AND flight~ConnectionID EQ routes~ConnectionID
    FIELDS routes~AirlineID,
           routes~ConnectionID,
           routes~AirportFromID,
           routes~AirportToID,
           routes~DepartureTime,
           routes~ArrivalTime,
           flight~FlightDate,
           flight~SeatPrice,
           flight~CurrencyCode
     WHERE ( routes~AirlineID EQ @me->gc_carrid_aa OR                 "AA
             routes~AirlineID EQ @me->gc_carrid_sq )                  "SQ
      INTO TABLE @DATA(lt_routes).
    IF sy-subrc IS NOT INITIAL.
      CLEAR lt_routes.
    ELSE.
      out->write( 'Left Outer Join to  Internal Table' ).
      out->write( lt_routes ).
    ENDIF.

    "SELECT and JOIN on internal table - TO INTERNAL TABLES IN ONE SELECT IS NOT ALLOWED.
*     SELECT
*       FROM @lt_routes AS routes                                       "Internal Table
*       LEFT OUTER JOIN @lt_flight AS flight                            "Internal Table
*                    ON flight~AirlineID    EQ routes~AirlineID
*                   AND flight~ConnectionID EQ routes~ConnectionID
*     FIELDS routes~*,
*            flight~*
*       INTO TABLE @DATA(lt_table).
*     IF sy-subrc IS NOT INITIAL.
*       CLEAR lt_routes.
*     ELSE.
*       out->write( lt_table ).
*     ENDIF.

    "UNION on Internal Table
    "JOIN to Internal Table LT_FLIGHT
    SELECT
      FROM ZVKSR_ConnectingRoutes AS routes
    FIELDS routes~AirlineID,
           routes~ConnectionID,
           routes~AirportFromID,
           routes~AirportToID,
           routes~DepartureTime,
           routes~ArrivalTime
     WHERE routes~AirlineID EQ @me->gc_carrid_aa                      "AA
      INTO TABLE @DATA(lt_routes_aa).
    IF sy-subrc IS NOT INITIAL.
      CLEAR lt_routes_aa.
    ELSE.

      SELECT
        FROM ZVKSR_ConnectingRoutes AS routes                         "Database Table
      FIELDS routes~AirlineID,
             routes~ConnectionID,
             routes~AirportFromID,
             routes~AirportToID,
             routes~DepartureTime,
             routes~ArrivalTime
       WHERE routes~AirlineID EQ @me->gc_carrid_lh                    "LH
       UNION
      SELECT
        FROM @lt_routes_aa AS routes                                  "Internal Table
      FIELDS routes~AirlineID,
             routes~ConnectionID,
             routes~AirportFromID,
             routes~AirportToID,
             routes~DepartureTime,
             routes~ArrivalTime
        INTO TABLE @DATA(lt_routes_aa_lh).
      IF sy-subrc IS NOT INITIAL.
        CLEAR lt_routes_aa_lh.
      ELSE.
        out->write( 'Union on Internal Table' ).
        out->write( lt_routes_aa_lh ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD sql_select_grouping_sets.
    "Example for GROUPING SETS
*    demo_select_grouping_sets
*    SELECT FROM sflight
*           FIELDS carrid,
*                  connid,
*                  planetype,
*                  SUM( seatsmax ) AS sum_seatsmax
*                  WHERE carrid = 'LH'
*           GROUP BY GROUPING SETS ( ( carrid, planetype ),
*                                    ( carrid, connid ) )
*           ORDER BY connid, planetype
*           INTO TABLE @DATA(result_grouping_sets).
*
  ENDMETHOD.


  METHOD sql_select_curr_conv.
*    DATA(out) = cl_demo_output=>new( ).
*
*    DATA currency TYPE c LENGTH 5 VALUE 'USD'.
*    cl_demo_input=>request( CHANGING field = currency ).
*    setup( ).
*
*    SELECT *
*           FROM demo_prices
*           ORDER BY id
*           INTO TABLE @DATA(original_prices).
*
*    out->begin_section( `Original Prices`
*      )->write( original_prices ).
*
*    IF cl_dbi_utilities=>is_logging_on( 'DEMO_PRICES' ) IS INITIAL.
*      MODIFY demo_prices FROM
*        ( SELECT FROM demo_prices
*                 FIELDS id,
*                        currency_conversion(
*                          amount = amount,
*                          source_currency = currency,
*                          target_currency = @currency,
*                          exchange_rate_date = @sy-datlo,
*                          on_error =
*                          @sql_currency_conversion=>c_on_error-fail )
*                            AS amount,
*                          @currency  AS currency
*                  ORDER BY id )
*      ##logging_versus_from_select[demo_prices] ##null_values.
*    ELSE.
*      SELECT FROM demo_prices
*             FIELDS  @sy-mandt AS client,
*                     id,
*                     currency_conversion(
*                       amount = amount,
*                       source_currency = currency,
*                       target_currency = @currency,
*                       exchange_rate_date = @sy-datlo,
*                       on_error =
*                       @sql_currency_conversion=>c_on_error-fail )
*                         AS amount,
*                       @currency  AS currency
*             ORDER BY id
*             INTO TABLE @DATA(buffer).
*      MODIFY demo_prices FROM TABLE buffer.
*    ENDIF.
*
*    SELECT *
*           FROM demo_prices
*           ORDER BY id
*           INTO TABLE @DATA(converted_prices).
  ENDMETHOD.


  METHOD sql_select_new_syntax.

    "Unanimously supports all new SELECT syntaxes like:
    "- UNION and UNION ALL
    "- Field Search using Ctrl + Space
    "- JOINS to Internal Table
    "- SELECT FROM Internal Table as the Data Source
    "- etc.

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
      out->write( lt_flight ).
    ENDIF.

  ENDMETHOD.


  METHOD sql_select_aggr_and_group_by.

    "HAVING is used to intend where condition on GROUP BY result

    "Group By
    SELECT
      FROM ZVKSR_FlightDetails AS flight
    FIELDS flight~AirlineID,
           SUM( flight~SeatPrice ) AS NetSeatPrice,
           flight~CurrencyCode
     WHERE ( flight~AirlineID EQ @me->gc_carrid_aa OR                 "AA
             flight~AirlineID EQ @me->gc_carrid_lh )                  "LH
      GROUP BY flight~AirlineID,
               flight~CurrencyCode
      HAVING flight~AirlineID EQ @me->gc_carrid_aa                    "AA
      ORDER BY flight~AirlineID
      INTO TABLE @DATA(lt_flight1).
    IF sy-subrc IS NOT INITIAL.
      CLEAR lt_flight1.
    ELSE.
      out->write( lt_flight1 ).
    ENDIF.

    "Group By on Aggregated Fields
    SELECT
      FROM ZVKSR_FlightDetails AS flight
    FIELDS flight~AirlineID,
           SUM( CAST( flight~SeatPrice AS FLTP ) / CAST( ( flight~SeatsOccupied - flight~SeatPrice ) AS FLTP ) ) AS PricePerSeat,
           flight~CurrencyCode
     WHERE flight~AirlineID EQ @me->gc_carrid_aa                      "AA
      GROUP BY flight~AirlineID,
               flight~CurrencyCode
      INTO TABLE @DATA(lt_flight2).
    IF sy-subrc IS NOT INITIAL.
      CLEAR lt_flight2.
    ELSE.
      out->write( lt_flight2 ).
    ENDIF.

*    SELECT
*      FROM sflight
*    FIELDS sflight~carrid,                            "Airline Code
*           sflight~connid,                            "Flight Connection Number
*           sflight~fldate,                            "Flight date
*           sflight~planetype,                         "Aircraft Type
*           sflight~price,                             "Airfare
*           sflight~currency,                          "Local currency of airline
*           sflight~paymentsum,                        "Total of current bookings
*           sflight~seatsmax,                          "Maximum Capacity in Economy Class
*           sflight~seatsocc                           "Occupied seats in economy class
*     WHERE sflight~carrid EQ 'AA'
*      INTO TABLE @DATA(lt_sflight).
*    IF sy-subrc IS NOT INITIAL.
*      CLEAR lt_sflight.
*    ELSE.
*      out->write( lt_sflight ).
*    ENDIF.

*    DATA carrid TYPE sflight-carrid VALUE 'AA'.
*    cl_demo_input=>request( CHANGING field = carrid ).
*
*    INSERT demo_sflight_agg FROM (
*    SELECT carrid,
*           connid,
*           CAST( '00000000' AS DATS ) AS fldate,
*           SUM( seatsocc ) AS seatsocc
*           FROM sflight
*           WHERE carrid = @( to_upper( carrid ) )
*           GROUP BY carrid, connid ).
*
*    SELECT ' ' AS mark, carrid, connid, fldate, seatsocc
*           FROM sflight
*           WHERE carrid = @( to_upper( carrid ) )
*           UNION SELECT 'X' AS mark,
*                        carrid, connid, fldate, seatsocc
*                        FROM demo_sflight_agg
*           ORDER BY carrid, connid, mark, fldate, seatsocc
*           INTO TABLE @DATA(result).
*
*    DELETE FROM demo_sflight_agg.
*    cl_demo_output=>display( result ).
*
*    SELECT char1, char2, num1, num2, num1 + num2 AS sum,
*                                     num1 * num2 AS product
*           FROM demo_expressions
*           ORDER BY char1, char2
*           INTO TABLE @DATA(ungrouped).
*    out->write( ungrouped ).
*
*    SELECT char1 && '_' && char2 AS group,
*           MAX( num1 + num2 ) AS max,
*           MIN( num1 + num2 ) AS min,
*           MIN( num1 * num2 ) AS min_product,
*           MEDIAN( num1 * num2 ) AS median_product
*           FROM demo_expressions
*           GROUP BY char1, char2
*           ORDER BY group
*           INTO TABLE @DATA(grouped).
*    out->write( grouped ).
*
*    SELECT char1 && '_' && char2 AS group,
*           MAX( num1 + num2 ) AS max,
*           MEDIAN( num1 + num2 ) AS median
*           FROM demo_expressions
*           GROUP BY char1, char2
*           HAVING MIN( num1 * num2 ) > 25
*           ORDER BY group
*           INTO TABLE @DATA(grouped_having).
*
*    DELETE FROM demo_expressions.
*    INSERT demo_expressions
*           FROM TABLE @(
*                VALUE #( LET r = cl_abap_random_int=>create(
*                                 seed = CONV i( sy-uzeit )
*                                 min = 1 max = 10 ) IN
*                         FOR i = 1 UNTIL i > 10
*                           ( id   = CONV #( i )
*                             num1 = r->get_next( ) ) ) ).
*
*    SELECT
*      FROM demo_expressions
*      FIELDS
*        AVG(          num1                ) AS avg_no_type,
*        AVG( DISTINCT num1                ) AS avg_no_type_distinct,
*        AVG(          num1 AS DEC( 10,0 ) ) AS avg_dec0,
*        AVG( DISTINCT num1 AS DEC( 10,0 ) ) AS avg_dec0_distinct,
*        AVG(          num1 AS DEC( 14,4 ) ) AS avg_dec4,
*        AVG( DISTINCT num1 AS DEC( 14,4 ) ) AS avg_dec4_distinct
*      INTO @DATA(result).

  ENDMETHOD.


  METHOD sql_select_bltin_coalesce_fn.

* !!! Doesn't work !!!

* Benefit - Code push down of procedural logic LOOP

*    DELETE FROM demo_expressions.
*    INSERT demo_expressions FROM TABLE @( VALUE #( ( id = '1' dats1 = sy-datum )
*                                                   ( id = '2' dats2 = sy-datum )
*                                                   ( id = '3' ) ) ).
*
*    SELECT
*     FROM demo_expressions
*    FIELDS id,
*           dats1,
*           dats2,
*           coalesce( dats1, dats2, 'No Date' ) AS dats_coalesce
*    INTO TABLE @DATA(lt_dats).
*    IF sy-subrc IS NOT INITIAL.
*      CLEAR lt_dats.
*    ELSE.
*      out->write( lt_dats ).
*    ENDIF.
*
*    DELETE FROM demo_expressions.
*    INSERT demo_expressions FROM TABLE @( VALUE #( ( id = '1' char1 = 'Good Day' )
*                                                   ( id = '2' char2 = 'Guten Tag' )
*                                                   ( id = '3' ) ) ).
*
*    SELECT
*      FROM demo_expressions
*     FIELDS id,
*            char1,
*            char2,
*            coalesce( char1, char2, 'Namaskar' ) AS char_coalesce
*     INTO TABLE @DATA(lt_char).
*    IF sy-subrc IS NOT INITIAL.
*      CLEAR lt_char.
*    ELSE.
*      out->write( lt_char ).
*    ENDIF.
*
*    DATA lv_1 TYPE numc10 VALUE '1'.
*    DATA lv_2 TYPE numc10 VALUE '2'.
*
*    DELETE FROM demo_expressions.
*    INSERT demo_expressions FROM TABLE @( VALUE #( ( id = '1' numc1 = |{ lv_1 ALPHA = IN }| )
*                                                   ( id = '2' numc2 = |{ lv_2 ALPHA = IN }| )
*                                                   ( id = '3' ) ) ).
*
*    SELECT
*      FROM demo_expressions
*     FIELDS id,
*            numc1,
*            numc2,
*            coalesce( numc1, numc2, '99' ) AS numc_coalesce
*     INTO TABLE @DATA(lt_numc).
*    IF sy-subrc IS NOT INITIAL.
*      CLEAR lt_numc.
*    ELSE.
*      out->write( lt_numc ).
*    ENDIF.

  ENDMETHOD.


  METHOD sql_select_bltin_numeric_fn.

    "Reference: https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-us/abensql_arith_func.htm

* Benefit - Code push down of procedural logic LOOP
* Buffer
*  - ABS      - negative and positive integer to positive integer
*  - DIVISION - divide with decimal output
*  - DIV      - divide with integer output
*  - CEIL     - round off to upper integer
*  - FLOOR    - round off to lower integer
*  - MOD      - remainder
*  - ROUND    - round off to specific number of decimals

    SELECT
      FROM ZVKSR_FlightDetails
    FIELDS AirlineID,
           ConnectionID,
           FlightDate,

           "Dummy Seat Price in Decimals
           division( 12345,SeatsMax, 2 ) AS SeatPrice,
           "Rounding off Seat Price to higher integer - CEIL
           ceil( division( 12345,SeatsMax, 2 ) ) AS SeatPriceCiel,
           "Rounding off Seat Price
           round( division( 12345,SeatsMax, 2 ), 0 ) AS SeatPriceRound,

           "Dummy Flight Time in Minutes
           125 AS FlightTimeMinutes,
           "Converting flight time from minutes to hours - DIVISION
           division( 123,60,2 ) AS FlightTimeHours,
           "Converting flight time from minutes to hours in text - DIV and MOD
           concat_with_space( concat_with_space( CAST( div( 125,60 ) AS CHAR ), 'hr', 1 ),
           concat_with_space( CAST( mod( 125,60 ) AS CHAR ), 'min', 1 ), 1 ) AS FlightTimeHoursText, "125/60 = 2.08

           SeatsMax,
           "Reducing capacity by 60% rounding it off to lower
           "integer as fraction seat cannot be allocated - FLOOR
           floor( ( SeatsMax - SeatsMax * division( 60,100,2 ) ) ) AS SeatsMaxActual,

           "Occupied seats
           SeatsOccupied,
           "Under booked/Over booked by positive of "<Actual Maximum Seats> - <Seats Occupied>" - ABS
           "Just an example, ABS can be omitted by using <Seats Occupied> - <Actual Maximum Seats> in case of over booking.
           CASE
           WHEN floor( ( SeatsMax - ( division( SeatsMax, 2, 2 ) ) ) ) - SeatsOccupied > 0
           THEN concat_with_space( 'Underbooked by', CAST( abs( floor( ( SeatsMax - ( division( SeatsMax, 2, 2 ) ) ) ) - SeatsOccupied ) AS CHAR ), 1 )

           WHEN floor( ( SeatsMax - ( division( SeatsMax, 2, 2 ) ) ) ) - SeatsOccupied < 0
           THEN concat_with_space( 'Overbooked  by', CAST(  abs( floor( ( SeatsMax - ( division( SeatsMax, 2, 2 ) ) ) ) - SeatsOccupied ) AS CHAR ), 1 )

           ELSE 'Fully booked' END AS SeatOccupancyStatus

    WHERE AirlineID EQ @me->gc_carrid_aa
    INTO TABLE @DATA(lt_flight).
    IF sy-subrc IS NOT INITIAL.
      CLEAR lt_flight.
    ELSE.
      out->write( 'Flight Table using Numeric Function' ).
      out->write( lt_flight ).
    ENDIF.

  ENDMETHOD.


  METHOD sql_select_bltin_string_fn.

    "Reference: https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abensql_string_func.htm

    INSERT zvkst_demo_exp FROM TABLE @( VALUE #( ( id    = 'X'
                                                   char1 = ' 0123'
                                                   char2 = 'aAaA' ) ) ).

    SELECT SINGLE
      FROM zvkst_demo_exp
    FIELDS char1 AS text1,
           char2 AS text2,
           concat(              char1,char2 )     AS concat,
           concat_with_space(   char1,char2, 1 )  AS concat_with_space,
           instr(               char1,'23' )      AS instr,               "
           left(                char1,3 )         AS left,                "N characters from left
           length(              char1 )           AS length,              "String length
           lower(               char2 )           AS lower,               "Small casing
           upper(               char2 )           AS upper,               "Capital casing
           lpad(                char1,10,'x' )    AS lpad,                "ALPHA In like conversion but can pad character of choice
           ltrim(               char1,' ' )       AS ltrim,               "ALPHA Out like conversion but can trim character of choice
           rpad(                char1,10,'x' )    AS rpad,                "LPAD from Right
           rtrim(               char1,'3' )       AS rtrim,               "LTRIM from Right
           replace(             char1,'12','__' ) AS replace,             "REPLACE ALL OCCURRENCES OF <> in <> WITH <>.
           right(               char1,3 )         AS right,               "
           substring(           char1,3,3 )       AS substring,           "
           "Regular Expression Functions
           like_regexpr(        pcre = '\CA',                             "Found = 1, Not Found = 0
                                value = char2 )   AS like_regex,
           occurrences_regexpr( pcre = '\CA',"
                                value = char2 )   AS occ_regex,           "No. of matches
           replace_regexpr(     pcre = '\CA',
                                value = char2,
                                with = 'bb' )     AS replace_regex        "Replace
      INTO @DATA(ls_string_fn).
    IF sy-subrc IS NOT INITIAL.
      CLEAR ls_string_fn.
    ELSE.
      out->write( 'Built-in String Functions' ).
      out->write( ls_string_fn ).
    ENDIF.

    DELETE FROM zvkst_demo_exp.

  ENDMETHOD.


  METHOD sql_select_bltin_timezone_fn.

    "Reference: https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/abensql_timezone_func.htm

*    DELETE FROM zvkst_demo_exp.
*    INSERT zvkst_demo_exp FROM @( VALUE #( id = 'X' ) ).

*    SELECT SINGLE
*     FROM zvkst_demo_exp
*   FIELDS abap_system_timezone( client   = @sy-mandt,
*                                on_error = @sql_abap_system_timezone=>set_to_null ) AS system_tz,
*          abap_user_timezone( user     = @sy-uname,
*                              client   = @sy-mandt,
*                              on_error = @sql_abap_user_timezone=>set_to_null ) AS user_tz
*        WHERE id EQ @abap_true
*           INTO @DATA(ls_timezone_fn).
*    IF sy-subrc IS NOT INITIAL.
*      CLEAR ls_timezone_fn.
*    ELSE.
*      out->write( 'Timezone Functions' ).
*      out->write( ls_timezone_fn ).
*    ENDIF.

    DELETE FROM zvkst_demo_exp.

  ENDMETHOD.


  METHOD sql_select_bltin_tstmpl_fn.

    "*** USE UTC LONG TIMESTAMP INSTEAD ***
    "*** Use tstmpl_to_utcl for the conversion ***

    "Reference: https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/abensql_date_time_conversions.htm
    "Reference: https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/abensql_timestamp_func.htm

    GET TIME STAMP FIELD DATA(lv_tmpstl).

    INSERT zvkst_demo_exp FROM @( VALUE #( id          = 'X'
                                           dats1       = sy-datum
                                           tims1       = sy-uzeit
                                           timestampl1 = lv_tmpstl
                                           timestampl2 = cl_abap_tstmp=>add( tstmp = lv_tmpstl
                                                                             secs  = 10 * 24 * 3600 + 10 * 3600 + 10 * 60 + 10 )
                                           utcl1       = utclong_current( ) ) ).

*    SELECT SINGLE
*           FROM zvkst_demo_exp
*           FIELDS
*             tstmp_current_utctimestamp( )                                                   AS tstmp,
*
*             tstmp_is_valid( timestamp1 )                                                    AS valid,
*
*             tstmp_to_dats( tstmp    = tstmp_current_utctimestamp( ),
*                            tzone    = abap_system_timezone( ),
*                            client   = @sy-mandt,
*                            on_error = @sql_tstmp_to_dats=>set_to_null )                     AS dats,
*
*             tstmp_to_tims( tstmp    = tstmp_current_utctimestamp( ),
*                            tzone    = abap_system_timezone( ),
*                            client   = @sy-mandt,
*                            on_error = @sql_tstmp_to_tims=>set_to_null )                     AS tims,
*
*             tstmp_to_dst( tstmp    = tstmp_current_utctimestamp( ),
*                           tzone    = abap_system_timezone( ),
*                           client   = @sy-mandt,
*                           on_error = @sql_tstmp_to_dst=>set_to_null )                       AS dst,
*
*             dats_tims_to_tstmp( date     = dats1,
*                                 time     = tims1,
*                                 tzone    = abap_system_timezone( ),
*                                 client   = @sy-mandt,
*                                 on_error = @sql_dats_tims_to_tstmp=>set_to_null )           AS dats_tims_to_tstmp,
*
*             tstmpl_to_utcl( tstmpl     = timestampl1,
*                             on_error   = @sql_tstmpl_to_utcl=>c_on_error-set_to_null,
*                             on_initial = @sql_tstmpl_to_utcl=>c_on_initial-set_to_initial ) AS utcl,
*
*             tstmpl_from_utcl( utcl = utcl1,
*                               on_null = @sql_tstmpl_from_utcl=>c_on_null-set_to_null )      AS from_utcl,
*
*             tstmp_add_seconds( tstmp    = timestamp1,
*                                seconds  = CAST( num1 AS DEC( 15,0 ) ),
*                                on_error = @sql_tstmp_add_seconds=>fail )                    AS tstmp_add_seconds,
*
*             tstmp_seconds_between( tstmp1 = timestampl1,
*                                    tstmp2 = timestampl2,
*                                    on_error = @sql_tstmp_seconds_between=>fail )            AS tstmp_seconds_between
*     WHERE id EQ @abap_true
*     INTO @DATA(ls_tstmplong_fn).
*    IF sy-subrc IS NOT INITIAL.
*      CLEAR ls_tstmplong_fn.
*    ELSE.
*      out->write( 'Long Timestamp Functions' ).
*      out->write( ls_tstmplong_fn ).
*    ENDIF.

    DELETE FROM zvkst_demo_exp.

    "Old: Using Time Stamp
    DATA tstl TYPE timestampl. "type p, length 11, decimals 7

    GET TIME STAMP FIELD tstl.

    tstl =  cl_abap_tstmp=>add( tstmp = tstl
                                secs  = 10 * 24 * 3600 + 10 * 3600 + 10 * 60 + 10 ).

    CONVERT TIME STAMP tstl
            TIME ZONE 'CET'
            INTO DATE DATA(dat)
                 TIME DATA(tim)
                 DAYLIGHT SAVING TIME DATA(dst).

    DATA(tstl_string) = |Date: { dat DATE = ISO }, Time: { tim TIME = ISO }, DST: { dst }|.

    "New: Using UTC Time Stamp
    DATA(utcl) = utclong_current( ).

    utcl = utclong_add( val     = utcl
                        days    = 10
                        hours   = 10
                        minutes = 10
                        seconds = 10 ).

    CONVERT UTCLONG utcl
            INTO DATE dat
                 TIME tim
                 FRACTIONAL SECONDS DATA(fs)
                 DAYLIGHT SAVING TIME dst
                 TIME ZONE 'CET'.

    DATA(utcl_string) = |Date: { dat DATE = ISO }, Time: { tim TIME = ISO }.{ fs }, DST: { dst }|.

  ENDMETHOD.


  METHOD sql_select_bltin_utcl_fn.

    "Both Date and Time Functions are applicable for UTC Long Timestamp.
    INSERT zvkst_demo_exp FROM @( VALUE #( id    = 'X'
                                           utcl1 = utclong_current( )
                                           utcl2 = utclong_add( val     = utclong_current( )
                                                                days    = 10
                                                                hours   = 10
                                                                minutes = 10
                                                                seconds = 10 ) ) ).

    SELECT SINGLE
      FROM zvkst_demo_exp
    FIELDS utcl1,
           utcl2,
           "There is no utcl_is_valid function.
           "Though we have is_valid( ) but it is generic.
           is_valid(             utcl1        ) AS is_valid,
           "Date Functions
           extract_year(         utcl1        ) AS extract_year,
           extract_month(        utcl1        ) AS extract_month,
           extract_day(          utcl1        ) AS extract_day,
           dayname(              utcl1        ) AS dayname,
           monthname(            utcl1        ) AS monthname,
           weekday(              utcl1        ) AS weekday,
           days_between(         utcl1,utcl2  ) AS days_between,
           add_days(             utcl1,100    ) AS add_days,
           add_months(           utcl1,-1     ) AS add_months,
           "Time Functions
           extract_hour(         utcl1        ) AS extract_hour,
           extract_minute(       utcl1        ) AS extract_minute,
           extract_second(       utcl1        ) AS extract_second,
           "UTCL Functions
           utcl_current( )                      AS utcl_current,
           utcl_seconds_between( utcl1, utcl2 ) AS diff,
           utcl_add_seconds(     utcl1, 10    ) AS add_seconds
     WHERE id EQ @abap_true
     INTO @DATA(ls_utclong_fn).
    IF sy-subrc IS NOT INITIAL.
      CLEAR ls_utclong_fn.
    ELSE.
      out->write( 'UTC Long Timestamp Functions' ).
      out->write( ls_utclong_fn ).
    ENDIF.

    DELETE FROM zvkst_demo_exp.

  ENDMETHOD.


  METHOD sql_select_case.

    DATA:
    lv_american_airlines TYPE c LENGTH 20.

    lv_american_airlines = TEXT-001.                                  "American Airlines

* Benefit - Code push down of procedural logic LOOP.

    SELECT
      FROM ZVKSR_FlightDetails
    FIELDS AirlineID,                                                 "Airline Code
* CASE Expression - Type 1
           CASE AirlineID
           WHEN 'AA' THEN @lv_american_airlines                       "Local language dependent text variable
           WHEN 'AC' THEN 'Air Canada'
           WHEN 'AF' THEN 'Air France'
           WHEN 'AZ' THEN 'Alitalia'
           WHEN 'BA' THEN 'British Airways'
           WHEN 'FJ' THEN 'Air Pacific'
           WHEN 'CO' THEN 'Continental Airlines'
           WHEN 'DL' THEN 'Delta Airlines'
           WHEN 'AB' THEN 'Air Berlin'
           WHEN 'LH' THEN 'Lufthansa'
           WHEN 'NG' THEN 'Lauda Air'
           WHEN 'JL' THEN 'Japan Airlines'
           WHEN 'NW' THEN 'Northwest Airlines'
           WHEN 'QF' THEN 'Qantas Airways'
           WHEN 'SA' THEN 'South African Air.'
           WHEN 'SQ' THEN 'Singapore Airlines'
           WHEN 'SR' THEN 'Swiss'
           WHEN 'UA' THEN 'United Airlines'
           ELSE 'Unknown'
           END AS airline_name,
           ConnectionID,
           FlightDate,
           SeatPrice,
           CurrencyCode,
           PlaneTypeID,
           SeatsMax,
           SeatsOccupied,
* CASE Expression - Type 2
           CASE
           WHEN SeatsMax - SeatsOccupied = 0 THEN @abap_false         "Local Constant
           ELSE @abap_true                                            "Local Constant
           END AS seats_avail                                         "Economy Class Seats Available
      WHERE AirlineID EQ @me->gc_carrid_aa
      INTO TABLE @DATA(lt_sflight).
    IF sy-subrc IS NOT INITIAL.
      CLEAR lt_sflight.
    ELSE.
      out->write( 'Flight Table using CASE' ).
      out->write( lt_sflight ).
    ENDIF.

  ENDMETHOD.


  METHOD sql_select_cast.

    "Reference: https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abensql_cast.htm

    "Benefit - Code push down of procedural logic loop

    DATA(timestamp) = cl_abap_tstmp=>utclong2tstmp_short( utclong_current( ) ).

    INSERT zvkst_demo_exp FROM @( VALUE #( id         = 'X'
                                           num1       = 111
                                           numlong1   = '123456789'
                                           dec3       = '123.456'
                                           dats2      = sy-datum
                                           timestamp1 = timestamp ) ).

    SELECT SINGLE
     FROM zvkst_demo_exp
   FIELDS timestamp1 AS time_stamp,
* Casting INT4 to CHAR
          CAST( num1     AS CHAR( 20 ) )                                AS num1_CHAR,
* Casting INT8 to CHAR
          CAST( numlong1 AS CHAR( 20 ) )                                AS numlong1_CHAR,
* Casting DEC to CHAR
          CAST( dec3     AS CHAR( 20 ) )                                AS dec3_CHAR,
* Casting DATS to CHAR
          CAST( dats2    AS CHAR( 20 ) )                                AS dats2_CHAR,
* Casting DEC to CHAR to DATS
          CAST( CAST( div( timestamp1 , 1000000 ) AS CHAR ) AS DATS )   AS date,
* Casting DEC to CHAR to TIMS
          CAST( substring( CAST( timestamp1 AS CHAR ), 9, 6 ) AS TIMS ) AS time
    WHERE id EQ @abap_true
     INTO @DATA(ls_work_area).
    IF sy-subrc IS NOT INITIAL.
      CLEAR ls_work_area.
    ELSE.
      out->write( 'Structure using CAST' ).
      out->write( ls_work_area ).
    ENDIF.

    "Delete or else, GTT will cause dump
    DELETE FROM zvkst_demo_exp.

  ENDMETHOD.


  METHOD sql_select_hierachies.
    "Report demo_sql_hierachies
    "Report demo_old_vs_new_sql_hierarchy
  ENDMETHOD.


  METHOD sql_select_joins.

    "INNER JOIN and LEFT OUTER JOIN replaces FOR ALL ENTRIES

    "INNER JOIN
    SELECT
      FROM ZVKSR_FlightDetails AS flight
      INNER JOIN ZVKSR_ConnectingRoutes AS routes
              ON routes~AirlineID EQ flight~AirlineID
             AND routes~ConnectionID EQ flight~ConnectionID
    FIELDS flight~AirlineID,
           flight~ConnectionID,
           flight~FlightDate,
           flight~SeatPrice,
           flight~CurrencyCode,
           routes~AirportFromID,
           routes~AirportToID,
           routes~DepartureTime,
           routes~ArrivalTime
     WHERE flight~AirlineID EQ @me->gc_carrid_aa                      "AA
      INTO TABLE @DATA(lt_flight).
    IF sy-subrc IS NOT INITIAL.
      CLEAR lt_flight.
    ELSE.
      out->write( lt_flight ).
    ENDIF.

    "LEFT OUTER JOIN
    SELECT
      FROM ZVKSR_ConnectingRoutes AS routes
      LEFT OUTER JOIN ZVKSR_FlightDetails AS flight
                   ON flight~AirlineID    EQ routes~AirlineID
                  AND flight~ConnectionID EQ routes~ConnectionID
    FIELDS flight~AirlineID,
           flight~ConnectionID,
           flight~FlightDate,
           flight~SeatPrice,
           flight~CurrencyCode,
           routes~AirportFromID,
           routes~AirportToID,
           routes~DepartureTime,
           routes~ArrivalTime
      WHERE routes~AirlineID EQ @me->gc_carrid_aa                      "AA
      INTO TABLE @DATA(lt_routes).
    IF sy-subrc IS NOT INITIAL.
      CLEAR lt_routes.
    ELSE.
      out->write( lt_routes ).
    ENDIF.

    "LEFT OUTER JOIN WITH NULL: Get values from left table, only if does not exist in the right table

    "DE5200: ZVKS_P_USR_AUTH_ROLE_COMPARE
*    SELECT
*      FROM ZVKSR_ConnectingRoutes AS routes
*      LEFT OUTER JOIN ZVKSR_FlightDetails AS flight
*                   ON flight~AirlineID    EQ routes~AirlineID
*                  AND flight~ConnectionID EQ routes~ConnectionID
*    FIELDS flight~AirlineID,
*           flight~ConnectionID,
*           flight~FlightDate,
*           flight~SeatPrice,
*           flight~CurrencyCode,
*           routes~AirportFromID,
*           routes~AirportToID,
*           routes~DepartureTime,
*           routes~ArrivalTime
*      WHERE routes~AirlineID EQ @me->gc_carrid_aa                      "AA
*        "AND flight~AirlineID IS NULL
*        "AND flight~ConnectionID IS NULL
*      INTO TABLE @DATA(lt_loj_null).
*    IF sy-subrc IS NOT INITIAL.
*      CLEAR lt_loj_null.
*    ELSE.
*      out->write( lt_loj_null ).
*    ENDIF.

    "RIGHT OUTER JOIN (Not used much)
    "CROSS JOIN (DO NOT USE - Performance Expensive)

  ENDMETHOD.


  METHOD sql_select_literals.

    DATA(lv_utcl) = utclong_current( ).

    SELECT SINGLE
      FROM ZVKSR_Airline
    FIELDS 'STRING_LITERAL'              AS string_literal,
           10                            AS numeric_literal,
           @lv_utcl                      AS variable_literal,
           @sy-sysid && ' ' && @sy-mandt AS concatenated_literal,
           AirlineID
     WHERE AirlineID EQ @me->gc_carrid_aa
      INTO @DATA(ls_airline).
    IF sy-subrc IS NOT INITIAL.
      CLEAR ls_airline.
    ELSE.
      out->write( |Airline Structure with Literals| ).
      out->write( ls_airline ).
    ENDIF.

    "Using Literals in SELECT to Create Range
    "IF_FSBP_CONST_RANGE
    CONSTANTS:
      lc_sign_include TYPE ddsign   VALUE 'I',
      lc_option_equal TYPE ddoption VALUE 'EQ'.

    SELECT
      FROM ZVKSR_Airport AS airport
    FIELDS @lc_sign_include  AS sign,                                 "Local Constant
           @lc_option_equal  AS option,                               "Local Constant
           airport~AirportID AS low,
           airport~AirportID AS high
     WHERE airport~CountryCode EQ 'DE'
        INTO TABLE @DATA(lr_airport_ids).
    IF sy-subrc IS NOT INITIAL.
      CLEAR lr_airport_ids.
    ELSE.

      out->write( |Generated Range:| ).
      out->write( lr_airport_ids ).

      SELECT
        FROM ZVKSR_ConnectingRoutes AS routes
      FIELDS routes~AirportFromID,
             COUNT( * ) AS route_count
      WHERE  routes~AirportFromID IN @lr_airport_ids
      GROUP BY routes~AirportFromID
      INTO TABLE @DATA(lt_routes_from_de).
      IF sy-subrc IS NOT INITIAL.
        CLEAR lt_routes_from_de.
      ELSE.
        out->write( 'Total routes form Germany Airports' ).
        out->write( lt_routes_from_de ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD sql_select_offset.
    "Offset
    "Reading of the data of all flights of a connection, except for the ten flights with the fewest seats taken.
*
* SELECT fldate
*       FROM sflight
*       WHERE carrid = 'LH' AND connid = '400'
*       ORDER BY seatsocc ASCENDING, fldate
*       INTO TABLE @DATA(result)
*       OFFSET 10.
  ENDMETHOD.


  METHOD sql_select_subqueries.

    "If the result set of the subquery defined by the clause subquery_clauses contains more than one row,
    " ALL: SubQuery is true for all rows in the result set.
    " ANY /SOME:  SubQuery is true for at least one row in the result set.

    "Considerations:
    " - First Subquery is executed > Parent Query
    " - WHERE Clause is executed on SELECT
    " - HAVING clause is executed on the Aggregated Result Set

    SELECT
      FROM ZVKSR_ConnectingRoutes AS routes
    FIELDS routes~AirportFromID,
           COUNT( * ) AS route_count
    WHERE  routes~AirportFromID EQ ANY ( SELECT
                                           FROM ZVKSR_Airport AS airport
                                         FIELDS airport~AirportID
                                          WHERE airport~CountryCode EQ 'DE' )
  GROUP BY routes~AirportFromID
*    HAVING routes~AirportFromID EQ ANY ( SELECT
*                                           FROM ZVKSR_Airport AS airport
*                                         FIELDS airport~AirportID
*                                          WHERE airport~CountryCode EQ 'DE' )
    INTO TABLE @DATA(lt_routes_from_de).
    IF sy-subrc IS NOT INITIAL.
      CLEAR lt_routes_from_de.
    ELSE.
      out->write( 'Total routes form Germany Airports' ).
      out->write( lt_routes_from_de ).
    ENDIF.

  ENDMETHOD.


  METHOD sql_select_union.

    "Replaces APPENDING <CORRESPONDING> TABLE OF

    "UNION: Duplicate records are merged.
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
     WHERE ( flight~AirlineID EQ @me->gc_carrid_aa OR                 "AA
             flight~AirlineID EQ @me->gc_carrid_lh )                  "LH
    UNION
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
     WHERE ( flight~AirlineID EQ @me->gc_carrid_aa OR                 "AA
             flight~AirlineID EQ @me->gc_carrid_sq )                  "SQ
      ORDER BY AirlineID
      INTO TABLE @DATA(lt_union).
    IF sy-subrc IS NOT INITIAL.
      CLEAR lt_union.
    ELSE.
      out->write( 'Union' ).
      out->write( lt_union ).
    ENDIF.

    "UNION ALL
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
     WHERE ( flight~AirlineID EQ @me->gc_carrid_aa OR                 "AA
             flight~AirlineID EQ @me->gc_carrid_lh )                  "LH
    UNION ALL
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
     WHERE ( flight~AirlineID EQ @me->gc_carrid_aa OR                 "AA
             flight~AirlineID EQ @me->gc_carrid_sq )                  "SQ
      ORDER BY AirlineID
      INTO TABLE @DATA(lt_union_all).
    IF sy-subrc IS NOT INITIAL.
      CLEAR lt_union_all.
    ELSE.
      out->write( 'Union All' ).
      out->write( lt_union_all ).
    ENDIF.

  ENDMETHOD.


  METHOD sql_select_window_exp.
    "Window Expression
* demo_select_over
* DEMO_SELECT_OVER_WIN_FRAME_SPE
* demo_select_over_order_by
* demo_select_over_ntile
* demo_select_over_lead_lag
* demo_select_over_lead_lag_diff
* demo_select_over_group
* demo_select_over_all

  ENDMETHOD.
ENDCLASS.

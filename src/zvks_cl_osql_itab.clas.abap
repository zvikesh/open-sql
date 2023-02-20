CLASS zvks_cl_osql_itab DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF gty_airline,
        AirlineID   TYPE /dmo/carrier_id,
        AirlineName TYPE /dmo/carrier_name,
      END OF gty_airline,
      gtt_airline TYPE STANDARD TABLE OF gty_airline WITH DEFAULT KEY,

      BEGIN OF gty_conn_routes,
        AirlineID     TYPE /dmo/carrier_id,
        ConnectionID  TYPE /dmo/connection_id,
        AirportFromID TYPE /dmo/airport_from_id,
        AirportToID   TYPE /dmo/airport_to_id,
      END OF gty_conn_routes,
      gtt_conn_routes TYPE STANDARD TABLE OF gty_conn_routes  WITH DEFAULT KEY,

      BEGIN OF gty_flight,
        AirlineID    TYPE /dmo/carrier_id,
        ConnectionID TYPE /dmo/connection_id,
        FlightDate   TYPE /dmo/flight_date,
        SeatPrice    TYPE /dmo/flight_price,
        CurrencyCode TYPE /dmo/currency_code,
      END OF gty_flight,
      gtt_flight TYPE STANDARD TABLE OF gty_flight  WITH DEFAULT KEY.

    INTERFACES if_oo_adt_classrun.

    METHODS main
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS get_flight_data
      EXPORTING
        et_airline     TYPE gtt_airline
        et_conn_routes TYPE gtt_conn_routes
        et_flight      TYPE gtt_flight.

    METHODS assignment
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS loop_at_groupby
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS meshes
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS secondary_sorted_key
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS table_exprression
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS base
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS move_corresponding
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS corresponding
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS cl_abap_corresponding
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS for
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS filter
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS reduce
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS reduce_type_inference
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS reduce_with_method
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS reduce_with_cond
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS reduce_with_for
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS cte
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS gtt
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS cond
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS switch
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS conv
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS let
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS summary
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

ENDCLASS.



CLASS zvks_cl_osql_itab IMPLEMENTATION.


  METHOD reduce.

**     Old
*
*    DATA sum_old TYPE i.
*    DATA wa LIKE LINE OF itab.
*    LOOP AT itab INTO wa.
*      sum_old = sum_old + wa.
*    ENDLOOP.
*
**     New
*
*    DATA(sum_new) = REDUCE i( INIT x = 0
*                              FOR <wa> IN itab
*                              NEXT x = x + <wa> ).


*    DATA itab TYPE TABLE OF i WITH EMPTY KEY.
*    itab = VALUE #( FOR j = 1 WHILE j <= 10 ( j ) ).
*    cl_demo_output=>write( itab ).
*
*    DATA(sum) = REDUCE i( INIT x = 0 FOR wa IN itab NEXT x += wa ).

  ENDMETHOD.


  METHOD loop_at_groupby.

    me->get_flight_data(
      IMPORTING
        et_flight = DATA(lt_flight) ).

    LOOP AT lt_flight ASSIGNING FIELD-SYMBOL(<lfs_flight_dummy>)
                                GROUP BY ( airlineid   = <lfs_flight_dummy>-airlineid
                                           group_size  = GROUP SIZE
                                           group_index = GROUP INDEX )
                      ASCENDING
                      ASSIGNING FIELD-SYMBOL(<lfs_flight_group>).

      out->write( <lfs_flight_group>-airlineid ).
      out->write( |Group Index: { <lfs_flight_group>-group_index }| ).
      out->write( |Group Size: { <lfs_flight_group>-group_size }| ).

      LOOP AT GROUP <lfs_flight_group> ASSIGNING FIELD-SYMBOL(<lfs_flight>).
        <lfs_flight>-seatprice += <lfs_flight>-seatprice.
      ENDLOOP.

      out->write( |{ <lfs_flight>-connectionid } { <lfs_flight>-flightdate COUNTRY = 'IN ' } | &&
                  |{ <lfs_flight>-seatprice COUNTRY = 'IN ' } { <lfs_flight>-currencycode }| ).

      UNASSIGN <lfs_flight>.

      out->write( cl_abap_char_utilities=>newline ).
    ENDLOOP.

  ENDMETHOD.


  METHOD cl_abap_corresponding.

    TYPES:
      BEGIN OF lty_flight_source,
        airlineid    TYPE /dmo/carrier_id,
        connectionid TYPE /dmo/connection_id,
        flightdate   TYPE /dmo/flight_date,
        seatprice    TYPE /dmo/flight_price,
        currencycode TYPE /dmo/currency_code,
      END OF lty_flight_source,
      ltt_flight_source TYPE STANDARD TABLE OF lty_flight_source.

    TYPES:
      BEGIN OF lty_flight_target,
        airlineid     TYPE /dmo/carrier_id,
        connectionid  TYPE /dmo/connection_id,
        flight_date   TYPE /dmo/flight_date,
        price         TYPE /dmo/flight_price,
        currency_code TYPE /dmo/currency_code,
      END OF lty_flight_target,
      ltt_flight_target TYPE STANDARD TABLE OF lty_flight_target.

    DATA:
      ls_flight_source TYPE lty_flight_source,
      ls_flight_target TYPE lty_flight_target,
      lt_flight_source TYPE ltt_flight_source,
      lt_flight_target TYPE ltt_flight_target.

    me->get_flight_data(
      IMPORTING
        et_flight      = lt_flight_source ).

    "Create Mapper Object using Structure
    FINAL(lo_mapper_str) = cl_abap_corresponding=>create(
                           source      = ls_flight_source
                           destination = ls_flight_target
                           mapping     = VALUE cl_abap_corresponding=>mapping_table( LET lc_0 = 0 IN
                                         ( level = lc_0 kind = cl_abap_corresponding=>mapping_component srcname = ls_flight_source-flightdate   dstname = ls_flight_target-flight_date   )
                                         ( level = lc_0 kind = cl_abap_corresponding=>mapping_component srcname = ls_flight_source-seatprice    dstname = ls_flight_target-price         )
                                         ( level = lc_0 kind = cl_abap_corresponding=>mapping_component srcname = ls_flight_source-currencycode dstname = ls_flight_target-currency_code ) ) ).

    "Move Corresponding using Mapper object
    CLEAR lt_flight_target.
    lo_mapper_str->execute( EXPORTING source      = lt_flight_source
                            CHANGING  destination = lt_flight_target ).

    "Create Mapper Object using Table
    FINAL(lo_mapper_tab) = cl_abap_corresponding=>create(
                           source      = lt_flight_source
                           destination = lt_flight_target
                           mapping     = VALUE cl_abap_corresponding=>mapping_table( LET lc_0 = 0 IN
                                         ( level = lc_0 kind = cl_abap_corresponding=>mapping_component srcname = `flightdate`   dstname = `flight_date`   )
                                         ( level = lc_0 kind = cl_abap_corresponding=>mapping_component srcname = `seatprice`    dstname = `price`         )
                                         ( level = lc_0 kind = cl_abap_corresponding=>mapping_component srcname = `currencycode` dstname = `currency_code` ) ) ).

    "Move Corresponding using Mapper object
    CLEAR lt_flight_target.
    lo_mapper_tab->execute( EXPORTING source      = lt_flight_source
                            CHANGING  destination = lt_flight_target ).

    "Deep Structures
    "demo_corresponding_class_str

    "Using Look Up Table
    "demo_corresponding_class_lkup

  ENDMETHOD.


  METHOD assignment.

    DATA(lv_abcde) = |ABCDEFGHIJKLMNOPQRSTUVWXYZ|.

    DATA(sum_new)  = 0.
    DATA(text_new) = ` `.

    DO 10 TIMES.
      sum_new  += 1.                            "sum_old  = sum_old  + 1.
      text_new &&= lv_abcde+sy-index(1).        "text_old = text_old && lv_abcde+sy-index(1).
    ENDDO.

    out->write( |{ sum_new }\n{ text_new }| ).

  ENDMETHOD.


  METHOD main.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Looping internal table
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "me->assignment( out ).
    "me->loop_at_groupby( out ).
    "me->meshes( out ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Secondary Sorted Key with Internal Table
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "me->secondary_sorted_key( out ).
    "me->table_exprression( out ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Copying data from one internal table to another internal table
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "me->base( out ).
    "me->move_corresponding( out ).
    "me->corresponding( out ).
    "me->cl_abap_corresponding( out ).
    "me->for( out ).
    me->filter( out ).
    me->reduce( out ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Other Operators
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "me->secondary_sorted_key( out ).
    "me->table_exprression( out ).

    "me->conv( out ).
    "me->cond( out ).
    "me->switch( out ).
    "me->conv( out )

    "lines   Row function
    "line_index  Index function

    "Random Integer generator
*      DATA(rnd) = cl_abap_random_int=>create(
*               seed = CONV i( sy-uzeit )
*               min = 1
*               max = 10 ).
*
*    DATA(lv_integer) = rnd->get_next( ).
*
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Summary Notes
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    me->summary( out ).

  ENDMETHOD.


  METHOD base.

    me->get_flight_data(
    IMPORTING
      et_flight      = DATA(lt_flight) ).

    DATA lt_flight_final TYPE zvks_cl_osql_itab=>gtt_flight.

    lt_flight_final = VALUE #( ( airlineid    = `ZZ`
                                 connectionid = 123
                                 flightdate   = cl_abap_context_info=>get_system_date( )
                                 seatprice    = 1234
                                 currencycode = `INR` ) ).

    lt_flight_final = VALUE #( BASE lt_flight ( airlineid    = `ZZ`
                                                connectionid = 123
                                                flightdate   = cl_abap_context_info=>get_system_date( )
                                                seatprice    = 1234
                                                currencycode = `INR` ) ).

    out->write( lt_flight_final ).

  ENDMETHOD.


  METHOD switch.

  ENDMETHOD.


  METHOD get_flight_data.

    SELECT
      FROM ZVKSR_Airline
    FIELDS AirlineID,
           AirlineName
     WHERE ( AirlineID EQ `AA` OR
             AirlineID EQ `LH` OR
             AirlineID EQ `SQ` )
      INTO TABLE @et_airline.
    IF sy-subrc IS NOT INITIAL.
      CLEAR et_airline.
    ENDIF.

    SELECT
      FROM ZVKSR_ConnectingRoutes
    FIELDS AirlineID,
           ConnectionID,
           AirportFromID,
           AirportToID
     WHERE ( AirlineID EQ `AA` OR
             AirlineID EQ `LH` OR
             AirlineID EQ `SQ` )
      INTO TABLE @et_conn_routes.
    IF sy-subrc IS NOT INITIAL.
      CLEAR et_conn_routes.
    ENDIF.

    SELECT
      FROM ZVKSR_FlightDetails
    FIELDS AirlineID,
           ConnectionID,
           FlightDate,
           SeatPrice,
           CurrencyCode
         WHERE ( AirlineID EQ `AA` OR
                 AirlineID EQ `LH` OR
                 AirlineID EQ `SQ` )
          INTO TABLE @et_flight.
    IF sy-subrc IS NOT INITIAL.
      CLEAR et_flight.
    ENDIF.

  ENDMETHOD.


  METHOD summary.

    "----------------------------------------------------------------"
    " TABLE EXPRESSIONS                                              "
    "----------------------------------------------------------------"
    "Replaces all:
    " - Read Table with BINARY SEARCH.
    " - Read Table with INDEX.
    " - Work Areas.
    " - Field Symbols.

    "----------------------------------------------------------------"
    " FOR                                                            "
    "----------------------------------------------------------------"
    " Copy all records of one table to another table with WHERE
    " condition and Built-In Functions.

    "----------------------------------------------------------------"
    " CORRESPONDING                                                  "
    "----------------------------------------------------------------"
    " Copy all records of one table to another table with MAPPING.

    "----------------------------------------------------------------"
    " CL_ABAP_CORRESPONDING                                          "
    "----------------------------------------------------------------"
    " Copy all records of one table to another table with MAPPING
    " using "single" MAPPER at all places.

    "----------------------------------------------------------------"
    " FOR + CORRESPONDING                                            "
    "----------------------------------------------------------------"
    " Copy all records of one table to another table with WHERE
    " condition and MAPPING.
    " !!! Built-In Functions cannot be used !!!

    "----------------------------------------------------------------"
    " LOOP                                                           "
    "----------------------------------------------------------------"
    " First, try if the complex ABAP logic or subroutines can be
    " wrapped inside class method with single return value.
    " Or else,
    " Use LOOP AT internal table.

    "----------------------------------------------------------------"
    " FILTER                                                         "
    "----------------------------------------------------------------"

    "----------------------------------------------------------------"
    " FOR + FILTER                                                   "
    "----------------------------------------------------------------"

    "----------------------------------------------------------------"
    " REDUCE                                                         "
    "----------------------------------------------------------------"

    "Performance based selection
    "*** Reading Table ****
    "1. Table Expression

    "*** Looping Table ****
    " FOR + CORRESPONDING
    " FOR (Built In Fns + Class Method)
    " CORRESPONDING / CL_ABAP_CORRESPONDING
    " LOOP

    "*** Looping and Reading Table ****
    " FILTER (Built In Fns + Class Method)
    " FOR + FILTER
    " LOOP AT + READ TABLE

"for Aggregation

    "SELECT
    "Use Union instead of appending lines into table.
    "Use INNER JOIN or LEFT OUTER JOIN on internal table instead of FOR
    " ALL ENTRIES.
    "


  ENDMETHOD.


  METHOD cte.

*    WITH
*      +cities AS ( SELECT cityfrom AS city
*                     FROM spfli
*                    WHERE carrid = 'AA'
*
*                    UNION DISTINCT
*
*                   SELECT cityto AS city
*                     FROM spfli
*                    WHERE carrid = 'AA' )
*
*      SELECT *
*             FROM sgeocity
*             WHERE city IN ( SELECT city FROM +cities )
*             ORDER BY city
*             INTO TABLE @DATA(result_new).

*    DATA cities TYPE SORTED TABLE OF sgeocity-city
*            WITH NON-UNIQUE KEY table_line.
*    SELECT cityfrom
*           FROM spfli
*           INTO TABLE cities
*           WHERE carrid = carrid.
*    SELECT cityto
*           FROM spfli
*           APPENDING TABLE cities
*           WHERE carrid = carrid.
*    DELETE ADJACENT DUPLICATES FROM cities.
*
*    IF cities IS NOT INITIAL.
*      SELECT *
*             FROM sgeocity
*             INTO TABLE result_old
*             FOR ALL ENTRIES IN cities
*             WHERE city = cities-table_line.
*      SORT result_old BY city.
*    ENDIF.


  ENDMETHOD.


  METHOD conv.
    "https://www.youtube.com/watch?v=YfK-2_a19bI&list=PLqz8SLrkjv2hWihbrGi4fkI8pNtVed6Zt&index=6
  ENDMETHOD.


  METHOD gtt.

**     Old
*
*    DATA itab TYPE TABLE OF demo_sumdist_agg WITH EMPTY KEY.
*    SELECT s~mandt s~carrname p~distid SUM( p~distance ) AS sum_distance
*           FROM scarr AS s
*             INNER JOIN spfli AS p ON s~carrid = p~carrid
*      INTO TABLE itab
*      GROUP BY s~mandt s~carrname p~distid.
*    INSERT demo_sumdist_agg FROM TABLE @itab.
*
*    SELECT *
*           FROM demo_sumdist_agg
*           ORDER BY carrname, distid, sum_distance
*           INTO TABLE @DATA(result_old).
*
*    DELETE FROM demo_sumdist_agg.
*
*
**     New
*
*    INSERT demo_sumdist_agg FROM
*      ( SELECT
*          FROM scarr AS s
*            INNER JOIN spfli AS p ON s~carrid = p~carrid
*          FIELDS s~carrname,
*                 p~distid,
*                 SUM( p~distance ) AS sum_distance
*          GROUP BY s~mandt, s~carrname, p~distid ).
*
*    SELECT *
*           FROM demo_sumdist_agg
*           ORDER BY carrname, distid, sum_distance
*           INTO TABLE @DATA(result_new).
*
*    DELETE FROM demo_sumdist_agg. "<--- Exception if GTT is not empty at end of program
  ENDMETHOD.


  METHOD for.

    TYPES:
      BEGIN OF lty_flight,
        airlineid     TYPE /dmo/carrier_id,
        connectionid  TYPE /dmo/connection_id,
        flightdate    TYPE /dmo/flight_date,
        price         TYPE /dmo/flight_price,
        currency_code TYPE /dmo/currency_code,
        message       TYPE string,
      END OF lty_flight,
      ltt_flight TYPE STANDARD TABLE OF lty_flight WITH DEFAULT KEY.

    me->get_flight_data(
      IMPORTING
        et_airline     = DATA(lt_airline)
        et_conn_routes = DATA(lt_conn_routes)
        et_flight      = DATA(lt_flight)
    ).

    "LOOP...WHERE...ENDLOOP.
    DATA(lt_flight_aa) = VALUE ltt_flight( FOR ls_flight               "into
                                            IN lt_flight               "loop at
                                         WHERE ( airlineid    = `AA` )
                                               ( airlineid     = ls_flight-airlineid
                                                 connectionid  = ls_flight-connectionid
                                                 flightdate    = ls_flight-flightdate
                                                 price         = ls_flight-seatprice
                                                 currency_code = ls_flight-currencycode
                                                 message       = |Object Key: { ls_flight-airlineid }{ ls_flight-connectionid }{ ls_flight-flightdate }| ) ).

    "LOOP...WHERE...ENDLOOP with Constant
    "FOR with Built-In Functions
    lt_flight_aa = VALUE ltt_flight( LET lc_aa = `AA` lc_object_key = `Object Key:` IN
                                     FOR ls_flight
                                      IN lt_flight
                                   WHERE ( airlineid    = lc_aa )
                                         ( airlineid     = ls_flight-airlineid
                                           connectionid  = ls_flight-connectionid
                                           flightdate    = ls_flight-flightdate
                                           price         = ls_flight-seatprice
                                           currency_code = ls_flight-currencycode
                                           message       = |{ lc_object_key } { ls_flight-airlineid }{ ls_flight-connectionid }{ ls_flight-flightdate }| ) ).

    "FOR + CORRESPONDING
    lt_flight_aa = VALUE ltt_flight( LET lc_aa = `AA` lc_object_key = `Object Key:` IN
                                     FOR ls_flight
                                      IN lt_flight
                                   WHERE ( airlineid    = lc_aa )
                                         ( CORRESPONDING #( ls_flight
                                                            MAPPING price = seatprice
                                                             EXCEPT AirlineID ) )
                                         "( message = |{ lc_object_key } { ls_flight-airlineid }{ ls_flight-connectionid }{ ls_flight-flightdate }| )
                                          ).

  ENDMETHOD.


  METHOD reduce_type_inference.

*    DATA txt TYPE c LENGTH 20.
*    DATA num TYPE i.
*
*    demo=>meth1( p = REDUCE #( INIT r1 = txt
*                               FOR i = 1
*                               UNTIL i > 9
*                               NEXT r1 &&= 'x' ) ).
*    demo=>meth1( p = REDUCE #( INIT r2 = num
*                               FOR i = 1
*                               UNTIL i > 9
*                               NEXT r2 += 1 ) ).
*    cl_demo_output=>line( ).
*
*    demo=>meth2( p = REDUCE #( INIT r1 = txt
*                               FOR i = 1
*                               UNTIL i > 9
*                               NEXT r1 &&= 'x' ) ).
*    "demo=>meth2( p = REDUCE #( INIT r2 = num
*    "                           FOR i = 1
*    "                           UNTIL i > 9
*    "                           NEXT r2 +=+ 1 ) ). "not possible
*    cl_demo_output=>line( ).
*
*    demo=>meth3( p = REDUCE #( INIT r1 = txt
*                               FOR i = 1
*                               UNTIL i > 9
*                               NEXT r1 &&= 'x' ) ).
*    demo=>meth3( p = REDUCE #( INIT r2 = num
*                               FOR i = 1
*                               UNTIL i > 9
*                               NEXT r2 += 1 ) ) ##type.

  ENDMETHOD.


  METHOD cond.

    "Add FOR, FILTER and REDUCE is possible

* Old
    DATA html_old TYPE string.
    DATA greeting TYPE string.
    IF sy-langu = 'D'.
      greeting = `Hallo Welt!`.
    ELSE.
      greeting = `Hello World!`.
    ENDIF.
    CONCATENATE `<html>`
                `<body>`
                greeting
                `</body>`
                `</html>` INTO html_old.

* New
    DATA(html_new) =
         `<html>`
      && `<body>`
      && COND #( WHEN sy-langu = 'D' THEN `Hallo Welt!`
                                     ELSE `Hello World!` )
      && `</body>`
      && `</html>`.
  ENDMETHOD.


  METHOD reduce_with_for.

*    DATA(result) = REDUCE result(
*                     INIT res = VALUE result( max  = 0
*                                              text = `Result: ` )
*                          sep  = ``
*                     FOR <wa> IN itab
*                     NEXT res-text &&= sep && <wa>-id
*                          res-sum += <wa>-num
*                          res-max = nmax( val1 = res-max
*                                          val2 = <wa>-num )
*                          sep     = `-` ).
*
*    cl_demo_output=>display( result ).

*    TYPES:
*      BEGIN OF line,
*        col1 TYPE i,
*        col2 TYPE i,
*      END OF line,
*      BEGIN OF line1,
*        col1 TYPE i,
*        col2 TYPE STANDARD TABLE OF line WITH EMPTY KEY,
*      END OF line1,
*      itab1 TYPE STANDARD TABLE OF line1 WITH EMPTY KEY,
*      BEGIN OF line2,
*        col1 TYPE i,
*        col2 TYPE i,
*        col3 TYPE i,
*      END OF line2.
*
*    DATA(out) = cl_demo_output=>new( ).
*
*    DATA(itab1) = VALUE itab1(
*      ( col1 = 1 col2 = VALUE line1-col2( ( col1 = 111 col2 = 112 )
*                                          ( col1 = 121 col2 = 122 ) ) )
*      ( col1 = 2 col2 = VALUE line1-col2( ( col1 = 211 col2 = 212 )
*                                          ( col1 = 221 col2 = 222 ) ) )
*      ( col1 = 3 col2 = VALUE line1-col2( ( col1 = 311 col2 = 312 )
*                                          ( col1 = 321 col2 = 322 ) ) )
*                             ).
*    LOOP AT itab1 INTO DATA(line1).
*      out->write( name = |ITAB1, Line { sy-tabix }, COL2|
*                  data = line1-col2 ).
*    ENDLOOP.
*
*    DATA(result) = REDUCE string(
*      INIT text TYPE string
*      FOR wa1 IN itab1
*      FOR wa2 IN wa1-col2
*      NEXT text =
*             |{ text } { wa1-col1 }, { wa2-col1 }, { wa2-col2 }\n| ).
*    out->write_html( `<b>Result</b>`
*      )->write(  result  ).

  ENDMETHOD.


  METHOD reduce_with_method.

*    DATA wa       TYPE scarr.
*    DATA text     TYPE string.
*    DATA carriers TYPE TABLE OF scarr.
*
*    SELECT * FROM scarr INTO TABLE @carriers.
*
*    DATA out TYPE REF TO if_demo_output.
*    out = REDUCE #(
*            INIT o = cl_demo_output=>new( )
*            FOR wa IN carriers
*            NEXT o = o->write( to_string( wa ) ) ).
*    out->display( ).
*
*    DO.
*      ASSIGN COMPONENT sy-index OF STRUCTURE wa TO FIELD-SYMBOL(<wa>).
*      IF sy-subrc <> 0.
*        EXIT.
*      ENDIF.
*      DESCRIBE FIELD <wa> OUTPUT-LENGTH DATA(olen).
*      text = |{ text }{ CONV string( <wa> ) WIDTH = olen + 2 }|.
*    ENDDO.

  ENDMETHOD.


  METHOD secondary_sorted_key.

    "Sorted table with Secondary Sorted Key
    TYPES:
      ltt_sorted_tab  TYPE SORTED TABLE OF me->gty_conn_routes
                      WITH UNIQUE KEY AirlineID
                                      ConnectionID
                      WITH NON-UNIQUE SORTED KEY K_AirlineFrom_AirportTo
                                      COMPONENTS AirportFromId
                                                 AirportToId.
    "Standard table with Secondary Sorted Key
    TYPES:
      ltt_conn_routes TYPE STANDARD TABLE OF me->gty_conn_routes WITH DEFAULT KEY
                      WITH NON-UNIQUE SORTED KEY K_AirportFrom_AirportTo
                                      COMPONENTS AirportFromId
                                                 AirportToId.

    DATA lt_conn_routes_ssk TYPE ltt_conn_routes.

    me->get_flight_data(
      IMPORTING
        et_conn_routes = DATA(lt_conn_routes) ).

    lt_conn_routes_ssk = lt_conn_routes.

    READ TABLE lt_conn_routes_ssk ASSIGNING FIELD-SYMBOL(<lfs_conn_routes>)
                                  WITH TABLE KEY K_AirportFrom_AirportTo
                                      COMPONENTS airportfromid = `JFK`
                                                 airporttoid   = `SFO`.
    IF sy-subrc = 0.
      "Logic here
    ENDIF.

  ENDMETHOD.


  METHOD reduce_with_cond.

*    DATA(out) = cl_demo_output=>new(
*      )->next_section( 'Summation'
*      )->write( REDUCE i( INIT sum = 0
*                          FOR n = 1 UNTIL n > 10
*                          NEXT sum = sum + n )
*      )->next_section( 'Concatenation without THEN'
*      )->write( REDUCE string( INIT text = `Count up:`
*                               FOR n = 1 UNTIL n > 10
*                               NEXT text &&= | { n }| )
*      )->next_section( 'Concatenation with THEN'
*      )->write( REDUCE string( INIT text = `Count down:`
*                               FOR n = 10 THEN n - 1 WHILE n > 0
*                               NEXT text &&= | { n }| )
*      )->next_section( 'Non arithmetic expression'
*      )->write( REDUCE string( INIT text = ``
*                               FOR t = `x` THEN t && `y`
*                                           UNTIL strlen( t ) > 10
*                               NEXT text &&= |{ t } | )
*      )->display( ).

  ENDMETHOD.


  METHOD table_exprression.

    me->get_flight_data(
      IMPORTING
        et_airline     = DATA(lt_airline)
        et_conn_routes = DATA(lt_conn_routes)
        et_flight      = DATA(lt_flight) ).

    "!!! DUMP !!!
    "DATA(ls_airline1) = lt_airline[ airlineid = `ZZ` ].

    "Avoid table expression dump using Exception Class
    TRY.
        DATA(ls_airline) = lt_airline[ airlineid = `ZZ` ].

      CATCH cx_sy_itab_line_not_found INTO DATA(lo_line_not_found).
        out->write( lo_line_not_found->get_text( ) ).
    ENDTRY.

    "Avoid table expression dump using OPTIONAL keyword
    ls_airline = VALUE #( lt_airline[ airlineid = `ZZ` ] OPTIONAL ).

    "Avoid table expression dump using LINES_EXITS() operator
    "(READ TABLE... IF SY-SUBRC = 0. ... ENDIF.)
    IF line_exists( lt_airline[ airlineid = `ZZ` ] ).
      ls_airline = lt_airline[ airlineid = `ZZ` ].
    ENDIF.

    "Avoid table expression dump using lines() operator
    IF lines( lt_airline ) GE 1.
      ls_airline = lt_airline[ 1 ].
      "DATA(lv_airline_lines) = lines( lt_airline ).
    ENDIF.

    "Read Structure Table Expression with Secondary Sorted Key
    TYPES:
      ltt_conn_routes TYPE STANDARD TABLE OF me->gty_conn_routes WITH DEFAULT KEY
                      WITH NON-UNIQUE SORTED KEY K_AirportFrom_AirportTo
                                      COMPONENTS AirportFromId
                                                 AirportToId.

    DATA lt_conn_routes_ssk TYPE ltt_conn_routes.

    lt_conn_routes_ssk = lt_conn_routes.

    IF line_exists( lt_conn_routes_ssk[ KEY K_AirportFrom_AirportTo COMPONENTS airportfromid = `JFK` airporttoid = `SFO` ] ).

      "Reading structure
      DATA(ls_conn_routes) =  lt_conn_routes_ssk[ KEY K_AirportFrom_AirportTo COMPONENTS airportfromid = `JFK` airporttoid = `SFO` ].

      "Avoid helper structure and pass directly to method or FM
      out->write( lt_conn_routes_ssk[ KEY K_AirportFrom_AirportTo COMPONENTS airportfromid = `JFK` airporttoid = `SFO` ] ).
    ENDIF.

    "Read Field from Table Expression using Secondary Sorted Key
    IF line_exists( lt_conn_routes_ssk[ KEY K_AirportFrom_AirportTo COMPONENTS airportfromid = `JFK` airporttoid = `SFO` ] ).

      "Reading AirlineID field
      DATA(ls_AirlineID) = lt_conn_routes_ssk[ KEY K_AirportFrom_AirportTo COMPONENTS airportfromid = `JFK` airporttoid = `SFO` ]-AirlineID.

      "Using the variable in SELECT
      CLEAR lt_conn_routes.
      SELECT
        FROM ZVKSR_ConnectingRoutes
      FIELDS AirlineID,
             ConnectionID,
             AirportFromID,
             AirportToID
       WHERE ( AirlineID EQ @ls_AirlineID )
        INTO TABLE @lt_conn_routes.
      IF sy-subrc IS NOT INITIAL.
        CLEAR lt_conn_routes.
      ENDIF.
    ENDIF.

    "Assigning to Fields Symbols
    IF line_exists( lt_flight[ airlineid = `AA` connectionid = 0322 flightdate = `20230924` ] ).

      ASSIGN lt_flight[ airlineid = `AA` connectionid = 0322 flightdate = `20230924` ]-seatprice TO FIELD-SYMBOL(<lfs_seat_price>).
      "Using Assignments
      <lfs_seat_price> /= 100.  "<lfs_seat_price> = <lfs_seat_price> / 100.
    ENDIF.

    "Avoiding Fields Symbols using Table Expression
    IF line_exists( lt_flight[ airlineid = `AA` connectionid = 0322 flightdate = `20230924` ] ).
      lt_flight[ airlineid = `AA` connectionid = 0322 flightdate = `20230924` ]-seatprice /= 100.
    ENDIF.

  ENDMETHOD.


  METHOD move_corresponding.

    TYPES:
      BEGIN OF lty_flight_source,
        airlineid    TYPE /dmo/carrier_id,
        connectionid TYPE /dmo/connection_id,
        flightdate   TYPE /dmo/flight_date,
        seatprice    TYPE /dmo/flight_price,
        currencycode TYPE /dmo/currency_code,
      END OF lty_flight_source,
      ltt_flight_source TYPE STANDARD TABLE OF lty_flight_source.

    TYPES:
      BEGIN OF lty_flight_target,
        airlineid     TYPE /dmo/carrier_id,
        connectionid  TYPE /dmo/connection_id,
        flight_date   TYPE /dmo/flight_date,
        price         TYPE /dmo/flight_price,
        currency_code TYPE /dmo/currency_code,
      END OF lty_flight_target,
      ltt_flight_target TYPE STANDARD TABLE OF lty_flight_target.

    DATA:
      lt_flight_source TYPE ltt_flight_source,
      lt_flight_target TYPE ltt_flight_target.

    me->get_flight_data(
      IMPORTING
        et_airline     = DATA(lt_airline)
        et_conn_routes = DATA(lt_conn_routes)
        et_flight      = DATA(lt_flight) ).

    MOVE-CORRESPONDING lt_flight_source TO lt_flight_target.

    "[EXPANDING NESTED TABLES]: For nested structures.
    "[KEEPING TARGET LINES]: Appends the record.

  ENDMETHOD.


  METHOD filter.

    TYPES:
      BEGIN OF lty_airline,
        AirlineID   TYPE /dmo/carrier_id,
        AirlineName TYPE /dmo/carrier_name,
      END OF lty_airline,
      ltt_airline TYPE STANDARD TABLE OF lty_airline WITH DEFAULT KEY
                  WITH NON-UNIQUE SORTED KEY k_AirlineID
                                  COMPONENTS AirlineID,

      BEGIN OF lty_flight,
        airlineid     TYPE /dmo/carrier_id,
        connectionid  TYPE /dmo/connection_id,
        flightdate    TYPE /dmo/flight_date,
        price         TYPE /dmo/flight_price,
        currency_code TYPE /dmo/currency_code,
        message       TYPE string,
      END OF lty_flight,
      ltt_flight TYPE STANDARD TABLE OF lty_flight WITH DEFAULT KEY.

    DATA lt_airline TYPE ltt_airline.

    me->get_flight_data(
      IMPORTING
        et_flight      = DATA(lt_flight) ).

    APPEND LINES OF VALUE ltt_airline( ( AirlineID = `AA` AirlineName = `American Airlines Inc.` )
                                       ( AirlineID = `AC` AirlineName = `Air Canada` ) ) TO lt_airline.

    "TARGET and SOURCE with Same Type
    DATA(lt_flight_same_type) = FILTER zvks_cl_osql_itab=>gtt_flight( lt_flight               "LOOP AT...
                                                                      "EXCEPT                 "IF SY-SUBRC <> 0.
                                                                   IN lt_airline              "READ TABLE...WITH...BINARY SEARCH
                                                            USING KEY k_AirlineID
                                                                WHERE airlineid = airlineid ).

    "TARGET and SOURCE with Different Type using FOR + FILTER with Built-In Functions
    DATA(lt_flight_diff_type) = VALUE ltt_flight( LET lc_object_key = `Object Key:` IN
                                                  FOR ls_flight
                                                  IN FILTER zvks_cl_osql_itab=>gtt_flight( lt_flight               "LOOP AT...
                                                                                           "EXCEPT                 "IF SY-SUBRC <> 0.
                                                                                        IN lt_airline              "READ TABLE...WITH...BINARY SEARCH
                                                                                 USING KEY k_AirlineID
                                                                                     WHERE airlineid = airlineid )
                                                 ( airlineid     = ls_flight-airlineid
                                                   connectionid  = ls_flight-connectionid
                                                   flightdate    = ls_flight-flightdate
                                                   price         = ls_flight-seatprice
                                                   currency_code = ls_flight-currencycode
                                                   message       = |{ lc_object_key } { ls_flight-airlineid }{ ls_flight-connectionid }{ ls_flight-flightdate }| ) ).

    "TARGET and SOURCE with Different Type using FOR + FILTER with CORRESPONDING
    lt_flight_diff_type = VALUE ltt_flight( FOR ls_flight
                                            IN FILTER zvks_cl_osql_itab=>gtt_flight( lt_flight               "LOOP AT...
                                                                                     "EXCEPT                 "IF SY-SUBRC <> 0.
                                                                                  IN lt_airline              "READ TABLE...WITH...BINARY SEARCH
                                                                           USING KEY k_AirlineID
                                                                               WHERE airlineid = airlineid )
                                           ( CORRESPONDING #( ls_flight
                                                              MAPPING price = seatprice
                                                                            currency_code = currencycode ) ) ).

    "Avoid the records instead of Overwriting
    APPEND LINES OF VALUE ltt_flight( FOR ls_flight
                                      IN FILTER zvks_cl_osql_itab=>gtt_flight( lt_flight               "LOOP AT...
                                                                               EXCEPT                  "IF SY-SUBRC <> 0.
                                                                            IN lt_airline              "READ TABLE...WITH...BINARY SEARCH
                                                                     USING KEY k_AirlineID
                                                                         WHERE airlineid = airlineid )
                                     ( CORRESPONDING #( ls_flight
                                                        MAPPING price = seatprice
                                                                currency_code = currencycode ) ) ) TO lt_flight_diff_type.

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    me->main( out ).
  ENDMETHOD.


  METHOD corresponding.

    TYPES:
      BEGIN OF lty_flight_source,
        airlineid    TYPE /dmo/carrier_id,
        connectionid TYPE /dmo/connection_id,
        flightdate   TYPE /dmo/flight_date,
        seatprice    TYPE /dmo/flight_price,
        currencycode TYPE /dmo/currency_code,
      END OF lty_flight_source,
      ltt_flight_source TYPE STANDARD TABLE OF lty_flight_source.

    TYPES:
      BEGIN OF lty_flight_target,
        airlineid     TYPE /dmo/carrier_id,
        connectionid  TYPE /dmo/connection_id,
        flight_date   TYPE /dmo/flight_date,
        price         TYPE /dmo/flight_price,
        currency_code TYPE /dmo/currency_code,
      END OF lty_flight_target,
      ltt_flight_target TYPE STANDARD TABLE OF lty_flight_target WITH DEFAULT KEY.

    DATA:
      ls_flight_source TYPE lty_flight_source,
      ls_flight_target TYPE lty_flight_target,

      lt_flight_source TYPE ltt_flight_source.

    me->get_flight_data(
      IMPORTING
        et_flight      = lt_flight_source ).

    DATA(lt_flight_target) = CORRESPONDING ltt_flight_target( lt_flight_source ).

    CLEAR lt_flight_target.
    lt_flight_target = CORRESPONDING #( lt_flight_source
                                        MAPPING flight_date   = flightdate
                                                price         = seatprice
                                                currency_code = currencycode ).

    "[DISCARDING DUPLICATES]: LT_FLIGHT_TARGET should be of TYPE SORTED TABLE.

    "CLEAR lt_flight_target.
    "lt_flight_target = CORRESPONDING #( lt_flight_source DISCARDING DUPLICATES
    "                                    MAPPING flight_date   = flightdate
    "                                            price         = seatprice
    "                                            currency_code = currencycode ).

    "BASE
    "lt_flight_target = CORRESPONDING #( BASE ( lt_flight_target ) lt_flight_source ).

    "BASE + EXPANDING NESTED STRUCTURES
    "ls_flight_target = CORRESPONDING #( DEEP ls_flight_source ).

    "BASE + EXPANDING NESTED STRUCTURES
    "lt_flight_target = CORRESPONDING #( DEEP BASE ( lt_flight_target ) lt_flight_source ).

    "BASE + KEEPING TARGET LINES
    "lt_flight_target = CORRESPONDING #( APPENDING BASE ( lt_flight_target ) lt_flight_source ).

    "BASE + EXPANDING NESTED STRUCTURES KEEPING TARGET LINES
    "lt_flight_target = CORRESPONDING #( DEEP APPENDING BASE ( lt_flight_target ) lt_flight_source ).

    "Mapping Deep Structure
    "demo_corresponding_deep_mapp

    "Mapping using Look Up Table
    "demo_corresponding_using

    "Move Corresponding on Self
    "demo_corresponding_using_self

  ENDMETHOD.


  METHOD let.

  ENDMETHOD.


  METHOD meshes.

    "The full power of meshes will become more clear in the moment when associations will be
    "supported by Open SQL for database views (CDS views, see below) in the future.

    TYPES:
      ltt_airline     TYPE SORTED TABLE OF me->gty_airline
                      WITH UNIQUE KEY AirlineID,

      ltt_conn_routes TYPE SORTED TABLE OF me->gty_conn_routes
                      WITH UNIQUE KEY AirlineID
                                      ConnectionID,

      ltt_flight      TYPE SORTED TABLE OF me->gty_flight
                      WITH UNIQUE KEY AirlineID
                                      ConnectionID
                                      FlightDate.

    TYPES:
      BEGIN OF MESH lmt_flight,
        airline           TYPE ltt_airline
             ASSOCIATION _conn_routes TO connecting_routes
                      ON AirlineID = AirlineID,

        connecting_routes TYPE ltt_conn_routes
             ASSOCIATION _flight TO flight
                      ON AirlineID    = AirlineID
                     AND ConnectionID = ConnectionID,

        flight            TYPE ltt_flight,
      END OF MESH lmt_flight.

    DATA lm_flight TYPE lmt_flight.

    me->get_flight_data(
      IMPORTING
        et_airline     = DATA(lt_airline)
        et_conn_routes = DATA(lt_conn_routes)
        et_flight      = DATA(lt_flight) ).

    lm_flight-airline           = lt_airline.
    lm_flight-connecting_routes = lt_conn_routes.
    lm_flight-flight            = lt_flight.

    LOOP AT lm_flight-airline ASSIGNING FIELD-SYMBOL(<lfs_airline>).
      out->write( <lfs_airline> ).

      "Connecting Routes
      out->write( lm_flight-airline\_conn_routes[ <lfs_airline> ] ).

      "Not Supported in restricted ABAP
      "out->write( lm_flight-airline\_conn_routes[ lm_flight-airline[ airlinename = <lfs_airline>-airlineid ] ] ).

      "Flights
      out->write( lm_flight-airline\_conn_routes[ <lfs_airline> ]\_flight[ ] ).

      "Not Supported in restricted ABAP
      "LOOP AT lm_flight-airline\_conn_routes[ <lfs_airline> ] ASSIGNING FIELD-SYMBOL(<lfs_conn_routes>).
      "ENDLOOP.

      "Not Supported in restricted ABAP
      "LOOP AT lm_flight-airline\_conn_routes[ <lfs_airline> ]\_flight[ ] ASSIGNING FIELD-SYMBOL(<lfs_flight>).
      "ENDLOOP.

      out->write( cl_abap_char_utilities=>newline ).
    ENDLOOP.

    "Reverse Association: Not supported in restricted ABAP
*    LOOP AT lm_flight-flight ASSIGNING FIELD-SYMBOL(<lfs_flight>).
*
*      out->write( <lfs_flight> ).
*
*      "Connecting Routes
*      out->write( lm_flight-flight\^_conn_routes~AirlineID ).
*
*      out->write( cl_abap_char_utilities=>newline ).
*    ENDLOOP.

    "demo_mesh_expressions_flights
    "demo_mesh_loop_at_flights

  ENDMETHOD.
ENDCLASS.

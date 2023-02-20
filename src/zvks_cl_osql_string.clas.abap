CLASS zvks_cl_osql_string DEFINITION
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

    METHODS concatenation
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS parallel_bars_concatenation
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS embedded_expression
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS built_in_string_fn
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS regex_matcher
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

ENDCLASS.



CLASS ZVKS_CL_OSQL_STRING IMPLEMENTATION.


  METHOD regex_matcher.

*    DATA email TYPE string VALUE `mr.important@sap.com`.
*    cl_demo_input=>request( CHANGING field = email ).
*
** Old
*    DATA(matcher) =
*      cl_abap_matcher=>create(
*        pattern     = `\w+(\.\w+)*@(\w+\.)+(\w{2,4})`
*        ignore_case = abap_true
*        text        = email ).
*
*    DATA(match) = matcher->match( ).
*
*    IF match = abap_true.
*      cl_demo_output=>write( 'yes' ).
*    ELSE.
*      cl_demo_output=>write( 'no' ).
*    ENDIF.
*
** New
*    cl_demo_output=>write(
*      COND #( WHEN matches( val   = email
*                            regex = `\w+(\.\w+)*@(\w+\.)+(\w{2,4})`
*                            case  = abap_false )
*                THEN 'yes'
*                ELSE 'no' ) ).

  ENDMETHOD.


  METHOD main.

*    me->concatenation( out ).
*    me->parallel_bars_concatenation( out ).
*    me->embedded_expression( out ).
*    me->built_in_string_fn( out ).

    "WIP
    "me->regex_matcher( out ).

  ENDMETHOD.


  METHOD concatenation.

    DATA(lv_var1)  = 'This'.
    DATA(lv_var2)  = 'is'.
    DATA(lv_var3)  = 'a'.
    DATA(lv_var4)  = 'String'.

    "Spaces are not respected
    out->write( lv_var1 && '_' && lv_var2 && ' ' && lv_var3 && space && lv_var4 ).

    "With string function
    out->write( to_upper( lv_var1 && lv_var2 && lv_var3 && lv_var4 ) ).

  ENDMETHOD.


  METHOD built_in_string_fn.

    "Benefit
    " All the built-in functions are allowed in restrictive ABAP.
    " They all are supported within parallel bar operators.

    "Reference: https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/abenbuilt_in_functions_overview.htm

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Built-in function for casing
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    out->write( cl_abap_char_utilities=>newline ).

    out->write( to_lower( 'This is a string' ) ).
    out->write( to_upper( 'This is a string' ) ).
    out->write( to_mixed( 'This is a string' ) ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Built-in function to reverse string
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    out->write( cl_abap_char_utilities=>newline ).

    out->write( reverse( 'This is a string' ) ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Built-in function to condense
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    out->write( cl_abap_char_utilities=>newline ).

    out->write( condense( 'This is a string' ) ). """"

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Built-in function to calculate string length
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    out->write( cl_abap_char_utilities=>newline ).

    out->write( strlen( 'This is a string     ' ) ). "Spaces are ignored in single quotes
    out->write( strlen( `This is a string     ` ) ). "Spaces are considered in back quotes

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Built-in function to find the count of substring in a string
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    out->write( cl_abap_char_utilities=>newline ).

    out->write( count( val = 'This is a string' sub = 'is' ) ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Built-in function to replace a string with another character
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    out->write( cl_abap_char_utilities=>newline ).

    "Default to first occurrence
    out->write( replace( val = 'This is a string' sub = ` ` with = '_' ) ).
    "Second occurrence
    out->write( replace( val = 'This is a string' sub = ` ` with = '_' occ = 2 ) ).
    "All occurrence
    out->write( replace( val = 'This is a string' sub = ` ` with = '_' occ = 0 ) ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Built-in function to concatenate table into string
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    out->write( cl_abap_char_utilities=>newline ).

    DATA lt_table TYPE STANDARD TABLE OF string.

    lt_table = VALUE #( ( |This| )
                        ( |is| )
                        ( |a| )
                        ( |string| ) ).

    out->write( concat_lines_of( table = lt_table sep = ` ` ) ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Built-in function to find similarity between two strings
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    out->write( cl_abap_char_utilities=>newline ).

    out->write( distance( val1 = 'This is a string'
                          val2 = 'Is this a string' ) ).

    out->write( distance( val1 = 'This is a string?'
                          val2 = 'What is the purpose of the string?' ) ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Built-in function to extract the substring from the string
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    out->write( cl_abap_char_utilities=>newline ).

    out->write( substring(        val = 'This is a string' off = 2 len = 2 ) ).
    out->write( substring_from(   val = 'This is a string' sub = 'is' ) ).
    out->write( substring_after(  val = 'This is a string' sub = 'is' ) ).
    out->write( substring_before( val = 'This is a string' sub = 'is' ) ).
    out->write( substring_to(     val = 'This is a string' sub = 'is' ) ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Built-in function to shift string left and right
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    out->write( cl_abap_char_utilities=>newline ).

    out->write( shift_left(   val = 'This is a string' places = 2 ) ).
    out->write( shift_right(  val = 'This is a string' places = 2 ) ).

  ENDMETHOD.


  METHOD parallel_bars_concatenation.

    "Supports string functions
    "https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abencase_functions.htm

    "Supports embedded expressions
    "https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abapcompute_string_format_options.htm

    "{} can be used embed "non-character" type variables or parameter return from method in concatenation

    DATA(lv_var1)  = `This`.
    DATA(lv_var2)  = 'is'.
    DATA(lv_var3)  = 'a'.
    DATA(lv_var4)  = 'String'.

    "Separated by Horizontal Tab
    out->write( |{ lv_var1 }\t{ lv_var2 }\t{ lv_var3 }\t{ lv_var4 }| ).

    "Separated by New Line
    out->write( |{ lv_var1 }\n{ lv_var2 }\n{ lv_var3 }\n{ lv_var4 }| ).

    "Split Long Text into multiple lines
    out->write(
    "Embed built-in function
    |\nHello { to_mixed( sy-uname ) }!\n| &&
    "Embed class method call with single return parameter in concatenation
    |Today is { cl_abap_context_info=>get_system_date( ) DATE = USER  }.{ cl_abap_char_utilities=>newline }| &&
    "Embed embedded string expressions
    |The hour is { cl_abap_context_info=>get_system_time( ) DIV 3600 }.| ).

    "Embed non-character field
    DATA lv_net_price TYPE /dmo/flight_price VALUE '111.23'.

    out->write( |\nNet Price is { lv_net_price NUMBER = USER }| ).

    "Embed Text Element
    out->write( |\nT001: { TEXT-001 }| ).

  ENDMETHOD.


  METHOD embedded_expression.

    "Reference: https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abapcompute_string_format_options.htm

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " ALPHA
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    out->write( cl_abap_char_utilities=>newline ).

    DATA lv_ebeln TYPE ebeln VALUE 1.
    out->write( |PO External: { lv_ebeln } | &&
                |\nPO Internal: { lv_ebeln ALPHA = IN }| ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Custom IN OUT Conversion - STRING
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    out->write( cl_abap_char_utilities=>newline ).

    DATA(lv_string) = |  MT101|.

    out->write( lv_string ).
    out->write( |{ lv_string WIDTH = 10 ALIGN = LEFT   PAD = '0' }| ).
    out->write( |{ lv_string WIDTH = 10 ALIGN = CENTER PAD = '0' }| ).
    out->write( |{ lv_string WIDTH = 10 ALIGN = RIGHT  PAD = '0' }| ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Custom IN OUT Conversion - INTEGER
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    out->write( cl_abap_char_utilities=>newline ).

    DATA(lv_integer) = 111111.

    out->write( lv_integer ).
    out->write( |{ lv_integer WIDTH = 10 ALIGN = LEFT   PAD = '0' }| ).
    out->write( |{ lv_integer WIDTH = 10 ALIGN = CENTER PAD = '0' }| ).
    out->write( |{ lv_integer WIDTH = 10 ALIGN = RIGHT  PAD = '0' }| ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Custom IN OUT Conversion - NUMC
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "*** Prefer ALPHA instead ****"

    out->write( cl_abap_char_utilities=>newline ).

    DATA(lv_numc) = lv_ebeln.

    out->write( lv_numc ).
    out->write( |{ condense( lv_numc )  WIDTH = 10 ALIGN = LEFT   PAD = '0' }| ).
    out->write( |{ condense( lv_numc )  WIDTH = 10 ALIGN = CENTER PAD = '0' }| ).
    out->write( |{ condense( lv_numc )  WIDTH = 10 ALIGN = RIGHT  PAD = '0' }| ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " DATE & COUNTRY
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    out->write( cl_abap_char_utilities=>newline ).

    out->write( |Date Raw: {   cl_abap_context_info=>get_system_date( ) DATE    = RAW   }| ).
    out->write( |Date User: {  cl_abap_context_info=>get_system_date( ) DATE    = USER  }| ).
    out->write( |Date US: {    cl_abap_context_info=>get_system_date( ) COUNTRY = 'US ' }| ).
    out->write( |Date India: { cl_abap_context_info=>get_system_date( ) COUNTRY = 'IN ' }| ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " NUMBER
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    out->write( cl_abap_char_utilities=>newline ).

    DATA lv_number TYPE p DECIMALS 2 VALUE '1234567.89'.

    out->write( |Date Raw: {   lv_number NUMBER  = RAW   }| ).
    out->write( |Date User: {  lv_number NUMBER  = USER  }| ).
    out->write( |Date US: {    lv_number COUNTRY = 'US ' }| ).
    out->write( |Date India: { lv_number COUNTRY = 'IN ' }| ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " CASE
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "*** Prefer Built-In string functions instead ****"

    "https://blogs.sap.com/2013/05/17/using-new-abap-stuff-new-options-for-strings/

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    me->main( out ).
  ENDMETHOD.
ENDCLASS.

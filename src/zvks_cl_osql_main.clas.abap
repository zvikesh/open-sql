CLASS zvks_cl_osql_main DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    METHODS main.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zvks_cl_osql_main IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    me->main( ).
  ENDMETHOD.


  METHOD main.

    "Inline declaration
    NEW zvks_cl_osql_inline( )->main( ).

    "SELECT statement
    NEW zvks_cl_osql_select( )->main( ).

    "String Operations
    NEW zvks_cl_osql_string( )->main( ).

    "Internal Table
    NEW zvks_cl_osql_itab( )->main( ).

    "OOABAP
    "NEW zvks_cl_osql_ooabap( )->main( ).

    "Vs
    "NEW zvks_cl_osql_vs( )->main( ).

    "EML
    "zvks_cl_osql_eml=>execute( ).

  ENDMETHOD.
ENDCLASS.

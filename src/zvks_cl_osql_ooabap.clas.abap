CLASS zvks_cl_osql_ooabap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    METHODS main
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out OPTIONAL.

    METHODS write_string
      IMPORTING
        iv_text TYPE string
        out     TYPE REF TO if_oo_adt_classrun_out OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS new_operator
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out OPTIONAL.

    METHODS casting
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out OPTIONAL.

    METHODS method_chaining
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out OPTIONAL.

    METHODS enumerations
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out OPTIONAL.

ENDCLASS.

CLASS zvks_cl_osql_ooabap IMPLEMENTATION.

  METHOD main.

    "me->new_operator( out ).
    me->casting( out ).

    "Exception Class with Dynamic Text
    " RAISE EXCEPTION TYPE zvks_cx_sy_dyn_text
    "           MESSAGE ID `ZVKS`
    "                 TYPE `E`
    "               NUMBER 000
    "               WITH lv_msgv1
    "                    lv_msgv2
    "                    lv_msgv3
    "                    lv_msgv4.

    "Prallelization
    "https://blogs.sap.com/2021/08/26/async-parallel-abap-in-a-oo-way/
    "https://blogs.sap.com/2013/07/16/parallel-abap-objects/
    "https://github.com/victorizbitskiy/zconcurrency_api
    "https://github.com/BiberM/ABAPParallelizationService

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.
    me->main( out ).
  ENDMETHOD.

  METHOD write_string.
    out->write( iv_text ).
  ENDMETHOD.

  METHOD new_operator.

    "Old
    DATA lo_osql_ooabap TYPE REF TO zvks_cl_osql_ooabap.
    CREATE OBJECT lo_osql_ooabap.

    lo_osql_ooabap->write_string(
      iv_text = `Hello Old!`
      out     = out
    ).

    "To CREATE OBJECT
    DATA(lo_osql_ooabap_new) = NEW zvks_cl_osql_ooabap( ).

    lo_osql_ooabap_new->write_string(
      iv_text = `Hello New!`
      out     = out
    ).

    "To call instance without explicit Object
    NEW zvks_cl_osql_ooabap( )->write_string(
      iv_text = `Hello without Object!`
      out     = out
    ).

  ENDMETHOD.

  METHOD casting.

    "Old
    DATA:
      ls_component   TYPE cl_abap_structdescr=>component,
      lt_component   TYPE cl_abap_structdescr=>component_table,
      ls_components  TYPE abap_compdescr_tab,
      lo_structdescr TYPE REF TO cl_abap_structdescr.

    lo_structdescr ?= cl_abap_typedescr=>describe_by_name( `/DMO/FLIGHT` ).
    lt_component   = lo_structdescr->get_components( ).

    DATA(ls_co) = lo_structdescr->components.

    out->write( lt_component ).

    "New
    DATA(lo_structdescr_new) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( `/DMO/FLIGHT` ) ).
    DATA(lt_component_new)   = lo_structdescr_new->get_components( ).

    out->write( cl_abap_char_utilities=>newline ).
    out->write( lt_component_new ).

  ENDMETHOD.

  METHOD method_chaining.

    "Old
    DATA(lo_structdescr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( `/DMO/FLIGHT` ) ).
    DATA(lt_component)   = lo_structdescr->get_components( ).

    out->write( cl_abap_char_utilities=>newline ).
    out->write( lt_component ).

    "New
    DATA(lt_component_new)   = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( `/DMO/FLIGHT` ) )->get_components( ).

    out->write( cl_abap_char_utilities=>newline ).
    out->write( lt_component_new ).

  ENDMETHOD.

  METHOD enumerations.
    "https://github.com/suriyarasu/New-ABAP-Tips-Tricks/blob/e3be77d997ed86434fc281f36e8dcf8475a50b90/Enumeration.clas.abap
    "demo_old_vs_new_enumerations
  ENDMETHOD.
ENDCLASS.

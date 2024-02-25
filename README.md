Inline Declaration (ZVKS_CL_OSQL_INLINE)
- Use of DATA and FINAL keyword for inline declaration of development objects.
- APPEND VALUE #() and APPEND LINES OF VALUE #() to an internal table.

SELECT Enhancement (ZVKS_CL_OSQL_SELECT)
String Operations (ZVKS_CL_OSQL_SELECT)
Internal Table (ZVKS_CL_OSQL_ITAB)
Object Oriented Programming (ZVKS_CL_OSQL_OOABAP)

**Create Another Table with Unique Records**

Old approach is bettter.

```
    TYPES:
      BEGIN OF lty_product_plant,
        product TYPE matnr,
        plant   TYPE werks_d,
      END OF lty_product_plant,
      ltty_product_plant TYPE STANDARD TABLE OF lty_product_plant WITH DEFAULT KEY,

      BEGIN OF lty_product,
        product TYPE matnr,
      END OF lty_product,
      ltty_product TYPE STANDARD TABLE OF lty_product WITH DEFAULT KEY.

    DATA:
      lt_product       TYPE ltty_product,
      lt_product_plant TYPE ltty_product_plant.

    DO 10000 TIMES.
      DATA(lv_product) = |MAT_{ sy-index }|.
      DO 1000 TIMES.
        APPEND VALUE #( product = CONV matnr( lv_product ) plant = |P{ sy-index }| ) TO lt_product_plant.
      ENDDO.
    ENDDO.

    out->write( |No of records fetched { lines( lt_product_plant ) }| ).

    "-----------------------------------------------------------------
    " Create a table with unique Materials - 7.5 Operators
    "-----------------------------------------------------------------
    GET TIME STAMP FIELD DATA(lv_tstmp_start).

    lt_product = VALUE #( FOR GROUPS <lfs_gr_product_plant> OF <lfs_product_plant>
                           IN lt_product_plant
                              "WHERE ( item_cat IS NOT INITIAL )
                              GROUP BY ( product = <lfs_product_plant>-product
                                         group_index = GROUP INDEX
                                         group_size = GROUP SIZE )
                              ASCENDING
                              ( "index   = <lfs_gr_product_plant>-group_index
                                product = <lfs_gr_product_plant>-product ) ).

    "DATA(lv_utcl_end) = utclong_current( ).
    GET TIME STAMP FIELD DATA(lv_tstmp_end).

    out->write( |Unique Products { lines( lt_product ) }| ).
    out->write( |Execution Time (7.5 Operators) { cl_abap_tstmp=>subtract( tstmp1 = lv_tstmp_end tstmp2 = lv_tstmp_start ) }| ).

    "-----------------------------------------------------------------
    " Create a table with unique Materials - Classic Approach
    "-----------------------------------------------------------------
    CLEAR: lt_product, lv_tstmp_start, lv_tstmp_end.

    GET TIME STAMP FIELD lv_tstmp_start.

    lt_product = CORRESPONDING #( lt_product_plant ).

    SORT lt_product BY product.
    DELETE ADJACENT DUPLICATES FROM lt_product COMPARING product.

    GET TIME STAMP FIELD lv_tstmp_end.

    out->write( |Unique Products { lines( lt_product ) }| ).
    out->write( |Execution Time (Classic Approach) { cl_abap_tstmp=>subtract( tstmp1 = lv_tstmp_end tstmp2 = lv_tstmp_start ) }| ).

  ENDMETHOD.
```

**Create Range**

```
    "Create material range with unique materials
    lr_matnr = VALUE #( FOR GROUPS lgr_data OF ls_data
                         IN it_data
                            WHERE ( item_cat IS NOT INITIAL )
                            GROUP BY ls_data-mat_num
                            ASCENDING
                            ( sign = zif_constants_declare=>gc_range_sign-include       "I
                              option = zif_constants_declare=>gc_range_option-equals_to "EQ
                              low = lgr_data ) ).
```

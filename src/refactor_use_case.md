### Original Code

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
    " Create a table with unique Materials - Classic Approach
    "-----------------------------------------------------------------
    CLEAR: lt_product.

    lt_product = CORRESPONDING #( lt_product_plant ).
    SORT lt_product BY product.
    DELETE ADJACENT DUPLICATES FROM lt_product COMPARING product.

    "1. Create a table lt_product with unique Materials as per 7.5 Operators.

    "2. Create a table lt_product with unique materials using FOR GROUPS opeator on lt_product_plant grouping by product field sorted by products in ascending order.

 
  ENDMETHOD.
```

### Expected Refactored Code

```
    lt_product = VALUE #( FOR GROUPS <lfs_gr_product_plant> OF <lfs_product_plant>
                           IN lt_product_plant
                              GROUP BY ( product = <lfs_product_plant>-product
                                         group_index = GROUP INDEX
                                         group_size = GROUP SIZE )
                              ASCENDING
                              ( "index   = <lfs_gr_product_plant>-group_index
                                product = <lfs_gr_product_plant>-product ) ).
```

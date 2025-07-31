# tbl_listing() works with default values

    Code
      tbl_listing(tld)$table_body
    Output
      # A tibble: 8 x 4
        trt      age marker stage
        <chr>  <dbl>  <dbl> <fct>
      1 Drug B     9  1.11  T2   
      2 Drug B    49  0.157 T2   
      3 Drug A    NA  2.07  T3   
      4 Drug A    78  0.175 T3   
      5 Drug B    34  0.205 T3   
      6 Drug B    63  0.06  T3   
      7 Drug A    47  0.266 T2   
      8 Drug A    52  0.719 T2   

# tbl_listing(keys, order_by) works with standard values

    Code
      head(out$table_body, n = 5)
    Output
      # A tibble: 5 x 4
        trt      stage   age marker
        <chr>    <chr> <dbl>  <dbl>
      1 "Drug B" "T2"      9  1.11 
      2 ""       "T3"     34  0.205
      3 "Drug A" "T2"     47  0.266
      4 "Drug B" ""       49  0.157
      5 "Drug A" ""       52  0.719

# tbl_listing(hide_duplicate_keys = FALSE) works with standard values

    Code
      w_duplicated_keys[seq(3), ]
    Output
      # A tibble: 3 x 4
        trt      stage   age marker
        <chr>    <chr> <dbl>  <dbl>
      1 "Drug A" "T2"     47  0.266
      2 ""       ""       52  0.719
      3 ""       "T3"     NA  2.07 

---

    Code
      wo_duplicated_keys[seq(3), ]
    Output
      # A tibble: 3 x 4
        trt    stage   age marker
        <chr>  <fct> <dbl>  <dbl>
      1 Drug A T2       47  0.266
      2 Drug A T2       52  0.719
      3 Drug A T3       NA  2.07 

# tbl_listing(blank_rows_by) works with standard values

    Code
      head(out$table_body, n = 5)
    Output
      # A tibble: 5 x 4
        trt    stage   age marker
        <chr>  <chr> <dbl>  <dbl>
      1 Drug A T2       47  0.266
      2 <NA>   <NA>     NA NA    
      3 Drug A T2       52  0.719
      4 <NA>   <NA>     NA NA    
      5 Drug A T3       NA  2.07 


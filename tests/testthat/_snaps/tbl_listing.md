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

# tbl_listing(hide_duplicate_keys = FALSE) works with standard values

    Code
      w_duplicated_keys$table_body[seq(3), ]
    Output
      # A tibble: 3 x 4
        trt        age marker stage
        <chr>    <dbl>  <dbl> <fct>
      1 "Drug B"     9  1.11  T2   
      2 ""          49  0.157 T2   
      3 "Drug A"    NA  2.07  T3   

---

    Code
      wo_duplicated_keys$table_body[seq(3), ]
    Output
      # A tibble: 3 x 4
        trt      age marker stage
        <chr>  <dbl>  <dbl> <fct>
      1 Drug B     9  1.11  T2   
      2 Drug B    49  0.157 T2   
      3 Drug A    NA  2.07  T3   

# tbl_listing(blank_rows_by) works with standard values

    Code
      dplyr::slice_head(out$table_body, n = 5)
    Output
      # A tibble: 5 x 4
        trt      age marker stage
        <chr>  <dbl>  <dbl> <fct>
      1 Drug B     9  1.11  T2   
      2 <NA>      NA NA     <NA> 
      3 Drug B    49  0.157 T2   
      4 <NA>      NA NA     <NA> 
      5 Drug A    NA  2.07  T3   

# tbl_listing(row_split) works with standard values

    Code
      out[[2]]$table_body
    Output
      # A tibble: 1 x 5
        trt      age marker stage row_number
        <chr>  <dbl>  <dbl> <fct>      <int>
      1 Drug A    NA   2.07 T3             3

# tbl_listing(row_split, blank_rows_by) works with standard values

    Code
      dplyr::slice_head(out[[3]]$table_body, n = 4)
    Output
      # A tibble: 4 x 5
        trt      age marker stage row_number
        <chr>  <dbl>  <dbl> <fct>      <int>
      1 Drug A    NA  2.07  T3             4
      2 Drug A    78  0.175 T3             5
      3 <NA>      NA NA     <NA>           6
      4 Drug B    34  0.205 T3             7

# tbl_listing(col_split) works with standard values

    Code
      dplyr::slice_head(out[[2]]$table_body, n = 4)
    Output
      # A tibble: 4 x 4
        trt        age marker stage
        <chr>    <dbl>  <dbl> <fct>
      1 "Drug B"     9  1.11  T2   
      2 ""          49  0.157 T2   
      3 "Drug A"    NA  2.07  T3   
      4 ""          78  0.175 T3   

# tbl_listing(row_split + col_split) works with standard values

    Code
      out[[4]]$table_body
    Output
      # A tibble: 4 x 5
        trt      age marker stage row_number
        <chr>  <dbl>  <dbl> <fct>      <int>
      1 <NA>      NA NA     <NA>           3
      2 Drug A    NA  2.07  T3             4
      3 Drug A    78  0.175 T3             5
      4 <NA>      NA NA     <NA>           6


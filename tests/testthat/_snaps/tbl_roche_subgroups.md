# tbl_roche_subgroups(time_to_event=NULL) works

    Code
      dplyr::mutate(dplyr::select(tbl$table_body, dplyr::starts_with("label"), dplyr::starts_with(
        "stat"), dplyr::starts_with("estimate"), dplyr::starts_with("ci"), dplyr::starts_with(
        "p.value")), dplyr::across(tidyselect::where(is.numeric), ~ round(.x, digits = 3)))
    Output
      # A tibble: 10 x 10
         label_1 label_2 label_3 stat_0_1 stat_1_1_2 stat_1_2_2 statistic_3 estimate_3
         <chr>   <chr>   <chr>   <chr>    <chr>      <chr>            <dbl>      <dbl>
       1 All Pa~ respon~ Chemot~ 193      95 (29.5 ~ 98 (33.7 ~       0.625      1.04 
       2 Grade   <NA>    <NA>    <NA>     <NA>       <NA>            NA         NA    
       3 groupn~ respon~ Chemot~ 67       35 (22.9 ~ 32 (40.6 ~       1.57       1.19 
       4 groupn~ respon~ Chemot~ 63       30 (23.3 ~ 33 (36.4 ~       1.12       1.14 
       5 groupn~ respon~ Chemot~ 63       30 (43.3 ~ 33 (24.2 ~      -1.61       0.826
       6 T Stage <NA>    <NA>    <NA>     <NA>       <NA>            NA         NA    
       7 groupn~ respon~ Chemot~ 52       28 (25.0 ~ 24 (45.8 ~       1.58       1.23 
       8 groupn~ respon~ Chemot~ 52       24 (25.0 ~ 28 (25.0 ~       0          1    
       9 groupn~ respon~ Chemot~ 40       20 (40.0 ~ 20 (35.0 ~      -0.319      0.951
      10 groupn~ respon~ Chemot~ 49       23 (30.4 ~ 26 (30.8 ~       0.025      1.00 
      # i 2 more variables: ci_3 <glue>, p.value_3 <dbl>

# tbl_roche_subgroups(time_to_event) works

    Code
      dplyr::select(tbl$table_body, dplyr::starts_with("label"), dplyr::starts_with(
        "stat"))
    Output
      # A tibble: 7 x 9
        label_1   label_2 label_3 stat_0_1 stat_1_1_2 stat_2_1_2 stat_1_2_2 stat_2_2_2
        <chr>     <chr>   <chr>   <chr>    <chr>      <chr>      <chr>      <chr>     
      1 All Part~ time    X1      10       5          9.0        5          9.2       
      2 grade     <NA>    <NA>    <NA>     <NA>       <NA>       <NA>       <NA>      
      3 groupnam~ time    X1      5        2          5.0        3          9.2       
      4 groupnam~ time    X1      5        3          13.4       2          33.8      
      5 strata    <NA>    <NA>    <NA>     <NA>       <NA>       <NA>       <NA>      
      6 groupnam~ time    X1      5        3          13.4       2          33.8      
      7 groupnam~ time    X1      5        2          5.0        3          9.2       
      # i 1 more variable: stat_0_3 <chr>


# tbl_roche_subgroups(time_to_event=NULL) works

    Code
      dplyr::select(tbl$table_body, dplyr::starts_with("label"), dplyr::starts_with(
        "stat"), dplyr::starts_with("estimate"), dplyr::starts_with("ci"), dplyr::starts_with(
        "p.value"))
    Output
      # A tibble: 10 x 10
         label_1 label_2 label_3 stat_0_1 stat_1_1_2 stat_1_2_2 statistic_3 estimate_3
         <glue>  <chr>   <chr>   <chr>    <chr>      <chr>            <dbl>      <dbl>
       1 All Pa~ respon~ Chemot~ 193      95 (29.5 ~ 98 (33.7 ~    6.25e- 1      1.04 
       2 Grade   <NA>    <NA>    <NA>     <NA>       <NA>         NA            NA    
       3 I       respon~ Chemot~ 67       35 (22.9 ~ 32 (40.6 ~    1.57e+ 0      1.19 
       4 II      respon~ Chemot~ 63       30 (23.3 ~ 33 (36.4 ~    1.12e+ 0      1.14 
       5 III     respon~ Chemot~ 63       30 (43.3 ~ 33 (24.2 ~   -1.61e+ 0      0.826
       6 T Stage <NA>    <NA>    <NA>     <NA>       <NA>         NA            NA    
       7 T1      respon~ Chemot~ 52       28 (25.0 ~ 24 (45.8 ~    1.58e+ 0      1.23 
       8 T2      respon~ Chemot~ 52       24 (25.0 ~ 28 (25.0 ~    1.26e-16      1    
       9 T3      respon~ Chemot~ 40       20 (40.0 ~ 20 (35.0 ~   -3.19e- 1      0.951
      10 T4      respon~ Chemot~ 49       23 (30.4 ~ 26 (30.8 ~    2.48e- 2      1.00 
      # i 2 more variables: ci_3 <glue>, p.value_3 <dbl>

# tbl_roche_subgroups(time_to_event) works

    Code
      dplyr::select(tbl$table_body, dplyr::starts_with("label"), dplyr::starts_with(
        "stat"))
    Output
      # A tibble: 7 x 9
        label_1   label_2 label_3 stat_0_1 stat_1_1_2 stat_2_1_2 stat_1_2_2 stat_2_2_2
        <glue>    <chr>   <chr>   <chr>    <chr>      <chr>      <chr>      <chr>     
      1 All Part~ time    X1      10       6          9.3        4          5.9       
      2 grade     <NA>    <NA>    <NA>     <NA>       <NA>       <NA>       <NA>      
      3 I         time    X1      4        2          16.9       2          17.9      
      4 II        time    X1      6        4          5.2        2          4.2       
      5 strata    <NA>    <NA>    <NA>     <NA>       <NA>       <NA>       <NA>      
      6 1         time    X1      3        2          15.0       1          1.3       
      7 2         time    X1      7        4          5.2        3          7.1       
      # i 1 more variable: stat_0_3 <chr>


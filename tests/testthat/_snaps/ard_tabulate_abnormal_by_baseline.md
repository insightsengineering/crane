# ard_tabulate_abnormal_by_baseline() works with standard inputs

    Code
      select(as.data.frame(res), variable, variable_level, stat_name, stat_label,
      stat)
    Output
         variable variable_level stat_name stat_label      stat
      1       Low        Not Low         n          n         2
      2       Low        Not Low         N          N         7
      3       Low        Not Low         p          % 0.2857143
      4       Low        Not Low         n          n         4
      5       Low        Not Low         N          N         7
      6       Low        Not Low         p          % 0.5714286
      7       Low        Not Low         n          n         4
      8       Low        Not Low         N          N         6
      9       Low        Not Low         p          % 0.6666667
      10      Low            Low         n          n         2
      11      Low            Low         N          N         7
      12      Low            Low         p          % 0.2857143
      13      Low            Low         n          n         3
      14      Low            Low         N          N         7
      15      Low            Low         p          % 0.4285714
      16      Low            Low         n          n         2
      17      Low            Low         N          N         6
      18      Low            Low         p          % 0.3333333
      19      Low          Total         n          n         2
      20      Low          Total         N          N         7
      21      Low          Total         p          % 0.2857143
      22      Low          Total         n          n         4
      23      Low          Total         N          N         7
      24      Low          Total         p          % 0.5714286
      25      Low          Total         n          n         4
      26      Low          Total         N          N         6
      27      Low          Total         p          % 0.6666667
      28     High       Not High         n          n         3
      29     High       Not High         N          N         7
      30     High       Not High         p          % 0.4285714
      31     High       Not High         n          n         3
      32     High       Not High         N          N         7
      33     High       Not High         p          % 0.4285714
      34     High       Not High         n          n         3
      35     High       Not High         N          N         6
      36     High       Not High         p          %       0.5
      37     High           High         n          n         2
      38     High           High         N          N         7
      39     High           High         p          % 0.2857143
      40     High           High         n          n         3
      41     High           High         N          N         7
      42     High           High         p          % 0.4285714
      43     High           High         n          n         2
      44     High           High         N          N         6
      45     High           High         p          % 0.3333333
      46     High          Total         n          n         3
      47     High          Total         N          N         7
      48     High          Total         p          % 0.4285714
      49     High          Total         n          n         3
      50     High          Total         N          N         7
      51     High          Total         p          % 0.4285714
      52     High          Total         n          n         3
      53     High          Total         N          N         6
      54     High          Total         p          %       0.5


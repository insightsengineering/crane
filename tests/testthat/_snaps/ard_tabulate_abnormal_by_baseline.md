# ard_tabulate_abnormal_by_baseline() works with standard inputs

    Code
      select(as.data.frame(res), variable, variable_level, stat_name, stat_label,
      stat)
    Output
         variable variable_level stat_name stat_label      stat
      1   LBNRIND       Not High         n          n         3
      2   LBNRIND       Not High         N          N         7
      3   LBNRIND       Not High         p          % 0.4285714
      4   LBNRIND       Not High         n          n         3
      5   LBNRIND       Not High         N          N         7
      6   LBNRIND       Not High         p          % 0.4285714
      7   LBNRIND       Not High         n          n         3
      8   LBNRIND       Not High         N          N         6
      9   LBNRIND       Not High         p          %       0.5
      10  LBNRIND           High         n          n         2
      11  LBNRIND           High         N          N         7
      12  LBNRIND           High         p          % 0.2857143
      13  LBNRIND           High         n          n         3
      14  LBNRIND           High         N          N         7
      15  LBNRIND           High         p          % 0.4285714
      16  LBNRIND           High         n          n         2
      17  LBNRIND           High         N          N         6
      18  LBNRIND           High         p          % 0.3333333
      19  LBNRIND         Not NA         n          n         0
      20  LBNRIND         Not NA         N          N         7
      21  LBNRIND         Not NA         p          %         0
      22  LBNRIND         Not NA         n          n         0
      23  LBNRIND         Not NA         N          N         7
      24  LBNRIND         Not NA         p          %         0
      25  LBNRIND         Not NA         n          n         0
      26  LBNRIND         Not NA         N          N         6
      27  LBNRIND         Not NA         p          %         0
      28  LBNRIND          Total         n          n         0
      29  LBNRIND          Total         N          N         7
      30  LBNRIND          Total         p          %         0
      31  LBNRIND          Total         n          n         0
      32  LBNRIND          Total         N          N         7
      33  LBNRIND          Total         p          %         0
      34  LBNRIND          Total         n          n         0
      35  LBNRIND          Total         N          N         6
      36  LBNRIND          Total         p          %         0


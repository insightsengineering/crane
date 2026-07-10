# annotate_riskdf() errors early on a discrete x-axis

    Code
      annotate_riskdf(p_disc, fit_strat)
    Condition
      Error in `annotate_riskdf()`:
      ! `gg_plt` must have a continuous (numeric) x-axis.
      i The risk table is aligned to survival times, which require numeric x-axis breaks.
      x A discrete or categorical x-axis was detected.


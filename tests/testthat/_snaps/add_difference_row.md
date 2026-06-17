# add_difference_row.tbl_survfit_times() error messaging works

    Code
      add_difference_row(tbl_survfit_times(df_unstrat), fit = fit_unstrat, reference = "Placebo")
    Condition
      Error in `add_difference_row()`:
      ! Cannot run `add_difference_row()` when the `fit` model does not include a strata/by variable.

---

    Code
      add_difference_row(tbl_survfit_times(surv_df), fit = fit_strat, reference = "No Treatment")
    Condition
      Error in `add_difference_row()`:
      ! Could not infer `times` dynamically from the table inputs. Please provide the `times` argument explicitly.


# crane 0.1.0.9035

* Added `theme_gtsummary_roche(font_size, print_egine)` arguments to control the default font size and the print engine.

* Added `roche_percent()` and `label_roche_percent()` functions.

* Updated Roche theme to use `roche_percent()` as the default function to format percentages.

* The {gtsummary} is now loaded with {crane}.

* The `tbl_demographics()` function has been renamed to `tbl_roche_summary()` with the following updates. (#73).

  * Updated `tbl_roche_summary()` to convert cells with `"0 (0.0%)"` to `"0"`.
  
  * Update the `tbl_roche_summary()` returned `inputs` list to match the `tbl_roche_summary()` argument inputs, whereas it previously returned the `tbl_summary()` inputs. (#31)
  
  * Changed the default header in `tbl_roche_summary()` tables from `**Characteristic**` to an empty string.
  
  * Fix in `tbl_roche_summary()` when a variable is all `NA` within a stratum. The zero count was displayed as `"0 (NA%)"` instead of `"0"`. (#60)

* Adding the `modify_zero_recode()` function that recodes `"0 (0.0%)"` to `"0"` in summary tables.

* Adding the `modify_header_rm_md()` function to remove bold and italic markdown syntax from the headers of a gtsummary table. (#53)

* Adding `tbl_hierarchical_rate_and_count()` and associated S3 method `add_overall()` to summarize hierarchical event rates and counts. (#36)

* Adding `tbl_survfit_times()` and `add_overall.tbl_survfit_times()`

* Added an article giving an overview of the {crane} package. (#37)

* Added `tbl_survfit_quantiles()` and `add_overall.tbl_survfit_quantiles()` to create a table of survival quantiles. (#19)

* Added {flextable} specs for `theme_gtsummary_roche()`. Mainly border line width (0.5) and footer font size (`font_size` - 1).

# crane 0.1.0

* Initial release

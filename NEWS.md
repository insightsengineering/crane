# crane 0.1.0.9023

* Adding the `strip_md_bold()` and `strip_md_italic()` functions to remove bold and italic markdown syntax from a character vector. (#53)

* Adding `tbl_hierarchical_rate_and_count()` and associated S3 method `add_overall()` to summarize hierarchical event rates and counts. (#36)

* Adding `tbl_survfit_times()` and `add_overall.tbl_survfit_times()`

* Added an article giving an overview of the {crane} package. (#37)

* Updated `tbl_demographics()` to convert cells with `"0 (0.0%)"` to `"0"`.

* Added `tbl_survfit_quantiles()` and `add_overall.tbl_survfit_quantiles()` to create a table of survival quantiles. (#19)

* Changed the default header in `tbl_demographics()` tables from `**Characteristic**` to an empty string.

* Update the `tbl_demographics()` returned `inputs` list to match the `tbl_demographics()` argument inputs, whereas it previously returned the `tbl_summary()` inputs. (#31)

* Loading gtsummary, cards, and cardx along with crane. (#29)

* Added `{flextable}` specs for `theme_gtsummary_roche()`. Mainly border line width (0.5) and footer font size (`font_size` - 1).

* Fix in `tbl_demographics()` when a variable is all `NA` within a stratum. The zero count was displayed as `"0 (NA%)"` instead of `"0"`. (#60)

# crane 0.1.0

* Initial release

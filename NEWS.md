# crane 0.1.0.9005

* Added `tbl_survfit_quantiles()` to create a table of survival quantiles. (#19)

* Changed the default header in `tbl_demographics()` tables from `**Characteristic**` to an empty string.

* Update the `tbl_demographics()` returned `inputs` list to match the `tbl_demographics()` argument inputs, whereas it previously returned the `tbl_summary()` inputs. (#31)

* Loading gtsummary, cards, and cardx along with crane. (#29)

* Added `{flextable}` specs for `theme_gtsummary_roche()`. Mainly border line width (0.5) and footer font size (`font_size` - 1).

# crane 0.1.0

* Initial release

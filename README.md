
<!-- README.md is generated from README.Rmd. Please edit that file -->

# crane

<!-- badges: start -->

<!-- badges: end -->

The {crane} package provides supplementary functions to the {gtsummary}
specifically for trial reporting in the pharmecuetical industry.

## Installation

You can install the development version of crane from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("insightsengineering/crane")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(crane)
#> Loading required package: gtsummary
theme_gtsummary_roche()
#> Setting theme "Roche"

tbl <- tbl_demographics(trial, by = trt, include = c(age, grade))
#> Setting theme "Roche"

tbl
```

<div id="jxhwchgiav" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#jxhwchgiav table {
  font-family: ui-monospace, 'Cascadia Code', 'Source Code Pro', Menlo, Consolas, 'DejaVu Sans Mono', monospace, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#jxhwchgiav thead, #jxhwchgiav tbody, #jxhwchgiav tfoot, #jxhwchgiav tr, #jxhwchgiav td, #jxhwchgiav th {
  border-style: none;
}
&#10;#jxhwchgiav p {
  margin: 0;
  padding: 0;
}
&#10;#jxhwchgiav .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#jxhwchgiav .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#jxhwchgiav .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#jxhwchgiav .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#jxhwchgiav .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#jxhwchgiav .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jxhwchgiav .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#jxhwchgiav .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#jxhwchgiav .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#jxhwchgiav .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#jxhwchgiav .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#jxhwchgiav .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#jxhwchgiav .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#jxhwchgiav .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#jxhwchgiav .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#jxhwchgiav .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#jxhwchgiav .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#jxhwchgiav .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#jxhwchgiav .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jxhwchgiav .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#jxhwchgiav .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#jxhwchgiav .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#jxhwchgiav .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jxhwchgiav .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#jxhwchgiav .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#jxhwchgiav .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jxhwchgiav .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jxhwchgiav .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#jxhwchgiav .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#jxhwchgiav .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#jxhwchgiav .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jxhwchgiav .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#jxhwchgiav .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jxhwchgiav .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#jxhwchgiav .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jxhwchgiav .gt_left {
  text-align: left;
}
&#10;#jxhwchgiav .gt_center {
  text-align: center;
}
&#10;#jxhwchgiav .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#jxhwchgiav .gt_font_normal {
  font-weight: normal;
}
&#10;#jxhwchgiav .gt_font_bold {
  font-weight: bold;
}
&#10;#jxhwchgiav .gt_font_italic {
  font-style: italic;
}
&#10;#jxhwchgiav .gt_super {
  font-size: 65%;
}
&#10;#jxhwchgiav .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#jxhwchgiav .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#jxhwchgiav .gt_indent_1 {
  text-indent: 5px;
}
&#10;#jxhwchgiav .gt_indent_2 {
  text-indent: 10px;
}
&#10;#jxhwchgiav .gt_indent_3 {
  text-indent: 15px;
}
&#10;#jxhwchgiav .gt_indent_4 {
  text-indent: 20px;
}
&#10;#jxhwchgiav .gt_indent_5 {
  text-indent: 25px;
}
&#10;#jxhwchgiav .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#jxhwchgiav div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_1"><span class='gt_from_md'><strong>Drug A</strong><br />
N = 98</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_2"><span class='gt_from_md'><strong>Drug B</strong><br />
N = 102</span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    n</td>
<td headers="stat_1" class="gt_row gt_center">91</td>
<td headers="stat_2" class="gt_row gt_center">98</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean (SD)</td>
<td headers="stat_1" class="gt_row gt_center">47 (15)</td>
<td headers="stat_2" class="gt_row gt_center">47 (14)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median (Q1, Q3)</td>
<td headers="stat_1" class="gt_row gt_center">46 (37, 60)</td>
<td headers="stat_2" class="gt_row gt_center">48 (39, 56)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Min, Max</td>
<td headers="stat_1" class="gt_row gt_center">6, 78</td>
<td headers="stat_2" class="gt_row gt_center">9, 83</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Grade, n (%)</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    n</td>
<td headers="stat_1" class="gt_row gt_center">98</td>
<td headers="stat_2" class="gt_row gt_center">102</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    I</td>
<td headers="stat_1" class="gt_row gt_center">35 (35.7%)</td>
<td headers="stat_2" class="gt_row gt_center">33 (32.4%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    II</td>
<td headers="stat_1" class="gt_row gt_center">32 (32.7%)</td>
<td headers="stat_2" class="gt_row gt_center">36 (35.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    III</td>
<td headers="stat_1" class="gt_row gt_center">31 (31.6%)</td>
<td headers="stat_2" class="gt_row gt_center">33 (32.4%)</td></tr>
  </tbody>
  &#10;  
</table>
</div>

Each gtsummary table also contains the Analysis Results Dataset (ARD)

``` r
# extract ARD from table
gather_ard(tbl)
#> $tbl_summary
#> {cards} data frame: 66 x 12
#>    group1 group1_level variable variable_level stat_name stat_label  stat
#> 1     trt       Drug A    grade              I         n          n    35
#> 2     trt       Drug A    grade              I         N          N    98
#> 3     trt       Drug A    grade              I         p          % 0.357
#> 4     trt       Drug B    grade              I         n          n    33
#> 5     trt       Drug B    grade              I         N          N   102
#> 6     trt       Drug B    grade              I         p          % 0.324
#> 7     trt       Drug A    grade             II         n          n    32
#> 8     trt       Drug A    grade             II         N          N    98
#> 9     trt       Drug A    grade             II         p          % 0.327
#> 10    trt       Drug B    grade             II         n          n    36
#>    gts_column
#> 1      stat_1
#> 2      stat_1
#> 3      stat_1
#> 4      stat_2
#> 5      stat_2
#> 6      stat_2
#> 7      stat_1
#> 8      stat_1
#> 9      stat_1
#> 10     stat_2
#> ℹ 56 more rows
#> ℹ Use `print(n = ...)` to see more rows
#> ℹ 4 more variables: context, fmt_fn, warning, error
```

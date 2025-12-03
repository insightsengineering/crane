f_conf_level <- function(conf_level) {
  # assert_proportion_value(conf_level) # Assuming assert_proportion_value is defined elsewhere
  paste0(conf_level * 100, "% CI")
}

control_surv_med_annot <- function(x = 0.8, y = 0.85, w = 0.32, h = 0.16, fill = TRUE) {
  list(x = x, y = y, w = w, h = h, fill = fill)
}

control_coxph_annot <- function(x = 0.29, y = 0.51, w = 0.4, h = 0.125, fill = TRUE, ref_lbls = FALSE) {
  checkmate::assert_logical(ref_lbls, any.missing = FALSE)

  res <- c(control_surv_med_annot(x = x, y = y, w = w, h = h), list(ref_lbls = ref_lbls))
  res
}

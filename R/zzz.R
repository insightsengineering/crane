.onAttach <- function(...) {
  attached <- crane_attach()
  if (!is_loading_for_tests()) {
    inform_startup(crane_attach_message(attached))
  }

  if (!is_attached("conflicted") && !is_loading_for_tests()) {
    conflicts <- crane_conflicts()
    if (!rlang::is_empty(conflicts)) {
      inform_startup(crane_conflict_message(conflicts))
    }
  }

  if (!is_loading_for_tests()) {
    inform_startup(crane_theme_message())
  }
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

is_loading_for_tests <- function() {
  !interactive() && identical(Sys.getenv("DEVTOOLS_LOAD"), "crane")
}

# if no packages, shows nothing

    Code
      cat(crane_attach_message(character()))

# message lists all attached packages

    Code
      cat(crane_attach_message(c("crane", "gtsummary", "cards", "cardx")))
    Output
      -- Attaching packages ----------------------------------------------------------
      v cards     1.0.0     v crane     1.0.0
      v cardx     1.0.0     v gtsummary 1.0.0

# highlights dev versions in red

    Code
      highlight_version(c("1.0.0", "1.0.0.9000", "0.9000.0.9000", "1.0.0-rc"))
    Output
      [1] "1.0.0"                                        
      [2] "1.0.0.\033[31m9000\033[39m"                   
      [3] "0.\033[31m9000\033[39m.0.\033[31m9000\033[39m"
      [4] "1.0.0-rc"                                     

# useful conflicts message

    Code
      crane_conflicts(c("testthat", "gtsummary"))
    Output
      -- Conflicts -------------------------------------------------------------------
      x crane::is_false() masks testthat::is_false()
      x crane::is_null()  masks testthat::is_null()
      x crane::is_true()  masks testthat::is_true()
      x crane::matches()  masks gtsummary::matches(), testthat::matches()
      i Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors


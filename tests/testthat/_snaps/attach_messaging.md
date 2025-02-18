# if no packages, shows nothing

    Code
      cat(crane_attach_message(character()))

# message lists all attached packages

    Code
      cat(crane_attach_message(c("crane", "gtsummary")))
    Output
      -- Attaching packages ----------------------------------------------------------
      v crane     1.0.0     v gtsummary 1.0.0

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
      crane_conflicts(c("base", "stats"))
    Output
      -- Conflicts -------------------------------------------------------------------
      x crane::library.dynam()        masks base::library.dynam()
      x crane::library.dynam.unload() masks base::library.dynam.unload()
      x crane::system.file()          masks base::system.file()
      i Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors


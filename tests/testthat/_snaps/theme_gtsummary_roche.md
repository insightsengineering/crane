# theme_gtsummary_roche() works

    Code
      gtsummary::check_gtsummary_theme(theme_gtsummary_roche(set_theme = FALSE))
    Message
      v Looks good!

# theme_gtsummary_roche() adds relevant {flextable} directives

    Code
      added_cmds_roche_specific
    Output
      $user_added1
      $user_added1[[1]]
      flextable::fontsize(size = 8, part = "all")
      
      $user_added1[[1]]
      flextable::fontsize(size = 8 - 1, part = "footer")
      
      $user_added2
      $user_added2[[1]]
      flextable::border_outer(part = "body", border = list(width = 0.5, 
          color = "#666666", style = "solid"))

      $user_added2[[2]]
      flextable::border_outer(part = "header", border = list(width = 0.5, 
          color = "#666666", style = "solid"))
      
      $user_added3
      $user_added3[[1]]
      flextable::valign(valign = "top", part = "all")
      
      $user_added3[[2]]
      flextable::font(fontname = "Arial", part = "all")
      
      $user_added3[[2]]
      flextable::padding(padding.top = 0, part = "all")
      
      $user_added3[[3]]
      flextable::padding(padding.bottom = 0, part = "all")
      
      $user_added3[[4]]
      flextable::line_spacing(space = 1, part = "all")
      
      $user_added3[[5]]
      flextable::set_table_properties(layout = "autofit")
      

# theme_gtsummary_roche() works

    Code
      gtsummary::check_gtsummary_theme(theme_gtsummary_roche(set_theme = FALSE))
    Message
      v Looks good!

# theme_gtsummary_roche() adds relevant {flextable} directives

    Code
      added_cmds_roche_specific
    Output
      $user_added2
      flextable::fontsize(size = 8 - 1, part = "footer")
      
      $user_added4
      flextable::border_outer(part = "header", border = list(width = 0.5, 
          color = "#666666", style = "solid"))
      
      $user_added3
      flextable::border_outer(part = "body", border = list(width = 0.5, 
          color = "#666666", style = "solid"))
      
      $user_added7
      flextable::line_spacing(space = 1, part = "all")
      
      $user_added6
      flextable::font(fontname = "Arial", part = "all")
      
      $user_added5
      flextable::valign(valign = "top", part = "all")
      


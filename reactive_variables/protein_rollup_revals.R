#' @details holder for the various conditions for bpquant to be valid.  Used in
#' The rollup UI to display a message if not all conditions are met, and in
#' rollup observers to disable the button.
#' 
bpquant_valid <- reactive({
      pro_class <- inherits(objects$omicsData, "proData")
      
      if (pro_class) {
        data <- objects$omicsData_pre_rollup
      } else {
        data <- objects$omicsData
      }
      
      stats <- objects$peptide_imdanova_res
      group_info <- pmartR:::get_group_DF(data)
      comp_info <- attr(stats, "comparisons")
      
      l1 <- nrow(unique(data$f_data[attr(group_info, "main_effects")]))
      l2 <- if(!is.null(attr(group_info, "covariates"))) {
        attr(group_info, "covariates") %>% 
          dplyr::select(-one_of(get_fdata_cname(data))) %>% 
          nrow()
      } else 0
      
      if (l1 == 1) l1 <- 0 # Not valid if it doesn't actually have multiple levels
      if (l2 == 1) l2 <- 0 # Not valid if it doesn't actually have multiple levels
      
      cond1 <- l1 + l2 < 3
      cond2 <- is.null(stats)
      if (is.null(nrow(comp_info))) {
        cond3 <- cond2 || length(comp_info) < 3
      } else {
        cond3 <- cond2 || nrow(comp_info) < 3
      }
      
      #' ANOVA must have been run on stats.  Kind of a janky way of determining 
      #' it, we just expect the columns 'P_value_A_<comparison>' to be in the 
      #' column names.
      expected_anova_cols <- paste0("P_value_A_", attr(stats, "comparisons"))
      has_anova_cols <- all(expected_anova_cols %in% colnames(stats))
      
      # makes a full "imd-anova was properly run" condition
      cond2 <- cond2 | !has_anova_cols 
      
      #' Checking if g-test has been run
      expected_gtest_cols <- paste0("P_value_G_", attr(stats, "comparisons"))
      has_gtest_cols <- all(expected_gtest_cols %in% colnames(stats))
      
      anova_only <- has_anova_cols & !has_gtest_cols
      
      # If ANOVA only is run, then all data must have valid comparisons
      cond4 = if(anova_only) {
        any(is.na(stats[expected_anova_cols]))
      } else F
      
      return(
        list(
          'cond1' = cond1,
          'cond2' = cond2,
          'cond3' = cond3,
          'cond4' = cond4,
          "has_anova_cols" = has_anova_cols,
          "has_gtest_cols" = has_gtest_cols,
          "anova_only" = anova_only
        )
      )
})
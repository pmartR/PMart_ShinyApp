# absolute pain to have to do this for two lipid objects
inspect_norm <- function(omicsData, subset_fn, norm_fn, params) {
  # align groups in group_DF with column names in e_data (otherwise kruskal-wallis test is sad)
  group_df <- attr(omicsData, "group_DF")
  reorder <- match(colnames(omicsData$e_data)[-which(colnames(omicsData$e_data) == get_edata_cname(omicsData))], as.character(group_df[, get_fdata_cname(omicsData)]))
  group <- group_df[reorder, ]$Group

  # create norm object and pull normalization parameters
  norm_object <- normalize_global(omicsData, subset_fn, norm_fn, params = params, apply_norm = FALSE)
  params <- norm_object$parameters$normalization

  # p value and dataframe of normalization factors for location
  p_location <- pmartR:::kw_rcpp(matrix(params$location, nrow = 1), group = as.character(group))
  loc_params <- stack(params$location) %>%
    rename("VAL__" = values) # very possible someone has the column name 'values' in their data, so rename

  # p value and dataframe of normalization factors for scale, if there are scale parameters
  if (!is.null(params$scale)) {
    p_scale <- pmartR:::kw_rcpp(matrix(params$scale, nrow = 1), group = as.character(group))
    scale_params <- stack(params$scale) %>%
      rename("VAL__" = values)
  }
  else {
    p_scale <- NULL
  }

  #### Create Plots ####

  # plot by group if the object has group assignments
  if (!is.null(attributes(omicsData)$group_DF)) {
    # dataframe with group information
    loc_df <- attributes(omicsData)$group_DF %>% 
      left_join(loc_params, by = setNames("ind", get_fdata_cname(omicsData)))
    # 'x' at the location parameter for each sample
    append_locs <- geom_point(data = loc_df, 
                              aes(x = !!rlang::sym(get_fdata_cname(omicsData)), 
                                  y = VAL__, size = 7), 
                              shape = 7, color = "red")
    # boxplots of location parameters by group
    loc_boxplot <- loc_df %>% ggplot() +
      geom_boxplot(aes(x = Group, y = VAL__, fill = Group)) +
      ylab("Location Parameter Value") +
      ggtitle("Location parameter values for chosen normalization, by group") +
      theme_bw()

    # same as above but for scale
    if (!is.null(params$scale)) {
      scale_df <- attributes(omicsData)$group_DF %>% left_join(scale_params, by = setNames("ind", get_fdata_cname(omicsData)))
      scale_boxplot <- scale_df %>% ggplot() +
        geom_boxplot(aes(x = Group, y = VAL__, fill = Group)) +
        ylab("Scale Parameter Value") +
        ggtitle("Scale parameter values for chosen normalization, by group") + 
        theme_bw()
    }
    else {
      scale_boxplot <- NULL
    }
  }
  # if no group df simply make the location and scale dataframes and erase the normalization factor boxplots
  else {
    loc_df <- omicsData$f_data %>% left_join(loc_params, by = setNames("ind", get_fdata_cname(omicsData)))
    append_locs <- geom_point(data = loc_df, aes(x = !!rlang::sym(get_fdata_cname(omicsData)), y = VAL__, size = 7), shape = 7, color = "red")

    # these are irrelevant if theres no grouping structure
    loc_boxplot <- scale_boxplot <- NULL

    if (!is.null(params$scale)) {
      scale_df <- omicsData$f_data %>% left_join(scale_params, by = setNames("ind", get_fdata_cname(omicsData)))
    }
  }

  # to draw a separate symbol for the location params
  loc_legend <- scale_size_continuous(name = "Location Parameters", labels = "")

  # store a plot list in reactive variable

  p <- plot(norm_object, color_by = "Group", order_by = "Group") #### Remove legend on left plot when combined

  # norm_modal_ba_plots <- list(p[[1]] + append_locs + loc_legend, p[[2]])        ###################### New change with updates
  norm_modal_ba_plots <- list(p)

  return(list(p_location = p_location, p_scale = p_scale, loc_boxplot = loc_boxplot, scale_boxplot = scale_boxplot, norm_modal_ba_plots = norm_modal_ba_plots))
}

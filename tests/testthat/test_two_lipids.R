library(shinytest2)

orig_envvar = Sys.getenv("MAP_VERSION")
Sys.setenv("MAP_VERSION"=0)
on.exit({Sys.setenv("MAP_VERSION" = orig_envvar)})

test_that("{shinytest2} recording: pmart_standalone", {
    # app <- AppDriver$new("http://127.0.0.1:3858", name = "pmart_standalone", height = 1187, width = 1263) 
    app <- AppDriver$new(name = "pmart_standalone", variant = platform_variant(), height = 1187, width = 1263, seed = 5237, timeout = 15000)
    app$set_inputs(top_page = "upload_data_tab")
    app$set_inputs(datatype = "lip")
    app$set_inputs(upload_collapse_left = "datselect")
    app$wait_for_idle()
    app$upload_file(file_edata = file.path(testthat::test_path(), "../../example_data/test_lipid_pos_edata.csv"))
    app$upload_file(file_edata_2 = file.path(testthat::test_path(), "../../example_data/test_lipid_neg_edata.csv"))
    
    app$wait_for_value(input = "transform")
    app$set_inputs(transform = "log2")
    app$set_inputs(na_symbol = "0")
    app$click("done_idcols")

    app$set_inputs(emeta_yn = "FALSE")
    app$click("makeobject")
    
    app$wait_for_value(input="goto_groups")
    app$click("goto_groups")
    
    app$wait_for_value(output = "fdata_UI")
    app$upload_file(file_fdata = file.path(testthat::test_path(), "../../example_data/test_lipid_fdata.csv"))
    
    app$wait_for_value(input = "gcol1")
    app$set_inputs(gcol1 = "Condition1")
    app$click("group_designation")
    
    app$wait_for_value(input = "goto_qc")
    app$click("goto_qc")
    
    app$wait_for_idle()
    app$set_inputs(qc_order_by = "Condition1")
    app$set_inputs(qc_color_by = "Condition2")
    app$click("qc_redraw_plot")
    app$wait_for_idle()

    app$set_inputs(qc_order_by_2 = "Group")
    app$set_inputs(qc_color_by_2 = "Condition2")
    app$click("qc_redraw_plot")
    app$wait_for_idle()

    app$set_inputs(qc_xlab = "Sampname")
    app$set_inputs(qc_ylab = "log2abund")
    app$set_inputs(qc_title = "New Title")
    app$click("qc_apply_style_plot_1")
    app$click("qc_apply_style_plot_2")

    app$set_inputs(which_qc_plot = "pca")
    app$set_inputs(qc_color_by_2 = "Group")
    
    app$click("go_to_filter")
    
    app$wait_for_value(input = "cv_threshold")
    app$set_inputs(cv_threshold = 50)
    app$click("add_cvfilt")
    
    app$run_js(open_collapse("filter_collapse", "customfilt"))
    
    app$wait_for_value(output = "fdata_customfilt")
    
    app$set_inputs(fdata_customfilt_choices = "Project_LipidPos_A_Y_1")
    app$set_inputs(fdata_customfilt_choices = c("Project_LipidPos_A_Y_1", "Project_LipidPos_A_Y_2"))
    
    app$set_inputs(fdata_customfilt_choices_2 = "Project_LipidPos_B_Y_1")
    app$set_inputs(fdata_customfilt_choices_2 = c("Project_LipidPos_B_Y_1", "Project_LipidPos_B_Y_2"))
    
    app$click("add_customfilt")
    app$click("review_filters")
    
    app$wait_for_value(input = "apply_filters")
    app$click("apply_filters")
    
    app$wait_for_value(input = "goto_norm")
    app$click("goto_norm")
    
    app$set_inputs(subset_fn = "ppp")
    app$set_inputs(norm_fn = "median")
    app$click("inspect_norm")
    app$wait_for_value(input = "apply_normalization_modal", timeout = 20000)
    app$click("apply_normalization_modal")

    combined_normed_lipids <- app$get_value(export = "omicsData_norm")
    prenorm <- app$get_value(export = "omicsData_prenorm")

    expect_equal(dim(combined_normed_lipids$e_data), c(384, 15))
    expect_true(length(attr(prenorm, "filters")) < length(attr(combined_normed_lipids, "filters")))

    ftypes_prenorm = sapply(attr(prenorm, "filters"), function(x) x$type)
    ftypes_postnorm = sapply(attr(combined_normed_lipids, "filters"), function(x) x$type)
    expect_true(
      all(ftypes_prenorm %in% ftypes_postnorm)
    )
    
    app$wait_for_idle()
    app$click("goto_statistics")
    
    app$wait_for_idle()
    app$set_inputs(stats_select_method = "imdanova")
    app$set_inputs(comparison_method = "All pairwise comparisons")
    app$wait_for_idle()
    app$set_inputs(imdanova_test_method = "combined")
    app$wait_for_idle()
    app$click("apply_imdanova")
    app$wait_for_idle()
    app$click("stats_dismiss")
    app$run_js(open_collapse("statistics_collapse_left", "stats-statistics-options"))
    app$set_inputs("stats_select_method" = "pca")
    app$click("apply_dimreduction")
    app$wait_for_idle()
    app$set_inputs("analysis_pca_shape_by" = "Condition2")
    
    app$set_inputs(top_page = "download_tab")
    
    app$run_js(open_collapse("download_collapse", "Generate Report"))
    report_name = app$get_value(input = "ReportName")
    fs <- app$get_download("ReportDownload")
    
    expect_true(basename(fs) == paste0(report_name, ".html"))
})

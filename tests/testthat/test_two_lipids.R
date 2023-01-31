library(shinytest2)

test_that("{shinytest2} recording: pmart_standalone", {
    # app <- AppDriver$new("http://127.0.0.1:3858", name = "pmart_standalone", height = 1187, width = 1263) 
    app <- AppDriver$new(name = "pmart_standalone", variant = platform_variant(), height = 1187, width = 1263, timeout = 10000, clean_logs=FALSE)
    app$set_inputs(datatype = "lip")
    app$set_inputs(upload_collapse_left = "datselect")
    app$upload_file(file_edata = file.path(testthat::test_path(), "../../example_data/lipid_edata_pos.csv"))
    app$upload_file(file_edata_2 = file.path(testthat::test_path(), "../../example_data/lipid_edata_neg.csv"))
    
    app$wait_for_value(input = "transform")
    app$set_inputs(transform = "log2")
    app$set_inputs(na_symbol = "0")
    app$click("done_idcols")

    app$set_inputs(emeta_yn = "FALSE")
    app$click("makeobject")
    
    app$wait_for_value(input="goto_groups")
    app$click("goto_groups")
    
    app$wait_for_value(output = "fdata_UI")
    app$upload_file(file_fdata = file.path(testthat::test_path(), "../../example_data/fdata_lipids.csv"))
    
    app$wait_for_value(input = "gcol1")
    app$set_inputs(gcol1 = "Condition1")
    app$click("group_designation")
    
    app$wait_for_value(input = "goto_qc")
    app$click("goto_qc")

    app$set_inputs(top_page = "filter_tab")
    
    app$wait_for_value(input = "cv_threshold")
    app$set_inputs(cv_threshold = 50)
    app$click("add_cvfilt")
    
    app$run_js(open_collapse("filter_collapse", "customfilt"))
    
    app$wait_for_value(output = "fdata_customfilt")
    
    app$set_inputs(fdata_customfilt_choices = "Project_LipidPos_A_Y_1")
    app$set_inputs(fdata_customfilt_choices = c("Project_LipidPos_A_Y_1", "Project_LipidPos_A_Y_2"))
    
    app$set_inputs(fdata_customfilt_choices_2 = "Project_LipidNeg_B_Y_1")
    app$set_inputs(fdata_customfilt_choices_2 = c("Project_LipidNeg_B_Y_1", "Project_LipidNeg_B_Y_2"))
    
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
    app$click("goto_downloads")
    
    app$run_js(open_collapse("download_collapse", "Generate Report"))
    report_name = app$get_value(input = "ReportName")
    fs <- app$get_download("ReportDownload")
    
    expect_true(basename(fs) == paste0(report_name, ".html"))
})

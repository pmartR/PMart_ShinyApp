library(shinytest2)

APP_PATH = switch(
  Sys.getenv("MAP_SHINYTEST"),
  "2" = "http://localhost:8301",
  testthat::test_path("../..")
)

test_that("{shinytest2} recording: pmart_standalone", {
  # app <- AppDriver$new("http://127.0.0.1:3858", name = "pmart_standalone", height = 1187, width = 1263) 
  app <- AppDriver$new(APP_PATH, name = "pmart_standalone", variant = platform_variant(), height = 1187, width = 1263, seed = 71444, load_timeout = 20000)
  app$set_inputs(top_page = "upload_data_tab")
  app$wait_for_idle(timeout = 60000)
  
  # Upload
  app$set_inputs(datatype = "seq")
  app$upload_file(file_edata = file.path(testthat::test_path(), "../../example_data/test_rnaseq_edata.csv"))
  
  app$wait_for_idle()
  app$click("done_idcols")
  
  app$wait_for_idle()
  app$set_inputs(emeta_yn = "FALSE")
  app$click("makeobject")
  
  #app$run_js(open_collapse("filter_collapse", "customfilt"))
  
  app$wait_for_idle(timeout = 50000)
  
  app$click("goto_groups")
  
  # Groups
  app$wait_for_idle()
  app$upload_file(file_fdata = file.path(testthat::test_path(), "../../example_data/test_rnaseq_fdata.csv"))
  
  app$wait_for_idle()
  app$set_inputs(fdata_id_col = "Samples")
  app$set_inputs(gcol1 = "Treatment")
  app$set_inputs(gcol2 = "Tissue")
  
  app$click("group_designation")
  app$wait_for_idle()

  app$click("goto_filter")
  
  # Filter
  app$wait_for_idle()
  app$click("add_molfilt")
  app$click("add_tcfilt")
  app$click("review_filters")
  
  app$wait_for_idle()
  app$click("apply_filters")
  app$click("goto_stats_filter")
  
  # Stats
  app$wait_for_idle()
  app$set_inputs(stats_select_method = "DESeq2")
  app$set_inputs(comparison_method = "all_pairwise")
  
  app$click("apply_diagnostic")
  app$click("apply_seqstats")
  
  app$wait_for_idle(timeout=80000)
  app$click("stats_dismiss")
  
  app$run_js(open_collapse("statistics_collapse_left", "stats-statistics-options"))
  app$set_inputs(stats_select_method = "voom")
  app$set_inputs(comparison_method = "all_pairwise")
  app$click("apply_diagnostic")
  app$click("apply_seqstats")
  
  app$wait_for_idle(timeout=20000)
  app$click("stats_dismiss")

  app$run_js(open_collapse("statistics_collapse_left", "stats-statistics-options"))
  app$set_inputs(stats_select_method = "edgeR")
  app$set_inputs(comparison_method = "all_pairwise")
  
  app$click("apply_diagnostic")
  app$click("apply_seqstats")
  app$wait_for_idle(timeout=20000)
  
  app$wait_for_value(input = "stats_dismiss")
  app$click("stats_dismiss")
  
  app$run_js(open_collapse("statistics_collapse_left", "stats-statistics-options"))
  app$set_inputs("stats_select_method" = "glmpca")
  app$wait_for_idle()
  app$click("apply_dimreduction")
  app$wait_for_idle(timeout=60000)
  app$set_inputs("analysis_pca_color_by" = "Tissue")
  app$set_inputs("analysis_pca_shape_by" = "Group")
  app$click("pca_update_plot_content")
  
  app$set_inputs(top_page = "download_tab")
  
  app$run_js(open_collapse("download_collapse", "Generate Report"))
  report_name = app$get_value(input = "ReportName")
  fs <- app$get_download("ReportDownload")
  
  expect_true(basename(fs) == paste0(report_name, ".html"))
})

context("basic workflow - peptides")
library(shinytest2)

orig_envvar = Sys.getenv("MAP_VERSION")
Sys.setenv("MAP_VERSION"=0)
on.exit({Sys.setenv("MAP_VERSION" = orig_envvar)})

test_that("App completes a basic workflow on peptide-level data", {
  # To test with the a browser run something like the following with a
  # shiny app running in a different R session and paste its address into
  # the first argument:
  # app <- AppDriver$new("http://127.0.0.1:6608", name = "pmart_standalone", height = 1199, width = 1299, variant = platform_variant(), timeout = 10000, seed = 551313)
  
  app <- AppDriver$new(name = "pmart_standalone", height = 1199, width = 1299, variant = platform_variant(), timeout = 10000, seed = 551313)
  app$set_inputs(datatype = "pep")
  app$set_inputs(upload_collapse_left = "datselect")
  app$upload_file(file_edata = file.path(testthat::test_path(), "../../example_data/test_pep_edata.csv"))
  
  app$set_inputs(upload_collapse_left = "columnids")
  app$wait_for_value(input = "transform")
  app$set_inputs(transform = "log2")
  
  app$wait_for_value(input = "done_idcols")
  app$click("done_idcols")
  app$wait_for_value(input = "emeta_yn")
  app$upload_file(file_emeta = file.path(testthat::test_path(), "../../example_data/test_pep_emeta.csv"))
  app$set_inputs(protein_column = "Protein")
  
  app$click("makeobject")
  
  app$wait_for_value(input = "goto_groups")
  app$click("goto_groups")
  
  # Groups tab
  app$wait_for_value(input = "usevizsampnames")
  app$upload_file(file_fdata = file.path(testthat::test_path(), "../../example_data/test_pep_fdata.csv"))
  app$set_inputs(groups_collapse_left = "fdata_columns")
  app$set_inputs(groups_collapse_right = "fdata_preview")
  
  app$wait_for_value(input = "gcol1")
  app$set_inputs(gcol1 = "Condition1")
  
  app$click("group_designation")
  app$wait_for_value(input = "goto_qc")
  app$click("goto_qc")

  # QC tab
  app$wait_for_value(input = "qc_order_by")
  app$set_inputs(qc_order_by = "Condition1")
  app$set_inputs(qc_color_by = "Condition2")
  app$set_inputs(qc_xlab = "Sampname")
  app$set_inputs(qc_ylab = "log2abund")
  app$set_inputs(qc_title = "New Title")
  app$click("qc_apply_style_plot_1")
  app$click("saveplot")
  
  saved_plot <- app$get_value(export = "saved_plot_data")
  
  # TODO:  Find some way to consitently test parts of a plot
  # expect_equal(digest::digest(saved_plot), "2facd08eb8de3b8f685d80631e2a26b2")
  
  # Filter tab
  app$set_inputs(top_page = "filter_tab")
  app$click("add_molfilt")
  app$click("add_cvfilt")
  app$click("add_imdanovafilt")
  app$click("add_profilt")
  
  app$run_js(open_collapse("filter_collapse", "sample_filters"))
  app$wait_for_value(input = "plot_rmdfilt")
  app$click("plot_rmdfilt")
  app$click("add_rmdfilt")
  
  app$run_js(open_collapse("filter_collapse", "customfilt"))
  app$wait_for_value(output = "fdata_customfilt")
  app$set_inputs(fdata_customfilt_choices = "Project_C_X_3")
  app$set_inputs(emeta_customfilt_which_values_1 = "1110")
  app$set_inputs(emeta_customfilt_which_values_1 = c("1110", "1192"))
  app$set_inputs(emeta_customfilt_which_values_1 = c("1110", "1192", "1201"))
  app$set_inputs(emeta_customfilt_which_values_1 = c("1110", "1192", "1201", "1205"))
  app$click("add_customfilt")
  
  app$click("review_filters")
  app$wait_for_value(input = "apply_filters")
  app$click("apply_filters")
  app$wait_for_value(input = "filter_dismiss")
  app$click("filter_dismiss")
  
  filters <- app$get_value(export = "filters")
  filter_classes <- sapply(filters, class)[1,]
  omicsData <- app$get_value(export = "omicsData")
  omicsData_filt <- app$get_value(export = "omicsData_filt")
  
  # TODO:  Actually re-run the filters and check equality
  expect_setequal(filter_classes, c("moleculeFilt", "cvFilt", "imdanovaFilt", "proteomicsFilt", "rmdFilt", "customFilt"))
  expect_equal(dim(omicsData$e_data), c(4481, 19))
  expect_equal(dim(omicsData_filt$e_data), c(2106, 17))

  app$set_inputs(top_page = "normalization_tab")
  
  # Normalization
  app$wait_for_value(input = "spans_or_manual")
  app$set_inputs(spans_or_manual = "spans")
  Sys.sleep(1)
  app$run_js(open_collapse("spans_submenu", "use_spans"))
  app$wait_for_value(output = "spans_conditionalbuttons")
  
  app$set_inputs(spans_which_subset_fn = c("all", "los", "complete", "rip", "ppp_rip"))
  app$set_inputs(spans_which_subset_fn = c("all", "los", "rip", "ppp_rip"))
  app$set_inputs(spans_which_subset_fn = c("all", "los", "ppp_rip"))
  app$set_inputs(spans_which_subset_fn = c("los", "ppp_rip"))
  app$set_inputs(spans_which_norm_fn = c("mean", "zscore", "mad"))
  app$set_inputs(spans_which_norm_fn = c("mean", "mad"))
  
  app$wait_for_value(input = "execute_spans")
  app$click("execute_spans")
  
  # params panel opened by default
  app$wait_for_idle(timeout = 50000)
  app$wait_for_value(output = "spans_table")
  app$set_inputs(
    spans_table_rows_selected = 1,
    spans_table_row_last_clicked = 1,
    allow_no_input_binding_ = TRUE
  )
  
  app$click("use_selected_spans")
  app$click("apply_normalization")
  
  app$wait_for_value(input = "normalization_dismiss")
  app$click("normalization_dismiss")
  
  omicsData_norm = app$get_value(export = "omicsData_norm")
  norm_info <- attributes(omicsData_norm)$data_info$norm_info
  
  expect_true(norm_info$is_normalized)
  expect_equal(norm_info$norm_type, "global")
  expect_equal(norm_info$subset_fn, "los")
  expect_equal(norm_info$subset_params$los, 0.2)
  expect_equal(norm_info$norm_fn, "mean")
  expect_equal(norm_info$n_features_calc, 637)
  
  expect_equal(
    unname(norm_info$params$norm_location), 
    c(
      23.6913243579832,
      23.5876540826849,
      23.4985959037066,
      23.6045605299241,
      23.4263876220424,
      23.6362881425376,
      23.4956245648087,
      23.7881275415636,
      23.6988180128446,
      23.7755521745674,
      23.502880708832,
      23.6618744044926,
      23.3067293384951,
      23.6220384305414,
      23.6906529147068,
      23.6815242130187
    )
  )
  
  app$set_inputs(top_page = "peptide_statistics_tab")
  
  # Peptide Statistics
  app$wait_for_js("$('#peptide_stats_select_method')[0]")
  app$set_inputs(peptide_stats_select_method = "imdanova")
  app$set_inputs(peptide_comparison_method = "All pairwise comparisons")
  app$wait_for_value(input = "imd_comparison_button_1")
  app$click("imd_comparison_button_1")
  app$wait_for_value(input = "imd_comparison_button_2")
  app$click("imd_comparison_button_2")
  app$wait_for_value(input = "imd_comparison_button_3")
  app$click("imd_comparison_button_3")
  app$set_inputs(peptide_imdanova_test_method = "combined")
  app$click("peptide_apply_imdanova")

  app$wait_for_value(input = "pepstats_dismiss")
  app$click("pepstats_dismiss")
  
  app$set_inputs(peptide_imdanova_plot_type = "volcano")
  app$set_inputs(peptide_imdanova_plot_type = "gheatmap")
  
  app$set_inputs(top_page = "protein_rollup_tab")
  app$run_js(open_collapse("rollup_sidebar", "isoform_identification"))

  app$set_inputs(bpquant_comps = "B vs A")
  app$set_inputs(bpquant_comps = c("B vs A", "C vs A"))
  app$set_inputs(bpquant_comps = c("B vs A", "C vs A", "B vs C"))
  app$set_inputs(bpquant_max_prot = 4)
  app$set_inputs(bpquant_lock = TRUE)
  app$click("bpquant")

  app$set_inputs(bpquant_apply = "No")
  app$set_inputs(which_combine_fn = "mean")
  
  app$click("apply_rollup")
  app$wait_for_value(input = "rollup_goto_stats")
  app$click("rollup_goto_stats")

  # Statistics
  app$wait_for_js("$('#stats_select_method')[0]")
  app$set_inputs(stats_select_method = "imdanova")
  app$set_inputs(comparison_method = "All pairwise comparisons")
  app$wait_for_value(output = "pairwise_comp_display")
  app$click("imd_comparison_button_1")
  app$wait_for_value(input = "imd_comparison_button_2")
  app$click("imd_comparison_button_2")
  app$wait_for_value(input = "imd_comparison_button_3")
  app$click("imd_comparison_button_3")
  app$set_inputs(imdanova_test_method = "combined")
  app$click("apply_imdanova")
  
  app$wait_for_value(input = "stats_dismiss")
  app$click("stats_dismiss")
  
  # Download
  app$set_inputs(top_page = "download_tab")
  app$click("makezipfile")
  
  # report download
  app$run_js(open_collapse("download_collapse", "Generate Report"))
  report_name = app$get_value(input = "ReportName")
  fs <- app$get_download("ReportDownload")
  
  expect_true(basename(fs) == paste0(report_name, ".html"))
})

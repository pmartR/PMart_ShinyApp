library(shinytest2)
library(mapDataAccess)

test_that("pmartR loads MAP projects", {
  if (is.null(Sys.getenv("MAP_VERSION")) || Sys.getenv("MAP_VERSION") < 1)
    skip("MAP not enabled")
  
  setwd("../..")
  
  if (Sys.getenv("MAP_SHINYTEST") != "1") {
    cfg_path = if(isTruthy(Sys.getenv("MAP_CONFIG"))) Sys.getenv("MAP_CONFIG")
    else "./cfg/minio_config.yml"
    
    if (!file.exists(cfg_path)) {
      fail(paste0("Could not find MAP config file (", cfg_path, ")"))
    }
  }
  
  tryCatch(
    {
      if (Sys.getenv("MAP_SHINYTEST") == "1") {
        MapConnect <- map_data_connection()
      } else {
        MapConnect <- map_data_connection(cfg_path)
      }
    }, 
    error = function(cond) {
      if (Sys.getenv("MAP_SHINYTEST") == "1") {
        fail(paste("Failed to connect to MAP.",
                   "Please restart your R session and try again."))
      } else {
        fail(paste0("Failed to connect to MAP. Please ensure your config file",
                    " (", cfg_path, ")",
                    " is properly configured and the minio server is running."))
      }
    }
  )
  
  # All object names used throughout the test should go here
  map_data_ids <- c(
    "test_map_project_01"
  )
  
  for (data_id in map_data_ids)
    tryCatch(
      {
        get_data(MapConnect, data_id)
      },
      error = function(cond) {
        fail(paste0("Could not download data ", data_id, " from MAP. ",
                    "Please ensure the MAP bucket contains this data.\n",
                    "The data can be found in tests/MAP_test_data."))
      }
    )
  
  setwd("tests/testthat")
  
  Sys.setenv("MAP_VERSION"=1)
  
  Sys.setenv("SHINYTEST_LOAD_MAP_OBJECT" = map_data_ids[1])
  set.seed(314159265)
  app <- AppDriver$new(name = "pmart_standalone", variant = platform_variant(),
                       height = 1187, width = 1263, wait = FALSE,
                       timeout = 15000)
  
  app$wait_for_idle(timeout = 60000)
  
  expect_equal(app$get_value(input = "top_page"), "upload_data_tab")
  
  app$set_inputs(transform = "log2")
  app$click("done_idcols")
  
  app$wait_for_idle()
  
  app$set_inputs(upload_collapse_left = "meta_collapse")
  
  app$wait_for_idle()
  
  app$set_inputs(protein_column = "ProteinList")
  app$click("makeobject")
  
  app$wait_for_idle(timeout = 60000)
  
  app$click("goto_groups")
  
  app$wait_for_idle()
  
  app$set_inputs(gcol1 = "Phenotype")
  app$set_inputs(gcol2 = "SecondPhenotype")
  app$click("group_designation")
  
  app$wait_for_idle()
  
  app$click("goto_filter")
  
  app$wait_for_idle()
  
  app$click("add_molfilt")
  app$click("add_cvfilt")
  app$click("review_filters")
  
  app$wait_for_idle()
  app$click("apply_filters")
  app$click("filter_dismiss")
  app$click("goto_norm")

  app$wait_for_idle()
  app$set_inputs(normalization_sidebar = c("normalize_global_sidebar", "normalize_global_sidebar"))
  app$set_inputs(spans_submenu = "choose_params")
  app$set_inputs(subset_fn = "rip")
  app$click("inspect_norm")
  app$wait_for_idle(timeout = 60000)
  app$set_inputs(norm_modal_plot_select = "ba")
  app$click("apply_normalization_modal")
  
  app$wait_for_idle(timeout = 60000)
  app$click("normalization_dismiss")
  
  app$wait_for_idle()
  app$set_inputs(top_page = "peptide_statistics_tab")
  app$wait_for_idle()
  app$set_inputs(peptide_stats_select_method = "imdanova")
  app$wait_for_idle()
  app$set_inputs(peptide_comparison_method = "All pairwise comparisons")
  app$wait_for_idle()
  app$set_inputs(peptide_imdanova_test_method = "combined")
  app$wait_for_idle()
  app$click("peptide_apply_imdanova")
  app$click("goto_rollup")
  
  app$wait_for_idle()
  app$click("apply_rollup")
  app$wait_for_idle(timeout = 60000)
  app$click("rollup_goto_stats")
  
  app$wait_for_idle()
  app$set_inputs(stats_select_method = "imdanova")
  app$wait_for_idle()
  app$set_inputs(comparison_method = "All pairwise comparisons")
  app$wait_for_idle()
  app$set_inputs(imdanova_test_method = "combined")
  app$wait_for_idle()
  app$click("apply_imdanova")
  app$wait_for_idle()
  app$click("saveplot")
  imdanova_stats_plot <- app$get_value(export = "cur_plot")
  vdiffr::expect_doppelganger("project_01_imdanova_stats_plot",
                              imdanova_stats_plot)
  app$click("goto_downloads")
})
library(shinytest2)
library(mapDataAccess)

test_that("pmartR loads MAP midpoints", {
  cfg_path = if(isTruthy(Sys.getenv("MAP_CONFIG"))) Sys.getenv("MAP_CONFIG")
             else "./cfg/minio_config.yml"
  
  setwd("../..")

  if (!file.exists(cfg_path)) {
    fail(paste0("Could not find MAP config file (", cfg_path, ")"))
  }
  
  tryCatch(
    {
      MapConnect <- map_data_connection(cfg_path)
    }, 
    error = function(cond) {
      fail(paste0("Failed to connect to MAP. Please ensure your config file (",
                  cfg_path,
                  ") is properly configured and the minio server is running."))
    }
  )
  
  # All object names used throughout the test should go here
  map_data_ids <- c(
    "test_map_midpoint_01",
    "test_map_midpoint_02"
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
  
  expect_equal(app$get_value(input = "top_page"), "statistics_tab")
  
  
  app$set_inputs(stats_select_method = "edgeR")

  app$wait_for_idle()
  
  app$set_inputs(comparison_method = "All pairwise comparisons")

  app$click("apply_diagnostic")

  app$wait_for_idle(timeout = 60000)
  
  app$click("apply_seqstats")

  app$wait_for_idle(timeout = 60000)
  
  app$click("stats_dismiss")
  app$click("goto_downloads")
  
  app$stop()
  
  Sys.setenv("SHINYTEST_LOAD_MAP_OBJECT" = map_data_ids[2])
  set.seed(314159265)
  app <- AppDriver$new(name = "pmart_standalone", variant = platform_variant(),
                       height = 1187, width = 1263, wait = FALSE,
                       timeout = 15000)
  
  app$wait_for_idle(timeout = 60000)
  
  expect_equal(app$get_value(input = "top_page"), "peptide_statistics_tab")
  
  app$set_inputs(peptide_stats_select_method = "imdanova")
  
  app$wait_for_idle()
  
  app$set_inputs(peptide_comparison_method = "All pairwise comparisons")
  
  app$wait_for_idle()
  
  app$set_inputs(peptide_imdanova_test_method = "combined")
  
  app$wait_for_idle()
  
  app$click("peptide_apply_imdanova")
  
  app$wait_for_idle()
  
  app$click("goto_rollup")

  app$wait_for_idle()
  
  app$click("apply_rollup")

  app$wait_for_idle()
  
  app$click("rollup_goto_stats")

  app$wait_for_idle()
  
  app$set_inputs(stats_select_method = "imdanova")
  
  app$wait_for_idle()
  
  app$set_inputs(imdanova_test_method = "combined")

  app$click("apply_imdanova")
  
  app$wait_for_idle()
})
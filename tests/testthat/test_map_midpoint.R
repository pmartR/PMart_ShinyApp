library(shinytest2)
library(mapDataAccess)

test_that("pmartR loads MAP midpoints", {
  if (is.null(Sys.getenv("MAP_SHINYTEST")) || Sys.getenv("MAP_SHINYTEST") != 1)
    skip("MAP testing not enabled. See README for info on how to enable it.")
  
  tryCatch(
    {
      MapConnect <- map_data_connection()
    }, 
    error = function(cond) {
      fail(paste("Failed to connect to MAP.",
                 "Please restart your R session and try again."))
    }
  )
  
  orig_envvar = Sys.getenv("MAP_VERSION")
  Sys.setenv("MAP_VERSION"=1)
  on.exit({Sys.setenv("MAP_VERSION" = orig_envvar)})
  
  # All object names used throughout the test should go here
  map_data_ids <- c(
    "test_map_midpoint_01",
    "test_map_midpoint_02"
  )
  
  for (data_id in map_data_ids) {
    tryCatch(
      {
        get_data(MapConnect, data_id)
      },
      error = function(cond) {
        fail(paste0("Could not download data ", data_id, " from MAP. ",
                    "Please ensure the MAP bucket contains this data.\n",
                    "The data can be found in tests/map_test_data."))
      }
    )
  }
  
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
  app$click("saveplot")
  stats_diagplot <- app$get_value(export = "cur_plot")
  vdiffr::expect_doppelganger("midpoint_01_stats_diagplot", stats_diagplot)
  
  app$click("apply_seqstats")

  app$wait_for_idle(timeout = 60000)
  app$click("saveplot")
  stats_mainplot <- app$get_value(export = "cur_plot")
  vdiffr::expect_doppelganger("midpoint_01_stats_mainplot", stats_mainplot)
  
  Sys.sleep(30)
  
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

  app$wait_for_idle(timeout = 60000)
  
  app$click("rollup_goto_stats")

  app$wait_for_idle()
  
  app$set_inputs(stats_select_method = "imdanova")
  
  app$wait_for_idle()
  
  app$set_inputs(imdanova_test_method = "combined")

  app$click("apply_imdanova")
  
  app$wait_for_idle()
  app$click("saveplot")
  imdanova_stats_plot <- app$get_value(export = "cur_plot")
  vdiffr::expect_doppelganger("midpoint_02_imdanova_stats_plot",
                              imdanova_stats_plot)
})
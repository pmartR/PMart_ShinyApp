library(shinytest2)
library(mapDataAccess)

# ---------------------------------------------------------------------------
# Helper: build a minimal "project omic" list from the example lipid CSVs.
# The class and field layout must match what MAP_Functions.R's startup observer
# expects (project$Project$DataType, project$Data$e_data, etc.).
# ---------------------------------------------------------------------------
.make_lip_project <- function(edata_file, fdata, data_type, root) {
  edata <- read.csv(file.path(root, edata_file), stringsAsFactors = FALSE, check.names = FALSE)
  structure(
    list(
      Project = list(DataType = data_type),
      Data = list(
        e_data           = edata,
        f_data           = fdata,
        e_meta           = NULL,
        e_data_filename  = basename(edata_file),
        f_data_filename  = "test_lipid_fdata.csv",
        e_meta_filename  = NULL
      )
    ),
    class = "project omic"
  )
}

test_that("pmartR loads MAP dual-UUID lipidomics via two-lipid pipeline", {

  if (is.null(Sys.getenv("MAP_SHINYTEST")) || !(Sys.getenv("MAP_SHINYTEST") > 0))
    skip("MAP testing not enabled. See README for info on how to enable it.")

  tryCatch(
    { MapConnect_local <- map_data_connection() },
    error = function(cond) {
      fail(paste("Failed to connect to MAP. Please restart your R session and try again."))
    }
  )

  orig_envvar <- Sys.getenv("MAP_VERSION")
  Sys.setenv("MAP_VERSION" = 1)
  on.exit({ Sys.setenv("MAP_VERSION" = orig_envvar) })

  root <- testthat::test_path("../..")

  # -------------------------------------------------------------------------
  # Build shared f_data for the positive dataset (SampleId = pos sample names,
  # plus condition columns used in the groups tab).
  # -------------------------------------------------------------------------
  fdata_raw <- read.csv(
    file.path(root, "example_data/test_lipid_fdata.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  # Pos fdata: rename SampleID_pos -> SampleId, drop the neg ID column
  pos_fdata <- fdata_raw
  pos_fdata[["SampleId"]] <- pos_fdata[["SampleID_pos"]]
  pos_fdata[["SampleID_pos"]] <- NULL
  pos_fdata[["SampleID_neg"]] <- NULL

  # -------------------------------------------------------------------------
  # Create project omic objects and upload to minio
  # -------------------------------------------------------------------------
  pos_project <- .make_lip_project(
    "example_data/test_lipid_pos_edata.csv", pos_fdata, "Lipidomics-Positive", root
  )
  neg_project <- .make_lip_project(
    "example_data/test_lipid_neg_edata.csv", pos_fdata, "Lipidomics-Negative", root
  )

  uuid_pos <- tryCatch(
    put_data(MapConnect_local, pos_project),
    error = function(e) fail(paste("Could not upload pos lipid test object to MAP:", e$message))
  )
  uuid_neg <- tryCatch(
    put_data(MapConnect_local, neg_project),
    error = function(e) fail(paste("Could not upload neg lipid test object to MAP:", e$message))
  )

  # -------------------------------------------------------------------------
  # Launch the app
  # -------------------------------------------------------------------------
  if (Sys.getenv("MAP_SHINYTEST") == "1") {
    Sys.setenv("SHINYTEST_LOAD_MAP_OBJECT" = paste(uuid_pos, uuid_neg, sep = "&"))
  }

  APP_PATH <- switch(
    Sys.getenv("MAP_SHINYTEST"),
    "2" = sprintf("http://localhost:8300/?data=%s&%s", uuid_pos, uuid_neg),
    testthat::test_path("../..")
  )

  set.seed(314159265)
  app <- AppDriver$new(
    APP_PATH,
    name         = "pmart_standalone",
    variant      = platform_variant(),
    height       = 1187, width = 1263,
    wait         = FALSE,
    timeout      = 15000,
    load_timeout = 30000
  )

  app$wait_for_idle(timeout = 60000)

  # -------------------------------------------------------------------------
  # Verify the dual-UUID MAP startup: both datasets loaded, two-lipid mode on
  # -------------------------------------------------------------------------
  expect_equal(app$get_value(input = "top_page"), "upload_data_tab")
  expect_equal(app$get_value(input = "datatype"),  "lip")
  expect_equal(app$get_value(input = "twolipids_yn"), "TRUE")

  # Both file inputs should show the loaded filenames (text inputs in MAP mode)
  expect_match(app$get_value(input = "file_edata"),   "test_lipid_pos_edata.csv")
  expect_match(app$get_value(input = "file_edata_2"), "test_lipid_neg_edata.csv")

  # -------------------------------------------------------------------------
  # Complete the upload step
  # -------------------------------------------------------------------------
  app$set_inputs(transform  = "log2")
  app$set_inputs(na_symbol  = "0")
  # emeta_yn irrelevant for two_lipids; emeta files are NULL in both projects
  app$click("done_idcols")

  app$wait_for_idle()
  app$click("makeobject")
  app$wait_for_idle(timeout = 60000)

  # Both omicsData objects must be non-null after makeobject
  omics1 <- app$get_value(export = "omicsData")
  omics2 <- app$get_value(export = "omicsData_2")
  expect_false(is.null(omics1), label = "omicsData (pos) should not be null")
  expect_false(is.null(omics2), label = "omicsData_2 (neg) should not be null")

  app$click("goto_groups")
  app$wait_for_idle()

  # -------------------------------------------------------------------------
  # Groups tab: assign groups from pos fdata columns
  # -------------------------------------------------------------------------
  app$set_inputs(gcol1 = "Condition1")
  app$click("group_designation")
  app$wait_for_idle()

  # -------------------------------------------------------------------------
  # Normalization: ppp/median, then confirm both datasets are combined
  # -------------------------------------------------------------------------
  app$click("goto_norm")
  app$wait_for_idle()

  app$set_inputs(subset_fn = "ppp")
  app$set_inputs(norm_fn   = "median")
  app$click("inspect_norm")
  app$wait_for_idle(timeout = 30000)
  app$click("apply_normalization_modal")
  app$wait_for_idle(timeout = 60000)

  combined <- app$get_value(export = "omicsData_norm")

  # The normalized, combined object should carry data from both edatas
  expect_false(is.null(combined), label = "combined omicsData should not be null after norm")
  expect_true(
    isTRUE(attr(combined, "data_info")[["is_combined"]]),
    label = "is_combined attribute should be TRUE after combine_omicsData"
  )

  n_pos_lipids <- nrow(pos_project$Data$e_data)
  n_neg_lipids <- nrow(neg_project$Data$e_data)
  # combined e_data should have rows from both datasets (minus any filtered)
  expect_lte(nrow(combined$e_data), n_pos_lipids + n_neg_lipids)
  expect_gt(nrow(combined$e_data), 0)

  app$stop()
})

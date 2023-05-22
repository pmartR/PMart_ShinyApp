source("tests/map_test_setup.R")

for (f in dir("tests/testthat")) {
  if (startsWith(f, "test_map_")) {
    shinytest2::test_app(filter = substring(f, nchar("test_") + 1, nchar(f) - nchar(".R")))
  }
}

Sys.setenv("MAP_SHINYTEST"=0)

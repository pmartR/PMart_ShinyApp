# Load application support files into testing environment
shinytest2::load_app_env()

open_collapse <- function(id, value) {
  runstring = sprintf("$(\"#%s > div[value='%s']\").find(\"a[data-toggle='collapse']\").click()", id, value)
  return(runstring)
}

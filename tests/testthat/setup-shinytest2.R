# Load application support files into testing environment
shinytest2::load_app_env()

open_collapse <- function(id, value) {
  runstring = sprintf("$(\"#%s > div[value='%s']\").find(\"a[data-toggle='collapse']\").click()", id, value)
  return(runstring)
}

testthat::local_edition(3)
project_root <- file.path(testthat::test_path(), "../..")

if (Sys.getenv("MAP_SHINYTEST") == 1) {
  
  if (!requireNamespace("mapDataAccess")) {
    cat("mapDataAccess is not installed. Do you want to download it?\n")
    cat("[Y/n]")
    resp <- readline()
    if (tolower(resp) != 'y') {
      stop("\n  Error: mapDataAccess not installed.")
    }
    
    pat_env = FALSE
    if (!is.null(Sys.getenv("GITLAB_PAT")) && Sys.getenv("GITLAB_PAT") != "")
      pat_env = TRUE
    else  {
      cat("Enter your GITLAB_PAT: ")
      resp <- readline()
      Sys.setenv("GITLAB_PAT"=resp)
    }
    
    renv::install('gitlab@code.emsl.pnl.gov::multiomics-analyses/mapdataaccess-lib@7225058a1563944d2f20b10655cb2b92ae23ed71')
    
    if (!pat_env)
      Sys.unsetenv("GITLAB_PAT")
    
    rm(resp)
    gc()
  }
  
  library(mapDataAccess)
  
  if (system2("docker", "--version") != 0) {
    stop("\n  Error: docker not found. Please ensure it is installed.")
  }
  
  images <- system2("docker", "images", stdout = TRUE)
  
  if (!any(grepl("minio/minio", images))) {
    cat("Minio container not detected. Do you wish to download it?\n")
    cat("[Y/n] ")
    resp <- readline()
    if (tolower(resp) != 'y') {
      stop("\n  Error: minio container not installed.")
    }
    
    system2("docker", "run -d -p 9000:9000 -p 9001:9001 --name minio minio/minio server /data  --console-address \":9001\"")
  }
  
  system2("docker", "start minio")
  
  orig_envvar = Sys.getenv("MAP_VERSION")
  Sys.setenv("MAP_VERSION"=1)
  on.exit({Sys.setenv("MAP_VERSION" = orig_envvar)})
  
  Sys.setenv("MAP_DATA_SOURCE"="minio")
  Sys.setenv("MINIO_ENDPOINT"="localhost:9000")
  Sys.setenv("MINIO_ACCESS_KEY"="minioadmin")
  Sys.setenv("MINIO_SECRET_KEY"="minioadmin")
  Sys.setenv("MINIO_BUCKET"="map")
  Sys.setenv("MINIO_SECURE"="False")

  cfg_path = if (!is.null(Sys.getenv("MAP_CONFIG")) &&
                 Sys.getenv("MAP_CONFIG") != "") {
    Sys.getenv("MAP_CONFIG")
  } else {
    file.path(project_root, "cfg/minio_config.yml")
  }
  python_venv <- NULL
  
  withr::with_dir(project_root,{
    if (file.exists(cfg_path)) {
      cfg <- yaml::read_yaml(cfg_path)
      python_venv <- cfg$python_venv
    }
  })
  
  if (is.null(Sys.getenv("MAP_PYTHON_VENV")) || Sys.getenv("MAP_PYTHON_VENV") == ""){
    cat("Please select an option for Python virtual environment:\n")
    cat("  0. Cancel\n")
    if (!is.null(python_venv)) cat("  1. Use existing venv (", python_venv, ")\n")
    cat("  2. Select another venv directory\n")
    cat("  3. Install a new Python venv\n\n")
    cat("[Select option] ")
    option <- readline()
    if (!is.null(python_venv) && strtoi(option) == 1) {
      Sys.setenv("MAP_PYTHON_VENV"=python_venv)
    } else if (strtoi(option) == 2) {
      cat("[Enter full path of Python venv] ")
      python_venv <- readline()
      Sys.setenv("MAP_PYTHON_VENV"=python_venv)
    } else if (strtoi(option) == 3) {
      if (system2("python", "-m pip install virtualenv") != 0 ||
          system2("python", sprintf("-m virtualenv %s", file.path(project_root, "venv"))) != 0 ||
          system2(file.path(project_root, "venv/bin/python"), sprintf("-m pip install -r %s", file.path(project_root, "requirements.txt")))) {
        stop("  Error: failed to create Python venv.")
      }
      Sys.setenv("MAP_PYTHON_VENV"="./venv")
    } else {
      stop("  Error: no Python venv.")
    }
  }
  
  MapConnect <- NULL
  count_fail <- 0
  while (TRUE) {
    MapConnect <- map_data_connection()
    if (!is.integer(MapConnect))
      break
    
    sleep(1)
    count_fail++
      if (count_fail > 20)
        stop("  Error: failed to connect to minio after 20 seconds.")
  }
  
  withr::with_dir(project_root,
    for (f in dir("tests/map_test_data")) {
      put_file(MapConnect, paste0("tests/map_test_data/", f), id=f)
    }
  )
} else {
  
  cfg_path = if (!is.null(Sys.getenv("MAP_CONFIG")) &&
                 Sys.getenv("MAP_CONFIG") != "") {
    Sys.getenv("MAP_CONFIG")
  } else {
    "cfg/minio_config.yml"
  }
  python_venv <- NULL
  
  withr::with_dir(project_root,{
    if (file.exists(cfg_path)) {
      cfg <- yaml::read_yaml(cfg_path)
      python_venv <- cfg$python_venv
    }
  })
  
  conda_envs <- tryCatch(
    {
      reticulate::conda_list()$python
    },
    error = function(e) {
      NULL
    }
  )
  
  is_conda <- any(sapply(conda_envs, function(envpath) {
    grepl(sprintf("^%s", normalizePath(python_venv)), envpath)
  }))
  
  if (is_conda) {
    reticulate::use_condaenv(python_venv, required = TRUE)
  } else {
    reticulate::use_virtualenv(python_venv, required = TRUE)
  }
}

# for testing plotly plots, see https://rdrr.io/cran/plotly/src/tests/testthat/helper-vdiffr.R
visual_testing = TRUE

imageServer <- if (visual_testing) {
  # https://github.com/plotly/plotly.R/issues/2179
  reticulate::py_run_string("import sys") 
  kaleido() 
} else {
  list(transform = function(...) stop("Visual testing is disabled!"))
}

# define logic for writing svg
write_plotly_svg <- function(p, file, title = "") {
  # before exporting, specify trace[i].uid so resulting svg is deterministic
  # https://github.com/plotly/orca/issues/133
  p <- plotly_build(p)
  uid_data <- paste0("-vdiffr-plotly-", seq_along(p$x$data))
  p$x$data <- Map(function(tr, id) { tr$uid <- id; tr }, p$x$data, uid_data)
  
  # write svg to disk
  owd <- setwd(dirname(file))
  on.exit(setwd(owd))
  imageServer$transform(p, file = basename(file), width = 640, height = 480)
  
  # strip out non-deterministic fullLayout.uid
  # TODO: if and when plotly provides an API to pre-specify, use it!
  svg_txt <- readLines(file, warn = FALSE)
  strextract <- function(str, pattern) regmatches(str, regexpr(pattern, str))
  def <- strextract(svg_txt, 'defs id=\\"defs-[[:alnum:]]+\\"')
  uid <- sub("defs-", "", strextract(def, "defs-[[:alnum:]]+"))
  svg_txt <- gsub(uid, "", svg_txt, fixed = TRUE)
  writeLines(svg_txt, file)
}


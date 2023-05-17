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

if (system2("docker", "--version") != 0) {
  stop("\n  Error: docker not found. Please ensure it is installed.")
}

containers <- system2("docker", "container list", stdout = TRUE)

if (!grepl("minio", containers[2])) {
  cat("Minio container not detected. Do you wish to download it?\n")
  cat("[Y/n] ")
  resp <- readline()
  if (tolower(resp) != 'y') {
    stop("\n  Error: minio container not installed.")
  }
  
  system2("docker", "run -d -p 9000:9000 -p 9001:9001 --name minio minio/minio server /data  --console-address \":9001\"")
}

system2("docker", "start minio")

Sys.setenv("MAP_VERSION"=1)
Sys.setenv("MAP_DATA_SOURCE"="minio")
Sys.setenv("MINIO_ENDPOINT"="localhost:9000")
Sys.setenv("MINIO_ACCESS_KEY"="minioadmin")
Sys.setenv("MINIO_SECRET_KEY"="minioadmin")
Sys.setenv("MINIO_BUCKET"="map")
Sys.setenv("MINIO_SECURE"="False")
cfg_path = if(!is.null(Sys.getenv("MAP_CONFIG")) && Sys.getenv("MAP_CONFIG") != "") Sys.getenv("MAP_CONFIG") else "./cfg/minio_config.yml"
python_venv <- NULL
tryCatch({
  cfg <- yaml::read_yaml(cfg_path)
  python_venv <- cfg$python_venv
})


if (is.null(Sys.getenv("MAP_PYTHON_VENV")) || Sys.getenv("MAP_PYTHON_VENV") == ""){
  cat("Please select an option for Python virtual environment:\n")
  cat("  0. Cancel\n")
  if (!is.null(python_venv)) cat("  1. Use existing venv (", python_venv, ")\n")
  cat("  2. Select another venv directory\n")
  cat("  3. Install a new Python venv\n\n")
  cat("[Select option] ")
  option <- readline()
  if (strtoi(option) == 1) {
    Sys.setenv("MAP_PYTHON_VENV"="./venv")
  } else if (strtoi(option) == 2) {
    cat("[Enter full path of Python venv] ")
    python_venv <- readline()
    Sys.setenv("MAP_PYTHON_VENV"=python_venv)
  } else if (strtoi(option) == 3) {
    if (system2("python", "-m pip install virtualenv") != 0 ||
        system2("python", "-m virtualenv ../venv") != 0 ||
        system2("../venv/bin/python", "-m pip install -r ../requirements.txt")) {
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

for (f in dir("tests/map_test_data")) {
  put_file(MapConnect, paste0("tests/map_test_data/", f), id=f)
}
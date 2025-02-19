# pmartR-shiny

Shiny GUI implementation of the pmartR R package.

This project implements a shiny-GUI for the analysis of a single \`omics dataset
using the pmartR package a backend.  The aim is for the bulk of the functionality
of the package to be available to the user without the need for familiarity with
R or the package itself.  The following capabilities, presented in the order of
a traditional `omics analysis, are available in the GUI:

- Data upload.  Upload expression data, sample information, and biomolecule metadata.  See the data-requirements section for details.
- Data transformation (raw to log2)
- Group assignment (main effects, covariates, pairing structure)
- Exploratory data analysis.  PCA, missing-variable plots, correlation heatmaps, and more.
- Filtering.  Filter biomolecules based on various criteria including minimum non-missing values and coefficient of variation thresholds.  Filter samples based on statistical metrics and other exploratory analyses.
- Normalization.  Center data using a variety of methods.  Determine appropriate measures automatically using the SPANS procedure for proteomics.
- Protein Quantification.  Various methods for rolling up peptide data up to the protein level.
- Statistical analysis.  ANOVA, G-test, and combined analyses to determine biomarkers.
- Visualize and download all resources.

### Running the app locally:

#### 1.  Using R/Rstudio/Shiny
Install the required packages.  You can do this either by inspecting the DESCRIPTION file and installing the appropriate packages, or by using `renv`.  

To install package with `renv`, first `install.packages("renv")`.  Then call `renv::restore()`.  This will install all packages contained in the renv.lock file.  See the [renv website](https://rstudio.github.io/renv/articles/renv.html) for more details.

Some in-development packages are not being tracked in renv.lock (to reduce Docker image build times).  These can be seen in the `install commonly updated packages` section of Dockerfile-base.  You will have to install these separately from the files that will be installed by calling `renv::restore()`.

You will also need a [Python virtual environment](https://packaging.python.org/en/latest/guides/installing-using-pip-and-virtual-environments/#creating-a-virtual-environment) with the packages from requirements.txt installed. 
Then, add a .yml file with `python_venv: <path-to-your-venv>` in it. Finally, set the MAP_CONFIG environment variable to point to that yml file. 
You can optionally put the following command in your .RProfile to automatically set the environment variable when the project is loaded: `Sys.setenv("MAP_CONFIG" = "<path-to-yml-file>")`.

Once all dependencies are installed, make sure calling `.libPaths()` displays the renv environment, then call `shiny::runApp()`.

#### 2.  Using docker:

Either build the container as described in the development section, or pull it from gitlab:

```bash

# Login 
docker login https://code-registry.emsl.pnl.gov

# docker pull
docker pull code-registry.emsl.pnl.gov/multiomics-analyses/pmart_standalone:<version>`

# docker compose
TOP_VERSION=<version> docker compose pull
```

Then run the docker container:
```bash
# manually
docker run -v /absolute/path/to/cfg/minio_config.yml:/srv/shiny-server/cfg/minio_config.yml -p 8300:8300 code-registry.emsl.pnl.gov/multiomics-analyses/pmart_standalone:<version>

# docker compose
TOP_VERSION=<version> docker compose up

# make
make run TOP_VERSION=<version>
```
... and navigate to https://127.0.0.1:8300


***

### **Development**

#### **1. Modularization**

The different parts of a shiny app (observers, reactive variables, UI elements) are roughly organized into folders.  Within each folder are the corresponding elements for that particular tab.  This is not a hard rule, and sometimes observers will make it into the reactive variables folder if it is deemed to make sense.  A list of the folders containing shiny elements:

- observers
- reactive_variables
- UI_elements (reactive elements usually constructed with renderUI)
- tabs_UI (Higher level, usually non-reactive elements)

#### **2. Dockerfiles**

We build a base container which has all the system libraries and R packages installed, and then build a container on top of it that simply copies the app source code and exposes the correct port.  There are two Dockerfiles, and two corresponding .dockerignore files.

**To build the base container**, you must provide a gitlab PAT in order to install mapDataAccess and other private git repos.  Assume you have an account that has access to the private gitlab repos in pmart; now generate a personal access token:  https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html.  Put this token in a text file next to the Dockerfile, say `.mysecret` with contains the line GITLAB_PAT=&lt;your token&gt;.  Sometimes github will rate limit installation from repositories, but you can provite a github PAT (https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens) to get around this:  Optionally, on another line in `.mysecret` put GITHUB_PAT=&lt;your token&gt;.

Now, replacing &lt;version&gt; with whatever version (e.g. 1.0.0), build the container:

```bash

# Login 
docker login https://code-registry.emsl.pnl.gov

# manually
docker build -f Dockerfile-base --secret id=access_tokens,src=.mysecret -t code-registry.emsl.pnl.gov/multiomics-analyses/pmart_standalone/base:<version> .

# using make
make build_base BASE_VERSION=<version> SECRET_PATH=.mysecret
```

**To build the 'top' container**:  
Make sure Dockerfile refers to the correct base container or specify the `--build-arg base_tag=<your base image tag>;` (e.g. code-registry.emsl.pnl.gov/multiomics-analyses/pmart_standalone:1.0.0) in the build command or the correction TOP_VERSION using `make` if you have updated any dependencies:  

```bash
# manually
docker build --build-arg base_tag=<your base image tag> -t code-registry.emsl.pnl.gov/multiomics-analyses/pmart_standalone:<version> .

# using make
make build_top BASE_VERSION=<base version> TOP_VERSION=<top version>
```

If all is well, push new containers to the registry:  `docker push <container_name>:<version>` or using make:

```bash
# top container
make push_top TOP_VERSION=<top version>

# base container
make push_base BASE_VERSION=<base version>

# both
make push TOP_VERSION=<top version> BASE_VERSION=<base version>
```

#### **3. Dependencies**

We use [renv](https://rstudio.github.io/renv/articles/renv.html) to track dependencies.  The renv.lock file contains a list of dependencies and various details about them.  (NOTE:  Currently two dependencies (pmartR and mapDataAccess-lib) that are under active development alongside the app are not tracked in the lockfile, but in `Dockerfile-base`, you will have to install these manually).  We use renv to manage the details about dependencies, but try keep track of them manually in DESCRIPTION as well.  This gives us the option of explicity telling renv to include a package when calling `renv::snapshot()`.  When updating the lockfile, we will do the following:

1.  Set renv to only install sub-dependencies in the "Depends" and "Imports" field of installed packages. `renv::settings$package.dependency.fields("Depends", "Imports")`.  This should get recorded in ./renv/settings.json so you only have to do it once.
2.  Snapshot only packages mentioned in the project (including in the DESCRIPTION file), as well as any packages mentioned in their "Depends" and "Imports" field by calling `renv::snapshot(type="implicit")`

#### **4. Running tests**

An easy way to run tests is to run `shinytest2::test_app()`. However, the MAP tests will not run by default. To run the MAP tests from the local project but pulling project and midpoint files from minio, set `Sys.setenv("MAP_SHINYTEST" = 1)` before running `shinytest2::test_app()`. The first run will walk through the installation of the environment required for MAP testing.

`Sys.setenv("MAP_SHINYTEST" = 2)` followed by `shinytest2::test_app()` will run the tests using the tests using the containerized app.  The containerized app is spun up using the `docker-compose.yml` file in the project root and run the `latest` version by default.  Setting the environment variable `TOP_VERSION` can change which container version is tested.

```bash
# Using make
make MAP_SHINYTEST=1 TOP_VERSION=1.0.0 test
```


#### **5. Misc**

**Long text**:  Long tooltips or info text should go in the `ttext_` or `infotext_` global variables in `global.R` and then referenced in the app to keep code tidy.

**Developer buttons**:  It is often useful to get access to the console when debugging changes.  To activate developer buttons, set `Sys.setenv("SHINY_DEBUG" = 1)` before running the app.  It helps to put this command in your local .Rprofile.  At least one button should appear floating in the corner of the screen, which when clicked, will give access to the R console for debugging while the app is running.


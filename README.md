# pmartR-shiny

Shiny GUI implementation of the pmartR R package.

This project implements a shiny-GUI for the analysis of a single \`omics dataset
using the pmartR package a backend.  The aim is for the bulk of the functionality
of the package to be available to the user without the need for familiarity with
R or the package itself.  The following capabilities, presented in the order of
a traditional `omics analysis, are available in the GUI:

- Data upload.  Upload data files, sample information, and biomolecule metadata.  See the data-requirements section for details.
- Data transformation (raw to log2)
- Group assignment (main effects and covariates)
- Exploratory data analysis.  PCA, missing-variable plots, correlation heatmaps, and more.
- Filtering.  Filter biomolecules based on various criteria including minimum non-missing values and coefficient of variation thresholds.  Filter samples based on statistical metrics and other exploratory analyses.
- Normalization.  Center data using a variety of methods.  Determine appropriate measures automatically using the SPANS procedure for proteomics.
- Protein Quantification.  Various methods for rollup peptide data up to the protein level.
- Statistical analysis.  ANOVA, G-test, and combined analyses to determine biomarkers.
- Visualize and download all resources.

### Running the app locally:

#### 1.  Using R/Shiny
Install the required packages.  You can do this either by inspecting the DESCRIPTION file and installing the appropriate packages, or by using `renv`.  

To install package with `renv`, first `install.packages("renv")`.  Then call renv::restore().  This will install all packages contained in the renv.lock file.

Then simply call shiny::runApp()

#### 2.  Using docker:
In a terminal/shell/command line at the project root:

build the docker container: `docker build -t pmartr-shiny .`  
run the docker container:  `docker run -p 8300:8300 pmartr-shiny`  
navigate to https://127.0.0.1:8300



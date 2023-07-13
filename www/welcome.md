# Welcome to the PMart App!

***

## What is PMart?

PMart encompasses the capabilities of the pmartR R package: https://github.com/pmartR/pmartR, which contains methods for processing and analysis of mass-spectrometry based `omics data.  
The core capabilities of the package, all implemented in this application are:

- Quality control
  - Data transformation (e.g. log-transform)
  - Outlier detection and removal
  - Filtering of biomolecules
  - Sample normalization

- Exploratory Visualization (PCA/glm-PCA, boxplots, missing values profiling)
  
- Quantification of peptide data to protein level

- Statistical analyses
  - iMd-ANOVA:  A combined ANOVA and presence absence based test of group differences
  - Transcriptomics based:  DESeq2, Limma-Voom, EdgeR

Visualizations of each step are available to save and download, along with processed data files, statistical results and a report detailing the processing steps taken.  The user provides the raw data and sample information in .csv format.  To get started, see our tutorial videos, or work through the tabs and click the help button at the top right for instructions.

**Click the 'Download example data' button at the bottom of this page to see some example input files as well as an example of a produced report**

***

PMart accepts the following data types:

* **Metabolomics**
    * GC-MS
    * NMR
* **Lipidomics**
    * Single ionization
    * Both negative and positive ionizations
* **Proteomics**
    *	Peptide data or protein data (not both)
        * Label-free
        * Isobaric labeled
* **Transcriptomics (RNA-seq)**
        
***

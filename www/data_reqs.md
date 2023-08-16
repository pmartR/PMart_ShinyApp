### <span style = 'color:navy'><strong>pmartR - general requirements</strong></span>

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

csv files containing the following information must be uploaded:
* (Upload tab) e_data is a cross-tab with rows for each biomolecule and columns for each sample plus another column that specifies the unique identifier for each biomolecule row. A header row contains the sample names and biomolecule type name (e.g. Metabolite, Lipid, Peptide).
* (Groups tab) The Sample Information File (f_data) is a cross-tab with rows for each sample, where one column contains the sample names (which must match identically the column names in the expression cross-tab) and other columns containing other relevant information about the samples such as experimental groups, conditions, run order, etc. 
* (Optional: Upload tab) e_meta (only required for peptide data) is a cross-tab with one column containing the unique biomolecule identifiers, and additional columns containing mappings to other identifiers or metainformation. For proteomics data, this cross-tab contains the peptide-to-protein mapping. 

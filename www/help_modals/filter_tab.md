# Filter Biomolecules or Samples from the Data

The filter tab allows you to visualize and apply filters to your data.

On the left side are various filters with buttons to 'Add/Remove' those filters from a list of filters *to be applied at the end*, and a button to preview a plot of the effects of that individual filter.  The selected filters are applied in the following order:

- Molecule filter
- Total count filter
- Proteomics filter*
- Coefficient of variation filter
- iMd-ANOVA filter
- RNA filter (library size)
- RNA filter (min nonzero counts)
- rMd filter
- Custom filter

So if the user selected the molecule and rMd filters, the app will first apply the molecule filter and then the rMd filter.  Filter options may differ between datatypes.  See the [pmartR package](https://github.com/pmartR/pmartR) for precise details about each filter.

- *A note about proteomics filter order:  The proteomics filter has an option to remove redundant peptides as well as proteins with too few peptides mapping to them.  If both options are selected, the redundant peptides will be removed before determining the number of peptides mapping to each protein.  

**Biomolecule Filters** filter the biomolecules (rows) of your data by criteria such as minimum number of samples with non-missing values.

**Sample Filters** remove samples (columns) from your data based on criteria such as how much of an outlier they are based on various metrics.  **Removal of sample outliers should be done with caution, and ideally removal should be informed by input from the experimental scientist and/or supporting evidence from more filters than just the rMd Filter.**

**Custom Filters** are where the user can choose specific samples or biomolecules to remove based on their knowledge of the data.

Once you have added all the filters you want to apply, click the 'Review and apply filters' button to apply them to your data.  You will see a popup modal with a review of the effects your filters will have.  If you are satisfied, click 'Apply all filters' to apply them to your data, which will bring up another modal prompting review or continuation to the normalization tab.


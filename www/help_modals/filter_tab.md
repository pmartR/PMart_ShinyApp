# Filter Biomolecules or Samples from the Data

The filter tab allows you to visualize and apply filters to your data.

On the left side are various filters with buttons to 'Add/Remove' those filters to a queue *to be applied at the end*, and a button to preview a plot of the effects of that individual filter.  Filter options may differ between datatypes.  See the [pmartR package](https://github.com/pmartR/pmartR) for precise details about each filter.

**Biomolecule Filters** filter the biomolecules (rows) of your data by criteria such as minimum number of samples with non-missing values.

**Sample Filters** remove samples (columns) from your data based on criteria such as how much of an outlier they are based on various metrics.

**Custom Filters** are where the user can choose specific samples or biomolecules to remove based on their knowledge of the data.

Once you have added all the filters you want to apply, click the 'Review and apply filters' button to apply them to your data.  You will see a popup modal with a review of the effects your filters will have.  If you are satisfied, click 'Apply all filters' to apply them to your data, which will bring up another modal prompting review or continuation to the normalization tab.


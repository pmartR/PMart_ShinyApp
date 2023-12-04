# Normalize Samples in Your Data

In this tab you can apply normalization to your samples to make sure they are comparable to each other.  This tab specifically deals with global statistical normalization, where every sample has normalization parameters computed based on its data characteristics.

The options for the way normalization is applied include:

1.  **Subset function:**  Which biomolecules to use when computing normalization parameters, and assocated parameters for the subset function.
2.  **Normalization function:**  Which function to use to normalize the data once the subset function has been selected (e.g. median, mean, z-score), and appropriate parameters where applicable.
3.  **Whether to backtransform the data:**  Often you will want to transform the data back to its original scale, which can be done by setting this option to "Yes".

**There are two options** for selecting these parameters:

1.  Manually select them using the dropdowns and input boxes.
2.  Use SPANS, which attempts to select a normalization scheme that introduces as little bias across groups as possible. 

## Manual

Select your normalization scheme and hit 'Diagnostics for normalization selection'; the app will check if this normalization approach induces bias in your data.  When finished, a modal will appear displaying the results of the analysis.  If you are satisfied with the current settings for normalization, click 'Apply normalization'.  If you wish to try a different normalization approach, simply select a different subset and normalization function and click the ‘Diagnostics for normalization selection’ button to evaluate the new selection.

## SPANS

SPANS is only available for proteomics data, and is a method for selecting normalization parameters that minimize the bias introduced by normalization across groups.  If you choose to use SPANS, you will be presented with options for which subset functions and normalization functions to test.  You can also specify which parameters for subset functions to test.  

Click the 'Run SPANS procedure' button and wait (it can often take a long time on larger datasets, > 10 minutes).  Once finished, a plot of the different normalization parameters and some options for the best set will be shown on the right.

You can select a set of parameters from the table or manually enter them on the left.  Once you are satisfied click 'Apply normalization'.



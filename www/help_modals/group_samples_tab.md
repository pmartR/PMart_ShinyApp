# Specify Sample Grouping

This tab is where you will upload your sample information (.csv) file and specify which columns in it indicate things like the main effects, covariates, and pairing structure.  The following general steps are taken on this tab:

**Upload Data**

Use the **Upload CSV Sample Information file** to find and upload your sample information file, whose structure is specified in the data requirements on the welcome page

```
The Sample Information File (f_data) is a cross-tab with rows for each sample, where one column contains the sample names (which must match identically the column names in the expression cross-tab) and other columns containing other relevant information about the samples such as experimental groups, conditions, run order, etc. 
```

**Specify Main Effects and Covariates**

This panel has dropdowns for each main effect and covariate.  You must select at least one main effect, and optionally a second main effect and/or up to two covariates.

Main effects are the variables of interest, those values across which you want to make comparisons.  Covariates are those values that are not of experimental interest, but must be controlled for in the statistical comparisons.

**Specify Pairing Structure**

Optionally, if your experimental design involves paired sampling, such as is the case for before/after measurements, you can specify a pairing structure as long as your sample information file contains the appropriate columns.  You will select three columns indicating, in the following order:

1. Which pairs of samples go together (i.e. each pair shares a unique identifier in this column).
2. The different samples in each pairing (i.e. we just need to have an identifier that specifies sample 1 vs 2 in pairing 'A')
3. Which sample is the 'denominator', or rather which sample will be subtracted when calculating log differences within a pair.

Finally, if you are satisfied with your choices, click the 'Apply grouping' button.  You will then see a popup modal prompting you to review or continue to subsequent tabs.

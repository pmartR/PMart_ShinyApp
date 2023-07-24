# Specify Your Sample Grouping

This tab is where you will upload your sample information (.csv) file and specify which columns in it indicate things like the main effects, covariates, and pairing structure.  Roughly do the following:

**Upload Data**

Use the **Upload CSV Sample Information file** to find and upload your sample information file, whose structure is specified in the data requirements on the welcome page

```
The Sample Information File (f_data) is a cross-tab with rows for each sample and columns containing sample names for the data, as well as additional information about the project such as experimental groups or conditions.
```

**Specify Main Effects and Covariates**

This panel has dropdowns for each main effect and covariate.  You must select at least one main effect, and optionally one more main effect and up to two covariates.

Main effects are the variables of interest, those values across which you want to make comparisons.  Covariates are those values that are not of experimental interest, but must be controlled for in the statistics.

**Specify Pairing Structure**

Optionally, if your sample information file contains the appropriate columns, you can specify a pairing structure.  You will select three columns indicating, in the following order:

1. Which pairs of samples go together (i.e. each pair shares a unique identifier in this column).
2. The different samples in each pairing (i.e. we just need to have an identifier that specifies sample 1 vs 2 in pairing 'A')
3. Which sample is the 'denominator', or rather which sample will be subtracted when calculating differences within a pair.

Finally, if you are satisfied with your choices, click the 'Apply grouping' button.  If everything goes well, you should see a popup modal prompting you to review or continue to subsequent tabs.

# Statistics 

This tab is for running statistical and other post-normalization analyses on your data.  If you began with peptide-level data this tab will operate on the protein-level data after the Protein Roll Up tab.  If you have lipid data with positive and negative ionization modes, the statistical analyses will combine your data objects if they have not already been combined on the normalization tab.  Roughly do the following:

1.  Select your analysis method via the dropdown.
2.  For statistical analyses, select which group comparisons you want to perform the analysis on.  You can select all comparisons, control to test, or user specified comparisons.  Additional menus/tables requesting information about the comparisons will appear depending on your selection.
3.  Fill out any additional settings such as whether to apply one or both types of statistical tests for iMd-ANOVA, or which multiple comparison adjustments to apply.
4.  Click the blue button at the bottom of the left column to perform the analysis.  A modal indicating success will appear and plots/tables of your results will become available in the right-side panels.

**A note on non-normal distributions and the ANOVA:** The F statistic is well-known to be robust to mild deviations from normality (Knief, 2021) (Lix, 1996), and in many cases a log transformation is considered adequate to achieve a sufficiently Normal distribution. However, in extreme cases involving small sample sizes and greater or heterogenous deviations from normality, ANOVA may not be appropriate. See (Lix, 1996) for discussion of alternative tests. ANOVA based tests are common practice and widely accepted for log-transformed proteomics, metabolomics, and lipidomics data. Some caution should be used with NMR-based data, and alternative tests are provided for RNAseq data.

***

[1] Knief, Ulrich, and Wolfgang Forstmeier. 2021. “Violating the Normality Assumption May Be the Lesser of Two Evils.” Behavior Research Methods 53 (6): 2576–90. https://doi.org/10.3758/s13428-021-01587-5.

[2] Lix, Lisa M., Joanne C. Keselman, and H. J. Keselman. 1996. “Consequences of Assumption Violations Revisited: A Quantitative Review of Alternatives to the One-Way Analysis of Variance ‘F’ Test.” Review of Educational Research 66 (4): 579–619. https://doi.org/10.2307/1170654.


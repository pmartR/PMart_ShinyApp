# Reference Normalization

This tab allows you to perform normalization to reference samples on data types such as labeled proteomics or NMR metabolomics.  You will be asked if your data contain reference samples and then to specify various columns needed to perform the normalization:

#### Reference Group

Reference normalization expects samples to belong to one of several several sets of samples (e.g. plexes or plates for labeled proteomics data), each having a single &quot;reference&quot; sample (e.g. a sample on the plex that is a pool of the study samples on that same plex).  Indicate the columns in the sample information file that specify these sets of samples.

#### Reference Sample

Select which column in the sample information file indicates whether or not a sample is the reference sample for its corresponding sample set. For example, this column might contain only 1s and 0s, with 1s indicating a reference sample, and would have only a single 1 for each sample set (e.g. plex or plate).

#### Reference Sample Indicator

Select which value indicates a reference sample.  In the column you selected as indicating reference samples, what value in that column indicates a reference sample.  For example, you might specify that &quot;1&quot; represents a reference sample.

****

Once you are finished, click 'Apply reference normalization' and wait for the modal to appear indicating success.  You can review tables and plots of the reference normalized data if desired.

# Upload Your Data

This tab is where you will upload your raw expression data.  Click the 'Data Requirements' panel on the right side of the page to see what you will need to upload on this tab and one subseqeuent tab.  On this page, it will be the expression data (e_data) and optionally a biomolecule information file (e_meta).  Example datasets are provided for download on the home page.

On the left column of panels, you will be prompted to specify the following in roughly this order:

- **Specify the type of data** you are uploading.  This will determine the type of analysis that will be performed.  Some datatypes have sub-options such as whether your peptide data is isobaric labeled or label-free, or if your lipid data has two files for positive and negative ionization modes.

- **Upload your data** An input will appear where you can browse to a file to upload your expression data.  Navigate to and select the file on your computer.

- **Specify ID columns and other data properties**:  You must specify which column contains the biomolecule identifiers, as well as other properties of your data such as the scale it is on (raw abundance versus a log-transformed scale), a scaling transformation to be applied, and what value denotes missing data.

- **(Optional) Upload a metadata file**:  In the final panel on the left side, you can upload a metadata file that contains additional information about each biomolecule.  This is most common for peptide level data, for which you will also need to specify the column which contains protein identifiers.

****

Finally, if everything is filled out properly, the "Create omicsData object" button will be enabled.  Clicking it will create your data object and prompt you to move to review plots and tables of the uploaded data or continue to the Groups tab.

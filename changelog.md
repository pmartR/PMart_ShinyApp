## Code and Dockerfile Changelog

### Code 

#### 2023

February
    - Downloadable reports for all data types 
    - Options to adjust p-values in iMd-ANOVA for multiple comparisons and/or multiple biomolecules (FDR)

#### 2022

November
    - More options for plot downloads, can select:
        - width/height/scale
        - file format (choose from html, png, jpeg, svg)

October
    - Filters for sequence data

August
    - Added support for paired data

Feb/March
    - Initial connectivity to MAP Portal

#### Docker Containers

**1.7**
- Updates to pmartR
- Fixes for two lipids dataset

**1.6**
- Downloadable reports
- Accomodate new changes to `imd_anova` in pmartR package

**1.5**
- Statistics for seqdata

**1.4**
mapDataAccess and pmartR moved inside dockerfile (instead of lockfile) to reduce build times.  
- 1.4.2:
    - Remove orca in favor of Kaleido
    - Add necessary python dependencies for Kaleido and move installation to end of build

**1.2**:  
Fixes issue with imd-anova plot not showing.  Rlang forced update to accommodate newer versions of dependencies. 
- rlang -> 1.0.2
- pmartR -> 959863b6fd8aaee4cb82f85ae1045f92bef494a7

**1.1** :  
- Install required python packages.
- mapDataAccess

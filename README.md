### Code and data for Walsh et al. (2021). Linking stormwater control performance to stream ecosystem outcomes: incorporating a performance metric into effective imperviousness.  

The RMarkdown documents with names like `walsh_etal_foundation_PLOSW_S1.Rmd` are the source code of the supplementary materials (S1-S5) of the paper, and `walsh_etal_foundation_PLOSW_figs_tab.Rmd` is the source of the paper's figures and table.  

The large data files used by this repository are stored in the Open Science Framework (OSF) repository [https://osf.io/57azq/](https://osf.io/57azq/).  Assuming you are working in a clone of this github repository, the script `load_ld_scms_tables.R` downloads the data from the OSF repository. (This file is sourced, and the data downloaded, if you run or knit most of the Rmd files.)  

The data were originally stored as a postgreSQL database, with this structure. 


![](images/ld_scms_ER_diagram.png?raw=true).  


All spatial tables are stored as `gpkg` files, and non-spatial tables are saved in the R data file `data/db_non_sf.rda`.   Field definitions for all tables are listed in the metadata table.  The structure of the database is described in detail in S3 appendix.  The script `compile_ld_scms_db_from_tables.R` provides guidance on reconstructing the postgreSQL database locally. 

`project_EBs.R` calculates performance metrics for all stormwater control measures in the project and was used to compile `data/scmProjects_EB.rda`. `references.bib`, `plos.csl`, and `officedown_template.docx` are used in knitting the Rmd documents.  

The R scripts require the following packages to be installed: "scales", "dplyr", "lubridate", "sf", "here", "RPostgreSQL", "mcr", "RColorBrewer", "data.table", "RMySQL", "DiagrammeR", "flextable", and "rstan".

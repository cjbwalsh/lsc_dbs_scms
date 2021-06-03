### Code and data for Walsh et al. (2021). Linking stormwater control performance to stream ecosystem outcomes: incorporating performance metrics into effective imperviousness.  

The RMarkdown document `WalshEtAl_wrr2021_Suppl.Rmd` is the source of the supplementary material of the paper, and `WalshEtAl_wrr2021_figs.Rmd` is the source of the paper's figures.  

The large data files used by this repository are stored in the Open Science Framework (OSF) repository [https://osf.io/57azq/](https://osf.io/57azq/).  Assuming you are working in a clone of this github repository, the script `load_ld_scms_tables.R` downloads the data from the OSF repository. (This file is sourced, and the data downloaded, if you run or knit either Rmd file.)  

The data were originally stored as a postgreSQL database, with this structure. 


![](images/ld_scms_ER_diagram.png?raw=true).  


The script `compile_ld_scms_db_from_tables.R` provides guidance on reconstructing the database locally.

The R scripts require the following packages to be installed: "scales", "dplyr", "lubridate", "sf", "here", "RPostgreSQL", "mcr", "RColorBrewer", "data.table", "RMySQL", "DiagrammeR", anf "flextable".

### Code and data for Walsh et al. (2021). Linking stormwater control performance to stream ecosystem outcomes: incorporating performance metrics into effective imperviousness.  

The large data files used by this repository are stored in the Open Science Framework (OSF) repository [https://osf.io/57azq/](https://osf.io/57azq/).  Assuming you are working in a clone of this github repository, the script `load_ld_scms_tables.R` downloads the data from the OSF repository.  The data were originally stored as a postgreSQL database, with ![this structure](images/ld_scms_ER_diagram.pdf?raw=true).  

The script `compile_ld_scms_db_from_tables.R` provides guidance on reconstructing the database locally.

The RMarkdown document `WalshEtAl_wrr2021_S1-3.Rmd` is the source of the supplementary material of the paper, and `WalshEtAl_wrr2021_figs.Rmd` is the source of the paper's figures.  
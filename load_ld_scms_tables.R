# This file ensures that all large data tables have been downloaded from the Open Science Framework
# repository https://osf.io/57azq/ (Walsh et al. 2021), into the working directory lsc_dbs_scms

# The code requires that you have cloned the associated github repository https://github.com/cjbwalsh/lsc_dbs_scms
# to your working computer.  To clone the repository with RStudio, follow the following steps. With a web browser, 
# navigate to https://github.com/cjbwalsh/lsc_dbs_scms. On the right side of the screen, click the green 
# "Clone or download" button. Click the Copy to clipboard icon to the right of the repository URL. 
# Open RStudio on your local computer. Click File, New Project, Version Control, Git. Paste the repository URL 
# and enter TAB to move to the Project directory name field. Click Create Project.

# Walsh, C. J., Burns, M. J., Fletcher, T. D., Bos, D. G., Kunapo, J., Poelsma, P., & Imberger, S. J. (2021), 
# Linking stormwater control performance to stream ecosystem outcomes: incorporating performance metrics into 
# effective imperviousness/Data and code, Open Science Framework. https://osf.io/57azq

source("code/BACRIfunctions.R")
#Check if all relevant spatial files have been downloaded: if not download them
gpkg_files <- dir(here::here("data"))[grep("gpkg",dir(here::here("data")))]
all_gpkgs <- c("Australia_GDA94_GCS.gpkg","catIA.gpkg","cats.gpkg","ia.gpkg",
               "lsc_dbs_streams.gpkg","parcels.gpkg","scms.gpkg","siteLabels.gpkg",
               "sites.gpkg","streams.gpkg","subcs.gpkg","Victoria_GDA94_GCS.gpkg")
guids <- c("4pz5d","8rd3x","ahstz","hgeuv","yazpb","f9b2e",
           "mgh6t","gku5p","dscqz","jm563","rneqf","ps8wy")
guids <- guids[!all_gpkgs %in% gpkg_files]
gpkg_files <- all_gpkgs[!all_gpkgs %in% gpkg_files]
if(length(gpkg_files) > 0){
  for(i in 1:length(gpkg_files))
    download.OSF.file(GUID = guids[i],
                      Access_Token = "https://osf.io/3um4v/?view_only=7392be708374475b98e5ebbf2f86855f",
                      file_name = gpkg_files[i], subdir = "data")
}

for(i in 1:length(all_gpkgs)){
  temp <- sf::st_read(here::here("data",paste(all_gpkgs[i],sep = "")), 
                      stringsAsFactors = FALSE, quiet = TRUE)
  temp <- sf::st_set_geometry(sf::st_set_geometry(temp,NULL), sf::st_geometry(temp)) # set geometry, return sf
  assign(gsub(".gpkg","",all_gpkgs[i]),temp)
}

# load non-spatial files
rda_files <-  dir(here::here("data"))[grep(".rda",dir(here::here("data")))]
for(i in 1:length(rda_files)){
  if(rda_files[i] == "Croydon_1966_hourly_rain_runoff_et.rda"){
    Croydon <- prepare_runoff_data(get(load(here::here("data",rda_files[i]))))  
  }else{
    load(here::here("data",rda_files[i]))
  }
}
# The following files (loaded above) are derived from the database tables using code in the Rmd files
# ei_ts.rda - see derivation in ei_ts chunk of WalshEtAl_wrr2021_S1-2.Rmd

#Tidy up some of the data ready for analysis and plotting
SCMs <- scms; rm(scms)
#add Little Stringybark and Dobsons impervious data to other catchments
catIA <- rbind(ia, catIA[names(catIA) != "address"])
#minor differences in sampling reaches over time and for different purposes, 
#and minor changes in catchment boundaries with some SCMs.  Select original hydrology sites for map
catMap11 <- cats[cats$hydrology == 1 & cats$origDrain == 1,]  
#Hydrology sites, pre-the Entrance, excluding 2 pipe sites
catMap11$col <- RColorBrewer::brewer.pal(3,"Dark2")[match(catMap11$treatment,c("R","E","C"))]
catMap11$sitecode <- substr(as.vector(catMap11$sitecode),1,7)
sites <- sites[!sites$sitecode %in% c("Heath","Wicks"),]
siteMap11 <- sites[sites$hydrology == 1,]
siteMap11$sitecode <- substr(siteMap11$sitecode,1,7)
siteMap11$col <- catMap11$col[match(siteMap11$sitecode, catMap11$sitecode)]
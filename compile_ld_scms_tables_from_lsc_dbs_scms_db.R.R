# This script records the compilation of tables stored at https://osf.io/57azq/ (Walsh et al. 2021)
# from the unimelb database lsc_dbs_scms.  The process primarily removes data fields
# that are not relevant to Walsh et al. (2021).  The script will only run if connected to the unimelb network.

# Walsh, C. J., Burns, M. J., Fletcher, T. D., Bos, D. G., Kunapo, J., Poelsma, P., & Imberger, S. J. (2021), 
# Linking stormwater control performance to stream ecosystem outcomes: incorporating performance metrics into 
# effective imperviousness/Data and code, Open Science Framework. https://osf.io/57azq

#See https://github.com/cjbwalsh/lsc_foundation/build_postgres_scmdb_from_mysql.R for the source code 
# that compiled the original lsc_dbs_scms postgres database 

source("code/BACRIfunctions.R")  #also loads required packages

# Read tables from the database:
# from server or from remote machine?
hostname <- ifelse(Sys.info()["nodename"] == "water-dev.its.unimelb.edu.au",
                   "localhost", "water-dev.its.unimelb.edu.au")
scm_db <- RPostgreSQL::dbConnect("PostgreSQL", dbname = "lsc_dbs_scms",
                host = hostname, port = 5432,
                user = "readonly", password = "reachcode_42")
dbTabs <- RPostgreSQL::dbListTables(scm_db)
dbTabs <- dbTabs[!dbTabs %in% c("spatial_ref_sys","metadata")]
#capitalization hangovers from old MySQL database
rTabs <- dbTabs
rTabs[match(c("parcelchanges","scmchanges","scmsdecommissioned","scmprojects"), rTabs)] <-
  c("parcelChanges","scmChanges","scmsDecommissioned","scmProjects")
for(i in 1:length(dbTabs)){
  assign(rTabs[i], sqlQuery(paste("SELECT * FROM ",dbTabs[i],";", sep = "")))
# sqlQuery() is a helper function for reading from this and other water-dev
# databases (see BACRIfunctions.R L9).
  }

#Remove fields with personal/non-relevant information for submission to public github repo
nds <- unique(parcels$nextds[!is.na(parcels$nextds)])
for(i in 1:length(nds)){
parcels$nextds[!is.na(parcels$nextds) & parcels$nextds == nds[i]] <- parcels$parcelID[parcels$address == nds[i]]
  }
parcels <- parcels[!names(parcels) %in% c("address","streetNo","streetSuff",
                                          "streetName","streetType","JKroofArea","JKpaveArea")]
parcels <- parcels[c("parcelID","nextds","pipeID","parcelType","roofAreaCon",
                     "roofAreaUncon","paveAreaCon","paveAreaUncon","parcelArea","area_m2","geometry" )]
ia <- ia[names(ia) != "address"]
parcelChanges <- parcelChanges[names(parcelChanges) != "address"]
scmProjects <- scmProjects[!names(scmProjects) %in% c("address","costProject","costGovt","costOwner")]
scms <- scms[names(scms) != "address"]
scmsDecommissioned <- scmsDecommissioned[names(scmsDecommissioned) != "address"]

metadata <- sqlQuery("SELECT * FROM metadata")
metadata <- metadata[metadata$field != "address",]
metadata <- metadata[!metadata$field %in% c("streetNo","streetSuff","streetName","streetType",
                                            "JKroofArea","JKpaveArea","costProject","costGovt","costOwner"),]

#save non-spatial tables in a single rda file
db_sf <-  c("sites","subcs","scms","parcels","ia","cats")
for(i in 1:length(db_sf)){
  sf::st_write(get(db_sf[i]),paste("data/",db_sf[i],".gpkg", sep = ""), delete_layer = TRUE)
}

#streams table in database includes many irrelevant streams.  Extract just LSC and DBS streams from mwstr database
source("https://tools.thewerg.unimelb.edu.au/documents/mwstr/mwstr_functions.R")
streams <- rbind(
sqlQuery(paste("SELECT s1.* FROM (SELECT streams.site, streams.strcode, streams.reach, streams.mi_prinx, 
                streams.type,streams.geometry FROM streams) s1 JOIN (SELECT site FROM subcs WHERE 
                site = ANY( SELECT UNNEST( allus || 61111) FROM subcs  WHERE site =  61111)) s2 ON (s1.site = s2.site);", 
               sep = ""), "mwstr_dev"),
sqlQuery(paste("SELECT s1.* FROM (SELECT streams.site, streams.strcode, streams.reach, streams.mi_prinx, 
                streams.type,streams.geometry FROM streams) s1 JOIN (SELECT site FROM subcs WHERE 
                site = ANY( SELECT UNNEST( allus || 71455) FROM subcs  WHERE site =  71455)) s2 ON (s1.site = s2.site);", 
      sep = ""), "mwstr_dev"))
sf::st_write(streams,"data/lsc_dbs_streams.gpkg", delete_layer = TRUE)
save(dds, filtprofs, metadata, parcelChanges, 
     raingardens, scmChanges, scmProjects, 
     scmsDecommissioned, tanks, file = "data/db_non_sf.rda", compress = "xz")

#Plus LSC hourly rainfall data for S4.
start6min <- ymd_hms("2001-05-31 10:00:00", tz = "UTC")
fin6min <- ymd_hms("2019-07-25 09:54:00", tz = "UTC")
lscRain6min <- sqlQuery("SELECT * FROM rainfall WHERE sitecode = 'LIS0004';", "lsc")
lscRain6min$dateTime <- ymd_hms(lscRain6min$dateTime, tz = "UTC")
lscET6min <- sqlQuery("SELECT * FROM potET WHERE sitecode = 'LIS0004';", "lsc")
lscET6min$dateTime <- ymd_hms(lscET6min$dateTime, tz = "UTC")
lscET6min <- lscET6min[lscET6min$dateTime >= start6min & lscET6min$dateTime <= fin6min,]
lscRain6min <- lscRain6min[lscRain6min$dateTime >= start6min & lscRain6min$dateTime <= fin6min,]
lscRain6min$ET <- lscET6min$Mpot_mm[match(lscRain6min$dateTime, lscET6min$dateTime)]
lscRain6min <- data.table::data.table(lscRain6min)
lscRain6min$hour <- floor_date(lscRain6min$dateTime,"hours")
lscRainHourly1 <- lscRain6min[, lapply(.SD, sum, na.rm=TRUE), by=hour, .SDcols=c("Rain_depth","ET") ]
lsc_rain_hourly <- lscRainHourly1
names(lsc_rain_hourly) <- c("datetime","rain_mm","et_mm")
save(lsc_rain_hourly, file = "data/lsc_rain_hourly.rda", compress = "xz")


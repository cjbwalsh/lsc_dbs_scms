#See ../lsc_foundation/build_postgres_scmdb_from_mysql.R for source code for the postgres database lsc_dbs_scms

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



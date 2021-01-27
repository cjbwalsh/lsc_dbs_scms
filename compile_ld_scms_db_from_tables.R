# This file contains code and instructions for building a postgreSQL database from the 
# tables in the Open Science Framework repository https://osf.io/57azq/ (Walsh et al. 2021) 
# and its associated github repository https://github.com/cjbwalsh/lsc_dbs_scms.
# These instructions should work on Linux and Mac OSX (not tested on Windows), 
# if the github repository has been cloned to the working computer,
# followed by running the following line from the lsc_dbs_scms directory
## source("load_ld_scms_tables.R").
# See load_ld_scms_tables.R for instructions on cloning the github repository.

# Walsh, C. J., Burns, M. J., Fletcher, T. D., Bos, D. G., Kunapo, J., Poelsma, P., & Imberger, S. J. (2021), 
# Linking stormwater control performance to stream ecosystem outcomes: incorporating performance metrics into 
# effective imperviousness/Data and code, Open Science Framework. https://osf.io/57azq

#To install postgreSQL on a Mac, use the installer at https://www.postgresql.org/download/macosx/
#Be sure to add the spatial extension PostGIS
#
#Having installed postgreSQL, you need to set up two user accounts for the following
#scripts (and those in mwstr_functions.R) to work.
#1. A readonly account (name 'readonly')
#2. An account with administrative rights with the same user name (called [admin-user-name] below) 
#   as you use to log into the computer you are installing postgres on 
#   In a terminal...
#1. Create new psql user readonly and set up .pgpass (if you have a superuser account)
#   local_dir:~$ psql mwstr_dev                                            
#
# [admin-user-name]@local_dir:~$ sudo -u [admin-user-name] createuser readonly

# postgres=# alter user [admin-user-name] with encrypted password 'INSERT_STRONG_PASSWORD_HERE';
#   postgres=#  alter user [admin-user-name] with SUPERUSER;
#   postgres=# \q
#   
#   Use a text editor (e.g. nano or gedit in linux to create a new file called .pgpass 
#   in the home directory of [admin-user-name] directory 
#   Add the following text to this file in one line (replacing username and password)
#   localhost:5432:*:[admin-user-name]:INSERT_STRONG_PASSWORD_HERE
#   
# change permissions of .pgpass so that they can only be read by the [admin-user-name]
# run the following two lines in a terminal
# ~$ touch ~/.pgpass
# ~$ chmod 0600 ~/.pgpass
# 
# Check connection works
# ~$ psql mwstr -w -U sm
#   mwstr=# \dt
#   mwstr=# \q
#   
#   Now try in R
# drv <- DBI::dbDriver("PostgreSQL")
# mwstr_db <- RPostgreSQL::dbConnect(drv, dbname = "mwstr_dev")

#Now create the new database ld_scms and add useful extensions (definitely need postgis)
# ~$ sudo -u postgres psql postgres
# postgres=# \list  #to check existing databases
# postgres=# CREATE DATABASE ld_scms;
# postgres=# \q
# psql -d ld_scms -c "CREATE EXTENSION postgis;"
# psql -d ld_scms -c "CREATE EXTENSION pg_trgm;"
# psql -d ld_scms -c "CREATE EXTENSION fuzzystrmatch;"
# psql -d ld_scms -c "GRANT SELECT ON ALL TABLES IN SCHEMA public TO readonly;"
# psql -d ld_scms -c "ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO readonly;"

# Read the tables from github.com/cjbwalsh/lsc_dbs_scms/data (assumes getwd = lsc_dbs_scms, cloned from github)
load(here::here("data","db_non_sf.rda"))
gpkg_files <- dir(here::here("data"))[grep("gpkg",dir(here::here("data")))]
for(i in 1:length(gpkg_files)){
  temp <- temp1 <- sf::st_read(here::here("data",paste(gpkg_files[i],sep = "")), 
                               stringsAsFactors = FALSE, quiet = TRUE)
  sf::st_geometry(temp) <- NULL
  temp1 <- sf::st_sf(temp, geometry = temp1$geom)
  assign(gsub(".gpkg","",gpkg_files[i]),temp1)
}

#put these into a postgres database.
drv <- DBI::dbDriver("PostgreSQL")
scm_db <- RPostgreSQL::dbConnect(drv, dbname = "ld_scms")
# RPostgreSQL::dbSendQuery(scm_db, "DROP TABLE ia;")
# RPostgreSQL::dbSendQuery(scm_db, "DROP TABLE cats;")
# RPostgreSQL::dbSendQuery(scm_db, "DROP TABLE sites;")
# RPostgreSQL::dbSendQuery(scm_db, "DROP TABLE dds;")
# RPostgreSQL::dbSendQuery(scm_db, "DROP TABLE filtprofs;")
# RPostgreSQL::dbSendQuery(scm_db, "DROP TABLE metadata;")
# RPostgreSQL::dbSendQuery(scm_db, "DROP TABLE \"parcelchanges\";")
# RPostgreSQL::dbSendQuery(scm_db, "DROP TABLE raingardens;")
# RPostgreSQL::dbSendQuery(scm_db, "DROP TABLE \"scmchanges\";")
# RPostgreSQL::dbSendQuery(scm_db, "DROP TABLE \"scmsdecommissioned\";")
# RPostgreSQL::dbSendQuery(scm_db, "DROP TABLE \"tanks\";")
# RPostgreSQL::dbSendQuery(scm_db, "DROP TABLE parcels;")
# RPostgreSQL::dbSendQuery(scm_db, "DROP TABLE scms;")
# RPostgreSQL::dbSendQuery(scm_db, "DROP TABLE \"scmprojects\";")
# RPostgreSQL::dbSendQuery(scm_db, "DROP TABLE subcs;")
#Some queries used to check validity of ia data...
#RPostgreSQL::dbGetQuery(scm_db, "SELECT DISTINCT \"constructionDate\" FROM ia;")
#RPostgreSQL::dbGetQuery(scm_db, "SELECT MAX(\"polyID\") FROM ia;")
#ia <- RPostgreSQL::dbGetQuery(scm_db, "SELECT * FROM ia;")
# RPostgreSQL::dbGetQuery(scm_db, "SELECT column_name, data_type FROM information_schema.columns WHERE table_name = 'scmsdecommissioned';")
# RPostgreSQL::dbGetQuery(scm_db, "SELECT DISTINCT date FROM parcelchanges;")

sf::st_write(ia, scm_db, "ia")
RPostgreSQL::dbGetQuery(scm_db, "ALTER TABLE ia ALTER COLUMN \"constructionDate\" TYPE DATE;")
sf::st_write(parcels, scm_db, "parcels")
sf::st_write(subcs, scm_db, "subcs")
sf::st_write(scms, scm_db, "scms")
sf::st_write(cats, scm_db, "cats")
sf::st_write(sites, scm_db, "sites")
sf::st_write(streams, scm_db, "streams")

RPostgreSQL::dbWriteTable(conn = scm_db, name = 'dds', value = dds, row.names = FALSE)
RPostgreSQL::dbWriteTable(conn = scm_db, name = 'filtprofs', value = filtprofs, row.names = FALSE)
RPostgreSQL::dbWriteTable(conn = scm_db, name = 'metadata', value = metadata, row.names = FALSE)
RPostgreSQL::dbWriteTable(conn = scm_db, name = 'parcelchanges', value = parcelChanges, row.names = FALSE)
RPostgreSQL::dbWriteTable(conn = scm_db, name = 'raingardens', value = raingardens, row.names = FALSE)
RPostgreSQL::dbWriteTable(conn = scm_db, name = 'scmchanges', value = scmChanges, row.names = FALSE)
RPostgreSQL::dbGetQuery(scm_db, "ALTER TABLE scmchanges ALTER COLUMN \"changeDate\" TYPE DATE;")
RPostgreSQL::dbWriteTable(conn = scm_db, name = 'scmprojects', value = scmProjects, row.names = FALSE)
RPostgreSQL::dbGetQuery(scm_db, "ALTER TABLE scmprojects ALTER COLUMN \"installDate\" TYPE DATE;")
RPostgreSQL::dbWriteTable(conn = scm_db, name = 'scmsdecommissioned', value = scmsDecommissioned, row.names = FALSE)
RPostgreSQL::dbGetQuery(scm_db, "ALTER TABLE scmsdecommissioned ALTER COLUMN \"decommDate\" TYPE DATE;")
RPostgreSQL::dbWriteTable(conn = scm_db, name = 'tanks', value = tanks, row.names = FALSE)

RPostgreSQL::dbSendQuery(scm_db, "ALTER TABLE scms ADD PRIMARY KEY (\"scmID\");")
RPostgreSQL::dbSendQuery(scm_db, "CREATE UNIQUE INDEX CONCURRENTLY \"scmID\" ON scms (\"scmID\");")
RPostgreSQL::dbSendQuery(scm_db, "ALTER TABLE scmchanges ADD CONSTRAINT \"scmID\" FOREIGN KEY (\"scmID\") REFERENCES scms (\"scmID\");")
RPostgreSQL::dbSendQuery(scm_db, 
                         "ALTER TABLE scmsdecommissioned ADD CONSTRAINT \"scmID\" FOREIGN KEY (\"scmID\") REFERENCES scms (\"scmID\");")
RPostgreSQL::dbSendQuery(scm_db, "ALTER TABLE dds ADD CONSTRAINT \"scmID\" FOREIGN KEY (\"scmID\") REFERENCES scms (\"scmID\");")
RPostgreSQL::dbSendQuery(scm_db, "ALTER TABLE tanks ADD CONSTRAINT \"scmID\" FOREIGN KEY (\"scmID\") REFERENCES scms (\"scmID\");")
RPostgreSQL::dbSendQuery(scm_db, "ALTER TABLE raingardens ADD CONSTRAINT \"scmID\" FOREIGN KEY (\"scmID\") REFERENCES scms (\"scmID\");")

RPostgreSQL::dbSendQuery(scm_db, "ALTER TABLE scmprojects ADD PRIMARY KEY (\"projectID\");")
RPostgreSQL::dbSendQuery(scm_db, "CREATE UNIQUE INDEX CONCURRENTLY \"projectID\" ON scmprojects (\"projectID\");")
RPostgreSQL::dbSendQuery(scm_db, 
                         "ALTER TABLE scms ADD CONSTRAINT \"projectID\" FOREIGN KEY (\"projectID\") REFERENCES scmprojects (\"projectID\");")

RPostgreSQL::dbSendQuery(scm_db, "ALTER TABLE subcs ADD PRIMARY KEY (\"pipeID\");")
RPostgreSQL::dbSendQuery(scm_db, "CREATE UNIQUE INDEX CONCURRENTLY \"pipeID\" ON subcs (\"pipeID\");")
RPostgreSQL::dbSendQuery(scm_db, 
                         "ALTER TABLE scmProjects ADD CONSTRAINT \"pipeID\" FOREIGN KEY (\"pipeID\") REFERENCES subcs (\"pipeID\");")
RPostgreSQL::dbSendQuery(scm_db, 
                         "ALTER TABLE ia ADD CONSTRAINT \"pipeID\" FOREIGN KEY (\"pipeID\") REFERENCES subcs (\"pipeID\");")
RPostgreSQL::dbSendQuery(scm_db, 
                         "ALTER TABLE parcels ADD CONSTRAINT \"pipeID\" FOREIGN KEY (\"pipeID\") REFERENCES subcs (\"pipeID\");")

RPostgreSQL::dbSendQuery(scm_db, "ALTER TABLE parcels ADD PRIMARY KEY (\"parcelID\");")
RPostgreSQL::dbSendQuery(scm_db, "CREATE UNIQUE INDEX CONCURRENTLY \"parcelID\" ON parcels (\"parcelID\");")
RPostgreSQL::dbSendQuery(scm_db, 
                         "ALTER TABLE ia ADD CONSTRAINT \"parcelID\" FOREIGN KEY (\"parcelID\") REFERENCES parcels (\"parcelID\");")
RPostgreSQL::dbSendQuery(scm_db, 
                         "ALTER TABLE parcelchanges ADD CONSTRAINT \"parcelID\" FOREIGN KEY (\"parcelID\") REFERENCES parcels (\"parcelID\");")

RPostgreSQL::dbSendQuery(scm_db, "ALTER TABLE cats ADD PRIMARY KEY (\"sitecode\");")
RPostgreSQL::dbSendQuery(scm_db, "CREATE UNIQUE INDEX CONCURRENTLY \"sitecode\" ON cats (\"sitecode\");")
RPostgreSQL::dbSendQuery(scm_db, "ALTER TABLE sites ADD PRIMARY KEY (\"sitecode\");")
RPostgreSQL::dbSendQuery(scm_db, 
                         "ALTER TABLE cats ADD CONSTRAINT \"sitecode\" FOREIGN KEY (\"sitecode\") REFERENCES sites (\"sitecode\");")

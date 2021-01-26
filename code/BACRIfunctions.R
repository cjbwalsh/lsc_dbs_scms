# Set up ---------------------------------
### A collection of functions for use with the lsc_dbs_scm database
 
requiredPackages <- c("scales","dplyr","lubridate","sf","here","RPostgreSQL",
                      "mcr","RColorBrewer","data.table","RMySQL")
lapply(requiredPackages, require, character.only = TRUE, quietly = TRUE)
#assumes you are either in lsc_foundation directory or a sibling directory of its
#parent directory
modfunctions <- ifelse(grepl("lsc_foundation", getwd()),
                             here::here("code", "modelfunctions2017.R"),
                       "../lsc_foundation/code/modelfunctions2017.R")
source(modfunctions)

# Postgres helper functions ---------------------------------
# # Minor datatabase management functions for lsc_dbs_scm database
sqlQuery <- function (query,   #Syntactically correct SQL function
                      db = "lsc_dbs_scms", # or "mwstr","mwstr_dev,"mwbugs" or "lsc" or...  
                      type = "get", #send = dbSendQuery; get = dbGetQuery
                      user = "readonly" # or "root" - the latter will only work on water-dev for admin users
) {
  # creating DB connection object with RPostgreSQL package
  hostname <- ifelse(Sys.info()["nodename"] == "water-dev.its.unimelb.edu.au",
                     "localhost", "water-dev.its.unimelb.edu.au")
  if(db %in% c("lsc_dbs_scms","mwstr","mwbugs","mwstr_dev")){
    drv <- DBI::dbDriver("PostgreSQL")
  if(user == "readonly")
    conn <- RPostgreSQL::dbConnect(drv, dbname = db,
                                     host = hostname, port = 5432,
                                     user = "readonly", password = "reachcode_42")  
  if(user == "root")
    conn <- RPostgreSQL::dbConnect(drv, dbname = "lsc_dbs_scms")
  }
if(db %in% c("lsc")){
  if(user == "readonly")
    #doesn't seem to be working for non localhost connections...
    conn <- dbConnect(RMySQL::MySQL(), host = hostname, user = "readonly",
                      password = "reachcode_42", dbname="lsc", port = 3306)
  if(user == "root")  #Doesn't work if you don't have .my.cnf in your home directory
    conn <- dbConnect(RMySQL::MySQL(),dbname="lsc", group='WERG')
}
  # close db connection after function call exits
  on.exit(dbDisconnect(conn))
  # send Query to obtain result set
  if(type == "get"){
    if(db %in% c("lsc_dbs_scms","mwstr","mwbugs","mwstr_dev")){
    # but first, check if query concerns a spatial table
    spatialtabs <- as.vector(t(dbGetQuery(conn, 
                                          "select f_table_name from geometry_columns")))
    spatialTRUE <- 0
    for(i in 1:length(spatialtabs)){
      if(grepl(paste("from",spatialtabs[i]), tolower(query)) & !grepl("scmsdecommissioned", tolower(query))) 
        spatialTRUE <- spatialTRUE + 1
    }
    if(spatialTRUE == 0 | (spatialTRUE == 1 & !(grepl("SELECT [*]", query) | grepl("[*] FROM", query)))){
      rs <- dbGetQuery(conn, query)
    }
    if(spatialTRUE == 1 & (grepl("SELECT [*]", query) | grepl("[*] FROM", query))){
      rs <- sf::st_read(conn, query = query)
    }
    }else{
      rs <- dbGetQuery(conn, query)
    }
  }
  if(type == "send")
    rs <- dbSendQuery(conn, query)
  # return the result
  return(rs)
}

#disconnect connections if too many    
# drv <- DBI::dbDriver("PostgreSQL")
# dbDisconnect(RPostgreSQL::dbListConnections(drv)[[1]])
# dbDisconnectAll <- function(){
#   ile <- length(dbListConnections(MySQL())  )
#   lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
#   cat(sprintf("%s connection(s) closed.\n", ile))
# }

# prepare_runoff_data() ---------------------------------  
#' Prepare an R object containing rainfall, impervious runoff and evapotranspiration
#' data for use by BACRIfunctions.R. 
#' 
#' @param hourlydata a data.frame or data.table with an hourly 
#'   time series in a datetime field containing a vector of lubridate 
#'   values, and three fields containing rainfall, runoff and evapotranspiration 
#'   data
#' @param rain_fieldname text string giving name of field containing the rainfall
#'  data (default "rain_mm") in the input table
#' @param runoff_fieldname text string giving name of field containing the runoff
#'  data (default "runoff_mm") in the input table
#' @param et_fieldname text string giving name of field containing the 
#'  evapotranspiration data (default "et_mm") in the input table
#' @return A list of two data.tables, with requisite fields for the BACRI 
#'    functions: hourly (hourly data) and daily (daily data).
#' @examples
#' # # prepare data from Croydon 1965-66 data used in EB calculations
#' # Croydon <- prepare_runoff_data(get(load(paste(here::here("data"), 
#' #          "/Croydon_1966_hourly_rain_runoff_et.rda", sep = ""))))  

prepare_runoff_data <- function(hourly_data,
                                rain_fieldname = "rain_mm",
                                runoff_fieldname = "runoff_mm",
                                et_fieldname = "et_mm"){
  if(nrow(hourly_data) %% 24 != 0){
    stop("Number of rows is not a multiple of 24")
  }
  hourly_data <- data.frame(hourly_data)
  start_water_day <- lubridate::hour(hourly_data$datetime[1])
  water_date <- lubridate::date(hourly_data$datetime - lubridate::hours(lubridate::hour(hourly_data$datetime[1])))
  hourly <- data.table::data.table(data.frame(datetime = hourly_data$datetime,
                                              date = water_date,
                                              daten = as.numeric(water_date - lubridate::ymd("1900-01-01")),
                                              year = lubridate::year(water_date),
                                              month = lubridate::month(water_date),
                                              rain_mm = hourly_data[,rain_fieldname],
                                              runoff_mm = hourly_data[,runoff_fieldname],
                                              et_mm = hourly_data[,et_fieldname]))
  daily <- hourly[,lapply(.SD, sum, na.rm=TRUE), by=date, .SDcols=c("rain_mm","runoff_mm","et_mm")]
  daily$year <- lubridate::year(daily$date)
  daily$month <- lubridate::month(daily$date)
  daily$daten <- as.numeric(daily$date - lubridate::ymd("1900-01-01"))
  list(hourly = hourly, daily = daily)
}

# allupstream() ---------------------------------  
#' Find all subcatchments upstream of a site in a dendritic hierarchy  
#' 
#' @param hierarchy a data.frame or data.table with a unique 
#'    field (a subcatchment or reach code) with name specified by the 
#'    'subc' argument, and a 'nextds' field identifying the next 
#'    'subc' downstream
#' @param catchname a value in the 'subc' field for which all subcs 
#'     upstream are to be extracted
#'    'hierarchy'.  
#' @param subc a character string indicating the column name identifying each 
#'      subcatchment.  
#' @return A vector of all subcs upstream of (and including) 'catchname'.
#' @details adapted from the function with the same name in 
#' wergStaff/ChrisW/R functions/catchcompile.functions.R for use with the
#' spatial tables of the lsc_dbs_scms database
#' @examples
#' # # load tables from lsc_dbs_scms database, including the subcs table
#' # load("data/lscdbsSCMs_db.rda")
#' # # All sites upstream of pipeID 53
#' # allupstream(hierarchy = subcs, catchname = "53", subc = "pipeID")

allupstream <- function(hierarchy,catchname, subc = "site"){
  #Function that uses the nextds field in a table 'hierarchy'
  #to extract all sites upstream of a site 'catchname'
  #subc is the name given to the field of subcatchment IDs
  if("sf" %in% class(hierarchy))
    sf::st_geometry(hierarchy) <- NULL
  if(subc != "site")
    hierarchy$site <- as.vector(hierarchy[,subc])
  if(length(which(hierarchy$site == catchname)) > 0)
  {
    catchname <- as.vector(catchname)
    hierarchy$site <- as.vector(hierarchy$site)
    hierarchy$nextds <- as.vector(hierarchy$nextds)
    allsc <- as.vector(hierarchy$site[hierarchy$nextds==catchname])
    allsc <- allsc[!is.na(allsc)]
    #subcatchments immediately upstream
    nbrnch <- scEnd <- length(allsc)
    #number of branches immediately upstream
    sc1 <- 1
    while(nbrnch>0)
      for(i in 1:73)
      {
        for(i in sc1:scEnd)
        {
          allsc <- c(allsc,as.vector(hierarchy$site[hierarchy$nextds==allsc[i]]))
          allsc <- allsc[!is.na(allsc)]
        }
        sc1 <- scEnd + 1
        scEnd <- length(allsc)
        nbrnch <- scEnd - (sc1 - 1)
      }
    allsc <- c(catchname,allsc)
    allsc
  } else
    cat(paste(catchname,"is not a site listed in the hierarchy table","\n"))
}

# alldownstream() ---------------------------------
#' Find all subcatchments downstream of a site in a dendritic hierarchy  
#' 
#' @param hierarchy a data.frame or data.table with a unique 
#'    field (a subcatchment or reach code) with name specified by the 
#'    subc' argument, and a 'nextds' field identifying the next 
#'    'subc' downstream
#' @param catchname a value in the 'subc' field for which all subcs 
#'     upstream are to be extracted
#'    'hierarchy'.  
#' @param subc a character string indicating the column name identifying each 
#'      subcatchment.  
#' @return a vector of all subcs downstream of (and including) 'catchname.
#' @details adapted from the function with the same name in 
#' wergStaff/ChrisW/R functions/catchcompile.functions.R for use with the
#' spatial tables of the lsc_dbs_scms database
#' @examples
#' # # load tables from lsc_dbs_scms database, including the subcs table
#' # load("data/lscdbsSCMs_db.rda")
#' # # All sites upstream of pipeID 53
#' # alldownstream(hierarchy = subcs, catchname = "29", subc = "pipeID")

alldownstream <- function(hierarchy, catchname, subc = "site"){
    if("sf" %in% class(hierarchy))
      sf::st_geometry(hierarchy) <- NULL
    if(subc != "site")
      hierarchy$site <- as.vector(hierarchy[,subc])
    if(length(which(hierarchy$site==catchname))>0){
      catchname <- as.vector(catchname)
    allsc <- as.vector(hierarchy$nextds[hierarchy$site==catchname])
    allsc <- allsc[!is.na(allsc)]
    #subcatchments immediately upstream
    nbrnch <- end <- length(allsc)
    #number of branches immediately upstream
    start <- 1
    while(nbrnch > 0 & !-1 %in% allsc)
    {
      for(j in start:end)
      {
        allsc <- c(allsc,as.vector(hierarchy$nextds[hierarchy$site == allsc[j]]))
        allsc <- allsc[!is.na(allsc)]
      }
      start <- end + 1
      end <- length(allsc)
      nbrnch <- end - (start - 1)
    }
    allsc <- c(catchname,allsc)
    allsc <- allsc[allsc != -1]
    allsc
  } else{
    cat(paste(catchname,"is not a site listed in the hierarchy table","\n"))
  }
  }

# scmMap() ---------------------------------  
#' Draw a map showing all stormwater control measures (SCMs) and their 
#' relationships in a specified subcatchment
#' 
#' @param pipeID the value in the 'pipeID' field in the subcs table to be 
#'    plotted.  
#' @param addLegend logical, if TRUE, legends of symbols are plotted
#' @param addSCMNetwork logical, if TRUE, connecting lines between upstream and
#'    downstream SCMs are plotted
#' @param ZoomToSCMs logical, if TRUE, the plot bounds are specified by the 
#'    distribution of SCMs rather than the subcatchment boundary  
#' @return A plot.
#' @details requires the following tables from lsc_dbs_SCMs database to be in 
#'    the environment: SCMs (renamed from scms), subcs, parcels, ia, streams
#' @examples
#' # # load tables from lsc_dbs_scms database, including all required tables
#' # load("data/lscdbsSCMs_db.rda")
#' # SCMs <- scms; rm(scms)
#' # scmMap(29)

scmMap <- function(pipeID, addLegend = TRUE,addSCMNetwork = TRUE, ZoomToSCMs = FALSE){
  par(mar = c(0,0,0,0))
  allsubcs <- allupstream(as.data.frame(subcs), pipeID, "pipeID")
  SCMsi <- which(SCMs$pipeID %in% allsubcs)
  if(ZoomToSCMs){
    plot(SCMs$geometry[SCMsi], , pch = 21, type = 'n')
    plot(subcs$geometry[subcs$pipeID %in% allsubcs], lty = 2, lwd = 2, border = "darkgreen", add = TRUE)
  }else{
    plot(subcs$geometry[subcs$pipeID %in% allsubcs], lty = 2, lwd = 2, border = "darkgreen")
  }
  plot(parcels$geometry[parcels$pipeID %in% allsubcs], lty = 1, lwd = 1, border = gray(0.75), add = TRUE)
  plot(streams$geometry, lty = 1, lwd = 2, col = "darkblue", add = TRUE)
  plot(ia$geometry[ia$pipeID %in% allsubcs], lty = 1, lwd = 1,
       border = c("#FFCC99","#660000")[match(ia$conn[ia$pipeID %in% allsubcs],0:1)],
       col = c(scales::alpha("#FFCC99",0.5), scales::alpha("#660000",0.5))[match(ia$conn[ia$pipeID %in% allsubcs],0:1)], add = TRUE)
  plot(SCMs$geometry[SCMsi], , pch = 21, bg = c("red","blue","yellow")[match(SCMs$type[SCMsi],c("tank","rg","dd"))], add = TRUE)
  if(addSCMNetwork){
    SCMs$temp <- 1
  for(i in which(SCMs$pipeID %in% allsubcs))
  {
    if(!is.na(SCMs$nextds[i]) & !(SCMs$nextds[i] %in% c("land","stormwater"))){
    x <- SCMs %>%
      slice(c(i,
              which(SCMs$scmID == SCMs$nextds[i]))) %>% group_by(temp) %>%
                    summarize(m = mean(as.numeric(pipeID))) %>% st_cast("LINESTRING")
    plot(x$geometry, add = TRUE)
    }
    }
}
  if(addLegend){
    legend("topright", pch = 21, pt.bg = c("red","blue","yellow"), legend = c("Tank","Raingarden","Dpipe diverter"))
    legend("bottomright", pch = 22, pt.bg = c(scales::alpha("#FFCC99",0.5), scales::alpha("#660000",0.5)),
           col = c("#FFCC99","#660000"),
           title = "Imp surfaces", legend = c("Unconnected","Connected"))
  }
}

# data_on_datex() ---------------------------------
#' Assemble ia and scm data for a specified date (from lsc_dbs_scms database)
#' @param pipeID the subcatchment for which data to be assembled (including all
#'    subcatchments upstream). Must be a value taken from the pipeID field of 
#'    table subcs.
#' @param datex a date object or a text string in the form "y-m-d" 
#' @return a list of tables required for calculating water balance statistics for
#'     impervious surfaces upstream of pipeID: subcs, parcels, ia, scmProjects,
#'     SCMs, tanks, raingardens, and dds.
#' @details Requires all tables from lsc_dbs_scms database to be loaded into 
#'    the environment. In addition to information in the database, this function
#'    also takes into account alterations to the network resulting from two 
#'    public SCMs (King St which diverts pipeID 75 to flow to pipeID 30 rather
#'    than pipeID 31; and The Entrance, which diverts pipeID 69 to flow to pipeID
#'    61 rather than pipeID 35)
#' @examples
#' # # load tables from lsc_dbs_scms database, including all required tables
#' # load("data/lscdbsSCMs_db.rda")
#' # db29 <- data_on_datex(29, "2018-01-01")

data_on_datex <- function(pipeID, datex){
  #1. Change subc network structure if King St or the Entrance were operational and relevant.
  subcsx <- subcs; kingst <- theentrance <- FALSE
  if(pipeID %in% unique(c(alldownstream(subcs, 30, "pipeID"),alldownstream(subcs, 31, "pipeID"))) & 
     (datex >= "2016-07-15" & datex < "2016-10-14") | #commissioning to first taking offline
     (datex >= "2017-03-17" & datex < "2017-09-15") | #First erosion problems fixed
     (datex >= "2018-02-05" & datex < "2019-06-02")){ #Second erosion problems addressed
     #dates from email from Patrick Jeschke 8 March 2018 (King St system turned off for some months to address flooding concerns)
     #finally taken offline ~2 June 2019 - see email from Patrick 6 June 2019 forwarded by Darren Bos 7 June 2019.
    subcsx$nextds[subcsx$pipeID == 75] <- 30
    kingst <- TRUE
  }
  if(pipeID %in% alldownstream(subcs, 69, "pipeID") & datex >= "2015-12-04"){
    subcsx$nextds[subcsx$pipeID == 69] <- 61
    theentrance <- TRUE  #No need to use this later, as in kingst, as there are no upstream SCMs in pipeID 69,
    #but I'll do it anyway, in case some SCMs are subsequently installed upstream.... (see below)
  }
  #2. restrict to subcs and parcels upstream of pipeID
  subcsx <- subcsx[subcsx$pipeID %in% 
                   allupstream(subcsx, pipeID,"pipeID"),]
  parcelsx <- parcels[parcels$pipeID %in% subcsx$pipeID,]
  #3. restrict ia to those subcs and correct for parcelChanges before datex
  iax <- ia[ia$pipeID %in% subcsx$pipeID & (is.na(ia$constructionDate) | 
              ia$constructionDate < datex),]
  pcx <- parcelChanges[parcelChanges$pipeID %in% subcsx$pipeID & 
                         parcelChanges$date < datex,]
   ## For LSC, all but two constructions are noted both as construction date in the ia table and as 
   ## an identified polyID in the parcelChanges table.  Thus any such entries in parcelChanges can
   ## be removed. (The other two have polyID NA)
     ## remove ia demolished before datex
  pcx <- pcx[is.na(pcx$polyID) | pcx$pipeID > 100,]
  pcx_constructions <- pcx[pcx$changeType == "construction",]
  if(dim(pcx_constructions)[1] > 0){
    #add any constructions assigned to parcelID rather than polyID 
    #as a circular polygon in the centre of the parcel
    #sf::st_geometry(iax) <- NULL
    for(i in 1:dim(pcx_constructions)[1]){
      ##If you want spatial data including these additional polygons...
      parci_centroid <- suppressWarnings(sf::st_centroid(parcelsx[parcelsx$parcelID == pcx$parcelID[i],]))
      polyi <- st_buffer(parci_centroid, dist = sqrt(pcx$area_m2[i]/pi))
      ##else
      polyi$polyID <- 20000 + i
      polyi$conn <- pcx$conn[i]
      polyi$area_m2 <- pcx$area_m2[i]
      polyi$parcType <- polyi$parcelType
      polyi$constructionDate <- pcx$date[i]
      polyi$surfType <- "roof" #almost always true. not important for ei calcs
      polyi <- polyi[c("polyID","address","pipeID","conn","nextds","area_m2",
                       "parcType","surfType","constructionDate","parcelID")] #see above if... #,"geometry")]
      iax <- rbind(iax, polyi)
    }
    }
  pcx_demolitions <- pcx[pcx$changeType == "demolition",]
  if(dim(pcx_demolitions)[1] > 0)
    iax <- iax[!iax$polyID %in% pcx_demolitions$polyID,]
   ## connect ia that was connected before datex
  pcx_connections <- pcx[pcx$changeType == "connection",]
  if(dim(pcx_connections)[1] > 0)
    iax$conn[iax$polyID %in% pcx_connections$polyID] <- 1
  #4. restrict scmProjects to those subcs and correct for parcelChanges before datex
  scmProjectsx <- scmProjects[scmProjects$pipeID %in% subcsx$pipeID &
                                scmProjects$installDate < datex,]
  #5. restrict SCMs to relevant scmProjects and update specs according to scmChanges
  if(dim(scmProjectsx)[1] > 0){
    SCMsx <- SCMs[SCMs$projectID %in% scmProjectsx$projectID,]
    scx <- scmChanges[scmChanges$scmID %in% SCMsx$scmID & 
                        scmChanges$changeDate < datex & 
                        scmChanges$param != "INFO",]
    scx <- scx[order(scx$changeDate),]
    #INFO records 2 changes to PL-8 - likely minimal change to hydrologic performance
    if(sum(duplicated(paste(scx$scmID,scx$param))) > 0){
      id_param <- unique(paste(scx$scmID,scx$param)[duplicated(paste(scx$scmID,scx$param))])
      #if same param of same SCM changed more than once, take the most recent change 
      # (and if param is area or vol, make the most recent value the sum)
      for(i in 1:length(id_param)){
        if(unique(scx$param[paste(scx$scmID,scx$param) == id_param[i]]) %in% c("tankvol","tankCarea")){
          scx$newValue[paste(scx$scmID,scx$param) == id_param[i]] <- 
            sum(scx$newValue[paste(scx$scmID,scx$param) == id_param[i]])
        }
        scx <- scx[-max(which(paste(scx$scmID,scx$param) == id_param[i])),]
      }
    }
  #5a.  Ensure next SCM downstream is in the set of SCMs (it may not be if the recorded nextds SCM was not constructed before datex)
        #If not, change the nextds value to the next downstream SCM in the set that existed on datex
    for(i in 1:dim(SCMsx)[1]){
      if(!is.na(SCMsx$nextds[i]) & !SCMsx$nextds[i] %in% SCMsx$scmID & SCMsx$nextds[i] != "land"){
        while(!is.na(SCMsx$nextds[i]) & !SCMsx$nextds[i] %in% SCMsx$scmID){
          SCMsx$nextds[i] <- SCMs$nextds[SCMs$scmID == SCMsx$nextds[i]]
          }
      }
    }
    tanksx <- tanks[tanks$projectID %in% scmProjectsx$projectID,]
    raingardensx <- raingardens[raingardens$projectID %in% scmProjectsx$projectID,]
    ddsx <- dds[dds$projectID %in% scmProjectsx$projectID,]
    #correct if King St is operational (see above), and if not operational remove from data (necessary because of its on-offness)
    if(kingst){
    SCMsx$nextds[SCMsx$pipeID == 75 & !is.na(SCMsx$nextds) & SCMsx$nextds == "RPLJ001"] <- "RPLR553"
    }else{
      torm <- SCMsx$scmID[SCMsx$projectID == "PL-75-King Street Upper"]
      raingardensx <- raingardensx[!raingardensx$scmID %in% torm,]
      SCMsx <- SCMsx[!SCMsx$scmID %in% torm,]
      scmProjectsx <- scmProjectsx[scmProjectsx$projectID != "PL-75-King Street Upper",]
    }
    #correct if The Entrance is operational (although at time of writing, there are no SCMs upstream of RPLR548)
    if(theentrance){
      SCMsx$nextds[SCMsx$pipeID == 69 & SCMsx$scmID != "RPLR548"] <- "RPLR548"
    }
  #5b. Make any scmChanges that occurred before date.
    if(dim(scx)[1] > 0){
      for(i in 1:dim(scx)[1]){
        if(sum(tanksx$scmID == scx$scmID[i]) ==  1){
          nv <- ifelse(scx$param[i] %in% c("hotwater","toilet","wmac","isveg"), 
                        as.logical(scx$newValue[i]),scx$newValue[i])
          tanksx[tanksx$scmID == scx$scmID[i],scx$param[i]] <- nv
          }
        if(sum(raingardensx$scmID == scx$scmID[i]) == 1)
          raingardensx[raingardensx$scmID == scx$scmID[i],scx$param[i]] <- scx$newValue[i]
        if(sum(ddsx$scmID == scx$scmID[i]) == 1)
          ddsx[raingardensx$scmID == scx$scmID[i],scx$param[i]] <- scx$newValue[i]
      }
    }
    #pipeID 30 is not well delineated and its impervious surfaces drain to two separate
    #PL systems. Drainage relationships are reliably indicated for this catchment by
    #the nextds field in the ia table.
    raingardensx$impCarea[raingardensx$scmID == "RPNR502"] <- sum((iax$area_m2*iax$conn)[!is.na(iax$nextds) & iax$nextds == "RPNR502"])
    #5c. If there are any public SCMs that capture all pipeID impervious runoff
    #upstream, then adjust their catchment areas to account for SCMs upstream
    PLs <- scmProjects[is.na(scmProjects$address),]
    PLs <- PLs[!PLs$projectID %in% c("PL-8-Pembroke (Morrisons Reserve)",
                                     "PL-24-Petrol Station"),]
    # 3 PLs with SCM receiving all pipeID runoff not the most upstream SCM
    PL_scms <- c("RPLR544", #terminal rg at Primary school
                 "RPLT537", #main tank at Petrol Station
                 "RPLT542") #main tank at Pembroke
    #For the rest, most upstream SCM receives the catchment runoff....
    for(i in 1:dim(PLs)[1]){
      scmspli <- SCMs[SCMs$projectID == PLs$projectID[i],]
      PL_scms <- c(PL_scms,scmspli$scmID[!scmspli$scmID %in% scmspli$nextds])
                 }
    if(sum(SCMsx$scmID %in% PL_scms) > 0){
      PL_scmsx <- SCMsx[SCMsx$scmID %in% PL_scms,]
      for(i in 1:dim(PL_scmsx)[1]) 
          PL_scmsx$nus[i] <- length(allupstream(subcsx,PL_scmsx$pipeID[i],"pipeID"))
      PL_scmsx <- PL_scmsx[order(PL_scmsx$nus),]
      for(i in 1:dim(PL_scmsx)[1]) {
        us_scmsi <- allupstream(SCMsx, PL_scmsx$scmID[i],"scmID")[-1]
        us_subcs <- allupstream(subcsx, PL_scmsx$pipeID[i], "pipeID")
            direct_ia <- sum((iax$area_m2*iax$conn)[iax$pipeID %in% us_subcs]) - 
            sum(raingardensx$impCarea[raingardensx$scmID %in% us_scmsi]) -
            sum(tanksx$tankCarea[tanksx$scmID %in% us_scmsi]) - 
            sum(ddsx$ddCarea[ddsx$scmID %in% us_scmsi])
      if(direct_ia < 0) stop()
        if(PL_scmsx$type[i] == "rg"){
          raingardensx$impCarea[raingardensx$scmID == PL_scmsx$scmID[i]] <- direct_ia
        }
        if(PL_scmsx$type[i] == "tank"){
          tanksx$tankCarea[tanksx$scmID == PL_scmsx$scmID[i]] <- direct_ia
        }
      }
    }
  }else{
    SCMsx <- SCMs[0,]
    tanksx <- tanks[0,]
    raingardensx <- raingardens[0,]
    ddsx <- dds[0,]
  }
  list(subcs = subcsx, parcels = parcelsx, ia = iax, scmProjects = scmProjectsx,
       SCMs = SCMsx, tanks = tanksx, raingardens = raingardensx, dds = ddsx)
}

# data_for_scm() ---------------------------------
#' Assemble ia and scm data for a specified date (from lsc_dbs_scms database)
#' @param pipeID the subcatchment for which data to be assembled (including all
#'    subcatchments upstream). Must be a value taken from the pipeID field of 
#'    table subcs.
#' @param datex a date object or a text string in the form "y-m-d" 
#' @return a list of tables required for calculating water balance statistics for
#'     impervious surfaces upstream of pipeID: subcs, parcels, ia, scmProjects,
#'     SCMs, tanks, raingardens, and dds.
#' @details Requires all tables from lsc_dbs_scms database to be loaded into 
#'    the environment. In addition to information in the database, this function
#'    also takes into account alterations to the network resulting from two 
#'    public SCMs (King St which diverts pipeID 75 to flow to pipeID 30 rather
#'    than pipeID 31; and The Entrance, which diverts pipeID 69 to flow to pipeID
#'    61 rather than pipeID 35)  
#' @examples
#' # #load tables from lsc_dbs_scms database, including all required tables
#' # load("data/lscdbsSCMs_db.rda")
#' # db29 <- data_on_datex(29, "2018-01-01")

data_for_scm <- function(scmID, fin_date = "2019-12-31"){
  #1. PL scms
  PLs <- scmProjects[is.na(scmProjects$address) | scmProjects$address == "33-35 MONBULK ROAD",]
  scmi <- SCMs[SCMs$scmID == scmID,]
  if(scmi$projectID %in% PLs$projectID){
    allusi <- allupstream(subcs, scmi$pipeID, "pipeID")
    if(scmi$pipeID %in% c(30,31,52) & fin_date > "2016-07-15")
      allusi <- unique(c(allusi,75))
    subcsx <- subcs[subcs$pipeID %in% allusi,]
    parcelsx <- parcels[parcels$pipeID %in% subcsx$pipeID,]
    iax <- ia[ia$pipeID %in% subcsx$pipeID & (is.na(ia$constructionDate) | 
                                              ia$constructionDate < fin_date),]
    pcx <- parcelChanges[parcelChanges$pipeID %in% subcsx$pipeID & 
                         parcelChanges$date < fin_date,]
    scmProjectsx <- scmProjects[scmProjects$pipeID %in% subcsx$pipeID &
                                scmProjects$installDate < fin_date,]
    SCMsx <- SCMs[SCMs$projectID %in% scmProjectsx$projectID,]
    scx <- scmChanges[scmChanges$scmID %in% SCMsx$scmID & 
                        scmChanges$changeDate < fin_date & 
                        scmChanges$param != "INFO",]
    scx <- scx[order(scx$changeDate),]
    sdx <- scmsDecommissioned[scmsDecommissioned$parcelID %in% parcelsx$parcelID & scmsDecommissioned$decommDate < fin_date,]
    }else{
      subcsx <- subcs[subcs$pipeID == scmi$pipeID,]
      allusi <- allupstream(SCMs, scmi$scmID, "scmID")
      SCMsx <- SCMs[SCMs$scmID %in% allusi,]
      scmProjectsx <- scmProjects[scmProjects$projectID %in% SCMsx$projectID,]
      parcelsx <- parcels[parcels$address %in% SCMsx$address,]
      iax <- ia[ia$parcelID %in% parcelsx$parcelID,]
      pcx <- parcels[0,] #for non-PLs, parcel changes irrelevant...
      scx <- scmChanges[scmChanges$scmID %in% SCMsx$scmID & scmChanges$changeDate < fin_date,]
      sdx <- scmsDecommissioned[scmsDecommissioned$scmID %in% SCMsx$scmID & scmsDecommissioned$decommDate < fin_date,]
    }
    tanksx <- tanks[tanks$projectID %in% scmProjectsx$projectID,]
    raingardensx <- raingardens[raingardens$projectID %in% scmProjectsx$projectID,]
    ddsx <- dds[dds$projectID %in% scmProjectsx$projectID,]
  list(subcs = subcsx, parcels = parcelsx, ia = iax, scmProjects = scmProjectsx,
       SCMs = SCMsx, tanks = tanksx, raingardens = raingardensx, dds = ddsx,
       parcelChanges = pcx, scmChanges = scx, scmsDecommissioned = sdx)
}

# ia_ts() ---------------------------------
#' Calculate a daily time series of total and effective impervious area upstream
#'  of a given subcatchment
#' 
#' @param pipeID the value in the 'pipeID' field in the subcs table to be 
#'    used.  
#' @param start_date a date object or a text string in the form "y-m-d", specifying 
#'    the start date of the time series
#' @param fin_date a date object or a text string in the form "y-m-d", specifying the 
#'    end date of the time series
#' @return a data.frame with fields date, carea (catchment area, invariate for 
#'    most subcatchments, except for those altered by King St Upper or the Entrance
#'    raingardens), tia (total impervious area in m2) and eia (effective ia), 
#'    with a row for every day between start_date and fin_date.
#' @details requires the following tables from lsc_dbs_SCMs database to be in 
#'    the environment: subcs, parcelChanges, ia, scmProjects, scmChanges,
#'    tanks, raingardens.
#' @examples
#' # # load tables from lsc_dbs_scms database, including all required tables
#' # load("data/lscdbsSCMs_db.rda")
#' # iats_31 <- ia_ts(31) #subcatchment area affected by King St upper being turned on and off...
#' # plot(iats_31$date, iats_31$eia, type = 'l')

ia_ts <- function(pipeID,
                  start_date = "2001-01-01",
                  fin_date = "2019-12-31"){
  start_date <- lubridate::ymd(start_date)
  fin_date <- lubridate::ymd(fin_date)
  db <- data_on_datex(pipeID, fin_date) 
  change_dates <- c(db$ia$constructionDate[!is.na(db$ia$constructionDate)],
                    parcelChanges$date[parcelChanges$address %in% db$parcels$address])
  if(pipeID %in% c(30,31,52) &
     #Network change resulting from King St upper
     fin_date >= "2016-10-14" & start_date <= "2019-06-02"){ #periods of on-off for King St Upper
    change_dates <- c(change_dates, lubridate::ymd(c("2016-07-15","2016-10-14","2017-03-17","2017-09-15",
                      "2018-02-05","2019-06-02")))
    db1 <- data_on_datex(pipeID, "2019-06-01") #last date King St upper was operational
    change_dates <- c(change_dates, db1$ia$constructionDate[!is.na(db1$ia$constructionDate)],
                      parcelChanges$date[parcelChanges$address %in% db1$parcels$address])
  }
  if(pipeID %in% c(36,61) & start_date >= "2015-12-04"){
    #Network change resulting from The Entrance RG
    db1 <- data_on_datex(pipeID, "2015-12-05") #installDate for The Entrance RG
    change_dates <- c(change_dates, db1$ia$constructionDate[!is.na(db1$ia$constructionDate)],
                      parcelChanges$date[parcelChanges$address %in% db1$parcels$address])
  }
  change_dates <- unique(change_dates)
  change_dates <- change_dates[change_dates >= start_date & change_dates < fin_date]
  change_dates <- change_dates[order(change_dates, decreasing = FALSE)]
  db <- data_on_datex(pipeID, start_date) 
  ia_ts <- data.table::data.table(date = seq.Date(start_date, fin_date, by = "days"),
                      carea = sum(db$subcs$scarea), 
                      tia = sum(db$ia$area_m2), 
                      eia = sum(db$ia$area_m2*db$ia$conn))
  if(length(change_dates) > 0){
  for(i in 1:length(change_dates)) {
    db <- data_on_datex(pipeID, lubridate::ymd(change_dates[i] + days(1))) 
    ia_ts[date >= change_dates[i], 
          `:=` (carea = sum(db$subcs$scarea),
                tia = sum(db$ia$area_m2),
                eia = sum(db$ia$area_m2*db$ia$conn))]
  }
    }
  ia_ts
}

# Zhang64ExfET() ---------------------------------
#' Estimate likely evapotranspiration and exfiltration using the Zhang curve from 
#' a garden with elevated effective rainfall from a leaky tank
Zhang64ExfET <- function(tanki,tankBudget,mrm = mean.annrain.mm){
  annual.eff.rainfall <-
    sum(tankBudget$leak)/tanki$soil.area.receiving.leak + mrm
  Zhang.leak.etprop.old <-
    (1 + 2820/mrm)/(1 + 2820/mrm + mrm/1410)
  Zhang.leak.etprop <-
    (1 + 2820/annual.eff.rainfall)/(1 + 2820/annual.eff.rainfall + annual.eff.rainfall/1410)
  ET <- sum(tankBudget$leak) * Zhang.leak.etprop
  exf <- sum(tankBudget$leak) - ET
  list(ET = ET, exf = exf)
}

# calcTankBudget() ---------------------------------
#' Calculate tank budget from tanks table data in lsc_dbs_scm database
#' @param tanki a single row from the tanks table
#' @param tankbegin volume of water in the tank at timestep 1, default is half
#'     the capacity of the tank
#' @param runoffData rainfall and runoff time series used for EB calculation. 
#'     Default is Croydon loaded above.
#' @param additional.inflow vector of runoff volume in L/d equivalent in length to 
#'     that of the runoff vector in runoffData.
#' @param leakMonths vector of months (1 = Jan to 12 = Dec) in which the tank
#'     leaks (not specified in database: all continuous, except for Pembroke, 
#'     which is dealt with by noting leak behaviour in scmChanges).
#' @param ... additional arguments to tankmodel()
#' @return 
#'     A list with two objects: hourly_budget (a data.table, if the SCM leaks to 
#'     stormwater; if not this object is NA), and daily_budget, a data.table. 
#'     Both tables contain water budget items (inflow, use, out [a leak
#'     flowing directly to stormwater], exf [volume assumed to infiltrate into
#'     groundwater as a result of leak], et [volume assumed to be lost to evapo-
#'     transpiration as a result of leak], overflow, store and void) for each 
#'     date in inflow vector (runoffData$dro1y$runoff.mm.d for daily, 
#'     runoffData$hro1y$runoff.mm for hourly). tank budgets also contain a 
#'     'leak' column (see details)
#' @details 
#'     This function uses gardenmodel() from modelfunctions2017.R. For subsequent
#'     analyses, leaks from 'leaky' tanks need to be partitioned into one of out,
#'     et (evapotranspiration) or (exfiltration).  Out is flow down a stormwater 
#'     pipe from an SCM: for raingardens this is flow into an under-
#'     drain, but for some tanks (identified by leakTo = "stormwater" or 
#'     "filtered to stormwater"), the leak flows directly to the drain. 'Out' flows
#'     need to be calculated in hourly time steps because they can flow into 
#'     downstream raingardens that need to be modelled in hourly time steps. Thus
#'     if leakTo contains "stormwater", then the tankmodel() function is run on
#'     hourly timesteps (otherwise daily).  (Daily overflows from tanks upstream
#'     tanks are converted to hourly using a separate function that is not applicable
#'     to leaks: see budget_scm_on_datex()).  Leaks that drain passively to 
#'     gardens are partitioned into et and exf. These losses are used in EB 
#'     calculations at an annual scale, so we partition these at that scale using
#'     the Zhang (2001) curve.  
#' @examples
#' # # load tables from lsc_dbs_scms database, including all required tables
#' # load("data/lscdbsSCMs_db.rda")
#' # calcTankBudget(tanks[tanks$scmID == "R1T020",])

calcTankBudget <- function(tanki,
                           tankbegin = tanki$tankvol/2,#A single row from the tanks table
                           runoffData = Croydon, #runoff data compiled in same format as Croydon
                           additional.inflow = 0, leakMonths = 1:12, ...){
  sarl <- ifelse(is.na(tanki$soil.area.receiving.leak),0,tanki$soil.area.receiving.leak)
  #If the tank leaks to stormwater then hourly outflow data for is required 
  # for modelling downstream systems.
  if(grepl("stormwater",tanki$leakTo)){
        runoff <- runoffData$hourly
  }else{
    runoff <- runoffData$daily
    et = runoffData$daily$et_mm
  }
  tankiMod <- tankmodel(runoff = runoff,
                        dailyet = et,
                        hourly.time.step,
                        tankvol = tanki$tankvol,
                        tankbegin = tankbegin,
                        carea = tanki$tankCarea,
                        additional.inflow = additional.inflow,
                        npeople = min(1,tanki$nPeople),
                        firstflush = 0,
                        garden.area = tanki$gardenArea,
                        isgarden = 1,   #this and thenext 4 were commented out. not sure why...
                        istoilet = tanki$toilet,
                        iswmac = tanki$wmac,
                        ishw = tanki$hotwater,
                        other = eval(parse(text = tanki$other)),
                        leak.rate = tanki$leak.rate,  #L/d
                        leak.at.propn.of.capacity = tanki$leak.at.propn.of.capacity,
                        leak.months = leakMonths,
                        soil.area.receiving.leak = sarl)
  use <- gsub("\\+$","",gsub("\\+\\+","+",gsub("\\+\\+","+",gsub("\\+\\+","+",
                                                                 paste("garden",ifelse(tanki$other != "rep(0,12)","other",""),
                                                                       ifelse(tanki$toilet, "toilet",""),
                                                                       ifelse(tanki$wmac,"laundry",""),
                                                                       ifelse(tanki$hotwater,"hot water",""), sep = "+")))))
  use <- ifelse(use == "garden+","garden",use)
  useIndex <- which(tankiMod$usage.descs == use)
  if(tanki$overTo == "land"){
    tankiMod$flows$overflow[,useIndex] <- pmin(0,tankiMod$flows$overflow[,useIndex])
  }
  #if leak is to stormwater, both hourly and daily budgets are returned. If not, 
  #hourly_budget is NA
  if(grepl("stormwater",tanki$leakTo)){
    hourly_budget <- data.table::data.table(data.frame(date = runoffData$hourly$date,
                                      inflow = tankiMod$inflow,
                                      use = tankiMod$flows$use[,useIndex],
                                      leak = tankiMod$flows$leak[,useIndex],
                                      overflow = tankiMod$flows$overflow[,useIndex],
                                      store = tankiMod$flows$store[,useIndex],
                                      void = tanki$tankvol - tankiMod$flows$store[,useIndex]))
    #count leak to stormwater as 'out' (for flow to nextds SCM)
    hourly_budget$out <- hourly_budget$leak
    hourly_budget$leak <- 0
    hourly_budget <- hourly_budget[hourly_budget$date %in% runoffData$daily$date,]
    daily_budget <- hourly_budget[, lapply(.SD, sum, na.rm=TRUE), by=date, .SDcols=c("inflow","use","leak","out","overflow"),]
    daily_budget$exf <- 0
    daily_budget$et <- 0
    daily_budget$store <- hourly_budget$store[hour(runoffData$hourly$datetime) == hour(runoffData$hourly$datetime[1])]
    daily_budget$void <- hourly_budget$void[hour(runoffData$hourly$datetime) == hour(runoffData$hourly$datetime[1])]
  }else{
    hourly_budget <- NA
    daily_budget <- data.table::data.table(data.frame(date = runoffData$daily$date,
                    inflow = tankiMod$inflow,
                    use = tankiMod$flows$use[,useIndex],
                    leak = tankiMod$flows$leak[,useIndex],
                    out = 0, exf = 0, et = 0,
                    overflow = tankiMod$flows$overflow[,useIndex],
                    store = tankiMod$flows$store[,useIndex],
                    void = tanki$tankvol - tankiMod$flows$store[,useIndex]))
  }
#Note that leak to garden needs to be partitioned into exf and et. This is done
# on longer-term synoptic basis in the leak-fates table generated by budget_scm_on_datex()
  cols <- c("date","inflow","use","leak","out","exf","et","overflow","store","void")
  daily_budget <- daily_budget[,..cols]
  list(hourly_budget = hourly_budget,
       daily_budget = daily_budget)
}

# calcRGBudget() ---------------------------------
#' Calculate raingarden budget from tanks table data in lsc_dbs_scm database
#' @param rgi a single row from the raingardens table
#' @param Vstart volume of water in the raingarden at timestep 1, default is 
#'     half the capacity of the raingarden (accounting for media porosity)
#' @param runoffData rainfall and runoff time series used for EB calculation. 
#'     Default is Croydon loaded above.
#' @param additional.inflow.h vector of runoff volume in L/h equivalent in length 
#'     to that of the runoff vector in runoffData.
#' @return 
#'     List of two data.tables of water budget items, one at hourly time-steps, 
#'     and one at daily time-steps with the same number of rows as runoffData$hro1y
#'     and runoffData$dro1y, respectively. They both have the following fields:
#'     date, inflow, use, out (usually from an underdrain to stormwater), 
#'     exf (volume assumed to exfiltrate into surrounding soils), 
#'     et (evapo-transpiration losses from raingarden), overflow, store 
#'     and void). rgiHourlyBudget contains additional fields of relevance to 
#'     raingarden dynamics.
#' @details 
#'     This function uses gardenmodel() from modelfunctions2017.R
#' @examples
#' # # load tables from lsc_dbs_scms database, including all required tables
#' # load("data/lscdbsSCMs_db.rda")
#' # calcRGBudget(raingardens[raingardens$scmID == "RDR388",])

calcRGBudget <- function(rgi,
                         Vstart = 0.5*0.5*rgi$Hf*rgi$Af*1000,
                         runoffData = Croydon, 
        #runoff data compiled in same format as Croydon (rainday starts at 1100 h)
                         additional.inflow.h = 0){
  filtprof <- set.filtprof(filtr.prfile = rgi$filtr.prfile,
                           Hp = rgi$Hp, Hf = rgi$Hf, Af = rgi$Af, Ap = rgi$Ap, 
                           topsoil.depth = rgi$topsoil.depth,
                           medium = rgi$medium,
                           lined.side = rgi$lined.side)
  rgiMod <- gardenmodel(inflow = rgi$impCarea*runoffData$hourly$runoff_mm +
                        additional.inflow.h,
                        et = runoffData$hourly$et_mm,
                        carea = rgi$impCarea, #impervious catchment area
                        Ksu.mm.h = ifelse(rgi$lined.bottom, 0, tail(filtprof$Ksu.dm.h*100,1)),
                        Af = rgi$Af, Pf = rgi$Pf, Hf = rgi$Hf,
                        Ap = rgi$Ap,
                        Hp = rgi$Hp,	#pond area (sq m) and depth (m)
                        isveg = as.logical(rgi$isveg),
                        filtr.prfile = filtprof,
                        Ho = rgi$Ho,
                        Vstart = Vstart, 		#Vol of water in system at t1 in L
                        outlet.rate.L.h = (rgi$impCarea + rgi$Af)*0.3,
                        adj.tree.canopy.area = rgi$adj.tree.canopy.area,
                        medium = rgi$medium)
  rgiMod$budget$inflow <- rgiMod$budget$flowin
  rgiMod$budget$use <- rep(0,length(rgiMod$budget$out))
  rgiMod$budget$out <- rgiMod$budget$out
  rgiMod$budget$exf <- rgiMod$budget$Qexf
  rgiMod$budget$overflow <- rgiMod$budget$over
  rgiMod$budget$store <- rgiMod$budget$store.pond + rgiMod$budget$store.filter
  rgiMod$budget$Nconcout <- rgiMod$budget$Nconcout
  rgiMod$budget$Pconcout <- rgiMod$budget$Pconcout
  rgiMod$budget$TSSout <- rgiMod$budget$TSSout
  rgiHourlyBudget <- data.table::data.table(data.frame(hour = runoffData$hourly$datetime, 
                                                       date = runoffData$hourly$date,
                                                       as.data.frame(rgiMod$budget)))
  rgiDailyBudget <- rgiHourlyBudget[, lapply(.SD, sum, na.rm=TRUE), 
                                    by=date, .SDcols=c("inflow","use","out",
                                                       "exf","et","overflow","store") ]
  #Set daily store to store at first hour of rainday.
  rain_day_start <- lubridate::hour(runoffData$hourly$datetime[1])
  rgiDailyBudget$store <- rgiHourlyBudget$store[lubridate::hour(runoffData$hourly$datetime) == rain_day_start]
  #final store to final store in hourly?
  rgiDailyBudget$store[dim(runoffData$daily)[1]] <- rgiHourlyBudget$store[dim(runoffData$hourly)[1]]
  rgiDailyBudget$void <- sum(filtprof$vol) - rgiDailyBudget$store
  list(dailyBudget = rgiDailyBudget,
       hourlyBudget = rgiHourlyBudget)
}

# calcDDBudget() ---------------------------------
#' Calculate downpipe-diverter budget from dds table data in lsc_dbs_scm database
#' and a performance look-up table
#' @param ddi a single row from the dds table
#' @param runoffData rainfall and runoff time series used for EB calculation. 
#'     Default is Croydon loaded above.
#' @return 
#'     Data.tables of water budget itemsat daily time-steps, with the same 
#'     number of rows as runoffData$dro1y, and with the following fields:
#'     date, inflow, use, out exf (exfiltration), et (evapo-transpiration), 
#'     overflow, store, and void). 
#' @details 
#'     This function uses a look-up table (ddRetention) derived from Walsh and
#'     Fletcher 2014 (WalshFletcher2014-Downpipe_diverters_assessment.pdf,
#'     at github.com/cjbwalsh/lsc_foundation: underlying R script and data on WERG 
#'     share drive: wergStaff/ChrisW/rstudio_projects/downpipe_diverter). This 
#'     simple model assumes that all of the flow diverted by the diverter is 
#'     lost to evapotranspiration in the receiving garden.  The volumes diverted
#'     by these systems is very small (and their specifications so uncertain) that
#'     any speculative partitioning of these flows into exf and et is unwarranted.
#' @examples
#' #  # load tables from lsc_dbs_scms database, including all required tables
#' #  load("data/lscdbsSCMs_db.rda")
#' #  calcDDBudget(dds[dds$scmID == "D4D002",])

calcDDBudget <- function(ddi,
                         runoffData = Croydon #runoff data compiled in same format as Croydon
){
  ddRetention <- data.frame(roofarea = c(3.25, 4, 5, 7.5, 10, 11, 12, 13, 14, 
                                  15, 17.5, 20, 22.5, 25, 30, 35, 40, 45, 1000),
                            daysRunoff = c(5, 9, 16, 30, 30, 35, 35, 35, 35, 
                              124, 124, 124, 124, 124, 124, 124, 124, 136, 136),
                            rc_mm = c(29.2, 21.4, 16.6, 12.5, 12.5, 10.7, 10.7, 
                  10.7, 10.7, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.1, 0.1))
  inflowPerDD <- ddi$ddCarea*runoffData$daily$runoff_mm/ddi$ddNo
  initLoss <- 
    ddRetention$rc_mm[which(abs(ddRetention$roofarea - ddi$ddCarea/ddi$ddNo) == 
    min(abs(ddRetention$roofarea - ddi$ddCarea/ddi$ddNo)))]*ddi$ddCarea/ddi$ddNo
  overflow <- pmax(inflowPerDD - initLoss,0)
  data.table::data.table(data.frame(date = runoffData$daily$date,
             inflow = inflowPerDD*ddi$ddNo,
             use = 0,
             out = 0,
             exf = 0,
             et = (inflowPerDD - overflow)*ddi$ddNo,
             #  this assumes all of the leak is taken up by the garden (very 
             #  small volumes, so not worth worrying further about this)
             overflow = overflow*ddi$ddNo,
             store = 0,
             void = initLoss*ddi$ddNo))
}

# budget_scm_on_datex() ---------------------------------
#' Calculate water balance budget of a stormwater control measure (SCM)
#' in the lsc_dbs_scm database on a specific date given a specified inflow series.
#' @param scmID a scmID of and SCM in table SCMs
#' @param db a list containing correctly subsetted tables---subcs, parcels, ia,
#'     scmProjects, SCMs, tanks, raingardens, dds---The function data_on_datex()
#'     will calculate such a list if the a pipeID downstream of scmID is 
#'     selected, and that function corrects all tables for the specified datex.
#' @param runoffData rainfall and runoff time series used for EB calculation. 
#'     Default is Croydon loaded above.
#' @param scm_start_vol ignored if specs_for_EB is TRUE (in which case start volumes
#'     are assumed to be 50% of capacity).  if specs_for_EB is FALSE, this must be 
#'     NULL (start volumes are assumed to be zero), or an end_store object from 
#'     a previous output from this function.
#' @param specs_for_EB logical - if TRUE, seasonal leaks in Morrisons Reserve and
#'     Primary School projects are treated as leak.rate = 34560 & 15866.4, respectively
#'     and leakMonths = 5:8 in years when leaks operated and leak.rate = 0 & 15866.4
#'     respectively and leakMonths 1:12 when not)
#' @return a list containing:
#'     - a data.table of the daily water-balance budget (plus hourly if the SCM is a raingarden*).
#'     Budget fields are; hour (for hourly budgets), date , inflow , use 
#'     exf (exfiltration), et (evapotranspiration) overflow,  store,  and void.
#'     - hourly budgets also include Nconcout (estimated N concentration), 
#'     Pconcout , TSSout)
#'     - imp_carea, the total impervious catchment area of the SCM (including 
#'       catchment areas of all upstream SCMs)
#'     - leak_fates, a data.frame with estimated evapotranspiration loss and 
#'        exfiltration resulting from passive irrigation from tank leaks (using 
#'        the function Zhang64ExfET())
#'     *Note that, if there are upstream SCMs, the hourly budget are only valid 
#'     for outflows from the SCM ("out", "overflow", "Nconcout","Pconcout", and
#'     "TSSout"). Upstream losses are not summed (but they are for daily budgets)
#' @details See WalshEtAl_LSCfoundation_figsTabs.Rmd for details on EB parameters
#'     and https://urbanstreams.net/lsc/EBcalctech.html for information on water
#'     quality parameters not considered in the foundation paper. 
#'     
#'     Budgets need to be collated for all SCMs upstream, that contribute to the
#'     inflow to this SCM. Budgets of the focus SCM and all upstream SCMs are 
#'     collated into two lists (dailybudgets and hourlybudgets).  Daily budgets 
#'     (quicker to calculate) suffice for estimates of losses (et and use), but 
#'     flows to downstream systems (out, overflow) need to be calculated on an hourly 
#'     onto gardens is uncertain. We estimate it on an annual timestep using the 
#'     timestep.  The fate of water leaking from tanks Zhang (2001) curve, and save 
#'     estimated total exf and et from leaks over the  full period of analysis into 
#'     the leak_fates table.
#' @examples
#' # # load tables from lsc_dbs_scms database, including all required tables
#' # load("data/lscdbsSCMs_db.rda")
#' # SCMs <- scms; rm(scms)
#' # budget_scm_on_datex("RPLR527", db = data_on_datex(0, "2018-02-04"))
#' #   # Hereford Rd RB:  EBc 59.8/119.1,  ~2 s  
#' # budget_scm_on_datex("D4T105", db = data_on_datex(101, "2018-02-04"))
#' #   # a not great tank: EBc 0.30/0.98,  < 1 s

budget_scm_on_datex <- function(scmID, 
                            db = list(subcs = subcs, parcels = parcels, ia = ia, 
                                      scmProjects = scmProjects, SCMs = SCMs, 
                                      tanks = tanks, raingardens = raingardens, 
                                      dds = dds),
                            runoffData = Croydon,
                            scm_start_vol = NULL, #Overridden if specs_for_EB = TRUE
                            specs_for_EB = TRUE) {
  if(is.null(scm_start_vol) & !specs_for_EB){
    stop("If 'specs_for_EB' is not TRUE, then 0 or a data.frame need to be provided for 'scm_start_vol'",
         call. = FALSE)
  }
  if(dim(db$tanks)[1] > 0)
  db$tanks$leakMonths <- "1,2,3,4,5,6,7,8,9,10,11,12"
  if(specs_for_EB){
    if(dim(db$tanks)[1] > 0)
      db$tanks$tankBegin <- db$tanks$tankvol/2
    if(dim(db$raingardens)[1] > 0)
      db$raingardens$Vstart <- 0.5*0.5*db$raingardens$Hf*db$raingardens$Af*1000
    ##Changes to leak regimes for Morrisons Reserve and the Primary School not recorded in the database
    if("RPLT547" %in% db$tanks){
    if(datex >= "2017-01-30"){
      db$tanks$leak.rate[db$tanks$scmID == "RPLT547"] <- 15866.4
      db$tanks$leakMonths[db$tanks$scmID == "RPLT547"] <- "1,2,3,4,5,6,7,8,9,10,11,12"
    }else{
      db$tanks$leak.rate[db$tanks$scmID == "RPLT547"] <- 15866.4
      db$tanks$leakMonths[db$tanks$scmID == "RPLT547"] <- "5,6,7,8"
    }
    if("RPLT542" %in% db$tanks){
      if((datex >= "2014-05-30" & datex < "2016-10-01") |
          datex >= "2017-09-01"){
        db$tanks$leak.rate[db$tanks$scmID == "RPLT542"] <- 34560
        db$tanks$leakMonths[db$tanks$scmID == "RPLT542"] <- "5,6,7,8"
      }else{
        db$tanks$leak.rate[db$tanks$scmID == "RPLT542"] <- 0
        db$tanks$leakMonths[db$tanks$scmID == "RPLT542"] <- "5,6,7,8"
      }
  }
  }
  }
  allusi <- allupstream(db$SCMs, scmID, "scmID")
  nus <- vector()
  for(i in 1:length(allusi)){
    nus <- c(nus, length(allupstream(db$SCMs, allusi[i], "scmID")))
  }
  allusi <- allusi[order(nus)]
  nus <- nus[order(nus)]
  ddstab <- db$dds[db$dds$scmID %in% allusi,]
  tankstab <- db$tanks[db$tanks$scmID %in% allusi,]
  raingardenstab <- db$raingardens[db$raingardens$scmID %in% allusi,]
  gardenarea <- ifelse(scmID %in% raingardenstab$scmID,
                       raingardenstab$Af[raingardenstab$scmID == scmID],0)
  carea <- sum(ddstab$ddCarea,tankstab$tankCarea,raingardenstab$impCarea) 
  v_start <- (sum(tankstab$tankvol) + 
                sum(0.5*raingardenstab$Hf*raingardenstab$Af*1000))/2
  dailybudgets <- list()
  hourlybudgets <- list()
  leak_fates <- data.frame(scmID = allusi, et = 0, exf = 0)
  for(j in 1:length(allusi)){
    if(db$SCMs$type[db$SCMs$scmID == allusi[j]] == "tank"){
      tankj <- tankstab[tankstab$scmID == allusi[j],]
      if(nus[j] == 1){ 
        adflow <- 0
      }else{
        flow_to_tank <- db$SCMs$scmID[!is.na(db$SCMs$nextds) & db$SCMs$nextds == allusi[j]]
        if(grepl("stormwater",tankj$leakTo)){
          nohrly <- flow_to_tank[!flow_to_tank %in% names(hourlybudgets)]
          #Only those tanks that leak to stormwater will have been calculated at an hourly timestep
          #Those that were only calculated at a daily timestep are listed in nohrly.
          #Distributing overflow for these tanks should suffice, because any tanks with 'out' flows
          #Or if not specs_for_EB, we need the 
          if(length(nohrly) > 0){
            for(k in 1:length(nohrly)){
              allus_flow_to_tankk <- allupstream(db$SCMs, nohrly[k], "scmID")
              totTankCarea <- sum(tankstab$tankCarea[tankstab$scmID %in% allus_flow_to_tankk]) +
                sum(ddstab$ddCarea[ddstab$scmID %in% allus_flow_to_tankk])
              if(db$SCMs$type[db$SCMs$scmID == nohrly[k]] == "tank")
                careak <- tankstab$tankCarea[tankstab$scmID == nohrly[k]]
              if(db$SCMs$type[db$SCMs$scmID == nohrly[k]] == "dd")
                careak <- ddstab$ddCarea[ddstab$scmID == nohrly[k]]
              hourlybudgets[[which(allusi == nohrly[k])]] <- 
                data.table::data.table(data.frame(datetime = runoffData$hourly$datetime,
                                                  date = runoffData$hourly$date,
                                                  overflow = compile.inflow(hrunoff = runoffData$hourly,
                                                                            carea = careak,
                                                                            doverflow = data.frame(daten = runoffData$daily$daten,
                                                                                                   flow = dailybudgets[[nohrly[k]]]$overflow,
                                                                                                   date = runoffData$daily$date),
                                                                            firstflush = 0,
                                                                            carea.tank = totTankCarea)$runoffandover$over*totTankCarea))
              hourlybudgets[[which(allusi == nohrly[k])]]$out <- 0
              names(hourlybudgets)[which(allusi == nohrly[k])] <- nohrly[k]
            }
          }
          if(dim(runoffData$daily)[1] > 1){
            adflow <- apply(sapply(hourlybudgets[flow_to_tank], '[[', "overflow"),1,FUN = sum) + 
                       apply(sapply(hourlybudgets[flow_to_tank], '[[', "out"),1,FUN = sum)
          }else{
            adflow <- sum(sapply(hourlybudgets[flow_to_tank], '[[', "overflow")) +
                        sum(sapply(hourlybudgets[flow_to_tank], '[[', "out"))
          }
        }else{
        if(dim(runoffData$daily)[1] > 1){
          adflow <- apply(sapply(dailybudgets[flow_to_tank], '[[', "overflow"),1,FUN = sum) + 
            apply(sapply(dailybudgets[flow_to_tank], '[[', "out"),1,FUN = sum)
        }else{
          adflow <- sum(sapply(dailybudgets[flow_to_tank], '[[', "overflow"))  +
            sum(sapply(dailybudgets[flow_to_tank], '[[', "out"))         
        }
          }
        }
      tankjMod <- calcTankBudget(tankj,
                                 hourly.time.step = ifelse(grepl("stormwater",tankj$leakTo),
                                                           TRUE,FALSE),
                                 leakMonths = tankj$leakMonths,
                                 tankbegin = ifelse(specs_for_EB,
                                                    tankj$tankBegin,
                                                    ifelse(tankj$scmID %in% scm_start_vol$scmID,
                                                    scm_start_vol$store[scm_start_vol$scmID == tankj$scmID],
                                                    0)),
                                 runoffData = runoffData,
                                 additional.inflow = adflow)
      dailybudgets[[j]] <- tankjMod$daily_budget
      names(dailybudgets)[j] <- allusi[j]
      hourlybudgets[[j]] <- tankjMod$hourly_budget
      # if(length(tankjMod$hourly_budget) > 1){#grepl("stormwater",tankj$leakTo)){
      # #If it drains to stormwater, the leak is considered "out" to be added to additional inflow
      # #to downstream SCMs below - #####this is now done in calcTankBudget, so this is unnecessary (and incorrect)###
      # hourlybudgets[[j]]$out <- hourlybudgets[[j]]$leak
      # names(hourlybudgets)[j] <- allusi[j]
      # }
      #partition leak into ET and exf if it drains to a garden    
    if(tankj$leak.rate > 0 & tankj$soil.area.receiving.leak > 0){
      eETexf <- Zhang64ExfET(tanki = tankj,
                             tankBudget = dailybudgets[[j]],
                             mrm = sum(runoffData$daily$runoff_mm))
      leak_fates$exf[j] <- eETexf$exf
      leak_fates$et[j] <- eETexf$ET
    }
    if(tankj$leak.rate > 0 & tankj$soil.area.receiving.leak == 0){
      #leak_fates$exf[j] <- sum(dailybudgets[[j]]$leak)
      #This was wrong, I think - leak to no soil area = to stormwater = out
    }
    }
  if(db$SCMs$type[db$SCMs$scmID == allusi[j]] == "rg"){
    rgj <- raingardenstab[raingardenstab$scmID == allusi[j],]
    if(nus[j] == 1){ 
        adflow <- 0
      }else{
        flow_to_rg <- db$SCMs$scmID[!is.na(db$SCMs$nextds) & db$SCMs$nextds == allusi[j]]
        nohrly <- flow_to_rg[!flow_to_rg %in% names(hourlybudgets)]
        #Only those tanks that leak to stormwater will have been calculated at an hourly timestep
        #Those that were only calculated at a daily timestep are listed in nohrly.
        #Distributing overflow for these tanks should suffice, because any tanks with 'out' flows
        if(length(nohrly) > 0){
          for(k in 1:length(nohrly)){
            allus_flow_to_tankk <- allupstream(db$SCMs, nohrly[k], "scmID")
            totTankCarea <- sum(tankstab$tankCarea[tankstab$scmID %in% allus_flow_to_tankk]) +
                            sum(ddstab$ddCarea[ddstab$scmID %in% allus_flow_to_tankk])
            if(db$SCMs$type[db$SCMs$scmID == nohrly[k]] == "tank")
            careak <- tankstab$tankCarea[tankstab$scmID == nohrly[k]]
            if(db$SCMs$type[db$SCMs$scmID == nohrly[k]] == "dd")
              careak <- ddstab$ddCarea[ddstab$scmID == nohrly[k]]
              hourlybudgets[[which(allusi == nohrly[k])]] <- 
              data.table::data.table(data.frame(datetime = runoffData$hourly$datetime,
                                                date = runoffData$hourly$date,
                                                overflow = compile.inflow(hrunoff = runoffData$hourly,
                                                                          carea = careak,
                                                                          doverflow = data.frame(daten = runoffData$daily$daten,
                                                                                                 flow = dailybudgets[[nohrly[k]]]$overflow,
                                                                                                 date = runoffData$daily$date),
                                                                          firstflush = 0,
                                                                          carea.tank = totTankCarea)$runoffandover$over*totTankCarea))
            hourlybudgets[[which(allusi == nohrly[k])]]$out <- 0
            names(hourlybudgets)[which(allusi == nohrly[k])] <- nohrly[k]
          }
          }
          adflow <-  apply(sapply(hourlybudgets[flow_to_rg], '[[', "overflow"),1,FUN = sum) +
                      apply(sapply(hourlybudgets[flow_to_rg], '[[', "out"),1,FUN = sum)
      }
      rb <- calcRGBudget(rgj,
                         runoffData = runoffData,
                         Vstart = ifelse(specs_for_EB,
                                            rgj$Vstart,
                                            ifelse(rgj$scmID %in% scm_start_vol$scmID,
                                            scm_start_vol$store[scm_start_vol$scmID == rgj$scmID],
                                            0)),
                         additional.inflow.h = adflow)
      cols <- c("hour","date","inflow","use","out","exf","et",
                 "overflow","store","Nconcout","Pconcout","TSSout")
      rb$hourlyBudget <- rb$hourlyBudget[, ..cols]
      dailybudgets[[j]] <- rb$dailyBudget
      hourlybudgets[[j]] <- rb$hourlyBudget
      names(dailybudgets)[j] <- names(hourlybudgets)[j] <- allusi[j]
    }
    if(db$SCMs$type[db$SCMs$scmID == allusi[j]] == "dd"){
      dailybudgets[[j]] <- calcDDBudget(ddstab[ddstab$scmID == allusi[j],],
                                        runoffData = runoffData)
      names(dailybudgets)[j] <- allusi[j]
    }
  }
    if(db$SCMs$type[db$SCMs$scmID == scmID] == "rg"){
  hourly_budget <- hourlybudgets[[scmID]]
  hourly_budget$inflow <- runoffData$hourly$runoff_mm*carea
  }else{
    hourly_budget <- NA
  }
  if(db$SCMs$type[db$SCMs$scmID == scmID] == "tank"){
      if(grepl("stormwater",tankstab$leakTo[tankstab$scmID == allusi[j]])){
       hourly_budget <- hourlybudgets[[scmID]]
       hourly_budget$inflow <- runoffData$hourly$runoff_mm*carea
  }else{
    hourly_budget <- NA
  }
  }
    #Hourly budgets for all if calculations are not just for EB
    if(!specs_for_EB & length(hourly_budget) == 1){
      totCarea <- carea
      carea <- ifelse(db$SCMs$type[db$SCMs$scmID == scmID] == "tank",
                      tankstab$tankCarea[tankstab$scmID == scmID], 
                      ddstab$ddCarea[ddstab$scmID == scmID])
      hourly_budget <- 
        data.table::data.table(data.frame(datetime = runoffData$hourly$datetime,
                                          date = runoffData$hourly$date,
                                          overflow = compile.inflow(hrunoff = runoffData$hourly,
                                                                    carea = carea,
                                                                    doverflow = data.frame(daten = runoffData$daily$daten,
                                                                                           flow = dailybudgets[[j]]$overflow,
                                                                                           date = runoffData$daily$date),
                                                                    firstflush = 0,
                                                                    carea.tank = totCarea)$runoffandover$over*totCarea))
      if(sum(dailybudgets[[scmID]]$out) > 0) stop(paste("daily 'out' flows not captured in hourly_budget for",scmID),call. = FALSE)
      hourly_budget$out <- 0
    }
  budget <- dailybudgets[[scmID]]
  #sum losses upstream
  if(dim(runoffData$daily)[1] > 1){
  budget$use <- apply(sapply(dailybudgets, '[[', "use"),1,  FUN = sum)
  budget$et <- apply(sapply(dailybudgets, '[[', "et"),1,  FUN = sum)
  budget$exf <- apply(sapply(dailybudgets, '[[', "exf"),1,  FUN = sum)
  budget$store <- apply(sapply(dailybudgets, '[[', "store"),1,  FUN = sum)
  budget$void <- apply(sapply(dailybudgets, '[[', "void"),1,  FUN = sum)
  }else{
    budget$use <- sum(sapply(dailybudgets, '[[', "use"))
    budget$et <- sum(sapply(dailybudgets, '[[', "et"))
    budget$exf <- sum(sapply(dailybudgets, '[[', "exf"))
    budget$store <- sum(sapply(dailybudgets, '[[', "store"))
    budget$void <- sum(sapply(dailybudgets, '[[', "void"))
  }
  #taking the treatment train as a whole, the inflow to the system equals...
  budget$inflow <- runoffData$daily$runoff_mm*carea  #dailybudgets[[scmID]]$inflow
  end_stores <- data.frame(scmID = names(dailybudgets)[1],
                           store = tail(dailybudgets[[1]]$store,1),
                           stringsAsFactors = FALSE)
  if(length(dailybudgets) > 1){
for(i in 2:length(dailybudgets))
  end_stores <- rbind(end_stores,  data.frame(scmID = names(dailybudgets)[i],
                                              store = tail(dailybudgets[[i]]$store,1),
                                              stringsAsFactors = FALSE))
  }
  list(budget = budget, hourly_budget = hourly_budget, imp_carea = carea,
       leak_fates = leak_fates, v_start = v_start, end_stores = end_stores)
}

# EB_scm_on_datex() ---------------------------------
#' Calculate Environmental Benefit Index of a stormwater control measure (SCM)
#' in the lsc_dbs_scm database on a specific date.
#' @param scmID a scmID of and SCM in table SCMs
#' @param runoffData rainfall and runoff time series used for EB calculation. 
#'     Default is Croydon loaded above.
#' @param R_n pre-urban overland flow frequency = target number of impervious runoff days 
#' @param Nconc_n target N concentration, default 0.6 mg/L
#' @param Pconc_n target P concentration, default 0.05 mg/L
#' @param TSSconc_n target TSS concentration, default 20 mg/L
#' @param percentile percentile concentration used for water quality targets, 
#'     default median
#' @param max_filter_flow_rate target maximum filtered flow rate, default 0.3 L/h/m2
#' @param reward.overextraction logical, if TRUE volume reduction index can 
#'       exceed 1. Over-extraction from any one SCM can be rewarded if overall, 
#'       volume reduction is well below target (and EB is being used as a cost
#'       mechanism)
#' @param override.FV_n logical for use in parallel SCMs, where one 
#'       system leaks more than permissible by itself. Excess filtered volume 
#'       can be acceptable if overall, FV is well below target
#' @return A list the sub-indices RO, RO_binary, VR, FV, and WQ; their 
#'       component variables R, R(b), V, F and Nconc, Pconc, and TSSconc from
#'       the SCM (_m), in the pre-urban state (_n), and from impervious surfaces 
#'       (_u); the primary EB index used in the foundation paper, the EB_old_calc
#'       index (used by the EB calculator, and in the LSC project development,
#'       calculated using R_mb instead of R_m), and the maximum EB index value
#'       that could have been achieved by this SCM.
#' @details See WalshEtAl_LSCfoundation_figsTabs.Rmd for details on EB variables
#'     and https://urbanstreams.net/lsc/EBcalctech.html for information on water
#'     quality parameters not considered in the foundation paper
#' @examples
#' # # load tables from lsc_dbs_scms database, including all required tables
#' # load("data/lscdbsSCMs_db.rda")
#' # t(unlist(EB_scm_on_datex("RPLJ001", db = data_on_datex(31, "2018-02-05"))))
#' #   #Fernhill Rd Jellyfish after King St Upper diversion: 
#' #   # EB 60.5 (out of max potential of 110.6), ~30 s
#' # t(unlist(EB_scm_on_datex("RPLJ001", db = data_on_datex(31, "2018-02-04"))))
#' #   #Fernhill Rd Jellyfish before King St Upper diversion: 
#' #   # EB 138.2 / 247.3, ~90 s (many upstream tanks with leaks to stormwater)
#' # t(unlist(EB_scm_on_datex("D4D004", db = data_on_datex(101, "2018-02-04"))))
#' #   # Not very effective downpipe diverter: EB 0.04 / 0.53, <1 s
#' # t(unlist(EB_scm_on_datex("D4T105", db = data_on_datex(101, "2018-02-04"))))
#' #   # a not great tank: EBc 0.22 / 0.98,  < 1 s
#' # t(unlist(EB_scm_on_datex("RPLR527", db = data_on_datex(0, "2018-02-04"))))
#' #   # Hereford Rd RB:  EB 59.5 / 117.7  ~2 s  

EB_scm_on_datex <- function(scmID, 
                           db = list(subcs = subcs, parcels = parcels, ia = ia, scmProjects = scmProjects,
                                     SCMs = SCMs, tanks = tanks, raingardens = raingardens, dds = dds),
                           runoffData = Croydon,
                           R_n = 12, #Number of days of runoff in the pre-urban state
                           Nconc_n = 0.6, Pconc_n = 0.05, TSSconc_n = 20, #Target contaminant concentrations 
                           percentile = 50, #percentile used for assessing concentrations against n and u
                           max_filter_flow_rate = 0.3,  #L/h/m2 of total imp catchment area should be ~0.1: see gardenmodel in modelfunctions2017.R
                           reward.overextraction = TRUE, #Over-extraction from any one SCM rewarded if overall, volume reduction is well below target
                           override.FV_n = FALSE #Excess filtered volume acceptable if overall, FV is well below target
                            ) {
    x <- budget_scm_on_datex(scmID, db, runoffData, )
    if(scmID %in% c(db$tanks$scmID, db$dds$scmID)){
      if(scmID %in% db$tanks$scmID){
         scm_stats <- db$tanks[db$tanks$scmID == scmID,]
         gardenarea <- scm_stats$gardenArea
         }
      if(scmID %in% db$dds$scmID){
         scm_stats <- db$tanks[db$dds$scmID == scmID,]
         gardenarea <- 10  #see Walsh and Fletcher 2014 for uncertainty (and irrelevance) around this
    }
       max_filter_flow_rate.L.h <- (x$imp_carea + gardenarea) * max_filter_flow_rate
       Rmtab <- data.table::data.table(
              data.frame(V_ui = runoffData$daily$runoff_mm*x$imp_carea,
                        out = x$budget$out,
                        over = x$budget$overflow))
    Rmtab$too_much_out <- Rmtab$out
    Rmtab$too_much_out[Rmtab$out <= max_filter_flow_rate.L.h*24] <- 0
    #FVm filtered volume through the SCMs via out
    F_m_out <- sum(Rmtab$out[Rmtab$out <= max_filter_flow_rate.L.h*24])
    }
  if(scmID %in% raingardens$scmID){
    if(scmID %in% db$raingardens$scmID){
      scm_stats <- db$raingardens[db$raingardens$scmID == scmID,]
      gardenarea <- scm_stats$Af
    }
    max_filter_flow_rate.L.h <- (x$imp_carea + gardenarea) * max_filter_flow_rate
    Rmtab <- data.table::data.table(data.frame(
                        date = runoffData$hourly$date,
                        V_ui = runoffData$hourly$runoff_mm*x$imp_carea,
                        out = x$hourly_budget$out,
                        over = x$hourly_budget$overflow))
    Rmtab$too_much_out <- Rmtab$out
    Rmtab$too_much_out[Rmtab$out <= max_filter_flow_rate.L.h] <- 0
    Rmtab <- Rmtab[, lapply(.SD, sum, na.rm=TRUE), by=date, .SDcols=c("V_ui","out","over","too_much_out") ]
    #FVm filtered volume through the SCMs via out
    F_m_out <- sum(Rmtab$out[Rmtab$out <= max_filter_flow_rate.L.h])
  }
    ### It is possible in treatment trains for overflow to continue into the next day because of routing delay
    ### This will be in the first hour of the next day. To avoid infinite Rm value, move any such delayed overflows
    ### into the previous day
    delayed_over_days <- which(Rmtab$over > 0 & Rmtab$V_ui == 0)
    if(length(delayed_over_days) > 0){
      for(i in 1:length(delayed_over_days)){
        Rmtab$over[delayed_over_days[i] - 1] <- Rmtab$over[delayed_over_days[i] - 1] + Rmtab$over[delayed_over_days[i]]
        Rmtab$over[delayed_over_days[i]] <- 0
      }
    }
    #Number of days of impervious runoff either untreated or filtered, but above the threshold 'base'flow
    Rmtab$V_oi <- (Rmtab$too_much_out + Rmtab$over)
    Rmtab$R_mi <- Rmtab$V_oi/Rmtab$V_ui
    Rmtab$R_mi[is.na(Rmtab$R_mi)] <- 0
    #R_mb is the binary version of Rm (see equations in Foundation paper) -used in EB calculator, but not in the paper 
    R_mb <- sum((Rmtab$V_oi) > 0)
    #R_m is the proportional version of Rm used in the Foundation paper
    R_m <- sum(Rmtab$R_mi)
    R_u <- sum(runoffData$daily$runoff_mm > 0) #121 Number of days of runoff from impervious surfaces
    
### 1. Runoff frequency index, RO
    RO <- (1 - max((R_m - R_n)/(R_u - R_n), 0)) * x$imp_carea/100
    RO_binary <- (1 - max((R_mb - R_n)/(R_u - R_n), 0)) * x$imp_carea/100

### 2. Volume reduction index, VR
    # Theoretical streamflow coefficients for forested and pasture catchments
    mean.annrain.mm <- sum(runoffData$daily$runoff_mm)
    Zhang.forest.ro <- 1 - (1 + 2820/mean.annrain.mm)/(1 + 2820/mean.annrain.mm + mean.annrain.mm/1410)
    Zhang.pasture.ro <- 1 - (1 + 550/mean.annrain.mm)/(1 + 550/mean.annrain.mm + mean.annrain.mm/1100)
    # if there is more in the SCMs at the end than in the 
    # beginning, ignore this difference i.e. if system is emptier at end of run
    # than at start, then don't include the difference as lost (i.e. used and 
    # fallen during the period) water
    delstore <- max(0, x$v_start - x$budget$store[dim(x$budget)[1]])
    #tank/rg start volume = store[1] + use[1] + exf[1] + et[1]
    V_m <- sum(c(x$budget$out,x$budget$overflow,x$budget$exf)) + 
               sum(x$leak_fates$exf) - delstore  #sum(totLostVol)
    V_u <- sum(Rmtab$V_ui)
    V_n <- x$imp_carea * Zhang.forest.ro * mean.annrain.mm
    VR <- (1 - max((V_m - V_n)/(V_u - V_n), 0)) * x$imp_carea/100
    if(reward.overextraction) {
    # thus possible for a specific SCM to score VR > 1: useful if using the  
    # EB index to value projects and aiming for catchment-wide volume reduction, 
    # but not able to achieve it in all SCMs.
      VR <- (1 - (V_m - V_n)/(V_u - V_n)) * x$imp_carea/100
        }

### 3. Filtered flow volume index, FV
      # filtered flows = upstream leaks to stormwater or exf (see above where they are partitioned into exf and out) + 
      #                  upstream exf + 
      #                  upstream out that is less than max_filter_flow_rate (F_m_out, calculated above)
      # (all compiled in budget_scm_on_datex())
      F_m <- sum(x$leak_fates$exf) + F_m_out + x$budget$exf[1]
      # impervious runoff minus runoff from mature forest according to Zhang curve.
      F_pasture <- Zhang.pasture.ro * (x$imp_carea + gardenarea) * mean.annrain.mm
      F_forest <- Zhang.forest.ro * (x$imp_carea + gardenarea) * mean.annrain.mm

      if (F_m < F_forest) {
          FV <- (F_m/F_forest) * x$imp_carea / 100
          } else {
      if (F_m > F_pasture) {
          FV <- max(0, 1 - (F_m - F_pasture)/F_forest) * x$imp_carea/100
          } else {
          FV <- x$imp_carea/100
           }
          }
  if(override.FV_n & F_m > F_forest) 
    FV <- x$imp_carea/100

### 4. water-quality index, WQ
      #WQ logic -
  #1. if the system overflows more frequently than the specified percentile, or 
  #   if it is a tank with an unfiltered leak that flows more frequently than 
  #   the specified percentile, then concentration is as bad as inflow stormwater. 
  #   (or if it is a downpipe diverter, then infiltration flows are assumed to 
  #   be negligible compared to the remaining flows that are untreated stormwater)
  #2. If not, and it is a tank with a filtered leak, a leak to soil, or no leak, 
  #   then the system makes no contribution to pollutant concentrations at 
  #   [percentile] flows, so set concentration to zero.
  #3. If not and it is a raingarden then use [percentile] concout values
      Nconc_u <- 2.2
      Pconc_u <- 0.35
      TSSconc_u <- 150
      UnfilteredLeak <- FALSE
      #downpipe diverters (are useless)
  if(db$SCMs$type[db$SCMs$scmID == scmID] == "dd"){
    Nconc_m <- Nconc_u
    Pconc_m <- Pconc_u
    TSSconc_m <- TSSconc_u
  }
      #Tanks
  if(db$SCMs$type[db$SCMs$scmID == scmID] == "tank"){
    #if they have an unfiltered leak to stormwater
    #or if they overflow more than percentile of the time (highly unlikely) 
    # or have an unfiltered leak or don't leak at all
  if((scm_stats$leakTo[scm_stats$scmID == scmID] == "stormwater" &
      scm_stats$leak.rate > 0) | 
     quantile(x$budget$overflow, probs = percentile/100) > 0 |
     UnfilteredLeak | scm_stats$leak.rate == 0){
    Nconc_m <- Nconc_u
    Pconc_m <- Pconc_u
    TSSconc_m <- TSSconc_u
     }else{
    #if they have a filtered leak ####NOT RIGHT YET!
    Nconc_m <- 0
    Pconc_m <- 0
    TSSconc_m <- 0
      }
  }
      #Raingardens
  if(db$SCMs$type[db$SCMs$scmID == scmID] == "rg"){
    #If it overflows more than 'percentile' of the time (highly unlikely)
    if(quantile(x$budget$overflow, probs = percentile/100) > 0){
      Nconc_m <- Nconc_u
      Pconc_m <- Pconc_u
      TSSconc_m <- TSSconc_u
    }else{
      #if it doesn't overflow too much, and if it has an underdrain that releases filtered flow
     if(quantile(x$budget$overflow, probs = percentile/100) == 0  & 
        sum(x$hourly_budget$out > 0)){
    Nconc_m <- as.vector(quantile(x$hourly_budget$Nconcout,
                              probs = percentile/100, na.rm = TRUE))
    Pconc_m <- as.vector(quantile(x$hourly_budget$Pconcout,
                              probs = percentile/100, na.rm = TRUE))
    TSSconc_m <- as.vector(quantile(x$hourly_budget$TSSout,
                              probs = percentile/100, na.rm = TRUE))
     }else{
       #if it doesn't overflow too much, and if it only loses water through exfiltration
       Nconc_m <- 0
       Pconc_m <- 0
       TSSconc_m <- 0
     }
    }
  }
  WQ <- mean(c((1 - max(0, (Nconc_m - Nconc_n)/
                        (Nconc_u - Nconc_n))) * x$imp_carea/100,
             (1 - max(0, (Pconc_m - Pconc_n)/
                        (Pconc_u - Pconc_n))) * x$imp_carea/100,
             (1 - max(0, (TSSconc_m - TSSconc_n)/
                          (TSSconc_u - TSSconc_n))) * x$imp_carea/100))
  # arguably these urban values (2.2, 0.35 and 150) are the more appropriate conc for 75th %ile
  # this index can potentially score < 1 (if N is added to the system)
 summary <- list("R_m" = R_m,"R_n" = R_n,"R_u" = R_u,"R_mb" = R_mb,
                 "V_m" = V_m,"V_n" = V_n,"V_u" = V_u,
                 "F_m" = F_m,"F_forest" = F_forest,"F_pasture" = F_pasture,"F_u" = 0,
                 "Nconc_m" = Nconc_m,"Nconc_n" = Nconc_n,"Nconc_u" = Nconc_u,
                 "Pconc_m" = Pconc_m,"Pconc_n" = Pconc_n,"Pconc_u" = Pconc_u,
                 "TSSconc_m" = TSSconc_m,"TSSconc_n" = TSSconc_n,"TSSconc_u" = TSSconc_n,
                 "RO" = RO,"RO_binary" = RO_binary,"VR" = VR,"FV" = FV,"WQ" = WQ, 
                 "EB" = mean(c(RO,VR,FV,WQ)),
                 "EB_old_calc" = mean(c(RO_binary,VR,FV,WQ)),
                 "EB_max" = x$imp_carea/100)
 return(summary)
}

# EB_subc_on_datex() ---------------------------------
#' Calculate Environmental Benefit Index of all stormwater control measures (SCMs)
#' in a subcatchment on a specified date, and resulting variants on effective
#' imperviousness.
#' @param pipeID a value from table subcs identifying subcatchment of interest
#' @param datex date of interest "YYYY-MM-DD"
#' @param runoffData rainfall and runoff time series used for EB calculation. 
#'     Default is Croydon loaded above.
#' @param upstream_scm_results if pipeID is downstream of other subcatchments 
#'     for which this function has already been calculated, this argument can be
#'     populated with a list of the output objects, and their EBs will not be 
#'     recalculated, but will be used to calculate EB for pipeID.  Default is 
#'     NULL (no previously calculated upstream results will be used)
#' @param ... arguments to EB_scm_on_datex()
#' @return A list of:
#'     scm_stats, data.frame of results of EB_scm_on_datex() for all terminal 
#'         SCMs in the subcatchment
#'     Imperviousness variants for the subc; TI total, EI effective (ignoring
#'     any SCMs in the catchment), EIs effective assuming all SCMs perfectly 
#'     disconnect their upstream impervious surfaces), EI_sRO effective (where
#'     surfaces upstream of each SCM are weighted by its Runoff frequency [EB 
#'     environmental benefit score] sub-index), EI_sVR effective (where surfaces
#'     upstream of each SCM are weighted by its volume reduction EB sub-index), 
#'     EI_sFV effective (where surfaces upstream of each SCM are weighted by its 
#'     Filtered volume EB sub-index), EI_sEB effective (where surfaces upstream 
#'     of each SCM are weighted by its Runoff frequency EB index).
#' @details See WalshEtAl_LSCfoundation_figsTabs.Rmd for details on EB variables
#'     and https://urbanstreams.net/lsc/EBcalctech.html for information on water
#'     quality parameters not considered in the foundation paper
#' @examples
#' # # load tables from lsc_dbs_scms database, including all required tables
#' # load("data/lscdbsSCMs_db.rda")
#' # t(unlist(EB_subc_on_datex(29,"2005-01-01")[-1])) #~0.2 s
#' # t(unlist(EB_subc_on_datex(29,"2010-01-01")[-1])) #~5 s
#' # t(unlist(EB_subc_on_datex(29,"2016-01-01")[-1])) #~5 s
#' #   # Wattle Valley Rd North, before its sealing and curbing, and at two times
#' #   # after sealing and implementation of increasingly more stormwater control
#' # EB_pipeID_53 <- EB_subc_on_datex(53,"2016-01-01") #LSN0001: ~1 min
#' # EB_pipeID_74 <- EB_subc_on_datex(74,"2016-01-01") #LIS0001: ~1 min
#' # EB_pipeID_36 <- EB_subc_on_datex(36,"2016-01-01") #LSS0001: ~2.2 mins
#' # EB_pipeID_71 <- EB_subc_on_datex(71,"2016-01-01",
#' #                          upstream_scm_results = list(EB_pipeID_53,
#' #                                                      EB_pipeID_74,
#' #                                                      EB_pipeID_36)) #LIS0004:~5.3 mins
#' # EB_pipeID_101 <- EB_subc_on_datex(101,"2016-01-01") #DBS0004: ~35 s
#' # EB_pipeID_103 <- EB_subc_on_datex(103,"2016-01-01",
#' #                                  upstream_scm_results = list(EB_pipeID_101)) #DBS0008: ~1 min
#' # t(unlist(EB_pipeID_103[-1]))
#' # save(EB_pipeID_53,EB_pipeID_74,EB_pipeID_36,EB_pipeID_71,
#' # EB_pipeID_101, EB_pipeID_103, file = "compiled_objects/EB_mainsites_2016-01-01.rda", compress = "xz")

EB_subc_on_datex <- function(pipeID, datex, 
                             upstream_scm_results = NULL, 
                             runoffData = Croydon, ...){
  db <- data_on_datex(pipeID, datex)
  carea <- db$subcs$carea[db$subcs$pipeID == pipeID]
  terminal_scms <- db$SCMs[is.na(db$SCMs$nextds) | db$SCMs$nextds == "land",]
  #if upstream_scm_results are to be used, combine them into a single look-up table us_scms
  if(!is.null(upstream_scm_results)){
    for(i in 1:length(upstream_scm_results)){
      if(i == 1){
        us_scms <- upstream_scm_results[[1]]$scm_stats
      }else{
          us_scms <- rbind(us_scms,
                           upstream_scm_results[[i]]$scm_stats)
        }
    }
  }
  tia <- sum(db$ia$area_m2)
  eia <- sum(db$ia$area_m2*db$ia$conn)  # connected IA ignoring SCMs
  if(dim(terminal_scms)[1] > 0){
    if(is.null(upstream_scm_results)){
      x <- data.frame(scmID = terminal_scms$scmID[1], 
                      t(unlist(EB_scm_on_datex(terminal_scms$scmID[1], db, runoffData))))  #, ...
    }else{
    if(!terminal_scms$scmID[1] %in% us_scms$scmID){
  x <- data.frame(scmID = terminal_scms$scmID[1], 
             t(unlist(EB_scm_on_datex(terminal_scms$scmID[1], db, runoffData))))  #, ...
    }else{
    x <- us_scms[us_scms$scmID == terminal_scms$scmID[1],]
  }}
  if(dim(terminal_scms)[1] > 1){
    for(i in 2:dim(terminal_scms)[1]){
      if(is.null(upstream_scm_results)){
        x <- rbind(x,
                   data.frame(scmID = terminal_scms$scmID[i], 
                      t(unlist(EB_scm_on_datex(terminal_scms$scmID[i], db, runoffData)))))  #, ...
      }else{
    if(!terminal_scms$scmID[i] %in% upstream_scm_results$scmID){
      x <- rbind(x,
                data.frame(scmID = terminal_scms$scmID[i],
                           t(unlist(EB_scm_on_datex(terminal_scms$scmID[i], db, 
                                                    runoffData)))))
    }else{
      x <- rbind(x,
                 us_scms[us_scms$scmID == terminal_scms$scmID[i],])
    }
        }}
  }
  eia_scms <- sum(x$EB_max)*100         #IA draining to an SCM
  eia_s <- eia - eia_scms               #all ia draining to an SCM, W = 0
  eia_sRO <- eia_s + sum(x$EB_max*(1- x$RO/x$EB_max))*100 # ia draining to an SCM, W = RO
  eia_sVR <- eia_s + sum(x$EB_max*(1- x$VR/x$EB_max))*100 # ia draining to an SCM, W = VR
  eia_sFV <- eia_s + sum(x$EB_max*(1- x$FV/x$EB_max))*100 # ia draining to an SCM, W = FV
  eia_sEB <- eia_s + sum(x$EB_max*(1- x$EB/x$EB_max))*100 # ia draining to an SCM, W = EB
  }else{
    eia_s <- eia_sRO <- eia_sVR <- eia_sFV <- eia_sEB <- eia
  }
  TI <- tia/carea
  EI <- eia/carea
  EI_s <- eia_s/carea
  EI_sRO <- eia_sRO/carea
  EI_sVR <- eia_sVR/carea
  EI_sFV <- eia_sFV/carea
  EI_sEB <- eia_sEB/carea
  if(dim(terminal_scms)[1] == 0) x <- NA
  list(scm_stats = x, TI = TI, EI = EI, EI_s = EI_s, EI_sRO = EI_sRO,
       EI_sVR = EI_sVR, EI_sFV = EI_sFV, EI_sEB = EI_sEB)
}
  
# EB_scm_time_series() ---------------------------------
#' Calculate Environmental Benefit Index of a stormwater control measure (SCM)
#' over time, returning changes in score and their dates.
#' @param scmID a scmID of and SCM in table SCMs
#' @param fin_date last date of time series as string "YYYY-MM-DD" (start date 
#'    is assumed to be the installation date of the scm)
#' @return A data.frame with fields scmID, date, and all of the output statistics
#'    from EB_scm_on_datex. Each row records the EB statistics for the day after
#'    a recorded change in the dataset
#' @details See WalshEtAl_LSCfoundation_figsTabs.Rmd for details on EB variables
#'     and https://urbanstreams.net/lsc/EBcalctech.html for information on water
#'     quality parameters not considered in the foundation paper
#' @examples
#' # # load tables from lsc_dbs_scms database, including all required tables
#' # load("data/lscdbsSCMs_db.rda")
#' # EB_scm_time_series(scmID = "RPLR530", fin_date = "2019-12-01") #~7 s
#' # # Terminal raingarden in pipeID 29 (Wattle Valley Rd North)
#' # EB_scm_time_series("R2T229", "2019-12-01") #~ 1 s
#' # # A non-terminal tank

EB_scm_time_series <- function(scmID, start_date, fin_date){
  db <- data_for_scm(scmID, fin_date)
  start_date <- lubridate::ymd(start_date)
  fin_date <- lubridate::ymd(fin_date)
  pipeID <- SCMs$pipeID[SCMs$scmID == scmID]
  change_dates <- c(db$scmProjects$installDate,
                    db$ia$constructionDate[!is.na(db$ia$constructionDate)],
                    db$scmChanges$changeDate,
                    db$parcelChanges$date)
  if(sum(scmID %in% c(alldownstream(SCMs, "RPLR553", "scmID"),"RPLJ001") &
         #scms downstream of network change caused by King St Upper
         fin_date >= "2016-10-14" & start_date < "2019-06-02") > 1) stop("scmID")
  
   if(scmID %in% c(alldownstream(SCMs, "RPLR553", "scmID"),"RPLJ001") &
    #scms downstream of network change caused by King St Upper
    fin_date >= "2016-10-14" & start_date < "2019-06-02"){ #periods of on-off for King St Upper
     change_dates <- c(change_dates, lubridate::ymd(c("2016-07-15","2016-10-14","2017-03-17","2017-09-15",
                       "2018-02-05","2019-06-02")))
  }
     change_dates <- unique(c(start_date, change_dates))
     change_dates <- change_dates[change_dates >= start_date & change_dates < fin_date]
     if(scmProjects$installDate[scmProjects$projectID == 
                                SCMs$projectID[SCMs$scmID == scmID]] > start_date)
     change_dates <- change_dates[change_dates >= scmProjects$installDate[scmProjects$projectID == 
                                                                              SCMs$projectID[SCMs$scmID == scmID]]]
     change_dates <- unique(c(change_dates, scmProjects$installDate[scmProjects$projectID == 
                                             SCMs$projectID[SCMs$scmID == scmID]]))
     change_dates <- change_dates[order(change_dates, decreasing = FALSE)]
        for(i in 1:length(change_dates)) {
     db <- data_on_datex(pipeID, change_dates[i] + days(1))
     #PL SCMs of PL projects encompass all upstream impervious surfaces
     if(!is.na(scmProjects$address[scmProjects$projectID == 
                                  SCMs$projectID[SCMs$scmID == scmID]])){
       us_scms <- allupstream(SCMs, scmID, "scmID")
       db$SCMs <- db$SCMs[db$SCMs$scmID %in% us_scms,]
       db$subcs <- db$subcs[db$subcs$pipeID %in% db$SCMs$pipeID,]
       db$scmProjects <- db$scmProjects[db$scmProjects$projectID %in% db$SCMs$projectID,]
       db$parcels <- db$parcels[db$parcels$address %in% unique(db$SCMs$address),]
       db$ia <- db$ia[db$ia$address %in% db$parcels$address,]
     }
     if(i == 1)
     EBs <- t(unlist(EB_scm_on_datex(scmID, db = db)))
     if(i > 1)
       EBs <- rbind(EBs, t(unlist(EB_scm_on_datex(scmID, db = db))))
   }
     EBs <- data.frame(scmID = scmID, 
                       date = change_dates,
                       EBs, stringsAsFactors = FALSE)
     EBs
}

# EI_subc_time_series() ---------------------------------
#' Calculate variants of EI for a subcatcatchment over a specified time series
#' @param pipeID a value from table subcs identifying subcatchment of interest
#' @param upstream_scm_results if pipeID is downstream of other subcatchments 
#'     for which this function has already been calculated, this argument can be
#'     populated with a list of the output objects, and their EBs will not be 
#'     recalculated, but will be used to calculate EB for pipeID.  Default is 
#'     NULL (no previously calculated upstream results will be used)
#' @param start_date first date of time series as string "YYYY-MM-DD" (start date 
#'    is assumed to be the installation date of the scm)
#' @param fin_date last date of time series as string "YYYY-MM-DD" (start date 
#'    is assumed to be the installation date of the scm)
#' @return a list of three data.frames: 
#'    EBs, EB stats (output of EB_scm_time_series) of all terminal 
#'    SCMs in the pipeID subcatchment, on the date of each change
#'    resulting in a change in EB;
#'    ia_ts, a daily time series of all variants of EI for the subc over the 
#'    specified time period.
#'    non_term_dates: scmIDs that were terminal SCMs for part but not all of the
#'    time series, with the date at which they stopped being terminal in the field
#'    date.
#' @details See WalshEtAl_LSCfoundation_figsTabs.Rmd for details on EB variables
#'     and https://urbanstreams.net/lsc/EBcalctech.html for information on water
#'     quality parameters not considered in the foundation paper
#' @examples
# system.time(ei_29 <- EI_subc_time_series(29)) #~8 s
# system.time(ei_53 <- EI_subc_time_series(53, 
#                     upstream_scm_results = list(ei_29))) #~31 min
# system.time(ei_36 <- EI_subc_time_series(36)) #~59 min

EI_subc_time_series <- function(pipeID, 
                                upstream_scm_results = NULL, 
                                start_date = "2001-01-01", 
                                fin_date = "2019-12-31"){
  if(!is.null(upstream_scm_results)){
    us_EBs <- upstream_scm_results[[1]]$EBs
    us_ntds <- upstream_scm_results[[1]]$non_term_dates
    if(length(upstream_scm_results) > 1){
      for(i in 2:length(upstream_scm_results)){
        us_EBs <- rbind(us_EBs,upstream_scm_results[[i]]$EBs)
        us_ntds <- upstream_scm_results[[i]]$non_term_dates
      }
    }
  }
    if(start_date < "2001-01-01")
    stop("Earliest start date is '2001-01-01'", call. = FALSE)
  allusi <- allupstream(subcs, pipeID, "pipeID")
  #For every scmProject in pipeID, find terminal scmIDs on installDate, and 
  #determine if they become non-terminal before fin_date (by istallation of an SCM downstream)
  scmps <- scmProjects[scmProjects$pipeID %in% allusi,]
  scmps <- scmps[order(scmps$installDate),]
  idates <- unique(scmps$installDate)
  idates <- idates[idates < fin_date & idates >= start_date]
  non_term_dates <- data.frame(scmID = NA, date = lubridate::ymd("2000-01-01"), 
                               stringsAsFactors = FALSE)
  if(length(idates) == 0) idates <- start_date
  for(i in 1:length(idates)){
    db <- data_on_datex(pipeID, idates[i] + days(1))
    if(i == 1){
      terminal_scms <- db$SCMs$scmID[is.na(db$SCMs$nextds) | 
                                       db$SCMs$nextds == "land"]
      if(!is.null(upstream_scm_results)){
        terminal_scms <- terminal_scms[!terminal_scms %in% us_EBs$scmID]
      }
    }
    if(i > 1){
      new_tcsms <- db$SCMs$scmID[is.na(db$SCMs$nextds) | 
                                 db$SCMs$nextds == "land"]
      if(!is.null(upstream_scm_results)){
        new_tcsms <- new_tcsms[!new_tcsms %in% us_EBs$scmID]
      }
      non_terms <- terminal_scms[!(terminal_scms %in% new_tcsms)]
      non_terms <- non_terms[!non_terms %in% non_term_dates$scmID]
      if(length(non_terms) > 0){
        non_term_dates <- rbind(non_term_dates,
                                data.frame(scmID = non_terms,
                                           date = idates[i],
                                           stringsAsFactors = FALSE))
                                  }
      terminal_scms <- unique(c(terminal_scms, new_tcsms))
    }
  }
  non_term_dates <- non_term_dates[-1,]
  #If any scms in non_term_dates table were decommissioned before their downstream SCM was constructed...
  if(sum(non_term_dates$scmID %in% scmsDecommissioned$scmID) > 0){
    decomms <- which(non_term_dates$scmID %in% scmsDecommissioned$scmID)
    for(j in decomms){
      if(scmsDecommissioned$decommDate[scmsDecommissioned$scmID == non_term_dates$scmID[j]] < non_term_dates$date[j])
if(length(non_term_dates$date[j]) != sum(scmsDecommissioned$scmID == non_term_dates$scmID[j])) stop("1670")
              non_term_dates$date[j] <- 
        scmsDecommissioned$decommDate[scmsDecommissioned$scmID == non_term_dates$scmID[j]]
    }
  }
  # and if any other terminal scms were decommissioned, add them to the non_term_dates table, 
  # so that their decommissioning can be accounted for below
  if(sum(terminal_scms %in% scmsDecommissioned$scmID & !terminal_scms %in% non_term_dates$scmID) > 0){
    decomms <- which(terminal_scms %in% scmsDecommissioned$scmID & !terminal_scms %in% non_term_dates$scmID)
    dcs <- terminal_scms[terminal_scms %in% scmsDecommissioned$scmID & !terminal_scms %in% non_term_dates$scmID]
    for(j in 1:length(dcs)){
      if(length(dcs[j]) != sum(scmsDecommissioned$scmID == dcs[j])) stop()
      non_term_dates <- rbind(non_term_dates,
                                   data.frame(scmID = dcs[j],
                                              date = scmsDecommissioned$decommDate[scmsDecommissioned$scmID == dcs[j]],
                                              stringsAsFactors = FALSE))
    }
  }
  # db <- data_on_datex(pipeID, fin_date)
  # carea <- db$subcs$carea[db$subcs$pipeID == pipeID]
  iats <- ia_ts(pipeID, start_date = start_date, fin_date = fin_date)
  iats$s <- iats$ro <- iats$vr <- iats$fv <- iats$eb <- iats$eia
  for(i in 1:length(terminal_scms)){
    if(terminal_scms[i] %in% non_term_dates$scmID) {
      end <- non_term_dates$date[non_term_dates$scmID == terminal_scms[i]]
    }else{
      end <- fin_date
    }
   EBi <- data.table::data.table(EB_scm_time_series(terminal_scms[i], start_date, end))
    EBi_dates <- unique(c(EBi$date, end))
    for(j in 1:(length(EBi_dates)-1)){
      iats[iats$date > EBi_dates[j] & iats$date <= EBi_dates[j + 1],
           c("s","ro","vr","fv","eb")] <- 
        iats[iats$date > EBi_dates[j] & iats$date <= EBi_dates[j + 1],
             c("s","ro","vr","fv","eb")] - 
        EBi[rep(j,sum(iats$date > EBi_dates[j] & iats$date <= EBi_dates[j + 1])),
            c("EB_max","RO","VR","FV","EB")] * 100
    }
    if(i == 1)
      EBs <- EBi
    if(i > 1)
      EBs <- rbind(EBs, EBi)
  } 
  if(!is.null(upstream_scm_results)){
      new_ntds <- non_term_dates
      new_EBs <- EBs
      non_term_dates <- us_ntds
      terminal_scms <- unique(as.vector(us_EBs$scmID))
      EBs <- us_EBs
      for(i in 1:length(terminal_scms)){
        if(terminal_scms[i] %in% non_term_dates$scmID) {
          end <- non_term_dates$date[non_term_dates$scmID == terminal_scms[i]]
        }else{
          end <- fin_date
        }
        EBi <- EBs[EBs$scmID == terminal_scms[i],]
        EBi_dates <- unique(c(EBi$date, end))
        for(j in 1:(length(EBi_dates)-1)){
          iats[iats$date > EBi_dates[j] & iats$date <= EBi_dates[j + 1],
               c("s","ro","vr","fv","eb")] <- 
            iats[iats$date > EBi_dates[j] & iats$date <= EBi_dates[j + 1],
                 c("s","ro","vr","fv","eb")] - 
            EBi[rep(j,sum(iats$date > EBi_dates[j] & iats$date <= EBi_dates[j + 1])),
                c("EB_max","RO","VR","FV","EB")] * 100
        }
      }
      EBs <- rbind(new_EBs,EBs)
      non_term_dates <- rbind(new_ntds, non_term_dates)
  }
  carea <- iats$carea
  cols <- c("tia","eia","eb","fv","vr","ro","s")
  iats[, (cols) := lapply(.SD, function(x) x/carea), .SDcols = cols]
  names(iats)[3:4] <- c("ti","ei")
  list(EBs = EBs, iats = iats, non_term_dates = non_term_dates)
}

# budget_scm_time_series() ---------------------------------
#' Calculate time series of water balance componenents of a stormwater control 
#' measure (SCM)over time.
#' @param scmID a scmID of and SCM in table SCMs
#' @param runoffData rainfall and runoff time series used for EB calculation. 
#'     Must be for the period post 2001-01-01.
#' @return A list of two data.tables, budget (daily time-step) and hourly_budget
#'    (if the SCM is a raingarden or a tank with a leak to stormwater), each with 
#'    fields date, inflow, use, out, exf, et, overflow, store and void.
#' @details See WalshEtAl_LSCfoundation_figsTabs.Rmd for details on EB variables
#'     and https://urbanstreams.net/lsc/EBcalctech.html for information on water
#'     quality parameters not considered in the foundation paper
#' @examples
#' # # load tables from lsc_dbs_scms database, including all required tables
#' # load("data/lscdbsSCMs_db.rda")
#' # budget_scm_time_series(scmID = "RPLR530", runoffData = lsc_runoff) #~ 47 s
#' # # Terminal raingarden in pipeID 29 (Wattle Valley Rd North)
#' # budget_scm_time_series("R2T229", lsc_runoff) #~4 s
#' # # A non-terminal tank

budget_scm_time_series <- function(scmID, runoffData){
  start_date <- runoffData$daily$date[1]
  fin_date <- tail(runoffData$daily$date,1)
  db <- data_for_scm(scmID, fin_date)
  fin_date <- lubridate::ymd(fin_date)
  pipeID <- SCMs$pipeID[SCMs$scmID == scmID]
  projID <- SCMs$projectID[SCMs$scmID == scmID]
  change_dates <- c(db$scmProjects$installDate,
                    db$ia$constructionDate[!is.na(db$ia$constructionDate)],
                    db$scmChanges$changeDate,
                    db$parcelChanges$date)
  if(diff(range(runoffData$daily$date)) < days(158))
    stop(paste("runoffData must be at least 6 months long, to permit the SCM to equilibrate"), call. = FALSE)
  if(scmID %in% c(alldownstream(SCMs, "RPLR553", "scmID"),"RPLJ001") &
     #scms downstream of network change caused by King St Upper
     fin_date >= "2016-10-14" & start_date < "2019-06-02"){ #periods of on-off for King St Upper
    change_dates <- c(change_dates, lubridate::ymd(c("2016-07-15","2016-10-14","2017-03-17","2017-09-15",
                                                     "2018-02-05","2019-06-02")))
  }
  change_dates <- unique(c(start_date, change_dates))
  change_dates <- change_dates[change_dates >= start_date & 
                                 change_dates >= db$scmProjects$installDate[db$scmProjects$projectID == projID] & 
                                 change_dates < fin_date]
  change_dates <- change_dates[order(change_dates, decreasing = FALSE)]
  if (scmID %in% scmsDecommissioned$scmID) {
    change_dates <- unique(c(change_dates, 
                             scmsDecommissioned$decommDate[scmsDecommissioned$scmID == scmID]))
  }else{
    change_dates <- unique(c(change_dates, fin_date))
  }
  change_dates <- change_dates[change_dates >= start_date]
  if(length(change_dates) == 1) change_dates <- c(change_dates,fin_date)
  change_dates <- change_dates[order(change_dates)]
  for(i in 1:(length(change_dates)-1)) {
     db <- data_on_datex(pipeID, change_dates[i] + days(1))
     #PL SCMs of PL projects encompass all upstream impervious surfaces
     if(!is.na(scmProjects$address[scmProjects$projectID == 
                                  SCMs$projectID[SCMs$scmID == scmID]])){
      us_scms <- allupstream(SCMs, scmID, "scmID")
      db$SCMs <- db$SCMs[db$SCMs$scmID %in% us_scms,]
      db$subcs <- db$subcs[db$subcs$pipeID %in% db$SCMs$pipeID,]
      db$scmProjects <- db$scmProjects[db$scmProjects$projectID %in% db$SCMs$projectID,]
      db$parcels <- db$parcels[db$parcels$address %in% unique(db$SCMs$address),]
      db$ia <- db$ia[db$ia$address %in% db$parcels$address,]
     }
     if(i == 1){
      begin_store <- data.frame(scmID = db$SCMs$scmID, store = 0)
      rodi <- list(daily = runoffData$daily[runoffData$daily$date > change_dates[i] & 
                                              runoffData$daily$date <= change_dates[i+1]],
                   hourly = runoffData$hourly[runoffData$hourly$datetime > runoffData$hourly$datetime[which(runoffData$daily$date == change_dates[i])*24] & 
                                              runoffData$hourly$datetime <= runoffData$hourly$datetime[which(runoffData$daily$date == change_dates[i+1])*24]])
      budget_ts <- budget_scm_on_datex(scmID, db = db, runoffData = rodi,
                                       scm_start_vol = begin_store,
                                       specs_for_EB = FALSE)
      end_stores <- budget_ts$end_stores
      budget_ts$budget$imp_carea <- budget_ts$imp_carea
      budget_ts$hourly_budget$imp_carea <- budget_ts$imp_carea
      if("Nconcout" %in% names(budget_ts$hourly_budget))
        budget_ts$hourly_budget[,c("Nconcout","Pconcout","TSSout"):=NULL]
     }
     if(i > 1){
         rodi <- list(daily = runoffData$daily[runoffData$daily$date > change_dates[i] & 
                                                 runoffData$daily$date <= change_dates[i+1]],
                      hourly = runoffData$hourly[runoffData$hourly$datetime > runoffData$hourly$datetime[which(runoffData$daily$date == change_dates[i])*24] & 
                                                   runoffData$hourly$datetime <= runoffData$hourly$datetime[which(runoffData$daily$date == change_dates[i+1])*24]])
         budget_tsi <- budget_scm_on_datex(scmID, db = db, runoffData = rodi,
                                          scm_start_vol = end_stores,
                                          specs_for_EB = FALSE)
         budget_tsi$budget$imp_carea <- budget_tsi$imp_carea
         end_stores <- budget_tsi$end_stores
         budget_ts$budget <- rbind(budget_ts$budget,budget_tsi$budget)
         if("Nconcout" %in% names(budget_tsi$hourly_budget))
         budget_tsi$hourly_budget[,c("Nconcout","Pconcout","TSSout"):=NULL]
         budget_tsi$hourly_budget$imp_carea <- budget_tsi$imp_carea
         budget_tsi$budget$imp_carea <- budget_tsi$imp_carea
         if(length(budget_ts$hourly_budget) > 1)
         budget_ts$hourly_budget <- rbind(budget_ts$hourly_budget,budget_tsi$hourly_budget)
     }
     budget_ts <- budget_ts[c("budget","hourly_budget")]
  }
     budget_ts
}

# budget_subc_time_series() ---------------------------------
#' Calculate time series of water balance componenents from impervious runoff
#'  for a subcatcatchment over a specified time series
#' @param pipeID a value from table subcs identifying subcatchment of interest
#' @param runoffData rainfall and runoff time series used for EB calculation. 
#'     must be in format like Croydon dataset, as produced by 
#'     prepare_runoff_data()
#' @return a list of three data.frames: 
#'    EBs, EB stats (output of EB_scm_time_series) of all terminal 
#'    SCMs in the pipeID subcatchment, on the date of each change
#'    resulting in a change in EB;
#'    ia_ts, a daily time series of all variants of EI for the subc over the 
#'    specified time period.
#'    non_term_dates: scmIDs that were terminal SCMs for part but not all of the
#'    time series, with the date at which they stopped being terminal in the field
#'    date.
#' @details See WalshEtAl_LSCfoundation_figsTabs.Rmd for details on EB variables
#'     and https://urbanstreams.net/lsc/EBcalctech.html for information on water
#'     quality parameters not considered in the foundation paper
#' @examples
#' # system.time(budget_29 <- budget_subc_time_series(pipeID = 29,
#' #                                             runoffData = lsc_runoff)) #~1.5 min
#' # system.time(budget_53 <- budget_subc_time_series(pipeID = 53, runoffData = lsc_runoff)) #~ 26 min

budget_subc_time_series <- function(pipeID, runoffData){
  start_date <- runoffData$daily$date[1]
  fin_date <- tail(runoffData$daily$date,1)
  if(start_date < "2000-01-01")
    stop("Earliest start date is '2000-01-01'", call. = FALSE)
  allusi <- allupstream(subcs, pipeID, "pipeID")
  #For every scmProject in pipeID, find terminal scmIDs on installDate, and 
  #determine if they become non-terminal before fin_date (by istallation of an SCM downstream)
  scmps <- scmProjects[scmProjects$pipeID %in% allusi,]
  scmps <- scmps[order(scmps$installDate),]
  idates <- unique(scmps$installDate)
  idates <- idates[idates < fin_date & idates >= start_date]
  non_term_dates <- data.frame(scmID = NA, date = lubridate::ymd("2000-01-01"), 
                               stringsAsFactors = FALSE)
  if(length(idates) == 0) idates <- start_date
  for(i in 1:length(idates)){
    db <- data_on_datex(pipeID, idates[i] + days(1))
    if(i == 1){
      terminal_scms <- db$SCMs$scmID[is.na(db$SCMs$nextds) | 
                                       db$SCMs$nextds == "land"]
    }
    if(i > 1){
      new_tcsms <- db$SCMs$scmID[is.na(db$SCMs$nextds) | 
                                   db$SCMs$nextds == "land"]
      non_terms <- terminal_scms[!(terminal_scms %in% new_tcsms)]
      non_terms <- non_terms[!non_terms %in% non_term_dates$scmID]
      if(length(non_terms) > 0){
        non_term_dates <- rbind(non_term_dates,
                                data.frame(scmID = non_terms,
                                           date = idates[i],
                                           stringsAsFactors = FALSE))
      }
      terminal_scms <- unique(c(terminal_scms, new_tcsms))
    }
  }
  non_term_dates <- non_term_dates[-1,]
  #If any scms in non_term_dates table were decommissioned before their downstream SCM was constructed...
  if(sum(non_term_dates$scmID %in% scmsDecommissioned$scmID) > 0){
    decomms <- which(non_term_dates$scmID %in% scmsDecommissioned$scmID)
    for(j in decomms){
      if(scmsDecommissioned$decommDate[scmsDecommissioned$scmID == non_term_dates$scmID[j]] < non_term_dates$date[j])
        if(length(non_term_dates$date[j]) != sum(scmsDecommissioned$scmID == non_term_dates$scmID[j])) stop("1670")
      non_term_dates$date[j] <- 
        scmsDecommissioned$decommDate[scmsDecommissioned$scmID == non_term_dates$scmID[j]]
    }
  }
  # and if any other terminal scms were decommissioned, add them to the non_term_dates table, 
  # so that their decommissioning can be accounted for below
  if(sum(terminal_scms %in% scmsDecommissioned$scmID & !terminal_scms %in% non_term_dates$scmID) > 0){
    decomms <- which(terminal_scms %in% scmsDecommissioned$scmID & !terminal_scms %in% non_term_dates$scmID)
    dcs <- terminal_scms[terminal_scms %in% scmsDecommissioned$scmID & !terminal_scms %in% non_term_dates$scmID]
    for(j in 1:length(dcs)){
      if(length(dcs[j]) != sum(scmsDecommissioned$scmID == dcs[j])) stop()
      non_term_dates <- rbind(non_term_dates,
                              data.frame(scmID = dcs[j],
                                         date = scmsDecommissioned$decommDate[scmsDecommissioned$scmID == dcs[j]],
                                         stringsAsFactors = FALSE))
    }
  }
  # db <- data_on_datex(pipeID, fin_date)
  # carea <- db$subcs$carea[db$subcs$pipeID == pipeID]
  subc_daily <- ia_ts(pipeID, start_date = start_date, fin_date = fin_date)
  subc_hourly <- runoffData$hourly[,c("datetime","date"), with = FALSE]
  setkey(subc_hourly,date)
  subc_hourly <- subc_hourly[subc_daily]
  subc_hourly$non_scm_eia <- subc_hourly$eia
  subc_hourly$ei_runoff <- subc_hourly$eia*runoffData$hourly$runoff_mm
  subc_hourly$runoff_mm <- runoffData$hourly$runoff_mm
  subc_daily$non_scm_eia <- subc_daily$eia
  subc_daily$ei_runoff <- subc_daily$eia*runoffData$daily$runoff_mm
  subc_daily$ei_scm_runoff <- 0 #start this at zero and build it up with each scm
  subc_hourly$ei_scm_runoff <- 0
  subc_daily$runoff_mm <- runoffData$daily$runoff_mm
  
  subc_daily$use <- subc_daily$out <- subc_daily$exf <- subc_daily$et <- 
      subc_daily$overflow <- subc_daily$store <- subc_daily$void <- 0
  for(i in 1:length(terminal_scms)){
    tscmi <- budget_scm_time_series(terminal_scms[i],runoffData)
    names(tscmi$hourly_budget)[1] <- "datetime"
    # restrict terminal_scms[i] budget to periods when it was actually terminal
    if(terminal_scms[i] %in% non_term_dates$scmID){
    tscmi$hourly_budget[tscmi$hourly_budget$date >= non_term_dates$date[non_term_dates$scmID == terminal_scms[i]],
                        `:=` (overflow = 0, imp_carea = 0)]
    tscmi$budget[tscmi$budget$date >= non_term_dates$date[non_term_dates$scmID == terminal_scms[i]],
                          `:=` (use = 0, out = 0, exf = 0, et = 0, overflow = 0, store = 0, void = 0, imp_carea = 0)]
    }
    setkey(subc_hourly,datetime)
    subc_hourly[tscmi$hourly_budget,
                non_scm_eia := non_scm_eia - tscmi$hourly_budget$imp_carea]
    subc_hourly[tscmi$hourly_budget,
                ei_scm_runoff := ei_scm_runoff + tscmi$hourly_budget$overflow + tscmi$hourly_budget$out]
    setkey(subc_daily,date)
    subc_daily[tscmi$budget,
                non_scm_eia :=  non_scm_eia - tscmi$budget$imp_carea]
    subc_daily[tscmi$budget, ei_scm_runoff :=  ei_scm_runoff +
                                               tscmi$budget$overflow + tscmi$budget$out]
    subc_daily[tscmi$budget, `:=` (use = use + tscmi$budget$use,
                                   out = out + tscmi$budget$out, 
                                   exf = exf + tscmi$budget$exf,
                                   et = et + tscmi$budget$et,
                                   overflow = overflow + tscmi$budget$overflow,
                                   store = store + tscmi$budget$store,
                                   void = void + tscmi$budget$void)]
  }
  subc_daily[, ei_scm_runoff := non_scm_eia*runoffData$daily$runoff_mm + ei_scm_runoff]
  subc_hourly[, ei_scm_runoff := non_scm_eia*runoffData$hourly$runoff_mm + ei_scm_runoff]
  list(hourly = subc_hourly, daily = subc_daily)
}


# plotlEItrends() ---------------------------------
#' Plot output of budget_subc_time_series

plotlEItrends <- function(pipeiTrend, legend = FALSE, ...) {
  with(pipeiTrend, plot(date, log10(100*ei + 0.1),
                        type = 'l', ylim = c(-0.8,1.5), lty = 2,lwd = 2, axes = FALSE,
                        xlab = "", ylab = "", col = "black"))
  with(pipeiTrend, lines(date, log10(100*eb + 0.1), col = "black"))
  with(pipeiTrend, lines(date, log10(100*fv + 0.1), col = "orange"))
  with(pipeiTrend, lines(date, log10(100*ro + 0.1), col = "red"))
  with(pipeiTrend, lines(date, log10(100*vr + 0.1), col = "green"))
  with(pipeiTrend, lines(date, log10(100*s + 0.1), col = "blue", lty = 3))
  axis(1, at=c(0,1577836800), labels=c("",""), lwd.ticks=0)
  axis.Date(1, pipeiTrend$date,
               at = seq.Date(lubridate::ymd("1995-01-01"),lubridate::ymd("2020-01-01"),"5 years"))
  axis(2,at = c(-2,log10(c(0.03,0.1,0.3,1,3,10,30,100) + 0.1)), labels = c("",0.03,0.1,0.3,1,3,10,30,100), las = 1)
  if(legend){
    legend(lty = c(2,1,1,1,1,3),lwd = c(2,1,1,1,1,1),
           col = c("black","black","orange","red","green","blue"),
           legend = c("EI","EB","FV","RO","VR","S"), ...)
  }
}

# bfi_ladson() ---------------------------------
#' Baseflow separation function

bfi_ladson <- function(val_to_reflect, alpha, raw_data) {
  
  #START BFI!
  
  padded <- NULL
  
  j <- 1	
  
  for(i in 1:((NROW(raw_data)+(2*val_to_reflect)))) {
    
    if(i >= 1 & i <= val_to_reflect) {
      
      padded[i] <- raw_data[val_to_reflect + (2-i)] }
    
    if(i > val_to_reflect & i <= (NROW(raw_data)+val_to_reflect)) {
      
      padded[i] <- raw_data[i-val_to_reflect] }
    
    if(i > (NROW(raw_data)+val_to_reflect)) {
      
      #putting check here
      
      pos_here <- NROW(raw_data) - j   	    
      padded[i] <- raw_data[pos_here]   
      j <- j + 1  }}
  
  #padded is the time series of which to calculate baseflow
  #now calculate baseflow
  
  #Declare an array 9 times [quick flow, forward backward etc] *    NROW(padded)
  baseflow_data <- array(dim = c(NROW(padded), 9))
  
  #start the BFI calculation
  for(i in 1:NROW(padded)) {
    
    #do first quickflow, initial value of quickflow = first value of padded data (from reflected set)
    
    if (i==1) {	baseflow_data[i,1] <- padded[i] }
    
    else {
      
      baseflow_data[i,1] <- (alpha * baseflow_data[(i-1), 1]) + (((1 + alpha)/(2))*(padded[i] - padded[i-1])) }
    
    #first condition of QF
    if(baseflow_data[i,1] > 0) { 
      baseflow_data[i,2] <- baseflow_data[i,1] } 
    else {
      baseflow_data[i,2] <- 0  }     
    
    #first pass baseflow 
    
    baseflow_data[i,3] <- padded[i] - baseflow_data[i,2] }
  
  ######### SECOND, BACKWARD pass
  for(i in 1:NROW(padded)) {
    
    if(i == 1) {
      #second backward qf, == last row of first pass baseflow
      baseflow_data[NROW(padded),4] <- baseflow_data[NROW(padded),3]   }
    
    else {
      #qf backward, post initial value
      baseflow_data[(NROW(padded)-i+1),4] <- (alpha * baseflow_data[(NROW(padded)-i+2),4]) + (((1 + alpha)/(2))*(baseflow_data[(NROW(padded)-i+1),3] - baseflow_data[(NROW(padded)-i+2),3]))   }  }
  
  ####condition second pass
  for(i in 1:NROW(padded)) {	
    #second pass conditioned of B
    if(baseflow_data[i,4] > 0) { 
      baseflow_data[i,5] <- baseflow_data[i,4] }
    else {	baseflow_data[i,5] <- 0  }
    
    #second pass baseflow
    baseflow_data[i,6] <- baseflow_data[i,3] - baseflow_data[i,5] }
  
  
  #### DO THIRD FORWARD PASS NOW 
  #quick flow third pass		
  
  for(i in 1:NROW(padded)) {
    
    #third quickflow initial value = value of base_6
    
    if (i==1) {	baseflow_data[i,7] <- baseflow_data[i,6] }
    
    #post initial value 
    
    else {
      
      baseflow_data[i,7] <- (alpha * baseflow_data[(i-1), 7]) + (((1 + alpha)/(2))*(baseflow_data[i,6] -baseflow_data[(i-1),6])) }
    
    ### condition third pass
    if(baseflow_data[i,7] > 0) { 
      baseflow_data[i,8] <- baseflow_data[i,7] } 
    else {
      baseflow_data[i,8] <- 0  }     
    
    #third pass baseflow 
    baseflow_data[i,9] <- baseflow_data[i,6] - baseflow_data[i,8] }
  
  
  #final baseflow 
  final_baseflow <- baseflow_data[(val_to_reflect+1):(NROW(padded)-val_to_reflect),9]
  
  return(final_baseflow) } 


# extract_wq_data() ---------------------------------
#' Extract water quality data from the lsc database
#' @param var a vector of water quality variables (values of var in the wqVar 
#'   table). Default is "all" variables in the dataset
#' @param site a vector of sites (values of sitecode in the sites table). Default
#'   is "all" sites in the dataset
#' @param period a vector of two dates spanning the period of interest, expressed as 
#'   as character strings in the format "yyyy-mm-dd". Default is "all" sites in the 
#'   dataset.
#' @return A data.frame with 9 columns: 
#'   sitecode, as detailed in the sites table
#'   samplecode, as detailed in the wqSamples table
#'   wqvar, as detailed in the wqVar table
#'   conc, concentration of the variable in units specified for that variable in the wqVar table
#'   qualitycode, data quality
#'   dateTime6min, time of the sample rounded to the nearest 6-minutes to match 
#'                 flow and rainfall data in other tables of the database
#'   tripNo, as detailed in the wqSamples and fieldTrips tables
#'   flowType, b or f; a not highly reliable classification of baseflow v high flow
#' @examples
#'   extract_wq_data(var = c("FRP","TP"), 
#'                   site = c("LIS0001","LSS0001"),
#'                   period = c("2017-01-01", "2017-02-01"))

extract_wq_data <- function(var = "all",
                            site = "all",
                            period = "all",
                            withFlow = FALSE){
  if(var[1] == "all"){
    var <- sqlQuery("SELECT var FROM wqVars;", db = "lsc")$var
  }
  if(site[1] == "all"){
    site <- sqlQuery("SELECT sitecode FROM sites;", db = "lsc")$sitecode
  }
longsql <- paste("SELECT sites.sitecode, wqData.samplecode, wqData.wqvar, 
                       wqData.conc, wqData.qualitycode, wqSamples.dateTime6min, 
                       fieldTrips.tripNo, wqSamples.flowType",
        ifelse(withFlow, ", streamflow.flow ", " "),
        "FROM wqData 
                      JOIN wqSamples ON 
                       wqData.samplecode = wqSamples.samplecode 
                       JOIN fieldTrips ON
                        wqSamples.tripcode = fieldTrips.tripcode
                        JOIN sites ON 
                        wqSamples.sitecode = sites.sitecode",
        ifelse(withFlow, " JOIN streamflow ON
                                 wqSamples.sitecode = streamflow.sitecode AND 
                                 wqSamples.dateTime6min = streamflow.dateTime ", " "),
        "WHERE wqData.wqvar IN ('",
        paste(var, collapse = "', '"),
        "') AND 
                       wqSamples.sitecode IN ('",
        paste(site, collapse = "', '"), 
        ifelse(period[1] == "all", "');", 
               paste("') AND wqSamples.dateTime6min BETWEEN '", 
                     period[1], "' AND '", period[2], "';", sep = "")),
        sep = "")
wq <- sqlQuery(strwrap(longsql, width=10000, simplify=TRUE), db = "lsc")
wq
}

ct <- function (rows, cols, values = NULL, FUN = sum, convertNAToZero = TRUE,...) 
{
  if(!is.vector(rows)) rows <- as.vector(rows)
  if(!is.vector(cols)) cols <- as.vector(cols)
  if(is.null(values)) values <- rep(1,length(rows))
  results <- tapply(values, list(rows, cols), FUN, ...)
  if(convertNAToZero)
    results[is.na(results)] <- 0
  as.data.frame(results)
}

#function for ordering vectors consistently, with any NAs sent to the end.
beetroot <- function(x) {
  # count NA
  num.na <- sum(is.na(x))
  # remove NA
  x <- x[!is.na(x)]
  # glue the number of NAs at the end
  x <- c(x, rep(NA, num.na))
  return(x)
}

#extract EI data from Foundation paper analyses for a set of samples identified by site and date.
#ei data are converted as log[10](x*100 + 0.1) (i.e. log of % EI)
#delEI is also calculated as difference from 
#This only works from a sibling directory of a clone of https://github.com/cjbwalsh/lsc_foundation
extract_ei <- function(sample_data, #must have fields samplecode, sitecode and date (as.Date)
                       ref_date = as.Date("2011-05-24"), #reference date from which delEI is calculated
                       dep_dates = seq.Date(as.Date("2010-11-12"),max(sample_data$date),"days")) {
  #if dep_dates is a vector of dates, adjustments are made to ei estimates for 
  #LIS0004 and DBS0008 to exclude catchments of tribs upstream on those dates.  
  #No adjustments are made if dep_dates[1] == NA. Only make such adjustments for 
  #hydrology and wq variables where value at downstream site is a clear sum of 
  #upstream inputs + inputs in between. Not so clearly the case for biological variables.
  
  #Load and compile EI change data from lsc_foundation repository 
  # (calculated using data_preparation.R in that repository)
  if(sum(c("ti", "ei", "eb", "fv", "vr", "ro", "s") %in% names(sample_data)) > 1)
    stop(call. = FALSE, "sample_data already contains EI fields ('ti', 'eb', 'fv', 'vr', 'ro', or 's')")
  cats <- sqlQuery("SELECT * FROM cats WHERE sitecode IN 
                   ('SAS0002', 'LYR0007', 'OLN0009','BRS0015','FER0006','DBS0004',
                   'DBS0008', 'LIS0004H', 'LIS0001','LSS0001','LSN0001');",  
                   #replace 'LIS0004H' with 'LIS0004' for ecological sites
                   "lsc_dbs_scms")      #see BACRIfunctions.R for this function
  cats$sitecode <- substr(cats$sitecode,1,7)
  #Treatments table updated 28 Aug 2020 by CW (see lsc_database_corrections_cw.R)
  ei_data <- data.table::data.table(sqlQuery("SELECT * FROM Treatments;", db = "lsc"))
  ei_data$date <- as.Date(ei_data$date)
  ref <- ei_data[ei_data$date == ref_date,]
  ref <- cbind(ref[,.(sitecode,date)],log10(ref[,.(ti,ei,eb,fv,vr,ro,s)]*100 + 0.1))
  #CW could not work out how to merge these two data tables correctly!
  sample_data <- data.frame(sample_data)
  ei_data <- data.frame(ei_data)
  for(i in 3:length(names(ei_data))){
    sample_data[,names(ei_data)[i]] <- ei_data[match(paste(sample_data$sitecode,sample_data$date),
                                                     paste(ei_data$sitecode,ei_data$date)),i]
  }
  sample_data <- data.table::data.table(sample_data)
  #Log all the EIvars as percentages:
  sample_data$lei <- log10(sample_data$ei*100 + 0.1)
  sample_data$ls <-  log10(sample_data$s*100 + 0.1)
  sample_data$leb <- log10(sample_data$eb*100 + 0.1)
  sample_data$lro <- log10(sample_data$ro*100 + 0.1)
  sample_data$lvr <- log10(sample_data$vr*100 + 0.1)
  sample_data$lfv <- log10(sample_data$fv*100 + 0.1)
  #delEIs
  sample_data$del_lei <- sample_data$lei - ref$ei[match(sample_data$sitecode,ref$sitecode)]
  sample_data$del_ls <- sample_data$ls - ref$s[match(sample_data$sitecode,ref$sitecode)]
  sample_data$del_leb <- sample_data$leb - ref$eb[match(sample_data$sitecode,ref$sitecode)]
  sample_data$del_lro <- sample_data$lro - ref$ro[match(sample_data$sitecode,ref$sitecode)]
  sample_data$del_lvr <- sample_data$lvr - ref$vr[match(sample_data$sitecode,ref$sitecode)]
  sample_data$del_lfv <- sample_data$lfv - ref$fv[match(sample_data$sitecode,ref$sitecode)]
  if(!is.na(dep_dates[1])){
    sample_data$nextds <- c(rep("LIS0004",3),"DBS0008")[match(sample_data$sitecode,c("LIS0001","LSS0001","LSN0001","DBS0004"))]
    #distances between sites in km, estimated from mwstr_v1.1 (see database mwstr)
    sample_data$D2nextds <- c(2.040,1.837,1.294,0.851)[match(sample_data$sitecode,c("LIS0001","LSS0001","LSN0001","DBS0004"))]
    sample_data$scarea <- cats$carea_m2[match(sample_data$sitecode, cats$sitecode)]
    cols <- c("ti","ei","eb","fv","vr","ro","s")
    dbs8 <- sample_data[sample_data$sitecode == "DBS0008",..cols]
    dbs8 <- dbs8*cats$carea_m2[cats$sitecode == "DBS0008"]
    dbs4 <- sample_data[sample_data$sitecode == "DBS0004",..cols]
    dbs4 <- dbs4*cats$carea_m2[cats$sitecode == "DBS0004"]
    ei_103 <- ei_data[ei_data$sitecode == "DBS0008",]
    dep_date_index <- which(ei_103$iats$date %in% dep_dates)
    for(col in c("ti","ei","eb","fv","vr","ro","s"))
      set(ei_103, i = dep_date_index, j=col, 
          value=(dbs8[dep_date_index, ..col] - dbs4[dep_date_index, ..col])/(cats$carea_m2[cats$sitecode == "DBS0008"] - 
                                                                               cats$carea_m2[cats$sitecode == "DBS0004"])[dep_date_index])
    # ei_103$carea[dep_date_index] <- (cats$carea_m2[cats$sitecode == "DBS0008"] - 
    #                                         cats$carea_m2[cats$sitecode == "DBS0004"])[dep_date_index]
    lis4 <- sample_data[sample_data$sitecode == "LIS0004",..cols]
    lis4 <- lis4*cats$carea_m2[cats$sitecode == "LIS0004"]
    lis1 <- sample_data[sample_data$sitecode == "LIS0001",..cols]
    lis1 <- lis1*cats$carea_m2[cats$sitecode == "LIS0001"]
    lss1 <- sample_data[sample_data$sitecode == "LSS0001",..cols]
    lss1 <- lss1*cats$carea_m2[cats$sitecode == "LSS0001"]
    lsn1 <- sample_data[sample_data$sitecode == "LSN0001",..cols]
    lsn1 <- lsn1*cats$carea_m2[cats$sitecode == "LSN0001"]
    ei_71 <- ei_data[ei_data$sitecode == "LIS0004",]
    for(col in c("ti","ei","eb","fv","vr","ro","s")) 
      set(ei_71, i = dep_date_index, j=col, 
          value=(lis4[dep_date_index, ..col] - lis1[dep_date_index, ..col] - lss1[dep_date_index, ..col] - lsn1[dep_date_index, ..col])/
            (cats$carea_m2[cats$sitecode == "LIS0004"] - cats$carea_m2[cats$sitecode == "LIS0001"] - 
               cats$carea_m2[cats$sitecode == "LSS0001"] - cats$carea_m2[cats$sitecode == "LSN0001"])[dep_date_index])
    # ei_71$carea[dep_date_index] <- (cats$carea_m2[cats$sitecode == "LIS0004"] - cats$carea_m2[cats$sitecode == "LIS0001"] - 
    #                                        cats$carea_m2[cats$sitecode == "LSS0001"] - cats$carea_m2[cats$sitecode == "LSN0001"])[dep_date_index]
    
    #re-assemble the adjusted daily EI data and merge with sample_data
    spa_data <- rbind(data.table::data.table(sitecode = "DBS0008", ei_103),
                      data.table::data.table(sitecode = "LIS0004", ei_71))
    sample_data_bu <- sample_data
    sample_data$lei[sample_data$sitecode == "DBS0008"] <- log10(spa_data$ei[match(sample_data$date[sample_data$sitecode == "DBS0008"],
                                                                                  spa_data$date)]*100 + 0.1)
    sample_data$lei[sample_data$sitecode == "LIS0004"] <- log10(spa_data$ei[match(sample_data$date[sample_data$sitecode == "LIS0004"],
                                                                                  spa_data$date)]*100 + 0.1)
    sample_data$ls[sample_data$sitecode == "DBS0008"] <- log10(spa_data$s[match(sample_data$date[sample_data$sitecode == "DBS0008"],
                                                                                spa_data$date)]*100 + 0.1)
    sample_data$ls[sample_data$sitecode == "LIS0004"] <- log10(spa_data$s[match(sample_data$date[sample_data$sitecode == "LIS0004"],
                                                                                spa_data$date)]*100 + 0.1)
    sample_data$leb[sample_data$sitecode == "DBS0008"] <- log10(spa_data$eb[match(sample_data$date[sample_data$sitecode == "DBS0008"],
                                                                                  spa_data$date)]*100 + 0.1)
    sample_data$leb[sample_data$sitecode == "LIS0004"] <- log10(spa_data$eb[match(sample_data$date[sample_data$sitecode == "LIS0004"],
                                                                                  spa_data$date)]*100 + 0.1)                                                             
    sample_data$lro[sample_data$sitecode == "DBS0008"] <- log10(spa_data$ro[match(sample_data$date[sample_data$sitecode == "DBS0008"],
                                                                                  spa_data$date)]*100 + 0.1)
    sample_data$lro[sample_data$sitecode == "LIS0004"] <- log10(spa_data$ro[match(sample_data$date[sample_data$sitecode == "LIS0004"],
                                                                                  spa_data$date)]*100 + 0.1)
    sample_data$lvr[sample_data$sitecode == "DBS0008"] <- log10(spa_data$vr[match(sample_data$date[sample_data$sitecode == "DBS0008"],
                                                                                  spa_data$date)]*100 + 0.1)
    sample_data$lvr[sample_data$sitecode == "LIS0004"] <- log10(spa_data$vr[match(sample_data$date[sample_data$sitecode == "LIS0004"],
                                                                                  spa_data$date)]*100 + 0.1)
    sample_data$lfv[sample_data$sitecode == "DBS0008"] <- log10(spa_data$fv[match(sample_data$date[sample_data$sitecode == "DBS0008"],
                                                                                  spa_data$date)]*100 + 0.1)
    sample_data$lfv[sample_data$sitecode == "LIS0004"] <- log10(spa_data$fv[match(sample_data$date[sample_data$sitecode == "LIS0004"],
                                                                                  spa_data$date)]*100 + 0.1)
    
    cols <- c("ti","ei","eb","fv","vr","ro","s")
    ref <- data.frame(ref)
    ref[ref$sitecode == "LIS0004", cols] <- log10(ei_71[ei_71$date == ref_date, cols]*100 + 0.1)
    ref[ref$sitecode == "DBS0008", cols] <- log10(ei_103[ei_103$date == ref_date,cols]*100 + 0.1)
    sample_data$del_lei[sample_data$date %in% dep_date_index] <- sample_data$lei[sample_data$date %in% dep_date_index] - ref$ei[match(sample_data$sitecode,ref$sitecode)]
    sample_data$del_ls[sample_data$date %in% dep_date_index] <- sample_data$ls[sample_data$date %in% dep_date_index] - ref$s[match(sample_data$sitecode,ref$sitecode)]
    sample_data$del_leb[sample_data$date %in% dep_date_index] <- sample_data$leb[sample_data$date %in% dep_date_index] - ref$eb[match(sample_data$sitecode,ref$sitecode)]
    sample_data$del_lro[sample_data$date %in% dep_date_index] <- sample_data$lro[sample_data$date %in% dep_date_index] - ref$ro[match(sample_data$sitecode,ref$sitecode)]
    sample_data$del_lfv[sample_data$date %in% dep_date_index] <- sample_data$lfv[sample_data$date %in% dep_date_index] - ref$fv[match(sample_data$sitecode,ref$sitecode)]
  }
  list(sample_data = sample_data, ref_ei = ei_data[ei_data$date == ref_date,])
}



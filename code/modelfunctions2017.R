library(lubridate)
library(gtools)

##########################################################
# INITIAL LOSS CALCULATION varying spell between events according to ET...
lose.initET <- function(rain, et, initloss.mm = 1){
  #runs through time series vector of rainfall values and removes (converts to zero)
  #the first initloss.mm in an event. Using et to vary loss means there is no need to 
  #define spell length
  rainl <- rain
  void <- rep(initloss.mm,length(rain))
  #Assume et loss applies in the first timestep?
  rainl[1] <- max(0,rain[1] - void[1] - et[1])
  for (i in 2:length(rain)) {
    rainl[i] <- max(rain[i] - void[i-1] - et[i], 0)
    void[i] <-  max(min(initloss.mm, void[i-1] + et[i] - rain[i]),0)
  }
  rainl
}

##########################################################
# INITIAL LOSS CALCULATION
lose.init <- function(x, initloss.mm = 1, ante.t = 1){
  #runs through time series vector of rainfall values and removes (converts to zero)
  #the first initloss.mm in an event. An event is defined as string of records
  #with >0 rain with no more than ante.t timesteps before or during with 0 rain
  event <- ifelse(x[1] == 0, FALSE, TRUE)
  spell <- ifelse(x[1] == 0, 1, 0)
  loss <- initloss.mm
  xl <- x
  xl[1] <- max(0,x[1] - initloss.mm)
  loss <- max(loss - x[1], 0)
  for (i in 2:length(x)) {
    if (!event) {
      spell <-  spell + 1
    }
    if (x[i] > 0) {
      event <- TRUE
      spell <- 0
      xl[i] <- max(x[i] - loss, 0)
      loss <- max(loss - x[i], 0)
    }
    if (x[i] == 0 & event) {
      spell <- spell + 1
      if (spell >= ante.t) {
        event <- FALSE
        loss <- initloss.mm
      }
    }
  }
  xl
}

##########################################################
# TANK MODEL
# runoff   data.frame with daily runoff data in mm (runoff_mm) or hourly if hourly.time.step = TRUE
# tankvol       tank volume in L
# hourly.time.step logical hourly data used if TRUE
# tankbegin     volume of water in L at start of run
# carea         tank's catchment area in square metres (of direct runoff)
# additional.inflow if not zero, then a series of same length as dailyrunoff of daily inputs in litres
#               from additional sources (such as a rain.garden Q.inf)
# propcon       proportion of carea that is connected to stormwater
# initloss      initial loss in mm (accounted for in dailyrunoff, so = zero)
# firstflush    first flush bypass volume in L)
# npeople       number of people in house
# toilet        average toilet usage per person per day in L
# other         any other uses for water from the tank, expressed as a string 
#               that can be converted to a vector of length 12, with the 
#               volume of uses in each calendar month (Jan-Dec) in L/month
# wmac1         average washing machine use per day in L for 1 person
# wmacadd       washing machine use for each additional person
# hotwater      average daily hot water use per person in L
# outdoor.annual    annual outdoor irrigation use in L per 100 sq m of garden
# garden.area   area of garden to be irrigated from the tank in sq m
# outdoor.dist
# leak.rate     rate of leak in L/d (as default try carea*0.1*24)
# leak.at.propn.of.capacity     proportion of tank volume above the leak
# leak.months      months in which tank is allowed to leak
# nlots         usage multiplier for precinct scale application

tankmodel <- function(runoff,
                      et,
                      hourly.time.step = FALSE,
                      tankvol,
                      tankbegin = tankvol/2,
                      carea,
                      additional.inflow = 0,
                      propcon = 1,
                      initloss = 0,
                      firstflush = carea * 0.2,
                      npeople = 2,
                      toilet = 18.9,
                      other = rep(0, 12), #unit = L/month
                      wmac1 = 35.31,
                      wmacadd = 23.54,
                      hotwater = 46.9,
                      outdoor.annual = 12971,
                      garden.area = 350,
                      outdoor.dist = c(0.27, 0.21, 0.09, 0.07, 0.05, 0, 0, 0, 0.03, 0.04, 0.04, 0.2),
                      leak.rate = 0,  #unit = L/d
                      leak.at.propn.of.capacity = 0.5,
                      leak.months = 6:11,
                      nlots = 1, ...)
  {
  # Monthly distribution of garden watering
#  options(chron.origin = c(month = 12, day = 30, year = 1899))
  if(length(additional.inflow) > 1 & length(additional.inflow) != dim(runoff)[1])
    stop("additional.inflow and runoff not the same number of time steps")
  # Create date
#dailyrunoff$date <- as.POSIXct(dailyrunoff$dro1y$date, tz = "UTC")
  
  # hacky fix to permit hourly modelling
  timestep_multiplier <- ifelse(hourly.time.step,24,1)
  
  # Calculate number of days and years in the dataset
  ndays <- nrow(runoff)/timestep_multiplier
  nyears <- round(ndays/(365.25), 0)

  # Create the daily outdoor and other water demand datasets
  # Each is a vector of water use for each day (so for outdoor and other monthly estimates divide by approx no. days per month)
  timestep.outdoor <- outdoor.dist[rep(match(month(runoff$date), 1:12))] * outdoor.annual * garden.area * 12/(30.4*36500*timestep_multiplier)
  timestep.other <- other[match(month(runoff$date), 1:12)]/(30.4*timestep_multiplier)

  # Annual outdoor demand
  annual.outdoor <- sum(timestep.outdoor)/(1000 * nyears)

  # Calculate internal water usage
  toilet <- npeople * toilet / timestep_multiplier
  hotwater <- npeople * hotwater / timestep_multiplier

  if(npeople < 1) {
    wmac <- 0
  } else {
    wmac <- wmac1 + (npeople - 1) * wmacadd / timestep_multiplier
  }

  # Create usage combinations matrix
  # Rows are a use type (ie row 1 is garden use, row 2 is other use)
  # Columns are the usage combinations (ie garden, garden + other)
  # Value of 1 = included in combination, 0 is not included
  # If "other" usage is equal to 0 it is not included in the combinations

  usage <- data.frame()

  # Create labels using use combinations vector
  labels <- c("garden", "other", "toilet", "laundry", "hot water")
  # Create matrix of daily demand for each use for all days
  usage <- data.frame(cbind(garden=timestep.outdoor, other=timestep.other, 
                            rep(toilet, ndays * timestep_multiplier), 
                            rep(wmac, ndays * timestep_multiplier), 
                            rep(hotwater, ndays * timestep_multiplier)))
  colnames(usage) <- labels

  # Remove all zero uses
  ###this line (added by Rhys Eddy) caused problems with EBcalc.htm, so removed
  #usage <- usage[,!colSums(usage)==0,drop=FALSE]

  # Update list of descriptions
  usage.desc <- colnames(usage)

  # Only make combinations if more than 1 use
  ###this line (added by Rhys Eddy) caused problems with EBcalc.htm, so removed
  #  if(length(usage.desc) > 1) {

    # All possible mixes of useage (minus no usage scenario)
    usecomb <- t((permutations(2,length(usage.desc),repeats.allowed=TRUE) - 1)[-1,])

    # Create vector of text labels for each use combination
    usage.descs <- apply(usecomb, 2, function(x) usage.desc[x*c(1:length(usage.desc))])
    usage.descs <- unlist(lapply(usage.descs, paste, collapse = "+"))

    n.uses <- length(usage.descs)

    colnames(usage) <- usage.desc

    # Calculate total daily comsumption for each use combination
    # Simply multiply the daily uses matrix by the use combination matrix
    consumption <- as.matrix(usage) %*% usecomb
    colnames(consumption) <- usage.descs
#   } else {
#     consumption <- usage
#     usage.descs <- usage.desc
#     n.uses <- length(usage.descs)
#   }
###these lines (added by Rhys Eddy) caused problems with EBcalc.htm, so removed

  # Multiply the use values by the number of lots in the reuse scheme
  # If no lots then consumption is 0
  if(nlots < 1) {
    # Create new 0 data frame
    consumption <- data.frame(none=rep(0,ndays * timestep_multiplier))

    # Update usage decs and number of uses
    usage.descs <- colnames(consumption)
    n.uses <- length(usage.descs)
  } else {
    consumption <- consumption * nlots
  }

  # Leak calcs
  leak.days <- month(runoff$date)#month.day.year(chron(2:(ndays + 1)))$month
  potential.leak <- rep(0, ndays*timestep_multiplier)
  potential.leak[leak.days %in% leak.months] <- leak.rate/timestep_multiplier

  # Create leak matrix and make leak = 0 if there is garden usage
  potential.leak <- as.data.frame(potential.leak)
  potential.leak <- potential.leak[,rep(1,n.uses),drop=FALSE]

  #CW 30/12/2013 has chosen to remove this exclusion -
  #but adds a caveat in the EBcalculator to check that the area of
  #garden being watered is different from the area receiving the leak
  #if(length(grep("garden", usage.descs)))
  #  potential.leak[,grep("garden", usage.descs)] <- 0

  colnames(potential.leak) <- usage.descs

  # Create beginning tank vector for each use
  tankbegin <- ifelse(length(tankbegin) == 1, rep(tankbegin, n.uses), tankbegin)

  annual.consumption <- colSums(consumption)/(1000 * nyears)
  annual.outdoor.consumption <- rep(0, ncol(consumption))
  annual.outdoor.consumption[grep("garden", usage.descs)] <- round(annual.outdoor, 0)

  # Calculate runoff volume vector (L/d)
  runoffvol <- carea * (runoff$runoff_mm - initloss)

  # Quickest way to remove negatives and replace with 0
  runoffvol <- (runoffvol + abs(runoffvol)) / 2

  # Calculate first flush vector
  tmp <- runoffvol - firstflush
  tmp <- (tmp + abs(tmp)) / 2
  firstflush <- runoffvol - tmp
  firstflush <- (firstflush + abs(firstflush)) / 2

  # Calculate the inflow vector
  inflow <- runoffvol - firstflush
  inflow <- (inflow + abs(inflow)) / 2 + additional.inflow

  # Create output list
  budget <- list(use = matrix(nrow = ndays*timestep_multiplier, ncol = n.uses),
                 leak = matrix(nrow = ndays*timestep_multiplier, ncol = n.uses),
                 overflow = matrix(nrow = ndays*timestep_multiplier, ncol = n.uses),
                 store = matrix(nrow = ndays*timestep_multiplier, ncol = n.uses))

  # Iterate
  for(i in 1:(ndays*timestep_multiplier)) {
    if (i == 1){
      budget$store[i,] <- tankbegin
    }else{
      budget$store[i,] <- budget$store[i - 1, ]
    }
    budget$use[i,] <- pmin(consumption[i, , drop=FALSE], budget$store[i,])

    budget$leak[i,] <- pmin(budget$store[i,] - budget$use[i,] -
                              (leak.at.propn.of.capacity) * tankvol,
                            as.numeric(potential.leak[i, ]))
    budget$leak[i,] <- (budget$leak[i,] + abs(budget$leak[i,])) / 2

    budget$overflow[i,] <- budget$store[i,] + inflow[i] -
                           budget$use[i,] - budget$leak[i,] - tankvol
    budget$overflow[i,] <- (budget$overflow[i,] + abs(budget$overflow[i,]))/2

    budget$store[i,] <- budget$store[i,] + inflow[i] - budget$use[i,] -
                        budget$leak[i,] - budget$overflow[i,]
    budget$store[i,] <- (budget$store[i,] + abs(budget$store[i,])) / 2
   
    }

  # Output results
  list(flows = budget, ff = firstflush, demand = consumption,
       usage = usage, usage.descs = usage.descs, inflow = inflow)

}

##########################################################
# tank.EBstats FUNCTION
# Function to calculate EB scores from tank model outputs
#
# dailyrunoff, budget       output from tankmodel.R (in daily timesteps)
# subset.period             if statsare required for a subset.period of the budget time series
# usage.descs               output from tankmodel.R if default, lists all usage combinations. can specify the one of interest
# carea                     catchment area of direct tank inflow (sq m)
# upstream.tanks
# carea.additional.inflow   catchment area of source of additional inflow (sq m)
# mean.annrain.mm           long term mean annual rainfall (mm)
# soil.area.receiving.leak  an arbitrary value for now: zero if going to stormwater
# assume.leak.wq.perfect    if leak goes to soil or to a filter
# ndaysro.target            days of runoff target
# reward.overextraction     if TRUE, vr.index can be > 1 if use exceeds vr target.  if FALSE max(vr.index) = 1
#                           (and overextraction is not penalized....as it can certainly be made up for from pavearea)

tank.EBstats <- function(dailyrunoff, budget, usage.descs, carea, upstream.tanks = list(),
                         carea.additional.inflow = 0, mean.annrain.mm = Croydon$rain.mm.y,
                         soil.area.receiving.leak = 0, assume.leak.wq.perfect = FALSE,
                         ndaysro.target = 12, reward.overextraction = FALSE, ...) {

  tcarea <- carea

  if (length(upstream.tanks) > 0) {
    for (i in 1:length(upstream.tanks)) {
      tcarea <- tcarea + upstream.tanks[[i]]$summary$tcarea[1]
    }
  }

  nyears <- round(dim(dailyrunoff)[1]/365.25, 0)
  total.runoff <- sum(dailyrunoff$runoff_mm) * tcarea
  Zhang.forest.ro <- mean.annrain.mm * (1 - (1 + 2820/mean.annrain.mm)/(1 + 2820/mean.annrain.mm + mean.annrain.mm/1410))
  Zhang.pasture.ro <- mean.annrain.mm * (1 - (1 + 550/mean.annrain.mm)/(1 + 550/mean.annrain.mm + mean.annrain.mm/1100))
  volred.target <- total.runoff - tcarea * Zhang.forest.ro

  #cw addition 1 Jan 2014: accounting for difference in store
  delstore <- pmax(0,budget$store[1,] +
                     budget$use[1,] -
                     budget$store[dim(budget$store)[2],])
  lost.vol <- lost.vol.alone <- round(apply(budget$use, 2, sum)/nyears, 0) - delstore
  #i.e. if tank is emptier at end of run than at start, then don't include the difference
    #as lost (i.e. used) water.

  if (length(upstream.tanks) > 0) {
    for (i in 1:length(upstream.tanks))
      if (dim(upstream.tanks[[i]]$summary)[1] > 1) {
      lost.vol <- lost.vol + upstream.tanks[[i]]$summary$volred.alone[upstream.tanks[[i]]$use.index]
       } else {
      lost.vol <- lost.vol + upstream.tanks[[i]]$summary$volred.alone
    }
  }
  #but if tankbegin is greater than annual demand, then it is possible that lost.vol exceeds
  #annual demand
  lost.vol <- pmin(total.runoff, lost.vol)
  lost.vol.alone <- pmin(total.runoff, lost.vol)

  vr.index <- (1 - (volred.target - lost.vol)/volred.target) * tcarea/100
  yield.kL.y <- round(lost.vol/1000, 0)
  days.runoff <- round(apply(budget$overflow > 0, 2, sum)/nyears, 0)
  days.imp.runoff <- round(sum(dailyrunoff$runoff_mm > 0)/nyears, 0)

  filtvol.hitarget <- Zhang.pasture.ro * tcarea
  filtvol.lowtarget <- Zhang.forest.ro * tcarea
  filtvol <- filtvol.alone <- fv.index <- wq.index <- 0
  grzero <- function(x) sum(x > 0)

  if (assume.leak.wq.perfect == FALSE) {
    wq.index <- 0
  }
  if (soil.area.receiving.leak > 0) {
    annual.eff.rainfall <- apply(budget$leak, 2, sum)/soil.area.receiving.leak + mean.annrain.mm
    Zhang.leak.etprop.old <- (1 + 2820/mean.annrain.mm)/(1 + 2820/mean.annrain.mm + mean.annrain.mm/1410)
    Zhang.leak.etprop <- (1 + 2820/annual.eff.rainfall)/(1 + 2820/annual.eff.rainfall + annual.eff.rainfall/1410)
    lost.vol <- lost.vol + apply(budget$leak, 2, sum) * Zhang.leak.etprop
    lost.vol.alone <- lost.vol.alone + apply(budget$leak, 2, sum) * Zhang.leak.etprop
    vr.index <- (1 - (volred.target - lost.vol)/volred.target) * tcarea/100
    filtvol <- filtvol.alone <- apply(budget$leak, 2, sum) * (1-  Zhang.leak.etprop)
      #23 Jan 2018 - It was the following, which I don't think is right...
      #(annual.eff.rainfall*(1 - Zhang.leak.etprop) -
      #mean.annrain.mm*(1 - Zhang.leak.etprop.old))*soil.area.receiving.leak
    #this is a small correction by cw 29-12-2013: subtract assumed filtered flow from garden before addition of leak
    if (length(upstream.tanks) > 0) {
      for (i in 1:length(upstream.tanks)) if (dim(upstream.tanks[[i]]$summary)[1] > 1) {
        filtvol <- filtvol + upstream.tanks[[i]]$summary$filtered.L.alone[upstream.tanks[[i]]$use.index]
      } else {
        filtvol <- filtvol + upstream.tanks[[i]]$summary$filtered.L.alone
      }
    }
    fv.index <- rep(tcarea/100, length(filtvol))
    fv.index[as.vector(filtvol) < filtvol.lowtarget] <-
      filtvol[as.vector(filtvol) < filtvol.lowtarget] *
                     tcarea/(filtvol.lowtarget *100)
    fv.index[as.vector(filtvol) > filtvol.hitarget] <-
      pmax(0, 1 - (filtvol[as.vector(filtvol) > filtvol.hitarget] -
                     filtvol.hitarget)/filtvol.lowtarget) * tcarea/100
    wq.index <- rep(0, length(filtvol))
    wq.index[filtvol > 0] <- tcarea/100
  }

  ### cases like the petrol station where all leak flow is filtered (or considered that way) and released at an appropriate
  ### rate to the stormwater system
  if (assume.leak.wq.perfect & soil.area.receiving.leak == 0) {
    wq.index <- tcarea/100

    filtvol <- apply(budget$leak, 2, sum)
    if (length(upstream.tanks) > 0) {
      for (i in 1:length(upstream.tanks)) if (dim(upstream.tanks[[i]]$summary)[1] > 1) {
        filtvol <- filtvol + upstream.tanks[[i]]$summary$filtered.L.alone[upstream.tanks[[i]]$use.index]
      } else {
        filtvol <- filtvol + upstream.tanks[[i]]$summary$filtered.L.alone
      }
    }
    fv.index <- rep(tcarea/100, length(filtvol))
    fv.index[as.vector(filtvol) < filtvol.lowtarget] <- filtvol[as.vector(filtvol) < filtvol.lowtarget] * tcarea/(filtvol.lowtarget *
                                                                                                                    100)
    fv.index[as.vector(filtvol) > filtvol.hitarget] <-
                 pmax(0, 1 - (filtvol[as.vector(filtvol) > filtvol.hitarget] -
                              filtvol.hitarget)/filtvol.lowtarget) * tcarea/100
  }

  ff.index <- (1 - (days.runoff - ndaysro.target)/(days.imp.runoff - ndaysro.target)) * tcarea/100

  # tanks rewarded for taking 'too much' water, as the more roof water harvested the better given the difficulty of reducing
  # volume through raingardens....
  if(!reward.overextraction)
    vr.index <- pmin(tcarea/100, vr.index)

  summary <- data.frame(tcarea = as.numeric(tcarea), uses = usage.descs,
                        ro.d = as.numeric(days.runoff),
                        rod.target = as.numeric(ndaysro.target),
                        ff.index = as.numeric(round(ff.index,2)),
                        yield.kL.y = as.numeric(yield.kL.y),
                        volred.L = as.numeric(lost.vol),
                        volred.alone = as.numeric(lost.vol.alone),
                        volred.target = as.numeric(volred.target),
                        vr.index = as.numeric(round(vr.index,2)),
                        min.filtered.vol.target.kL = as.numeric(round(filtvol.lowtarget/1000, 1)),
                         max.filtered.vol.target.kL = as.numeric(round(filtvol.hitarget/1000,1)),
                         filtered.kL.y = as.numeric(round(filtvol/1000, 1)),
                         filtered.L.alone = as.numeric(filtvol.alone),
                         fv.index = as.numeric(round(fv.index, 2)),
                         wq.index = as.numeric(round(wq.index,2)),
                         total.EB = as.numeric(round((vr.index + ff.index + fv.index + wq.index)/4, 2)))
}

##########################################################
# distribute.firstflush FUNCTION
# Functions for packagin output from tankmodel.R for inclusion as inflow to gardenmodel.R
#
# firstflush    a firstflush depth (mm/d)
# hrrunoff      24 hourly runoff depth values (mm/d)

distribute.firstflush <- function(firstflush, hrrunoff) {

  rn <- hrrunoff[which(hrrunoff > 0)[order(which(hrrunoff > 0), decreasing = FALSE)]]
  cumrn <- cumsum(rn)
  ff <- vector(mode = "numeric", length = 24)

  if (firstflush > sum(rn))
    stop(cat("i = ", i, "dover", dover[i], "rn", rn, "firstflush depth exceeds rainfall depth"), call. = FALSE)

  if (firstflush <= cumrn[1]) {
    ff1 <- firstflush
  } else {
    ff1 <- c(rn[cumrn < firstflush], firstflush - cumrn[cumrn < firstflush][sum(cumrn < firstflush)])
  }

  ff[which(hrrunoff > 0)[order(which(hrrunoff > 0), decreasing = FALSE)]][1:length(ff1)] <- ff1
  return(ff)
}


distribute.overflow <- function(dover, #a daily overflow depth (mm/d)
                                hrrunoff){ #a vector of 24 hourly runoff depth values (mm/h)
  #this function distributes daily overflow depth values (as produced by the
  #daily tankmodel.R model) into the last hourly runoff records for that day.
  ## extract hourly records in hrrunoff with >0 runoff and order them
  ## order them from last hour to first hour
  rn <- hrrunoff[which(hrrunoff>0)[order(which(hrrunoff>0),decreasing = TRUE)]]
  ##create a vector of the cumulative sum of rn
  cumrn <- cumsum(rn)
  ##set up matrix for resulting distributions of dover across each value in dover
  over <- rep(0,24)
  if(sum(dover) - sum(rn) > 0.1)
      stop("overflow depth exceeds rainfall depth", call. = FALSE)
  if(dover <= cumrn[1]){
    ##if it is less than the last hour's rain, all of dover can go in the last hour
    over1 <- dover} else {
      ##otherwise dover is distributed across all the hours with a cumulative value < dover
      #this line avoids for rounding errors in determination of dover depth
      if(dover - cumrn[cumrn < dover][sum(cumrn < dover)] < 0.01){
        over1 <- rn[cumrn < dover]}else {
          #and the remainder (dover - last cumulative sum value)  is distributed to the next earliest hour
          over1 <- c(rn[cumrn < dover], dover - cumrn[cumrn < dover][sum(cumrn < dover)])
        }
    }
  over[which(hrrunoff>0)[order(which(hrrunoff>0),
                               decreasing = TRUE)]][1:min(sum(hrrunoff>0),length(over1))] <-
    over1[over1 > 0][1:min(sum(hrrunoff>0),length(over1))]  #
  over
}

compile.inflow <- function(hrunoff, #data.frame of hourly runoff in mm (called runoff_mm),
                           #with cols datetime,date,year,month, (and final col daten) formatted using chron)
                           carea, #catchment area for (direct) hrunoff in sq m
                           doverflow = NULL, #data frame with first field daten (formated with chron)
                           #and 2nd column from daily overflow data from,
                           #tank model $budget$overflow (in L/d)
                           firstflush = NA, #if !is.na, = firstflush in L from tankmodel.R
                           carea.tank = 0){ #catchment area for tank producing
  #overflow in sq m
  if(sum(unique(hrunoff$date) != doverflow$date) > 0 |
       length(hrunoff$date)/24 != length(doverflow$date))
    stop("Dates for the two periods do not match", call. = FALSE)
  if(is.null(doverflow) & (is.na(firstflush) | firstflush == 0)){
    inflow <- data.frame(datetime = hrunoff$datetime,
                         date = hrunoff$date,
                         year = hrunoff$year,
                         month = hrunoff$month,
                         inflow.L = hrunoff$runoff_mm*carea,
                         runoff = hrunoff$runoff_mm*carea,
                         tank.overflow = 0,
                         tank.firstflush = 0)
    runoffandover <- NULL
  } else {
    drunoff <- aggregate(hrunoff$runoff_mm, by = list(daten = hrunoff$daten), sum)
    doverflow.mm <- as.vector(doverflow[,2])/carea.tank
    if(carea > 0){real.hro <- hrunoff$runoff_mm} else {real.hro <- rep(0, length(hrunoff$runoff_mm))}
    runoffandover <- data.frame(daten = hrunoff$daten,
                                runoff.mm = real.hro,
                                over = 0, #rep(0,length(hrunoff$runoff.mm)),
                                ff = 0)
    #runoffandover$runoff.mm[runoffandover$runoff.mm < 1] <- 0
    #names(runoffandover)[3:(dim(doverflow)[2]+2)] <- letters(1:nd)
    if(is.na(firstflush) | firstflush == 0){
      if(sum(doverflow.mm >0) == 0) {
        runoffandover[,3] <- 0
      } else {
        indices <- which(doverflow.mm >0)
        dates <- drunoff$daten[doverflow.mm > 0]
        for(i in 1:length(indices)){
          index <- indices[i]
          runoffandover[runoffandover$daten == dates[i], 3] <-
            distribute.overflow(doverflow.mm[index],
                                hrunoff$runoff_mm[hrunoff$daten == dates[i]])
        }
      }
    }else{
      indices <- which(drunoff[,2] >0)
      dates <- drunoff$daten[drunoff[,2] > 0]
      ff <- as.vector(apply(cbind(drunoff[,2],rep(firstflush/carea.tank,dim(drunoff)[1])), 1, min))
      ff[drunoff[,2] < ff] <- drunoff[drunoff[,2] < ff, 2]
      for(i in 1:length(indices)){
        index <- indices[i]
        if(doverflow[index,2] == 0){
          #            c(firstflush, rep(0,23))
          runoffandover[runoffandover$daten == dates[i],3] <- 0
          runoffandover[runoffandover$daten == dates[i],4] <-
            distribute.firstflush(ff[index],
                                  hrunoff$runoff_mm[hrunoff$daten == dates[i]])
        }else{
          runoffandover[runoffandover$daten == dates[i],4] <-
            distribute.firstflush(ff[index],
                                  hrunoff$runoff_mm[hrunoff$daten == dates[i]])
          runoffandover[runoffandover$daten == dates[i],3] <-
            distribute.overflow(as.numeric(doverflow.mm[index]),
                                hrunoff$runoff_mm[hrunoff$daten == dates[i]]) #+
          #                                  runoffandover[runoffandover$daten == dates[i],4]
        }
      }
    }
    inflow <- data.frame(datetime = hrunoff$datetime,
                         date = hrunoff$date,
                         year = hrunoff$year,
                         month = hrunoff$month,
                         inflow.L = runoffandover[,3]*carea.tank +
                           runoffandover[,4]*carea.tank + real.hro*carea,
                         runoff = real.hro*carea,
                         tank.overflow = runoffandover[,3]*carea.tank,
                         tank.firstflush = runoffandover[,4]*carea.tank)
  }
  list(inflow = inflow, runoffandover = runoffandover)
}

##########################################################
# distribute.overflow function
# This function distributes daily overflow depth values (as produced by the daily tankmodel.R model) into the last hourly
# runoff records for that day
# dover       a daily overflow depth (mm/d)
# hrrunoff    a vector of 24 hourly runoff depth values (mm/h)

distribute.overflow.old <- function(dover, hrrunoff) {

  # Extract hourly records in hrrunoff with >0 runoff and order them order them from last hour to first hour
  rn <- hrrunoff[which(hrrunoff > 0)[order(which(hrrunoff > 0), decreasing = TRUE)]]

  # Create a vector of the cumulative sum of rn
  cumrn <- cumsum(rn)

  # Set up matrix for resulting distributions of dover across each value in dover
  over <- rep(0, 24)
  if (round(sum(rn), 2) < round(sum(dover), 2))
    stop("overflow depth exceeds rainfall depth", call. = FALSE)
  if (dover <= cumrn[1]) {
    # If it is less than the last hour's rain, all of dover can go in the last hour
    over1 <- dover
  } else {
    # otherwise dover is distributed across all the hours with a cumulative value < dover this line avoids for rounding errors
    # in determination of dover depth
    if (dover - cumrn[cumrn < dover][sum(cumrn < dover)] < 0.01) {
      over1 <- rn[cumrn < dover]
    } else {
      # and the remainder (dover - last cumulative sum value) is distributed to the next earliest hour
      over1 <- c(rn[cumrn < dover], dover - cumrn[cumrn < dover][sum(cumrn < dover)])
    }
  }

  over[which(hrrunoff > 0)[order(which(hrrunoff > 0), decreasing = TRUE)]][1:length(over1)] <- over1[over1 > 0]

  return(over)
}

#===============================
# compile.inflow FUNCTION
# hrunoff     data.frame of hourly runoff in mm (called runoff_mm) with cols datetime,date,year,month, (and final col daten formatted using chron)
# carea       catchment area for (direct) hrunoff in sq m
# doverflow   data frame with first field daten (formated with chron) and 2nd column from daily overflow data from, tank model $budget$overflow (in L/d)
# firstflush  if !is.na, = firstflush in L from tankmodel.R
# carea.tank  catchment area for tank producing overflow in sq m

compile.inflow.old <- function(hrunoff, carea, doverflow = NULL, firstflush = NA, carea.tank = 0) {

  #if (sum(unique(hrunoff$date) != as.numeric(doverflow$date)) > 0)
  #stop("Dates for the two periods do not match", call. = FALSE)

  if (is.null(doverflow) & (is.na(firstflush) | firstflush == 0)) {
    # No tank overflow to distribute just generate the data for the rain garden
    inflow <- data.frame(datetime = hrunoff$datetime, date = hrunoff$date,
                         year = as.numeric(hrunoff$year),
                         month = as.numeric(hrunoff$month),
                         inflow.L = as.numeric(hrunoff$runoff_mm * carea),
                         runoff = as.numeric(hrunoff$runoff_mm * carea),
                         tank.overflow = 0, tank.firstflush = 0)
    runoffandover <- NULL
  } else {
    drunoff <- aggregate(hrunoff$runoff_mm, by = list(daten = hrunoff$daten), sum)

    doverflow[,2] <- doverflow[,2]/carea.tank

    # Calculate amount of runoff from catchment
    if (carea > 0) {
      hro <- hrunoff$runoff_mm
    } else {
      hro <- rep(0, nrow(hrunoff))
    }

    runoffandover <- data.frame(daten = as.numeric(hrunoff$daten),
                                runoff.mm = as.numeric(hro), over = 0, ff = 0)

    if (is.na(firstflush) | firstflush == 0) {
      if (sum(doverflow > 0) == 0) {
        runoffandover[, 3] <- 0
      } else {
        indices <- which(doverflow > 0)
        dates <- drunoff$daten[doverflow > 0]
        for (i in 1:length(indices)) {
          index <- indices[i]
          runoffandover[runoffandover$daten == dates[i], 3] <- distribute.overflow(doverflow[index], hrunoff$runoff_mm[hrunoff$daten ==
                                                                                                                         dates[i]])
        }
      }
    } else {
      indices <- which(drunoff[, 2] > 0)
      dates <- drunoff$daten[drunoff[, 2] > 0]
      ff <- as.vector(apply(cbind(drunoff[, 2], rep(firstflush/carea.tank, dim(drunoff)[1])), 1, min))
      ff[drunoff[, 2] < ff] <- drunoff[drunoff[, 2] < ff, 2]
      for (i in 1:length(indices)) {
        index <- indices[i]
        if (doverflow[index] == 0) {
          runoffandover[runoffandover$daten == dates[i], 3] <- 0
          runoffandover[runoffandover$daten == dates[i], 4] <- distribute.firstflush(ff[index], hrunoff$runoff_mm[hrunoff$daten ==
                                                                                                                    dates[i]])
        } else {
          runoffandover[runoffandover$daten == dates[i], 4] <- distribute.firstflush(ff[index], hrunoff$runoff_mm[hrunoff$daten ==
                                                                                                                    dates[i]])
          runoffandover[runoffandover$daten == dates[i], 3] <- distribute.overflow(as.numeric(doverflow[index]), hrunoff$runoff_mm[hrunoff$daten ==
                                                                                                                                     dates[i]])
        }
      }
    }

    inflow <- data.frame(inflow = as.numeric(runoffandover[, 3] *
                                  carea.tank + runoffandover[, 4] * carea.tank + hro * carea),
                         runoff = as.numeric(hro * carea),
                         tank.overflow = as.numeric(runoffandover[, 3] * carea.tank),
                         tank.firstflush = as.numeric(runoffandover[, 4] * carea.tank))
  }
  return(inflow)
}


##########################################################
# filter.profile FUNCTION
# Creates the filter profile for use in the raingarden model
#
# d.pond.m          depth of ponding zone above filter (in m) = Hp: zero if absent
# d.sandyloam.m     depth (in m) of sandy loam layer. Enter zero if layer is missing. Sandy loam must be at surface
# d.sand.m          depth (in m) of sand layer. Enter zero if layer is missing
# d.scoria.m        depth (in m) of scoria layer. Enter zero if layer is missing. Scoria must be deepest. Total of all three depths = depth of filter Hf
# soil.strata.m     vector of depths (in m from pond surface) at which surrounding soils change in their infiltration capacity. Final value = bottom of filter
# strata.Ksu.mm.h   vector (same length as soil.strata.m) of infiltration rates for each stratum (in mm.h) (if lined system single value = 0) #function returns a
#                   data.frame with soil profile (in dm) suitable for the Ksu.profile argument of gardenmodel.R
filter.profile <- function(d.pond.m, d.sandyloam.m, d.sand.m, d.scoria.m, soil.strata.m, strata.Ksu.mm.h) {

  if (length(soil.strata.m) != length(strata.Ksu.mm.h))
    stop("soil.strata.m and strata.Ksu.mm.h vectors must be the same length", call. = FALSE)

  if (sum(c(d.pond.m, d.sandyloam.m, d.sand.m, d.scoria.m)) != soil.strata.m[length(soil.strata.m)])
    stop("total depth of the three media + pond must equal total depth of soil strata", call. = FALSE)

  if (sum(diff(soil.strata.m <= 0) > 0))
    stop("soil.strata.m should be a vector of increasing depths", call. = FALSE)

  filter.layers <- c(d.pond.m, d.sandyloam.m, d.sand.m, d.scoria.m)
  porosities <- c(1, 0.4, 0.4, 0.6)[filter.layers > 0]
  n.media <- length(porosities)
  filter.layers <- unique(cumsum(filter.layers[filter.layers > 0]))

  d <- unique(c(filter.layers, soil.strata.m))
  d <- d[order(d)]
  x <- data.frame(d.dm = as.numeric(d * 10), porosity = NA, Ksu.dm.h = NA)

  for (i in 1:length(d)) {
    x$porosity[i] <- porosities[which(filter.layers >= d[i])[1]]
    x$Ksu.dm.h[i] <- strata.Ksu.mm.h[which(soil.strata.m >= d[i])[1]]/100
  }

  # Assume pond is lined
  x$Ksu.dm.h[x$porosity == 1] <- 0
  list(profile = x, top.medium = c("loamy sand", "sand", "gravel (scoria)")[which(c(d.sandyloam.m, d.sand.m, d.scoria.m) > 0)[1]])
}

#### calculate filter profile filtr.prfile = '/var/www/Rpad/filtprof.RData',
#### Hp, #as in gardenmodel (m)
#### Hf, #as ingardenmodel (m)
#### Af, #as in gardenmodel (sq m)
#### topsoil.depth, #as in gardenmodel (m) medium, #as in gardenmodel
#### lined.side (logical: are the sides of the raingarden fully-lined) ){

set.filtprof <- function(filtprofTab = filtprofs, filtr.prfile = "filtprof.MtEv.default.RData", Hp, Hf, Af,Ap,
                         topsoil.depth, medium, lined.side, underdrain = FALSE) {

  filtprof <- filtprofTab[filtprofTab$filtprofFile == filtr.prfile,]
  filtprof$d.dm = filtprof$depth_m*10
  filtprof$Ksu.dm.h <- filtprof$Ks_mm.h/100
  filtprof <- filtprof[c("d.dm","porosity","Ksu.dm.h")]
  #### assume that shallow systems with underdrains are all gravel
  if (Hf <= 0.3 & underdrain) {
    filtprof$porosity[filtprof$porosity < 1] <- ifelse(medium == "gravel (scoria)", 0.6, 0.4)
  } else {
    # prepare profile for adjustment of media if none of the depths in filtr.prfile are exactly half of Hf...
    if (sum(filtprof$d.dm == 5 * Hf + topsoil.depth * 5) == 0) {
      ### add a depth that is exactly half of Hf
      filtprof <- rbind(data.frame(d.dm = as.numeric(5 * Hf + topsoil.depth * 5),
                                   porosity = NA,
                                   Ksu.dm.h = NA), filtprof)
      filtprof <- filtprof[order(filtprof$d.dm), ]
      ### and give it the porosity of the medium at the next depth down
      filtprof$porosity[filtprof$d.dm == 5 * Hf + topsoil.depth * 5] <- filtprof$porosity[which(filtprof$d.dm == 5 * Hf +
                                                                                                  topsoil.depth * 5) + 1]
      #### and the exfiltration rate of the medium at the next depth down
      filtprof$Ksu.dm.h[filtprof$d.dm == 5 * Hf + topsoil.depth * 5] <- filtprof$Ksu.dm.h[which(filtprof$d.dm == 5 * Hf +
                                                                                                  topsoil.depth * 5) + 1]
    }
    #### bottom 1/2 of deeper systems will be scoria
    if (medium == "gravel (scoria)") {
      ### if nominated medium is scoria, then set all depths to 0.6
      filtprof$porosity[filtprof$porosity < 1] <- 0.6
    } else {
      ### if not, then depths from half way down to the bottom set to 0.6 (scoria)
      filtprof$porosity[(1 + which(filtprof$d.dm == 5 * Hf + topsoil.depth * 5)):dim(filtprof)[1]] <- 0.6
    }
  }
  if (topsoil.depth > 0) {
    if (Hp == 0) {
      filtprof$d.dm <- filtprof$d.dm - topsoil.depth * 10
      filtprof <- filtprof[filtprof$d.dm > 0, ]
    }
    if (topsoil.depth >= 0 & Hp > 0 & Hp < topsoil.depth) {
      stop("If the top of the filter is below the soil surface, then Hp must be either zero\n (i.e. the system is a sub-surface infiltration system), \n or the pond must be at least as deep as the distance between \nthe filter surface and the soil surface (i.e. Hp >= topsoil.depth)",
           .call = FALSE)
    }
    if (Hp > 0 & Hp == topsoil.depth) {
      filtprof <- rbind(data.frame(d.dm = as.numeric(topsoil.depth * 10), porosity = 1, Ksu.dm.h = NA), filtprof)

      if (filtprof$d.dm[1] < filtprof$d.dm[2]) {
        filtprof$Ksu.dm.h[1] <- filtprof$Ksu.dm.h[2]
      } else {
        filtprof <- filtprof[order(filtprof$d.dm), ]
        filtprof$Ksu.dm.h[filtprof$porosity == 1] <- filtprof$Ksu.dm.h[which(filtprof$porosity == 1) + 1]
      }
    }
    if (Hp > 0 & Hp > topsoil.depth) {
      filtprof <- rbind(data.frame(d.dm = c(as.numeric((topsoil.depth - Hp)) * 10, 0), porosity = c(1, 1),
                                   Ksu.dm.h = c(0, NA)), filtprof)

      filtprof$d.dm <- filtprof$d.dm + Hp * 10
      if (filtprof$d.dm[2] < filtprof$d.dm[3]) {
        filtprof$Ksu.dm.h[2] <- filtprof$Ksu.dm.h[3]
      } else {
        filtprof <- filtprof[order(filtprof$d.dm), ]
        filtprof$Ksu.dm.h[is.na(filtprof$Ksu.dm.h)] <- filtprof$Ksu.dm.h[which(is.na(filtprof$Ksu.dm.h)) + 1]
      }
    }
  } else {
    if (topsoil.depth < 0) {
      filtprof <- rbind(data.frame(d.dm = 0, porosity = as.numeric(filtprof$porosity[1]), Ksu.dm.h = 0), filtprof)

      filtprof$d.dm <- filtprof$d.dm - 1 * topsoil.depth * 10
    }
    if (Hp > 0) {
      filtprof <- rbind(data.frame(d.dm = 0, porosity = 1, Ksu.dm.h = 0), filtprof)

      filtprof$d.dm <- filtprof$d.dm + Hp * 10
    }
  }
  if (Hp > 0 & filtprof$d.dm[1] > filtprof$d.dm[2]) {
    # if ponding depth extends below first change in Ksu
    filtprof <- filtprof[order(filtprof$d.dm), ]
    filtprof$Ksu.dm.h[filtprof$porosity == 1] <- filtprof$Ksu.dm.h[which(filtprof$porosity == 1) - 1]
    filtprof$porosity[1:which(filtprof$porosity == 1)] <- 1
  }
  if (lined.side) {
    filtprof$Ksu.dm.h <- 0
  }

  filtprof <- filtprof[1:ifelse(sum(filtprof$d.dm > (Hf + Hp) * 10) > 0, which(filtprof$d.dm > (Hf + Hp) * 10)[1], length(filtprof$d.dm)),
                       ]

  filtprof$d.dm[dim(filtprof)[1]] <- (Hf + Hp) * 10

  # remove redundant rows of profile
  change.points <- c(which(abs(diff(filtprof$Ksu.dm.h)) > 0), which(diff(filtprof$porosity) != 0))
  change.points <- unique(c(change.points[order(change.points)], dim(filtprof)[1]))
  filtprof <- filtprof[change.points, ]
  filtprof$ht.dm <- c(filtprof$d.dm[1], diff(filtprof$d.dm))
  filtprof$vol <- filtprof$ht.dm * filtprof$porosity * Af * 100
  # Increase volume of pond if area of pond is bigger than filter area
  if (Ap > Af)
    filtprof$vol[filtprof$porosity == 1] <- filtprof$vol[filtprof$porosity == 1] * Ap / Af
  filtprof
}

###########################################
# RAIN GARDEN MODEL
# inflow    vector of hourly inflows in L (no date or time fields)
# et        vector of hourly evapotranspiration values in mm/h (sets to zero if isveg is FALSE)
# Af        Filter area (sq m)
# Pf        Filter perimeter (m)
# Hf        Filter depth (m)
# Ap        Ponding area (sq m)
# Hp        Ponding depth (m)
# isveg     Is system vegitated
# Ho        Distance from base to invert of underdrain outlet or orifice of of standpipe outlet (m)
#           If no underdrain or standpipe, set Ho to Hf + Hp
# Vstart    Vol of water in system at t1 in L
# carea     As in compile.inflow()
# Ksu.mm.h  Underlying infiltration rate: set to zero if lined bottom
# outlet.rate.L.h = 0.3, #L/h should be equal to pre-urban subsurface runoff following event equivalent to filtro.target
                       #was 0.068 L/h/m2 (so that value * carea) -rounded to 0.1.  This value determines the rate at which the outlet pipe,
                       #if present, drains. If Ho <= 0.05 (i.e. the pipe is at the bottom of the system)
                       #the outlet pipe is assumed to be unchoked. If outlet.rate.L.h > Ksf of medium*carea, the outlet.rate is set to Ksf*carea.
                       #Thus this argument only applies if Hf + Hp > Ho > 0.05.
#filtr.prfile #A dataframe compiled using the set.filtprof function
# adj.tree.canopy.area  Canopy area (sq m) of trees with canopy edges < 3m from an at least partly unlined raingarden
# medium    filter medium, must be sandy loam, sand, or gravel (scoria) decides Ksf = 0.5,2.5, 36(dm/h)
#           respectively (assumes halving of K for loamy sand and sand over time) but for tim's scenario, Ksf of loamy sand = 1.5
#           must correspond to top layer in filter.profile - to be safe use filter.profile$top.medium

gardenmodel <- function(inflow, et, Af, Pf, Hf, Ap, Hp, isveg = TRUE, Ho, Vstart, carea, Ksu.mm.h = 0.05, outlet.rate.L.h = 0.3, filtr.prfile, adj.tree.canopy.area = 0, medium = "loamy sand") {

  # Input validation
  if (is.na(match(medium, c("loamy sand", "sand", "gravel (scoria)"))))
    stop("Medium must be one of 'loamy sand', 'sand' or 'gravel (scoria)'", .call = FALSE)

  if (isveg & medium == "gravel (scoria)")
    stop("The calculator does not allow vegetated gravel systems, as they are prone to produce nitrate", .call = FALSE)

  Ksu.dm.h <- Ksu.mm.h / 100

  if (Ksu.dm.h == 0 & filtr.prfile$Ksu.dm.h[nrow(filtr.prfile)] > 0)
    stop(paste("Value of Ksu.mm.h = zero, in which case so should the deepest Ksu in filter.profile (currently ", filtr.prfile$Ksu.dm.h[dim(filtr.prfile)[1]] *
                 100, ")", sep = ""), call. = FALSE)

  # If system has a tub in the bottom but unsealed sides, deepest layer in filter.profile should have Ksu = 0
  lined.bottomandsides <- ifelse(sum(filtr.prfile$Ksu.dm.h) == 0 & Ksu.dm.h == 0, TRUE, FALSE)
  lined.side <- ifelse(sum(filtr.prfile$Ksu.dm.h) == 0, TRUE, FALSE)

  # Conversions to dm
  Ho <- Ho * 10
  Hf <- Hf * 10
  Hp <- Hp * 10
  Pf <- Pf * 10

  # Convert to square dm
  Afd <- Af * 100

  Ho.true <- Ho

  #####CJW needs to seek advice on the logic of this step before uncommenting it.
  # Adjust outlet height if media is gravel
  #if (Ho < Hf & medium != "gravel (scoria)")
  #  Ho <- Ho + (Hf - Ho) * 0.5

  # Hourly evapotranspiration
  if (isveg) {
    if (!lined.bottomandsides)
      et <- et * (Af + 0.5 * adj.tree.canopy.area)
    else
      et <- et * Af
  } else {
    if (!lined.bottomandsides)
      et <- et * (0.5 * adj.tree.canopy.area)
    else
      et <- rep(0, length(inflow))
  }

  # Pollutant parameters
  if (isveg) {
    N1 <- c(0.59, 0.72, NA)[match(medium, c("loamy sand", "sand", "gravel (scoria)"))]
    P1 <- c(0.74, 0.74, NA)[match(medium, c("loamy sand", "sand", "gravel (scoria)"))]
    TSS1 <- c(0.99, 0.99, NA)[match(medium, c("loamy sand", "sand", "gravel (scoria)"))]
  } else {
    N1 <- c(-1.01, 0.32, 0.39)[match(medium, c("loamy sand", "sand", "gravel (scoria)"))]
    P1 <- c(0.86, 0.86, 0.2)[match(medium, c("loamy sand", "sand", "gravel (scoria)"))]
    ### made up 0.2 waiting on TDF advice
    TSS1 <- c(0.99, 0.99, 0.2)[match(medium, c("loamy sand", "sand", "gravel (scoria)"))]
    ### made up 0.2 waiting on TDF advice
  }

  Ksf <- c(1.5, 2.5, 36)[match(medium, c("loamy sand", "sand", "gravel (scoria)"))]

  # Number of layers
  nlayers <- nrow(filtr.prfile)

  # L/h - if outlet pipe is at bottom (<= 0.5 dm from bottom), assume system is unchoked; else assume it is choked to filtro.target
  outlet.rate <- ifelse(Ho > 0.5, outlet.rate.L.h, Afd * Ksf)
  #if filtro.target > Ksf of medium, the system is the limiting factor, so set outlet rate to Ksf
  if(outlet.rate > Afd*Ksf) outlet.rate <- Afd*Ksf

  # Height of filter profile in decimeters
  filtr.prfile$ht.dm <- c(filtr.prfile$d.dm[1], diff(filtr.prfile$d.dm))

  # Precalculate the volume (L) per decimetre (dm) depth. Used to calculate depth from volume
  filtr.prfile$voidarea <- Afd * filtr.prfile$porosity

  # Volume of filter layers
  filtr.prfile$vol <- filtr.prfile$ht.dm * filtr.prfile$voidarea

  # Cumulative volume of filter layers from base to top
  filtr.prfile$cumvol <- rev(cumsum(rev(filtr.prfile$vol)))

  # Increase volume of pond if area of pond is bigger than filter area
  if (Ap > Af)
    filtr.prfile$vol[filtr.prfile$porosity == 1] <- filtr.prfile$vol[filtr.prfile$porosity == 1] * Ap / Af

  # Volume of ponding
  pondV <- sum(filtr.prfile$vol[filtr.prfile$porosity == 1])

  # Filter volume
  filterV <- sum(filtr.prfile$vol) - pondV

  # Setup filter layer parameters (height, depth to outlet etc...)
  if (Ho >= Hf + Hp) {
    # Ho adjusted as fix for field capacity
    filtr.prfile$ho.dm <- filtr.prfile$ht.dm
    # true Ho
    filtr.prfile$ho.true.dm <- filtr.prfile$ht.dm
  } else {
    # Calculate the height to the layer, is equal to layer depth is outlet not in layer
    cumdepths <- rev(cumsum(rev(filtr.prfile$ht.dm)))
    filtr.prfile$ho.dm <- pmax(pmin(Ho - cumdepths, 0) + filtr.prfile$ht.dm, 0)
    filtr.prfile$ho.true.dm <- pmax(pmin(Ho.true - cumdepths, 0) + filtr.prfile$ht.dm, 0)
  }

  # Sub outlet water volume
  suboV.true <- sum(filtr.prfile$voidarea * filtr.prfile$ho.true.dm)

  # Initial volume of filter below outlet pipe in L volume of water in each section in Litres
  Vp0 <- max(Vstart - filterV, 0)
  Vf0 <- min(filterV, Vstart)

  # Inflow matrix, remove the first four columns (these are date/time vars)
  len <- length(inflow)

  # N removal estimates (and adwp.trigger and dry.penalty are adjustments for adwp) assumes hourly timestep
  adwp <- 0
  adwp.trigger <- ifelse(isveg, 4 * 24, 5 * 24)
  dry.penalty <- ifelse(isveg, 0.03, 0.02)

  # Setup output
  budget <- list(flowin = inflow)

  # Main loop
  for (i in 1:len) {
    if (i == 1) {
      Vfprev <- Vf0
      Vpprev <- Vp0
    } else {
      Vfprev <- budget$store.filter[i - 1]
      Vpprev <- budget$store.pond[i - 1]
    }

    # If the previous filter volume was 0 then skip all height calcs as they are = 0
    if(Vfprev == 0) {
      filterLayerVols <- 0
      filterLayerDeps <- 0
      Hwpprev <- 0
      Hwfprev <- 0
    } else {
      if (nlayers == 1) {
        # Single layer of media, volume of the filter is the volume in the single layer
        filterLayerVols <- Vfprev
        filterLayerDeps <- Vfprev / filtr.prfile$voidarea
        Hwfprev <- filterLayerDeps
        Hwpprev <- 0
      } else {
        # Determine the volume of water in each layer
        filterLayerVols <- pmax(pmin(Vfprev - filtr.prfile$cumvol, 0) + filtr.prfile$vol, 0)

        # Calculate the height of water (dm) in each layer from the volume and void area
        filterLayerDeps <- filterLayerVols / filtr.prfile$voidarea

        if (Hp > 0) {
          Hwfprev <- sum(filterLayerDeps[-1])
          Hwpprev <- filterLayerDeps[1] <- ifelse(pondV == 0, 0, Hp * Vpprev^(2/3) / pondV^(2/3))  #dm
          filterLayerVols[1] <- Vpprev
        } else {
          Hwfprev <- sum(filterLayerDeps)
          Hwpprev <- 0
        }
      }
    }

    # Calculate exfiltration into soil
    # No exfiltration if no water in filter or fully lined system
    if(Vfprev == 0 | lined.bottomandsides) {
      exfil <- 0
    } else {
      # Exfiltration from the base
      exfil <- Afd * Ksu.dm.h

      # If side is not lined then add exfiltration from sides
      if(!lined.side) {
        exfil <- exfil + sum(pmin(filtr.prfile$Ksu.dm.h * filterLayerDeps * Pf, filterLayerVols))
      }

      exfil <- min(Vfprev + Vpprev, exfil)
    }

    # Calculate evaporation
    evap <- min(et[i], Vfprev + Vpprev - exfil)

    # Volume that will either soak into filter or overflow
    pond2filterorover <- Vpprev + inflow[i]

    # Volume that filter can accept if enough space (given infiltration rate)
    filtertakerate <- Afd * Ksf * max(Hf, Hwfprev + Hwpprev)/Hf

    # Maximum volume that could flow into filter if enough space
    max2filter <- min(pond2filterorover, filtertakerate)
    flow2filter <- min(max2filter, filterV - Vfprev + exfil + evap)

    # volume that filter could accept allowing for all losses except outlet pipe losses total filter volume - volume already in
    # fliter + exfiltration + et losses + flow out of outlet pipe
    if (Ho < Hf & (Vfprev + flow2filter) > suboV.true) {
      # If outflow is enough to raise water above Ho
      outflow <- min(outlet.rate, Vfprev + flow2filter - suboV.true)
      flow2filter <- min(max2filter, flow2filter + outflow)
    } else {
      outflow <- 0
    }

    # Calculate overflow, then remove negative values
    overflow <- pond2filterorover - flow2filter - pondV #+ Vpprev ### error discovered 7 Jul 2018???? (isn't Vpprev already in pond2ifilterorover??)
    overflow <- (overflow + abs(overflow)) / 2

    # Calculate pond store, then remove negative values
    store.pond <- Vpprev + inflow[i] - flow2filter - overflow
    store.pond <- (store.pond + abs(store.pond)) / 2

    # Calculate filter store, then remove negative values
    store.filter <- Vfprev + flow2filter - outflow - exfil - evap
    store.filter <- (store.filter + abs(store.filter)) / 2

    # If no flow then increment adwp counter & if there is flow, reset adwp
    adwp <- ifelse(inflow[i] > 0, 0, adwp + 1)
    #Rhys had it as this, which just monotonically increased N concententrions: adwp + (inflow[i] == 0)

    # TSS, TN, TP outflow concentration calculations
    if (medium == "loamy sand") {
      Nred <- ifelse(adwp <= adwp.trigger, N1, N1 - dry.penalty * ceiling((adwp - adwp.trigger)/24))
    } else {
      Nred <- N1
    }

    if (outflow == 0 & exfil == 0) {
      Nout <- 0
      Pout <- 0
      Tout <- 0
    } else {
      # Total water leaving
      QoutT <- outflow + exfil

      # assume exfil and outflow have the same N concentration (given mobility of NOx)
      Nout <- 2.2 * (1 - Nred)

      # weighted mean of conc in outflow and exfil: assumes v good P retention in exfiltration
      Pout <- (outflow * 0.35 * (1 - P1) + exfil * 0.001) / QoutT

      # weighted mean of conc in outflow and exfil: assumes complete TSS retention in exfiltration
      Tout <- (outflow * 150 * (1 - TSS1)) / QoutT
    }

    # Assign values calculated in this loop to output arrays
    budget$Qexf[i] <- exfil
    budget$et[i] <- evap
    budget$out[i] <- outflow
    budget$over[i] <- overflow
    budget$store.pond[i] <- store.pond
    budget$store.filter[i] <- store.filter
    budget$height.dm[i] <- Hwfprev
    budget$flow2filter[i] <- flow2filter
    budget$filtertakerate[i] <- filtertakerate
    budget$max2filter[i] <- max2filter
    budget$Nconcout[i] <- Nout
    budget$Pconcout[i] <- Pout
    budget$TSSout[i] <- Tout

  }  ## End of main loop

  ## Function output
  list(budget = budget, filtprof = filtr.prfile)
}

##########################################################
# Function to calculate EB stats from garden model output
# budget:          output from gardenmodel.R
# interval:        interval between data points in budget (hrs)
# carea:           impervious catchment area
# gardenarea:      catchment area of raingarden and raingarden area (sq m)
# mean.annrain.mm: long term mean annual rainfall (mm)
# ndaysro.target:  target (reference) number of permissible overflow
# ndaysro.urban:   number of days of impervious runoff
#
# reference water quality and flow rate for outflow or overflow from SCMs.  
# SEPP targets use 75th percentiles (for lowland Yarra: 1.1 mg/L TN, 0.055 mg/L TP, no target for TSS)
# MUSIC models used to estimate outflow concentrations are medians. We therefore primarily used 
# median concentrations of our reference streams to determine targets for SCM performance
# source("https://tools.thewerg.unimelb.edu.au/documents/mwstr/mwstr_functions.R")
#
# Pconc.target = 0.05, percentile = 50
# median(sqlQuery("SELECT conc FROM wqData WHERE wqVar = 'TP' AND substr(samplecode,4,3) IN ('LYR','OLN','SAS')", "lsc")$conc)
# # median TP in reference streams = 0.03, but the minimum median concentration achievable by biofilters 
# # (as modelled by MUSIC) = 0.05: this was therefore set as the TP target
#
# Nconc.target = 0.6, percentile = 50
# median(sqlQuery("SELECT conc FROM wqData WHERE wqVar = 'TN' AND substr(samplecode,4,3) IN ('LYR')", "lsc")$conc)
# # median TN in LYR (reference stream with only 1 septic tank in its catchment, whereas OLN and SAS have many) = 0.81
# # TN target was set at 0.6: arguably lower than appropriate
#
# TSS.target = 20, percentile = 50
# median(sqlQuery("SELECT conc FROM wqData WHERE wqVar = 'TSS' AND substr(samplecode,4,3) IN ('LYR','OLN','SAS')", "lsc")$conc)
# # median TSS in reference streams = 22, TSS target set at 20
#
# filtro.target = 0.1:  maximum flow rate for filtered flows, L/h per m2 of impervious catchment
# area, should be equal to pre-urban subsurface runoff following event equivalent to ndaysro.target
# filtro.target was 0.068 L/h/m2 in first draft; changed to 0.1 L/h/m2 (16 May 08)
# We assume that an acceptable outflow from a bioretention system equals
# the infiltration rate of the surrounding soil mulitplied by the system's catchment area
# As a comforting reality check, this is close to equivalent to the
# depth of flow observed in a reference creek (Olinda Creek at Mt Evelyn)
# at the peak of an event that is sufficient to commence overland flow: #about 6 m3/s for a 31.8 km2
# default changed to 0.3 31 Jan 2014 to match achievable leak, and more like winter baseflow rate
# catchment = 0.068 mm/h reward.overextraction = TRUE

garden.EBstats <- function(budget, interval = 1, carea, gardenarea, mean.annrain.mm = 956,
                           ndaysro.target = 12, ndaysro.urban = 121,
                           Nconc.target = 0.6, Pconc.target = 0.05, TSS.target = 20,
                           percentile = 50, filtro.target = 0.3, reward.overextraction = TRUE) {
  budget$overflow <- budget$over
  nyears <- round(interval * length(budget$flowin)/(24 * 365.25), 0)
  total.runoff <- sum(budget$flowin)/nyears
  rows.per.day <- 24/interval
  day <- rep(seq(1, ceiling(length(budget$flowin))/rows.per.day), each = rows.per.day)

  if (length(day) != length(budget$flowin)) {
    day <- day[1:length(budget$flowin)]
  }

  ro <- budget$out > filtro.target * (carea + gardenarea) | budget$over > 0
  ro.by.day <- aggregate(ro, by = list(day = day), FUN = sum)$x
  ndaysro.treatment <- round(sum(ro.by.day > 0)/nyears, 0)
  ff.index <- (1 - max(0, (ndaysro.treatment - ndaysro.target)/(ndaysro.urban - ndaysro.target))) * carea/100

  filtro.target <- (carea + gardenarea) * filtro.target/interval
  Zhang.forest.ro <- mean.annrain.mm * (1 - (1 + 2820/mean.annrain.mm)/(1 + 2820/mean.annrain.mm + mean.annrain.mm/1410))
  Zhang.pasture.ro <- mean.annrain.mm * (1 - (1 + 550/mean.annrain.mm)/(1 + 550/mean.annrain.mm + mean.annrain.mm/1100))
  volred.target <- total.runoff - carea * Zhang.forest.ro
  # impervious runoff minus runoff from mature forest according to Zhang curve.
  filtvol.hitarget <- Zhang.pasture.ro * (carea + gardenarea)
  filtvol.lowtarget <- Zhang.forest.ro * (carea + gardenarea)
  filtvol <- (sum(budget$Qexf) + sum(budget$out[budget$out <= filtro.target]))/nyears
  # #2 July 2018 - this decision to not penalize excess exfiltration rescinded, 
  # #as there doesn't seem to be a lot of justification for it.
  # # filtvol includes Qexf up to the target, but only out is include above target
  # # if filtvol including Qexf and out (at acceptable rate) exceeds target...
  # if (filtvol > filtvol.hitarget) {
  #   # if filtvol excluding Qexf is less than target, then include Qexf & out up to the target
  #   if (sum(budget$out[budget$out <= filtro.target])/nyears < filtvol.hitarget) {
  #     filtvol <- filtvol.hitarget
  #   } else {
  #     # otherwise just include flows out of pipe in filtvol
  #     filtvol <- sum(budget$out[budget$out <= filtro.target])/nyears
  #   }
  # }
  if (filtvol < filtvol.lowtarget) {
    fro.index <- filtvol * carea/(filtvol.lowtarget * 100)
  } else {
    if (filtvol > filtvol.hitarget) {
      fro.index <- max(0, 1 - (filtvol - filtvol.hitarget)/filtvol.lowtarget) * carea/100
    } else {
      fro.index <- carea/100
    }
  }
  lost.vol <- sum(budget$et)/nyears
    #cw addition 1 Jan 2014: accounting for difference in store
  delstore <- pmax(0,budget$store.filter[1] + budget$store.pond[1] +
                     budget$et[1] -
                     budget$store.filter[length(budget$store.filter)] -
                     budget$store.pond[length(budget$store.pond)])
  lost.vol <- round(sum(budget$et)/nyears, 0) - delstore
  #i.e. if system is emptier at end of run than at start, then don't include the difference
    #as lost (i.e. used) water.

  vr.index <- (1 - (volred.target - lost.vol)/volred.target) * carea/100
  if (vr.index > carea/100 + (!reward.overextraction)) {
    vr.index <- (1 - (lost.vol - volred.target)/(volred.target)) * carea/100
  }
  # normally penalty for overextraction as we are certainly not going to be able reduce total volumes to the pre-urban volume
  # from paved surfaces.  However, the reward.overextraction permits this reward to be turned off.

  Nconc.pcntile <- ifelse(quantile(budget$over, probs = percentile/100) > 0, 2.2, quantile(budget$Nconcout, probs = percentile/100,
                                                                                           na.rm = TRUE))
  Pconc.pcntile <- ifelse(quantile(budget$over, probs = percentile/100) > 0, 0.35, quantile(budget$Pconcout, probs = percentile/100,
                                                                                            na.rm = TRUE))
  TSS.pcntile <- ifelse(quantile(budget$over, probs = percentile/100) > 0, 150, quantile(budget$TSSout, probs = percentile/100,
                                                                                         na.rm = TRUE))
  N.index <- (1 - max(0, (Nconc.pcntile - Nconc.target)/(2.2 - Nconc.target))) * carea/100
  P.index <- (1 - max(0, (Pconc.pcntile - Pconc.target)/(0.35 - Pconc.target))) * carea/100
  TSS.index <- (1 - max(0, (TSS.pcntile - TSS.target)/(150 - TSS.target))) * carea/100
  ###### would these urban values (2.2, 0.35 and 150) be the appropriate conc for 75th percentile????? this index can
  ###### potentially score < 1 (if N is added to the system)
  wq.index <- mean(c(N.index, P.index, TSS.index))
  summary <- data.frame(index.name = c("Runoff freq (d/y)", "Filtered flow (kL/y)",
                                       "Volume reduction (kL/y)", "Nitrogen (mg/L)",
                                       "Phosphorus (mg/L)", "TSS (mg/L)", "Water quality",
                                       "TOTAL EB"),
                       target = c(ndaysro.target, round(filtvol.lowtarget/1000,0),
                                  round(volred.target/1000, 0), Nconc.target, Pconc.target,
                                  TSS.target, NA, NA),
                       treatment = c(ndaysro.treatment, round(filtvol/1000, 0),
                                     round(lost.vol/1000, 0), round(Nconc.pcntile, 2),
                                     round(Pconc.pcntile, 3), round(TSS.pcntile, 2), NA, NA),
                       index.score = round(c(ff.index,fro.index, vr.index, N.index, P.index,
                                           TSS.index, wq.index, mean(c(ff.index, fro.index,
                                           vr.index, wq.index))), 2))
  summary
}

# Here's a simple error bar function in R.  produce vertical error bars on a plot--
# aliased vbars in anticipation of the day
# when I need hbars to plot horizontal error bars....
# error bars originate at (x, y0) and radiate to y0 + y1 and y0 - y2

ebars <- vbars <- function(x, y0, y1, y2, ...) {
  for (i in 1:length(x)) {
    if (y0[i] != 0 & !is.na(y0[i]) & y1[i] != 0 & !is.na(y1[i]) & (y0[i] != y2[i])) {
      arrows(x[i], y0[i], x[i], y1[i], angle = 90, length = 0.05, ...)
    }
    if (y0[i] != 0 & !is.na(y0[i]) & y2[i] != 0 & !is.na(y2[i]) & (y0[i] != y2[i])) {
      arrows(x[i], y0[i], x[i], y2[i], angle = 90, length = 0.05, ...)
    }
  }
}

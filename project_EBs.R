source(here::here("load_ld_scms_tables.R"))

#Trial of correcting likely mistake...
tanks$leak.at.propn.of.capacity[tanks$leak.at.propn.of.capacity > 0.5] <- 1 - tanks$leak.at.propn.of.capacity[tanks$leak.at.propn.of.capacity > 0.5]

scmProjects$term_scm_1 <- NA
scmProjects$n_term_scms <- NA
for (i in 1:dim(scmProjects)[1]){
  dbi <- data_on_datex(scmProjects$pipeID[i],scmProjects$installDate[i] + days(1))
  scmsi <- SCMs[SCMs$projectID == scmProjects$projectID[i],]
  termsi <- scmsi$scmID[!scmsi$nextds %in% scmsi$scmID]
  scmProjects$term_scm[i] <- termsi[1]
  scmProjects$n_term_scms[i] <- sum(!scmsi$nextds %in% scmsi$scmID)
  if(!is.na(scmProjects$parcelID[i])){
    scmProjects$parcel_ia[i] <- (dbi$parcels$roofAreaCon + dbi$parcels$paveAreaCon)[dbi$parcels$parcelID == scmProjects$parcelID[i]]
  }
  if(i == 1){
  scmProjects_EB <- data.frame(scmProjects[1,],   
                               t(unlist(EB_scm_on_datex(termsi,
                                                        scmProjects$installDate[i] + days(1)))))
  scmProjects_EB <- scmProjects_EB[0,]}
  for(j in 1:length(termsi)){
    if(j == 1){
    ebi <- data.frame(t(unlist(EB_scm_on_datex(termsi[j], 
                             scmProjects$installDate[i] + days(1)))))
    }else{
      ebi[c("RO","RO_binary","VR","FV","WQ","EB","EB_old_calc","EB_max")] <- 
        ebi[c("RO","RO_binary","VR","FV","WQ","EB","EB_old_calc","EB_max")] + 
        data.frame(t(unlist(EB_scm_on_datex(termsi[j],
                                 scmProjects$installDate[i] + days(1)))))[c("RO","RO_binary","VR","FV","WQ","EB","EB_old_calc","EB_max")]
    }
  }
  scmProjects_EB <- rbind(scmProjects_EB,
                          data.frame(scmProjects[i,],
                                     ebi))
}
scmProjects_EB$S <- 1 - (scmProjects_EB$EB/scmProjects_EB$EB_max)
scmProjects_EB$perc_parcel_treated <- 100*scmProjects_EB$EB_max*100/scmProjects_EB$parcel_ia
#All PL (Large public) systems had no parcelID, but treated all impervious surfaces upstream 
scmProjects_EB$perc_parcel_treated[is.na(scmProjects_EB$parcelID)] <- 100
# 37 parcels in DBS had SCMs installed, despite being disconnected already. 
scmProjects_EB$perc_parcel_treated[scmProjects_EB$parcel_ia == 0] <- NA
# sum(!is.na(scmProjects_EB$perc_parcel_treated) & scmProjects_EB$perc_parcel_treated > 101)
# 22 with scm specs saying more IA than area on property. 11 of these were systems in the LSC
# catchment that had impervious area spanning multiple parcels, so these are ok.  
# The remaining 11 were in Dobsons Creek catchment, 6 were associated with downpipe diverters,
# and therefore inconsequential, the remaining 5 were minor errors resulting  
# from imperfect ia delineation in dobsons.  Not corrected, as they will have 
# very little effect on results.

scmProjects_EB$perc_runoff_lost <- 100*(scmProjects_EB$V_u - scmProjects_EB$V_m)/scmProjects_EB$V_u
sum(scmProjects_EB$perc_runoff_lost < 0)
#Check this one problem...
scmProjects_EB[scmProjects_EB$perc_runoff_lost < 0,]
# Definitely a rounding error: very very small number.  That's ok

sum(scmProjects_EB$V_m < 0)
#zero - thank goodness

head(scmProjects_EB[order(scmProjects_EB$S),],15)
head(scmProjects_EB[order(scmProjects_EB$S, decreasing = TRUE),],25)

temp <- scmProjects_EB[!grepl("Ex",scmProjects_EB$projectID),]
temp <- temp[!grepl("RN-",temp$projectID),]
temp <- temp[!grepl("RE-",temp$projectID),]
head(temp[order(temp$S, decreasing = TRUE),],25)

temp_dbs <- temp[grep("DBS",temp$round),]
head(temp_dbs[order(temp_dbs$S, decreasing = TRUE),],25)
temp_lsc <- temp[-grep("DBS",temp$round),]
head(temp_lsc[order(temp_lsc$S, decreasing = TRUE),],25)

with(temp_lsc[temp_lsc$round %in% 1:4,],sum(VR/EB_max > 0.5)/sum(temp_lsc$round %in% 1:4))
with(temp_lsc[temp_lsc$round %in% 1:4,], hist(perc_runoff_lost))

with(temp_lsc[temp_lsc$round %in% 1:4,],sum(perc_runoff_lost > 0.550345 * 100)/sum(temp_lsc$round %in% 1:4))  #27% (67 properties)
sum(temp_lsc$EB_max[temp_lsc$round %in% 1:4 & temp_lsc$perc_runoff_lost > 0.550345 * 100])/100 #1.5 ha
with(temp_dbs[temp_dbs$round %in% c("DBS tanks round 1","DBS tank round 2"),], 
     sum(perc_runoff_lost > 0.550345 * 100)/sum(temp_dbs$round %in% c("DBS tanks round 1","DBS tank round 2"))) #16% (34 properties)
sum(temp_dbs$EB_max[temp_dbs$round %in% c("DBS tanks round 1","DBS tank round 2") & temp_dbs$perc_runoff_lost > 0.550345 * 100])/100 
  #0.5 ha
#Proportion of private property treatments which achieved target runoff reduction (band in Fig. 3)

save(scmProjects_EB, file = "data/scmProjects_EB.rda", compress = "xz")



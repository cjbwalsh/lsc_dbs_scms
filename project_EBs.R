source(here::here("load_ld_scms_tables.R"))
scmProjects$term_scm_1 <- NA
scmProjects$n_term_scms <- NA
for (i in 1:dim(scmProjects)[1]){
  dbi <- data_on_datex(scmProjects$pipeID[i],scmProjects$installDate[i] + days(1))
  scmsi <- SCMs[SCMs$projectID == scmProjects$projectID[i],]
  termsi <- scmsi$scmID[!scmsi$nextds %in% scmsi$scmID]
  scmProjects$term_scm[i] <- termsi[1]
  scmProjects$n_term_scms[i] <- sum(!scmsi$nextds %in% scmsi$scmID)
  if(!is.na(scmProjects$parcelID[i])){
    scmProjects$parcel_ia <- (dbi$parcels$roofAreaCon + dbi$parcels$paveAreaCon)[dbi$parcels$parcelID == scmProjects$parcelID[i]]
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
# 24 with scm specs saying more IA than area on property. 11 of these were systems in the LSC
# catchment that had impervious area spanning multiple parcels, so these are ok.  
# The remaining 13 were in Dobsons Creek catchment, 6 were associated with downpipe diverters,
# and therefore inconsequential, two were substantial errors requiring fixing..
tanks$tankCarea[tanks$scmID == "D4T401"] <- 170
tanks$tankCarea[tanks$scmID == "WRT402"] <- 155.76637
###FOR NOW...###
scmProjects_EB$perc_parcel_treated[scmProjects_EB$term_scm %in% c("D4T401","WRT402")] <- 100
# the remaining 5 were minor errors resulting from imperfect ia delineation in dobsons. 
# Not corrected, as they will have very little effect on results.

scmProjects_EB$perc_runoff_lost <- 100*(scmProjects_EB$V_u - scmProjects_EB$V_m)/scmProjects_EB$V_u
sum(scmProjects_EB$perc_runoff_lost < -0.1)
#Check these two problems...
scmProjects_EB[scmProjects_EB$perc_runoff_lost < 0,]
t(unlist(EB_scm_on_datex("RPLR552","2017-01-28")))
t(unlist(EB_scm_on_datex("RPNT480","2013-07-02")))
t(unlist(EB_scm_on_datex("RPNT470", "2013-07-02")))

scmProjects_EB[scmProjects_EB$V_m < 0,]  #VR can be > 100% (reward overextraction)
t(unlist(EB_scm_on_datex("WRT330", "2014-03-31")))
t(unlist(EB_scm_on_datex("R1T031", "2008-12-02")))

save(scmProjects_EB, file = "data/scmProjects_EB.rda", compress = "xz")



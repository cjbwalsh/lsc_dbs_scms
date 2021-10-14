# S5 code translated to rethinking functions, with substantial improvement in 
# calculation time.

## (relevant parts of) load data chunk
source("load_ld_scms_tables.R")
current.seed <- 787603905 #
library(rethinking)
nChains <- 4

## prepare_sim_set_chunk 
dates <- seq.Date(from = as.Date("2001-02-01"),
                  to = as.Date("2019-11-01"), 
                  by = "3 months")
t <- 1:length(dates)
sitecodes <- siteMap11$sitecode[!siteMap11$sitecode %in% c("LIS0004","DBS0008")]
# Adjust ei so that del_ei is zero before experiment in all sites
# i.e. include existing SCMs in calculation of ei_s1
exp_cats <- unique(ei_ts_all$sitecode)
for(i in 1:length(exp_cats)){
  if(ei_ts_all$ei[ei_ts_all$sitecode == exp_cats[i]][2] != 
     ei_ts_all$eb[ei_ts_all$sitecode == exp_cats[i]][2]) 
    #    cat(exp_cats[i], "\n")
    ei_ts_all$ei[ei_ts_all$sitecode == exp_cats[i]] <- 
      ei_ts_all$ei[ei_ts_all$sitecode == exp_cats[i]] - 
      (ei_ts_all$ei[ei_ts_all$sitecode == exp_cats[i]][2] - 
         ei_ts_all$eb[ei_ts_all$sitecode == exp_cats[i]][2])
  ei_ts_all$s[ei_ts_all$sitecode == exp_cats[i]] <- 
    ei_ts_all$s[ei_ts_all$sitecode == exp_cats[i]] - 
    (ei_ts_all$s[ei_ts_all$sitecode == exp_cats[i]][2] - 
       ei_ts_all$eb[ei_ts_all$sitecode == exp_cats[i]][2])
  ei_ts$ei[ei_ts$sitecode == exp_cats[i]] <- ei_ts_all$ei[ei_ts_all$sitecode == exp_cats[i]]
  ei_ts$s[ei_ts$sitecode == exp_cats[i]] <- ei_ts_all$s[ei_ts_all$sitecode == exp_cats[i]]
}

ei_ts$lei <- log10(ei_ts$ei*100 + 0.1)
ei_ts$lei_s <- ei_ts$lei_s0 <- ei_ts$lei
for(i in 1:length(exp_cats)){
  ei_ts$lei_s[ei_ts$sitecode == exp_cats[i]] <- 
    log10(ei_ts_all$eb[ei_ts_all$sitecode == exp_cats[i]]*100 + 0.1)
  ei_ts$lei_s0[ei_ts$sitecode == exp_cats[i]] <- 
    log10(ei_ts_all$s[ei_ts_all$sitecode == exp_cats[i]]*100 + 0.1)
}
ei_ts$del_ei_s <-  ei_ts$lei_s - ei_ts$lei
#Small rounding error causing a small number of positive values of del_ei_s (1.1e-16)
ei_ts$del_ei_s[ei_ts$del_ei_s > 0] <- 0

sim_set <- data.frame(site = rep(sitecodes, each = length(t)),
                      date = rep(dates, length(sitecodes)),
                      t = rep(t, length(sitecodes)),
                      lei_s1 = NA, del_ei_s = NA)

for(i in 1:length(sitecodes)){
  sim_set$lei_s1[sim_set$site == sitecodes[i]] <- 
    ei_ts$lei[ei_ts$sitecode == sitecodes[i] & ei_ts$date %in% sim_set$date]
  sim_set$del_ei_s[sim_set$site == sitecodes[i]] <- 
    ei_ts$del_ei_s[ei_ts$sitecode == sitecodes[i] & ei_ts$date %in% sim_set$date]
}

## Chunk 3: create *y* and *y1* for the 76 samples and a scaled 
#         time variable (*t_scaled*), which ranges between -1.5 and 1.5. 
#  Additionally create siteno integer variable

lFRP50 <- c(-1.56864E+00, -2.22185E+00, -2.09691E+00, -2.22185E+00, -2.52288E+00, -1.88606E+00, -2.04576E+00, -2.52288E+00, -2.52288E+00, -2.69897E+00, -2.00000E+00, -2.39794E+00, -1.92082E+00, -1.92082E+00) + 3 # + 3 to convert from mg/L to ug/L
EI <- c(1.95167E-01, 4.26416E-03, 3.62570E-02, 4.19733E-03, 0.00000E+00, 9.52280E-02, 4.63966E-01, 0.00000E+00, 0.00000E+00, 1.16397E-03, 1.03733E-02, 6.65377E-03, 3.82980E-01, 5.47130E-02)
lei <- log10(EI*100 + 0.1)
mod_frp_ei <- lm(lFRP50 ~ lei)
# 1. vary among sites as a function of EI as observed for log(median FRP)
sim_set$y <- rnorm(dim(sim_set)[1], 
                   summary(mod_frp_ei)$coefficients[1,1],
                   summary(mod_frp_ei)$coefficients[1,2]) +  #distribution of model intercept
  summary(mod_frp_ei)$coefficients[2,1] * sim_set$lei_s1 + #model slope
  rnorm(dim(sim_set)[1], 0,
        summary(mod_frp_ei)$coefficients[2,2])  #distribution of model slope error
# For the simulation, we allow values of y to fall below the detection limit   

# 2. add a linear increase with time unrelated to the SCM implementation, 
# and make it a greater increase with greater EI...
sim_set$y <- sim_set$y + (sim_set$lei_s1 + 1.05)*sim_set$t/(76*7.5)
# ~0.08% increase per year for LYR - ei = 0% lei_s1 = -1: 10^(0.05*4/(76*7.5))
# ~3.9% increase per year for BRS  - ei = 21.8% lei_s1 = 1.337589: 10^((1.05 + 1.337829)*4/(76*7.5))

# 3. create variable y1 so that its response to del_ei equals its response to ei_s1
sim_set$y1 <- sim_set$y
sim_set$y1 <- sim_set$y + 
  summary(mod_frp_ei)$coefficients[2,1] * sim_set$del_ei_s  #model slope

# 4. create a time variable scaled to range between -1.5 and 1.5
sim_set$t_scaled <- (sim_set$t - 38.5)/25

# 5. create an integer siteno for stan model
sim_set$siteno <- coerce_index(factor(sim_set$site, 
                                      levels = c("SAS0002","LYR0007","OLN0009",
                                                 "BRS0015","FER0006","DBS0004",
                                                 "LSS0001", "LSN0001", "LIS0001")))
sitecodes <- data.frame(sitecode = c("SAS0002","LYR0007","OLN0009",
                                              "BRS0015","FER0006","DBS0004",
                                              "LSS0001", "LSN0001", "LIS0001"), 
                        siteno = as.integer(1:9))

## Chunk 4. A. Prepare set of values for the model to predict to  ######
del_ei_p <- seq(-1,0, length = 15)
#make relevant del_ei_p values equal actual achieved reductions
del_ei_p[7] <- min(sim_set$del_ei_s[sim_set$site == "LSN0001"])
del_ei_p[14] <- min(sim_set$del_ei_s[sim_set$site == "LIS0001"])
del_ei_p[12] <- min(sim_set$del_ei_s[sim_set$site == "LSS0001"])
del_ei_p <- c(del_ei_p[1:11],min(sim_set$del_ei_s[sim_set$site == "DBS0004"]),
              del_ei_p[14:15])
# Make predictions for a reference site, a control site and the 4 experimental 
# sites used in sim_set with ei set at level at end of experiment
ei <- ei_ts$lei[ei_ts$sitecode %in% c("LYR0007","BRS0015","DBS0004",
                                      "LSN0001","LIS0001","LSS0001") & 
                  ei_ts$date == max(ei_ts$date)]
new_X <- model.matrix(~ ei_p + del_ei_p + t_scaled_p + site_p,
                      expand.grid(ei_p = ei,
                                  del_ei_p = del_ei_p, 
                                  t_scaled_p = max(sim_set$t_scaled),
                                  site_p = 1:9))

## Chunk 4 B. Re-draft data compilation and model specification

## Create dataset for model
d <- sim_set[c("y","siteno")]
d$ei <- sim_set$lei_s1
d$del_ei <- sim_set$del_ei_s
d$t <- sim_set$t_scaled
d$y <- sim_set$y1

#system.time({ 
mod_y1 <- map2stan(alist(y ~ dnorm(mu, sigma),
                           mu ~ a + a_t[siteno] + b_ei * ei + b_del_ei * del_ei +
                             b_t[siteno] * t,
                           a ~ dnorm(0,20),
                           b_ei ~ dnorm(0,5),
                           b_del_ei ~ dnorm(0,5),
                           a_t[siteno] ~  dnorm(0, a_t_sigma),
                           a_t_sigma ~ dcauchy(0,2), #dexp(1),  #
                           b_t[siteno] ~ dnorm(0,5),
                           sigma ~ dcauchy(0,2)), #dexp(1)),    #
                     data=d, iter=5000, warmup = 2000, log_lik=TRUE,
                     cores = 4, chains = nChains, rng_seed = current.seed)

#Calculate t_auto from residual estimates from model without auto_t
y1_pred <- link(mod_y1, data = d)
y1_pred_mean <- apply(y1_pred, 2, FUN = mean)
sim_set$non_auto_resids_y1 <- sim_set$y1 - y1_pred_mean
sim_set$auto_t_y1 <- NA
for (j in 1:length(unique(sim_set$siteno))) {
  sim_set$auto_t_y1[sim_set$siteno == j][-1] <- 
    sim_set$non_auto_resids_y1[sim_set$siteno == j][-sum(sim_set$siteno == j)]
}

d$auto_t <- sim_set$auto_t_y1
d <- d[!is.na(d$auto_t),]

  mod_y1_auto_t <- map2stan(alist(y ~ dnorm(mu, sigma),
                           mu ~ a + a_t[siteno] + b_ei * ei + b_del_ei * del_ei +
                             b_t[siteno] * t + b_autot *auto_t,
                           a ~ dnorm(0,35), #adjusting this prior achieved 0 divergents
                           b_ei ~ dnorm(0,5),
                           b_del_ei ~ dnorm(0,5),
                           a_t[siteno] ~  dnorm(0, a_t_sigma),
                           a_t_sigma ~ dcauchy(0,2), # dexp(1), #alternative prior if convergence problematic
                           b_t[siteno] ~ dnorm(0,5),
                           b_autot ~ dnorm(0,5),
                           sigma ~ dcauchy(0,2)), #dexp(1)),    
                     data=d, iter=4000, warmup = 2000, 
                     control=list(adapt_delta=0.98), 
                     cores = 4, chains = nChains, rng_seed = current.seed)

#Re-run mod_y1 with reduced dataset so that they are comparable
mod_y1 <- map2stan(alist(y ~ dnorm(mu, sigma),
                           mu ~ a + a_t[siteno] + b_ei * ei + b_del_ei * del_ei +
                             b_t[siteno] * t,
                           a ~ dnorm(0,15), #adjusted to minimize divergent transitions
                           b_ei ~ dnorm(0,5),
                           b_del_ei ~ dnorm(0,5),
                           a_t[siteno] ~  dnorm(0, a_t_sigma),
                           a_t_sigma ~ dcauchy(0,2), 
                           b_t[siteno] ~ dnorm(0,5),
                           sigma ~ dcauchy(0,2)), 
                     data=d, iter=5000, warmup = 2000,
                     cores = 4, chains = nChains, rng_seed = current.seed)

#And compare both to mod_y1_no_t
mod_y1_no_t <- map2stan(alist(y ~ dnorm(mu, sigma),
                           mu ~ a + a_t[siteno] + b_ei * ei + b_del_ei * del_ei,
                           a ~ dnorm(0,50),
                           b_ei ~ dnorm(0,10),
                           b_del_ei ~ dnorm(0,10),
                           a_t[siteno] ~  dnorm(0, a_t_sigma),
                           a_t_sigma ~ dcauchy(0,2), #dexp(1), 
                           sigma ~ dcauchy(0,2)), #dexp(1)), 
                     data=d, iter=4500, warmup = 2000, 
                     control=list(adapt_delta=0.95, max_treedepth = 15),
                     cores = 4, chains = nChains, rng_seed = current.seed)

## Create dataset for model
d <- sim_set[c("y","siteno")]
d$ei <- sim_set$lei_s1
d$del_ei <- sim_set$del_ei_s
d$t <- sim_set$t_scaled
d$y <- sim_set$y

mod_y <- map2stan(alist(y ~ dnorm(mu, sigma),
                          mu ~ a + a_t[siteno] + b_ei * ei + b_del_ei * del_ei +
                            b_t[siteno] * t,
                          a ~ dnorm(0,20),  #adjusted to minimize divergent transitions
                          b_ei ~ dnorm(0,5),
                          b_del_ei ~ dnorm(0,5),
                          a_t[siteno] ~  dnorm(0, a_t_sigma),
                          a_t_sigma ~ dcauchy(0,2), #dexp(1),  #
                          b_t[siteno] ~ dnorm(0,5),
                          sigma ~ dcauchy(0,2)), #dexp(1)),    #
                    data=d, iter=4000, warmup = 2000, 
                    cores = 4, chains = nChains, rng_seed = current.seed)
                    # 1 divergent iteration
y_pred <- link(mod_y, data = d)
y_pred_mean <- apply(y_pred, 2, FUN = mean)
sim_set$non_auto_resids_y <- sim_set$y - y_pred_mean
sim_set$auto_t_y <- NA
for (j in 1:length(unique(sim_set$siteno))) {
  sim_set$auto_t_y[sim_set$siteno == j][-1] <- 
    sim_set$non_auto_resids_y[sim_set$siteno == j][-sum(sim_set$siteno == j)]
}

d$auto_t <- sim_set$auto_t_y
d <- d[!is.na(d$auto_t),]

 mod_y_auto_t <- map2stan(alist(y ~ dnorm(mu, sigma),
                                  mu ~ a + a_t[siteno] + b_ei * ei + b_del_ei * del_ei +
                                    b_t[siteno] * t + b_autot *auto_t,
                                  a ~ dnorm(0,30), #adjusting this prior achieved fewer divergents
                                  b_ei ~ dnorm(0,5),
                                  b_del_ei ~ dnorm(0,5),
                                  a_t[siteno] ~  dnorm(0, a_t_sigma),
                                  a_t_sigma ~ dcauchy(0,2), # dexp(1),    #
                                  b_t[siteno] ~ dnorm(0,5),
                                  b_autot ~ dnorm(0,5),
                                  sigma ~ dcauchy(0,2)), #dexp(1)),    #
                            data=d, iter=4000, warmup = 2000, 
                            control=list(adapt_delta=0.95), 
                            cores = 4, chains = nChains, rng_seed = current.seed)
# 1 divergent iteration
 
#Re-run mod_y1 with reduced dataset so that they are comparable
 mod_y <- map2stan(alist(y ~ dnorm(mu, sigma),
                         mu ~ a + a_t[siteno] + b_ei * ei + b_del_ei * del_ei +
                           b_t[siteno] * t,
                         a ~ dnorm(0,15),  #adjusted to minimize divergent transitions
                         b_ei ~ dnorm(0,5),
                         b_del_ei ~ dnorm(0,5),
                         a_t[siteno] ~  dnorm(0, a_t_sigma),
                         a_t_sigma ~ dcauchy(0,2), #dexp(1),  #
                         b_t[siteno] ~ dnorm(0,5),
                         sigma ~ dcauchy(0,2)), #dexp(1)),    #
                   data=d, iter=4000, warmup = 2000, 
                   cores = 4, chains = nChains, rng_seed = current.seed)
 
 mod_y_no_t <- map2stan(alist(y ~ dnorm(mu, sigma),
                           mu ~ a + a_t[siteno] + b_ei * ei + b_del_ei * del_ei,
                           a ~ dnorm(0,50),
                           b_ei ~ dnorm(0,5),
                           b_del_ei ~ dnorm(0,5),
                           a_t[siteno] ~  dnorm(0, a_t_sigma),
                           a_t_sigma ~ dcauchy(0,2), #dexp(1),  #
                           sigma ~ dcauchy(0,2)), #dexp(1)),    #
                     data=d, iter=4500, warmup = 2000,
                     control=list(max_treedepth = 15),
                     cores = 4, chains = nChains, rng_seed = current.seed)
#})  # ~30 minutes for all 8 model runs

# with some warnings, which can be summarised using the model_diagnostics 
# function sourced from map_mod_diagnostics
if(!"stan_utility.R" %in% dir("code"))
download.OSF.file(GUID = "8zme2",file_name = "stan_utility.R", subdir = "code")
source("code/stan_utility.R")
source("code/map_mod_diagnostics.R")
model_diagnostics(mod_y1_auto_t, nChains = nChains)               # all good
model_diagnostics(mod_y1, nChains = nChains)                      # all good    
model_diagnostics(mod_y1_no_t, nChains = nChains, max_depth = 15) # all good
model_diagnostics(mod_y_auto_t, nChains = nChains)                # 1 divergent transition
model_diagnostics(mod_y, nChains = nChains)       # all good, but note first run of mod_y had 1 divergent transition
model_diagnostics(mod_y_no_t, nChains = nChains, max_depth = 15)  # all good

compare(mod_y1, mod_y1_auto_t, mod_y1_no_t)
#mod_y1, mod_y1_auto_t are of similar plausibility, both much more plausible than mod_y1_no_t
compare(mod_y, mod_y_auto_t, mod_y_no_t)
#ditto

save(mod_y1, file = "~/uomShare/wergStaff/ChrisW/git-data/lsc_dbs_scms/model_objects/mod_y1_map.rda")
save(mod_y1_auto_t, file = "~/uomShare/wergStaff/ChrisW/git-data/lsc_dbs_scms/model_objects/mod_y1_auto_t_map.rda")
save(mod_y1, file = "~/uomShare/wergStaff/ChrisW/git-data/lsc_dbs_scms/model_objects/mod_y_map.rda")
save(mod_y1_auto_t, file = "~/uomShare/wergStaff/ChrisW/git-data/lsc_dbs_scms/model_objects/mod_y_auto_t_map.rda")

#Load the saved files to save 30 minutes recompiling the models....

#Fig. 6 in paper using the above models

# First create new dataset to predict to:
del_ei_p <- seq(-1,0, length = 15)
#make relevant del_ei_p values equal actual achieved reductions
del_ei_p[7] <- min(sim_set$del_ei_s[sim_set$site == "LSN0001"])
del_ei_p[14] <- min(sim_set$del_ei_s[sim_set$site == "LIS0001"])
del_ei_p[12] <- min(sim_set$del_ei_s[sim_set$site == "LSS0001"])
del_ei_p <- c(del_ei_p[1:11],min(sim_set$del_ei_s[sim_set$site == "DBS0004"]),
              del_ei_p[14:15])
# Make predictions for a reference site, a control site and the 4 experimental 
# sites used in sim_set with ei set at level at end of experiment
ei <- ei_ts$lei[ei_ts$sitecode %in% c("LYR0007","BRS0015","DBS0004",
                                      "LSN0001","LIS0001","LSS0001") & 
                  ei_ts$date == max(ei_ts$date)]
new_X <- model.matrix(~ siteno + ei + del_ei + t + auto_t,
                      expand.grid(siteno = as.integer(1:9), 
                                  ei = ei,
                                  del_ei = del_ei_p, 
                                  t = max(sim_set$t_scaled),
                                  auto_t = 0))

for(i in 1:2){
  new_d <- as.data.frame(new_X)
  new_d <- new_d[,-1]
  load(paste0("~/uomShare/wergStaff/ChrisW/git-data/lsc_dbs_scms/model_objects/mod_y",ifelse(i == 1, "1",""),"_auto_t_map.rda"))
  if(i == 1) assign("sim_set_mod_auto_t",mod_y1_auto_t)
  if(i == 2) assign("sim_set_mod_auto_t",mod_y_auto_t)
  assign(paste0("coeffs_y",ifelse(i == 1, "1","")),
         as.data.frame(summary(sim_set_mod_auto_t@stanfit, 
                               pars = c("a","b_ei","b_del_ei",
                                        "b_autot","b_t"),
                               probs = c(0.025,0.10,0.5,0.9,0.975))$summary))
  new_y_pred <- link(sim_set_mod_auto_t, new_d)
  assign(paste0("new_y_pred_y",ifelse(i == 1, "1","")),
         data.frame(p025 = apply(new_y_pred,2,FUN = quantile, prob = 0.025),
                    p100 = apply(new_y_pred,2,FUN = quantile, prob = 0.1),
                    p500 = apply(new_y_pred,2,FUN = quantile, prob = 0.5),
                    p900 = apply(new_y_pred,2,FUN = quantile, prob = 0.9),
                    p975 = apply(new_y_pred,2,FUN = quantile, prob = 0.975)))
}

new_X_y <- cbind(new_y_pred_y, new_d)
new_X_y1 <- cbind(new_y_pred_y1, new_d)
new_X_y1$sitecode <- new_X_y$sitecode <- sitecodes$sitecode[match(new_X_y$siteno,1:9)]

# pdf(file = "images/Fig 6.pdf", width = 7, height = 5.5)
layout(matrix(c(1,3,2,4,5,5),3,2,byrow = TRUE), 
       heights=c(10,11,2),widths = c(10,10))
par(mar = c(2,4,1,1))
plot(c(-0.5,0.5),c(0.5,2.5), type = 'n', ylab = "", xlab = "", axes = 0)
points(coeffs_y$`50%`[2:3], 1:2, pch = 21, bg = "gray", cex = 1.5)
for(i in 1:2){
  lines(c(coeffs_y$`2.5%`[i+1],coeffs_y$`97.5%`[i+1]), c(i,i), lwd = 1.5)
  lines(c(coeffs_y$`10%`[i+1],coeffs_y$`90%`[i+1]), c(i,i), lwd = 3.5)
}
axis(1, at = seq(-1,1,0.2), labels = rep("",11))
axis(2, at = 0:3, labels = c("",expression(EI[S1]),
                             expression(~Delta~EI[S]),""), las = 1)
abline(v = 0, lty = 3)
title(main = "A. y", adj = 0)
par(mar = c(4,4,1,1))
plot(c(-0.5,0.5),c(0.5,2.5), type = 'n', ylab = "", xlab = "", axes = 0)
points(coeffs_y1$`50%`[2:3], 1:2, pch = 21, bg = "gray", cex = 1.5)
for(i in 1:2){
  lines(c(coeffs_y1$`2.5%`[i+1],coeffs_y1$`97.5%`[i+1]), c(i,i), lwd = 1.5)
  lines(c(coeffs_y1$`10%`[i+1],coeffs_y1$`90%`[i+1]), c(i,i), lwd = 3.5)
}
axis(1, at = seq(-1,1,0.2))
axis(2, at = 0:3, labels = c("",expression(EI[S1]),
                             expression(~Delta~EI[S]),""), las = 1)
abline(v = 0, lty = 3)
title(main = "C. y1", adj = 0)
title(xlab = "Coefficient")

# Four 4 experimental catchments, predict for del_ei = 0 (no SCMs),
# del_ei = minimum achieved del_ei achieved in that catchment, and 
# del_ei = -1 (if SCMs were to reduce ei by an order of magnitude)
# and also the reference state LYR0007 at end of study period
experimental_cats <- c("LSN0001","LSS0001","DBS0004","LIS0001")
temp <- new_X_y[0,]
for(i in 1:4){
  temp <- rbind(temp,
                new_X_y[new_X_y$sitecode == experimental_cats[i] & 
                          new_X_y$ei == -1 &  # i.e. LYR0007 EI
                          new_X_y$del_ei == 0,])
  temp <- rbind(temp,
                new_X_y[new_X_y$sitecode == experimental_cats[i] & 
                          new_X_y$ei == ei_ts$lei[ei_ts$sitecode == experimental_cats[i] & 
                                                      ei_ts$date == max(ei_ts$date)] & 
                          round(new_X_y$del_ei,4) %in% round(c(-1,
                                                                 ifelse(i == 2,-0.28571429, min(sim_set$del_ei_s[sim_set$site == experimental_cats[i]])),0),4),])
}
temp$x <- rep(c(-0.15,-0.075,0.075,0.15),4) + rep(1:4, each = 4)
temp$col <- rep(RColorBrewer::brewer.pal(5, "RdYlGn")[c(5,4,2,1)],4)

par(mar = c(2,4,1,1))
plot(temp$x, temp$p500, ylim = c(0.5,1.6), pch = 21, bg = temp$col, 
     cex = 1.25, axes = FALSE, ylab = "y", xlab = "")
for(i in 1:dim(temp)[1]){
  lines(rep(temp$x[i],2),c(temp$p025[i],temp$p975[i]), 
        col = temp$col[i], lwd = 1.5)
  lines(rep(temp$x[i],2),c(temp$p100[i],temp$p900[i]), 
        col = temp$col[i], lwd = 3.5)
}
points(temp$x, temp$p500, pch = 21, bg = temp$col, cex = 1.25)
axis(2, at = seq(-1,2,0.5), las = 1)
axis(1, at = 0:5, labels = rep("",6))
title("B.", adj = 0)

temp <- new_X_y[0,]
for(i in 1:4){
  temp <- rbind(temp,
                new_X_y1[new_X_y1$sitecode == experimental_cats[i] & 
                           new_X_y1$ei == -1 &  # i.e. LYR0007 EI
                           new_X_y1$del_ei == 0,])
  temp <- rbind(temp,
                new_X_y1[new_X_y1$sitecode == experimental_cats[i] & 
                           new_X_y1$ei == ei_ts$lei[ei_ts$sitecode == experimental_cats[i] & 
                                                        ei_ts$date == max(ei_ts$date)] & 
                           round(new_X_y1$del_ei,4) %in% round(c(-1,
                                                                   ifelse(i == 2,-0.28571429, min(sim_set$del_ei_s[sim_set$site == experimental_cats[i]])),0),4),])
}
temp$x <- rep(c(-0.15,-0.075,0.075,0.15),4) + rep(1:4, each = 4)
temp$col <- rep(RColorBrewer::brewer.pal(5, "RdYlGn")[c(5,4,2,1)],4)

par(mar = c(4,4,1,1))
plot(temp$x, temp$p500, ylim = c(0.5,1.6), pch = 21, bg = temp$col, 
     cex = 1.25, axes = FALSE, ylab = "y1", xlab = "")
for(i in 1:dim(temp)[1]){
  lines(rep(temp$x[i],2),c(temp$p025[i],temp$p975[i]), 
        col = temp$col[i], lwd = 1.5)
  lines(rep(temp$x[i],2),c(temp$p100[i],temp$p900[i]), 
        col = temp$col[i], lwd = 3.5)
}
points(temp$x, temp$p500, pch = 21, bg = temp$col, cex = 1.25)
axis(2, at = seq(-1,2,0.5), las = 1)
axis(1, at = 0:5, labels = c("","Ln","Ls","D4","L1",""))
title("D.", adj = 0)
title(xlab = "Experimental site")
par(mar = c(0,0,0,0))
plot.new()
legend("center", c("No SCMs", "Achieved reduction", 
                   "EIs reduced by factor of 10", "Reference condition"),
       pch = 21, pt.bg = RColorBrewer::brewer.pal(5, "RdYlGn")[c(1,2,4,5)],
       ncol = 4)
#dev.off()

# S5 code translated to rethinking functions, with substantial improvement in 
# calculation time.

## (relevant parts of) load data chunk
source("load_ld_scms_tables.R")
current.seed <- 787603905 #
library(rethinking)

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

system.time({ 
  mod_y1 <- map2stan(alist(y ~ dnorm(mu, sigma),
                           mu ~ a + a_t[siteno] + b_ei * ei + b_del_ei * del_ei +
                             b_t[siteno] * t,
                           a ~ dnorm(0,10),
                           b_ei ~ dnorm(0,5),
                           b_del_ei ~ dnorm(0,5),
                           a_t[siteno] ~  dnorm(0, a_t_sigma),
                           a_t_sigma ~ dcauchy(0,2), #dexp(1),  #
                           b_t[siteno] ~ dnorm(0,5),
                           sigma ~ dcauchy(0,2)), #dexp(1)),    #
                     data=d, iter=4000, warmup = 2000, 
                     cores = 4, chains=4, rng_seed = 787603905)

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
                           a_t_sigma ~ dcauchy(0,2), # dexp(1),    #
                           b_t[siteno] ~ dnorm(0,5),
                           b_autot ~ dnorm(0,5),
                           sigma ~ dcauchy(0,2)), #dexp(1)),    #
                     data=d, iter=4000, warmup = 2000, 
                     control=list(adapt_delta=0.98), 
                     cores = 4, chains=4, rng_seed = 787603905)

#Re-run mod_y1 with reduced dataset so that they are comparable
  mod_y1 <- map2stan(alist(y ~ dnorm(mu, sigma),
                           mu ~ a + a_t[siteno] + b_ei * ei + b_del_ei * del_ei +
                             b_t[siteno] * t,
                           a ~ dnorm(0,20),
                           b_ei ~ dnorm(0,5),
                           b_del_ei ~ dnorm(0,5),
                           a_t[siteno] ~  dnorm(0, a_t_sigma),
                           a_t_sigma ~ dcauchy(0,2), ##dexp(1),  #
                           b_t[siteno] ~ dnorm(0,5),
                           sigma ~ dcauchy(0,2)), #dexp(1)),    #
                     data=d, iter=4500, warmup = 2000,
                     control=list(adapt_delta=0.98),
                     cores = 4, chains=4, rng_seed = 787603905)
#3 divergent transitions, and 500 extra iterations to get sufficient ESS

#And compare both to mod_y1_no_t
  mod_y1_no_t <- map2stan(alist(y ~ dnorm(mu, sigma),
                           mu ~ a + a_t[siteno] + b_ei * ei + b_del_ei * del_ei,
                           a ~ dnorm(0,50),
                           b_ei ~ dnorm(0,10),
                           b_del_ei ~ dnorm(0,10),
                           a_t[siteno] ~  dnorm(0, a_t_sigma),
                           a_t_sigma ~ dcauchy(0,2), ##dexp(1),  #
                           sigma ~ dcauchy(0,2)), #dexp(1)),    #
                     data=d, iter=4500, warmup = 2000, 
                     control=list(adapt_delta=0.95, max_treedepth = 15),
                     cores = 4, chains=4, rng_seed = 787603905)

compare(mod_y1, mod_y1_auto_t, mod_y1_no_t)
#mod_y1, mod_y1_auto_t are of similar plausibility, both much more so than mod_y1_no_t

## Create dataset for model
d <- sim_set[c("y","siteno")]
d$ei <- sim_set$lei_s1
d$del_ei <- sim_set$del_ei_s
d$t <- sim_set$t_scaled
d$y <- sim_set$y

mod_y <- map2stan(alist(y ~ dnorm(mu, sigma),
                          mu ~ a + a_t[siteno] + b_ei * ei + b_del_ei * del_ei +
                            b_t[siteno] * t,
                          a ~ dnorm(0,100),
                          b_ei ~ dnorm(0,5),
                          b_del_ei ~ dnorm(0,5),
                          a_t[siteno] ~  dnorm(0, a_t_sigma),
                          a_t_sigma ~ dcauchy(0,2), #dexp(1),  #
                          b_t[siteno] ~ dnorm(0,5),
                          sigma ~ dcauchy(0,2)), #dexp(1)),    #
                    data=d, iter=4000, warmup = 2000, 
                    cores = 4, chains=4, rng_seed = 787603905)

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
                                  a ~ dnorm(0,35), #adjusting this prior achieved 0 divergents
                                  b_ei ~ dnorm(0,5),
                                  b_del_ei ~ dnorm(0,5),
                                  a_t[siteno] ~  dnorm(0, a_t_sigma),
                                  a_t_sigma ~ dcauchy(0,2), # dexp(1),    #
                                  b_t[siteno] ~ dnorm(0,5),
                                  b_autot ~ dnorm(0,5),
                                  sigma ~ dcauchy(0,2)), #dexp(1)),    #
                            data=d, iter=4000, warmup = 2000, 
                            control=list(adapt_delta=0.98), 
                            cores = 4, chains=4, rng_seed = 787603905)

#Re-run mod_y1 with reduced dataset so that they are comparable
 mod_y_no_t <- map2stan(alist(y ~ dnorm(mu, sigma),
                           mu ~ a + a_t[siteno] + b_ei * ei + b_del_ei * del_ei,
                           a ~ dnorm(0,20),
                           b_ei ~ dnorm(0,5),
                           b_del_ei ~ dnorm(0,5),
                           a_t[siteno] ~  dnorm(0, a_t_sigma),
                           a_t_sigma ~ dcauchy(0,2), #dexp(1),  #
                           sigma ~ dcauchy(0,2)), #dexp(1)),    #
                     data=d, iter=4500, warmup = 2000,
                     control=list(adapt_delta=0.98, max_treedepth = 15),
                     cores = 4, chains=4, rng_seed = 787603905)

compare(mod_y1, mod_y1_auto_t, mod_y_no_t)
})  # 

save(mod_y1, file = "~/uomShare/wergStaff/ChrisW/git-data/lsc_dbs_scms/model_objects/mod_y1_map.rda")
save(mod_y1_auto_t, file = "~/uomShare/wergStaff/ChrisW/git-data/lsc_dbs_scms/model_objects/mod_y1_auto_t_map.rda")
save(mod_y1, file = "~/uomShare/wergStaff/ChrisW/git-data/lsc_dbs_scms/model_objects/mod_y_map.rda")
save(mod_y1_auto_t, file = "~/uomShare/wergStaff/ChrisW/git-data/lsc_dbs_scms/model_objects/mod_y_auto_t_map.rda")

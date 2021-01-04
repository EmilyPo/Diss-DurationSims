# Oct 2020 
# I swear to god this is the last time I want to run these scenarios 

library(here)
library(EpiModelHIV)

# nets1 <- no cross no offset 
nets1 <- readRDS(here("estimation", "ergm310", "netests_july2020_nocross.rds"))
# nets2 <- no cross yes offset
nets2 <- readRDS(here("estimation", "ergm310", "netests_july2020_offset_nocross.rds"))
# nets3 <- no cross yes offset yes edapprox
# update nets2 for edapprox
# nets4 <- no cross yes offset yes edapprox yes mort
nets4 <- readRDS(here("estimation", "ergm310", "netests_offset__nocross_edapproxmort.rds"))
# nets5 <- no cross yes offset yes nodefactor boost 
nets5 <- readRDS(here("estimation", "ergm310", "netests_sept2020_offset_nf_nocross.rds"))

#### 1. Base Model (no offset) ####
sim1 <- netsim(nets1, 
               param = param_het_reldur(olderpartner = FALSE, crossnetwork = FALSE, keep.completed = FALSE),
               init = init_het_reldur(),
               control = control_het_reldur(nsims=10, nsteps=52*60))
saveRDS(sim1, here("sims", "Oct2020", "Base_NoCross.rds"))

##### 2. Older Partner Offset (nets2) ####
sim2 <- netsim(nets2, 
               param = param_het_reldur(olderpartner = TRUE, crossnetwork = FALSE, keep.completed = FALSE),
               init = init_het_reldur(),
               control = control_het_reldur(nsims=10, nsteps=52*60))
saveRDS(sim2, here("sims", "Oct2020", "NoCross_Offset.rds"))

##### 3. Offset + Increased Age Boundary (nets2) ####
sim3 <- netsim(nets2, 
               param = param_het_reldur(olderpartner = TRUE, crossnetwork = FALSE, keep.completed = FALSE,
                                        prohibitOlderEgos = TRUE, breaks = c(15, 20, 25, 30, 35, 40, 65, 70)),
               init = init_het_reldur(),
               control = control_het_reldur(nsims=10, nsteps=52*60))
saveRDS(sim3, here("sims", "Oct2020", "NoCross_Offset_65.rds"))

##### 4. Offset + Edapprox (nets3) ####
m <- nets2[[1]]
newDur <- 413
pg <- (newDur - 1)/newDur
durCoef <- log(pg/(1 - pg))
crude <- m$coef.form.crude[1]
m$coef.form[1] <- crude - durCoef

nets3 <- list(m, nets2[[2]])

sim4 <- netsim(nets3, 
               param = param_het_reldur(olderpartner = TRUE, crossnetwork = FALSE, keep.completed = FALSE),
               init = init_het_reldur(),
               control = control_het_reldur(nsims=10, nsteps=52*60))
saveRDS(sim4, here("sims", "Oct2020", "NoCross_Offset_Edapprox.rds"))

#### 5. Offset + Edapprox + Departure Corrections (nets4) ####
sim5 <- netsim(nets4, 
              param = param_het_reldur(olderpartner = TRUE, crossnetwork = FALSE, keep.completed = FALSE),
              init = init_het_reldur(),
              control = control_het_reldur(nsims=10, nsteps=52*60))

saveRDS(sim5, here("sims", "Oct2020", "NoCross_Offset_EdapproxMort_DefaultDebut.rds"))
##### 6. Offset + Edapprox + Departure Corrections (nets4) BUT ELIGIBLE, NOT DEBUTED ####
sim6 <- netsim(nets4, 
               param = param_het_reldur(debut.entry.prob = .9, olderpartner = TRUE, crossnetwork = FALSE, keep.completed = FALSE),
               init = init_het_reldur(),
               control = control_het_reldur(nsims=10, nsteps=52*60))
saveRDS(sim6, here("sims", "Oct2020", "NoCross_Offset_EdapproxMort_EffectiveDebut15.rds"))

##### 7. Offset + Edapprox + Departure Corrections + nodefactor boost (nets5) ####
# make the edapprox and mortality adjustments 
# adjust marcoh formation coef 
m <- nets5[[1]]
newDur <- 413
pg <- (newDur - 1)/newDur
durCoef <- log(pg/(1 - pg))
crude <- m$coef.form.crude[1]
m$coef.form[1] <- crude - durCoef

n2 <- list(m, nets5[[2]])

#for reference: standard d.rate=1-0.9993355
marRate <- 1-0.999251
casRate <- 1-0.9997193

#marriage
pg <- (n2[[1]]$coef.diss$duration - 1)/n2[[1]]$coef.diss$duration
ps2 <- (1 - marRate)^2

n2[[1]]$coef.diss$d.rate <- marRate
n2[[1]]$coef.diss$coef.adj <- log(pg/(ps2 - pg))

#casual
pg <- (n2[[2]]$coef.diss$duration - 1)/n2[[2]]$coef.diss$duration
ps2 <- (1 - casRate)^2

n2[[2]]$coef.diss$d.rate <- casRate
n2[[2]]$coef.diss$coef.adj <- log(pg/(ps2 - pg))

nf <- n2

nf[[1]]$coef.form[[5]] <- n2[[1]]$coef.form[[5]]*18
nf[[2]]$coef.form[[5]] <- 3

sim7 <- netsim(nf, 
               param = param_het_reldur(olderpartner = TRUE, crossnetwork = FALSE, keep.completed = FALSE, young_boost = TRUE),
               init = init_het_reldur(),
               control = control_het_reldur(nsims=10, nsteps=52*60))
saveRDS(sim7, here("sims", "Oct2020", "NoCross_Offset_EdapproxMort_youngboost.rds"))
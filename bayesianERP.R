setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Google Drive/Doctorado Social Neuroscience/Exp 1 - Social Conformity/Results/EEG Data/ERP2022/Bayesian ERP")


library(scales)
library(effects)
library(ggplot2)
library(tidyr)
library(brms)
library(bayestestR)
library(rstan)
library(spatstat.utils)
library(pushoverr)


finaldb <- read.csv('finaldb.csv', sep=',', header=T)

finaldb2 <- gather(finaldb, electrode, signal, C_225_Fz:C_500_Pz, factor_key=TRUE)

names<-c('AbsSuj','Block','TrialRep','ID','Trial','Lead','Adjustment')
finaldb2[,names] <- lapply(finaldb2[,names],factor)

#############SINGLE PREDICTOR MODEL SPECIFICATION

bprior <- c(prior(normal(0,1), class='Intercept'), prior(normal(0,1) , class='b'), prior(gamma(2,2), class='sd'))

# ADJUSTMENT in 1st TRIALREP
finaldb3 <- finaldb2[finaldb2$TrialRep == "0",]

adjust <- brm(signal ~ Adjustment + (1|ID/AbsSuj) + (1+Adjustment||electrode), family=student, data=finaldb3,
                prior=bprior, chains=4, cores=4, iter=10000, warmup=1000,
                inits='0', control=list(adapt_delta=0.9, max_treedepth = 10))

pos <- as_draws(adjust)

saveRDS(adjust, file = "adjustment_v1.rds")

#TRIAL REPETITION 

trialrep <- brm(signal ~ TrialRep + (1|ID/AbsSuj) + (1+TrialRep||electrode), family=student, data=finaldb2,
                prior=bprior, chains=4, cores=4, iter=10000, warmup=1000,
                inits='0', control=list(adapt_delta=0.9, max_treedepth = 10))

saveRDS(trialrep, file = "trialrep_v1.rds")


#############LEAD
bprior <- set_prior("normal(muH1,sigmaH1)", class="b", coef = 'TrialRep1') +
  set_prior("normal(muH1,sigmaH1)", class="b", coef = 'TrialRep2') +
  set_prior("normal(muH2,sigmaH2)", class="b", coef = 'Lead2:TrialRep1') +
  set_prior("normal(muH2,sigmaH2)", class="b", coef = 'Lead2:TrialRep2') +
  set_prior("target+=normal_lpdf(muH1|0,1)", check=F) +
  set_prior("target+=normal_lpdf(muH2|0,1)", check=F) +
  set_prior("target+=gamma_lpdf(sigmaH1|2,2)", check=F) +
  set_prior("target+=gamma_lpdf(sigmaH2|2,2)", check=F) +
  set_prior("normal(0,1)", class="Intercept") + 
  set_prior("gamma(2,2)", class="sd")


stanvars <- stanvar(scode = "real muH1;", block = "parameters") +
  stanvar(scode = "real muH2;", block = "parameters") +
  stanvar(scode = "real<lower=0> sigmaH1;", block = "parameters")+
  stanvar(scode = "real<lower=0> sigmaH2;", block = "parameters")

lead <- brm(signal ~ Lead*TrialRep + (1|ID/AbsSuj) + (1+Lead*TrialRep||electrode), family=student, data=finaldb2,
            prior=bprior, chains=4, cores=4, iter=10000, warmup=1000, stanvars = stanvars,
            inits='0', control=list(adapt_delta=0.9, max_treedepth = 10))

saveRDS(lead, file = "lead_v1.rds")

levels(finaldb2$Lead) <- c("2","1")

follower <- brm(signal ~ Lead*TrialRep + (1|ID/AbsSuj) + (1+Lead*TrialRep||electrode), family=student, data=finaldb2,
                prior=bprior, chains=4, cores=4, iter=10000, warmup=1000, stanvars = stanvars,
                inits='0', control=list(adapt_delta=0.9, max_treedepth = 10))

saveRDS(follower, file = "follower_v1.rds")


##########INTRA DIFERENCE

bprior <- set_prior("normal(muH1,sigmaH1)", class="b", coef = 'TrialRep1') +
  set_prior("normal(muH1,sigmaH1)", class="b", coef = 'TrialRep2') +
  set_prior("normal(muH2,sigmaH2)", class="b", coef = 'intra_scale:TrialRep1') +
  set_prior("normal(muH2,sigmaH2)", class="b", coef = 'intra_scale:TrialRep2') +
  set_prior("target+=normal_lpdf(muH1|0,1)", check=F) +
  set_prior("target+=normal_lpdf(muH2|0,1)", check=F) +
  set_prior("target+=gamma_lpdf(sigmaH1|2,2)", check=F) +
  set_prior("target+=gamma_lpdf(sigmaH2|2,2)", check=F) +
  set_prior("normal(0,1)", class="Intercept") + 
  set_prior("gamma(2,2)", class="sd")

intradif <- brm(signal ~ intra_scale*TrialRep + (1|ID/AbsSuj) + (1+intra_scale*TrialRep||electrode), family=student, data=finaldb2,
                prior=bprior, chains=4, cores=4, iter=10000, warmup=1000, stanvars = stanvars,
                inits='0', control=list(adapt_delta=0.9, max_treedepth = 10))

saveRDS(intradif, file = "intradif_v1.rds")

#################INTER DIFERENCE
bprior <- set_prior("normal(muH1,sigmaH1)", class="b", coef = 'TrialRep1') +
  set_prior("normal(muH1,sigmaH1)", class="b", coef = 'TrialRep2') +
  set_prior("normal(muH2,sigmaH2)", class="b", coef = 'div_scale:TrialRep1') +
  set_prior("normal(muH2,sigmaH2)", class="b", coef = 'div_scale:TrialRep2') +
  set_prior("target+=normal_lpdf(muH1|0,1)", check=F) +
  set_prior("target+=normal_lpdf(muH2|0,1)", check=F) +
  set_prior("target+=gamma_lpdf(sigmaH1|2,2)", check=F) +
  set_prior("target+=gamma_lpdf(sigmaH2|2,2)", check=F) +
  set_prior("normal(0,1)", class="Intercept") + 
  set_prior("gamma(2,2)", class="sd")


inter_resc <- brm(signal ~ div_scale*TrialRep + (1|ID/AbsSuj) + (1+div_scale*TrialRep||electrode), family=student, data=finaldb2,
                  prior=bprior, chains=4, cores=4, iter=10000, warmup=1000, stanvars = stanvars,
                  inits='0', control=list(adapt_delta=0.9, max_treedepth = 10))

saveRDS(inter_resc, file = "inter_resc_v1.rds")


pushover('Stats Processed', user = "u2yje7zfuhi3rzs4zwpkrostp6wrdj", app = "a36jriy3tey1gddfh1yzetjjy2bhxy")    



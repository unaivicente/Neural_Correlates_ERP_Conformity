setwd("~/Google Drive/Doctorado Social Neuroscience/Exp 1 - Social Conformity/Results/EEG Data/New Julio2020/ERP Método Alberto")


library(scales)
library(effects)
library(ggplot2)
library(tidyr)
library(brms)
library(bayestestR)
library(rstan)
library(spatstat.utils)
library(pushoverr)


finaldb <- read.csv('finaldb_v1.csv', sep=',', header=T)

finaldb2 <- gather(finaldb, electrodo2, signal, C_230_Fz:C_500_Pz, factor_key=TRUE)

names<-c('AbsSuj','Block','TrialRep','Trial','Lead')
finaldb2[,names] <- lapply(finaldb2[,names],factor)

#bprior <- c(prior(normal(0,1), class='Intercept'), prior(normal(0,1) , class='b'),
 #           prior(gamma(2,2), class='sd'))

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

#TRIAL REPETITION 

trialrep <- brm(signal ~ TrialRep + (1|ID/AbsSuj) + (1+TrialRep||electrodo2), family=student, data=finaldb2,
                prior=bprior, chains=4, cores=4, iter=10000, warmup=1000, stanvars = stanvars,
                inits='0', control=list(adapt_delta=0.9, max_treedepth = 10))

saveRDS(trialrep, file = "trialrep_v2.rds")


#############LEAD

lead <- brm(signal ~ Lead*TrialRep + (1|ID/AbsSuj) + (1+Lead*TrialRep||electrodo2), family=student, data=finaldb2,
            prior=bprior, chains=4, cores=4, iter=10000, warmup=1000, stanvars = stanvars,
            inits='0', control=list(adapt_delta=0.9, max_treedepth = 10))

saveRDS(lead, file = "lead_v2.rds")

levels(finaldb2$Lead) <- c("2","1")

follower <- brm(signal ~ Lead*TrialRep + (1|ID/AbsSuj) + (1+Lead*TrialRep||electrodo2), family=student, data=finaldb2,
                prior=bprior, chains=4, cores=4, iter=10000, warmup=1000, stanvars = stanvars,
                inits='0', control=list(adapt_delta=0.9, max_treedepth = 10))

saveRDS(follower, file = "follower_v2.rds")


##########INTRA DIFERENCE

intradif <- brm(signal ~ Intra_dif_abs*TrialRep + (1|ID/AbsSuj) + (1+Intra_dif_abs*TrialRep||electrodo2), family=student, data=finaldb2,
                prior=bprior, chains=4, cores=4, iter=10000, warmup=1000, stanvars = stanvars,
                inits='0', control=list(adapt_delta=0.9, max_treedepth = 10))

saveRDS(intradif, file = "intradif_v2.rds")

#################INTER DIFERENCE

inter_resc <- brm(signal ~ Inter_div_abs*TrialRep + (1|ID/AbsSuj) + (1+Intra_div_abs*TrialRep||electrodo2), family=student, data=finaldb2,
                  prior=bprior, chains=4, cores=4, iter=10000, warmup=1000, stanvars = stanvars,
                  inits='0', control=list(adapt_delta=0.9, max_treedepth = 10))

saveRDS(inter_resc, file = "inter_resc_v2.rds")


################# WINNER

winner <- brm(signal ~ winner*TrialRep + (1|ID/AbsSuj) + (1+winner*TrialRep||electrodo2), family=student, data=finaldb2,
              prior=bprior, chains=4, cores=4, iter=10000, warmup=1000, stanvars = stanvars,
              inits='0', control=list(adapt_delta=0.9, max_treedepth = 10))

saveRDS(winner, file = "winner_v2.rds")

set_pushover_user(user = "u2yje7zfuhi3rzs4zwpkrostp6wrdj")
set_pushover_app(token = "a36jriy3tey1gddfh1yzetjjy2bhxy")

pushover(message="Processing for ERP new Analysis is ready, go fetch it!")



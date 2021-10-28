setwd("~/UNAI/ERP_Alberto/")

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

finaldb2 <- gather(finaldb, electrodo2, signal, C_230_Fz:C_500_Pz, factor_key=TRUE)

bprior <- c(prior(normal(0,1), class='Intercept'), prior(normal(0,1) , class='b'),
            prior(gamma(2,2), class='sd'))

#TRIAL REPETITION 

trialrep <- brm(signal ~ TrialRep + (1|ID:AbsSuj) + (1+TrialRep||electrodo2), family=student, data=finaldb2,
                prior=bprior, chains=4, cores=4, iter=4000, warmup=800,
                inits='0', control=list(adapt_delta=0.9, max_treedepth = 10))


trialrep <- bayestestR::hdi(TrialRep, ci = 0.95)
trialrep$model <- "TrialRep"
saveRDS(trialrep, file = "trialrep.rds")

for (i in 1:nrow(trialrep)){
  r1 <- c(trialrep$CI_low[i],trialrep$CI_high[i])
  trialrep$Significance[i] <- inside.range(0, r1)
}


##########INTRA DIFERENCE

intradif <- brm(signal ~ Intra_dif_abs + (1|ID:AbsSuj) + (1+TrialRep||electrodo2), family=student, data=finaldb2,
                prior=bprior, chains=4, cores=4, iter=4000, warmup=800,
                inits='0', control=list(adapt_delta=0.9, max_treedepth = 10))

intradif <- bayestestR::hdi(intradif, ci = 0.95)
intradif$model <- "intradif"
saveRDS(intradif, file = "intradif.rds")

for (i in 1:nrow(intradif)){
  r1 <- c(intradif$CI_low[i],intradif$CI_high[i])
  intradif$Significance[i] <- inside.range(0, r1)
}

#################INTER DIFERENCE

inter_resc <- brm(signal ~ Inter_div_abs + (1|ID:AbsSuj) + (1+TrialRep||electrodo2), family=student, data=finaldb2,
                  prior=bprior, chains=4, cores=4, iter=4000, warmup=800,
                  inits='0', control=list(adapt_delta=0.9, max_treedepth = 10))

inter_resc <- bayestestR::hdi(inter_resc, ci = 0.95)
inter_resc$model <- "inter_resc"
saveRDS(inter_resc, file = "inter_resc.rds")

for (i in 1:nrow(inter_resc)){
  r1 <- c(inter_resc$CI_low[i],inter_resc$CI_high[i])
  inter_resc$Significance[i] <- inside.range(0, r1)
}


################# WINNER

winner <- brm(signal ~ winner + (1|ID:AbsSuj) + (1+TrialRep||electrodo2), family=student, data=finaldb2,
              prior=bprior, chains=4, cores=4, iter=4000, warmup=800,
              inits='0', control=list(adapt_delta=0.9, max_treedepth = 10))
saveRDS(winner, file = "winner.rds")

winner <- bayestestR::hdi(winner, ci = 0.95)
winner$model <- "winner"


for (i in 1:nrow(winner)){
  r1 <- c(winner$CI_low[i],winner$CI_high[i])
  winner$Significance[i] <- inside.range(0, r1)
}

#############LEAD
lead <- brm(signal ~ Lead + (1|ID:AbsSuj) + (1+TrialRep||electrodo2), family=student, data=finaldb2,
            prior=bprior, chains=4, cores=4, iter=4000, warmup=800,
            inits='0', control=list(adapt_delta=0.9, max_treedepth = 10))

saveRDS(lead, file = "lead.rds")
lead <- bayestestR::hdi(lead, ci = 0.95)
lead$model <- "lead"


for (i in 1:nrow(lead)){
  r1 <- c(lead$CI_low[i],lead$CI_high[i])
  lead$Significance[i] <- inside.range(0, r1)
}


#########

allsims  <- rbind(trialrep, intradif, interdif, winner, lead)

#########

df <- read.csv('finaldb.csv')

prior <- c(prior(normal(0,1), class='Intercept'), prior(normal(0,1) , class='b'),
           prior(gamma(2,2), class='sd'))

modeltheta <- brm(timefreq4_8 ~ TrialRep + (1|ID:AbsSuj) + (1+TrialRep||electrodo2), family=student, data=finaldb2,
                  prior=bprior, chains=4, cores=4, iter=4000, warmup=800,
                  inits='0', control=list(adapt_delta=0.9, max_treedepth = 10))

h1 <- equivalence_test(modeltheta, ci=(.95), range=c(-.01,.01))
saveRDS(modeltheta, file = "modeltheta.rds")

modelalpha <- brm(timefreq8_12 ~ TrialRep + (1|ID:AbsSuj) + (1+TrialRep||electrodo2), family=student, data=finaldb2,
                  prior=bprior, chains=4, cores=4, iter=4000, warmup=800,
                  inits='0', control=list(adapt_delta=0.9, max_treedepth = 10))

h2 <- equivalence_test(modelalpha, ci=(.95), range=c(-.01,.01))
saveRDS(modelalpha, file = "modelalpha.rds")

modelbeta <- brm(timefreq12_30 ~ TrialRep + (1+TrialRep||electrodo2), family=student, data=finaldb2,
                 prior=bprior, chains=4, cores=4, iter=4000, warmup=800,
                 inits='0', control=list(adapt_delta=0.9, max_treedepth = 10))

h3 <- equivalence_test(modelbeta, ci=(.95), range=c(-.01,.01))
saveRDS(modelbeta, file = "modelbeta.rds")

#########




set_pushover_user(user = "u2yje7zfuhi3rzs4zwpkrostp6wrdj")
set_pushover_app(token = "a36jriy3tey1gddfh1yzetjjy2bhxy")

pushover(message="Time Frequency bayesian analysis método de Alberto finished, go fetch it!")

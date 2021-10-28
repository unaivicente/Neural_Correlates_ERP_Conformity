setwd("~/Google Drive/Doctorado Social Neuroscience/Exp 1 - Social Conformity/Results/EEG Data/New Julio2020/TimeFrequency")


library(brms)
library(bayestestR)
library(tidyr)
library(pushoverr)

df <- read.csv('finaldb_tf_v4.csv')

names<-c('AbsSuj','TrialRep','Trial')
df[,names] <- lapply(df[,names],factor)

prior <- c(prior(normal(0,1), class='Intercept'), prior(normal(0,1) , class='b'),
           prior(gamma(2,2), class='sd'))



modeltheta <- brm(timefreq4_8 ~ TrialRep + (1|ID/AbsSuj) + (1+TrialRep||electrodo), family=Gamma(link="log"), data=df,
                  prior=prior, chains=4, cores=4, iter=8000, warmup=1000,
                  inits='0', control=list(adapt_delta=0.90, max_treedepth = 10),
                  silent=F, refresh=0, open_progress=F)

modelalpha <- brm(timefreq8_12 ~ TrialRep + (1|ID/AbsSuj) + (1+TrialRep||electrodo), family=Gamma(link="log"), data=df,
                  prior=prior, chains=4, cores=4, iter=8000, warmup=1000,
                  inits='0', control=list(adapt_delta=0.90, max_treedepth = 10),
                  silent=F, refresh=0, open_progress=F)

modelbeta <- brm(timefreq12_30 ~ TrialRep + (1|ID/AbsSuj) + (1+TrialRep||electrodo), family=Gamma(link="log"), data=df,
                 prior=prior, chains=4, cores=4, iter=8000, warmup=1000,
                 inits='0', control=list(adapt_delta=0.90, max_treedepth = 10),
                 silent=F, refresh=0, open_progress=F)

saveRDS(modeltheta, file = "modeltheta_v11.rds")
saveRDS(modelalpha, file = "modelalpha_v11.rds")
saveRDS(modelbeta, file = "modelbeta_v11.rds")


set_pushover_user(user = "u2yje7zfuhi3rzs4zwpkrostp6wrdj")
set_pushover_app(token = "a36jriy3tey1gddfh1yzetjjy2bhxy")

pushover(message="Processing for TimeFreqs in LABPC is ready, go fetch it!")

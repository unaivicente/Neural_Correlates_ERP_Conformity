

library(brms)
library(bayestestR)
library(tidyr)
library(pushoverr)

df <- read.csv('finaldb_tf_v2.csv')

names<-c('AbsSuj','TrialRep','Trial')
df[,names] <- lapply(df[,names],factor)

prior <- c(prior(normal(0,1), class='Intercept'), prior(normal(0,1) , class='b'),
           prior(gamma(2,2), class='sd'))



modeltheta180 <- brm(timefreq4_8_180 ~ TrialRep + (1|ID/AbsSuj) + (1+TrialRep||electrodo), family=Gamma(link="log"), data=df,
                  prior=prior, chains=4, cores=4, iter=10000, warmup=1000,
                  inits='0', control=list(adapt_delta=0.90, max_treedepth = 10),
                  silent=F, refresh=0, open_progress=F)

modeltheta230 <- brm(timefreq4_8_230 ~ TrialRep + (1|ID/AbsSuj) + (1+TrialRep||electrodo), family=Gamma(link="log"), data=df,
                  prior=prior, chains=4, cores=4, iter=10000, warmup=1000,
                  inits='0', control=list(adapt_delta=0.90, max_treedepth = 10),
                  silent=F, refresh=0, open_progress=F)


saveRDS(modeltheta180, file = "modeltheta180.rds")
saveRDS(modeltheta230, file = "modeltheta230.rds")


set_pushover_user(user = "u2yje7zfuhi3rzs4zwpkrostp6wrdj")
set_pushover_app(token = "a36jriy3tey1gddfh1yzetjjy2bhxy")

pushover(message="Processing for TimeFreqs THETA is ready, go fetch it!")

setwd("~/Google Drive/Doctorado Social Neuroscience/Exp 1 - Social Conformity/Results/EEG Data/New Julio2020/TimeFrequency")


library(brms)
library(bayestestR)
library(tidyr)
library(pushoverr)

df <- read.csv('predictors_tf.csv')


prior <- c(prior(normal(0,1), class='Intercept'), prior(normal(0,1) , class='b'),
           prior(gamma(2,2), class='sd'))

#Lead
df$Lead <- as.factor(df$Lead, levels = c("1","2"))


modeltheta1 <- brm(timefreq4_8 ~ Lead*TrialRep + (1|ID/AbsSuj) + (1+Lead*TrialRep||electrodo), family=Gamma(link="log"), data=df,
                   prior=prior, chains=4, cores=4, iter=8000, warmup=1000,
                   inits='0', control=list(adapt_delta=0.90, max_treedepth = 10),
                   silent=F, refresh=0, open_progress=F)

modelalpha1 <- brm(timefreq8_12 ~ Lead*TrialRep + (1|ID/AbsSuj) + (1+Lead*TrialRep||electrodo), family=Gamma(link="log"), data=df,
                   prior=prior, chains=4, cores=4, iter=8000, warmup=1000,
                   inits='0', control=list(adapt_delta=0.90, max_treedepth = 10),
                   silent=F, refresh=0, open_progress=F)

modelbeta1 <- brm(timefreq12_30 ~ Lead*TrialRep + (1|ID/AbsSuj) + (1+Lead*TrialRep||electrodo), family=Gamma(link="log"), data=df,
                  prior=prior, chains=4, cores=4, iter=8000, warmup=1000,
                  inits='0', control=list(adapt_delta=0.90, max_treedepth = 10),
                  silent=F, refresh=0, open_progress=F)


saveRDS(modeltheta1, file = "modeltheta_lead.rds")
saveRDS(modelalpha1, file = "modelalpha_lead.rds")
saveRDS(modelbeta1, file = "modelbeta_lead.rds")

#Follower
df$Lead <- as.factor(df$Lead, levels = c("2","1"))

modeltheta2 <- brm(timefreq4_8 ~ Lead*TrialRep + (1|ID/AbsSuj) + (1+Lead*TrialRep||electrodo), family=Gamma(link="log"), data=df,
                   prior=prior, chains=4, cores=4, iter=8000, warmup=1000,
                   inits='0', control=list(adapt_delta=0.90, max_treedepth = 10),
                   silent=F, refresh=0, open_progress=F)

modelalpha2 <- brm(timefreq8_12 ~ Lead*TrialRep + (1|ID/AbsSuj) + (1+Lead*TrialRep||electrodo), family=Gamma(link="log"), data=df,
                   prior=prior, chains=4, cores=4, iter=8000, warmup=1000,
                   inits='0', control=list(adapt_delta=0.90, max_treedepth = 10),
                   silent=F, refresh=0, open_progress=F)

modelbeta2 <- brm(timefreq12_30 ~ Lead*TrialRep + (1|ID/AbsSuj) + (1+Lead*TrialRep||electrodo), family=Gamma(link="log"), data=df,
                  prior=prior, chains=4, cores=4, iter=8000, warmup=1000,
                  inits='0', control=list(adapt_delta=0.90, max_treedepth = 10),
                  silent=F, refresh=0, open_progress=F)


saveRDS(modeltheta2, file = "modeltheta_follow.rds")
saveRDS(modelalpha2, file = "modelalpha_follow.rds")
saveRDS(modelbeta2, file = "modelbeta_follow.rds")

#IntraDif

modeltheta3 <- brm(timefreq4_8 ~ Intra_dif_abs*TrialRep + (1|ID/AbsSuj) + (1+Intra_dif_abs*TrialRep||electrodo), family=Gamma(link="log"), data=df,
                  prior=prior, chains=4, cores=4, iter=8000, warmup=1000,
                  inits='0', control=list(adapt_delta=0.90, max_treedepth = 10),
                  silent=F, refresh=0, open_progress=F)

modelalpha3 <- brm(timefreq8_12 ~ Intra_dif_abs*TrialRep + (1|ID/AbsSuj) + (1+Intra_dif_abs*TrialRep||electrodo), family=Gamma(link="log"), data=df,
                  prior=prior, chains=4, cores=4, iter=8000, warmup=1000,
                  inits='0', control=list(adapt_delta=0.90, max_treedepth = 10),
                  silent=F, refresh=0, open_progress=F)

modelbeta3 <- brm(timefreq12_30 ~ Intra_dif_abs*TrialRep + (1|ID/AbsSuj) + (1+Intra_dif_abs*TrialRep||electrodo), family=Gamma(link="log"), data=df,
                 prior=prior, chains=4, cores=4, iter=8000, warmup=1000,
                 inits='0', control=list(adapt_delta=0.90, max_treedepth = 10),
                 silent=F, refresh=0, open_progress=F)

saveRDS(modeltheta3, file = "modeltheta_intradif.rds")
saveRDS(modelalpha3, file = "modelalpha_intradif.rds")
saveRDS(modelbeta3, file = "modelbeta_intradif.rds")

#InterDif

modeltheta4 <- brm(timefreq4_8 ~ Inter_div_abs*TrialRep + (1|ID/AbsSuj) + (1+Inter_div_abs*TrialRep||electrodo), family=Gamma(link="log"), data=df,
                  prior=prior, chains=4, cores=4, iter=8000, warmup=1000,
                  inits='0', control=list(adapt_delta=0.90, max_treedepth = 10),
                  silent=F, refresh=0, open_progress=F)

modelalpha4 <- brm(timefreq8_12 ~ Inter_div_abs*TrialRep + (1|ID/AbsSuj) + (1+Inter_div_abs*TrialRep||electrodo), family=Gamma(link="log"), data=df,
                  prior=prior, chains=4, cores=4, iter=8000, warmup=1000,
                  inits='0', control=list(adapt_delta=0.90, max_treedepth = 10),
                  silent=F, refresh=0, open_progress=F)

modelbeta4 <- brm(timefreq12_30 ~ Inter_div_abs*TrialRep + (1|ID/AbsSuj) + (1+Inter_div_abs*TrialRep||electrodo), family=Gamma(link="log"), data=df,
                 prior=prior, chains=4, cores=4, iter=8000, warmup=1000,
                 inits='0', control=list(adapt_delta=0.90, max_treedepth = 10),
                 silent=F, refresh=0, open_progress=F)

saveRDS(modeltheta4, file = "modeltheta_interdif.rds")
saveRDS(modelalpha4, file = "modelalpha_interdif.rds")
saveRDS(modelbeta4, file = "modelbeta_interdif.rds")

#Winner

modeltheta5 <- brm(timefreq4_8 ~ winner*TrialRep + (1|ID/AbsSuj) + (1+winner*TrialRep||electrodo), family=Gamma(link="log"), data=df,
                   prior=prior, chains=4, cores=4, iter=8000, warmup=1000,
                   inits='0', control=list(adapt_delta=0.90, max_treedepth = 10),
                   silent=F, refresh=0, open_progress=F)

modelalpha5 <- brm(timefreq8_12 ~ winner*TrialRep + (1|ID/AbsSuj) + (1+winner*TrialRep||electrodo), family=Gamma(link="log"), data=df,
                   prior=prior, chains=4, cores=4, iter=8000, warmup=1000,
                   inits='0', control=list(adapt_delta=0.90, max_treedepth = 10),
                   silent=F, refresh=0, open_progress=F)

modelbeta5 <- brm(timefreq12_30 ~ winner*TrialRep + (1|ID/AbsSuj) + (1+winner*TrialRep||electrodo), family=Gamma(link="log"), data=df,
                  prior=prior, chains=4, cores=4, iter=8000, warmup=1000,
                  inits='0', control=list(adapt_delta=0.90, max_treedepth = 10),
                  silent=F, refresh=0, open_progress=F)

saveRDS(modeltheta5, file = "modeltheta_win.rds")
saveRDS(modelalpha5, file = "modelalpha_win.rds")
saveRDS(modelbeta5, file = "modelbeta_win.rds")


set_pushover_user(user = "u2yje7zfuhi3rzs4zwpkrostp6wrdj")
set_pushover_app(token = "a36jriy3tey1gddfh1yzetjjy2bhxy")

pushover(message="Processing for TimeFreq Predictor Analysis is ready, go fetch it!")




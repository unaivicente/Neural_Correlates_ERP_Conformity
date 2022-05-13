#setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Google Drive/Doctorado Social Neuroscience/Exp 1 - Social Conformity/Results/EEG Data/ERP2022/Bayesian ERP/")
setwd("~/Desktop/Analysis/")


library(brms)
library(bayestestR)
library(tidyr)


trialrep <- readRDS('trialrep_v1.rds')

posterior_data <- as_draws(trialrep)
posterior_data <- do.call(rbind.data.frame, posterior_data)

time <- c('225', '275','350','500')
elec <- c('Fz','Cz','Pz')

df = expand.grid(a = time, b = elec)
output <- data.frame()

for (i in 1:nrow(df)){
  
  b0 <- posterior_data[['b_Intercept']] + posterior_data[[sprintf('r_electrodo2.C_%s_%s.Intercept.',df$a[i],df$b[i])]]
  b1 <- posterior_data[['b_TrialRep1']] + posterior_data[[sprintf('r_electrodo2.C_%s_%s.TrialRep1.',df$a[i],df$b[i])]]
  b2 <- posterior_data[['b_TrialRep2']] + posterior_data[[sprintf('r_electrodo2.C_%s_%s.TrialRep2.',df$a[i],df$b[i])]]
  
  var_hyp1 <- -b1
  var_hyp2 <- -b2
  var_hyp3 <- b1-b2
  
  hyp1test <- equivalence_test(var_hyp1, ci=(.95), range=c(-.01,.01))
  hyp2test <- equivalence_test(var_hyp2, ci=(.95), range=c(-.01,.01))
  hyp3test <- equivalence_test(var_hyp3, ci=(.95), range=c(-.01,.01))
  
  
  hyp1test$Hypothesys <- "TrialRep 1"
  hyp1test$Elec <- sprintf('%s_%s',df$a[i],df$b[i])
  hyp2test$Hypothesys <- "TrialRep 2"
  hyp2test$Elec <- sprintf('%s_%s',df$a[i],df$b[i])
  hyp3test$Hypothesys <- "Difference"
  hyp3test$Elec <- sprintf('%s_%s',df$a[i],df$b[i])
  
  dat <- rbind(hyp1test, hyp2test, hyp3test)
  output <- rbind(dat, output)
}

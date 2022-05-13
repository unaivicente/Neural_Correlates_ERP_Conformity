#setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Google Drive/Doctorado Social Neuroscience/Exp 1 - Social Conformity/Results/EEG Data/ERP2022/Bayesian ERP/")
setwd("~/Desktop/Analysis/")


library(brms)
library(bayestestR)
library(tidyr)


inter <- readRDS('inter_resc_v1.rds') #lead as intercept

posterior_data <- as_draws(inter)
posterior_data <- do.call(rbind.data.frame, posterior_data)

time <- c('225', '275','350','500')
elec <- c('Fz','Cz','Pz')

df = expand.grid(a = time, b = elec)

output <- data.frame()

for (i in 1:nrow(df)){

  b0 <- posterior_data[['b_Intercept']] + posterior_data[[sprintf('r_electrode.C_%s_%s.Intercept.',df$a[i],df$b[i])]]
  b1 <- posterior_data[['b_div_scale']] + posterior_data[[sprintf('r_electrode.C_%s_%s.div_scale.',df$a[i],df$b[i])]]
  b2 <- posterior_data[['b_TrialRep1']] + posterior_data[[sprintf('r_electrode.C_%s_%s.TrialRep1.',df$a[i],df$b[i])]]
  b3 <- posterior_data[['b_TrialRep2']] + posterior_data[[sprintf('r_electrode.C_%s_%s.TrialRep2.',df$a[i],df$b[i])]]
  b4 <- posterior_data[['b_div_scale.TrialRep1']]+posterior_data[[sprintf('r_electrode.C_%s_%s.div_scale.TrialRep1.',df$a[i],df$b[i])]]
  b5 <- posterior_data[['b_div_scale.TrialRep2']]+posterior_data[[sprintf('r_electrode.C_%s_%s.div_scale.TrialRep2.',df$a[i],df$b[i])]]
  

  var_hyp1 <- -b2 #"TrialRep 1 over Intercept"
  var_hyp2 <- -b3 #"TrialRep 2 over Intercept"
  var_hyp3 <- b2+b4-b3-b5 #"Difference 2 over 1"
  
  hyp1test <- equivalence_test(var_hyp1, ci=(.95), range=c(-.01,.01))
  hyp2test <- equivalence_test(var_hyp2, ci=(.95), range=c(-.01,.01))
  hyp3test <- equivalence_test(var_hyp3, ci=(.95), range=c(-.01,.01))
  
  hyp1test$Hypothesys <- "Inter: TrialRep 1 over Intercept"
  hyp1test$ElecTime <- sprintf('%s_%s',df$a[i],df$b[i])
  hyp2test$Hypothesys <- "Inter: TrialRep 2 over Intercept"
  hyp2test$ElecTime <- sprintf('%s_%s',df$a[i],df$b[i])
  hyp3test$Hypothesys <- "Inter: Difference 2 over 1"
  hyp3test$ElecTime <- sprintf('%s_%s',df$a[i],df$b[i])
  
  
  dat <- rbind(hyp1test, hyp2test, hyp3test)
  output <- rbind(dat, output)

}



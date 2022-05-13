#setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Google Drive/Doctorado Social Neuroscience/Exp 1 - Social Conformity/Results/EEG Data/ERP2022/Bayesian ERP/")
setwd("~/Desktop/Analysis/")


library(brms)
library(bayestestR)
library(tidyr)


adjstmnt <- readRDS('adjustment_v1.rds')

posterior_data <- as_draws(adjstmnt)
posterior_data <- do.call(rbind.data.frame, posterior_data)

time <- c('225', '275','350','500')
elec <- c('Fz','Cz','Pz')

df = expand.grid(a = time, b = elec)
output <- data.frame()

for (i in 1:nrow(df)){
  
  b0 <- posterior_data[['b_Intercept']] + posterior_data[[sprintf('r_electrode.C_%s_%s.Intercept.',df$a[i],df$b[i])]]
  b1 <- posterior_data[['b_Intercept']] + posterior_data[['b_AdjustmentLow']] + posterior_data[[sprintf('r_electrode.C_%s_%s.Intercept.',df$a[i],df$b[i])]] + + posterior_data[[sprintf('r_electrode.C_%s_%s.AdjustmentLow.',df$a[i],df$b[i])]]

  var_hyp1 <- b0-b1

  
  hyp1test <- equivalence_test(var_hyp1, ci=(.95), range=c(-.01,.01))

  
  
  hyp1test$Hypothesys <- "Difference"
  hyp1test$Elec <- sprintf('%s_%s',df$a[i],df$b[i])

  
  output <- rbind(hyp1test, output)
}


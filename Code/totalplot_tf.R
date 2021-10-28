setwd("~/Google Drive/Doctorado Social Neuroscience/Exp 1 - Social Conformity/Results/EEG Data/New Julio2020/TimeFrequency")

library(tidyverse)
library(grid)
library(gridExtra)
library(cowplot)
library(ggplot2)
library(ggridges)
library(brms)
library(bayestestR)

modeltheta <- readRDS('modeltheta_lead_v2.rds')
modelalpha <- readRDS('modelalpha_lead_v2.rds')
modelbeta <- readRDS('modelbeta_lead_v2.rds')



freq <- c('theta', 'alpha', 'beta')
elec <- c('Fz','Cz','Pz')

df <- expand.grid(a = freq, b = elec)
df <- df[order(df$a),]
row.names(df) <- NULL

var_hyp0 <- list()
var_hyp1 <- list()
var_hyp2 <- list()
var_hyp3 <- list()

for (i in 1:nrow(df)){
  
  if (i <= 3){
  posterior_data <- posterior_samples(modeltheta)
  }else if ( i > 3 && i <= 6){
  posterior_data <- posterior_samples(modelalpha)
  }else{
  posterior_data <- posterior_samples(modelbeta)
  }
  
  b0 <- posterior_data[['b_Intercept']] + posterior_data[[sprintf('r_electrodo[%s,Intercept]', df$b[i])]]
  b1 <- posterior_data[['b_Lead2']] + posterior_data[[sprintf('r_electrodo[%s,Lead2]', df$b[i])]]
  b2 <- posterior_data[['b_TrialRep1']] + posterior_data[[sprintf('r_electrodo[%s,TrialRep1]', df$b[i])]]
  b3 <- posterior_data[['b_TrialRep2']] + posterior_data[[sprintf('r_electrodo[%s,TrialRep2]', df$b[i])]]
  b4 <- posterior_data[['b_Lead2:TrialRep1']]+posterior_data[[sprintf('r_electrodo[%s,Lead2:TrialRep1]', df$b[i])]]
  b5 <- posterior_data[['b_Lead2:TrialRep2']]+posterior_data[[sprintf('r_electrodo[%s,Lead2:TrialRep2]', df$b[i])]]
  
  
  var_hyp0[[i]] <- b1 #Differences (Lint-F) in Intercept
  var_hyp1[[i]] <- -b2 #LEADERs: TrialRep 1 over Intercept
  var_hyp2[[i]] <- -b2-b4 #FOLLOWERs: TrialRep 1 over Intercept
  var_hyp3[[i]] <- b4 #LEADERs-FOLLOWERs: TrialRep1 over Intercept

  
}

#Differences (Lint-F) in Intercept

#Theta
resultfztheta <- estimate_density(var_hyp0[[1]])
resultfztheta$Electrode <- rep("Fz", nrow(resultfztheta))

resultcztheta <- estimate_density(var_hyp0[[2]])
resultcztheta$Electrode <- rep("Cz", nrow(resultcztheta))

resultpztheta <- estimate_density(var_hyp0[[3]])
resultpztheta$Electrode <- rep("Pz", nrow(resultpztheta))

dframe1 <- rbind(resultfztheta,resultcztheta,resultpztheta)
densdf1 <- do.call(rbind, lapply(split(dframe1, dframe1$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))


#Alpha
resultfzalpha <- estimate_density(var_hyp0[[4]])
resultfzalpha$Electrode <- rep("Fz", nrow(resultfzalpha))

resultczalpha <- estimate_density(var_hyp0[[5]])
resultczalpha$Electrode <- rep("Cz", nrow(resultczalpha))

resultpzalpha <- estimate_density(var_hyp0[[6]])
resultpzalpha$Electrode <- rep("Pz", nrow(resultpzalpha))

dframe2 <- rbind(resultfzalpha,resultczalpha,resultpzalpha)
densdf2 <- do.call(rbind, lapply(split(dframe2, dframe2$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

#Beta
resultfzbeta <- estimate_density(var_hyp0[[7]])
resultfzbeta$Electrode <- rep("Fz", nrow(resultfzbeta))

resultczbeta <- estimate_density(var_hyp0[[8]])
resultczbeta$Electrode <- rep("Cz", nrow(resultczbeta))

resultpzbeta <- estimate_density(var_hyp0[[9]])
resultpzbeta$Electrode <- rep("Pz", nrow(resultpzbeta))

dframe3 <- rbind(resultfzbeta,resultczbeta,resultpzbeta)
densdf3 <- do.call(rbind, lapply(split(dframe3, dframe3$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

p1 <- ggplot(data = densdf1, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf1[densdf1$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-0.25, 0.25), y = c(0, 20)) +
  labs(title="THETA (300-500ms)", x="VALUES", y="DENSITY") +
  theme_bw() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title = element_text(size=14))

#Extract legend
leg1 <- get_legend(p1)
#Remove legend
p1 <- p1 + theme(legend.position = "none")

p2 <- ggplot(data = densdf2, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf2[densdf2$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-0.25, 0.25), y = c(0, 20)) +
  labs(title="ALPHA (180-230ms)", x="VALUES", y="DENSITY") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title = element_text(size=14))

p3 <- ggplot(data = densdf3, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf3[densdf3$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-0.25, 0.25), y = c(0, 20)) +
  labs(title="BETA (180-230ms)", x="VALUES", y="DENSITY") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title = element_text(size=14))

a0 <- grid.arrange(p1, p2, p3, nrow=3, top = textGrob("A. LEADER-FOLLOWER:(FB1)",gp=gpar(fontsize=18,font=2)))

#LEADERs: TrialRep 1 over Intercept
#Theta 
resultfztheta <- estimate_density(var_hyp1[[1]])
resultfztheta$Electrode <- rep("Fz", nrow(resultfztheta))

resultcztheta <- estimate_density(var_hyp1[[2]])
resultcztheta$Electrode <- rep("Cz", nrow(resultcztheta))

resultpztheta <- estimate_density(var_hyp1[[3]])
resultpztheta$Electrode <- rep("Pz", nrow(resultpztheta))

dframe1 <- rbind(resultfztheta,resultcztheta,resultpztheta)
densdf1 <- do.call(rbind, lapply(split(dframe1, dframe1$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

 
#Alpha
resultfzalpha <- estimate_density(var_hyp1[[4]])
resultfzalpha$Electrode <- rep("Fz", nrow(resultfzalpha))

resultczalpha <- estimate_density(var_hyp1[[5]])
resultczalpha$Electrode <- rep("Cz", nrow(resultczalpha))

resultpzalpha <- estimate_density(var_hyp1[[6]])
resultpzalpha$Electrode <- rep("Pz", nrow(resultpzalpha))

dframe2 <- rbind(resultfzalpha,resultczalpha,resultpzalpha)
densdf2 <- do.call(rbind, lapply(split(dframe2, dframe2$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

#Beta
resultfzbeta <- estimate_density(var_hyp1[[7]])
resultfzbeta$Electrode <- rep("Fz", nrow(resultfzbeta))

resultczbeta <- estimate_density(var_hyp1[[8]])
resultczbeta$Electrode <- rep("Cz", nrow(resultczbeta))

resultpzbeta <- estimate_density(var_hyp1[[9]])
resultpzbeta$Electrode <- rep("Pz", nrow(resultpzbeta))

dframe3 <- rbind(resultfzbeta,resultczbeta,resultpzbeta)
densdf3 <- do.call(rbind, lapply(split(dframe3, dframe3$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

p1 <- ggplot(data = densdf1, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf1[densdf1$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-0.25, 0.25), y = c(0, 20)) +
  labs(title="THETA (300-500ms)", x="VALUES", y="DENSITY") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title = element_text(size=14))

p2 <- ggplot(data = densdf2, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf2[densdf2$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-0.25, 0.25), y = c(0, 20)) +
  labs(title="ALPHA (180-230ms)", x="VALUES", y="DENSITY") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title = element_text(size=14))

p3 <- ggplot(data = densdf3, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf3[densdf3$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-0.25, 0.25), y = c(0, 20)) +
  labs(title="BETA (180-230ms)", x="VALUES", y="DENSITY") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title = element_text(size=14))

a1 <- grid.arrange(p1, p2, p3, nrow=3, top = textGrob("B. LEADER:(FB1-FB2)",gp=gpar(fontsize=18,font=2)))

#FOLLOWERs: TrialRep 1 over Intercept

#Theta
resultfztheta <- estimate_density(var_hyp2[[1]])
resultfztheta$Electrode <- rep("Fz", nrow(resultfztheta))

resultcztheta <- estimate_density(var_hyp2[[2]])
resultcztheta$Electrode <- rep("Cz", nrow(resultcztheta))

resultpztheta <- estimate_density(var_hyp2[[3]])
resultpztheta$Electrode <- rep("Pz", nrow(resultpztheta))

dframe1 <- rbind(resultfztheta,resultcztheta,resultpztheta)
densdf1 <- do.call(rbind, lapply(split(dframe1, dframe1$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))


#Alpha
resultfzalpha <- estimate_density(var_hyp2[[4]])
resultfzalpha$Electrode <- rep("Fz", nrow(resultfzalpha))

resultczalpha <- estimate_density(var_hyp2[[5]])
resultczalpha$Electrode <- rep("Cz", nrow(resultczalpha))

resultpzalpha <- estimate_density(var_hyp2[[6]])
resultpzalpha$Electrode <- rep("Pz", nrow(resultpzalpha))

dframe2 <- rbind(resultfzalpha,resultczalpha,resultpzalpha)
densdf2 <- do.call(rbind, lapply(split(dframe2, dframe2$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

#Beta
resultfzbeta <- estimate_density(var_hyp2[[7]])
resultfzbeta$Electrode <- rep("Fz", nrow(resultfzbeta))

resultczbeta <- estimate_density(var_hyp2[[8]])
resultczbeta$Electrode <- rep("Cz", nrow(resultczbeta))

resultpzbeta <- estimate_density(var_hyp2[[9]])
resultpzbeta$Electrode <- rep("Pz", nrow(resultpzbeta))

dframe3 <- rbind(resultfzbeta,resultczbeta,resultpzbeta)
densdf3 <- do.call(rbind, lapply(split(dframe3, dframe3$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

p1 <- ggplot(data = densdf1, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf1[densdf1$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-0.25, 0.25), y = c(0, 20)) +
  labs(title="THETA (300-500ms)", x="VALUES", y="DENSITY") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title = element_text(size=14))

p2 <- ggplot(data = densdf2, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf2[densdf2$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-0.25, 0.25), y = c(0, 20)) +
  labs(title="ALPHA (180-230ms)", x="VALUES", y="DENSITY") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title = element_text(size=14))

p3 <- ggplot(data = densdf3, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf3[densdf3$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-0.25, 0.25), y = c(0, 20)) +
  labs(title="BETA (180-230ms)", x="VALUES", y="DENSITY") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title = element_text(size=14))

a2 <- grid.arrange(p1, p2, p3, nrow=3, top = textGrob("C. FOLLOWER:(FB1-FB2)",gp=gpar(fontsize=18,font=2)))


#FOLLOWERs over LEADERs

#Theta
resultfztheta <- estimate_density(var_hyp3[[1]])
resultfztheta$Electrode <- rep("Fz", nrow(resultfztheta))

resultcztheta <- estimate_density(var_hyp3[[2]])
resultcztheta$Electrode <- rep("Cz", nrow(resultcztheta))

resultpztheta <- estimate_density(var_hyp3[[3]])
resultpztheta$Electrode <- rep("Pz", nrow(resultpztheta))

dframe1 <- rbind(resultfztheta,resultcztheta,resultpztheta)
densdf1 <- do.call(rbind, lapply(split(dframe1, dframe1$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))


#Alpha
resultfzalpha <- estimate_density(var_hyp3[[4]])
resultfzalpha$Electrode <- rep("Fz", nrow(resultfzalpha))

resultczalpha <- estimate_density(var_hyp3[[5]])
resultczalpha$Electrode <- rep("Cz", nrow(resultczalpha))

resultpzalpha <- estimate_density(var_hyp3[[6]])
resultpzalpha$Electrode <- rep("Pz", nrow(resultpzalpha))

dframe2 <- rbind(resultfzalpha,resultczalpha,resultpzalpha)
densdf2 <- do.call(rbind, lapply(split(dframe2, dframe2$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

#Beta
resultfzbeta <- estimate_density(var_hyp3[[7]])
resultfzbeta$Electrode <- rep("Fz", nrow(resultfzbeta))

resultczbeta <- estimate_density(var_hyp3[[8]])
resultczbeta$Electrode <- rep("Cz", nrow(resultczbeta))

resultpzbeta <- estimate_density(var_hyp3[[9]])
resultpzbeta$Electrode <- rep("Pz", nrow(resultpzbeta))

dframe3 <- rbind(resultfzbeta,resultczbeta,resultpzbeta)
densdf3 <- do.call(rbind, lapply(split(dframe3, dframe3$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

p1 <- ggplot(data = densdf1, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf1[densdf1$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-0.25, 0.45), y = c(0, 20)) +
  labs(title="THETA (300-500ms)", x="VALUES", y="DENSITY") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title = element_text(size=14))

p2 <- ggplot(data = densdf2, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf2[densdf2$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-0.25, 0.45), y = c(0, 20)) +
  labs(title="ALPHA (180-230ms)", x="VALUES", y="DENSITY") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title = element_text(size=14))

p3 <- ggplot(data = densdf3, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf3[densdf3$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-0.25, 0.45), y = c(0, 20)) +
  labs(title="BETA (180-230ms)", x="VALUES", y="DENSITY") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title = element_text(size=14))

a3 <-grid.arrange(p1, p2, p3, nrow=3, top = textGrob("D. LEADER-FOLLOWER:(FB1-FB2)",gp=gpar(fontsize=18,font=2)))

totalplot <- grid.arrange(a0, a1, a2, a3, leg1, ncol=5, top = textGrob("TIME-FREQUENCY ESTIMATED DENSITIES",gp=gpar(fontsize=24,font=2)))
ggsave("totalplot_tf.pdf",totalplot, device="pdf", width = 50, height = 40, units="cm")

setwd("~/Google Drive/Doctorado Social Neuroscience/Exp 1 - Social Conformity/Results/EEG Data/New Julio2020/ERP Método Alberto")


library(tidyverse)
library(grid)
library(gridExtra)
library(ggplot2)
library(ggridges)
library(brms)
library(bayestestR)

#Intra-difference
intradif <- readRDS('intradif_v3.rds') #lead as intercept
posterior_data <- posterior_samples(intradif)

time <- c('230', '300', '500')
elec <- c('Fz','Cz','Pz')

df <- expand.grid(a = time, b = elec)
df <- df[order(df$a),]
row.names(df) <- NULL


output <- data.frame()

var_hyp <- list()
var2_hyp <- list()

for (i in 1:nrow(df)){
  
  b0 <- posterior_data[['b_Intercept']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,Intercept]',df$a[i],df$b[i])]]
  b1 <- posterior_data[['b_User']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,User]',df$a[i],df$b[i])]]
  b2 <- posterior_data[['b_TrialRep1']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,TrialRep1]',df$a[i],df$b[i])]]
  b3 <- posterior_data[['b_TrialRep2']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,TrialRep2]',df$a[i],df$b[i])]]
  b4 <- posterior_data[['b_TrialRep1']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,TrialRep1]',df$a[i],df$b[i])]]+ posterior_data[['b_User:TrialRep1']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,User:TrialRep1]',df$a[i],df$b[i])]]
  b5 <- posterior_data[['b_User:TrialRep2']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,User:TrialRep2]',df$a[i],df$b[i])]]
  
  var_hyp[[i]] <- -b2-b4 #Intra: TrialRep 1 over Intercept
  var2_hyp[[i]] <- b4-b5  #Intra: Differences 2 over 1
  
}





resultfz230 <- estimate_density(var_hyp[[1]])
resultfz230$Electrode <- rep("Fz", nrow(resultfz230))

resultcz230 <- estimate_density(var_hyp[[2]])
resultcz230$Electrode <- rep("Cz", nrow(resultcz230))

resultpz230 <- estimate_density(var_hyp[[3]])
resultpz230$Electrode <- rep("Pz", nrow(resultpz230))

dframe1 <- rbind(resultfz230,resultcz230,resultpz230)
densdf1 <- do.call(rbind, lapply(split(dframe1, dframe1$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz300 <- estimate_density(var_hyp[[4]])
resultfz300$Electrode <- rep("Fz", nrow(resultfz300))

resultcz300 <- estimate_density(var_hyp[[5]])
resultcz300$Electrode <- rep("Cz", nrow(resultcz300))

resultpz300 <- estimate_density(var_hyp[[6]])
resultpz300$Electrode <- rep("Pz", nrow(resultpz300))

dframe2 <- rbind(resultfz300,resultcz300,resultpz300)
densdf2 <- do.call(rbind, lapply(split(dframe2, dframe2$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz500 <- estimate_density(var_hyp[[7]])
resultfz500$Electrode <- rep("Fz", nrow(resultfz500))

resultcz500 <- estimate_density(var_hyp[[8]])
resultcz500$Electrode <- rep("Cz", nrow(resultcz500))

resultpz500 <- estimate_density(var_hyp[[9]])
resultpz500$Electrode <- rep("Pz", nrow(resultpz500))

dframe3 <- rbind(resultfz500,resultcz500,resultpz500)
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
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 230-270ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

p2 <- ggplot(data = densdf2, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf2[densdf2$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 300-350ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

p3 <- ggplot(data = densdf3, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf3[densdf3$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 500-700ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

a7 <- grid.arrange(p1, p2, p3, nrow=3, top = textGrob("Intra-difference:(FB1-FB2)",gp=gpar(fontsize=20,font=2)))

#Intra 2-3FB

resultfz230 <- estimate_density(var2_hyp[[1]])
resultfz230$Electrode <- rep("Fz", nrow(resultfz230))

resultcz230 <- estimate_density(var2_hyp[[2]])
resultcz230$Electrode <- rep("Cz", nrow(resultcz230))

resultpz230 <- estimate_density(var2_hyp[[3]])
resultpz230$Electrode <- rep("Pz", nrow(resultpz230))

dframe1 <- rbind(resultfz230,resultcz230,resultpz230)
densdf1 <- do.call(rbind, lapply(split(dframe1, dframe1$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz300 <- estimate_density(var2_hyp[[4]])
resultfz300$Electrode <- rep("Fz", nrow(resultfz300))

resultcz300 <- estimate_density(var2_hyp[[5]])
resultcz300$Electrode <- rep("Cz", nrow(resultcz300))

resultpz300 <- estimate_density(var2_hyp[[6]])
resultpz300$Electrode <- rep("Pz", nrow(resultpz300))

dframe2 <- rbind(resultfz300,resultcz300,resultpz300)
densdf2 <- do.call(rbind, lapply(split(dframe2, dframe2$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz500 <- estimate_density(var2_hyp[[7]])
resultfz500$Electrode <- rep("Fz", nrow(resultfz500))

resultcz500 <- estimate_density(var2_hyp[[8]])
resultcz500$Electrode <- rep("Cz", nrow(resultcz500))

resultpz500 <- estimate_density(var2_hyp[[9]])
resultpz500$Electrode <- rep("Pz", nrow(resultpz500))

dframe3 <- rbind(resultfz500,resultcz500,resultpz500)
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
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 230-270ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

p2 <- ggplot(data = densdf2, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf2[densdf2$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 300-350ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

p3 <- ggplot(data = densdf3, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf3[densdf3$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 500-700ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

a8 <- grid.arrange(p1, p2, p3, nrow=3, top = textGrob("Intra-difference:(FB2-FB3)",gp=gpar(fontsize=20,font=2)))

########
########
########
####LIF#


lead <- readRDS('lead_v3.rds') #lead as intercept
posterior_data <- posterior_samples(lead)


time <- c('230', '300', '500')
elec <- c('Fz','Cz','Pz')

df <- expand.grid(a = time, b = elec)
df <- df[order(df$a),]
row.names(df) <- NULL


output <- data.frame()

var_hyp0 <- list()
var_hyp1 <- list()
var_hyp4 <- list()
var_hyp2 <- list()
var_hyp6 <- list()
var_hyp7 <- list()
var_hyp8 <- list()



for (i in 1:nrow(df)){
  
  b0 <- posterior_data[['b_Intercept']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,Intercept]',df$a[i],df$b[i])]]
  b1 <- posterior_data[['b_Lead2']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,Lead2]',df$a[i],df$b[i])]]
  b2 <- posterior_data[['b_TrialRep1']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,TrialRep1]',df$a[i],df$b[i])]]
  b3 <- posterior_data[['b_TrialRep2']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,TrialRep2]',df$a[i],df$b[i])]]
  b4 <- posterior_data[['b_Lead2:TrialRep1']]+posterior_data[[sprintf('r_electrodo2[C_%s_%s,Lead2:TrialRep1]',df$a[i],df$b[i])]]
  b5 <- posterior_data[['b_Lead2:TrialRep2']]+posterior_data[[sprintf('r_electrodo2[C_%s_%s,Lead2:TrialRep2]',df$a[i],df$b[i])]]
  
  
  var_hyp0[[i]] <- b1 #Differences (Lint-F) in Intercept
  var_hyp1[[i]] <- -b2-b4 #Followers: TrialRep 1 over Intercept
  var_hyp4[[i]] <- -b2 #Leaders: TrialRep 1 over Intercept
  var_hyp2[[i]] <- b2+b4-b3-b5  #Followers: Difference 2 over 3
  var_hyp6[[i]] <- b2-b3 #Leaders: Difference 2 over
  var_hyp7[[i]] <- b4 #Leaders-Followers: TrialRep1 over Intercept
  var_hyp8[[i]] <- b5 #Leader-Follower: TrialRep 2 over Intercept
  
}

#Differences in Intercept

resultfz230 <- estimate_density(var_hyp0[[1]])
resultfz230$Electrode <- rep("Fz", nrow(resultfz230))

resultcz230 <- estimate_density(var_hyp0[[2]])
resultcz230$Electrode <- rep("Cz", nrow(resultcz230))

resultpz230 <- estimate_density(var_hyp0[[3]])
resultpz230$Electrode <- rep("Pz", nrow(resultpz230))

dframe1 <- rbind(resultfz230,resultcz230,resultpz230)
densdf1 <- do.call(rbind, lapply(split(dframe1, dframe1$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz300 <- estimate_density(var_hyp0[[4]])
resultfz300$Electrode <- rep("Fz", nrow(resultfz300))

resultcz300 <- estimate_density(var_hyp0[[5]])
resultcz300$Electrode <- rep("Cz", nrow(resultcz300))

resultpz300 <- estimate_density(var_hyp0[[6]])
resultpz300$Electrode <- rep("Pz", nrow(resultpz300))

dframe2 <- rbind(resultfz300,resultcz300,resultpz300)
densdf2 <- do.call(rbind, lapply(split(dframe2, dframe2$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz500 <- estimate_density(var_hyp0[[7]])
resultfz500$Electrode <- rep("Fz", nrow(resultfz500))

resultcz500 <- estimate_density(var_hyp0[[8]])
resultcz500$Electrode <- rep("Cz", nrow(resultcz500))

resultpz500 <- estimate_density(var_hyp0[[9]])
resultpz500$Electrode <- rep("Pz", nrow(resultpz500))

dframe3 <- rbind(resultfz500,resultcz500,resultpz500)
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
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 230-270ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

p2 <- ggplot(data = densdf2, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf2[densdf2$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 300-350ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

p3 <- ggplot(data = densdf3, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf3[densdf3$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 500-700ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

a0 <- grid.arrange(p1, p2, p3, nrow=3, top = textGrob("A.H1 Differences in FB1 (L-F)",gp=gpar(fontsize=20,font=2)))



#Leaders


resultfz230 <- estimate_density(var_hyp4[[1]])
resultfz230$Electrode <- rep("Fz", nrow(resultfz230))

resultcz230 <- estimate_density(var_hyp4[[2]])
resultcz230$Electrode <- rep("Cz", nrow(resultcz230))

resultpz230 <- estimate_density(var_hyp4[[3]])
resultpz230$Electrode <- rep("Pz", nrow(resultpz230))

dframe1 <- rbind(resultfz230,resultcz230,resultpz230)
densdf1 <- do.call(rbind, lapply(split(dframe1, dframe1$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz300 <- estimate_density(var_hyp4[[4]])
resultfz300$Electrode <- rep("Fz", nrow(resultfz300))

resultcz300 <- estimate_density(var_hyp4[[5]])
resultcz300$Electrode <- rep("Cz", nrow(resultcz300))

resultpz300 <- estimate_density(var_hyp4[[6]])
resultpz300$Electrode <- rep("Pz", nrow(resultpz300))

dframe2 <- rbind(resultfz300,resultcz300,resultpz300)
densdf2 <- do.call(rbind, lapply(split(dframe2, dframe2$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz500 <- estimate_density(var_hyp4[[7]])
resultfz500$Electrode <- rep("Fz", nrow(resultfz500))

resultcz500 <- estimate_density(var_hyp4[[8]])
resultcz500$Electrode <- rep("Cz", nrow(resultcz500))

resultpz500 <- estimate_density(var_hyp4[[9]])
resultpz500$Electrode <- rep("Pz", nrow(resultpz500))

dframe3 <- rbind(resultfz500,resultcz500,resultpz500)
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
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 230-270ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

p2 <- ggplot(data = densdf2, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf2[densdf2$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 300-350ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

p3 <- ggplot(data = densdf3, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf3[densdf3$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 500-700ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

a1 <- grid.arrange(p1, p2, p3, nrow=3, top = textGrob("B.H2 Leader:(FB1-FB2)",gp=gpar(fontsize=20,font=2)))

#Leaders 2-3

resultfz230 <- estimate_density(var_hyp6[[1]])
resultfz230$Electrode <- rep("Fz", nrow(resultfz230))

resultcz230 <- estimate_density(var_hyp6[[2]])
resultcz230$Electrode <- rep("Cz", nrow(resultcz230))

resultpz230 <- estimate_density(var_hyp6[[3]])
resultpz230$Electrode <- rep("Pz", nrow(resultpz230))

dframe1 <- rbind(resultfz230,resultcz230,resultpz230)
densdf1 <- do.call(rbind, lapply(split(dframe1, dframe1$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz300 <- estimate_density(var_hyp6[[4]])
resultfz300$Electrode <- rep("Fz", nrow(resultfz300))

resultcz300 <- estimate_density(var_hyp6[[5]])
resultcz300$Electrode <- rep("Cz", nrow(resultcz300))

resultpz300 <- estimate_density(var_hyp6[[6]])
resultpz300$Electrode <- rep("Pz", nrow(resultpz300))

dframe2 <- rbind(resultfz300,resultcz300,resultpz300)
densdf2 <- do.call(rbind, lapply(split(dframe2, dframe2$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz500 <- estimate_density(var_hyp6[[7]])
resultfz500$Electrode <- rep("Fz", nrow(resultfz500))

resultcz500 <- estimate_density(var_hyp6[[8]])
resultcz500$Electrode <- rep("Cz", nrow(resultcz500))

resultpz500 <- estimate_density(var_hyp6[[9]])
resultpz500$Electrode <- rep("Pz", nrow(resultpz500))

dframe3 <- rbind(resultfz500,resultcz500,resultpz500)
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
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 230-270ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

p2 <- ggplot(data = densdf2, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf2[densdf2$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 300-350ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

p3 <- ggplot(data = densdf3, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf3[densdf3$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 500-700ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

a2 <- grid.arrange(p1, p2, p3, nrow=3, top = textGrob("C.H3 Leader:(FB2-FB3)",gp=gpar(fontsize=20,font=2)))

#Followers


resultfz230 <- estimate_density(var_hyp1[[1]])
resultfz230$Electrode <- rep("Fz", nrow(resultfz230))

resultcz230 <- estimate_density(var_hyp1[[2]])
resultcz230$Electrode <- rep("Cz", nrow(resultcz230))

resultpz230 <- estimate_density(var_hyp1[[3]])
resultpz230$Electrode <- rep("Pz", nrow(resultpz230))

dframe1 <- rbind(resultfz230,resultcz230,resultpz230)
densdf1 <- do.call(rbind, lapply(split(dframe1, dframe1$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz300 <- estimate_density(var_hyp1[[4]])
resultfz300$Electrode <- rep("Fz", nrow(resultfz300))

resultcz300 <- estimate_density(var_hyp1[[5]])
resultcz300$Electrode <- rep("Cz", nrow(resultcz300))

resultpz300 <- estimate_density(var_hyp1[[6]])
resultpz300$Electrode <- rep("Pz", nrow(resultpz300))

dframe2 <- rbind(resultfz300,resultcz300,resultpz300)
densdf2 <- do.call(rbind, lapply(split(dframe2, dframe2$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz500 <- estimate_density(var_hyp1[[7]])
resultfz500$Electrode <- rep("Fz", nrow(resultfz500))

resultcz500 <- estimate_density(var_hyp1[[8]])
resultcz500$Electrode <- rep("Cz", nrow(resultcz500))

resultpz500 <- estimate_density(var_hyp1[[9]])
resultpz500$Electrode <- rep("Pz", nrow(resultpz500))

dframe3 <- rbind(resultfz500,resultcz500,resultpz500)
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
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 230-270ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

p2 <- ggplot(data = densdf2, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf2[densdf2$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 300-350ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

p3 <- ggplot(data = densdf3, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf3[densdf3$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 500-700ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

a3 <- grid.arrange(p1, p2, p3, nrow=3, top = textGrob("D.H4 Follower:(FB1-FB2)",gp=gpar(fontsize=20,font=2)))



#Followers 2-3

resultfz230 <- estimate_density(var_hyp2[[1]])
resultfz230$Electrode <- rep("Fz", nrow(resultfz230))

resultcz230 <- estimate_density(var_hyp2[[2]])
resultcz230$Electrode <- rep("Cz", nrow(resultcz230))

resultpz230 <- estimate_density(var_hyp2[[3]])
resultpz230$Electrode <- rep("Pz", nrow(resultpz230))

dframe1 <- rbind(resultfz230,resultcz230,resultpz230)
densdf1 <- do.call(rbind, lapply(split(dframe1, dframe1$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz300 <- estimate_density(var_hyp2[[4]])
resultfz300$Electrode <- rep("Fz", nrow(resultfz300))

resultcz300 <- estimate_density(var_hyp2[[5]])
resultcz300$Electrode <- rep("Cz", nrow(resultcz300))

resultpz300 <- estimate_density(var_hyp2[[6]])
resultpz300$Electrode <- rep("Pz", nrow(resultpz300))

dframe2 <- rbind(resultfz300,resultcz300,resultpz300)
densdf2 <- do.call(rbind, lapply(split(dframe2, dframe2$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz500 <- estimate_density(var_hyp2[[7]])
resultfz500$Electrode <- rep("Fz", nrow(resultfz500))

resultcz500 <- estimate_density(var_hyp2[[8]])
resultcz500$Electrode <- rep("Cz", nrow(resultcz500))

resultpz500 <- estimate_density(var_hyp2[[9]])
resultpz500$Electrode <- rep("Pz", nrow(resultpz500))

dframe3 <- rbind(resultfz500,resultcz500,resultpz500)
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
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 230-270ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

p2 <- ggplot(data = densdf2, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf2[densdf2$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 300-350ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

p3 <- ggplot(data = densdf3, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf3[densdf3$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 500-700ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

a4 <- grid.arrange(p1, p2, p3, nrow=3, top = textGrob("E.H5 Follower:(FB2-FB3)",gp=gpar(fontsize=20,font=2)))

#LEADERS-FOLLOWERS F1-F2

resultfz230 <- estimate_density(var_hyp7[[1]])
resultfz230$Electrode <- rep("Fz", nrow(resultfz230))

resultcz230 <- estimate_density(var_hyp7[[2]])
resultcz230$Electrode <- rep("Cz", nrow(resultcz230))

resultpz230 <- estimate_density(var_hyp7[[3]])
resultpz230$Electrode <- rep("Pz", nrow(resultpz230))

dframe1 <- rbind(resultfz230,resultcz230,resultpz230)
densdf1 <- do.call(rbind, lapply(split(dframe1, dframe1$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz300 <- estimate_density(var_hyp7[[4]])
resultfz300$Electrode <- rep("Fz", nrow(resultfz300))

resultcz300 <- estimate_density(var_hyp7[[5]])
resultcz300$Electrode <- rep("Cz", nrow(resultcz300))

resultpz300 <- estimate_density(var_hyp7[[6]])
resultpz300$Electrode <- rep("Pz", nrow(resultpz300))

dframe2 <- rbind(resultfz300,resultcz300,resultpz300)
densdf2 <- do.call(rbind, lapply(split(dframe2, dframe2$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz500 <- estimate_density(var_hyp7[[7]])
resultfz500$Electrode <- rep("Fz", nrow(resultfz500))

resultcz500 <- estimate_density(var_hyp7[[8]])
resultcz500$Electrode <- rep("Cz", nrow(resultcz500))

resultpz500 <- estimate_density(var_hyp7[[9]])
resultpz500$Electrode <- rep("Pz", nrow(resultpz500))

dframe3 <- rbind(resultfz500,resultcz500,resultpz500)
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
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 230-270ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

p2 <- ggplot(data = densdf2, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf2[densdf2$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 300-350ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

p3 <- ggplot(data = densdf3, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf3[densdf3$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 500-700ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

a5 <- grid.arrange(p1, p2, p3, nrow=3, top = textGrob("F.H6 Leader-Follower(FB1-FB2)",gp=gpar(fontsize=20,font=2)))


#LEADERS - FOLLOWERS F2-F3

resultfz230 <- estimate_density(var_hyp8[[1]])
resultfz230$Electrode <- rep("Fz", nrow(resultfz230))

resultcz230 <- estimate_density(var_hyp8[[2]])
resultcz230$Electrode <- rep("Cz", nrow(resultcz230))

resultpz230 <- estimate_density(var_hyp8[[3]])
resultpz230$Electrode <- rep("Pz", nrow(resultpz230))

dframe1 <- rbind(resultfz230,resultcz230,resultpz230)
densdf1 <- do.call(rbind, lapply(split(dframe1, dframe1$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz300 <- estimate_density(var_hyp8[[4]])
resultfz300$Electrode <- rep("Fz", nrow(resultfz300))

resultcz300 <- estimate_density(var_hyp8[[5]])
resultcz300$Electrode <- rep("Cz", nrow(resultcz300))

resultpz300 <- estimate_density(var_hyp8[[6]])
resultpz300$Electrode <- rep("Pz", nrow(resultpz300))

dframe2 <- rbind(resultfz300,resultcz300,resultpz300)
densdf2 <- do.call(rbind, lapply(split(dframe2, dframe2$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz500 <- estimate_density(var_hyp8[[7]])
resultfz500$Electrode <- rep("Fz", nrow(resultfz500))

resultcz500 <- estimate_density(var_hyp8[[8]])
resultcz500$Electrode <- rep("Cz", nrow(resultcz500))

resultpz500 <- estimate_density(var_hyp8[[9]])
resultpz500$Electrode <- rep("Pz", nrow(resultpz500))

dframe3 <- rbind(resultfz500,resultcz500,resultpz500)
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
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 230-270ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

p2 <- ggplot(data = densdf2, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf2[densdf2$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 300-350ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

p3 <- ggplot(data = densdf3, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf3[densdf3$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_fill_discrete(breaks=c("Fz","Cz","Pz")) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 0) +
  lims(x = c(-4, 4), y = c(0, 2.25)) +
  labs(title="Interval 500-700ms", x="values", y="density") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18))

a6 <- grid.arrange(p1, p2, p3, nrow=3, top = textGrob("H7: Leader-Follower(FB1-FB3)",gp=gpar(fontsize=20,font=2)))







totalplot <- grid.arrange(a0, a1, a2, a3, a4, a5, a6, a7, a8, ncol=9, top = textGrob("Estimated Densities",gp=gpar(fontsize=22,font=2)))
ggsave("superplot.pdf",totalplot, device="pdf", width = 90, height = 40, units="cm")

#posterplot <- grid.arrange(a1, a2, a3, a4, ncol=4, top = textGrob("Estimated Densities",gp=gpar(fontsize=22,font=2)))
#ggsave("posterplot.pdf",posterplot, device="pdf", width = 60, height = 40, units="cm")

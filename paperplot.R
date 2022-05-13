#setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Google Drive/Doctorado Social Neuroscience/Exp 1 - Social Conformity/Results/EEG Data/ERP2022/Bayesian ERP/")
setwd("~/Desktop/Analysis/")


library(tidyverse)
library(grid)
library(gridExtra)
library(ggplot2)
library(cowplot)
library(ggridges)
library(brms)
library(bayestestR)

lead <- readRDS('lead_v1.rds') #lead as intercept
posterior_data <- as_draws(lead)
posterior_data <- do.call(rbind.data.frame, posterior_data)

time <- c('225', '275','350','500')
elec <- c('Fz','Cz','Pz')

df = expand.grid(a = time, b = elec)
df <- df[order(df$a),]
row.names(df) <- NULL


output <- data.frame()

var_hypA <- list()
var_hypB <- list()
var_hypC <- list()
var_hypD <- list()
var_hypE <- list()
var_hypF <- list()


for (i in 1:nrow(df)){
  
  b0 <- posterior_data[['b_Intercept']] + posterior_data[[sprintf('r_electrodo2.C_%s_%s,Intercept.',df$a[i],df$b[i])]]
  b1 <- posterior_data[['b_Lead2']] + posterior_data[[sprintf('r_electrodo2.C_%s_%s.Lead2.',df$a[i],df$b[i])]]
  b2 <- posterior_data[['b_TrialRep1']] + posterior_data[[sprintf('r_electrodo2.C_%s_%s.TrialRep1.',df$a[i],df$b[i])]]
  b3 <- posterior_data[['b_TrialRep2']] + posterior_data[[sprintf('r_electrodo2.C_%s_%s.TrialRep2.',df$a[i],df$b[i])]]
  b4 <- posterior_data[['b_Lead2.TrialRep1']]+posterior_data[[sprintf('r_electrodo2.C_%s_%s.Lead2.TrialRep1.',df$a[i],df$b[i])]]
  b5 <- posterior_data[['b_Lead2.TrialRep2']]+posterior_data[[sprintf('r_electrodo2.C_%s_%s.Lead2.TrialRep2.',df$a[i],df$b[i])]]
  
  
  var_hypA[[i]] <- b1 #Differences (Lint-F) in Intercept
  var_hypB[[i]] <- -b2 #Leaders: TrialRep 1 over Intercept
  var_hypC[[i]] <- b2-b3 #Leaders: Difference 2 over
  var_hypD[[i]] <- -b2-b4 #Followers: TrialRep 1 over Intercept
  var_hypE[[i]] <- b2+b4-b3-b5  #Followers: Difference 2 over 3
  var_hypF[[i]] <- b4 #Leaders-Followers: TrialRep1 over Intercept
}

col <- c("#004d8d", "#cc2701", "#e5b400")

#Differences in Intercept

resultfz225 <- estimate_density(var_hypA[[1]])
resultfz225$Electrode <- rep("Fz", nrow(resultfz225))

resultcz225 <- estimate_density(var_hypA[[2]])
resultcz225$Electrode <- rep("Cz", nrow(resultcz225))

resultpz225 <- estimate_density(var_hypA[[3]])
resultpz225$Electrode <- rep("Pz", nrow(resultpz225))

dframe1 <- rbind(resultfz225,resultcz225,resultpz225)
densdf1 <- do.call(rbind, lapply(split(dframe1, dframe1$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz275 <- estimate_density(var_hypA[[4]])
resultfz275$Electrode <- rep("Fz", nrow(resultfz275))

resultcz275 <- estimate_density(var_hypA[[5]])
resultcz275$Electrode <- rep("Cz", nrow(resultcz275))

resultpz275 <- estimate_density(var_hypA[[6]])
resultpz275$Electrode <- rep("Pz", nrow(resultpz275))

dframe1 <- rbind(resultfz275,resultcz275,resultpz275)
densdf1 <- do.call(rbind, lapply(split(dframe1, dframe1$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz350 <- estimate_density(var_hypA[[7]])
resultfz350$Electrode <- rep("Fz", nrow(resultfz350))

resultcz350 <- estimate_density(var_hypA[[8]])
resultcz350$Electrode <- rep("Cz", nrow(resultcz350))

resultpz350 <- estimate_density(var_hypA[[9]])
resultpz350$Electrode <- rep("Pz", nrow(resultpz350))

dframe2 <- rbind(resultfz350,resultcz350,resultpz350)
densdf2 <- do.call(rbind, lapply(split(dframe2, dframe2$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz500 <- estimate_density(var_hypA[[10]])
resultfz500$Electrode <- rep("Fz", nrow(resultfz500))

resultcz500 <- estimate_density(var_hypA[[11]])
resultcz500$Electrode <- rep("Cz", nrow(resultcz500))

resultpz500 <- estimate_density(var_hypA[[12]])
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
  scale_color_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +   
  scale_fill_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept = 0) +
  lims(x = c(-3, 2), y = c(0, 2.25)) +
  labs(title="INTERVAL 225-275ms", x="VALUES", y="DENSITY") +
  theme_bw() +   
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title=element_text(size=14))

#Extract legend
leg1 <- get_legend(p1)
#Remove legend
p1 <- p1 + theme(legend.position = "none")

p2 <- ggplot(data = densdf1, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf1[densdf1$CI,], 
            aes(color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_color_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +
  scale_fill_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept = 0) +
  lims(x = c(-3, 2), y = c(0, 2.25)) +
  labs(title="INTERVAL 275-350ms", x="VALUES", y="DENSITY") +
  theme_bw() +   
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title=element_text(size=14))


#Remove legend
p2 <- p2 + theme(legend.position = "none")

p3 <- ggplot(data = densdf2, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf2[densdf2$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_color_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +   
  scale_fill_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept = 0) +
  lims(x = c(-3, 2), y = c(0, 2.25)) +
  labs(title="INTERVAL 350-500ms", x="VALUES", y="DENSITY") +
  theme_bw() +   
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title=element_text(size=14))


#Remove legend
p3 <- p3 + theme(legend.position = "none")

p4 <- ggplot(data = densdf3, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf3[densdf3$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_color_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +   
  scale_fill_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept = 0) +
  lims(x = c(-3, 2), y = c(0, 2.25)) +
  labs(title="INTERVAL 500-700ms", x="VALUES", y="DENSITY") +
  theme_bw() +   
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title=element_text(size=14))

#Remove legend
p4 <- p4 + theme(legend.position = "none")

h1 <- grid.arrange(p1, p2, p3, p4, nrow=4, top = textGrob("DIFFERENCES IN FB1 (L-F)",gp=gpar(fontsize=16,font=2)))



#Leaders


resultfz225 <- estimate_density(var_hypB[[1]])
resultfz225$Electrode <- rep("Fz", nrow(resultfz225))

resultcz225 <- estimate_density(var_hypB[[2]])
resultcz225$Electrode <- rep("Cz", nrow(resultcz225))

resultpz225 <- estimate_density(var_hypB[[3]])
resultpz225$Electrode <- rep("Pz", nrow(resultpz225))

dframe1 <- rbind(resultfz225,resultcz225,resultpz225)
densdf1 <- do.call(rbind, lapply(split(dframe1, dframe1$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz275 <- estimate_density(var_hypB[[4]])
resultfz275$Electrode <- rep("Fz", nrow(resultfz275))

resultcz275 <- estimate_density(var_hypB[[5]])
resultcz275$Electrode <- rep("Cz", nrow(resultcz275))

resultpz275 <- estimate_density(var_hypB[[6]])
resultpz275$Electrode <- rep("Pz", nrow(resultpz275))

dframe1 <- rbind(resultfz275,resultcz275,resultpz275)
densdf1 <- do.call(rbind, lapply(split(dframe1, dframe1$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz350 <- estimate_density(var_hypB[[7]])
resultfz350$Electrode <- rep("Fz", nrow(resultfz350))

resultcz350 <- estimate_density(var_hypB[[8]])
resultcz350$Electrode <- rep("Cz", nrow(resultcz350))

resultpz350 <- estimate_density(var_hypB[[9]])
resultpz350$Electrode <- rep("Pz", nrow(resultpz350))

dframe2 <- rbind(resultfz350,resultcz350,resultpz350)
densdf2 <- do.call(rbind, lapply(split(dframe2, dframe2$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz500 <- estimate_density(var_hypB[[10]])
resultfz500$Electrode <- rep("Fz", nrow(resultfz500))

resultcz500 <- estimate_density(var_hypB[[11]])
resultcz500$Electrode <- rep("Cz", nrow(resultcz500))

resultpz500 <- estimate_density(var_hypB[[12]])
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
  scale_color_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +   
  scale_fill_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept = 0) +
  lims(x = c(-3, 2), y = c(0, 2.25)) +
  labs(title="INTERVAL 225-275ms", x="VALUES", y="DENSITY") +
  theme_bw() +   
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title=element_text(size=14))

#Remove legend
p1 <- p1 + theme(legend.position = "none")

p2 <- ggplot(data = densdf1, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf1[densdf1$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_color_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +   
  scale_fill_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept = 0) +
  lims(x = c(-3, 2), y = c(0, 2.25)) +
  labs(title="INTERVAL 275-350ms", x="VALUES", y="DENSITY") +
  theme_bw() +   
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title=element_text(size=14))

#Remove legend
p2 <- p2 + theme(legend.position = "none")

p3 <- ggplot(data = densdf2, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf2[densdf2$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_color_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +   
  scale_fill_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept = 0) +
  lims(x = c(-3, 2), y = c(0, 2.25)) +
  labs(title="INTERVAL 350-500ms", x="VALUES", y="DENSITY") +
  theme_bw() +   
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title=element_text(size=14))

#Remove legend
p3 <- p3 + theme(legend.position = "none")

p4 <- ggplot(data = densdf3, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf3[densdf3$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_color_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +   
  scale_fill_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept = 0) +
  lims(x = c(-3, 2), y = c(0, 2.25)) +
  labs(title="INTERVAL 500-700ms", x="VALUES", y="DENSITY") +
  theme_bw() +   
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title=element_text(size=14))

#Remove legend
p4 <- p4 + theme(legend.position = "none")

h2 <- grid.arrange(p1, p2, p3, p4, nrow=4, top = textGrob("A. LEADER:(FB1-FB2)",gp=gpar(fontsize=16,font=2)))


#Followers


resultfz225 <- estimate_density(var_hypD[[1]])
resultfz225$Electrode <- rep("Fz", nrow(resultfz225))

resultcz225 <- estimate_density(var_hypD[[2]])
resultcz225$Electrode <- rep("Cz", nrow(resultcz225))

resultpz225 <- estimate_density(var_hypD[[3]])
resultpz225$Electrode <- rep("Pz", nrow(resultpz225))

dframe1 <- rbind(resultfz225,resultcz225,resultpz225)
densdf1 <- do.call(rbind, lapply(split(dframe1, dframe1$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz275 <- estimate_density(var_hypD[[4]])
resultfz275$Electrode <- rep("Fz", nrow(resultfz275))

resultcz275 <- estimate_density(var_hypD[[5]])
resultcz275$Electrode <- rep("Cz", nrow(resultcz275))

resultpz275 <- estimate_density(var_hypD[[6]])
resultpz275$Electrode <- rep("Pz", nrow(resultpz275))

dframe1 <- rbind(resultfz275,resultcz275,resultpz275)
densdf1 <- do.call(rbind, lapply(split(dframe1, dframe1$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz350 <- estimate_density(var_hypD[[7]])
resultfz350$Electrode <- rep("Fz", nrow(resultfz350))

resultcz350 <- estimate_density(var_hypD[[8]])
resultcz350$Electrode <- rep("Cz", nrow(resultcz350))

resultpz350 <- estimate_density(var_hypD[[9]])
resultpz350$Electrode <- rep("Pz", nrow(resultpz350))

dframe2 <- rbind(resultfz350,resultcz350,resultpz350)
densdf2 <- do.call(rbind, lapply(split(dframe2, dframe2$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz500 <- estimate_density(var_hypD[[10]])
resultfz500$Electrode <- rep("Fz", nrow(resultfz500))

resultcz500 <- estimate_density(var_hypD[[11]])
resultcz500$Electrode <- rep("Cz", nrow(resultcz500))

resultpz500 <- estimate_density(var_hypD[[12]])
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
  scale_color_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +   
  scale_fill_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept = 0) +
  lims(x = c(-3, 2), y = c(0, 2.25)) +
  labs(title="INTERVAL 225-275ms", x="VALUES", y="DENSITY") +
  theme_bw() +   
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title=element_text(size=14))

#Remove legend
p1 <- p1 + theme(legend.position = "none")

p2 <- ggplot(data = densdf1, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf1[densdf1$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_color_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +   
  scale_fill_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept = 0) +
  lims(x = c(-3, 2), y = c(0, 2.25)) +
  labs(title="INTERVAL 275-350ms", x="VALUES", y="DENSITY") +
  theme_bw() +   
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title=element_text(size=14))

#Remove legend
p2 <- p2 + theme(legend.position = "none")

p3 <- ggplot(data = densdf2, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf2[densdf2$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_color_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +   
  scale_fill_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept = 0) +
  lims(x = c(-3, 2), y = c(0, 2.25)) +
  labs(title="INTERVAL 350-500ms", x="VALUES", y="DENSITY") +
  theme_bw() +   
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title=element_text(size=14))

#Remove legend
p3 <- p3 + theme(legend.position = "none")

p4 <- ggplot(data = densdf3, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf3[densdf3$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_color_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +   
  scale_fill_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept = 0) +
  lims(x = c(-3, 2), y = c(0, 2.25)) +
  labs(title="INTERVAL 500-700ms", x="VALUES", y="DENSITY") +
  theme_bw() +   
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title=element_text(size=14))

#Remove legend
p4 <- p4 + theme(legend.position = "none")

h4 <- grid.arrange(p1, p2, p3, p4, nrow=4, top = textGrob("B. FOLLOWER:(FB1-FB2)",gp=gpar(fontsize=16,font=2)))




#LEADERS-FOLLOWERS F1-F2

resultfz225 <- estimate_density(var_hypF[[1]])
resultfz225$Electrode <- rep("Fz", nrow(resultfz225))

resultcz225 <- estimate_density(var_hypF[[2]])
resultcz225$Electrode <- rep("Cz", nrow(resultcz225))

resultpz225 <- estimate_density(var_hypF[[3]])
resultpz225$Electrode <- rep("Pz", nrow(resultpz225))

dframe1 <- rbind(resultfz225,resultcz225,resultpz225)
densdf1 <- do.call(rbind, lapply(split(dframe1, dframe1$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz275 <- estimate_density(var_hypF[[4]])
resultfz275$Electrode <- rep("Fz", nrow(resultfz275))

resultcz275 <- estimate_density(var_hypF[[5]])
resultcz275$Electrode <- rep("Cz", nrow(resultcz275))

resultpz275 <- estimate_density(var_hypF[[6]])
resultpz275$Electrode <- rep("Pz", nrow(resultpz275))

dframe1 <- rbind(resultfz275,resultcz275,resultpz275)
densdf1 <- do.call(rbind, lapply(split(dframe1, dframe1$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz350 <- estimate_density(var_hypF[[7]])
resultfz350$Electrode <- rep("Fz", nrow(resultfz350))

resultcz350 <- estimate_density(var_hypF[[8]])
resultcz350$Electrode <- rep("Cz", nrow(resultcz350))

resultpz350 <- estimate_density(var_hypF[[9]])
resultpz350$Electrode <- rep("Pz", nrow(resultpz350))

dframe2 <- rbind(resultfz350,resultcz350,resultpz350)
densdf2 <- do.call(rbind, lapply(split(dframe2, dframe2$Electrode), function(z)
{
  integ <- cumsum(z$y * mean(diff(z$x)))
  CI <-  integ > 0.025 & integ < 0.975
  data.frame(x = z$x, y = z$y, Electrode = z$Electrode[1], CI = CI)
}))

resultfz500 <- estimate_density(var_hypF[[10]])
resultfz500$Electrode <- rep("Fz", nrow(resultfz500))

resultcz500 <- estimate_density(var_hypF[[11]])
resultcz500$Electrode <- rep("Cz", nrow(resultcz500))

resultpz500 <- estimate_density(var_hypF[[12]])
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
  scale_color_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +   
  scale_fill_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept = 0) +
  lims(x = c(-3, 2), y = c(0, 2.25)) +
  labs(title="INTERVAL 225-275ms", x="VALUES", y="DENSITY") +
  theme_bw() +   
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title=element_text(size=14))

#Remove legend
p1 <- p1 + theme(legend.position = "none")

p2 <- ggplot(data = densdf1, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf1[densdf1$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_color_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +   
  scale_fill_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept = 0) +
  lims(x = c(-3, 2), y = c(0, 2.25)) +
  labs(title="INTERVAL 275-350ms", x="VALUES", y="DENSITY") +
  theme_bw() +   
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title=element_text(size=14))

#Remove legend
p2 <- p2 + theme(legend.position = "none")

p3 <- ggplot(data = densdf2, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf2[densdf2$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_color_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +   
  scale_fill_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept = 0) +
  lims(x = c(-3, 2), y = c(0, 2.25)) +
  labs(title="INTERVAL 350-500ms", x="VALUES", y="DENSITY") +
  theme_bw() +   
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title=element_text(size=14))

#Remove legend
p3 <- p3 + theme(legend.position = "none")

p4 <- ggplot(data = densdf3, mapping = aes(x = x, y = y)) +
  geom_area(data = densdf3[densdf3$CI,], 
            aes(fill = Electrode, color = Electrode),
            outline.type = "full", alpha = 0.3, size = 1) +
  geom_line(aes(color = Electrode), size = 1) +
  scale_color_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +   
  scale_fill_manual(values = col, breaks = c("Fz", "Cz", "Pz")) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept = 0) +
  lims(x = c(-3, 2), y = c(0, 2.25)) +
  labs(title="INTERVAL 500-700ms", x="VALUES", y="DENSITY") +
  theme_bw() +   
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title=element_text(size=14))

#Remove legend
p4 <- p4 + theme(legend.position = "none")

h6 <- grid.arrange(p1, p2, p3, p4, nrow=4, top = textGrob("C. LEADER-FOLLOWER:(FB1-FB2)",gp=gpar(fontsize=16,font=2)))

paperplot <- grid.arrange(h2, h4, h6, leg1, ncol=4, widths=c(1,1,1,0.5), top = textGrob("ESTIMATED DENSITIES",gp=gpar(fontsize=18,font=2)))

ggsave("paperplot.pdf",paperplot, device="pdf", width = 60, height = 40, units="cm")



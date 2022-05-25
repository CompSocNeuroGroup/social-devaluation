library(nlme)
library(cowplot)
library(ggplot2)
library (Rmisc)
library(ggbeeswarm)
library(ggdist)
library(gghalves)
library(ggforce)
library(lme4)
library(rethinking)
library(rstan) 
library(scales)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library("RColorBrewer")
setwd("/home/jthompsz/data/Pilot-Vignettes/")
#mydir <- "rawData_072921/"
#mydir <- "rawData_111821/"
mydir <- "rawData_021522/"
#mydir <- "rawData_030422/"
dat_out <- read.csv(paste0 (mydir, "dat-3FaceLearning-Vignettes.csv"))


dat_out[,'ID']<-factor(dat_out[,'ID'])
dat_out[,'Gender']<-factor(dat_out[,'Gender'])
dat_out[,'Face']<-factor(dat_out[,'Face'])

# logit transform
dat_out$tAcc <- dat_out$Acc
dat_out$tAcc[which(dat_out$tAcc==1)] = 1-0.01
dat_out$tAcc[which(dat_out$tAcc==0)] = 0.01
dat_out$qAcc <- qlogis(dat_out$tAcc)
dat_out$lRT <- log(dat_out$RT)

basic.acc = lmer(formula = qAcc ~ 1 + Face + (1|ID), data=dat_out, na.action=na.omit)
summary(basic.acc)

basic.RT = lmer(formula = lRT~ 1 + Face + (1|ID), data=dat_out, na.action=na.omit)
summary(basic.RT)

### Make data for Stan
# Make Stan data

rDat <- dat_out[complete.cases(dat_out$Acc),]
rDat <- rDat[-c(which(rDat$ID==5095221)),]
Nsubj = length(unique(rDat$ID))
rDat$subj = rep(1:Nsubj, times=1, each=2)

# Fit Stan model using map2stan
formula = qAcc ~ 1 + Face + (1|subj)
m3 <- glimmer( formula , rDat , gaussian )

m3s <- map2stan( m3$f , data=m3$d, warmup=1000 , iter=2000 , chains=4)
precis(m3s, prob=0.95)

formula = lRT~ 1 + Face + (1|subj)
m3 <- glimmer( formula , rDat , gaussian )

m3s <- map2stan( m3$f , data=m3$d, warmup=1000 , iter=2000 , chains=4)
precis(m3s, prob=0.95)

### Make plots
data_sumstan <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    stanDat <- list(n = nrow(x),
                    x = x[[col]])
    choiceFit <- stan(file = "BehaviorMean.stan", data = stanDat,
                      iter = 2000, chains = 4)
    mu_sigma <- summary(choiceFit, pars=c("mu"))$summary[,c("mean", "2.5%", "97.5%")]
    c(mean = mu_sigma[1], lo = mu_sigma[2], hi = mu_sigma[3])
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- plyr::rename(data_sum, c("mean.mean" = varname, "lo.2.5%" = "lo" ,"hi.97.5%" = "hi" ))
  return(data_sum)
}

data_sumstanRT <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    stanDat <- list(n = nrow(x),
                    x = x[[col]])
    choiceFit <- stan(file = "BehaviorRTMean.stan", data = stanDat,
                      iter = 2000, chains = 4)
    mu_sigma <- summary(choiceFit, pars=c("mu"))$summary[,c("mean", "2.5%", "97.5%")]
    c(mean = mu_sigma[1], lo = mu_sigma[2], hi = mu_sigma[3])
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- plyr::rename(data_sum, c("mean.mean" = varname, "lo.2.5%" = "lo" ,"hi.97.5%" = "hi" ))
  return(data_sum)
}

df3 <- data_sumstan(rDat, varname="Acc", groupnames = c("Face"))

ggplot(geom_bar(data = df3$Acc))

lp <- ggplot(dat_out[complete.cases(rDat$Face),], aes(x = Face, y = Acc)) +
  geom_bar(data = df3, stat='identity',
           width = .4, size = 1.2,
           color="black",
           fill = c("#999999","#E69F00")) +
  geom_errorbar(data = df3,
    mapping=aes(x=Face, ymin=lo, ymax=hi),
    width = .05, size = 1.2
  ) +
  labs(y = "Accuracy", x = "something") +
  ggbeeswarm::geom_quasirandom(
    ## draw bigger points
    size = 3,
    ## add some transparency
    alpha = .4,
    ## control range of the beeswarm
    width = .2
  ) +
  coord_cartesian(xlim = c(1.2, NA), clip = "off")

p1 <- lp +  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank(),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 18)) +
  scale_x_discrete(labels=c("no vignette" = "No Vignette", "vignette" = "Vignette"))

df3 <- data_sumstan(rDat, varname="lRT", groupnames = c("Face"))

lp <- ggplot(dat_out[complete.cases(rDat$Face),], aes(x = Face, y = lRT)) +
  geom_bar(data = df3, stat='identity',
           width = .4, size = 1.2,
           color="black",
           fill = c("#999999","#E69F00")) +
  scale_y_continuous(limits=c(4,8),oob = rescale_none) +
  geom_errorbar(data = df3,
                mapping=aes(x=Face, ymin=lo, ymax=hi),
                width = .05, size = 1.2
  ) +
  
  labs(y = "Reaction Time log(ms)", x = "something") +
  ggbeeswarm::geom_quasirandom(
    ## draw bigger points
    size = 3,
    ## add some transparency
    alpha = .4,
    ## control range of the beeswarm
    width = .2
  ) #+
  #coord_cartesian(xlim = c(1.2, NA), ylim = c(4,8),clip = "off")

p2 <- lp +  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank(),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 18)) +
  scale_x_discrete(labels=c("no vignette" = "No Vignette", "vignette" = "Vignette"))

g <- plot_grid(p1,p2, labels = "AUTO", align = "h")
g
ggsave("Study4AccuracyRT-Learning.pdf", g)

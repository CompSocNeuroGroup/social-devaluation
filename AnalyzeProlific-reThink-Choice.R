library(nlme)
library(cowplot)
library(ggplot2)
library (Rmisc)
library(ggbeeswarm)
library(ggdist)
library(gghalves)
library(ggforce)
library(dplyr)
library(lme4)
library(lmerTest)
library(rethinking)
library(rstan) 
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library("RColorBrewer")
setwd("/home/jthompsz/data/Pilot-Vignettes/")
#mydir <- "rawData_072921/"
mydir <- "rawData_111821/"
#mydir <- "rawData_021122/"
#mydir <- "rawData_021522/"
#mydir <- "rawData_030422/"
#mydir <- "rawData_050622/"
dat_out <- read.csv(paste0 (mydir, "dat-2SymbolChoice.csv"))


#dat_out[,'ID']<-factor(dat_out[,'ID'])
#dat_out[,'Gender']<-factor(dat_out[,'Gender'])
#dat_out[,'Vignette']<-factor(dat_out[,'Vignette'], levels=c('vignette', 'no vignette'))
#dat_out[,'Pre_Post']<-factor(dat_out[,'Pre.Post'])
#dat_out[,'hi_or_lo']<-factor(dat_out[,'hi_or_lo'])

# logit transform
dat_out$twithin <- dat_out$within
dat_out$twithin[which(dat_out$twithin==1)] = 1-0.01
dat_out$twithin[which(dat_out$twithin==0)] = 0.01
dat_out$qwithin <- qlogis(dat_out$twithin)

dat_out$tbetween <- dat_out$between
dat_out$tbetween[which(dat_out$tbetween==1)] = 1-0.01
dat_out$tbetween[which(dat_out$tbetween==0)] = 0.01
dat_out$qbetween <- qlogis(dat_out$tbetween)

basic.qbetween = lmer(formula = qbetween ~ 1 + Pre.Post + hi_or_lo + Pre.Post*hi_or_lo + (1|ID), data=dat_out, na.action=na.omit)
summary(basic.qbetween)

basic.qwithin = lmer(formula = qwithin ~ 1 + Pre.Post + Vignette + Pre.Post*Vignette + (1|ID), data=dat_out, na.action=na.omit)
summary(basic.qwithin)




dat_out %>%
  group_by(hi_or_lo,Pre.Post) %>%
  summarise(Mean = mean(between, na.rm = TRUE), SE=sd(between, na.rm = TRUE))

dat_out %>%
  group_by(Vignette,Pre.Post) %>%
  summarise(Mean = mean(within, na.rm = TRUE), SE=sd(within, na.rm = TRUE))

### Make data for Stan
# Make Stan data

rDat <- dat_out[complete.cases(dat_out$qwithin),]
Nsubj = length(unique(rDat$ID))
rDat$subj = rep(1:Nsubj, times=1, each=4)

# Fit Stan model using map2stan
formula = qbetween ~ 1 + Pre.Post + hi_or_lo + Pre.Post*hi_or_lo + (1|subj)
m3 <- glimmer( formula , rDat , gaussian )

m3s <- map2stan( m3$f , data=m3$d, warmup=1000 , iter=2000 , chains=4)
precis(m3s)

formula = qwithin ~ 1 + Pre.Post + Vignette + Pre.Post*Vignette + (1|subj)
m3 <- glimmer( formula , rDat , gaussian )

m3s <- map2stan( m3$f , data=m3$d, warmup=1000 , iter=2000 , chains=4)
precis(m3s)

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


df3 <- data_sumstan(rDat, varname="between", groupnames = c("Pre.Post", "hi_or_lo"))

face_names <- c("hihi" = "High vs High", "lolo" = "Low vs Low")


lp <- ggplot(dat_out[complete.cases(dat_out$between),], aes(x = Pre.Post, y = between, group = Pre.Post)) +
  geom_bar(data = df3, stat='identity',
           width = .4, size = 1.2,
           color="black",
           fill = c("#E66101","#E69F00","#5e3c99","#b2abd2")) +
  geom_errorbar(data = df3,
                mapping=aes(x=Pre.Post, ymin=lo, ymax=hi),
                width = .05, size = 1.2
  ) +
  labs(y = "Preference for Vignette Symbol", x = "something") +
  facet_grid(. ~ hi_or_lo,labeller = as_labeller(face_names))  +
  ggbeeswarm::geom_quasirandom(
    ## draw bigger points
    size = 3,
    ## add some transparency
    alpha = .3,
    ## control range of the beeswarm
    width = .2
  ) +
  coord_cartesian(xlim = c(1.2, NA), clip = "off") +
  geom_hline(yintercept=.5, linetype="dashed", color = "black")

p1 <- lp +  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank(),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 18),
        strip.text.x = element_text(size = 18)) +
  scale_x_discrete(limits=c("Pre", "Post"))


p1
ggsave("Study4Pre-Post-ChoiceBetween.pdf", p1)

### Within

face_names <- c("no vignette" = "No Vignette","vignette" = "Vignette")
df3 <- data_sumstan(rDat, varname="within", groupnames = c("Pre.Post", "Vignette"))

lp <- ggplot(dat_out[complete.cases(dat_out$within),], aes(x = Pre.Post, y = within, group = Pre.Post)) +
  geom_bar(data = df3, stat='identity',
           width = .4, size = 1.2,
           color="black",
           fill = c("#E69F00","#b2abd2","#E66101","#5e3c99")) +
  geom_errorbar(data = df3,
                mapping=aes(x=Pre.Post, ymin=lo, ymax=hi),
                width = .05, size = 1.2
  ) +
  labs(y = "Preference for High Value Symbol", x = "something") +
  facet_grid(. ~ Vignette ,labeller = as_labeller(face_names))  +
  ggbeeswarm::geom_quasirandom(
    ## draw bigger points
    size = 3,
    ## add some transparency
    alpha = .3,
    ## control range of the beeswarm
    width = .2
  ) +
  coord_cartesian(xlim = c(1.2, NA), clip = "off") +
  geom_hline(yintercept=.5, linetype="dashed", color = "black")

p1 <- lp +  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank(),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 18),
        strip.text.x = element_text(size = 18)) +
  scale_x_discrete(limits=c("Pre", "Post"))


p1
ggsave("Study4Pre-Post-ChoiceWithin.pdf", p1)


##### Study 3 Plots
basic.qwithin = lme(qwithin ~ 1 + Vignette, random=~1|ID, data=dat_out, na.action=na.omit)
summary(basic.qwithin)
anova(basic.qwithin)

basic.qbetween = lme(qbetween ~ 1 + hi_or_lo, random=~1|ID, data=dat_out, na.action=na.omit)
summary(basic.qbetween)
anova(basic.qbetween)


### Between
dat_out %>%
  group_by(hi_or_lo) %>%
  summarise(Mean = mean(between, na.rm = TRUE), SE=sd(between, na.rm = TRUE))

dat_out %>%
  group_by(Vignette) %>%
  summarise(Mean = mean(within, na.rm = TRUE), SE=sd(within, na.rm = TRUE))

face_names <- c("hihi" = "High vs High", "lolo" = "Low vs Low")


lp <- ggplot(dat_out[complete.cases(dat_out$between),], aes(x = 1, y = between, group = hi_or_lo)) +
  geom_bar(stat = "summary", fun = "mean",
           width = .1, size = 1.2,
           color="black",
           #fill = brewer.pal(n = 4, name = "Set3")) +
           fill = c("#E66101","#5e3c99")) +
  stat_summary(
    geom = "errorbar",
    fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
    fun.min = function(x) mean(x) - sd(x)/sqrt(length(x)),
    width = .05, size = 1.2
  ) +
  xlim(c(0.8,1.2)) +
  ylim(c(0,1.0)) +
  labs(y = "Preference for Vignette Symbol", x = "something") +
  facet_grid(. ~ hi_or_lo,labeller = as_labeller(face_names))  +
  ggbeeswarm::geom_quasirandom(
    ## draw bigger points
    size = 3,
    ## add some transparency
    alpha = .3,
    ## control range of the beeswarm
    width = .05
  ) +
  #coord_cartesian(xlim = c(1.2, NA), clip = "off") +
  geom_hline(yintercept=.5, linetype="dashed", color = "black")

p1 <- lp +  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 18),
        axis.ticks.x = element_blank(), strip.text.x = element_text(size = 18))


p1
ggsave("Study3-PostOnly-ChoiceBetween.pdf", p1)

### Within

face_names <- c("no vignette" = "No Vignette","vignette" = "Vignette")


lp <- ggplot(dat_out[complete.cases(dat_out$within),], aes(x = 1, y = within, group = Vignette)) +
  geom_bar(stat = "summary", fun = "mean",
           width = .1, size = 1.2,
           color="black",
           #fill = brewer.pal(n = 4, name = "Set3")) +
           fill = c("#E66101","#5e3c99")) +
  stat_summary(
    geom = "errorbar",
    fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
    fun.min = function(x) mean(x) - sd(x)/sqrt(length(x)),
    width = .05, size = 1.2
  ) +
  xlim(c(0.8,1.2)) +
  ylim(c(0,1.0)) +
  labs(y = "Preference for High Value", x = "something") +
  facet_grid(. ~ Vignette,labeller = as_labeller(face_names))  +
  ggbeeswarm::geom_quasirandom(
    ## draw bigger points
    size = 3,
    ## add some transparency
    alpha = .3,
    ## control range of the beeswarm
    width = .05
  ) +
  #coord_cartesian(xlim = c(1.2, NA), clip = "off") +
  geom_hline(yintercept=.5, linetype="dashed", color = "black")

p1 <- lp +  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 18),
        axis.ticks.x = element_blank(), strip.text.x = element_text(size = 18))


p1

ggsave("Study3-PostOnly-ChoiceWithin.pdf", p1)

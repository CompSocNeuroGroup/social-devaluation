library(nlme)
library(cowplot)
library(ggplot2)
library (Rmisc)
library(ggbeeswarm)
library(ggdist)
library(gghalves)
library(ggforce)
library(rethinking)
library(rstan) 
rstan_options(auto_write = TRUE)
library("RColorBrewer")
setwd("/home/jthompsz/data/Pilot-Vignettes/")

#mydir <- "rawData_072921/"
#mydir <- "rawData_111821/"
mydir <- "rawData_021522/"
#mydir <- "rawData_030422/"
dat_out <- read.csv(paste0 (mydir, "dat-3FaceRatings.csv"))

dat_out[,'ID']<-factor(dat_out[,'ID'])
dat_out[,'Gender']<-factor(dat_out[,'Gender'])
dat_out[,'Vignette']<-factor(dat_out[,'Vignette'])
dat_out[,'Pre.Post']<-factor(dat_out[,'Pre.Post'])

basic.friendly = lmer(formula = friendly ~ 1 + Pre.Post + Vignette + Pre.Post*Vignette + (1|ID), data=dat_out, na.action=na.omit)
summary(basic.friendly)


basic.socialize = lmer(formula = socialize ~ 1 + Pre.Post + Vignette + Pre.Post*Vignette + (1|ID), data=dat_out, na.action=na.omit)
summary(basic.socialize)

basic.trustworthy = lmer(formula = trustworthy ~ 1 + Pre.Post + Vignette + Pre.Post*Vignette + (1|ID), data=dat_out, na.action=na.omit)
summary(basic.trustworthy)

basic.warmth = lmer(formula = warmth ~ 1 + Pre.Post + Vignette + Pre.Post*Vignette + (1|ID), data=dat_out, na.action=na.omit)
summary(basic.warmth)

basic.problems = lmer(formula = problems ~ 1 + Pre.Post + Vignette + Pre.Post*Vignette + (1|ID), data=dat_out, na.action=na.omit)
summary(basic.problems)

### Make data for Stan
# Make Stan data

rDat <- dat_out[complete.cases(dat_out$warmth),]
#rDat <- rDat[-c(which(rDat$ID==4436408)),]
Nsubj = length(unique(rDat$ID))
rDat$subj = rep(1:Nsubj, times=1, each=4)

# Fit Stan model using map2stan
formula = friendly ~ 1 + Pre.Post + Vignette + Pre.Post*Vignette + (1|ID)
m1 <- glimmer( formula , rDat , gaussian )

m1s <- map2stan( m1$f , data=m1$d, warmup=1000 , iter=2000 , chains=4)
precis(m1s, prob=0.95)

formula = socialize ~ 1 + Pre.Post + Vignette + Pre.Post*Vignette + (1|ID)
m2 <- glimmer( formula , rDat , gaussian )

m2s <- map2stan( m2$f , data=m2$d, warmup=1000 , iter=2000 , chains=4)
precis(m2s, prob=0.95)

formula = trustworthy ~ 1 + Pre.Post + Vignette + Pre.Post*Vignette + (1|ID)
m3 <- glimmer( formula , rDat , gaussian )

m3s <- map2stan( m3$f , data=m3$d, warmup=1000 , iter=2000 , chains=4)
precis(m3s, prob=0.95)

formula = warmth ~ 1 + Pre.Post + Vignette + Pre.Post*Vignette + (1|ID)
m4 <- glimmer( formula , rDat , gaussian )

m4s <- map2stan( m4$f , data=m4$d, warmup=1000 , iter=2000 , chains=4)
precis(m4s, prob=0.95)

formula = problems ~ 1 + Pre.Post + Vignette + Pre.Post*Vignette + (1|ID)
m5 <- glimmer( formula , rDat , gaussian )

m5s <- map2stan( m5$f , data=m5$d, warmup=1000 , iter=2000 , chains=4)
precis(m5s, prob=0.95)



## Posterior means and standard deviations
stanDat <- list(n = nrow(rDat),
                x = rDat$friendly)

# Fit Stan model
choiceFit <- stan(file = "BehaviorMean.stan", data = stanDat,
                  iter = 2000, chains = 4)

mu_sigma <- summary(choiceFit, pars=c("mu", "sigma"))$summary[,"mean"]

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



# give faces new label names
face_names <- c("vignette" = "Vignette", "no vignette" = "No Vignette")

# plot figure
# plot figure
# calculate group means and sds for one of the variables

df3 <- data_sumstan(rDat, varname="friendly", groupnames = c("Vignette", "Pre.Post"))

lp <- ggplot(data = df3, aes(x = Pre.Post, y = friendly, group = Vignette)) +
  scale_x_discrete(limits=rev) +
  geom_line(size=1.25, color=c("#999999","#999999","#E69F00", "#E69F00")) + 
  geom_point(size = 3, color=c("#999999","#999999","#E69F00", "#E69F00")) + 
  ylim(0, 4) + 
  geom_errorbar(aes(ymin=lo, ymax=hi), width=.05, orientation=c('y','y'),
                color=c("#999999","#999999","#E69F00", "#E69F00")) +
   
  ylab("Rating") + 
  labs(title = "Friendly")

p1 <- lp  + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.background = element_blank(),
           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank(),
           axis.text=element_text(size=15),axis.title.y=element_text(size=18),plot.title=element_text(size=20, hjust=0.5))


df3 <- data_sumstan(rDat, varname="socialize", groupnames = c("Vignette", "Pre.Post"))

lp <- ggplot(data = df3, aes(x = Pre.Post, y = socialize, group = Vignette)) +
  scale_x_discrete(limits=rev) +
  geom_line(size=1.25, color=c("#999999","#999999","#E69F00", "#E69F00")) + 
  geom_point(size = 3, color=c("#999999","#999999","#E69F00", "#E69F00")) + 
  ylim(0, 4) + 
  geom_errorbar(aes(ymin=lo, ymax=hi), width=.05, orientation=c('y','y'),
                color=c("#999999","#999999","#E69F00", "#E69F00")) +
  
  ylab("Rating") + 
  labs(title = "Socialize")

p2 <- lp + scale_color_manual(labels=c("Negative", "Neutral"), values=c("#E69F00", "#999999", "#FF0000")) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank(),
        axis.text=element_text(size=15),axis.title.y=element_blank(),plot.title=element_text(size=20, hjust=0.5))

df3 <- data_sumstan(rDat, varname="trustworthy", groupnames = c("Vignette", "Pre.Post"))

lp <- ggplot(data = df3, aes(x = Pre.Post, y = trustworthy, group = Vignette)) +
  scale_x_discrete(limits=rev) +
  geom_line(size=1.25, color=c("#999999","#999999","#E69F00", "#E69F00")) + 
  geom_point(size = 3, color=c("#999999","#999999","#E69F00", "#E69F00")) + 
  ylim(0, 4) + 
  geom_errorbar(aes(ymin=lo, ymax=hi), width=.05, orientation=c('y','y'),
                color=c("#999999","#999999","#E69F00", "#E69F00")) +
  
  #ylab("Rating") + 
  labs(title = "Trustworthy")

p3 <- lp + scale_color_manual(labels=c("Negative", "Neutral"), values=c("#E69F00", "#999999", "#FF0000")) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank(),
        axis.text=element_text(size=15),axis.title.y=element_blank(),plot.title=element_text(size=20, hjust=0.5))

df3 <- data_sumstan(rDat, varname="warmth", groupnames = c("Vignette", "Pre.Post"))

lp <- ggplot(data = df3, aes(x = Pre.Post, y = warmth, group = Vignette)) +
  scale_x_discrete(limits=rev) +
  geom_line(size=1.25, color=c("#999999","#999999","#E69F00", "#E69F00")) + 
  geom_point(size = 3, color=c("#999999","#999999","#E69F00", "#E69F00")) + 
  ylim(0, 4) + 
  geom_errorbar(aes(ymin=lo, ymax=hi), width=.05, orientation=c('y','y'),
                color=c("#999999","#999999","#E69F00", "#E69F00")) +
  
  ylab("Rating") + 
  labs(title = "Warmth")

p4 <- lp + scale_color_manual(labels=c("Negative", "Neutral"), values=c("#E69F00", "#999999", "#FF0000")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank(),
        axis.text=element_text(size=15),axis.title.y=element_text(size=18),plot.title=element_text(size=20, hjust=0.5))

df3 <- data_sumstan(rDat, varname="problems", groupnames = c("Vignette", "Pre.Post"))

lp <- ggplot(data = df3, aes(x = Pre.Post, y = problems, group = Vignette)) +
  scale_x_discrete(limits=rev) +
  geom_line(size=1.25, color=c("#999999","#999999","#E69F00", "#E69F00")) + 
  geom_point(size = 3, color=c("#999999","#999999","#E69F00", "#E69F00")) + 
  ylim(0, 4) + 
  geom_errorbar(aes(ymin=lo, ymax=hi), width=.05, orientation=c('y','y'),
                color=c("#999999","#999999","#E69F00", "#E69F00")) +
  
  #ylab("Rating") + 
  labs(title = "Problems")

p5 <- lp + scale_color_manual(labels=c("Negative", "Neutral"), values=c("#E69F00", "#999999", "#FF0000")) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank(),
        axis.text=element_text(size=15),axis.title.y=element_blank(),plot.title=element_text(size=20, hjust=0.5))

g <- plot_grid(p1,p2,p3,p4,p5, labels = NULL, align = "h")
g
ggsave("Study1a-Ratings-Bayes.pdf", g)



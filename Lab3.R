library(Sleuth3)
library(ggplot2)

names(case0301)

qplot(Treatment, Rainfall, data=case0301, geom="boxplot")

library(gridExtra)
set.seed(2021)
log.X<-rnorm(5000,0.6,0.5)  
X<-exp(log.X)
theme_set(theme_gray(base_size = 24))
plot1 <- qplot(X,geom="histogram",xlab="Y")
plot2 <- qplot(log.X,geom="histogram",xlab="log(Y)")
grid.arrange(plot1, plot2, ncol=2)

qplot(Treatment, log(Rainfall), data=case0301, geom="boxplot")

with(case0301, summary(Treatment)) # Check to see which group R puts first.
t.test(log(Rainfall)~Treatment, data=case0301, var.equal=TRUE, 
       alternative="greater")

t.test(log(Rainfall)~Treatment, data=case0301, var.equal=TRUE)

exp(0.2408651)
exp(2.0466973)

logit.X<-rnorm(5000,-0.4,1.25)  
X<-1/(1+exp(-logit.X))
plot1 <- qplot(X,geom="histogram",xlab="Y")
plot2 <- qplot(logit.X,geom="histogram",xlab="logit(Y)")
grid.arrange(plot1, plot2, ncol=2)

C_dalli <- read.csv("C:/Users/welab/Downloads/C_dalli.csv")

View(C_dalli)

qplot(as.factor(time_point), pct_cover, data=C_dalli, geom="boxplot")

logit_pct <- with(C_dalli, log((pct_cover)/(100-pct_cover)))

qplot(as.factor(time_point), logit_pct, data=C_dalli, geom="boxplot")

head(case0302)
qplot(Veteran, Dioxin, data=case0302, geom="boxplot")

plot(Dioxin~Veteran, data=case0302)
with(case0302, identify(x=Veteran, y=Dioxin))

summary(case0302$Veteran) # Check R's ordering of the groups.
t.test(Dioxin~Veteran, data=case0302, var.equal=TRUE, alternative="less")

t.test(Dioxin~Veteran, data=case0302, var.equal=TRUE, alternative="less",
       subset=-646)

t.test(Dioxin~Veteran, data=case0302, var.equal=TRUE, alternative="less",
       subset=-c(646,645))

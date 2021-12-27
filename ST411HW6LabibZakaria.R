HW6Dat <- read.csv("amoeba.csv")
(HW6Dat_aov <- aov(Yield~Treatment, data=HW6Dat))
anova(HW6Dat_aov)
with(HW6Dat, unlist(lapply(split(Yield, Treatment), mean)))
with(HW6Dat,sd(Yield))
with(HW6Dat,mean(Yield))
(HW6Date_aov <- aov(Yield~1, data=HW6Dat))
anova(HW6Date_aov)
63524.08-43858
5-1
49-45
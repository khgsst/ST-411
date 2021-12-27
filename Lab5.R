library(Sleuth3)
library(ggplot2)

View(case0501)

summary(case0501$Diet)

qplot(Diet, Lifetime, data=case0501, geom="boxplot")

case0501_aov <- aov(Lifetime~Diet, data=case0501)

case0501_aov

anova(case0501_aov)

t.test(Depth~Year, data=case0201, var.equal=TRUE)

case0201_aov <- aov(Depth~Year, data=case0201)
anova(case0201_aov)

(-4.5833)^2

(s1 <- with(case0201, sd(Depth[Year==1976])))
(s2 <- with(case0201, sd(Depth[Year==1978])))
(n1 <- with(case0201, length(Depth[Year==1976])))
(n2 <- with(case0201, length(Depth[Year==1978])))
(sp <- sqrt(((n1-1) * s1^2 + (n2-1) * s2^2)/(n1 + n2 - 2)))

sp^2

166.638/176 

with(case0501, unlist(lapply(split(Lifetime, Diet), mean)))

with(case0501, unlist(lapply(split(Lifetime, Diet), length)))

qt(0.975, 343)

42.29718-39.68571 - qt(0.975, 343)*sqrt(44.6)*sqrt(1/71 + 1/56)
42.29718-39.68571 + qt(0.975, 343)*sqrt(44.6)*sqrt(1/71 + 1/56)

anova(case0501_aov)

nrow(case0501) # Find total sample size
length(unique(case0501$Diet)) # How many different groups?

qplot(Diet, Lifetime, data=case0501, geom="boxplot") + 
  stat_summary(fun=mean, geom="point", shape=3, size=3)

2546.8/44.6

library(Sleuth3)
library(ggplot2)

case0701_lm <- lm(Distance~Velocity, data=case0701)

summary(case0701_lm)

0.3991704/0.1186662

2*(1-pt(3.363809,22))

qt(0.975, 22)

0.3991704 - qt(0.975, 22)*0.1186662
0.3991704 + qt(0.975, 22)*0.1186662

confint(case0701_lm)

confint(case0701_lm, level=0.9)

0.0013724 - qt(0.975, 22)*0.0002278
0.0013724 + qt(0.975, 22)*0.0002278

0.0013724/0.0002278

2*(1-pt(6.024583,22))

case0701_noint <- lm(Distance~Velocity-1, data=case0701)
summary(case0701_noint)

0.0019214 - qt(.975, 23)*0.0001913
0.0019214 + qt(.975, 23)*0.0001913

confint(case0701_noint)

plot(case0701_lm, which=2)

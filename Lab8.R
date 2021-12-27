library(Sleuth3)
library(ggplot2)

head(case0701)
qplot(Velocity, Distance, data=case0701)

is.numeric(case0701$Velocity)

case0701_lm <- lm(Distance~Velocity, data=case0701)

summary(case0701_lm)

confint(case0701_lm)

case0701_lm$coefficients

qplot(Velocity,Distance, data=case0701) + 
  geom_abline(slope=case0701_lm$coefficients["Velocity"], 
              intercept=case0701_lm$coefficients["(Intercept)"])

qplot(Velocity,Distance, data=case0701) + 
  geom_abline(slope=0.001372408, 
              intercept=0.3991704)

ggplot(case0701, aes(x=Velocity, y=Distance)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)

pred_intervals <- data.frame(predict(case0701_lm, interval="prediction"))

head(pred_intervals)

case0701_df2 <- cbind(case0701, pred_intervals)
head(case0701_df2)

ggplot(case0701_df2, aes(x=Velocity, y=Distance)) +
  geom_point() +
  geom_smooth(method=lm) +
  geom_line(aes(y=lwr), color = "blue", linetype = "dashed") +
  geom_line(aes(y=upr), color = "blue", linetype = "dashed")

predict(case0701_lm, data.frame(Velocity=c(600,-200)), 
        interval="confidence")

head(case0701_lm$residuals)

plot(case0701_lm, which=1)

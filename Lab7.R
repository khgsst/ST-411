library(Sleuth3)

case0601_aov<-aov(Score~Handicap, data=case0601)
anova(case0601_aov)
with(case0601, unlist(lapply(split(Score,Handicap), mean)))
with(case0601, unlist(lapply(split(Score,Handicap), length)))

qtukey(0.95, 5, 65)/sqrt(2)

TukeyHSD(case0601_aov)

library(xtable)
print(xtable(TukeyHSD(case0601_aov)$Handicap, 
             caption="95\\% Tukey Confidence Intervals"), 
      comment=FALSE, caption.placement="top")

library(multcomp)

summary(case0601$Handicap) # Check the original ordering.
case0601$Handicap <- relevel(case0601$Handicap, "None") # Put None first.
summary(case0601$Handicap) # Check to make sure of the order.

case0601_aov <- aov(Score~Handicap, data=case0601)

case0601_glht<- glht(case0601_aov, linfct=mcp(Handicap="Dunnett"))
confint(case0601_glht)

print(xtable(confint(case0601_glht)$confint, 
             caption="95\\% Dunnett's Confidence Intervals"), 
      comment=FALSE, caption.placement="top")

qf(0.95, 4, 65)

(M<-sqrt(4 * qf(0.95, 4, 65)))

SE <- sqrt(2.6665) * sqrt((0.5)^2/14 + (0.5)^2/14 + (0.5)^2/14 + (0.5)^2/14)
(5.921429+5.342857 )/2 - (4.428571+4.05)/2 - M*SE
(5.921429+5.342857 )/2 - (4.428571+4.05)/2 + M*SE

alpha <- 0.05/3 # Set Bonferroni alpha to nominal alpha divided by k.
(M <- qt(1-alpha/2, 65))

pt_est <- 4.9 - (4.428571+5.921429+4.05+5.342857)/4
SE <- sqrt(2.6665)*sqrt(1/14 + 4*(0.25)^2/14)
pt_est - M*SE
pt_est + M*SE

Bonf_table <- data.frame(
  row.names=c("None - avg. of the others",
              "Hearing - avg. of Amputee, Crutches, and Wheelchair",
              "Crutches - avg. of Amputee, Hearing, and Wheelchair"),
  lwr=c(-1.23, -2.42, 0.08),
  upr=c(1.16, 0.06, 2.55))
print(xtable(Bonf_table, 
             caption="95\\% Bonferroni Confidence Intervals"), 
             comment=FALSE, caption.placement="top")

qt(1-(0.05/10)/2, 65)

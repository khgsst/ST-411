sf <- with(ex0112, sd(BP[Diet=="FishOil"]))
sr <- with(ex0112, sd(BP[Diet=="RegularOil"]))
rs <-sf/sr
t.test(BP~Diet, data=ex0112,alternative="greater")
t.test(BP~Diet, data=ex0112)
ex0318 <- data.frame(
  Metabol=c(20.1, 22.9, 18.8, 20.9, 20.9, 22.7, 21.4,
              20, 38.5, 25.8, 22, 23, 37.6, 30, 24.5),
  Group=c(rep("Nontrauma", times=8), rep("Trauma", times=7)))
with(ex0318,wilcox.test(x=Metabol[Group=="Trauma"],
                      y=Metabol[Group=="Nontrauma"],
                      alternative="greater",
                      correct=TRUE,
                      exact=FALSE))
binom.test(4,9)


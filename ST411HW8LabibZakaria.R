head(ex0727)
ex0727_lm<-lm(Tcell~Mass, data=ex0727)
summary(ex0727_lm)
ggplot(ex0727, aes(x=Mass, y=Tcell)) +
  geom_point() +
  geom_smooth(method=lm)+
  xlab("Mean Stone Mass (g)")+
    ylab("T-cell Response (mm)")
c(0.08750-qt(1-.05/2,19)*0.07868,0.08750+qt(1-.05/2,19)*0.07868)
c(0.03282-qt(1-.05/2,19)*0.01064,0.03282+qt(1-.05/2,19)*0.01064)
predict(ex0727_lm, data.frame(Mass=5), se.fit=TRUE, 
        interval="confidence")
predict(ex0727_lm, data.frame(Mass=6), se.fit=TRUE, 
        interval="prediction")
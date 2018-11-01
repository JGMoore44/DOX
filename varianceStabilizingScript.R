#Enter data
trt1 = c(31,10,21,4,1)
trt2 = c(62,40,24,30,35)
trt3 = c(53,27,120,97,68)
semiCondData = data.frame(method = factor(c(rep(1,5),rep(2,5),rep(3,5))),
                          count = c(trt1,trt2,trt3))

anova(aov(count~method,data = semiCondData))

plot(lm(count~method,data = semiCondData),which = 1:2)

figureOutAlpha = data.frame(y = c(log(sd(trt1)),log(sd(trt2)),log(sd(trt3))),
                            x = c(log(mean(trt1)),log(mean(trt2)),log(mean(trt3))))
plot(figureOutAlpha$x,figureOutAlpha$y, 
     xlab = "log(mean(yi))",
     ylab = "log(Si)")
abline(lm(y~x,data = figureOutAlpha))

transSemi = semiCondData
transSemi$count = sqrt(transSemi$count)

TukeyHSD(aov(count~method,data = transSemi))
plot(lm(count~method,data = transSemi),which = 1)

library(dplyr, warn.conflicts = F)
data("mtcars")
df = mtcars
str(df) # or use glimpse(df)
df1 <- select(df,-c(cyl,vs,am,gear,carb))
mlr1 <-lm(mpg~.,data = df1)
summary(mlr1)
par(mfrow=c(2,2))
plot(mlr1)
dev.off() # to reset grephic parameter to default
lmtest::bptest(mlr1)  # Breusch-Pagan test
car::ncvTest(mlr1)    #NCV Test
car::vif(mlr1)
mlr2 <-update(mlr1,~.-qsec-disp)
summary(mlr2)
mlr3 <- update(mlr2,~.-drat)
summary(mlr3)

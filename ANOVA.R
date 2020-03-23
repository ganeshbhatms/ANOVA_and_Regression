# One-Way ANOVA
#setwd('D:/your_working_directory') set your working directory if needed
library(dplyr)
library(ggplot2)

data("mtcars")
df = mtcars
dim(df)
str(df) #glimpse(df)
sapply(df, class)
sapply(df[c('cyl','vs','am','gear','carb')], table)

df %>% mutate_at(vars('cyl','vs','am','gear','carb'),as.factor) ->df #df = mutate_at(df,vars('cyl','vs','am','gear','carb'),as.factor)
df %>% filter(carb!=6 & carb!=8) ->df1 #df[-c(which(df$carb==6),which(df$carb==8)),] -> df1

ggplot(data = df1,aes(x = mpg))+
  geom_histogram(binwidth = 4,aes(y = ..density..),colour='black',fill='white')+
  geom_density(colour='black',fill='red',alpha = 0.3, linetype = 'dashed')

ggplot(data = df1,aes(sample=mpg))+
  stat_qq()+
  stat_qq_line()

shapiro.test(df1$mpg)  #accept null hypothesis mpg is normal

#using base function
hist(df1$mpg,prob=T,ylim = c(0,0.08),breaks=5)
lines(density(df1$mpg))
qqnorm(df1$mpg)
qqline(df1$mpg)

#effect of cylinder on mileage
df1 %>%  group_by(cyl) %>% summarise(count = n(),
                                     mean = mean(mpg,na.rm = T),
                                     sd = sd(mpg,na.rm = T))

ggplot(data=df1,aes(x=cyl,y=mpg,fill=cyl))+
  geom_boxplot(outlier.colour = 'red')+
  ggtitle('Boxplot of mpg vs cyl')
#as number of cyllinder increases mileage decreases,let's check this by using ANOVA model
cyl.aov <- aov(mpg~cyl,data=df1)
summary(cyl.aov)
TukeyHSD(cyl.aov)
pairwise.t.test(df1$mpg,df1$cyl,p.adjust.method = 'BH')
plot(cyl.aov,1)
shapiro.test(cyl.aov$residuals)   #Shapiro-Wilk normality test
bartlett.test(mpg~cyl,data=df1)   #homogeneity of variances
oneway.test(mpg~cyl,data=df1)     
kruskal.test(mpg~cyl,data=df1)    #Kruskal-Wallis rank sum test, if mpg does not follows normality assumtion

#effect of transmission(am) on mileage
df1 %>%  group_by(am) %>% summarise(count = n(),
                                     mean = mean(mpg,na.rm = T),
                                     sd = sd(mpg,na.rm = T))
ggplot(data=df1,aes(am,mpg,fill=am))+
  geom_boxplot(outlier.colour = 'red',fill='chocolate',notch = T)+
  ggtitle('Boxplot of mpg vs am')
am.aov <- aov(mpg~am,data=df1)
summary(am.aov)
plot(am.aov,1)
shapiro.test(am.aov$residuals)   #Shapiro-Wilk normality test
bartlett.test(mpg~am,data=df1)   #homogeneity of variances


#effect of carborator on mileage
df1 %>%  group_by(carb) %>% summarise(count = n(),
                                     mean = mean(mpg,na.rm = T),
                                     sd = sd(mpg,na.rm = T))

ggplot(data=df1,aes(x=carb,y=mpg,fill=carb))+
  geom_boxplot(outlier.colour = 'red')+
  ggtitle('Boxplot of mpg vs carb')
#cars having 1 and 2 carborator and 3 and 4 craborator have no difference in mpg,let's check this by using ANOVA model
carb.aov <- aov(mpg~carb,data=df1)
summary(carb.aov)
TukeyHSD(carb.aov)
pairwise.t.test(df1$mpg,df1$carb,p.adjust.method = 'BH')
plot(carb.aov,1)
shapiro.test(carb.aov$residuals)   #Shapiro-Wilk normality test
bartlett.test(mpg~carb,data=df1)   #homogeneity of variances

'''_________________________ One-way ANOVA_________________________'''
library(dplyr) 
library(ggplot2) #for visualizing the data

data("mtcars")
?mtcars  #To get more information about the dataset and variables
df = mtcars
dim(df)
str(df)
summary(df)
'''cyl,vs,am,gear,carb are categorical variables. ANOVA moddel can be fit to these'''
sapply(df[c('cyl','vs','am','gear','carb')], table)   #df %>% select(cyl,vs:carb) %>% sapply(table)

'''only two cars have 6 carborators and 8 carborators each, 
It may affect the model so we delete those two observation from the data'''
df %>% mutate_at(vars('cyl','vs','am','gear','carb'),as.factor) ->df #df <- mutate_at(df,vars('cyl','vs','am','gear','carb'),as.factor)
df %>% filter(carb!=6 & carb!=8) ->df1   #df[-c(which(df$carb==6),which(df$carb==8)),] -> df1

'''============================================================================================================================='''
'''_________________________Checking for normality of responce variable mpg_________________________'''
'''#using base function
hist(df1$mpg,prob=T,ylim = c(0,0.08),breaks=5)
lines(density(df1$mpg))
qqnorm(df1$mpg)
qqline(df1$mpg)'''

#using ggplot2
ggplot(data = df1,aes(x = mpg))+
  geom_histogram(binwidth = 4,aes(y = ..density..),colour='black',fill='white')+
  geom_density(colour='black',fill='red',alpha = 0.3, linetype = 'dashed')
ggplot(data = df1,aes(sample=mpg))+
  stat_qq()+
  stat_qq_line()
shapiro.test(df1$mpg)  
'''accept null hypothesis, mpg is normal because p-value is greater than 0.05'''


'''_________________________Effect of cylinder on mileage_________________________'''
df1 %>%  group_by(cyl) %>% summarise(count = n(),
                                     mean = mean(mpg,na.rm = T),
                                     sd = sd(mpg,na.rm = T))

ggplot(data=df1,aes(x=cyl,y=mpg,fill=cyl))+
  geom_boxplot(outlier.colour = 'red')+
  ggtitle('Boxplot of mpg vs cyl')
''' as number of cyllinder increases mileage decreases,
let\'s check this by using ANOVA model'''

cyl.aov <- aov(mpg~cyl,data=df1)
summary(cyl.aov)
'''# here p-value is less than 0.05,we reject null hypothesis,
therefore there is a significant difference in mean among the three groups'''
pairwise.t.test(df1$mpg,df1$cyl,p.adjust.method = 'BH')
TukeyHSD(cyl.aov)    # to see the difference of means among the groups

# model adequecy
shapiro.test(cyl.aov$residuals)   #Shapiro-Wilk normality test, residuals are normally distributed
bartlett.test(mpg~cyl,data=df)   #there is no homogeneity of variances
oneway.test(mpg~cyl,data=df)   #One-way analysis of means (not assuming equal variances), there is difference in mean among thegroups


'''_________________________Effect of transmission(am) on mileage_________________________'''
df1 %>%  group_by(am) %>% summarise(count = n(),
                                     mean = mean(mpg,na.rm = T),
                                     sd = sd(mpg,na.rm = T))

ggplot(data=df1,aes(am,mpg,fill=am))+
  geom_boxplot(outlier.colour = 'red',fill='chocolate',notch = T)+
  ggtitle('Boxplot of mpg vs am')

am.aov <- aov(mpg~am,data=df1)
summary(am.aov)
''' the p-value is less than 0.05, there is significance difference in mean among two groups'''

# Checking model adequecy
plot(am.aov,1)  #points are evenly distributed across zero line
shapiro.test(am.aov$residuals)   #Shapiro-Wilk normality test, residuals are normally distributed
bartlett.test(mpg~am,data=df)    #homogeneity of variances

'''_________________________Effect of carborator on mileage_________________________'''
df1 %>%  group_by(carb) %>% summarise(count = n(),
                                     mean = mean(mpg,na.rm = T),
                                     sd = sd(mpg,na.rm = T))
ggplot(data=df1,aes(x=carb,y=mpg,fill=carb))+
  geom_boxplot(outlier.colour = 'red')+
  ggtitle('Boxplot of mpg vs carb')
'''cars having 1 and 2 carborator and 3 and 4 craborator have no difference in mpg,
let\'s check this by using ANOVA model'''

carb.aov <- aov(mpg~carb,data=df1)
summary(carb.aov) 
'''the p-value is less than 0.05, there is significance difference in mean among two groups'''
TukeyHSD(carb.aov)
pairwise.t.test(df1$mpg,df1$carb,p.adjust.method = 'BH') # pairwise comparision
'''there is no significant difference in mean among the cars having 1 and 2 carborators, 
2 and 3  carborators, and 3 and 4 carborators
'''
# Checking model adequecy
plot(carb.aov,1)   #points are evenly distributed across zero line
shapiro.test(carb.aov$residuals)   #Shapiro-Wilk normality test, residuals are normally distributed
bartlett.test(mpg~carb,data=df1)   #homogeneity of variances among the groups

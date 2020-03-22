<font><h1 align='center'> One-way ANOVA</h1></font>
```r
library(dplyr) 
library(ggplot2)
```
I have used _mtcars_ dataset available in R, to get more information about the dataset use comand ```?mtcars```
```r
df = mtcars
dim(df)
str(df)
summary(df)
```
#### Converting numeric to factor

_cyl,vs,am,gear_ and _carb_ are categorical variables. when we see the structure of dataset these variables are numeric, let convert them into factors. Only two cars have 6 carborators and 8 carborators each, It may affect the model so we delete those two observation from the dataset
```r
sapply(df[c('cyl','vs','am','gear','carb')], table)
df %>% mutate_at(vars('cyl','vs','am','gear','carb'),as.factor) ->df # or df <- mutate_at(df,vars('cyl','vs','am','gear','carb'),as.factor)
df %>% filter(carb!=6 & carb!=8) ->df1   # or df[-c(which(df$carb==6),which(df$carb==8)),] -> df1
```
#### Checking for normality of responce variable mpg
```r
ggplot(data = df1,aes(x = mpg))+
  geom_histogram(binwidth = 4,aes(y = ..density..),colour='black',fill='white')+
  geom_density(colour='black',fill='red',alpha = 0.3, linetype = 'dashed')
ggplot(data = df1,aes(sample=mpg))+
  stat_qq()+
  stat_qq_line()
shapiro.test(df1$mpg)
```
Here p-value is greater than 0.05,so we accept the null hypothesis that mpg is normally distributed.
#### Effect of cylinder on mileage
```r
df1 %>%  group_by(cyl) %>% summarise(count = n(),
                                     mean = mean(mpg,na.rm = T),
                                     sd = sd(mpg,na.rm = T))
```
|cyl|count|mean|sd|
|:---:|:---:|:---:|:---:|
|4  |11|26.7|4.51|
|6|6|19.8|1.59|
|8|13|15.1|2.66|
```r
ggplot(data=df1,aes(x=cyl,y=mpg,fill=cyl))+
  geom_boxplot(outlier.colour = 'red')+
  ggtitle('Boxplot of mpg vs cyl')
```
from the above graph we can say than, as number of cyllinder increases mileage decreases, let's check this by using ANOVA model
```r
cyl.aov <- aov(mpg~cyl,data=df1)
summary(cyl.aov)
```
```
            Df Sum Sq Mean Sq F value   Pr(>F)    
cyl          2  797.7   398.9   35.75 2.58e-08 ***
Residuals   27  301.2    11.2                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
here p-value is less than 0.05,we reject null hypothesis, therefore there is a significant difference in mean among the three groups
```r
pairwise.t.test(df1$mpg,df1$cyl,p.adjust.method = 'BH')
```
```
      	Pairwise comparisons using t tests with pooled SD 

data:  df1$mpg and df1$cyl 

  4       6      
6 0.00054 -      
8 1.4e-08 0.00898

P value adjustment method: BH 
```
below we can see the difference in means among the groups
```r
TukeyHSD(cyl.aov)
```
```
      Tukey multiple comparisons of means
       95% family-wise confidence level

Fit: aov(formula = mpg ~ cyl, data = df1)

$cyl
          diff        lwr        upr     p adj
6-4  -6.913636 -11.116871 -2.7104020 0.0010165
8-4 -11.555944 -14.948826 -8.1630624 0.0000000
8-6  -4.642308  -8.729834 -0.5547809 0.0236113
```
Checking model adequecy:
1. Checking normality assumption for residuals
```r
shapiro.test(cyl.aov$residuals)
```
```
	Shapiro-Wilk normality test

data:  cyl.aov$residuals
W = 0.97194, p-value = 0.5935
```
Here p-value is greater than 0.05, we accept null hypothesis, so the residuals are normally distributed. \
2. Residual v/s fitted curve:
```r
Plot(cyl.aov,1)
```
Data points are equally distributed on both side of the zero line, therefore the model is good fit.\
3. Bartlett test of homogeneity of variances: 
```r
bartlett.test(mpg~cyl,data=df)
```
```
	Bartlett test of homogeneity of variances

data:  mpg by cyl
Bartlett's K-squared = 8.3934, df = 2, p-value = 0.01505
```
Here p-value is less than 0.05, so we reject the null hypothesis of homogenity of variances.\
we go for the below test which does not assumes the homogenity of variances.
```r
   	One-way analysis of means (not assuming equal variances)

data:  mpg and cyl
F = 31.624, num df = 2.000, denom df = 18.032, p-value = 1.271e-06
```
here p-value is less than 0.05,we reject null hypothesis, therefore there is a significant difference in mean among the three groups


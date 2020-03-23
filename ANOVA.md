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
### Converting numeric to factor

_cyl,vs,am,gear_ and _carb_ are categorical variables. when we see the structure of dataset these variables are numeric, let convert them into factors. Only two cars have 6 carborators and 8 carborators each, It may affect the model so we delete those two observation from the dataset
```r
sapply(df[c('cyl','vs','am','gear','carb')], table)
df %>% mutate_at(vars('cyl','vs','am','gear','carb'),as.factor) ->df # or df <- mutate_at(df,vars('cyl','vs','am','gear','carb'),as.factor)
df %>% filter(carb!=6 & carb!=8) ->df1   # or df[-c(which(df$carb==6),which(df$carb==8)),] -> df1
```
### Checking for normality of responce variable mpg
```r
ggplot(data = df1,aes(x = mpg))+
  geom_histogram(binwidth = 4,aes(y = ..density..),colour='black',fill='white')+
  geom_density(colour='black',fill='red',alpha = 0.3, linetype = 'dashed')
```
![](../master/images/nomallity.png)
```r
ggplot(data = df1,aes(sample=mpg))+
  stat_qq()+
  stat_qq_line()
```
![](../master/images/qqplot.png)
```r
shapiro.test(df1$mpg)
```
```
	Shapiro-Wilk normality test

data:  df1$mpg
W = 0.95255, p-value = 0.1978
```
Here p-value is greater than 0.05, so we accept the null hypothesis that mpg is normally distributed.
### Effect of cylinder(cyl) on mileage(mpg)
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
![](../master/images/boxplotcyl.png)\
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
**Checking model adequecy:**
1. Checking normality assumption for residuals
```r
shapiro.test(cyl.aov$residuals)
```
```
	Shapiro-Wilk normality test

data:  cyl.aov$residuals
W = 0.97194, p-value = 0.5935
```
Here p-value is greater than 0.05, we accept null hypothesis, so the residuals are normally distributed.

2. Residual v/s fitted curve
```r
Plot(cyl.aov,1)
```
![](../master/images/residualcyl.png) \
Data points are equally distributed on both side of the zero line, therefore the model is good fit.

3. Bartlett test of homogeneity of variances
```r
bartlett.test(mpg~cyl,data=df1)
```
```
	Bartlett test of homogeneity of variances

data:  mpg by cyl
Bartlett's K-squared = 6.4583, df = 2, p-value = 0.03959
```
Here p-value is less than 0.05, so we reject the null hypothesis of homogenity of variances.\
we go for the below test which does not assumes the homogenity of variances.
```r
oneway.test(mpg~cyl,data=df1)
```
```
	One-way analysis of means (not assuming equal variances)

data:  mpg and cyl
F = 29.102, num df = 2.000, denom df = 16.671, p-value = 3.649e-06
```
here p-value is less than 0.05,we reject null hypothesis, therefore there is a significant difference in mean among the three groups
### Effect of transmission(am) on mileage(mpg)
```r
df1 %>%  group_by(am) %>% summarise(count = n(),
                                     mean = mean(mpg,na.rm = T),
                                     sd = sd(mpg,na.rm = T))
```
```
|cyl|count|mean|sd|
|:---:|:---:|:---:|:---:|
|0|19|17.1|3.83|
|1|11|25.7|5.73|
```
Let us use boxplot to visualize the effect of transmission on mileage
```r
ggplot(data=df1,aes(am,mpg,fill=am))+
  geom_boxplot(outlier.colour = 'red',fill='chocolate',notch = T)+
  ggtitle('Boxplot of mpg vs am')
```
![](../master/images/boxplotam.png) \
Here we can see than mean of mileage for manual transmission is more than that of automatic transmission, let us check this by using anova model.
```r
am.aov <- aov(mpg~am,data=df1)
summary(am.aov)
```
```
            Df Sum Sq Mean Sq F value   Pr(>F)    
am           1  506.3   506.3   23.92 3.73e-05 ***
Residuals   28  592.6    21.2                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
For the above model p-value is less than 0.05,so we can reject the null hypothsis and concluide that there is significant difference in mean of mileage among the two groups.
**Checking model adequecy:**
1. Checking normality assumption for residuals
```r
shapiro.test(am.aov$residuals) 
```
```
	Shapiro-Wilk normality test

data:  am.aov$residuals
W = 0.98294, p-value = 0.8971
```
Here p-value is greater than 0.05, we accept null hypothesis and concluide that the residuals are normally distributed.

2. Residual v/s fitted curve
```r
Plot(am.aov,1)
```
![](../master/images/residualsam.png) \
Data points are equally distributed on both side of the zero line, therefore the model is good fit.

3. Bartlett test of homogeneity of variances
```r
bartlett.test(mpg~am,data=df1)
```
```
	Bartlett test of homogeneity of variances

data:  mpg by am
Bartlett's K-squared = 2.0965, df = 1, p-value = 0.1476
```
Here p-value is greater than 0.05, so we do not reject the null hypothesis of homogenity of variances.
### Effect of carborator(carb) on mileage(mpg)
```r
df1 %>%  group_by(carb) %>% summarise(count = n(),
                                     mean = mean(mpg,na.rm = T),
                                     sd = sd(mpg,na.rm = T))
```
```
|cyl|count|mean|sd|
|:---:|:---:|:---:|:---:|
|1|7|25.3|6.00|
|2|10|22.4|5.47|
|3|3|16.3|1.05|
|4|10|15.8|3.91|
```
Let us use boxplot to visualize the effect of carborators on mileage
```r
ggplot(data=df1,aes(x=carb,y=mpg,fill=carb))+
  geom_boxplot(outlier.colour = 'red')+
  ggtitle('Boxplot of mpg vs carb')
```
![](../master/images/boxplotcarb.png) \
Here we can see that mileage decreases as carborator increases, let us check this by using anova model.
```r
carb.aov <- aov(mpg~carb,data=df1)
summary(carb.aov)
```
```
summary(carb.aov)
            Df Sum Sq Mean Sq F value  Pr(>F)   
carb         3  473.5  157.83   6.561 0.00189 **
Residuals   26  625.5   24.06                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
For the above model p-value is less than 0.05,so we can reject the null hypothsis and concluide that there is significant difference in mean of mileage among the groups.
**Checking model adequecy:**
1. Checking normality assumption for residuals
```r
shapiro.test(carb.aov$residuals) 
```
```
	Shapiro-Wilk normality test

data:  carb.aov$residuals
W = 0.95944, p-value = 0.2998
```
Here p-value is greater than 0.05, we accept null hypothesis and concluide that the residuals are normally distributed.

2. Residual v/s fitted curve
```r
Plot(carb.aov,1)
```
![](../master/images/residualcarb.png) \
Data points are equally distributed on both side of the zero line, therefore the model is good fit.

3. Bartlett test of homogeneity of variances
```r
bartlett.test(mpg~carb,data=df1)
```
```
	Bartlett test of homogeneity of variances

data:  mpg by carb
Bartlett's K-squared = 5.3321, df = 3, p-value = 0.149
```
Here p-value is greater than 0.05, so we do not reject the null hypothesis of homogenity of variances.
### Findings
* As number of cyllinder increases mileage of cars decreases.
* Car with manual transmission has more mileage than with automatic transmission
* There is no difference in mileage of cars having one and two crborators and cars of three and four carborators also got no difference in mileage, but cars with one or two carborators have more mileage thane the otheres
### Conclusion
A car with fours cyllinders, one or two carborators and of manual transmission may have more mileage.\
let us do some more analysis using regression to find the factors which are affecting the mileage of cars using regression model 

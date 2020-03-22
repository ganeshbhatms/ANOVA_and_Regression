# One-way ANOVA
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
_cyl,vs,am,gear_ and _carb_ are categorical variables.when we see the structure of dataset these variables are of numeric let convert them into factors. Only two cars have 6 carborators and 8 carborators each, It may affect the model so we delete those two observation from the dataset
```r
sapply(df[c('cyl','vs','am','gear','carb')], table)
df %>% mutate_at(vars('cyl','vs','am','gear','carb'),as.factor) ->df # or df <- mutate_at(df,vars('cyl','vs','am','gear','carb'),as.factor)
df %>% filter(carb!=6 & carb!=8) ->df1   # or df[-c(which(df$carb==6),which(df$carb==8)),] -> df1
```

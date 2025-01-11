##EMERGE##

library(dplyr)
load("\\Users\\brend\\Downloads\\data")
ls
View(tmpDat)
## Creating binary column for Insulin ##
tmpDat$binary_insulin<-0
tmpDat$binary_insulin[thesis.df$InsulinYN == "Yes"]<-1

thesis.df<-tmpDat

## T-values and Z-scores for the original study ##
StudyInsulinTest<- prop.test(x = c(sum(tmpDat$InsulinYN[tmpDat$Group== "Metformin"] == "Yes", na.rm=T),
                              sum(tmpDat$InsulinYN[tmpDat$Group== "Placebo"] == "Yes", na.rm=T)),
                        n = c(sum(tmpDat$Group == "Metformin"),sum(tmpDat$Group == "Placebo")))
z_score_squared_insulin<-sqrt(StudyInsulinTest$statistic)       ## This is not used, binary insulin more appropriate to use ##                       

insulin_t_value<-t.test(tmpDat$binary_insulin[tmpDat$Group == "Metformin"], tmpDat$binary_insulin[tmpDat$Group == "Placebo"])$statistic
gw32_t_value<-t.test(tmpDat$gw32[tmpDat$Group == "Metformin"], tmpDat$gw32[tmpDat$Group == "Placebo"])$statistic
gw38_t_value<-t.test(tmpDat$gw38[tmpDat$Group == "Metformin"], tmpDat$gw38[tmpDat$Group == "Placebo"])$statistic

##Composites for Original study##
##Squared##
study_squared_composite<-z_score_squared_insulin+(gw32_t_value^2)+(gw38_t_value^2)
##Not squared##
study_non_squared_composite<-sqrt(z_score_squared_insulin)+gw32_t_value+gw38_t_value

##Permutation Code for t-values for continuous data (gw32, gw38) and for z-score for binary data (InsulinYN) ##

## Z-score for Insulin ##
proportion_permutation_function<-function(x){
  permutation_chisquare<-numeric(10000)
  i <-1
  repeat{
    thesis.df$Group<-sample(thesis.df$Group, 535, replace = F)
    permutation_result<-prop.test(x = c(sum(x[thesis.df$Group== "Metformin"] == "Yes", na.rm=T),
                                        sum(x[thesis.df$Group== "Placebo"] == "Yes", na.rm=T)),
                                  n = c(sum(thesis.df$Group == "Metformin"),sum(thesis.df$Group == "Placebo"))) 
    permutation_chisquare[i]<-permutation_result$statistic
    i<- i +1
    if(i > 10000) break}
  return(permutation_chisquare)
}

summary(proportion_permutation_function(thesis.df$InsulinYN))
hist(proportion_permutation_function(thesis.df$InsulinYN),
     breaks = 500, main= "Histogram of Permuted Z-scores of Insulin comparing Treatment Groups",
     xlab= "Z-scores Insulin")

##T-values for gw32, gw 38 ##
continuous_permutation_function<-function(x){
  permutation_t_value<-numeric(10000)
  i <-1
  repeat{
    thesis.df$Group<-sample(thesis.df$Group, 535, replace = F)
    permutation_result<-t.test(x[thesis.df$Group == "Metformin"], x[thesis.df$Group == "Placebo"])
    permutation_t_value[i]<-permutation_result$statistic
    i<- i +1
    if(i > 10000) break}
  return(permutation_t_value)
}

summary(continuous_permutation_function(thesis.df$gw32))
hist(continuous_permutation_function(thesis.df$gw32),
     breaks = 500, main= "Histogram of Permuted t-values of gw32 comparing Treatment Groups",
     xlab= "T-values gw32")

summary(continuous_permutation_function(thesis.df$gw38))
hist(continuous_permutation_function(thesis.df$gw38),
     breaks = 500, main= "Histogram of Permuted t-values of gw38 comparing Treatment Groups",
     xlab= "T-values gw38")

## Creating the Composite ##
Zi+t32+t38
## The squared method ##

squared_composite<-((continuous_permutation_function(thesis.df$binary_insulin))^2) +
  ((continuous_permutation_function(thesis.df$gw32))^2) +
  ((continuous_permutation_function(thesis.df$gw38))^2)

## Non-squared method ##
non_squared_composite<-continuous_permutation_function(thesis.df$binary_insulin) +
  continuous_permutation_function(thesis.df$gw32) +
  continuous_permutation_function(thesis.df$gw38)

##P-value calculation##

##Individual variables##
p_value_gw32<-sum(continuous_permutation_function(thesis.df$gw32)<gw32_t_value)/
  length(continuous_permutation_function(thesis.df$gw32))
p_value_gw38<-sum(continuous_permutation_function(thesis.df$gw38)<gw38_t_value)/
  length(continuous_permutation_function(thesis.df$gw38))
p_value_binary_insulin<-sum(continuous_permutation_function(thesis.df$binary_insulin)<insulin_t_value)/
  length(continuous_permutation_function(thesis.df$binary_insulin))

##Composite P-values##
p_value_squared_composite<-sum(squared_composite>study_squared_composite)/
  length(squared_composite)
p_value_non_squared_composite<-sum(non_squared_composite<study_non_squared_composite)/
  length(non_squared_composite)
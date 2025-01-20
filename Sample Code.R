##EMERGE##

library(dplyr)
load("\\Users\\brend\\Downloads\\data")
ls
View(tmpDat)
library(ggplot2)
library(hrbrthemes)
## Creating binary column for Insulin ##
tmpDat$binary_insulin<-0
tmpDat$binary_insulin[tmpDat$InsulinYN == "Yes"]<-1

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

##Permutation Code for t-values for continuous data (gw32, gw38, binary_insulin) ##


continuous_permutation_function<-function(x){
  permutation_t_value<-numeric(500000)
  i <-1
  repeat{
    thesis.df$Group<-sample(thesis.df$Group, 535, replace = F)
    permutation_result<-t.test(x[thesis.df$Group == "Metformin"], x[thesis.df$Group == "Placebo"])
    permutation_t_value[i]<-permutation_result$statistic
    i<- i +1
    if(i > 500000) break}
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

## The squared method ##

squared_composite<-((continuous_permutation_function(thesis.df$binary_insulin))^2) +
  ((continuous_permutation_function(thesis.df$gw32))^2) +
  ((continuous_permutation_function(thesis.df$gw38))^2)

## Non-squared method ##
non_squared_composite<-continuous_permutation_function(thesis.df$binary_insulin) +
  continuous_permutation_function(thesis.df$gw32) +
  continuous_permutation_function(thesis.df$gw38)

##Permuted Squared Test Statistic Graph##

percentile_95<-quantile(squared_composite, 0.95)

squared_composite.df<-data.frame(squared_composite)
ggplot(squared_composite.df, aes(x= squared_composite))+
  geom_histogram(bins = 200, fill = "darkslategray", color = "darkslategray")+
  scale_x_continuous(limits = c(0, 20)) +
  labs(x = "Squared Composite", y = "Frequency", title = "Squared Composite Test Statistic") +
  geom_vline(xintercept = percentile_95, color = "red", linetype = "dashed", size = 0.75)+
  annotate("text", x = percentile_95, y = 10000, label = "95th Percentile", color = "red", angle = 90, vjust = -0.5)+
  theme_minimal()






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


#######################PPPPPPPPPPPPPPPPPPPPOOOOOOOOOOOOOOOOOOOOOOOOWWWWWWWWWWWWWWWWWWEEEEEEEEEEEEEEEEERRRRRRRRRRRRR###################

##Power Code for Individual variables##
power_function <- function(x) {
  power_result <- numeric(100000)  
  i <- 1
  repeat {
    placebo_group <- thesis.df[thesis.df$Group == "Placebo", ]
    clean_placebo_group<-as.vector(na.omit(placebo_group[[x]]))
    sampled_placebo <- sample(clean_placebo_group, 15, replace = T)
    
    metformin_group <- thesis.df[thesis.df$Group == "Metformin", ]
    clean_metformin_group<-as.vector(na.omit(metformin_group[[x]]))
    sampled_metformin <- sample(clean_metformin_group, 15, replace = T)
    
    t_test_result<- t.test(sampled_metformin, sampled_placebo)
    power_result[i] <- t_test_result$statistic
    i<- i +1
    if(i > 100000) break}
  return(sum(power_result<(-qnorm(0.975)))/100000)
}
power_function("gw32")
power_function("gw38")
power_function("binary_insulin")

##Power Code for Squared Composite##
power_function <- function(x, y, z) {
  power_result <- numeric(1000)  
  i <- 1
  repeat {
    placebo_group <- tmpDat[tmpDat$Group == "Placebo", ]
    clean_placebo_group1<-as.vector(na.omit(placebo_group[[x]]))
    clean_placebo_group2<-as.vector(na.omit(placebo_group[[y]]))
    clean_placebo_group3<-as.vector(na.omit(placebo_group[[z]]))
    sampled_placebo1 <- sample(clean_placebo_group1, 15, replace = T)
    sampled_placebo2 <- sample(clean_placebo_group2, 15, replace = T)
    sampled_placebo3 <- sample(clean_placebo_group3, 15, replace = T)
    
    metformin_group <- tmpDat[tmpDat$Group == "Metformin", ]
    clean_metformin_group1<-as.vector(na.omit(metformin_group[[x]]))
    clean_metformin_group2<-as.vector(na.omit(metformin_group[[y]]))
    clean_metformin_group3<-as.vector(na.omit(metformin_group[[z]]))
    
    sampled_metformin1 <- sample(clean_metformin_group1, 15, replace = T)
    sampled_metformin2 <- sample(clean_metformin_group2, 15, replace = T)
    sampled_metformin3 <- sample(clean_metformin_group3, 15, replace = T)
    
    t_test_result1<- t.test(sampled_metformin1, sampled_placebo1)
    t_test_result2<- t.test(sampled_metformin2, sampled_placebo2)
    t_test_result3<- t.test(sampled_metformin3, sampled_placebo3)
    
    
    power_result[i] <- (((t_test_result1$statistic)^2)+((t_test_result2$statistic)^2)+((t_test_result3$statistic)^2))
    i<- i +1
    if(i > 1000) break}
  return(sum((power_result>quantile(squared_composite, 0.95))/ 1000))
}

power_function("gw32", "gw38", "binary_insulin")


## Not very tidy, is this an issue? Also, is there a problem with how I used the squared composite when getting the proportion? Should it be a variable of the function?

## Power Chart, generated with 500,000 test statistic permutations, starting at a sample size of 30, with increments of 10
## and ending at 80, with 500,000 resamples ##
##Composite test statistic used was 95th quantile which is stored as APPLE (7.881899)##
APPLE
power_n_30<-0.219598
power_n_40<-0.264418
power_n_50<-0.314862
power_n_60<-0.367066
power_n_70<-0.418002
power_n_80<-0.468156
power_results<-c(power_n_30, power_n_40, power_n_50, power_n_60, power_n_70, power_n_80)
sample_size<-c(30, 40, 50, 60, 70, 80)

power_graph.df<-data.frame(sample_size, power_results)
ggplot(power_graph.df, aes(x=sample_size, y=power_results)) +
  geom_line( color="mediumseagreen", size=1.5, alpha=0.9, linetype=1) +
  theme_classic() + labs(title = "Power of Permuted Test Statistics Method", x = "Sample Size", y= "Power")+
  scale_x_continuous(limits = c(30, NA), expand = c(0, 0))  +
  ylim(0, NA) 


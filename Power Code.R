View(tmpDat)
thesis.df<-tmpDat
View(thesis.df)

gw32_t_value<-t.test(tmpDat$gw32[tmpDat$Group == "Metformin"], tmpDat$gw32[tmpDat$Group == "Placebo"])$statistic



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





str(thesis.df$gw32)
metformin_group <- thesis.df[thesis.df$Group == "Metformin", ]
clean_metformin_group<-na.omit(metformin_group$gw32)
sum(is.na(thesis.df$gw32[thesis.df$Group == "Metformin"]))




metformin_group <- thesis.df[thesis.df$Group == "Metformin", ]
clean_metformin_group<-as.vector(na.omit(metformin_group$gw32))
sampled_metformin <- sample(clean_metformin_group, 15, replace = T)
print(sampled_metformin)

placebo_group <- thesis.df[thesis.df$Group == "Placebo", ]
clean_placebo_group<-as.vector(na.omit(placebo_group$gw32))
sampled_placebo <- sample(clean_placebo_group, 15, replace = T)
print(sampled_placebo)

t_test_result<- t.test(sampled_metformin, sampled_placebo)
power_result <- t_test_result$statistic
print(power_result)



placebo_group <- thesis.df[thesis.df$Group == "Placebo", ]
clean_placebo_group<-as.vector(na.omit(placebo_group[["gw32"]]))
sampled_placebo <- sample(clean_placebo_group, 15, replace = T)

metformin_group <- thesis.df[thesis.df$Group == "Metformin", ]
clean_metformin_group<-as.vector(na.omit(metformin_group[["gw32"]]))
sampled_metformin <- sample(clean_metformin_group, 15, replace = T)

t_test_result<- t.test(sampled_metformin, sampled_placebo)
print(t_test_result$statistic)
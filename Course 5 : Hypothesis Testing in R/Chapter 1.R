#library ----
library(dplyr)
library(fst)
library(ggplot2)

#read files ----
late_shipments = read_fst("late_shipments.fst")
glimpse(late_shipments)
late_shipments_boot_distn = read_fst("late_ship_boot_distn.fst")
glimpse(late_shipments_boot_distn)

#Calculating the sample mean ---- 

# View the late_shipments dataset
View(late_shipments)

# Calculate the proportion of late shipments(point estimate)
late_prop_samp <- late_shipments %>%
  summarize(point_estimate = mean(late == "Yes"))%>%
  pull(point_estimate)
#or 
#late_prop_samp = mean(late_shipments$late=="Yes")
# See the results
late_prop_samp

#Calculating the z-score  ----
  #Hypothesize that the proportion is 6%
  late_prop_hyp <- 0.06

  #Calculate the standard error
  std_error <- sd(late_shipments_boot_distn$late_rate)
  #Find z-score of late_prop_samp
  z_score <- ( late_prop_samp - late_prop_hyp)/std_error

  #See the results
  z_score


#or try to calculate & visualize the bootstrap distribution yourself ---- 
boot_dist = replicate(n=5000,
    exp = {
    #resample 
    late_shipments %>%
    slice_sample(prop = 1, replace = TRUE) %>%
    #calculate point estimate 
     summarize(late_mean= mean(late == "Yes")) %>% 
     pull(late_mean)
      })
  boot_dist
# visualize the bootstrap distribution 

tibble(boot = boot_dist)%>% 
  ggplot(aes(x = boot)) + 
  geom_histogram(bins = 15)
#calculate the standard error 
std_error2 <- sd(boot_dist)
# Find z-score of late_prop_samp
z_score2 <- ( late_prop_samp - late_prop_hyp)/ std_error2
# See the results
z_score2
#Calculating p-values ---- 
#Calculating p-value assuming a right tail test 
p_value = pnorm(z_score, lower.tail = FALSE)
#see the results 
p_value
#compare the p_value to the significance level
p_value <= alpha
#Calculating confidence intervals ----
#Calculate 95% confidence interval using quantile method
#the confidence interval for a significance level of 0.05  
confidence_interval_of_0.05 = 1 - 0.05
alpha <- 0.05 
hypo_late_prop <- 0.06 

conf_int_quantile <- late_shipments_boot_distn %>%
  summarize(lower = quantile(late_rate,0.025),
            upper = quantile(late_rate,0.975))
  
# See the result
conf_int_quantile


 

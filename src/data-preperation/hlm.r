###---HLM ESTIMATION---###
#Load packages
install.packages("lme4")
install.packages("sjstats")
library(lme4)
library(sjstats)
library(stargazer)
library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(lmerTest)
###----
# load the data
covid_stringency<-fread("../../gen/temp/covid_stringency_prepared.csv") #save the data in a dataframe
user_info<- fread("../../gen/temp/users_cleaned.csv")
all_books <-fread("../../gen/temp/books_cleaned.csv") 

all_books<- all_books %>% select (-V1) #remove variable we won't be using

# For testing purpose: take a random sample of all_books data
set.seed(123)
all_books<- all_books %>% slice_sample(n = 10000)
################################################################################
# create a variable that represents the first day of the week  in which the book was added, such that we can later aggregate on a weekly level:
all_books$first_day_of_week_added<- floor_date(as.Date(all_books$date_added, "%Y-%m/-%d"), unit="week", week_start = 1)
all_books$first_day_of_week_added <- as.Date(all_books$first_day_of_week_added)


################################################################################
################################################################################
################################################################################
# start with examining the number of books added per user per week
weekly_per_user<- all_books %>% group_by(`reader id`, first_day_of_week_added) %>% summarise(total=sum(dummy))

# create a list of all weeks between first and last time something was added
first_day_per_user<-weekly_per_user %>% group_by(`reader id`) %>% summarise(first=min(first_day_of_week_added))
last_day_per_user<-weekly_per_user %>% group_by(`reader id`) %>% summarise(last=max(first_day_of_week_added))

#then, create an empty dataframe in which we will later store all data
df_col_names<-c("date", "user")
weekly_per_user_complete<- data.frame(as.Date("2022-01-01"),NA)
names(weekly_per_user_complete)<-df_col_names

# make the loop below more efficient i.e. less time consuming?
#loop from the first till the last day:
#count=1

#for (user in first_day_per_user$`reader id`){
 #   count<-count+1
  #  first_day<-first_day_per_user[which(first_day_per_user$`reader id`==user), 2]
   # last_day<-last_day_per_user[which(last_day_per_user$`reader id`==user),2]
    #days_of_interest<-as.data.frame(seq(as.Date(pull(first_day[1,1])), as.Date(pull(last_day[1,1])), by="weeks"))
    #days_of_interest$user<-user
    #names(days_of_interest)<-df_col_names
    #weekly_per_user_complete<-rbind(weekly_per_user_complete,days_of_interest)
    
#}


# Improved code snippet 


(days_per_user<-weekly_per_user %>% group_by(`reader id`) %>% summarise(first=min(first_day_of_week_added),
                                                                        last=max(first_day_of_week_added)))

df = data.table(date=seq(from=min(days_per_user$first), to=max(days_per_user$last),by='weeks'))
df[, v1:=1] # what does this do?


df_users = data.table(user = unique(days_per_user$`reader id`))[, v1:=1]

weekly_per_user_complete=merge(df_users, df, by=c('v1'), allow.cartesian=T)

dt_days_per_user = data.table(days_per_user)
setkey(dt_days_per_user, `reader id`)
setkey(weekly_per_user_complete, user)
weekly_per_user_complete[dt_days_per_user, ':=' (first=i.first, last=i.last)]

weekly_per_user_complete <- weekly_per_user_complete[date>=first & date <= last]

gc()


#add the number of books read each week
weekly_per_user_complete<-weekly_per_user_complete%>% left_join(weekly_per_user, by=c("user"="reader id", "date"="first_day_of_week_added"))

#fill the NAs with zeros:
weekly_per_user_complete<- weekly_per_user_complete %>% replace(is.na(.), 0)


# add the country of origin to the data
data_to_add<-user_info%>% select(user_id,Country)
weekly_per_user_complete<-weekly_per_user_complete%>% left_join(data_to_add, by=c("user"="user_id"))


# next, we add the weekly stringency per country to the data.
covid_stingency_long<-pivot_longer(covid_stringency, 3:34)


# add the covid stringencies:
covid_stingency_long$first_day_of_week<-as.Date(covid_stingency_long$first_day_of_week)
covid_stingency_long<-covid_stingency_long%>% select(-V1) #remove what we will not use
weekly_per_user_complete<- weekly_per_user_complete %>% select(,-c(Country.y)) %>% rename(Country = Country.x) # temp code to remove country column duplicate
weekly_per_user_complete <- weekly_per_user_complete %>% left_join(covid_stingency_long, by=c("date"="first_day_of_week", "Country"="name"))
weekly_per_user_complete<- weekly_per_user_complete %>% select(,-c(total.y)) %>% rename(total = total.x) # temp code to remove country column duplicate

# fill the NA's:
weekly_per_user_complete<- weekly_per_user_complete %>% replace(is.na(.), 0)


# filter for the time period of interest
all_books14<- all_books %>% filter(first_day_of_week_added > "2013-12-24" & first_day_of_week_added < "2022-01-07")
weekly_per_user_complete<- weekly_per_user_complete %>% filter(date > "2013-12-24" & date < "2022-01-07")
weekly_per_user_complete<-weekly_per_user_complete %>% filter(!(Country == "0")) #remove the dummy country

################################################################################

# Estimations
# ------------DV1: Number of Books--------------#
# Model 1a: Null model (Intercept only)
# Note: Here we include 2 random effects - one is 'country' and the other is 'user' level
model_1a<- lmer(total ~ 1+(1|Country) + (1|user), data = weekly_per_user_complete, REML=FALSE)
summary(model_1a)
# ranef() can be used to obtain the individual random effects ( level 2 residuals of the intercept)
ranef(model_1a)
# Interpretation: Here we find that the variance is higher in the user case so the user-to-user variability has the greatest contribution, than the country variation. Finally, the residual variance is the variability that cannot be attributed to either.

# Intraclass correlation(icc): quantifies the extent of "non-independence" in the dependent variable due to systematic level 2 differences in the measured characteristic. The greater the proportion of the level 2 variance(variance of the group means) relative to the total variance (sum of the level 2 variance and the level 1 or residual variance), the greater the similarities WITHIN the level 2 units compared to BETWEEN the level 2 units
performance::icc(model_1a)

# Signaficance test for the intercept variance
ranova(model_1a)

# Model 1b: Random intercept model 
model_1b<- lmer(total ~ 1+value+(1|Country) + (1|user), data = weekly_per_user_complete, REML=FALSE)
summary(model_1b)

performance::icc(model_1b)

# Model 1c: Random coefficients model ( this model contains BOTH a random intercept AND a random slope)
#Note: I'm a bit suss about this one... 
model_1c<- lmer(total ~ value+(value|Country) + (value|user), data = weekly_per_user_complete, REML=FALSE)
summary(model_1c)

performance::icc(model_1c)

# compare models with AIC
anova(model_1a,model_1b,model_1c)

################################################################################
#################### DV2: Days per book ############################################
################################################################################
# days per book
all_books_read_time <- all_books14 %>% filter(!is.na(read_time_days))
days_per_user<- all_books_read_time %>%group_by(`reader id`,first_day_of_week_added) %>% summarise(days=mean(read_time_days))

#add the user info

# add the country of origin to the data
data_to_add<-user_info%>% select(user_id,Country)
days_per_user<-days_per_user%>% left_join(data_to_add, by=c("reader id"="user_id"))


# next, we add the weekly stringency per country to the data.
covid_stingency_long<-pivot_longer(covid_stringency, 3:34)


# add the covid stringencies:
covid_stingency_long$first_day_of_week<-as.Date(covid_stingency_long$first_day_of_week)
covid_stingency_long<-covid_stingency_long%>% select(-V1) #remove what we will not use
days_per_user <- days_per_user %>% left_join(covid_stingency_long, by=c("first_day_of_week_added"="first_day_of_week", "Country"="name"))

# fill the NA's:
days_per_user<- days_per_user %>% replace(is.na(.), 0)


#add the week number
days_per_user$week<-as.character(week(days_per_user$first_day_of_week_added))
# Estimations
# ------------DV2: Number of days per book (Speed) --------------#
# Model 2a: Null model (Intercept only)
# Note: Here we include 2 random effects - one is 'country' and the other is 'user' level
model_2a<- lmer(days ~ 1+(1|Country)+(1|`reader id`), data = days_per_user, REML=FALSE)

summary(model_2a)
# ranef() can be used to obtain the individual random effects ( level 2 residuals of the intercept)
ranef(model_2a)
# Interpretation: Here we find that the variance is higher in the user case so the user-to-user variability has the greatest contribution, than the country variation. Finally, the residual variance is the variability that cannot be attributed to either.

# Intraclass correlation(icc): quantifies the extent of "non-independence" in the dependent variable due to systematic level 2 differences in the measured characteristic. The greater the proportion of the level 2 variance(variance of the group means) relative to the total variance (sum of the level 2 variance and the level 1 or residual variance), the greater the similarities WITHIN the level 2 units compared to BETWEEN the level 2 units
performance::icc(model_2a)

# Signaficance test for the intercept variance
ranova(model_2a)

# Model 2b: Random intercept model 
model_2b<- lmer(days ~ 1+value+(1|Country) + (1|`reader id`), data = days_per_user, REML=FALSE)

summary(model_2b)
ranova(model_2b)

performance::icc(model_2b)

# ERROR!! Model 1c: Random coefficients model ( this model contains BOTH a random intercept AND a random slope)
#Note: unable to add 2 random effects.. model becomes unidentifiable..
model_2c<- lmer(days ~ value+(value|Country) + (value|`reader id`), data = days_per_user, REML=FALSE)
summary(model_2c)

performance::icc(model_2c)

# compare models with AIC
anova(model_2a,model_2b,model_2c)

################################################################################    


# !!ERROR!! Adding a random slope term (singularity issue with this model: https://stats.stackexchange.com/questions/378939/dealing-with-singular-fit-in-mixed-models)
model_2c<- lmer(days ~ value+(value|Country), data = days_per_user, REML=FALSE)
summary(model_2c)

performance::icc(model_2c)

# Source of issue of singularity: When you obtain a singular fit, this is often indicating that the model 
                                #is overfitted – that is, the random effects structure is too complex to be 
                                #supported by the data, which naturally leads to the advice to remove the 
                                #most complex part of the random effects structure (usually random slopes). 
                                #The benefit of this approach is that it leads to a more parsimonious model that is not over-fitted. 

# compare models with AIC
anova(model_2a,model_2b)

# DV3: Rating 

# DV4: Number of pages 

# DV5: % Nostalgic 

# DV6: % Recent published






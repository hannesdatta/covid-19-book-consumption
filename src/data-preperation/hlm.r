###---HLM ESTIMATION---###
#Load packages
install.packages("lme4")
install.packages("sjstats")
library(lme4)
library(sjstats)

# Estimations
regression_nr_books <- lm((total)~ value+week+date+Country, weekly_per_user_complete)
summary(regression_nr_books)
write.csv2(as.data.frame(summary(regression_nr_books)$coefficients), file = "../../gen/output/model1_qty.csv", fileEncoding = "UTF-8")

# Null model (Intercept only)
model_1<- lmer(total ~ 1+(1|Country), data = weekly_per_user_complete, REML=FALSE)
summary(model_1)

performance::icc(model_1)

# Adding other fixed effects
model_2<- lmer(total ~ 1+value+(1|Country), data = weekly_per_user_complete, REML=FALSE)
summary(model_2)

performance::icc(model_2)

# Adding a random slope term 
model_3<- lmer(total ~ value+(value|Country), data = weekly_per_user_complete, REML=FALSE)
summary(model_3)

performance::icc(model_3)

# compare models with AIC
anova(model_1,model_2,model_3)



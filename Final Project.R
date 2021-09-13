library(tidyverse)

#import data and review

df <- read.delim("~/UNCG/IAF 602/bodyfat.txt", header = T)

str(df)
summary(df)

#Add identifier
df <- tibble::rowid_to_column(df, "ID")

#Split Data into Test and Train Datasets

library(rsample)
set.seed(645)

df_split <- initial_split(df, prop = 0.7)

df_train <- training(df_split)
df_test <- testing(df_split)

#preliminary scatterplots of values vs dependent variable

pairs(Pct.BF ~ Neck + Chest + Hip + Thigh + Knee + Ankle + Bicep + Forearm + Wrist, upper.panel = panel.smooth, data = df_train)

library(ggplot2)

plot1 <- ggplot(df_train, aes(x = Neck, y = Pct.BF)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE, color = "red")

plot2 <- ggplot(df_train, aes(x = Chest, y = Pct.BF)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE, color = "red")

plot3 <- ggplot(df_train, aes(x = Hip, y = Pct.BF)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE, color = "red")

plot4 <- ggplot(df_train, aes(x = Thigh, y = Pct.BF)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE, color = "red")

plot5 <- ggplot(df_train, aes(x = Knee, y = Pct.BF)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE, color = "red")

plot6 <- ggplot(df_train, aes(x = Ankle, y = Pct.BF)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE, color = "red")

plot7 <- ggplot(df_train, aes(x = Bicep, y = Pct.BF)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE, color = "red")

plot8 <- ggplot(df_train, aes(x = Forearm, y = Pct.BF)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE, color = "red")

plot9 <- ggplot(df_train, aes(x = Wrist, y = Pct.BF)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE, color = "red")

#fit it onto one plot matrix

library(ggpubr)

ggarrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, ncol=3, nrow=3)

#fit preliminary model

lmbody <- lm(Pct.BF ~ Neck + Chest + Hip + Thigh + Knee + Ankle + Bicep + Forearm + Wrist, data = df_train)

#Residuals vs Fitted
plot(lmbody,1, main = "Residuals vs Fitted")

#Potentially Influential Observations at 79 and 206

#Q-Q Plot
plot(lmbody,2, main = "Q-Q plot of residuals")

#Potentially Influential Observations at observations 79 and 84

#Case influence statistics

lm1hat = hatvalues(lmbody) # hatvalues
id.lm1hat = which(lm1hat > 2*length(lmbody$coefficients)/length(df_train$Pct.BF))
lm1hat[id.lm1hat]

plot(df_train$ID,lm1hat, main = "Leverage Values")
abline(h=2*length(lmbody$coefficients)/length(df_train$Pct.BF))

#Potentially Influential Observations at observations 31, 36, 40, 52, 84, 104, 157, 173, and 204

library(MASS)

lm1stud.in = stdres(lmbody) # internally studentized
id.lm1stud1 = which(abs(lm1stud.in)>2)
lm1stud.in[id.lm1stud1] 

plot(df_train$ID,lm1stud.in, main= "Internally Studentized Residuals")
abline(h=c(-2,2))

#Potentially Influential Observations as observations 9, 79, 84, 170, 205, and 206

lm1stud.ex = studres(lmbody) # externally studentized residuals
id.lm1stud2 = which(abs(lm1stud.ex)>2)
lm1stud.ex[id.lm1stud2]

plot(df_train$ID,lm1stud.ex, main = "Externally Studentized Residuals") 
abline(h=c(-2,2)) 

#Potentially Influential Observations 9, 79, 84, 170, 205, 206, and 233

lm1cook = cooks.distance(lmbody) # Cook's distances

plot(df_train$ID,lm1cook)

plot(lmbody, which=4)

#Potentially Influential Observations at number 31, 84, and 173

#Let's look at these observations: 9, 31, 36, 40, 52, 79, 84, 104, 157, 170, 173, 204, 205, 206, and 233  

subset(df_train, ID == 9 | ID == 31 |  ID == 36 |  ID == 40 |  ID == 52 |  ID == 79 |  ID ==84 |  ID == 104 |  ID ==157
                       |  ID == 170 |  ID == 173 |  ID == 204 |  ID == 205 |  ID == 206 | ID == 233)

View(df)
#Observation #9 seems odd - 4.1% Pct.BF is exceptionally low. Observations with similar Pct.BF are all much lower weight and smaller measures
#Not sure why this observation wasn't seen; however, in reviewing the data, observation 180 has a 0% body fat which is not possible
#Observation #31 is problematic Ankle measurement of 33.9 is exceptionally large, especially given the low weight
#Observation 36 is exceptionally high for Pct.BF. Other observations with large Pct.BF have larger weight and waist
#Observation 40 has large measurements across the board, but doesn't seem too out of place.
#Observation 52 has a small hip and thigh but a large bicep and forearm. Pct.BF given weight seems accurate. Subject should probably stop skipping leg day.
#Observation 79 has a very large Pct.BF given a relatively low weight and average height.
#Observation 84 has a very large ankle measure especially given the average measures for everything else
#Observation 104 has a peculiar neck measurement
#Observation 157 has an exceptionally large ankle measure
#Observation 170 has an exceptionally low Pct.BF and that seems unlikely
#Observation 173 has a low Pct.BF given the high measurements elsewhere
#Observation 204 has a low Pct. BF given the high weight, neck, chest, and abdomen measures
#Observation 205 has a high Pct.BF given the low weight
#Observation 206 has a low weight given the high Pct.BF and high measures elsewhere
#Observation 233 has a low weight given the Pct.BF


#According to the internet, the following formula is a good estimate of percent body fat based on BMI

quickBF <- function(Weight, Height, Age){
  Height2 = Height * Height
  BMI = Weight / Height2 * 703
  BFP = 1.2 * BMI + 0.23 * Age - 16.2
  return(BFP)
}

observations <- c(9,31,36,40,52,79,84,104,157,170,173,204,205,206,233)

for (val in observations){
  print(c(val, df_train$Pct.BF[which(df_train$ID == val)], quickBF(df_train$Weight[which(df_train$ID == val)], df_train$Height[which(df_train$ID == val)], df_train$Age[which(df_train$ID == val)])))
}

#Observations 79 seems within reason, but chest and abdomen measures are abnormal
#Observation 157 seems within reason, but ankle measure is exceptionally large
#Observation 233 seems within reason.

#Remove outliers

df_train2 <- subset(df_train, ID != 9 & ID != 31 &  ID != 36 &  ID != 40 &  ID != 52 &  ID != 79 &  ID !=84 &  ID != 104 &  ID !=157
                    &  ID != 170 &  ID != 173 &  ID != 204 &  ID != 205 &  ID != 206 & ID != 233)

lmbody2 <- lm(Pct.BF ~ Neck + Chest + Hip + Thigh + Knee + Ankle + Bicep + Forearm + Wrist, data = df_train2)

summary(lmbody)
summary(lmbody2)

#Coefficients and p-values change without outliers. Removing them. Needs noted in report

#Plot without outliers

plot1 <- ggplot(df_train2, aes(x = Neck, y = Pct.BF)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE, color = "red")

plot2 <- ggplot(df_train2, aes(x = Chest, y = Pct.BF)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE, color = "red")

plot3 <- ggplot(df_train2, aes(x = Hip, y = Pct.BF)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE, color = "red")

plot4 <- ggplot(df_train2, aes(x = Thigh, y = Pct.BF)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE, color = "red")

plot5 <- ggplot(df_train2, aes(x = Knee, y = Pct.BF)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE, color = "red")

plot6 <- ggplot(df_train2, aes(x = Ankle, y = Pct.BF)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE, color = "red")

plot7 <- ggplot(df_train2, aes(x = Bicep, y = Pct.BF)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE, color = "red")

plot8 <- ggplot(df_train2, aes(x = Forearm, y = Pct.BF)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE, color = "red")

plot9 <- ggplot(df_train2, aes(x = Wrist, y = Pct.BF)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE, color = "red")

#fit it onto one plot matrix

ggarrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, ncol=3, nrow=3)

#check for multi-collinearity
library(car)
vif(lmbody2)


#Fit all 1 predictor models
lmneck <- lm(Pct.BF ~ Neck, data = df_train2)
lmchest <- lm(Pct.BF ~ Chest, data = df_train2)
lmhip <- lm(Pct.BF ~ Hip, data = df_train2)
lmthigh <- lm(Pct.BF ~ Thigh, data = df_train2)
lmknee <- lm(Pct.BF ~ Knee, data = df_train2)
lmankle <- lm(Pct.BF ~ Ankle, data = df_train2)
lmbicep <- lm(Pct.BF ~ Bicep, data = df_train2)
lmforearm <- lm(Pct.BF ~ Forearm, data = df_train2)
lmwrist <- lm(Pct.BF ~ Wrist, data = df_train2)

tidy(lmneck)
tidy(lmchest)
tidy(lmhip)
tidy(lmthigh)
tidy(lmknee)
tidy(lmankle)
tidy(lmbicep)
tidy(lmforearm)
tidy(lmwrist)

#All models have p-values less than 0.20, do careful selection

tidy(lmbody2)

#Remove largest p-value, one at a time, keeping anything p<0.10

#Start with knee
lmbody3 <- lm(Pct.BF ~ Neck + Chest + Hip + Thigh + Ankle + Bicep + Forearm + Wrist, data = df_train2)
tidy(lmbody3)
#Remove Wrist
lmbody4 <- lm(Pct.BF ~ Neck + Chest + Hip + Thigh + Ankle + Bicep + Forearm, data = df_train2)
tidy(lmbody4)
#Remove Neck
lmbody5 <- lm(Pct.BF ~ Chest + Hip + Thigh + Ankle + Bicep + Forearm, data = df_train2)
tidy(lmbody5)
#Remove Hip
lmbody6 <- lm(Pct.BF ~ Chest + Thigh + Ankle + Bicep + Forearm, data = df_train2)
tidy(lmbody6)
#Remove Bicep
lmbody7 <- lm(Pct.BF ~ Chest + Thigh + Ankle + Forearm, data = df_train2)
tidy(lmbody7)
#Remove Ankle
lmbody8 <- lm(Pct.BF ~ Chest + Thigh + Forearm, data = df_train2)
tidy(lmbody8)

#Keep Chest, Thigh, Forearm, add dropped variables back, keep any with p < 0.10

#Knee
lmbody9 <- lm(Pct.BF ~ Chest + Thigh + Forearm + Knee, data = df_train2)
tidy(lmbody9)
#Knee is still statistically insignificant. Move forward without

#Wrist
lmbody10 <- lm(Pct.BF ~ Chest + Thigh + Forearm + Wrist, data = df_train2)
tidy(lmbody10)
#Wrist is still statistically insignificant. Move forward without

#Neck, hip, bicep, ankle
lmbody11 <- lm(Pct.BF ~ Chest + Thigh + Forearm + Neck, data = df_train2)
tidy(lmbody11)
#Neck is still statistically insignificant. Move forward without

#Hip, bicep, ankle
lmbody12 <- lm(Pct.BF ~ Chest + Thigh + Forearm + Hip, data = df_train2)
tidy(lmbody12)
#Hip is still statistically insignificant. Move forward without

#Bicep, ankle
lmbody13 <- lm(Pct.BF ~ Chest + Thigh + Forearm + Bicep, data = df_train2)
tidy(lmbody13)
#Bicep is still statistically insignificant. Move forward without

#Ankle
lmbody14 <- lm(Pct.BF ~ Chest + Thigh + Forearm + Ankle, data = df_train2)
tidy(lmbody14)
#Ankle is still statistically insignificant. Move forward without

lm_intentional <- lm(Pct.BF ~ Chest + Thigh + Forearm, data = df_train2)
summary(lm_intentional)

#Try Stepwise Selection

step.model <- stepAIC(lmbody2, direction = "both", 
                      trace = FALSE)
summary(step.model)

#Stepwise selects Chest, Thigh, Ankle, and Forearm.

#Try Best Subsets Selection

library(leaps)

fit.best=regsubsets(Pct.BF ~ Neck + Chest + Hip + Thigh + Knee + Ankle + Bicep + Forearm + Wrist, data=df_train2)
sum.fit.best <- summary(fit.best)
sum.fit.best

sum.fit.best$cp
#Smallest Cp is model 4, which use variables Chest, Thigh, Ankle, and Forearm
sum.fit.best$bic
#Smallest BIC is model 3, which uses variables Chest, Thigh, and Forearm

lm_stepwise_lowCp <- lm(Pct.BF ~ Chest + Thigh + Ankle + Forearm, data = df_train2)

#Test results

predicted_intentional <- lm_intentional %>% 
  predict(newdata = df_test)
  
predicted_stepwise_lowCp <- lm_stepwise_lowCp %>% 
  predict(newdata = df_test)

predicted_full <- lmbody2 %>%
  predict(newdata= df_test)

df_test <- df_test %>% 
  mutate(.fitted_intentional = predicted_intentional) %>% 
  mutate(.fitted_stepwise_lowCp = predicted_stepwise_lowCp) %>%
  mutate(.fitted_full = predicted_full)

#Plot predicted v. actual

par(mfrow=c(2,2))
plot(df_test$.fitted_full,df_test$Pct.BF,
     xlab="predicted",ylab="actual", main = "Full Model")
abline(a=0,b=1)
plot(df_test$.fitted_stepwise_lowCp,df_test$Pct.BF,
     xlab="predicted",ylab="actual", main = "Stepwise Model")
abline(a=0,b=1)
plot(df_test$.fitted_intentional,df_test$Pct.BF,
     xlab="predicted",ylab="actual", main = "Intentional")
abline(a=0,b=1)

library(yardstick)

#RMSE evaluations

df_test %>% 
  rmse(.fitted_intentional, Pct.BF)

df_test %>% 
  rmse(.fitted_stepwise_lowCp, Pct.BF)

df_test %>% 
  rmse(.fitted_full, Pct.BF)
View(df_test)

#RMSE favors the stepwise_lowCp model that uses Chest, Thigh, Ankle, and Forearm

par(mfrow=c(1,2))
plot(df_test$.fitted_full,df_test$Pct.BF,
     xlab="predicted",ylab="actual", main = "Full Model")
abline(a=0,b=1)
plot(df_test$.fitted_stepwise_lowCp,df_test$Pct.BF,
     xlab="predicted",ylab="actual", main = "Stepwise Model")
abline(a=0,b=1)

summary(lmbody2)
summary(lm_stepwise_lowCp)
#Final model, stepwise_lowCp can account for ~54% of the variation in Pct.BF

vif(lm_stepwise_lowCp)

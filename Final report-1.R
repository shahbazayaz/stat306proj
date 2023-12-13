install.packages("ggplot2")
install.packages("corrplot")
install.packages("dplyr")
install.packages("leaps")
install.packages("GGally")
library(leaps)
library(corrplot)
library(dplyr)
library(ggplot2)
library(GGally)

insurance <- read.csv("insurance.csv")
head(insurance)

insurance$sex <- factor(insurance$sex, levels = c("male", "female"))
insurance$smoker <- factor(insurance$smoker, levels = c("yes", "no"))
insurance$region <- factor(insurance$region, levels = c("southwest", "southeast", "northwest", "northeast"))
insurance$children <- factor(insurance$children, levels = c("0", "1", "2", "3", "4", "5"))

pair_plot <- ggpairs(insurance, lower = list(continuous = wrap('points', alpha = 0.4)), diag = list(continuous = "barDiag"), upper = list(continuous = wrap("cor", size = 4)))
pair_plot

model1<- lm(charges ~ age + sex + bmi + children + smoker + region, data=insurance)
summary(model1)

plot(fitted(model1), resid(model1), xlab = "Fitted Values for Model 1", ylab = "Residuals", main = "Residual Plot for Model 1")
qqnorm(resid(model1))
qqline(resid(model1))


insurance$sex <- as.factor(insurance$sex)
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)
insurance$children <-as.factor(insurance$children)


result <- regsubsets(charges ~ age + sex + bmi + children + smoker + region, data=insurance)

summary_result<-summary(result)
summary_result
summary_result$rsq
summary_result$adjr2
summary_result$cp
summary_result$bic
summary_result$which

plot(summary_result$cp, xlab = "Number of Variables", ylab = "Cp",type="l",ylim=c(0, 100))
abline(0,1)

model2<-lm(charges ~ age + bmi + children + smoker + region, data=insurance)
summary(model2)
plot(fitted(model2), resid(model2), xlab = "Fitted Values for Model 2", ylab = "Residuals", main = "Residual Plot for Model 2")




model3_1<-lm( charges ~ age + bmi + children + region + smoker + 
     smoker * age + smoker * bmi + smoker * children + smoker * 
     region, data = insurance)
summary(model3_1)
model3_2<-lm( charges ~ age + bmi + children + region + smoker + 
     region* age + region* bmi + region* children + region * smoker
     , data = insurance)
summary(model3_2)
model3_3<-lm(charges ~ age + bmi + children + region + smoker + 
     children* age + children* bmi + children*smoker + children* 
     region, data = insurance)
summary(model3_3)
model3<-lm( charges ~ age + bmi + children + region + smoker + 
              smoker * bmi, data = insurance)
summary(model3)
plot(fitted(model3), resid(model3), xlab = "Fitted Values for Model 3", ylab = "Residuals", main = "Residual Plot for Model 3")

model4 <- lm(sqrt(charges) ~ smoker + smoker * bmi + bmi + age + 
               children + region, data = insurance)
summary(model4)
plot(fitted(model4), resid(model4), xlab = "Fitted Values for Model 4", ylab = "Residuals", main = "Residual Plot for Model 4")


model5 <- lm(log(charges) ~ smoker + smoker*bmi + bmi + age + children + region, data = insurance)
summary(model5)
plot(fitted(model5), resid(model5), xlab = "Fitted Values for Model 5", ylab = "Residuals", main = "Residual Plot for Model 5")

model6_1<-lm(formula = charges ~ age + I(age^2) + bmi  + children + 
                  region + smoker + smoker * bmi, data = insurance)
summary(model6_1)
model6_2<-lm(charges ~ age + I(age^2) + I(age^3) +  bmi  + children + 
               region + smoker + smoker * bmi, data = insurance)
summary(model6_2)

model6_3<-lm( charges ~ age + bmi+ I(bmi^2)  + children + 
                region + smoker + smoker * bmi,data = insurance)
summary(model6_3)

model6_4<-lm(charges ~ age + bmi+ I(bmi^2)  + I(bmi^3) + children + 
             region + smoker + smoker * bmi, data = insurance)
summary(model6_4)

model6 <- lm(lm(formula = charges ~ I(age^2) + I(bmi^2) + I(bmi^3) + children + 
                  region + smoker + smoker * bmi, data = insurance))
qqnorm(resid(model6))
qqline(resid(model6))
plot(fitted(model6), resid(model6), xlab = "Fitted Values for Model 6", ylab = "Residuals", main = "Residual Plot for Model 6")

ggplot(insurance, aes(x=fitted(model6), y = resid(model6), colour =region )) + geom_point() + xlab("Fitted values for Model 6") + ylab("Residuals")+ggtitle("Residual plot for model 6")
ggplot(insurance, aes(x=fitted(model6), y = resid(model6), colour = smoker)) + geom_point() + xlab("Fitted values for Model 6") + ylab("Residuals")
ggplot(insurance, aes(x=fitted(model6), y = resid(model6), colour = children)) + geom_point() + xlab("Fitted values for Model 6") + ylab("Residuals")
plot(cooks.distance(model6), pch=20, ylab="Cook's distance", main = "Cook's distance for Model 6")



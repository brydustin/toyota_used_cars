# Loaging the library
library(devtools)
install_github("vqv/ggbiplot")
library(purrr)
library(ggplot2)
library(tidyverse)
library(ggbiplot)
library(dplyr)
installed.packages("glmnet")

df <- read.csv("toyota.csv")

#We are converting the non-numerics to numerics using characteristic fuctions (indicator variables).
#When a vehicle satisfies the condition of the variable in question then the variable obtains a 
#value of 1 for this vehicle, otherwise it is 0. 
#This allows us to build linear models using variables that we could otherwise not pass to the functions in R 
#which we use to build said models.

#First we do this for the fuel type:
df$Is_Diesel <- ifelse(df$fuelType=="Diesel", 1, 0)
df$Is_Petrol <- ifelse(df$fuelType=="Petrol", 1, 0)
df$Is_Hybrid <- ifelse(df$fuelType=="Hybrid", 1, 0)
df$Is_OtherFuel <- ifelse(df$fuelType=="Other", 1, 0)

#Now we do it for the transmission type: 
df$Is_Manual    <- ifelse(df$transmission=="Manual", 1, 0)
df$Is_Automatic <- ifelse(df$transmission=="Automatic", 1, 0)
df$Is_SemiAuto  <- ifelse(df$transmission=="Semi-Auto", 1, 0)
df$Is_OtherTrans <- ifelse(df$transmission=="Other", 1, 0)

#Now we do it for the model of the vehicle:
df$Is_GT86 <- ifelse(df$model==" GT86", 1, 0) 
df$Is_Corolla <- ifelse(df$model==" Corolla", 1, 0)
df$Is_RAV4 <- ifelse(df$model==" RAV4", 1, 0)
df$Is_Yaris <- ifelse(df$model==" Yaris", 1, 0)
df$Is_Auris <- ifelse(df$model==" Auris", 1, 0)
df$Is_Aygo  <- ifelse(df$model==" Aygo", 1, 0)
df$Is_CHR  <- ifelse(df$model==" C-HR", 1, 0)
df$Is_Prius  <- ifelse(df$model==" Prius", 1, 0)
df$Is_Avensis  <- ifelse(df$model==" Avensis", 1, 0)
df$Is_Verso  <- ifelse(df$model==" Verso", 1, 0)
df$Is_Hilux  <- ifelse(df$model==" Hilux", 1, 0)
df$Is_Proace_V  <- ifelse(df$model==" PROACE VERSO", 1, 0)
df$Is_Land_Cru  <- ifelse(df$model==" Land Cruiser", 1, 0)
df$Is_Supra  <- ifelse(df$model==" Supra", 1, 0)
df$Is_Camry  <- ifelse(df$model==" Camry", 1, 0)
df$Is_VersoS  <- ifelse(df$model==" Verso-S", 1, 0)
df$Is_IQ  <- ifelse(df$model==" IQ", 1, 0)
df$Is_Urban_Cru  <- ifelse(df$model==" Urban Cruiser", 1, 0)








library(glmnet)
#head(mtcars)
# Getting the independent (input) variables for the (ridge) regression model. 
x_var <- data.matrix(df[, c("year", "mileage", "mpg", "engineSize", 
                            "Is_Diesel", "Is_Petrol","Is_Hybrid", "Is_OtherFuel",
                            "Is_Manual", "Is_Automatic", "Is_SemiAuto", "Is_OtherTrans",
                            "Is_GT86", "Is_Corolla", "Is_RAV4", "Is_Yaris", "Is_Auris", 
                            "Is_Aygo", "Is_CHR", "Is_Prius", "Is_Avensis", "Is_Verso", 
                            "Is_Hilux", "Is_Proace_V", "Is_Land_Cru", "Is_Supra", 
                            "Is_Camry", "Is_VersoS", "Is_IQ", "Is_Urban_Cru"
)])






# Getting the dependent (output) variable
y_var <- df[, "price"]

# Setting the range of lambda values (this factor helps our solutions to stay reasonable, it's very technical so don't worry about the interpretation)
#Notably we are testing a range of lambdas
lambda_seq <- 10^seq(2, -2, by = -.1)

# Using glmnet function to build the ridge regression in r
fit <- glmnet(x_var, y_var, alpha = 0, lambda  = lambda_seq)
# Checking the model
summary(fit)


# Using cross validation glmnet
ridge_cv <- cv.glmnet(x_var, y_var, alpha = 0, lambda = lambda_seq)
# Best lambda value
best_lambda <- ridge_cv$lambda.min
best_lambda

best_fit <- ridge_cv$glmnet.fit
head(best_fit)

# Rebuilding the model with optimal lambda value
best_ridge <- glmnet(x_var, y_var, alpha = 0, lambda = best_lambda)


#Now we are using the build model to estimate the costs, and later we will compare this to the actual costs.
df$Predicted_Price <- (coef(best_ridge)[1]             + 
                         (coef(best_ridge)[2])*(df$year) +
                         (coef(best_ridge)[3])*(df$mileage) +
                         (coef(best_ridge)[4])*(df$mpg) +
                         (coef(best_ridge)[5])*(df$engineSize) +
                         (coef(best_ridge)[6])*(df$Is_Diesel) +
                         (coef(best_ridge)[7])*(df$Is_Petrol) +
                         (coef(best_ridge)[8])*(df$Is_Hybrid) +
                         (coef(best_ridge)[9])*(df$Is_OtherFuel)  + 
                         (coef(best_ridge)[10])*(df$Is_Manual) + 
                         (coef(best_ridge)[11])*(df$Is_Automatic) +
                         (coef(best_ridge)[12])*(df$Is_SemiAuto) +
                         (coef(best_ridge)[13])*(df$Is_OtherTrans) +
                         (coef(best_ridge)[14])*(df$Is_GT86) )
(coef(best_ridge)[15])*(df$Is_Corolla) +
  (coef(best_ridge)[16])*(df$Is_RAV4) +
  (coef(best_ridge)[17])*(df$Is_Yaris) +
  (coef(best_ridge)[18])*(df$Is_Auris) +
  (coef(best_ridge)[19])*(df$Is_Aygo) +
  (coef(best_ridge)[20])*(df$Is_CHR) +
  (coef(best_ridge)[21])*(df$Is_Prius) +
  (coef(best_ridge)[22])*(df$Is_Avensis) +
  (coef(best_ridge)[23])*(df$Is_Verso) +
  (coef(best_ridge)[24])*(df$Is_Hilux) +
  (coef(best_ridge)[25])*(df$Is_Proace_V) +
  (coef(best_ridge)[26])*(df$Is_Land_Cru) +
  (coef(best_ridge)[27])*(df$Is_Supra) +
  (coef(best_ridge)[28])*(df$Is_Camry) +
  (coef(best_ridge)[29])*(df$Is_VersoS) +
  (coef(best_ridge)[30])*(df$Is_IQ) +
  (coef(best_ridge)[31])*(df$Is_Urban_Cru) 


df$Price_Error <- abs(df$Predicted_Price - df$price)




#Taken as a whole, the lower the MPG the higher the price of the vechicles.
ggplot(df, aes(mpg, price, color=model) ) +
  geom_point()+
  ggtitle("Price as a Function of Miles Per Gallon ")



#However, the plot below suggests there does not seem to be a correlation between MPG and Price when viewed within individual models.
#This suggests that the price is highly correlated to the model (since specific models have a given range where we expect their MPG to fall.). For those interested in a technical explanation, this is an instance of Simpson's paradox.
ggplot(df, aes(mpg, price) ) +
  geom_point()+
  facet_wrap(~ model)+
  ggtitle("Price as a Function of Miles Per Gallon")


#We expect there to be a high correlation between year and price across all models, and the graphs below indicate this.
ggplot(df, aes(year, price) ) +
  geom_point()+
  facet_wrap(~ model)+
  ggtitle("Price as a Function of Year")



#From the graphs above we can see that the Land Cruiser depreciates in value the quickest, followed by Hilux, RAV4, and Prius.

#Suprisingly there is little correlation between engine size an the price. 
#The example below focuses on the Auris, but this trend is quite typical across all models.
ggplot(filter(df, model == " Auris"), aes(engineSize, price) ) +
  geom_point()+
  ggtitle("Price as a Function of Engine Size for Auris ")




ggplot(df, aes(mileage, price) ) +
  geom_point()+
  facet_wrap(~ model)+
  ggtitle("Price as a Function of Mileage")


#From the graphs below we can once again see that the Land Cruiser depreciates in value the quickest, followed by Hilux, RAV4, and Prius.




ggplot(df, aes(fuelType, price) ) +
  geom_point()+
  facet_wrap(~ model)+
  ggtitle("Price as a Function of Fule Type")

#The results below indicate there is no relationship between Fule Type and Price.


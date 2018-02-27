#--------------
# Data Preparation
#--------------

#install.packages("MASS")
#install.packages("car")
#install.packages("dummies")

library(MASS) 
library(car)
library(reshape2)
library(tidyr)
library(dummies)
library(ggplot2)


# import that carprice dataset
carprice <- read.csv(file = file.choose(),stringsAsFactors = T)
#carprice <- read.csv(file = "CarPrice_Assignment.csv",stringsAsFactors = T)
#carprice_bkp <- carprice
str(carprice)
View(carprice)


# split the column into car Make and Car model
carprice <- separate(data = carprice,col = CarName,into = c("carmake","modelname"),sep = " ",extra = "merge")


# split the carName variable into model and make
#carprice <- data.frame(colsplit(string = carprice$CarName,pattern = " ",c("car_maker","car_model")),carprice)
# removing the unwanted column CarName
#carprice <- carprice[,-which(colnames(carprice) == "CarName")]

# other ways of splitting the columns
#do.call("rbind",strsplit(sub(" ",";",carnames), ";"))
#do.call("rbind",regmatches(carnames, regexpr(" ", carnames), invert = TRUE))

#----------------
# Date cleaning
#----------------

# REPLACE NAs with "missing"
carprice$modelname[which(is.na(carprice$modelname))] <- "missing"

# DUPLICATES check for duplicate rows, columns
unique(carprice$car_ID)
# number of duplicates
sum(duplicated(carprice))

# SPELLING MISTAKES: remove spelling mistakes, values in the data sets
# toyota,vw,porcshce, mazda, vokswagen

carprice$carmake[which(carprice$carmake %in% c("toyouta"))] <- "toyota"
carprice$carmake[which(carprice$carmake %in% c("vw","vokswagen"))] <- "volkswagen"
carprice$carmake[which(carprice$carmake %in% c("porcshce"))] <- "porsche"
carprice$carmake[which(carprice$carmake %in% c("maxda"))] <- "mazda"

# LOWER CASE: convert to lower case characters
carprice$carmake <- tolower(carprice$carmake)
carprice$modelname <- tolower(carprice$modelname)

# verify the uniques
unique(carprice$carmake)
unique(carprice$modelname)

# INVALID CHARACTERES: cleaning invalid characters
carprice$modelname[which(carprice$modelname %in% c("100 ls"))] <- "100ls"

# ROUNDNG ERRORS:round the decimals in the prices to near $
carprice$price <- round(x = carprice$price,0)

# FACTORIZATION OF COLUMNS: convert characters to factors
str(carprice)

# covnerting the in symboling, carmake, car model name into factors.
carprice$symboling <- as.factor(carprice$symboling)
carprice$carmake <- as.factor(carprice$carmake)
carprice$modelname <- as.factor(carprice$modelname)

# converting the factor variables with two values to integers
carprice$enginelocation <- ifelse(carprice$enginelocation == "front",1,0)
carprice$doornumber <- ifelse(carprice$doornumber == "four",1,0)
carprice$aspiration <- ifelse(carprice$aspiration == "std",1,0)
carprice$fueltype <- ifelse(carprice$fueltype == "gas",1,0)


# UNWANTED COLUMNS: removing columns that are not needed for analysis

# car_id is an identifier of a car and hence it is not an attribute of car. Thus it can be ignored from the anaylsis of the car.
# model_name of car and is aggregation of all attributes. Hence, can be ignored from the analysis. 
# Thus, deleting the car_id column
carprice <- carprice[,-which(names(carprice) == "car_ID")]
# and deleting the modelname column
carprice <- carprice[,-which(names(carprice) == "modelname")]

str(carprice)

# CHECKING OUTLIERS

# Carlength outliers: none found. mean and median close to each other
quantile(x = carprice$carlength,seq(0,1,0.01))
summary(carprice$carlength)

# carheight outliers: none found. Mean and median are close to each other
quantile(carprice$carheight,seq(0,1,0.01))
summary(carprice$carheight)

#carwidth outliers: none found. Mean and median are close to each other
quantile(carprice$carwidth,seq(0,1,0.01))
summary(carprice$carwidth)

#highwaympg outliers: none found. Mean and median are close to each other
quantile(x = carprice$highwaympg,seq(0,1,0.01))
summary(carprice$highwaympg)

# citympg outliers: none found. Mean and median are close to each other
quantile(x = carprice$citympg,seq(0,1,0.01))
summary(carprice$citympg)


# peak rpm outliers: none found. Mean and median are close to each other
quantile(x = carprice$peakrpm,seq(0,1,0.01))
summary(carprice$peakrpm)

# horspower outliers: 
quantile(x = carprice$horsepower,seq(0,1,0.01))
# slightly high horsepower found. Hence normalized with 99%
carprice$horsepower[which(carprice$horsepower > 207)] <- 207

# compression ratio outliers: 
quantile(x = carprice$compressionratio,seq(0,1,0.01))
summary(carprice$compressionratio)
# deviation found from 91 percentile, normalized with 10.94 - 90th percentile item
carprice$compressionratio[which(carprice$compressionratio > 10.94)] <- 10.94

# stroke outliers: none found. Mean and median are close to each other
quantile(carprice$stroke,seq(0,1,0.01))
summary(carprice$stroke)
summary(carprice$boreratio)

# curbweight outliers: none found. Mean and median are close to each other
quantile(carprice$curbweight,seq(0,1,0.01))
summary(carprice$curbweight)

#carprice$curbweight[which(carprice$curbweight > 3768.40)] <- 3768.40
#carprice$curbweight[which(carprice$curbweight < 1819.72)] <- 1819.72


#--------------
# DUMMY VARIABLES:  creating dummary variables using dummies
#--------------

# creating dummies for symboling, carmake, carbody, drivewheel,enginetype, cylindernumber,fuelsystem,
summary(carprice$symboling)
summary(carprice$carmake)
summary(carprice$carbody)
summary(carprice$drivewheel)
summary(carprice$enginetype)
summary(carprice$cylindernumber)
summary(carprice$fuelsystem)

# ****symboling****
# Reducing the levels in symboling
# symb_var <- carprice$symboling

levels(carprice$symboling) <- list("m2_0" = c("-2","-1","0"),
                                   "p1_3" = c("1","2","3"))

# creating dummy variable for Symboling
symb_dummy_1 <- data.frame(model.matrix(~symboling,data = carprice))
# #removing the (intercept) and keeping the "symbolingp1_3"
# # removing the first column
symb_dummy_1 <- symb_dummy_1[,-1]

# binding the model matrix with symboling columns that contains the integers
carprice <- cbind(symb_dummy_1,carprice[,-which(names(carprice) == "symboling")])

#rename the symboling column names
names(carprice)[which(names(carprice) == "symb_dummy_1")] <- "symboling_p1_3"



# **** carmake ****
#Reducing the levels of carmake
# AH represents the carmakes beginning from A to H. Similarly
# IP represents the carmakers beginning from I to P
# RV represents the carmakers beginning from R to V

#carprice$carmake <- carprice_test$carmake
levels(carprice$carmake) <- list(AH = c("alfa-romero","audi","bmw","buick","chevrolet","dodge","honda"),
                         IP = c("isuzu","jaguar","mazda","mercury","mitsubishi","nissan","peugeot","plymouth", "porsche"),
                         RV = c( "renault","saab","subaru","toyota","volkswagen","volvo" ))

# creating the dummy variable for Carmake
carmake_dummy_1 <- data.frame(model.matrix(object = ~carmake,data = carprice))
carmake_dummy_1 <- carmake_dummy_1[,-1]
carprice <- cbind(carmake_dummy_1,carprice[,-which(names(carprice) == "carmake")])


# **** cylindernumbers *****
# Reducing the levels of cyclindernumers by grouping the cylinders
# two,three - 23
# four, five, six - 456
# eight, twelve - 8_12

levels(carprice$cylindernumber) <- list( "2_3" = c("two","three"),  
                            "4_5_6" = c("four", "five", "six"), 
                            "8_12" = c("eight", "twelve"))


# creating the dummy variable for cylindernumer
cyln_dummy_1 <- data.frame(model.matrix(object = ~cylindernumber,data = carprice))
cyln_dummy_1 <- cyln_dummy_1[,-1]
carprice <- cbind(cyln_dummy_1,carprice[,-which(names(carprice) == "cylindernumber")])


# **** fuelsystem ****
# reducing the levels in fuel system
#fuelsys_var <- carprice$fuelsystem
levels(carprice$fuelsystem) <- list( "1_4" = c("1bbl","2bbl","4bbl"),
                             "i_m" = c("idi","mfi","mpfi"),
                             "s" = c("spdi","spfi"))

# creating Dummyvariables for fuelsystem
fuel_dummy_1 <- data.frame(model.matrix(object = ~fuelsystem,data = carprice))
# removing intercept
fuel_dummy_1 <- fuel_dummy_1[,-1]
# binding the fueldummy variable and carprice model matrix dataframe
carprice <- cbind(fuel_dummy_1,carprice[,-which(names(carprice) == "fuelsystem")])


# **** enginetype ****
# reducing the levels in enginetype
#eng_ty <- carprice$enginetype
levels(carprice$enginetype) <- list(dh = c("dohc","dohcv"),
                                    oh = c("ohc","ohcf","ohcv"),
                                    lr = c("l","rotor"))

# creating dummy variables for enginetype
eng_typ_dummy_1 <- data.frame(model.matrix(object = ~enginetype,data = carprice))
# removing the intercept
eng_typ_dummy_1 <- eng_typ_dummy_1[,-1]
carprice <- cbind(eng_typ_dummy_1,carprice[,-which(names(carprice) == "enginetype")])


# **** carbody ****
# reducing the number of levels in the "carbody"
summary(carprice$carbody)
# converting the levels in accordance with the first characters
levels(carprice$carbody) <- list(ch = c("convertible","hardtop","hatchback"), sw = c("sedan","wagon"))

# creaating the "carbody" dummy variables
carbody_dummy_1 <- data.frame(model.matrix(object = ~carbody,data = carprice))
# removing the intercept from the carbody
carbody_dummy_1 <- carbody_dummy_1[,-1]

# cbind the carbody with carprice dataframe
carprice <- cbind(carbody_dummy_1,carprice[,-which(names(carprice) == "carbody")])

# rename carbody_dummy_1 with "carbody_sw"
names(carprice)[which(names(carprice) == "carbody_dummy_1")] <- "carbody_sw"


# creating a dummy data frame from the available list of factors
#carprice_dummydf_new <- dummy.data.frame(data = carprice,sep = "_",drop = T)

# **** drivewheel ****
summary(carprice$drivewheel)
# convert drivewheel to dummy variables
drive_wheel_dummy <- data.frame(model.matrix(object = ~drivewheel,data = carprice))
drive_wheel_dummy <- drive_wheel_dummy[,-1]
carprice <- cbind(drive_wheel_dummy,carprice[,-which(names(carprice) == "drivewheel")])



#----------------------
# EXTRACTING test dataset and train dataset FROM THE MAIN DATASET
#---------------------

set.seed(100)
# selecting the indices training indices randomly
indices_carprice <- sample(x = 1:nrow(carprice),size = 0.7 * nrow(carprice))
# training dataset of carprice indices
train_carprice <- carprice[indices_carprice,]

# testing carprice dataset
test_carprice <- carprice[-indices_carprice,]

# checking na in carprice_dummy_df
sum(is.na(carprice))

#---------------------
# MODEL CREATION
#--------------------
# testing the model using the dummy dataframe of carprice

model_carprice_1 <- lm(price~.,data = train_carprice)
summary(model_carprice_1)

# executing the stepAIC to generate the recurrsive linearl regression model in both directions.
step <- stepAIC(object = model_carprice_1,direction = "both")
step

# using the variables outputed from StepAIC function.

model_carprice_2 <- lm(formula = price ~ drivewheelrwd + enginetypeoh + enginetypelr + 
                         fuelsystemi_m + cylindernumber4_5_6 + cylindernumber8_12 + 
                         carmakeIP + carmakeRV + aspiration + doornumber + enginelocation + 
                         carwidth + carheight + curbweight + enginesize + boreratio + 
                         stroke + horsepower + peakrpm + citympg + highwaympg, data = train_carprice)
summary(model_carprice_2)
vif(model_carprice_2)
write.csv(x = vif(model_carprice_2),file = "vif1.csv")

# REMOVING INSIGNIFICANT AND COLLINEAR VARIABLES

# Mode1_3 , removing the insignificant variable, "peakrpm"	9.02E-01	5.94E-01	1.52	p-value = 0.13121		15.85524884

model_carprice_3 <- lm(formula = price ~ drivewheelrwd + enginetypeoh + enginetypelr + 
                         fuelsystemi_m + cylindernumber4_5_6 + cylindernumber8_12 + 
                         carmakeIP + carmakeRV + aspiration + doornumber + enginelocation + 
                         carwidth + carheight + curbweight + enginesize + boreratio + 
                         stroke + horsepower + citympg + highwaympg, data = train_carprice)
summary(model_carprice_3)
vif(model_carprice_3)

write.csv(x = vif(model_carprice_3),file = "vif1.csv")


# model_4: removing the insignificant variable "citympg"	-201.962	138.589	-1.457	p-value=0.14761		vif=32.89608736

model_carprice_4 <- lm(formula = price ~ drivewheelrwd + enginetypeoh + enginetypelr + 
                         fuelsystemi_m + cylindernumber4_5_6 + cylindernumber8_12 + 
                         carmakeIP + carmakeRV + aspiration + doornumber + enginelocation + 
                         carwidth + carheight + curbweight + enginesize + boreratio + 
                         stroke + horsepower + highwaympg, data = train_carprice)
summary(model_carprice_4)
vif(model_carprice_4)

write.csv(x = vif(model_carprice_4),file = "vif1.csv")

# mode_5 : removing the insignificant variable, "horsepower"	-1.66E+01	1.33E+01	-1.253	p-value = 0.21		vif = 10.18157278

model_carprice_5 <- lm(formula = price ~ drivewheelrwd + enginetypeoh + enginetypelr + 
                         fuelsystemi_m + cylindernumber4_5_6 + cylindernumber8_12 + 
                         carmakeIP + carmakeRV + aspiration + doornumber + enginelocation + 
                         carwidth + carheight + curbweight + enginesize + boreratio + 
                         stroke + highwaympg, data = train_carprice)
summary(model_carprice_5)
vif(model_carprice_5)

write.csv(x = vif(model_carprice_5),file = "vif1.csv")

# model_6:removing the insifnificant variable "curbweight"	3	1.308	2.294	p-value=0.023456	*	vif = 18.12140588

model_carprice_6 <- lm(formula = price ~ drivewheelrwd + enginetypeoh + enginetypelr + 
                         fuelsystemi_m + cylindernumber4_5_6 + cylindernumber8_12 + 
                         carmakeIP + carmakeRV + aspiration + doornumber + enginelocation + 
                         carwidth + carheight +  enginesize + boreratio + 
                         stroke + highwaympg, data = train_carprice)
summary(model_carprice_6)
vif(model_carprice_6)

write.csv(x = vif(model_carprice_6),file = "vif1.csv")

# model_7: removing the insignificant variable, "fuelsystemi_m"	828.88	539.6	1.536	p-value 0.127043		2.671184688

model_carprice_7 <- lm(formula = price ~ drivewheelrwd + enginetypeoh + enginetypelr + 
                         cylindernumber4_5_6 + cylindernumber8_12 + 
                         carmakeIP + carmakeRV + aspiration + doornumber + enginelocation + 
                         carwidth + carheight +  enginesize + boreratio + 
                         stroke + highwaympg, data = train_carprice)
summary(model_carprice_7)
vif(model_carprice_7)

write.csv(x = vif(model_carprice_7),file = "vif1.csv")

# model_8: removing the lesser significant "highwaympg"	-31.34	38.22	-0.82	p-value= 0.413816		vif = 2.563276132

model_carprice_8 <- lm(formula = price ~ drivewheelrwd + enginetypeoh + enginetypelr + 
                         cylindernumber4_5_6 + cylindernumber8_12 + 
                         carmakeIP + carmakeRV + aspiration + doornumber + enginelocation + 
                         carwidth + carheight +  enginesize + boreratio + stroke, data = train_carprice)
summary(model_carprice_8)
vif(model_carprice_8)

write.csv(x = vif(model_carprice_8),file = "vif1.csv")

# model_9: removing the insignificiant variable, "boreratio"	-1909.06	973.99	-1.96	p-value = 0.052	.	vif = 2.58446419

model_carprice_9 <- lm(formula = price ~ drivewheelrwd + enginetypeoh + enginetypelr + 
                         cylindernumber4_5_6 + cylindernumber8_12 + 
                         carmakeIP + carmakeRV + aspiration + doornumber + enginelocation + 
                         carwidth + carheight +  enginesize + stroke, data = train_carprice)
summary(model_carprice_9)
vif(model_carprice_9)

write.csv(x = vif(model_carprice_9),file = "vif1.csv")

# model_10: removing insignificant variable "drivewheelrwd"	1119.78	552.38	2.027	p-value =0.044719

model_carprice_10 <- lm(formula = price ~ enginetypeoh + enginetypelr + 
                          cylindernumber4_5_6 + cylindernumber8_12 + 
                          carmakeIP + carmakeRV + aspiration + doornumber + enginelocation + 
                          carwidth + carheight +  enginesize + stroke, data = train_carprice)
summary(model_carprice_10)
vif(model_carprice_10)

write.csv(x = vif(model_carprice_10),file = "vif1.csv")

# model_11: removing insignificant variable "doornumber"	745.584	427.368	1.745	p-value=0.083437

model_carprice_11 <- lm(formula = price ~ enginetypeoh + enginetypelr + 
                          cylindernumber4_5_6 + cylindernumber8_12 + 
                          carmakeIP + carmakeRV + aspiration + enginelocation + 
                          carwidth + carheight +  enginesize + stroke, data = train_carprice)
summary(model_carprice_11)
vif(model_carprice_11)

write.csv(x = vif(model_carprice_11),file = "vif1.csv")

# model_12: removing insignificant variable "enginetypeoh"	-2456.12	677.91	-3.623	0.000416	***

model_carprice_12 <- lm(formula = price ~ enginetypelr + 
                          cylindernumber4_5_6 + cylindernumber8_12 + 
                          carmakeIP + carmakeRV + aspiration + enginelocation + 
                          carwidth + carheight +  enginesize + stroke, data = train_carprice)
summary(model_carprice_12)
vif(model_carprice_12)

write.csv(x = vif(model_carprice_12),file = "vif1.csv")


# model_13: removing insignificant variable "enginetypelr	-3510.3	1061.89	-3.306	0.001222	**

model_carprice_13 <- lm(formula = price ~ cylindernumber4_5_6 + cylindernumber8_12 + 
                          carmakeIP + carmakeRV + aspiration + enginelocation + 
                          carwidth + carheight +  enginesize + stroke, data = train_carprice)
summary(model_carprice_13)
vif(model_carprice_13)

write.csv(x = vif(model_carprice_13),file = "vif1.csv")

# model_14: removing insignificant variable "carheight	199.556	84.33	2.366	p-value = 0.019417	*"

model_carprice_14 <- lm(formula = price ~ cylindernumber4_5_6 + cylindernumber8_12 + 
                          carmakeIP + carmakeRV + aspiration + enginelocation + 
                          carwidth +  enginesize + stroke, data = train_carprice)
summary(model_carprice_14)
vif(model_carprice_14)

write.csv(x = vif(model_carprice_14),file = "vif1.csv")


# model_15: removing insignificant variable "cylindernumber8_12	-5025.09	2057.107	-2.443	0.015886	*

model_carprice_15 <- lm(formula = price ~ cylindernumber4_5_6 + 
                          carmakeIP + carmakeRV + aspiration + enginelocation + 
                          carwidth +  enginesize + stroke, data = train_carprice)
summary(model_carprice_15)
vif(model_carprice_15)

write.csv(x = vif(model_carprice_15),file = "vif1.csv")


# model_16: removing insignificant variable "aspiration"	-1957.804	539.807	-3.627	0.000407	***	1.225568958

model_carprice_16 <- lm(formula = price ~ cylindernumber4_5_6 + 
                          carmakeIP + carmakeRV + enginelocation + 
                          carwidth +  enginesize + stroke, data = train_carprice)
summary(model_carprice_16)
vif(model_carprice_16)

write.csv(x = vif(model_carprice_16),file = "vif1.csv")


# model_17: removing insignificant variable "stroke"	-2472.816	736.666	-3.357	0.00102	**	1.320993559

model_carprice_17 <- lm(formula = price ~ cylindernumber4_5_6 + 
                          carmakeIP + carmakeRV + enginelocation + 
                          carwidth +  enginesize, data = train_carprice)
summary(model_carprice_17)
vif(model_carprice_17)

write.csv(x = vif(model_carprice_17),file = "vif1.csv")


# model_18: removing insignificant variable "carmakeRV"	-2043.952	554.149	-3.688	0.000325	***	1.713765067

model_carprice_18 <- lm(formula = price ~ cylindernumber4_5_6 + 
                          carmakeIP + enginelocation + 
                          carwidth +  enginesize, data = train_carprice)
summary(model_carprice_18)
vif(model_carprice_18)

write.csv(x = vif(model_carprice_18),file = "vif1.csv")


# model_19: removing insignificant variable ""carmakeIP"	-1373.125	458.165	-2.997	0.00324	**

model_carprice_19 <- lm(formula = price ~ cylindernumber4_5_6 + 
                           enginelocation + 
                          carwidth +  enginesize, data = train_carprice)
summary(model_carprice_19)
vif(model_carprice_19)

write.csv(x = vif(model_carprice_19),file = "vif1.csv")


# model_20: removing insignificant variable ""cylindernumber4_5_6 + "	-1373.125	458.165	-2.997	0.00324	**

model_carprice_20 <- lm(formula = price ~ 
                          enginelocation + 
                          carwidth +  enginesize, data = train_carprice)
summary(model_carprice_20)
vif(model_carprice_20)

write.csv(x = vif(model_carprice_20),file = "vif1.csv")

# predicting the results with the help of the model_20 with the test carprice

predicted_value_test_carprice <- predict(model_carprice_20,test_carprice)

# remove(model_carprice_13,model_carprice_12,model_carprice_14,model_carprice_15,model_carprice_16,model_carprice_17,model_carprice_18,model_carprice_19,model_carprice_20,
#        model_carprice_21,model_carprice_22,model_carprice_23,model_carprice_24,model_carprice_25,model_carprice_26,model_carprice_27,model_carprice_28)

test_carprice$predicted_price <- predicted_value_test_carprice
View(test_carprice)

# error in the model

test_carprice$error_model11 <- test_carprice$price - test_carprice$predicted_price
test_carprice$s_no <- c(1:nrow(test_carprice))

# PLOTS FOR RANDOMNESS 

ggplot(test_carprice,aes(x = s_no,y = error_model11)) + 
  geom_point() + geom_line() + 
  geom_abline(intercept = 0,slope = 1) + 
  xlab("No of Cars") + ylab("Error between predicted and actual price")

## COMMENTS: error in the predicted price and price is random and it is evenly distributed.


# PLOTS:  PREDICTED PRICE VS ACTUAL PRICE
ggplot(test_carprice,aes(x = s_no,y = test_carprice$predicted_price)) + 
  geom_line(aes(color = factor("red",labels = c("predicted_price")))) +
  geom_line(aes(x = s_no, y = price, colour = factor("blue",labels = c("price")))) + 
  labs(title = "Predicted_Price Vs ActualPrice", x = "CarIDs", y = "Prices")  +
  theme(legend.title = element_blank())

# COMMENTS: From the graph, it can be inferred THAT the model is predicting the Valuesaccurately 
# and the predicted values are close to actual values. 


# IMPORTANT QUESTIONS ANSWERED  
# Which variables are significant in predicting the price of a car
# How well those variables describe the price of a car

# CONCLUSIONS:
#The final model with four MOST SIGNIFICANT variables are in the below decending order..
# 1. ENGINE SIZE
# 2. CARWIDTH
# 3. ENGINE LOCATION - front or rear
# 4. LAST BUT NOT LEAST CYCLINDERS, - four, five and six types (LEAST SIGNIFICANT)


#and adjusted R-squared approximately equal to 87% percent is very close to predicting the values as compared to original prices

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -77577.376  10680.695  -7.263 2.46e-11 ***
#   enginelocation -15824.093   1870.934  -8.458 3.36e-14 ***
#   carwidth         1410.986    185.175   7.620 3.57e-12 ***
#   enginesize        105.110      9.959  10.554  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2878 on 139 degrees of freedom
# Multiple R-squared:  0.8786,	Adjusted R-squared:  0.8759 
# F-statistic: 335.2 on 3 and 139 DF,  p-value: < 2.2e-16



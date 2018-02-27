# A Chinese automobile company Geely Auto aspires to enter the US market by setting up their manufacturing unit 
# there and producing cars locally to give competition to their US and European counterparts. 
# 
# 
# 
# They have contracted an automobile consulting company ****to understand the factors on which the pricing of a car depends****. 
# Specifically, they want to understand the factors affecting the pricing of cars in the American marketing, since those may be very different from the Chinese market. 
# Essentially, the company wants to know:
#   
# Which variables are significant in predicting the price of a car
# How well those variables describe the price of a car

#-------------
#Goal of this assignment
#-------------

# You are required to model the price of cars with the available independent variables. 
# It will be used by the management to understand how exactly the prices vary with the independent variables. They can accordingly manipulate the design of the cars, the business strategy etc. to meet certain price levels. 
# Further, the model will be a good way for the management to understand the pricing dynamics of a new market. 

#--------------
# Data Preparation
#--------------
# 
# There is a variable named CarName which is comprised of two parts - the first word is the name of 'car company' and the second is the 'car model'. 
# For example, chevrolet impala has 'chevrolet' as the car company name and 'impala' as the car model name. 
# ***You need to consider only company name as the independent variable for the model building. ***

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
carprice <- read.csv(file = "CarPrice_Assignment.csv",stringsAsFactors = T)
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

# check outliers
quantile(x = carprice$price,probs = seq(0,1,0.01))

# replace NAs with "missing"
carprice$modelname[which(is.na(carprice$modelname))] <- "missing"

# check for duplicate rows, columns
unique(carprice$car_ID)
# number of duplicates
sum(duplicated(carprice))

# remove spelling mistakes, values in the data sets
# toyota,vw,porcshce, mazda, vokswagen

carprice$carmake[which(carprice$carmake %in% c("toyouta"))] <- "toyota"
carprice$carmake[which(carprice$carmake %in% c("vw","vokswagen"))] <- "volkswagen"
carprice$carmake[which(carprice$carmake %in% c("porcshce"))] <- "porsche"
carprice$carmake[which(carprice$carmake %in% c("maxda"))] <- "mazda"

# convert to lower case characters
carprice$carmake <- tolower(carprice$carmake)
carprice$modelname <- tolower(carprice$modelname)

# verify the uniques
unique(carprice$carmake)
unique(carprice$modelname)

# cleaning invalid characters
carprice$modelname[which(carprice$modelname %in% c("100 ls"))] <- "100ls"

# round the decimals in the prices to near $
carprice$price <- round(x = carprice$price,0)

# convert characters to factors
str(carprice)

# covnerting the in symboling, carmake, car model name into factors.
carprice$symboling <- as.factor(carprice$symboling)
carprice$carmake <- as.factor(carprice$carmake)
carprice$modelname <- as.factor(carprice$modelname)

# converting the variables with two values to integer
carprice$enginelocation <- ifelse(carprice$enginelocation == "front",1,0)
carprice$doornumber <- ifelse(carprice$doornumber == "four",1,0)
carprice$aspiration <- ifelse(carprice$aspiration == "std",1,0)
carprice$fueltype <- ifelse(carprice$fueltype == "gas",1,0)


# removing columns that are not needed for analysis

# car_id is an identifier of a car and hence it is not an attribute of car. Thus it can be ignored from the anaylsis of the car.
# model_name of car and is aggregation of all attributes. Hence, can be ignored from the analysis. 
# Thus, deleting the car_id column
carprice <- carprice[,-which(names(carprice) == "car_ID")]
# anb deleting the modelname column
carprice <- carprice[,-which(names(carprice) == "modelname")]

str(carprice)

#--------------
# creating dummary variables using dummies
#--------------

# creating dummies for symboling, carmake, carbody, drivewheel,enginetype, cylindernumber,fuelsystem,
summary(carprice$symboling)
summary(carprice$carmake)
summary(carprice$carbody)
summary(carprice$drivewheel)
summary(carprice$enginetype)
summary(carprice$cylindernumber)
summary(carprice$fuelsystem)

# creating a dummy data frame from the available list of factors
carprice_dummydf <- dummy.data.frame(carprice)
View(carprice_dummydf)
str(carprice_dummydf)

#----------------------
# test dataset and train dataset
#---------------------

set.seed(100)
# selecting the indices training indices randomly
indices_carprice <- sample(x = 1:nrow(carprice_dummydf),size = 0.7 * nrow(carprice_dummydf))
# training dataset of carprice indices
train_carprice <- carprice_dummydf[indices_carprice,]

# testing carprice dataset
test_carprice <- carprice_dummydf[-indices_carprice,]

# checking na in carprice_dummy_df
sum(is.na(carprice_dummydf))

#---------------------
# model creating
#--------------------
# testing the model using the dummy dataframe of carprice

model_carprice_1 <- lm(price~.,data = train_carprice)
summary(model_carprice_1)

# executing the stepAIC to generate the recurrsive linearl regression model in both directions.
step <- stepAIC(object = model_carprice_1,direction = "both")

# using the variables outputed from StepAIC function.
model_carprice_2 <- lm(price ~ symboling2 + `carmakealfa-romero` + carmakeaudi + carmakebmw + 
                         carmakechevrolet + carmakedodge + carmakeisuzu + carmakejaguar + 
                         carmakemitsubishi + carmakenissan + carmakepeugeot + carmakeplymouth + 
                         carmakeporsche + carmakesaab + carmakesubaru + carmaketoyota + 
                         aspiration + doornumber + carbodyconvertible + carbodyhardtop + 
                         drivewheel4wd + drivewheelfwd + enginelocation + wheelbase + 
                         carlength + carheight + curbweight + enginetypedohc + enginetypedohcv + 
                         enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                         enginesize + fuelsystem1bbl + fuelsystemmpfi + boreratio + 
                         stroke + compressionratio + peakrpm + highwaympg, data = train_carprice)
summary(model_carprice_2)

write.csv(x = vif(model_carprice_2),file = "vif1.csv")

# Mode1_3 , removing the insignificant variable, fuelsystemmpfi p-value, 0.169103	vif = 4.252265519, # replacing drivewheel4wd, p-value: 0.144905, vif = 3.87542768

model_carprice_3 <- lm(price ~ symboling2 + `carmakealfa-romero` + carmakeaudi + carmakebmw + 
                         carmakechevrolet + carmakedodge + carmakeisuzu + carmakejaguar + 
                         carmakemitsubishi + carmakenissan + carmakepeugeot + carmakeplymouth + 
                         carmakeporsche + carmakesaab + carmakesubaru + carmaketoyota + 
                         aspiration + doornumber + carbodyconvertible + carbodyhardtop + drivewheel4wd +
                         drivewheelfwd + enginelocation + wheelbase + 
                         carlength + carheight + curbweight + enginetypedohc + enginetypedohcv + 
                         enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                         enginesize + fuelsystem1bbl + boreratio + 
                         stroke + compressionratio + peakrpm + highwaympg, data = train_carprice)
summary(model_carprice_3)
vif(model_carprice_3)

write.csv(x = vif(model_carprice_3),file = "vif2.csv")

# model_4: removing drivewheel4wd	= p-value0.15038	vif =	3.875053183


model_carprice_4 <- lm(price ~ symboling2 + `carmakealfa-romero` + carmakeaudi + carmakebmw + 
                         carmakechevrolet + carmakedodge + carmakeisuzu + carmakejaguar + 
                         carmakemitsubishi + carmakenissan + carmakepeugeot + carmakeplymouth + 
                         carmakeporsche + carmakesaab + carmakesubaru + carmaketoyota + 
                         aspiration + doornumber + carbodyconvertible + carbodyhardtop +
                         drivewheelfwd + enginelocation + wheelbase + 
                         carlength + carheight + curbweight + enginetypedohc + enginetypedohcv + 
                         enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                         enginesize + fuelsystem1bbl + boreratio + 
                         stroke + compressionratio + peakrpm + highwaympg, data = train_carprice)
summary(model_carprice_4)
vif(model_carprice_4)

write.csv(x = vif(model_carprice_4),file = "vif2.csv")

# mode_5 : compressionratio	-9.57E+01	6.08E+01	-1.573	p-value = 0.11886		vif = 4.383351696


model_carprice_5 <- lm(price ~ symboling2 + `carmakealfa-romero` + carmakeaudi + carmakebmw + 
                         carmakechevrolet + carmakedodge + carmakeisuzu + carmakejaguar + 
                         carmakemitsubishi + carmakenissan + carmakepeugeot + carmakeplymouth + 
                         carmakeporsche + carmakesaab + carmakesubaru + carmaketoyota + 
                         aspiration + doornumber + carbodyconvertible + carbodyhardtop +
                         drivewheelfwd + enginelocation + wheelbase + 
                         carlength + carheight + curbweight + enginetypedohc + enginetypedohcv + 
                         enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                         enginesize + fuelsystem1bbl + boreratio + 
                         stroke + peakrpm + highwaympg, data = train_carprice)
summary(model_carprice_5)
vif(model_carprice_5)

write.csv(x = vif(model_carprice_5),file = "vif2.csv")

# model_6: highwaympg	6.56E+01	4.29E+01	1.528	p-value = 0.12949	 vif =	6.537119297

model_carprice_6 <- lm(price ~ symboling2 + `carmakealfa-romero` + carmakeaudi + carmakebmw + 
                         carmakechevrolet + carmakedodge + carmakeisuzu + carmakejaguar + 
                         carmakemitsubishi + carmakenissan + carmakepeugeot + carmakeplymouth + 
                         carmakeporsche + carmakesaab + carmakesubaru + carmaketoyota + 
                         aspiration + doornumber + carbodyconvertible + carbodyhardtop +
                         drivewheelfwd + enginelocation + wheelbase + 
                         carlength + carheight + curbweight + enginetypedohc + enginetypedohcv + 
                         enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                         enginesize + fuelsystem1bbl + boreratio + 
                         stroke + peakrpm, data = train_carprice)
summary(model_carprice_6)
vif(model_carprice_6)

write.csv(x = vif(model_carprice_6),file = "vif2.csv")

# model_7: drivewheelfwd	-9.55E+02	5.06E+02	-1.888	0.06173	.	4.499411349

model_carprice_7 <- lm(price ~ symboling2 + `carmakealfa-romero` + carmakeaudi + carmakebmw + 
                         carmakechevrolet + carmakedodge + carmakeisuzu + carmakejaguar + 
                         carmakemitsubishi + carmakenissan + carmakepeugeot + carmakeplymouth + 
                         carmakeporsche + carmakesaab + carmakesubaru + carmaketoyota + 
                         aspiration + doornumber + carbodyconvertible + carbodyhardtop +
                         enginelocation + wheelbase + 
                         carlength + carheight + curbweight + enginetypedohc + enginetypedohcv + 
                         enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                         enginesize + fuelsystem1bbl + boreratio + 
                         stroke + peakrpm, data = train_carprice)
summary(model_carprice_7)
vif(model_carprice_7)

write.csv(x = vif(model_carprice_7),file = "vif2.csv")

#-----------------------------------------------
# variables with high VIF are of high sifnificance hence, they are left
# now considering the variable with high-pvalue and ignoring the vif as it turns out to be lesser significant
#-----------------------------------------------

# model_8: removing the lesser significant "carbodyhardtop"	8.90E+02	7.96E+02	1.118	p-value=0.26593

model_carprice_8 <- lm(price ~ symboling2 + `carmakealfa-romero` + carmakeaudi + carmakebmw + 
                         carmakechevrolet + carmakedodge + carmakeisuzu + carmakejaguar + 
                         carmakemitsubishi + carmakenissan + carmakepeugeot + carmakeplymouth + 
                         carmakeporsche + carmakesaab + carmakesubaru + carmaketoyota + 
                         aspiration + doornumber + carbodyconvertible +
                         enginelocation + wheelbase + 
                         carlength + carheight + curbweight + enginetypedohc + enginetypedohcv + 
                         enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                         enginesize + fuelsystem1bbl + boreratio + 
                         stroke + peakrpm, data = train_carprice)
summary(model_carprice_8)
vif(model_carprice_8)

write.csv(x = vif(model_carprice_8),file = "vif2.csv")


# model_9: carmakeaudi	9.75E+02	1.02E+03	0.96	p-value=0.33925

model_carprice_9 <- lm(price ~ symboling2 + `carmakealfa-romero` + carmakebmw + 
                         carmakechevrolet + carmakedodge + carmakeisuzu + carmakejaguar + 
                         carmakemitsubishi + carmakenissan + carmakepeugeot + carmakeplymouth + 
                         carmakeporsche + carmakesaab + carmakesubaru + carmaketoyota + 
                         aspiration + doornumber + carbodyconvertible +
                         enginelocation + wheelbase + 
                         carlength + carheight + curbweight + enginetypedohc + enginetypedohcv + 
                         enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                         enginesize + fuelsystem1bbl + boreratio + 
                         stroke + peakrpm, data = train_carprice)
summary(model_carprice_9)
vif(model_carprice_9)

write.csv(x = vif(model_carprice_9),file = "vif2.csv")



# model_10: removing symboling2	-4.49E+02	4.04E+02	-1.111	p-value = 0.26915

model_carprice_10 <- lm(price ~ `carmakealfa-romero` + carmakebmw + 
                         carmakechevrolet + carmakedodge + carmakeisuzu + carmakejaguar + 
                         carmakemitsubishi + carmakenissan + carmakepeugeot + carmakeplymouth + 
                         carmakeporsche + carmakesaab + carmakesubaru + carmaketoyota + 
                         aspiration + doornumber + carbodyconvertible +
                         enginelocation + wheelbase + 
                         carlength + carheight + curbweight + enginetypedohc + enginetypedohcv + 
                         enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                         enginesize + fuelsystem1bbl + boreratio + 
                         stroke + peakrpm, data = train_carprice)
summary(model_carprice_10)
vif(model_carprice_10)

write.csv(x = vif(model_carprice_10),file = "vif2.csv")

# model_11: removing carmakenissan	-5.36E+02	5.66E+02	-0.947	p-value=0.34595

model_carprice_11 <- lm(price ~ `carmakealfa-romero` + carmakebmw + 
                          carmakechevrolet + carmakedodge + carmakeisuzu + carmakejaguar + 
                          carmakemitsubishi + carmakepeugeot + carmakeplymouth + 
                          carmakeporsche + carmakesaab + carmakesubaru + carmaketoyota + 
                          aspiration + doornumber + carbodyconvertible +
                          enginelocation + wheelbase + 
                          carlength + carheight + curbweight + enginetypedohc + enginetypedohcv + 
                          enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                          enginesize + fuelsystem1bbl + boreratio + 
                          stroke + peakrpm, data = train_carprice)
summary(model_carprice_11)
vif(model_carprice_11)

write.csv(x = vif(model_carprice_11),file = "vif2.csv")


# model_12: removing carmakeisuzu	-1.49E+03	8.10E+02	-1.833	p-value = 0.06946

model_carprice_12 <- lm(price ~ `carmakealfa-romero` + carmakebmw + 
                          carmakechevrolet + carmakedodge + carmakejaguar + 
                          carmakemitsubishi + carmakepeugeot + carmakeplymouth + 
                          carmakeporsche + carmakesaab + carmakesubaru + carmaketoyota + 
                          aspiration + doornumber + carbodyconvertible +
                          enginelocation + wheelbase + 
                          carlength + carheight + curbweight + enginetypedohc + enginetypedohcv + 
                          enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                          enginesize + fuelsystem1bbl + boreratio + 
                          stroke + peakrpm, data = train_carprice)
summary(model_carprice_12)
vif(model_carprice_12)

write.csv(x = vif(model_carprice_12),file = "vif2.csv")

# model_13: removing `carmakealfa-romero`	-4.32E+03	2.18E+03	-1.987	p-value = 0.04936

model_carprice_13 <- lm(price ~ carmakebmw + 
                          carmakechevrolet + carmakedodge + carmakejaguar + 
                          carmakemitsubishi + carmakepeugeot + carmakeplymouth + 
                          carmakeporsche + carmakesaab + carmakesubaru + carmaketoyota + 
                          aspiration + doornumber + carbodyconvertible +
                          enginelocation + wheelbase + 
                          carlength + carheight + curbweight + enginetypedohc + enginetypedohcv + 
                          enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                          enginesize + fuelsystem1bbl + boreratio + 
                          stroke + peakrpm, data = train_carprice)
summary(model_carprice_13)
vif(model_carprice_13)

write.csv(x = vif(model_carprice_13),file = "vif2.csv")

# model_14: removing carmakejaguar	-4.47E+03	2.02E+03	-2.211	p-value = 0.02904

model_carprice_14 <- lm(price ~ carmakebmw + 
                          carmakechevrolet + carmakedodge +
                          carmakemitsubishi + carmakepeugeot + carmakeplymouth + 
                          carmakeporsche + carmakesaab + carmakesubaru + carmaketoyota + 
                          aspiration + doornumber + carbodyconvertible +
                          enginelocation + wheelbase + 
                          carlength + carheight + curbweight + enginetypedohc + enginetypedohcv + 
                          enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                          enginesize + fuelsystem1bbl + boreratio + 
                          stroke + peakrpm, data = train_carprice)
summary(model_carprice_14)
vif(model_carprice_14)

write.csv(x = vif(model_carprice_14),file = "vif2.csv")


# model_15: removing "doornumber"	7.88E+02	3.79E+02	2.078	p-value=0.04002

model_carprice_15 <- lm(price ~ carmakebmw + 
                          carmakechevrolet + carmakedodge +
                          carmakemitsubishi + carmakepeugeot + carmakeplymouth + 
                          carmakeporsche + carmakesaab + carmakesubaru + carmaketoyota + 
                          aspiration + carbodyconvertible +
                          enginelocation + wheelbase + 
                          carlength + carheight + curbweight + enginetypedohc + enginetypedohcv + 
                          enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                          enginesize + fuelsystem1bbl + boreratio + 
                          stroke + peakrpm, data = train_carprice)
summary(model_carprice_15)
vif(model_carprice_15)

write.csv(x = vif(model_carprice_15),file = "vif2.csv")




# model_16: removing "carlength"	-8.87E+01	3.84E+01	-2.31	p-value = 0.02271

model_carprice_16 <- lm(price ~ carmakebmw + 
                          carmakechevrolet + carmakedodge +
                          carmakemitsubishi + carmakepeugeot + carmakeplymouth + 
                          carmakeporsche + carmakesaab + carmakesubaru + carmaketoyota + 
                          aspiration + carbodyconvertible +
                          enginelocation + wheelbase + 
                          carheight + curbweight + enginetypedohc + enginetypedohcv + 
                          enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                          enginesize + fuelsystem1bbl + boreratio + 
                          stroke + peakrpm, data = train_carprice)
summary(model_carprice_16)
vif(model_carprice_16)

write.csv(x = vif(model_carprice_16),file = "vif2.csv")


# model_17: removing "fuelsystem1bbl"	-1.21E+03	7.18E+02	-1.687	p-value=0.09440

model_carprice_17 <- lm(price ~ carmakebmw + 
                          carmakechevrolet + carmakedodge +
                          carmakemitsubishi + carmakepeugeot + carmakeplymouth + 
                          carmakeporsche + carmakesaab + carmakesubaru + carmaketoyota + 
                          aspiration + carbodyconvertible +
                          enginelocation + wheelbase + 
                          carheight + curbweight + enginetypedohc + enginetypedohcv + 
                          enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                          enginesize + boreratio + 
                          stroke + peakrpm, data = train_carprice)
summary(model_carprice_17)
vif(model_carprice_17)

write.csv(x = vif(model_carprice_17),file = "vif2.csv")



# model_18: removing carmakechevrolet	-1.85E+03	1.14E+03	-1.623	p-value=0.10729

model_carprice_18 <- lm(price ~ carmakebmw + 
                          carmakedodge +
                          carmakemitsubishi + carmakepeugeot + carmakeplymouth + 
                          carmakeporsche + carmakesaab + carmakesubaru + carmaketoyota + 
                          aspiration + carbodyconvertible +
                          enginelocation + wheelbase + 
                          carheight + curbweight + enginetypedohc + enginetypedohcv + 
                          enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                          enginesize + boreratio + 
                          stroke + peakrpm, data = train_carprice)
summary(model_carprice_18)
vif(model_carprice_18)

write.csv(x = vif(model_carprice_18),file = "vif2.csv")


# model_19: removing peakrpm	1.17E+00	4.06E-01	2.884	p-value = 0.00467

model_carprice_19 <- lm(price ~ carmakebmw + 
                          carmakedodge +
                          carmakemitsubishi + carmakepeugeot + carmakeplymouth + 
                          carmakeporsche + carmakesaab + carmakesubaru + carmaketoyota + 
                          aspiration + carbodyconvertible +
                          enginelocation + wheelbase + 
                          carheight + curbweight + enginetypedohc + enginetypedohcv + 
                          enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                          enginesize + boreratio + 
                          stroke, data = train_carprice)
summary(model_carprice_19)
vif(model_carprice_19)

write.csv(x = vif(model_carprice_19),file = "vif2.csv")


# model_20: removing enginelocation	7.31E+03	2.88E+03	2.536	p-value = 0.01252

model_carprice_20 <- lm(price ~ carmakebmw + 
                          carmakedodge +
                          carmakemitsubishi + carmakepeugeot + carmakeplymouth + 
                          carmakeporsche + carmakesaab + carmakesubaru + carmaketoyota + 
                          aspiration + carbodyconvertible +
                          wheelbase + 
                          carheight + curbweight + enginetypedohc + enginetypedohcv + 
                          enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                          enginesize + boreratio + 
                          stroke, data = train_carprice)
summary(model_carprice_20)
vif(model_carprice_20)

write.csv(x = vif(model_carprice_20),file = "vif2.csv")


# model_21: removing "carmakesaab"	2.87E+03	1.07E+03	2.684	0.00832	**	1.253973876

model_carprice_21 <- lm(price ~ carmakebmw + 
                          carmakedodge +
                          carmakemitsubishi + carmakepeugeot + carmakeplymouth + 
                          carmakeporsche + carmakesubaru + carmaketoyota + 
                          aspiration + carbodyconvertible +
                          wheelbase + 
                          carheight + curbweight + enginetypedohc + enginetypedohcv + 
                          enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                          enginesize + boreratio + 
                          stroke, data = train_carprice)
summary(model_carprice_21)
vif(model_carprice_21)

write.csv(x = vif(model_carprice_21),file = "vif2.csv")

# model_22: removing "carbodyconvertible"	2704.101	1081.148	2.501	p-value=0.01373

model_carprice_22 <- lm(price ~ carmakebmw + 
                          carmakedodge +
                          carmakemitsubishi + carmakepeugeot + carmakeplymouth + 
                          carmakeporsche + carmakesubaru + carmaketoyota + 
                          aspiration + 
                          wheelbase + 
                          carheight + curbweight + enginetypedohc + enginetypedohcv + 
                          enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                          enginesize + boreratio + 
                          stroke, data = train_carprice)
summary(model_carprice_22)
vif(model_carprice_22)

write.csv(x = vif(model_carprice_22),file = "vif2.csv")


# model_22: removing "carbodyconvertible"	2704.101	1081.148	2.501	p-value=0.01373

model_carprice_22 <- lm(price ~ carmakebmw + 
                          carmakedodge +
                          carmakemitsubishi + carmakepeugeot + carmakeplymouth + 
                          carmakeporsche + carmakesubaru + carmaketoyota + 
                          aspiration + 
                          wheelbase + 
                          carheight + curbweight + enginetypedohc + enginetypedohcv + 
                          enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                          enginesize + boreratio + 
                          stroke, data = train_carprice)
summary(model_carprice_22)
vif(model_carprice_22)

write.csv(x = vif(model_carprice_22),file = "vif2.csv")

# model_23: removing "carmakedodge"	-2.40E+03	7.01E+02	-3.428	p-value=0.00083	***

model_carprice_23 <- lm(price ~ carmakebmw + 
                          carmakemitsubishi + carmakepeugeot + carmakeplymouth + 
                          carmakeporsche + carmakesubaru + carmaketoyota + 
                          aspiration + 
                          wheelbase + 
                          carheight + curbweight + enginetypedohc + enginetypedohcv + 
                          enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                          enginesize + boreratio + 
                          stroke, data = train_carprice)
summary(model_carprice_23)
vif(model_carprice_23)

write.csv(x = vif(model_carprice_23),file = "vif2.csv")




# model_24: removing "carheight"	-298.196	92.916	-3.209	p-value = 0.0017

model_carprice_24 <- lm(price ~ carmakebmw + 
                          carmakemitsubishi + carmakepeugeot + carmakeplymouth + 
                          carmakeporsche + carmakesubaru + carmaketoyota + 
                          aspiration + 
                          wheelbase + 
                          curbweight + enginetypedohc + enginetypedohcv + 
                          enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                          enginesize + boreratio + 
                          stroke, data = train_carprice)
summary(model_carprice_24)
vif(model_carprice_24)

write.csv(x = vif(model_carprice_24),file = "vif2.csv")

# model_25: removing "carmakeporsche"	4789.828	1704.013	2.811	p=0.00575

model_carprice_25 <- lm(price ~ carmakebmw + 
                          carmakemitsubishi + carmakepeugeot + carmakeplymouth + 
                          carmakesubaru + carmaketoyota + 
                          aspiration + 
                          wheelbase + 
                          curbweight + enginetypedohc + enginetypedohcv + 
                          enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                          enginesize + boreratio + 
                          stroke, data = train_carprice)
summary(model_carprice_25)
vif(model_carprice_25)

write.csv(x = vif(model_carprice_25),file = "vif2.csv")

# model_26: removing "carmakeplymouth"	-2345.34	830.116	-2.825	0.005507

model_carprice_26 <- lm(price ~ carmakebmw + 
                          carmakemitsubishi + carmakepeugeot + 
                          carmakesubaru + carmaketoyota + 
                          aspiration + 
                          wheelbase + 
                          curbweight + enginetypedohc + enginetypedohcv + 
                          enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                          enginesize + boreratio + 
                          stroke, data = train_carprice)
summary(model_carprice_26)
vif(model_carprice_26)

write.csv(x = vif(model_carprice_26),file = "vif2.csv")

# model_27: removing "carmakemitsubishi"	-2001.196	737.596	-2.713	p-value = 0.007606

model_carprice_27 <- lm(price ~ carmakebmw + 
                          carmakepeugeot + 
                          carmakesubaru + carmaketoyota + 
                          aspiration + 
                          wheelbase + 
                          curbweight + enginetypedohc + enginetypedohcv + 
                          enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                          enginesize + boreratio + 
                          stroke, data = train_carprice)
summary(model_carprice_27)
vif(model_carprice_27)

write.csv(x = vif(model_carprice_27),file = "vif2.csv")

# model_28: removing "enginetypedohcv"	-6946.436	2303.291	-3.016 p-value =	0.0031

model_carprice_28 <- lm(price ~ carmakebmw + 
                          carmakepeugeot + 
                          carmakesubaru + carmaketoyota + 
                          aspiration + 
                          wheelbase + 
                          curbweight + enginetypedohc + 
                          enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                          enginesize + boreratio + 
                          stroke, data = train_carprice)
summary(model_carprice_28)
vif(model_carprice_28)

write.csv(x = vif(model_carprice_28),file = "vif2.csv")












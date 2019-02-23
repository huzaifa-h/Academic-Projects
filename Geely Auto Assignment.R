setwd("C:/Users/user/Downloads")

install.packages("DescTools")
library(DescTools)
library(dplyr)
library(tidyr)
library(car)
library(MASS)
library(ggplot2)
install.packages("Hmisc")
library(Hmisc)

# Importing data into R 

cars <- read.csv("CarPrice_Assignment.csv")

# Cursory examination of the data

View(cars)
str(cars)
dim(cars)
summary(cars)
describe(cars)

# Checking data for duplicate values

unique(cars)
cars[which(duplicated(cars)),]
## No duplicate values

# Checking for missing values

sum(is.na(cars)) 
#No missing values

# Preparing data for modelling

# Exploring categorical variables in the data

describe(cars$CarName)
summary(cars$CarName)
## Seperating car name and model

cars <- separate(cars, CarName, c('company', 'model'), sep = '[[:blank:]]')
View(cars)
## Drop model column as not used for analysis

cars <- cars[, -4]

View(cars)
describe(cars$company)
View(cars$company)
table(cars$company)

## spelling mistakes identified
## Correcting spelling mistakes

## Changing names with consistent names in cars$company

cars$company <- tolower(cars$company) # for nissan
cars$company[which(cars$company == "maxda")] = "mazda"
cars$company[which(cars$company == "porcshce")] = "porsche"
cars$company[which(cars$company == "toyouta")] = "toyota"
cars$company[which(cars$company == "vokswagen" | cars$company == "vw")] = "volkswagen"

table(cars$company)
View(cars)

## Car company spelling mistakes fixed

install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)

describe(as.factor(cars$symboling))

describe(cars$fueltype)

describe(cars$aspiration)

describe(cars$doornumber)

describe(cars$doornumber)

describe(cars$carbody)

describe(cars$drivewheel)

describe(cars$enginelocation)

describe(cars$enginetype)

describe(cars$cylindernumber)

describe(cars$fuelsystem)

# No issues identified with the categorical variables. 

# Exploring continuous variables in the data


describe(cars$symboling)
quantile(cars$symboling, seq(0,1,0.01))
## '-2' appears only 3 times (1.5%)
## Altough this is categorical variable. but since the values are numberical so considering as continuous

describe(cars$wheelbase)
quantile(cars$wheelbase, seq(0,1,0.01))
boxplot(cars$wheelbase)
cars$wheelbase

quantile(cars$carlength, seq(0,1,0.01))
boxplot(cars$carlength) #hardly any outlier
                     
quantile(cars$carwidth, seq(0,1,0.01))
boxplot(cars$carwidth) # 2% outlier data. 
Car$width

quantile(cars$carheight, seq(0,1,0.01))
boxplot(cars$carheight) #No outliers

quantile(cars$curbweight, seq(0,1,0.01))
boxplot(cars$curbweight) #No outliers

quantile(cars$enginesize, seq(0,1,0.01))
boxplot(cars$enginesize)
cars$enginesize[which(cars$enginesize > 209.00)] <- 209.00 #normalizing outlier values

quantile(cars$boreratio, seq(0,1,0.01))
boxplot(cars$boreratio) #No outliers

quantile(cars$stroke, seq(0,1,0.01))
boxplot(cars$stroke) #Lets see

quantile(cars$compressionratio, seq(0,1,0.01))
boxplot(cars$compressionratio)

quantile(cars$horsepower, seq(0,1,0.01))
boxplot(cars$horsepower)

quantile(cars$peakrpm, seq(0,1,0.01))
boxplot(cars$peakrpm) # hardly any outlier

quantile(cars$citympg, seq(0,1,0.01))
boxplot(cars$citympg) # hardly any outlier

quantile(cars$highwaympg, seq(0,1,0.01))
boxplot(cars$highwaympg) #hardly any outlier

# Deriving Variables

# Parking area (lenght x width x height)

car_dimension <-  cars$carlength * cars$carwidth * cars$carwidth
car_dimension
cars$car_dimension <- round(cars$carlength * cars$carwidth * cars$carwidth, 0)
View(car_dimension)

# Treating categorical variables and creating dummies for columns with more than 2 levels

cars$symboling <- as.factor(cars$symboling)
summary(cars$symboling)
dummy1 <- data.frame(model.matrix(~cars$symboling, data = cars))
View(dummy1)
dummy1 <- dummy1[,-1]

cars_1 <- cbind(cars[,-2], dummy1)
View(cars_1)

# Company (care make)
dummy2 <- data.frame(model.matrix(~cars$company, data = cars_1))
dummy2 <- dummy2[,-1]
cars_2 <- cbind(cars_1[,-2], dummy2)
View(cars_2)

# Fuel type
dummy3 <- data.frame(model.matrix(~cars$carbody, data = cars_2))
dummy3 <- dummy3[,-1]
cars_3 <- cbind(cars_2[,-5], dummy3)
View(cars_3)

# Drive wheel
dummy4 <- data.frame(model.matrix(~cars$drivewheel, data = cars_3))
dummy4 <- dummy4[,-1]
cars_4 <- cbind(cars_3[,-5], dummy4)
View(cars_4)

# Engine type
dummy5 <- data.frame(model.matrix(~cars$enginetype, data = cars_4))
dummy5 <- dummy5[,-1]
cars_5 <- cbind(cars_4[,-11], dummy5)
View(cars_5)

# Cylinder number
dummy6 <- data.frame(model.matrix(~cars$cylindernumber, data = cars_5))
dummy5 <- dummy6[,-1]
cars_6 <- cbind(cars_5[,-11], dummy6)
View(cars_6)

#Fuel System
dummy7 <- data.frame(model.matrix(~cars$fuelsystem, data = cars_6))
dummy7 <- dummy7[,-1]
cars_7 <- cbind(cars_6[,-12], dummy7)
View(cars_7)

# Treating categorical variables for 2 levels

levels(cars_7$fueltype) <- c(0,1) #Converted gas to 1 and diesel to 0
cars_7$fueltype <- as.numeric(levels(cars_7$fueltype))[cars_7$fueltype]

levels(cars_7$aspiration) <- c(0,1) #Converted turbo to 1 and std to 0
cars_7$aspiration <- as.numeric(levels(cars_7$aspiration))[cars_7$aspiration]

levels(cars_7$doornumber) <- c(0,1) #Converted two to 1 and four to 0
cars_7$doornumber <- as.numeric(levels(cars_7$doornumber))[cars_7$doornumber]

levels(cars_7$enginelocation) <- c(0,1) #Converted rear to 1 and front to 0
cars_7$enginelocation <- as.numeric(levels(cars_7$enginelocation))[cars_7$enginelocation]

View(cars_7)
str(cars_7) #entire data has been converted to conitnuous

cars_modelling_data <- cars_7[,-1]
View(cars_modelling_data)

## ## ## ## ## ## Dividing train and test data

set.seed(100)
train_data = sample(1:nrow(cars_modelling_data), 0.7*nrow(cars_modelling_data))

# Generate train dataset

train = cars_modelling_data[train_data,]

# Generate test dataset

test = cars_modelling_data[-train_data,]
View(test)

## ## ## ## ## Start modelling excercise

model_1 <- lm(price~., data = train)
summary(model_1)

# Multiple R-squared:  0.9823,	Adjusted R-squared:  0.9694 

library(car)
library(MASS)

step_model_1 <- stepAIC(model_1, direction = "both")

# Move ahead with model building without the variables removed in StepAIC method

model_2 <- lm(price ~ aspiration + enginelocation + carlength + carwidth + 
                curbweight + enginesize + stroke + car_dimension + cars.companyaudi + 
                cars.companybmw + cars.companybuick + cars.companychevrolet + 
                cars.companydodge + cars.companyisuzu + cars.companyjaguar + 
                cars.companymazda + cars.companymitsubishi + cars.companynissan + 
                cars.companypeugeot + cars.companyplymouth + cars.companyrenault + 
                cars.companysubaru + cars.companytoyota + cars.companyvolkswagen + 
                cars.carbodyhardtop + cars.carbodyhatchback + cars.carbodysedan + 
                cars.carbodywagon + cars.drivewheelrwd + cars.enginetypeohc + 
                cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix + 
                cars.fuelsystem2bbl + cars.fuelsystemmpfi, data = train)
summary(model_2)
# Multiple R-squared:  0.9809,	Adjusted R-squared:  0.9747 
vif(model_2)

# Removing car dimension since the VIF is very high

model_3 <- lm(price ~ aspiration + enginelocation + carlength + carwidth + 
                curbweight + enginesize + stroke + cars.companyaudi + 
                cars.companybmw + cars.companybuick + cars.companychevrolet + 
                cars.companydodge + cars.companyisuzu + cars.companyjaguar + 
                cars.companymazda + cars.companymitsubishi + cars.companynissan + 
                cars.companypeugeot + cars.companyplymouth + cars.companyrenault + 
                cars.companysubaru + cars.companytoyota + cars.companyvolkswagen + 
                cars.carbodyhardtop + cars.carbodyhatchback + cars.carbodysedan + 
                cars.carbodywagon + cars.drivewheelrwd + cars.enginetypeohc + 
                cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix + 
                cars.fuelsystem2bbl + cars.fuelsystemmpfi, data = train)
summary(model_3)
# Multiple R-squared:  0.9744,	Adjusted R-squared:  0.9663

vif(model_3)

# Variables with high vif have low p-value. Hence removing companyisuzu based on p-value




model_4 <- lm(price ~ aspiration + enginelocation + carlength + carwidth + 
                curbweight + enginesize + stroke + cars.companyaudi + 
                cars.companybmw + cars.companybuick + cars.companychevrolet + 
                cars.companydodge + cars.companyjaguar + 
                cars.companymazda + cars.companymitsubishi + cars.companynissan + 
                cars.companypeugeot + cars.companyplymouth + cars.companyrenault + 
                cars.companysubaru + cars.companytoyota + cars.companyvolkswagen + 
                cars.carbodyhardtop + cars.carbodyhatchback + cars.carbodysedan + 
                cars.carbodywagon + cars.drivewheelrwd + cars.enginetypeohc + 
                cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix + 
                cars.fuelsystem2bbl + cars.fuelsystemmpfi, data = train)
summary(model_4)
# Multiple R-squared:  0.9742,	Adjusted R-squared:  0.9664

vif(model_4)

# Based on vif >10 and p value > 0.05 removing carbodyhatchback from the model

model_5 <- lm(price ~ aspiration + enginelocation + carlength + carwidth + 
                curbweight + enginesize + stroke + cars.companyaudi + 
                cars.companybmw + cars.companybuick + cars.companychevrolet + 
                cars.companydodge + cars.companyjaguar + 
                cars.companymazda + cars.companymitsubishi + cars.companynissan + 
                cars.companypeugeot + cars.companyplymouth + cars.companyrenault + 
                cars.companysubaru + cars.companytoyota + cars.companyvolkswagen + 
                cars.carbodyhardtop + cars.carbodysedan + 
                cars.carbodywagon + cars.drivewheelrwd + cars.enginetypeohc + 
                cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix + 
                cars.fuelsystem2bbl + cars.fuelsystemmpfi, data = train)
summary(model_5)
# Multiple R-squared:  0.9735,	Adjusted R-squared:  0.9658

vif(model_5)

# Based on vif >10 and p value > 0.05 removing carlength from the model

model_6 <- lm(price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + stroke + cars.companyaudi + 
                cars.companybmw + cars.companybuick + cars.companychevrolet + 
                cars.companydodge + cars.companyjaguar + 
                cars.companymazda + cars.companymitsubishi + cars.companynissan + 
                cars.companypeugeot + cars.companyplymouth + cars.companyrenault + 
                cars.companysubaru + cars.companytoyota + cars.companyvolkswagen + 
                cars.carbodyhardtop + cars.carbodysedan + 
                cars.carbodywagon + cars.drivewheelrwd + cars.enginetypeohc + 
                cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix + 
                cars.fuelsystem2bbl + cars.fuelsystemmpfi, data = train)
summary(model_6)
# Multiple R-squared:  0.9736,	Adjusted R-squared:  0.9660

vif(model_6)

# Based on p value > 0.05 removing carbodyhardtop as it has the highest p-value. Still not removing curbwieght
# and enginesize in spite of high vif because of its significance i.e, low p-value

model_7 <- lm(price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + stroke + cars.companyaudi + 
                cars.companybmw + cars.companybuick + cars.companychevrolet + 
                cars.companydodge + cars.companyjaguar + 
                cars.companymazda + cars.companymitsubishi + cars.companynissan + 
                cars.companypeugeot + cars.companyplymouth + cars.companyrenault + 
                cars.companysubaru + cars.companytoyota + cars.companyvolkswagen + 
                cars.carbodysedan + 
                cars.carbodywagon + cars.drivewheelrwd + cars.enginetypeohc + 
                cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix + 
                cars.fuelsystem2bbl + cars.fuelsystemmpfi, data = train)
summary(model_7)
# Multiple R-squared:  0.9734,	Adjusted R-squared:  0.9663

vif(model_7)

# Based on p value > 0.05 removing carbodysedan as it has the highest p-value. Still not removing curbwieght
# and enginesize in spite of high vif because of its significance i.e, low p-value

model_8 <- lm(price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + stroke + cars.companyaudi + 
                cars.companybmw + cars.companybuick + cars.companychevrolet + 
                cars.companydodge + cars.companyjaguar + 
                cars.companymazda + cars.companymitsubishi + cars.companynissan + 
                cars.companypeugeot + cars.companyplymouth + cars.companyrenault + 
                cars.companysubaru + cars.companytoyota + cars.companyvolkswagen + 
                cars.carbodywagon + cars.drivewheelrwd + cars.enginetypeohc + 
                cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix + 
                cars.fuelsystem2bbl + cars.fuelsystemmpfi, data = train)
summary(model_8)
# Multiple R-squared:  0.9733,	Adjusted R-squared:  0.9664

vif(model_8)

# Based on p value > 0.05 removing carbodywagon as it has the highest p-value. Still not removing curbwieght
# and enginesize in spite of high vif because of its significance i.e, low p-value

model_9 <- lm(price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + stroke + cars.companyaudi + 
                cars.companybmw + cars.companybuick + cars.companychevrolet + 
                cars.companydodge + cars.companyjaguar + 
                cars.companymazda + cars.companymitsubishi + cars.companynissan + 
                cars.companypeugeot + cars.companyplymouth + cars.companyrenault + 
                cars.companysubaru + cars.companytoyota + cars.companyvolkswagen + 
                cars.drivewheelrwd + cars.enginetypeohc + 
                cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix + 
                cars.fuelsystem2bbl + cars.fuelsystemmpfi, data = train)
summary(model_9)
# Multiple R-squared:  0.9731,	Adjusted R-squared:  0.9665

vif(model_9)

# Removing curb weight and noticing its impact as the significance of curbweight has fallen

model_10 <- lm(price ~ aspiration + enginelocation + carwidth + 
                enginesize + stroke + cars.companyaudi + 
                cars.companybmw + cars.companybuick + cars.companychevrolet + 
                cars.companydodge + cars.companyjaguar + 
                cars.companymazda + cars.companymitsubishi + cars.companynissan + 
                cars.companypeugeot + cars.companyplymouth + cars.companyrenault + 
                cars.companysubaru + cars.companytoyota + cars.companyvolkswagen + 
                cars.drivewheelrwd + cars.enginetypeohc + 
                cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix + 
                cars.fuelsystem2bbl + cars.fuelsystemmpfi, data = train)
summary(model_10)
# Multiple R-squared:  0.9705,	Adjusted R-squared:  0.9636

vif(model_10)

# Removing curb weight has increased the significance of other variables. 
# Apparently it was collienear with other variable
# Removing companyaudi from the model based on highest p-value i.e., no star

model_11 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + 
                 cars.companybmw + cars.companybuick + cars.companychevrolet + 
                 cars.companydodge + cars.companyjaguar + 
                 cars.companymazda + cars.companymitsubishi + cars.companynissan + 
                 cars.companypeugeot + cars.companyplymouth + cars.companyrenault + 
                 cars.companysubaru + cars.companytoyota + cars.companyvolkswagen + 
                 cars.drivewheelrwd + cars.enginetypeohc + 
                 cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix + 
                 cars.fuelsystem2bbl + cars.fuelsystemmpfi, data = train)
summary(model_11)
# Multiple R-squared:  0.9697,	Adjusted R-squared:  0.9629

vif(model_11)

# Removing fuelsystembbl from the model based on highest p-value i.e., one star

model_12 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + 
                 cars.companybmw + cars.companybuick + cars.companychevrolet + 
                 cars.companydodge + cars.companyjaguar + 
                 cars.companymazda + cars.companymitsubishi + cars.companynissan + 
                 cars.companypeugeot + cars.companyplymouth + cars.companyrenault + 
                 cars.companysubaru + cars.companytoyota + cars.companyvolkswagen + 
                 cars.drivewheelrwd + cars.enginetypeohc + 
                 cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix + 
                 cars.fuelsystemmpfi, data = train)
summary(model_12)
# Multiple R-squared:  0.9684,	Adjusted R-squared:  0.9616

vif(model_12)

# Removing fuelsystempfi from the model based on highest p-value i.e., one star

model_13 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + 
                 cars.companybmw + cars.companybuick + cars.companychevrolet + 
                 cars.companydodge + cars.companyjaguar + 
                 cars.companymazda + cars.companymitsubishi + cars.companynissan + 
                 cars.companypeugeot + cars.companyplymouth + cars.companyrenault + 
                 cars.companysubaru + cars.companytoyota + cars.companyvolkswagen + 
                 cars.drivewheelrwd + cars.enginetypeohc + 
                 cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix 
                 , data = train)
summary(model_13)
# Multiple R-squared:  0.9671,	Adjusted R-squared:  0.9604

vif(model_13)

# Removing companyvolkswagen from the model based on highest p-value i.e., one star

model_14 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + 
                 cars.companybmw + cars.companybuick + cars.companychevrolet + 
                 cars.companydodge + cars.companyjaguar + 
                 cars.companymazda + cars.companymitsubishi + cars.companynissan + 
                 cars.companypeugeot + cars.companyplymouth + cars.companyrenault + 
                 cars.companysubaru + cars.companytoyota + 
                 cars.drivewheelrwd + cars.enginetypeohc + 
                 cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix 
               , data = train)
summary(model_14)
# Multiple R-squared:  0.9655,	Adjusted R-squared:  0.9589

vif(model_14)

# Removing companyreanault from the model based on highest p-value i.e., one star

model_15 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + 
                 cars.companybmw + cars.companybuick + cars.companychevrolet + 
                 cars.companydodge + cars.companyjaguar + 
                 cars.companymazda + cars.companymitsubishi + cars.companynissan + 
                 cars.companypeugeot + cars.companyplymouth + 
                 cars.companysubaru + cars.companytoyota + 
                 cars.drivewheelrwd + cars.enginetypeohc + 
                 cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix 
               , data = train)
summary(model_15)
# Multiple R-squared:  0.9643,	Adjusted R-squared:  0.9578

vif(model_15)

# Significance of drivewheelrwd has reduced sharply. Removing companydodge, nissan and plymouth to 
# observe the significance of drivewheel. It maybe insignificant now but could be a better action item for 
# design team and better from modelling story telling point.

model_16 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + 
                 cars.companybmw + cars.companybuick + cars.companychevrolet + 
                 cars.companyjaguar + 
                 cars.companymazda + cars.companymitsubishi + 
                 cars.companypeugeot + 
                 cars.companysubaru + cars.companytoyota + 
                 cars.drivewheelrwd + cars.enginetypeohc + 
                 cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix 
               , data = train)
summary(model_16)
# Multiple R-squared:  0.9606,	Adjusted R-squared:  0.9545

vif(model_16)

# in model_16 drivewheelrwd fell further. Hence removing that and adding back dodge, nissan and plymouth

model_17 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + 
                 cars.companybmw + cars.companybuick + cars.companychevrolet + 
                 cars.companydodge + cars.companyjaguar + 
                 cars.companymazda + cars.companymitsubishi + cars.companynissan + 
                 cars.companypeugeot + cars.companyplymouth + 
                 cars.companysubaru + cars.companytoyota + 
                 cars.enginetypeohc + 
                 cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix 
               , data = train)
summary(model_17)
# Multiple R-squared:  0.9633,	Adjusted R-squared:  0.9569

vif(model_17)

# Removing companydodge from the model

model_18 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + 
                 cars.companybmw + cars.companybuick + cars.companychevrolet + 
                 cars.companyjaguar + 
                 cars.companymazda + cars.companymitsubishi + cars.companynissan + 
                 cars.companypeugeot + cars.companyplymouth + 
                 cars.companysubaru + cars.companytoyota + 
                 cars.enginetypeohc + 
                 cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix 
               , data = train)
summary(model_18)
# Multiple R-squared:  0.9619,	Adjusted R-squared:  0.9556

vif(model_18)

# Removing companynissan from the model based on p-value > 0.05

model_19 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + 
                 cars.companybmw + cars.companybuick + cars.companychevrolet + 
                 cars.companyjaguar + 
                 cars.companymazda + cars.companymitsubishi + 
                 cars.companypeugeot + cars.companyplymouth + 
                 cars.companysubaru + cars.companytoyota + 
                 cars.enginetypeohc + 
                 cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix 
               , data = train)
summary(model_19)
# Multiple R-squared:  0.9607,	Adjusted R-squared:  0.9547

vif(model_19)

# Removing companyplymouth from the model based on p-value > 0.05

model_20 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + 
                 cars.companybmw + cars.companybuick + cars.companychevrolet + 
                 cars.companyjaguar + 
                 cars.companymazda + cars.companymitsubishi + 
                 cars.companypeugeot + 
                 cars.companysubaru + cars.companytoyota + 
                 cars.enginetypeohc + 
                 cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix 
               , data = train)
summary(model_20)
# Multiple R-squared:  0.9599,	Adjusted R-squared:  0.9541

vif(model_20)

# Removing companychevrolet from the model based on p-value > 0.05

model_21 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + 
                 cars.companybmw + cars.companybuick + 
                 cars.companyjaguar + 
                 cars.companymazda + cars.companymitsubishi + 
                 cars.companypeugeot + 
                 cars.companysubaru + cars.companytoyota + 
                 cars.enginetypeohc + 
                 cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix 
               , data = train)
summary(model_21)
# Multiple R-squared:  0.9559,	Adjusted R-squared:  0.9534

vif(model_21)

# Removing companytoyota from the model based on p-value > 0.05

model_22 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + 
                 cars.companybmw + cars.companybuick + 
                 cars.companyjaguar + 
                 cars.companymazda + cars.companymitsubishi + 
                 cars.companypeugeot + 
                 cars.companysubaru + 
                 cars.enginetypeohc + 
                 cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix 
               , data = train)
summary(model_22)
# Multiple R-squared:  0.9564,	Adjusted R-squared:  0.9508

vif(model_22)

# Removing companymitsubishi from the model based on higher p-value i.e., one star

model_23 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + 
                 cars.companybmw + cars.companybuick + 
                 cars.companyjaguar + 
                 cars.companymazda + 
                 cars.companypeugeot + 
                 cars.companysubaru + 
                 cars.enginetypeohc + 
                 cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix 
               , data = train)
summary(model_23)
# Multiple R-squared:  0.9545,	Adjusted R-squared:  0.9492

vif(model_23)

# Removing companymazda from the model based on higher p-value i.e., one star

model_24 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + 
                 cars.companybmw + cars.companybuick + 
                 cars.companyjaguar + 
                 cars.companypeugeot + 
                 cars.companysubaru + 
                 cars.enginetypeohc + 
                 cars.cylindernumberfive + cars.cylindernumberfour + cars.cylindernumbersix 
               , data = train)
summary(model_24)
# Multiple R-squared:  0.9524,	Adjusted R-squared:  0.9472

vif(model_24)

# Removing cylindernumber5 from the model based on higher p-value i.e., two star

model_25 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + 
                 cars.companybmw + cars.companybuick + 
                 cars.companyjaguar + 
                 cars.companypeugeot + 
                 cars.companysubaru + 
                 cars.enginetypeohc + 
                 cars.cylindernumberfour + cars.cylindernumbersix 
               , data = train)
summary(model_25)
# Multiple R-squared:  0.9498,	Adjusted R-squared:  0.9448

vif(model_25)

# Removing cylindernumber4 from the model based on higher p-value i.e., one star

model_26 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + 
                 cars.companybmw + cars.companybuick + 
                 cars.companyjaguar + 
                 cars.companypeugeot + 
                 cars.companysubaru + 
                 cars.enginetypeohc + 
                 cars.cylindernumbersix 
               , data = train)
summary(model_26)
# Multiple R-squared:  0.9482,	Adjusted R-squared:  0.9434

vif(model_26)

# Removing aspiration from the model based on higher p-value i.e., two star

model_27 <- lm(price ~ enginelocation + carwidth + 
                 enginesize + stroke + 
                 cars.companybmw + cars.companybuick + 
                 cars.companyjaguar + 
                 cars.companypeugeot + 
                 cars.companysubaru + 
                 cars.enginetypeohc + 
                 cars.cylindernumbersix 
               , data = train)
summary(model_27)
# Multiple R-squared:  0.9442,	Adjusted R-squared:  0.9395

vif(model_27)

# All the variables have significant p-value i.ei, < 0.05 or three stars. 
# However there are certain variables which have vif > 2. Removing them to see the impact on the model
# Removing cylindernumbersix

model_28 <- lm(price ~ enginelocation + carwidth + 
                 enginesize + stroke + 
                 cars.companybmw + cars.companybuick + 
                 cars.companyjaguar + 
                 cars.companypeugeot + 
                 cars.companysubaru + 
                 cars.enginetypeohc, data = train)
summary(model_28)

# Multiple R-squared:  0.9380,	Adjusted R-squared:  0.9333

# We may conclude that model_28 is our final linear regression model. All the variables are significant in this model.
# model_28 has R-squarred and Adjusted R-squarred both extremely close to earch other.

# Check for model accuracy
# Predict price on test data

test$predicted_price <- predict(model_28, test[,-18])
# Storing predicted price in a new variable

# Check for correlation between predicted price and actual price
check_correlation <- cor(test$predicted_price, test$price)
check_correlation

# corrrelation value at 0.9133

calc_rsquarred <- round((check_correlation)^2, 3)
calc_rsquarred

# R-Squarred value at 0.834. It suggests we have a good model.

# Calculating Predicted price error

test$error <- test$price - test$predicted_price

library(ggplot2)
ggplot(test, aes(price, error)) + geom_point() + geom_hline(yintercept = 0)

# Plot shows that our error is randomly distributed. This implies that there are no additional variables which can 
# make the model better.

## ## ## END ## ## ## END ## ## ## END ## ## ## END ## ## ## END ## ## ## END ## ## ##


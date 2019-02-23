############################ SVM Letter Recogniser #################################
# 1.  Business Understanding
# 2.  Data Understanding
# 3.  Data Preparation
# 4.  Model Building 
# 4.1 Linear kernel
# 4.2 RBF Kernel
# 5   Hyperparameter tuning and cross validation

#####################################################################################

# 1. Business Understanding: 

#The objective is to develop the model using support vector machine which would correctly identify the 
#handwritten digits based on pixel values as features

#####################################################################################

# 2. Data Understanding: 
# The MNIST database (Modified National Institute of Standards and Technology database) is a lrage database of
# handwritten digits that are commonly used for training various image processing systems. 
# More on this can be found at: https://en.wikipedia.org/wiki/MNIST_database and http://yann.lecun.com/exdb/mnist/

# Number of Instances in Train data: 60000
# Number of Instances in Test data: 20000
# Number of Attributes: 784
# Label values are 0 to 9.
# Pixel values are organised row-wise. Pixel value 0 means background white and 255 means foreground black

#3. Data Preparation: 

#Loading Neccessary libraries
library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(caTools)

#Loading data 
mnist_train <- read.csv("mnist_train.csv", header = FALSE)
mnist_test <- read.csv("mnist_test.csv", header = FALSE)

#Running some preliminary check on data
dim(mnist_train)
dim(mnist_test)

str(mnist_train)
str(mnist_test)
#all integers

summary(mnist_train)

#Checking for missing values
sapply(mnist_train, function(x) sum(is.na(x)))
sapply(mnist_test, function(x) sum(is.na(x)))
#No missing values

head(mnist_train)

View(mnist_test)

#Changing the column name of the first column of Train and Test data both
colnames(mnist_train) [1] <- "value"
colnames(mnist_test) [1] <- "value"

View(mnist_test)

#Converting Value column to factor
mnist_train$value <- factor(mnist_train$value)
mnist_test$value <- factor(mnist_test$value)

str(mnist_train$value)
str(mnist_test$value)

# As the data is is a bit big and would need considerable computation power taking only 7% of the train data
# for final evaluation

set.seed(100)
train.indices = sample(1:nrow(mnist_train), 0.07*nrow(mnist_train))
train = mnist_train[train.indices, ]

ggplot(mnist_train, aes(x=value)) + geom_bar(stat = "count", position = "dodge")

#Data seems to be pretty evenly distributed

# Model building
# 1. Linear Kernel

linear_model <- ksvm(value~., data = train, scale = FALSE, kernel = "vanilladot")
#Error about 'cannot scale data' can be ignored.

linear_eval <- predict(linear_model, mnist_test)

confusionMatrix(linear_eval, mnist_test$value)

#Confusion Matrix and Statistics

#Reference
#Prediction    0    1    2    3    4    5    6    7    8    9
#0           959    0   16   10    2   13   19    1    7    7
#1             0 1116   14    2    1    5    3   17    7    3
#2             2    6  928   17   10    7   17   15   18    2
#3             1    2   16  889    0   41    0   14   44   12
#4             1    0    9    0  929    4   10   12   12   56
#5             7    3    2   46    2  772   20    1   46    6
#6             8    2    6    1    2    7  889    0   11    1
#7             1    0    9   11    4    4    0  941   10   27
#8             1    5   28   29    2   27    0    2  809   13
#9             0    1    4    5   30   12    0   25   10  882

#Overall Statistics

#Accuracy : 0.9114          
#95% CI : (0.9057, 0.9169)
#No Information Rate : 0.1135          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.9015          
#Mcnemar's Test P-Value : NA              

#Statistics by Class:

#                     Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity            0.9786   0.9833   0.8992   0.8802   0.9460   0.8655   0.9280   0.9154   0.8306   0.8741
#Specificity            0.9917   0.9941   0.9895   0.9855   0.9885   0.9854   0.9958   0.9926   0.9881   0.9903
#Pos Pred Value         0.9275   0.9555   0.9080   0.8724   0.8993   0.8530   0.9590   0.9345   0.8832   0.9102
#Neg Pred Value         0.9977   0.9978   0.9884   0.9865   0.9941   0.9868   0.9924   0.9903   0.9818   0.9859
#Prevalence             0.0980   0.1135   0.1032   0.1010   0.0982   0.0892   0.0958   0.1028   0.0974   0.1009
#Detection Rate         0.0959   0.1116   0.0928   0.0889   0.0929   0.0772   0.0889   0.0941   0.0809   0.0882
#Detection Prevalence   0.1034   0.1168   0.1022   0.1019   0.1033   0.0905   0.0927   0.1007   0.0916   0.0969
#Balanced Accuracy      0.9851   0.9887   0.9444   0.9329   0.9672   0.9254   0.9619   0.9540   0.9094   0.9322


#Non-linear, RBF Kernel for model building

rbf_model <- ksvm(value~., data = train, scale = FALSE, kernel = "rbfdot")

#Same scaling errors as that of Vanilladot. Please ignore.

rbf_eval <- predict(rbf_model, mnist_test)

#Confusion Matrix - RBF Kernel
confusionMatrix(rbf_eval, mnist_test$value)

#Confusion Matrix and Statistics

#Reference
#Prediction    0    1    2    3    4    5    6    7    8    9
#0           966    0   12    1    1    9    9    0    6    8
#1             0 1115    1    0    1    2    3   19    2    6
#2             2    3  955   15    4    6    2   21    5    0
#3             0    3   11  954    0   20    1    4   16   12
#4             0    1    7    0  932    3    6    8    9   30
#5             4    2    2   16    1  832   14    0   18    6
#6             4    3    7    1    6    9  922    0    9    0
#7             1    0   10    9    3    2    0  949    8   13
#8             1    8   25   11    2    6    1    2  895    7
#9             2    0    2    3   32    3    0   25    6  927

#Overall Statistics

#Accuracy : 0.9447        
#95% CI : (0.94, 0.9491)
#No Information Rate : 0.1135        
#P-Value [Acc > NIR] : < 2.2e-16     

#Kappa : 0.9385        
#Mcnemar's Test P-Value : NA            

#Statistics by Class:

#Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity            0.9857   0.9824   0.9254   0.9446   0.9491   0.9327   0.9624   0.9232   0.9189   0.9187
#Specificity            0.9949   0.9962   0.9935   0.9925   0.9929   0.9931   0.9957   0.9949   0.9930   0.9919
#Pos Pred Value         0.9545   0.9704   0.9427   0.9344   0.9357   0.9296   0.9594   0.9538   0.9342   0.9270
#Neg Pred Value         0.9984   0.9977   0.9914   0.9938   0.9944   0.9934   0.9960   0.9912   0.9913   0.9909
#Prevalence             0.0980   0.1135   0.1032   0.1010   0.0982   0.0892   0.0958   0.1028   0.0974   0.1009
#Detection Rate         0.0966   0.1115   0.0955   0.0954   0.0932   0.0832   0.0922   0.0949   0.0895   0.0927
#Detection Prevalence   0.1012   0.1149   0.1013   0.1021   0.0996   0.0895   0.0961   0.0995   0.0958   0.1000
#Balanced Accuracy      0.9903   0.9893   0.9595   0.9686   0.9710   0.9629   0.9791   0.9590   0.9560   0.9553


#RBF Model is performing better than the simple Vanilla model.

#Performing systematic cross validation to determine the best value of C for RBF model

############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number =  Number of folds in CV.

trainControl <- trainControl(method="cv", number=3)

#Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

install.packages("doParallel")
library(doParallel)

cl <- makePSOCKcluster(3)
registerDoParallel(cl)

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
set.seed(70)
grid <- expand.grid(.sigma=c(0.63e-7,1.63e-7, 2.63e-7), .C=c(1, 2, 3))


#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

fit.svm <- train(value~., data=train, metric="Accuracy", method="svmRadial", tuneGrid=grid,
                 trControl = trainControl, allowParellel = TRUE, preProcess = NULL)

print(fit.svm)
plot(fit.svm)

#Support Vector Machines with Radial Basis Function Kernel 

#4200 samples
#784 predictor
#10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 

#No pre-processing
#Resampling: Cross-Validated (3 fold) 
#Summary of sample sizes: 2800, 2800, 2800 
#Resampling results across tuning parameters:
  
#  sigma     C  Accuracy   Kappa    
#6.30e-08  1  0.9123810  0.9026131
#6.30e-08  2  0.9226190  0.9139965
#6.30e-08  3  0.9283333  0.9203499
#1.63e-07  1  0.9345238  0.9272300
#1.63e-07  2  0.9447619  0.9386099
#1.63e-07  3  0.9466667  0.9407277
#2.63e-07  1  0.9442857  0.9380802
#2.63e-07  2  0.9514286  0.9460197
#2.63e-07  3  0.9500000  0.9444322

#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were sigma = 2.63e-07 and C = 2.

#Evaluating the model on test
eval_linear_test <- predict(fit.svm, mnist_test)

#Confusion Matrix
confusionMatrix(eval_linear_test, mnist_test$value)
#Confusion Matrix and Statistics

#Reference
#Prediction    0    1    2    3    4    5    6    7    8    9
#0           965    0   12    0    1    5    8    0    5    6
#1             0 1117    0    0    0    1    3   12    0    3
#2             2    3  985    8    6    6    2   17    6    1
#3             1    3    5  968    0   16    0    4   17    8
#4             0    1    4    0  942    1    7    5   10   23
#5             4    1    1   12    1  847   10    0   14    4
#6             3    4    1    1    4    8  927    0    5    0
#7             1    1    9    6    2    1    0  972    6   10
#8             1    5   14   11    2    5    1    1  907    8
#9             3    0    1    4   24    2    0   17    4  946

#Overall Statistics

#Accuracy : 0.9576          
#95% CI : (0.9535, 0.9615)
#No Information Rate : 0.1135          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.9529          
#Mcnemar's Test P-Value : NA              

#Statistics by Class:

#                     Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity            0.9847   0.9841   0.9545   0.9584   0.9593   0.9496   0.9676   0.9455   0.9312   0.9376
#Specificity            0.9959   0.9979   0.9943   0.9940   0.9943   0.9948   0.9971   0.9960   0.9947   0.9939
#Pos Pred Value         0.9631   0.9833   0.9508   0.9472   0.9486   0.9474   0.9727   0.9643   0.9497   0.9451
#Neg Pred Value         0.9983   0.9980   0.9948   0.9953   0.9956   0.9951   0.9966   0.9938   0.9926   0.9930
#Prevalence             0.0980   0.1135   0.1032   0.1010   0.0982   0.0892   0.0958   0.1028   0.0974   0.1009
#Detection Rate         0.0965   0.1117   0.0985   0.0968   0.0942   0.0847   0.0927   0.0972   0.0907   0.0946
#Detection Prevalence   0.1002   0.1136   0.1036   0.1022   0.0993   0.0894   0.0953   0.1008   0.0955   0.1001
#Balanced Accuracy      0.9903   0.9910   0.9744   0.9762   0.9768   0.9722   0.9824   0.9708   0.9629   0.9657

# Since, the final values used for the model were sigma = 2.63e-07 and C = 2.
# We will use these values to build our final model

final_model <- ksvm(value~., data=train, scale=FALSE, kernel="rbfdot", C=2, kpar=list(sigma=2.63e-07))
final_model

#Support Vector Machine object of class "ksvm" 

#SV type: C-svc  (classification) 
#parameter : cost C = 2 

#Gaussian Radial Basis kernel function. 
#Hyperparameter : sigma =  2.63e-07 

#Number of Support Vectors : 2230 

#Testing model accuracy on the test data

eval_final_model_test <- predict(final_model, mnist_test)
confusionMatrix(eval_final_model_test, mnist_test$value)

#Final model accuracy on test data = 0.9576

#Accuracy of the final model which is the RBF model is better than the Vanilla Linear model. 
#The RBF model is apparently more complex then the linear model but has high accuracy. Considering the problem
#at hand we would need a model which is more accurate than simpler. 
#Hence the Final RBF model is to be used for problem solving in the given case.

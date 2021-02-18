
# linear regression problem

library(readxl)
salarydata <- read_excel("Salary_Data.xlsx")

#load data 
salarydata

#no any missing value
summary(salarydata)

#pre processing 
# no any outlier
boxplot(salarydata$YearsExperience)
bx = boxplot(salarydata$Salary)

# show statistics
bx$stats

#   model - methematical equation 

#  training dataset and test dataset

#break the data set into traing and test


# caTools helps to break the dataset
install.packages("caTools")
library(caTools)

set.seed(10)

# sample.split works for only target variable and splitratio convert part you want to take for training

split = sample.split(salarydata$Salary, SplitRatio = 0.75)

train = subset(salarydata, split == T)
testdata = subset(salarydata, split == F)

train
testdata

# creating the model

model <- lm(formula = Salary~. , data = train)
summary(model)

# prediction

prediction <- predict(model, newdata = testdata)
prediction

difference <- testdata$Salary - prediction

testdata <- cbind(testdata, prediction, difference)

square <- (difference)^2
testdata <- cbind(testdata, square)
plot(testdata$Salary,type = "o",col = "red", xlab = "Actual", ylab = "Predict", 
     main = "Difference between actual and predict")

lines(testdata$prediction, type = "o", col = "blue")
View(testdata)





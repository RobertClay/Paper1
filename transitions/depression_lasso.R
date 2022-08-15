require(glmnet)# load in lasso function.
source("transitions/utils.R") # load in utility functions.

#Load in data
#Load in data
source <-"/Users/robertclay/PycharmProjects/Icarus/data/corrected_US/"
years <- c(2012, 2013, 2014, 2015)
data <- format_depression_data(source, years)
data<- data[order(data$pidp, data$time),]
n<- nrow(data)

set.seed(8)
#test_size<- 8000
#Inx <- sample(n, ((n-test_size)/n))
Inx <- sample(n, 0.90*n)
train<-data[Inx,]
test<-data[-Inx,]
#test_size<- 8000
#train<- head(data, n-test_size)
#test<- tail(data, test_size)
y <- train$depression_state
y2 <- test$depression_state

x <- model.matrix(factor(depression_state) ~ factor(sex) +
                    factor(ethnicity) + 
                    age + 
                    factor(education_state) +
                    factor(labour_state) +
                    factor(job_sec)+
                    factor(education_state) * factor(ethnicity),
                  data)
x2<- x[-Inx,]
x<- x[Inx,]

# Fit LASSO
lasso <- glmnet(x, y, alpha = 1, family = "binomial", type.measure = "class")
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial", type.measure = "class")


# Plot LASSO
plot(lasso, xvar = "lambda", label = T) 
abline(v = log(cv.lasso$lambda.min), col = "blue",lty=2)
abline(v = log(cv.lasso$lambda.1se), col = "red")
#text(-7.34, 1.6, "Minimum error", col="blue")
#text(-4.75, 1.6, "1 standard error", col="red")

plot(cv.lasso)
coef(cv.lasso, s = "lambda.min")

best<- glmnet(x2, y2, alpha=1, lambda = cv.lasso$lambda.min)
# note the type = "class" is preferable here. 
# the choice of a cutoff at p=0.5 is very arbitrary and this function helps
# to optimise the position of that line.
# For the the bubble plot is best for visual interpretation.
preds<- invlogit(predict(cv.lasso, s = "lambda.min", x2))# type = "class"
preds2<- invlogit(predict(cv.lasso, s = "lambda.1se", x2))# type = "class"

plot_depression_residuals(preds, y2)
plot_depression_residuals(preds2, y2)

require(geepack)
#require(MASS)
#require(nlme)
source("transitions/utils.R")

#Load in data
source <-"/Users/robertclay/paper1/data/corrected_US/"
years <- c(2012, 2013, 2014, 2015)

data <- format_depression_data(source, years)

#n<- nrow(data)
#Inx <- sample(n, 0.9 * n)
#train<-data[Inx,]
#test<-data[-Inx,]
n<- nrow(data)
test_size<- 3000 # test set must be large enough to include all factor levels. (annoyingly.)
# need to fix.

data<- data[order(data$pidp, data$time),]
train<- head(data, n-test_size)
test<- tail(data, test_size)
y <- train$depression_state
y2 <- test$depression_state

dep.gee <- geeglm(depression ~
                factor(sex) + 
                factor(ethnicity) + 
                age + 
                factor(education_state) +
                factor(labour_state) + 1:pidp, data = train, family = binomial,id=pidp
              , corstr="unstructured")

#asd<- glmmPQL(depression_state ~
#                factor(sex) + 
#                factor(ethnicity) + 
#                age + 
#                factor(education_state) +
#                factor(labour_state), random = ~1|pidp, data = train, family = binomial,
#              , correlation=corAR1(form=~time))

print(summary(dep.gee))
print(QIC(dep.gee))
preds<- invlogit(predict(dep.gee, test))
plot_depression_residuals(preds, y2)
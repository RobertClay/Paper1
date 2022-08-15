
require(ordinal)
source("utils.R")# load in utility functions
source("notebook_utils.R")# load in utility functions
#Load in data
source <-"/Users/robertclay/PycharmProjects/Icarus/data/complete_case_US/"
years <- c(2012, 2013, 2014, 2015)
data <- format_baseline_data(source, years)
data<- data[order(data$pidp, data$time),]
n<- nrow(data)

#test_size<- 5349 # test set must be large enough to include all factor levels. (annoyingly.)
# need to fix.
#train<- head(data, n-test_size)
#test<- tail(data, test_size)

set.seed(8)
Inx <- sample(n, 0.8*n)
train<-data[Inx,]
test<-data[-Inx,]

y <- train$depression
y2<- test$depression

clm.formula <- "factor(depression_change) ~ factor(sex) + factor(ethnicity) + age  + factor(education_state) + factor(labour_state) + factor(job_sec)"
dep.clm <- clm(clm.formula, data= data,
             link = "logit")
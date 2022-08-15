source("transitions/utils.R")# load in utility functions
#Load in data
source <-"/Users/robertclay/PycharmProjects/Icarus/data/complete_case_US/"
years <- c(2012, 2013, 2014, 2015)
data <- format_depression_data(source, years)
data<- data[order(data$pidp, data$time),]
n<- nrow(data)

#test_size<- 5349 # test set must be large enough to include all factor levels. (annoyingly.)
# need to fix.
#train<- head(data, n-test_size)
#test<- tail(data, test_size)

set.seed(8)
Inx <- sample(n, 0.80*n)
train<-data[Inx,]
test<-data[-Inx,]

y <- train$depression_change
y2<- test$depression_change

#glm <- glm(factor(depression_state) ~ factor(sex) + factor(ethnicity) + age + factor(education_state) + factor(labour_state),
#              data,
#              family = binomial(link="probit"))
logit <- glm(factor(depression_change) ~ factor(sex) + factor(ethnicity) + age + I(age**2) + factor(education_state) + factor(labour_state)
             + factor(job_sec), data,
             family = binomial(link="logit"))
prs <- 1 - logit$deviance / logit$null.deviance
print(prs)
preds <- invlogit(predict(logit, test))

print(summary(logit))
#pdf("plots/depression_logit.pdf")
plot_depression_residuals(preds, y2)
#dev.off()
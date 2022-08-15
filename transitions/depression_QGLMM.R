require(MASS) # for qglmm
require(nlme)
source("transitions/utils.R")

#Load in data
source <-"/Users/robertclay/PycharmProjects/Icarus/data/complete_case_US/"
years <- c(2012, 2013, 2014, 2015)
data <- format_depression_data(source, years)
data<- data[order(data$pidp, data$time),]
n<- nrow(data)

set.seed(8)
#test_size<- 8000
#Inx <- sample(n, ((n-test_size)/n))
Inx <- sample(n, 0.80*n)
train<-data[Inx,]
test<-data[-Inx,]
#test_size<- 8000
#train<- head(data, n-test_size)
#test<- tail(data, test_size)
y <- train$depression
y2 <- test$depression

dep.qglmm<- glmmPQL(factor(depression) ~
                factor(sex) + 
                factor(ethnicity) + 
                age + 
                factor(education_state) +
                factor(job_sec) + 
                factor(labour_state), 
                random = ~1|pidp, 
                data = train, 
                family = binomial(link="logit"),
                #correlation=corAR1(form = ~time|pidp)
                #correlation = corCompSymm()
                )

print(summary(dep.qglmm))
#print(QIC(dep.qglmm))
preds<- invlogit(predict(dep.qglmm, test))
#pdf("plots/depression_AR1_logistic_normal.pdf")
plot_depression_residuals(preds, y2)
#dev.off()
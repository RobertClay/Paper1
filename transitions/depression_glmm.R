require(lme4)
source("transitions/utils.R")

#Load in data
source <-"/Users/robertclay/PycharmProjects/Icarus/data/complete_case_US/"
years <- c(2012, 2013, 2014, 2015)
data <- format_depression_data(source, years)
data<- data[order(data$pidp, data$time),]
n<- nrow(data)

set.seed(8)
Inx <- sample(n, 0.9 * n)
train<-data[Inx,]
test<-data[-Inx,]
n<- nrow(data)
test_size<- 5000 # test set must be large enough to include all factor levels. (annoyingly.)
# need to fix.
#train<- head(data, n-test_size)
#test<- tail(data, test_size)

y <- train$depression_change
y2 <- test$depression_change

dep.glmm <- glmer(factor(depression) ~
                factor(sex) + 
                factor(ethnicity) + 
                age + I(age**2) +
                factor(education_state) + 
                factor(job_sec) +
                factor(labour_state) +
                (1|pidp), data = train, family = binomial(link="logit"), verbose = 1, nAGQ =0)


# This is dumb. has the same null deviance as the logit model but cant extract it from dep.glmm.
glmm.sum <- summary(dep.glmm)
prs <- 1 - glmm.sum$AICtab[4]/logit$null.deviance
print(prs)

# nagq 0 for fast but sloppy convergence. 1 for accurate and slow (laplacian). see ?glmer
preds <- invlogit(predict(dep.glmm, test, allow.new.levels=T))
print(summary(dep.glmm))

file_name<- "transitions/depression_glmm.rda"
save(dep.glmm, file=file_name)

#pdf("plots/depression_logistic_normal.pdf")
plot_depression_residuals(preds, y2)
#dev.off()

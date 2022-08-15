source("transitions/utils.R")
#source("transitions/notebook_utils.R")# load in utility functions
require(lme4)
require(tidyr)
# for presentation.
require(ggfortify)
require(Hmisc)
# for missingness.
require(tidyr)
require(VIM)
require(mice)
require(lattice) # for histogram.
require(laeken) # for weighedMean.
# for extrapolation.
require(tidyr)
require(glmnet)
# for heterogeneity.
require(tidyr)
require(glmnet)
require(geepack)
require(lme4)
require(MuMIn)

# Data Processing ############################################################

data <- format_baseline_data("/Users/robertclay/paper1/data/corrected_US/", c(2011,2012,2013))
summary(data)
# Rescale continuous variables
scaled_variables <- c("age", "gross_hh_income", "SF.12")
data[,scaled_variables] <- scale(data[,scaled_variables])

# Convert discrete variables to factors. 
data$depression_change <- factor(data$depression_change, ordered=T)

data <- tidyr::drop_na(data)
n <- nrow(data)
set.seed(88)
Inx <- sample(n, 0.8*n)
train<-data[Inx,]
test<-data[-Inx,]

y <- train$depression
y2<- test$depression


# description plots ##################################################################


pdf("../plots/total_missingness_structure.pdf")
aggr(data, numbers=T, cex.axis=0.5, col = c(blue, orange))
dev.off()

pdf("../plots/depression_missingness_structure.pdf")
aggr(data[, c("SF.12", "depression", "depression_change")], labels = c("SF.12", "depression", "depression_change"), cex.axis = 0.7, oma=c(8,8,4,4), combined=F, sortVars=T, varheight=F, numbers=T, prop=T, col = c(blue, orange))
dev.off()


pdf("../plots/ethnicity_SF12_spinogram.pdf")
spineMiss(data[, c("ethnicity", "SF.12")], col = c(blue, orange))
dev.off()

pdf("../plots/age_SF12_spinogram.pdf")
spineMiss(data[, c("age", "SF.12")], col = c(blue, orange))
dev.off()

pdf("../plots/jbsec_SF12_spinogram.pdf")
spineMiss(data[, c("job_sec", "SF.12")], col = c(blue, orange))
dev.off()

pdf("../plots/labour_SF12_spinogram.pdf")
spineMiss(data[, c("labour_state", "SF.12")], col = c(blue, orange))
dev.off()

is_missing_sf12 <- is.na(data$SF.12)
pdf("../plots/age_SF12_histograms.pdf")
histogram(~age|is_missing_sf12, data)
dev.off()

# MICE #########################################################################

data$depression <- as.factor(data$depression)
data$depression_change <- as.factor(data$depression_change)
#imp_columns <- c("SF.12", "depression_change","depression", "labour_state",
#                 "age", "sex", "ethnicity", "job_sec", "education_state")
print(summary(data[,c("SF.12", "depression_change", "depression")]))
mice_set <- mice(data = data,
                 m = 2, maxit = 5, seed = 500,
                 remove.collinear=T)
print(summary(complete(mice_set)[,c("SF.12", "depression_change", "depression")]))


# Pooled OLS fit ##############################################################

mice.sf12.lm <- with(mice_set, lm(SF.12 ~ factor(sex) + 
                                    factor(ethnicity) + 
                                    data$age + 
                                    factor(education_state) + 
                                    factor(labour_state) + 
                                    factor(job_sec)))
final.pool<- with(data=mice_set, expr={lm(SF.12~depression+depression_change)})

summary(final)
mice.preds<- predict(mice.sf12.lm, test)

pdf("../plots/SF12_MICE_comparison_histogram.pdf")
hist(data$SF.12, col = t_blue, border = blue, freq=F, ylim = c(0, 0.05))
hist(mice.preds, col = t_orange, border = orange, freq=F, add = T)
legend("topright", c("Complete", "Imputed"), fill=c(blue, orange))
dev.off()


pdf("..plots/MICE_depression_scatter.pdf")
plot_depression_residuals(mice.preds, test$SF.12)
dev.off()





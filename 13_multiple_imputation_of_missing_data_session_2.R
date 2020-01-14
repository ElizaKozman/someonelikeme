library(stargazer)
library(kableExtra)
library(knitr)
library(plyr)
library(dplyr)
library(purrr)
library(stargazer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape)
library(BBmisc)
library(extrafont)
library("xtable")
library(rmarkdown)
library(mice)
library(VIM)
library(doRNG)
library(tidyverse)

full_data<-read.csv("rand_matched_with_full_data_and_school_data_formatted_2.csv")
full_data$maths_score<-as.numeric(as.character(full_data$maths_score))

# Keep rows where sheets returned and not blank
full_data<-subset(full_data, sheet_returned_s2==1)
full_data<-subset(full_data, p1_blank_s2==0)

# Try with just wwcb
full_data<-subset(full_data, full_data$wwcb==1)

# Drop where no mob otherwise will impute
full_data<-subset(full_data, !is.na(mob))

nrow(full_data[is.na(full_data$isq_total_s2),])
nrow(full_data[is.na(full_data$uni_s2),])
nrow(full_data[is.na(full_data$app_s2),])
nrow(full_data[is.na(full_data$lottery_s2),])

# Try with just wwcb
full_data<-subset(full_data, full_data$wwcb==1)

#### Impute missing isq scores (where no data) ####

# Predictor matrix to exclude treatment indicators
# See https://rpubs.com/kaz_yos/mice-exclude
allVars <- names(full_data)
predictorMatrix <- matrix(0, ncol = length(allVars), nrow = length(allVars))
rownames(predictorMatrix) <- allVars
colnames(predictorMatrix) <- allVars

## names of variables with missingness
missVars <- names(full_data)[colSums(is.na(full_data)) > 0]
full_data$ea
###  Specify Variables informing imputation
imputerVars <- c("school", "wwcb", "prop_wwcb_class", "female", "sen", "fsm", "white_british", "isq_total", "uni", "app", "lottery", "q_total", "treatment")
## Keep variables that actually exist in dataset
imputerVars <- intersect(unique(imputerVars), allVars)
imputerVars
imputerMatrix <- predictorMatrix
imputerMatrix[,imputerVars] <- 1
imputerMatrix

# Specify variables with missingness to be imputed
imputedOnlyVars <- c("isq_total_s2",  "uni_s2",  "app_s2", "lottery_s2")
## Imputers that have missingness must be imputed
imputedVars <- intersect(unique(c(imputedOnlyVars, imputerVars)), missVars)
imputedVars
imputedMatrix <- predictorMatrix
imputedMatrix[imputedVars,] <- 1
imputedMatrix

# Construct a full predictor matrix (rows: imputed variables; cols: imputer variables)
# Keep correct imputer-imputed pairs only
predictorMatrix <- imputerMatrix * imputedMatrix
# Diagonals must be zeros (a variable cannot impute itself)
diag(predictorMatrix) <- 0
predictorMatrix

##  Dry-run mice for imputation methods
dryMice <- mice(data = full_data, m = 1, predictorMatrix = predictorMatrix, maxit = 0)
# Update predictor matrix
predictorMatrix <- dryMice$predictorMatrix
imputerVars <- colnames(predictorMatrix)[colSums(predictorMatrix) > 0]
imputerVars
imputedVars <- rownames(predictorMatrix)[rowSums(predictorMatrix) > 0]
imputedVars
setdiff(imputerVars, imputedVars)
intersect(imputerVars, imputedVars)
setdiff(imputedVars, imputerVars)
setdiff(missVars, imputedVars)
predictorMatrix[rowSums(predictorMatrix) > 0, colSums(predictorMatrix) > 0]

dryMice$method[setdiff(allVars, imputedVars)] <- ""
dryMice$method[sapply(dryMice$method, nchar) > 0]

M <- 5
## Parallelized execution
miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
  miceout <- mice(data = full_data, m = 1, print = TRUE,
                  predictorMatrix = predictorMatrix, method = dryMice$method,
                  MaxNWts = 2000)
  ## Make sure to return the output
  miceout
}

imp<-miceout
summary(imp)

#### EXAMINE IMP ####

bwplot(imp, isq_total)
bwplot(imp, uni)
bwplot(imp, app)
bwplot(imp, q_total)
bwplot(imp, isq_total_s2)
bwplot(imp, uni_s2)
bwplot(imp, app_s2)
bwplot(imp, t_uni)
bwplot(imp, t_app)

# NB expect distn to be similar but not identical
densityplot(imp, ~isq_total)
densityplot(imp, ~uni)
densityplot(imp, ~app)
densityplot(imp, ~q_total)
densityplot(imp, ~lottery)

# Use example in documentation: https://cran.r-project.org/web/packages/miceadds/miceadds.pdf
datlist <- miceadds::mids2datlist(imp)

# Subset to just look at wwcb
# datlist_wwcb<-miceadds::subset_datlist(datlist, subset=c(datlist[[1]]$wwcb==1))

#### READ IN MEAN IMPUTED ISQ ####
mean_imputed<-read.csv("manually_imputed_data_s2.csv")
mean_imputed<-subset(mean_imputed, mean_imputed$wwcb==1)
#### ANY_RM VERSUS CONTROL  ####

m0 <- miceadds::lm.cluster(data=mean_imputed, 
                           formula=isq_imputed_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=mean_imputed$class_id)

summary(m0)
beta0<-m0$results
p_value0<-m0$p
se_0<-m0$se

m0_d <- lm(isq_imputed_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=mean_imputed)


# 01
m01 <- miceadds::lm.cluster(data=mean_imputed, 
                           formula=isq_imputed_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school+any_rm*prop_wwcb_class,
                           cluster=mean_imputed$class_id)

beta01<-m01$results
p_value01<-m01$p
se_01<-m01$se

m01_d <- lm(isq_imputed_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school+any_rm*prop_wwcb_class, data=mean_imputed)

# 1

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=isq_total_s2 ~ any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )

# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta1<-a$results
p_value1<-a$p
se_1<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)
number<-as.numeric(as.character(nrow(single_list)))
m1_d<-lm(isq_total_s2 ~ any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school,data=single_list)

# 2

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=isq_total_s2 ~ any_rm+prime+fsm+sen+mob+prop_wwcb_class+year+any_rm*prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta2<-a$results
p_value2<-a$p
se_2<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)
number<-as.numeric(as.character(nrow(single_list)))
m2_d<-lm(isq_total_s2 ~ any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school,data=single_list)
summary(m2_d)


# 3

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=uni_s2 ~ any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta3<-a$results
p_value3<-a$p
se_3<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)
number<-as.numeric(as.character(nrow(single_list)))
m3_d<-lm(uni_s2 ~ any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school,data=single_list)
summary(m3_d)

# 4

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=uni_s2 ~ any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta4<-a$results
p_value4<-a$p
se_4<-a$se
# Choose first df
single_list<-mice::complete(imp, 1)
number<-as.numeric(as.character(nrow(single_list)))
m4_d<-lm(uni_s2 ~ any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school,data=single_list)
summary(m4_d)

# 5

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=app_s2 ~ any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta5<-a$results
p_value5<-a$p
se_5<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)

number<-as.numeric(as.character(nrow(single_list)))
m5_d<-lm(app_s2 ~ any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school,data=single_list)
summary(m5_d)

# 6

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=app_s2 ~ any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta6<-a$results
p_value6<-a$p
se_6<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)

number<-as.numeric(as.character(nrow(single_list)))
m6_d<-lm(app_s2 ~ any_rm+prime+fsm+sen+mob+year+year+prop_wwcb_class+any_rm*prop_wwcb_class+school,data=single_list)
summary(m6_d)

# 8

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=q_total ~ any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta7<-a$results
p_value7<-a$p
se_7<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)

number<-as.numeric(as.character(nrow(single_list)))
m7_d<-lm(q_total ~ any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school,data=single_list)
summary(m7_d)

# 8

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=q_total ~ any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta8<-a$results
p_value8<-a$p
se_8<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)

number<-as.numeric(as.character(nrow(single_list)))
m8_d<-lm(q_total ~ any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school,data=single_list)
summary(m8_d)


# 9

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::glm.cluster(data=full_data, formula=lottery_s2 ~ any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school, family="binomial",
                       cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta9<-a$results
p_value9<-a$p
se_9<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)

number<-as.numeric(as.character(nrow(single_list)))
m9_d<-glm(lottery_s2 ~ any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school,data=single_list,family="binomial")
summary(m9_d)


# 10

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::glm.cluster(data=full_data, formula=lottery_s2 ~ any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school, family="binomial",
                        cluster=full_data$class_id)
} )

# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta10<-a$results
p_value10<-a$p
se_10<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)

number<-as.numeric(as.character(nrow(single_list)))
m10_d<-glm(lottery_s2 ~ any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school,data=single_list,family="binomial")


stargazer(m0_d, m01_d,m1_d,m2_d,m3_d,m4_d,m5_d, m6_d, m9_d, m10_d,
          se = list(se_0, se_01, se_1, se_2, se_3, se_4, se_5, se_6, se_9, se_10),
          p=list(p_value0, p_value01,p_value1, p_value2, p_value3, p_value4, p_value5, p_value6, p_value9, p_value10),
          coef=list(beta0, beta01, beta1, beta2, beta3, beta4, beta5, beta6, beta9, beta10),
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          omit = c("school", "fsm", "sen","mob", "year"),
          covariate.labels = c("Any role model", "Similarity prime", "Proportion WWCB", "Any role model x Proportion WWCB"),
          star.char = c("+", "*", "**"), 
          title="",
          notes.append = FALSE,
          notes.label = "",
          omit.stat = c("f", "rsq", "adj.rsq", "ser" ),
          header=FALSE,
          column.sep.width = "-1pt",
          font.size="scriptsize")

#### UNI VERSUS CONTROL  ####

m0 <- miceadds::lm.cluster(data=mean_imputed, 
                           formula=isq_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=mean_imputed$class_id)

summary(m0)
beta0<-m0$results
p_value0<-m0$p
se_0<-m0$se

m0_d <- lm(isq_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=mean_imputed)

# 01
m01 <- miceadds::lm.cluster(data=mean_imputed, 
                            formula=isq_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school+t_uni*prop_wwcb_class,
                            cluster=mean_imputed$class_id)

beta01<-m01$results
p_value01<-m01$p
se_01<-m01$se

m01_d <- lm(isq_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school+t_uni*prop_wwcb_class, data=mean_imputed)

# 1

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=isq_total_s2 ~ t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )

# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta1<-a$results
p_value1<-a$p
se_1<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)
number<-as.numeric(as.character(nrow(single_list)))
m1_d<-lm(isq_total_s2 ~ t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school,data=single_list)

# 2

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=isq_total_s2 ~ t_uni+prime+fsm+sen+mob+prop_wwcb_class+year+t_uni*prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta2<-a$results
p_value2<-a$p
se_2<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)
number<-as.numeric(as.character(nrow(single_list)))
m2_d<-lm(isq_total_s2 ~ t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school,data=single_list)
summary(m2_d)


# 3

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=uni_s2 ~ t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta3<-a$results
p_value3<-a$p
se_3<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)
number<-as.numeric(as.character(nrow(single_list)))
m3_d<-lm(uni_s2 ~ t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school,data=single_list)
summary(m3_d)

# 4

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=uni_s2 ~ t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta4<-a$results
p_value4<-a$p
se_4<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)
number<-as.numeric(as.character(nrow(single_list)))
m4_d<-lm(uni_s2 ~ t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school,data=single_list)
summary(m4_d)

# 5

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=app_s2 ~ t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta5<-a$results
p_value5<-a$p
se_5<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)

number<-as.numeric(as.character(nrow(single_list)))
m5_d<-lm(app_s2 ~ t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school,data=single_list)
summary(m5_d)

# 6

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=app_s2 ~ t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta6<-a$results
p_value6<-a$p
se_6<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)

number<-as.numeric(as.character(nrow(single_list)))
m6_d<-lm(app_s2 ~ t_uni+prime+fsm+sen+mob+year+year+prop_wwcb_class+t_uni*prop_wwcb_class+school,data=single_list)
summary(m6_d)

# 8

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=q_total ~ t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta7<-a$results
p_value7<-a$p
se_7<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)

number<-as.numeric(as.character(nrow(single_list)))
m7_d<-lm(q_total ~ t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school,data=single_list)
summary(m7_d)

# 8

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=q_total ~ t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta8<-a$results
p_value8<-a$p
se_8<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)

number<-as.numeric(as.character(nrow(single_list)))
m8_d<-lm(q_total ~ t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school,data=single_list)
summary(m8_d)


# 9

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::glm.cluster(data=full_data, formula=lottery_s2 ~ t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school, family="binomial",
                        cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta9<-a$results
p_value9<-a$p
se_9<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)

number<-as.numeric(as.character(nrow(single_list)))
m9_d<-glm(lottery_s2 ~ t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school,data=single_list,family="binomial")
summary(m9_d)


# 10

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::glm.cluster(data=full_data, formula=lottery_s2 ~ t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school, family="binomial",
                        cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta10<-a$results
p_value10<-a$p
se_10<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)

number<-as.numeric(as.character(nrow(single_list)))
m10_d<-glm(lottery_s2 ~ t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school,data=single_list,family="binomial")
summary(m10_d)

# NEED TO ADD INTERACTION BY HAND

stargazer(m0_d, m01_d,m1_d,m2_d,m3_d,m4_d,m5_d, m6_d, m9_d, m10_d,
          se = list(se_0, se_01, se_1, se_2, se_3, se_4, se_5, se_6, se_9, se_10),
          p=list(p_value0, p_value01,p_value1, p_value2, p_value3, p_value4, p_value5, p_value6, p_value9, p_value10),
          coef=list(beta0, beta01, beta1, beta2, beta3, beta4, beta5, beta6, beta9, beta10),
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          omit = c("school", "fsm", "sen","mob", "year"),
          covariate.labels = c("University role model", "Similarity prime", "Proportion WWCB", "Any role model x Proportion WWCB"),
          star.char = c("+", "*", "**"), 
          title="",
          notes.append = FALSE,
          notes.label = "",
          omit.stat = c("f", "rsq", "adj.rsq", "ser" ),
          header=FALSE,
          column.sep.width = "-1pt",
          font.size="scriptsize",
          type="text")

#### APP VERSUS CONTROL  ####

m0 <- miceadds::lm.cluster(data=mean_imputed, 
                           formula=isq_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=mean_imputed$class_id)

summary(m0)
beta0<-m0$results
p_value0<-m0$p
se_0<-m0$se

m0_d <- lm(isq_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=mean_imputed)

# 01
m01 <- miceadds::lm.cluster(data=mean_imputed, 
                            formula=isq_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school+t_app*prop_wwcb_class,
                            cluster=mean_imputed$class_id)

beta01<-m01$results
p_value01<-m01$p
se_01<-m01$se

m01_d <- lm(isq_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school+t_app*prop_wwcb_class, data=mean_imputed)

# 1

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=isq_total_s2 ~ t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )

# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta1<-a$results
p_value1<-a$p
se_1<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)
number<-as.numeric(as.character(nrow(single_list)))
m1_d<-lm(isq_total_s2 ~ t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school,data=single_list)

# 2

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=isq_total_s2 ~ t_app+prime+fsm+sen+mob+prop_wwcb_class+year+t_app*prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta2<-a$results
p_value2<-a$p
se_2<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)
number<-as.numeric(as.character(nrow(single_list)))
m2_d<-lm(isq_total_s2 ~ t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school,data=single_list)
summary(m2_d)


# 3

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=uni_s2 ~ t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta3<-a$results
p_value3<-a$p
se_3<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)
number<-as.numeric(as.character(nrow(single_list)))
m3_d<-lm(uni_s2 ~ t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school,data=single_list)
summary(m3_d)

# 4

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=uni_s2 ~ t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta4<-a$results
p_value4<-a$p
se_4<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)
number<-as.numeric(as.character(nrow(single_list)))
m4_d<-lm(uni_s2 ~ t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school,data=single_list)
summary(m4_d)

# 5

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=app_s2 ~ t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta5<-a$results
p_value5<-a$p
se_5<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)

number<-as.numeric(as.character(nrow(single_list)))
m5_d<-lm(app_s2 ~ t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school,data=single_list)
summary(m5_d)

# 6

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=app_s2 ~ t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta6<-a$results
p_value6<-a$p
se_6<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)

number<-as.numeric(as.character(nrow(single_list)))
m6_d<-lm(app_s2 ~ t_app+prime+fsm+sen+mob+year+year+prop_wwcb_class+t_app*prop_wwcb_class+school,data=single_list)
summary(m6_d)

# 8

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=q_total ~ t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta7<-a$results
p_value7<-a$p
se_7<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)

number<-as.numeric(as.character(nrow(single_list)))
m7_d<-lm(q_total ~ t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school,data=single_list)
summary(m7_d)

# 8

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::lm.cluster(data=full_data, formula=q_total ~ t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school,
                       cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta8<-a$results
p_value8<-a$p
se_8<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)

number<-as.numeric(as.character(nrow(single_list)))
m8_d<-lm(q_total ~ t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school,data=single_list)
summary(m8_d)


# 9

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::glm.cluster(data=full_data, formula=lottery_s2 ~ t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school, family="binomial",
                        cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta9<-a$results
p_value9<-a$p
se_9<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)

number<-as.numeric(as.character(nrow(single_list)))
m9_d<-glm(lottery_s2 ~ t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school,data=single_list,family="binomial")
summary(m9_d)


# 10

# Linear regression with cluster robust standard errors
mod <- lapply(datlist_wwcb, FUN=function(data){
  miceadds::glm.cluster(data=full_data, formula=lottery_s2 ~ t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school, family="binomial",
                        cluster=full_data$class_id)
} )


# Extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# Conduct statistical inference 
summary(miceadds::pool_mi( qhat=betas, u=vars ) )

a<-summary( miceadds::pool_mi( qhat=betas, u=vars ) )
beta10<-a$results
p_value10<-a$p
se_10<-a$se
# Choose first df
single_list<-  mice::complete(imp, 1)

number<-as.numeric(as.character(nrow(single_list)))
m10_d<-glm(lottery_s2 ~ t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school,data=single_list,family="binomial")
summary(m10_d)

# NEED TO ADD INTERACTION BY HAND

stargazer(m0_d, m01_d,m1_d,m2_d,m3_d,m4_d,m5_d, m6_d, m9_d, m10_d,
          se = list(se_0, se_01, se_1, se_2, se_3, se_4, se_5, se_6, se_9, se_10),
          p=list(p_value0, p_value01,p_value1, p_value2, p_value3, p_value4, p_value5, p_value6, p_value9, p_value10),
          coef=list(beta0, beta01, beta1, beta2, beta3, beta4, beta5, beta6, beta9, beta10),
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          omit = c("school", "fsm", "sen","mob", "year"),
          covariate.labels = c("Apprenticeship role model", "Similarity prime", "Proportion WWCB", "Any role model x Proportion WWCB"),
          star.char = c("+", "*", "**"), 
          title="",
          notes.append = FALSE,
          notes.label = "",
          omit.stat = c("f", "rsq", "adj.rsq", "ser" ),
          header=FALSE,
          column.sep.width = "-1pt",
          font.size="scriptsize",
          type="text")





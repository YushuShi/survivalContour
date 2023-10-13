# survivalContour

## General info
Show the predicted survival or cumulative incidence function (competing risks data) over time for single continuous covariate in the form of contour plot or 3D contour plot. The estimate for survival probability and cumulative incidence function(CIF) are based on various survival models. The models that survivalContour supports are listed below. The corresponding R shiny app is availble at [survivalContourRshinyapp](https://github.com/YushuShi/survivalContourRshiny).

## Install

```
library(devtools)
install_github("YushuShi/survivalContour")
```
	

## Usage

```
survivalContour(
  data,
  model,
  contCov,
  D3 = FALSE,
  CI3D = FALSE,
  contCovName = NULL,
  nCovEval = 30,
  otherCov = NULL,
  strataName = NULL
)
```

* **data** The original data used fit the model.

* **model** The model used to analyze the data.

* **contCov** Specify which continuous covariate is used for the y-axis of the contour plot.

* **D3** Should "survivalContour" generate a 2D contour plot, or a 3D contour plot.

* **CI3D** Should "survivalContour" generate a semi-transparent layer of confidence intervals for 3D contour plot. This option is only applicable for Cox model or stratified Cox model with or without interval censored data, and parametric or spline models. Fine and Gray model for competing risks data or deep neural network models are not applicable.

* **contCovName** A character string on the name of the continuous covariate shown on the contour plot. If not provided by the user, the printed name will be the covaraite name.

* **nCovEval** Number of covaraite values where predicted survival is evaluated.

* **otherCov** A vector of values of other covariates except for the continuous covariate used to generate contour plot. The default value is NULL, and the mean of continuous covariates and the most frequent values of categorical covariates are used.

* **strataName** The print name of the stratifying covariate used in stratified Cox model. If not provided by the user, "survivalContour" will use the name of the stratifying covariate.

## Output
A colored contour plot showing the association between survival time and continuous covariate value.

## Details

| Model | Package | survivalContour function Input object type |Confidence interval in 3D plot|
  |---------|---------|---------|---------|
  | Cox model| survival| coxph | Y |
  | stratified Cox model| survival |coxph| Y |
  | Cox model with interval censored data| mets |phreg| Y |
  | stratified Cox model with interval censored data| mets |phreg| Y |
  | parametric and spline model | flexsurv | flexsurvreg|Y |
  | Fine and Gray model for competing risks data | riskRegression+prodlim | FRG|N |
  | Fine and Gray model for competing risks data with interval censored data|intccr |ciregic| N |
  |random survival forest| randomForestSRC|rfsrc|N|
  |coxtime,deepsurv,deephit,loghaz,pchazard| reticulate+survivalmodels|pycox|N|

## Reference
Shi Y, Zhang L, Do KA, Jenq RR, Peterson CB (2023) survivalContour: Visualizing predicted survival for continuous covariate via colored contour plot

## Note

The survival contour plot focuses on displaying predicted survival, and does not aim to provide any insights regarding model fit. We recommend users to calculate Harrell's Concordance Index (C-index) to evaluate model fit before sharing results from survivalContour. Complex deep neural network-based models, like deephit or deepsurv, often require significant hand tuning and the best practices for fitting such intricate models is beyond the scope of this paper. Additionally, flexible models may be susceptible to overfitting, so it is always advisable to use an independent dataset for validation.


## Examples

```

library(devtools)
library(mets)
library(flexsurv)
library(prodlim)
library(riskRegression)
library(intccr)
library(reticulate)
library(survivalmodels)
library(survivalContour)
library(survival)
# Cox model without interval censoring
dataPath<-system.file("extdata","veteran.csv",
                      package = "survivalContour")
data<-read.csv(dataPath,row.names = 1)
coxResult<-coxph(Surv(time,status)~PRS+age+diagtime,data=data)

survivalContour(data,coxResult,"PRS")
survivalContour(data,coxResult,"PRS",D3=TRUE)
survivalContour(data,coxResult,"PRS",D3=TRUE,CI3D=TRUE)

paraResult<-flexsurvreg(Surv(time,status)~.,data=data,dist="weibull")
survivalContour(data,paraResult,"karno")
survivalContour(data,paraResult,"karno",D3=TRUE)
survivalContour(data,paraResult,"karno",D3=TRUE,CI3D=TRUE)

splineResult<-flexsurvspline(Surv(time,status)~.,data=data)
survivalContour(data,splineResult,"karno")
survivalContour(data,splineResult,"karno",D3=TRUE)
survivalContour(data,splineResult,"karno",D3=TRUE,CI3D=TRUE)
# stratified data analysis
dataPath<-system.file("extdata","veteran2.csv",
                      package = "survivalContour")
data<-read.csv(dataPath,row.names = 1)
coxResult<-coxph(Surv(time,status)~.-celltype+strata(celltype),data=data)
survivalContour(data,coxResult,"karno")
survivalContour(data,coxResult,"karno",D3=TRUE)
survivalContour(data,coxResult,"karno",D3=TRUE,CI3D=TRUE)
#########################
dataPath<-system.file("extdata","bcdeter2.csv",
                      package = "survivalContour")
data<-read.csv(dataPath,row.names = 1)
coxResult<-phreg(Surv(lower,upper,type="interval2")~cont+treat,data=data)
survivalContour(data,coxResult,"cont")
survivalContour(data,coxResult,"cont",D3=TRUE)
survivalContour(data,coxResult,"cont",D3=TRUE,CI3D=TRUE)


coxResult<-phreg(Surv(lower,upper,type="interval2")~cont+strata(treat),data=data)
survivalContour(data,coxResult,"cont")
survivalContour(data,coxResult,"cont",D3=TRUE)
survivalContour(data,coxResult,"cont",D3=TRUE,CI3D=TRUE)

################
#Fine and Gray model
dataPath<-system.file("extdata","Paquid.csv",
                      package = "survivalContour")
data<-read.csv(dataPath,row.names = 1)
FGResult<-FGR(Hist(time,status)~DSST+MMSE,data=data,cause=1)
survivalContour(data,FGResult,"DSST")
survivalContour(data,FGResult,"DSST",D3=TRUE)

#Fine and Gray interval censored data
dataPath<-system.file("extdata","sim1234.csv",
                      package = "survivalContour")
data<-read.csv(dataPath,row.names = 1)
FGResult<-ciregic(intccr::Surv2(v=lower,u=upper,event=event)~continuousCov,data=data,alpha = c(0, 1), nboot = 0, do.par = FALSE)
survivalContour(data,FGResult,"continuousCov")
survivalContour(data,FGResult,"continuousCov",D3=TRUE)
#######################
dataPath<-system.file("extdata","data1.csv",
                      package = "survivalContour")
data1<-read.csv(dataPath)
data1$num_comorb<-factor(data1$num_comorb)
data1$race<-factor(data1$race)
data1$cancer<-factor(data1$cancer)
data1$diab<-factor(data1$diab)
data1$sex<-factor(data1$sex)
data1$dementia<-factor(data1$dementia)
data1$time<-data1$duration
data1$duration<-NULL
data1$status<-data1$event
data1$event<-NULL
trainModel<-deepsurv(data=data1,frac=0.2,
                     activation="relu",
                     num_nodes=c(64L,64L),
                     dropout=0.2,
                     early_stopping=TRUE,
                     epochs=1000L,
                     patience=50L,
                     batch_norm = TRUE,
                     batch_size=256L,
                     shuffle=TRUE)

survivalContour(data1,trainModel,"resp")
survivalContour(data1,trainModel,"resp",D3=TRUE)

trainModel<-deephit(data=data1,frac=0.2,
                    activation="relu",
                    num_nodes=c(64L,64L),
                    dropout=0.2,
                    early_stopping=TRUE,
                    epochs=1000L,
                    patience=50L,
                    batch_norm = TRUE,
                    batch_size=256L,
                    shuffle=TRUE)

survivalContour(data1,trainModel,"resp")
survivalContour(data1,trainModel,"resp",D3=TRUE)

trainModel<-coxtime(data=data1,frac=0.2,
                    activation="relu",
                    num_nodes=c(64L,64L),
                    dropout=0.2,
                    early_stopping=TRUE,
                    epochs=1000L,
                    patience=50L,
                    batch_norm = TRUE,
                    batch_size=256L,
                    shuffle=TRUE)

survivalContour(data1,trainModel,"resp")
survivalContour(data1,trainModel,"resp",D3=TRUE)

trainModel<-loghaz(data=data1,frac=0.2,
                   activation="relu",
                   num_nodes=c(64L,64L),
                   dropout=0.2,
                   early_stopping=TRUE,
                   epochs=1000L,
                   patience=50L,
                   batch_norm = TRUE,
                   batch_size=256L,
                   shuffle=TRUE)

survivalContour(data1,trainModel,"resp")
survivalContour(data1,trainModel,"resp",D3=TRUE)

trainModel<-pchazard(data=data1,frac=0.2,
                     activation="relu",
                     num_nodes=c(64L,64L),
                     dropout=0.2,
                     early_stopping=TRUE,
                     epochs=1000L,
                     patience=50L,
                     batch_norm = TRUE,
                     batch_size=256L,
                     shuffle=TRUE)

survivalContour(data1,trainModel,"resp")
survivalContour(data1,trainModel,"resp",D3=TRUE)

########################

 data(veteran,package="randomForestSRC")
 veteran.grow<-rfsrc(Surv(time,status)~.,veteran,ntree=100)
 survivalContour(veteran,veteran.grow,"karno")
 survivalContour(veteran,veteran.grow,"karno",D3=TRUE)
```

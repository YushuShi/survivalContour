#' survivalContour
#'
#'Show the predicted survival or cumulative incidence function (competing risks data) over time for single continuous covariate in the form of contour plot or 3D contour plot. The estimate for survival probability and cumulative incidence function(CIF) are based on various survival models. The models that survivalContour supports are listed below:

#' | Model | Package | Object |Confidence interval in 3D plot|
#'   |---------|---------|---------|---------|
#'   | Cox model| survival| coxph | Y |
#'   | stratified Cox model| survival |coxph| Y |
#'   | Cox model with interval censored data| mets |phreg| Y |
#'   | stratified Cox model with interval censored data| mets |phreg| Y |
#'   | parametric and spline model | flexsurv | flexsurvreg|Y |
#'   | Fine and Gray model for competing risks data | riskRegression+prodlim | FRG|N |
#'   | Fine and Gray model for competing risks data with interval censored data|intccr |ciregic| N |
#'   |coxtime,deepsurv,deephit,loghaz,pchazard| reticulate+survivalmodels|pycox|N|
#' @param data The original data used fit the model.
#' @param model The model used to analyze the data.
#' @param contCov Specify which continuous covariate is used for the y-axis of the contour plot.
#' @param D3 Should "survivalContour" generate a 2D contour plot, or a 3D contour plot.
#' @param CI3D Should "survivalContour" generate a semi-transparent layer of confidence intervals for 3D contour plot. This option is only applicable for Cox model or stratified Cox model with or without interval censored data, and parametric or spline models. Fine and Gray model for competing risks data or deep neural network models are not applicable.
#' @param contCovName A character string on the name of the continuous covariate shown on the contour plot. If not provided by the user, the printed name will be the covaraite name.
#' @param nCovEval Number of covaraite values where predicted survival is evaluated.
#' @param otherCov A vector of values of other covariates except for the continuous covariate used to generate contour plot. The default value is NULL, and the mean of continuous covariates and the most frequent values of categorical covariates are used.
#' @param strataName The print name of the stratifying covariate used in stratified Cox model. If not provided by the user, "survivalContour" will use the name of the stratifying covariate.
#' @return The result of the function.
#' @export
#' @examples
#' library(devtools)
#' library(mets)
#' library(flexsurv)
#' library(prodlim)
#' library(riskRegression)
#' library(intccr)
#' library(reticulate)
#' library(survivalmodels)
#' library(survivalContour)
#' library(survival)
#' # Cox model without interval censoring
#' dataPath<-system.file("extdata","veteran.csv",
#'                       package = "survivalContour")
#' data<-read.csv(dataPath,row.names = 1)
#' coxResult<-coxph(Surv(time,status)~PRS+age+diagtime,data=data)
#'
#' survivalContour(data,coxResult,"PRS")
#' survivalContour(data,coxResult,"PRS",D3=TRUE)
#' survivalContour(data,coxResult,"PRS",D3=TRUE,CI3D=TRUE)
#'
#' paraResult<-flexsurvreg(Surv(time,status)~.,data=data,dist="weibull")
#' survivalContour(data,paraResult,"karno")
#' survivalContour(data,paraResult,"karno",D3=TRUE)
#' survivalContour(data,paraResult,"karno",D3=TRUE,CI3D=TRUE)
#'
#' splineResult<-flexsurvspline(Surv(time,status)~.,data=data)
#' survivalContour(data,splineResult,"karno")
#' survivalContour(data,splineResult,"karno",D3=TRUE)
#' survivalContour(data,splineResult,"karno",D3=TRUE,CI3D=TRUE)
#' # stratified data analysis
#' dataPath<-system.file("extdata","veteran2.csv",
#'                       package = "survivalContour")
#' data<-read.csv(dataPath,row.names = 1)
#' coxResult<-coxph(Surv(time,status)~.-celltype+strata(celltype),data=data)
#' survivalContour(data,coxResult,"karno")
#' survivalContour(data,coxResult,"karno",D3=TRUE)
#' survivalContour(data,coxResult,"karno",D3=TRUE,CI3D=TRUE)
#' #########################
#' dataPath<-system.file("extdata","bcdeter2.csv",
#'                       package = "survivalContour")
#' data<-read.csv(dataPath,row.names = 1)
#' coxResult<-phreg(Surv(lower,upper,type="interval2")~cont+treat,data=data)
#' survivalContour(data,coxResult,"cont")
#' survivalContour(data,coxResult,"cont",D3=TRUE)
#' survivalContour(data,coxResult,"cont",D3=TRUE,CI3D=TRUE)
#'
#' # This has problems
#' coxResult<-phreg(Surv(lower,upper,type="interval2")~cont+strata(treat),data=data)
#' survivalContour(data,coxResult,"cont")
#' survivalContour(data,coxResult,"cont",D3=TRUE)
#' survivalContour(data,coxResult,"cont",D3=TRUE,CI3D=TRUE)
#'
#' ################
#' #Fine and Gray model
#' dataPath<-system.file("extdata","Paquid.csv",
#'                       package = "survivalContour")
#' data<-read.csv(dataPath,row.names = 1)
#' FGResult<-FGR(Hist(time,status)~DSST+MMSE,data=data,cause=1)
#' survivalContour(data,FGResult,"DSST")
#' survivalContour(data,FGResult,"DSST",D3=TRUE)
#'
#' #Fine and Gray interval censored data
#' dataPath<-system.file("extdata","sim1234.csv",
#'                       package = "survivalContour")
#' data<-read.csv(dataPath,row.names = 1)
#' FGResult<-ciregic(intccr::Surv2(v=lower,u=upper,event=event)~continuousCov,data=data,alpha = c(0, 1), nboot = 0, do.par = FALSE)
#' survivalContour(data,FGResult,"continuousCov")
#' survivalContour(data,FGResult,"continuousCov",D3=TRUE)
#' #######################
#' dataPath<-system.file("extdata","data1.csv",
#'                       package = "survivalContour")
#' data1<-read.csv(dataPath)
#' data1$num_comorb<-factor(data1$num_comorb)
#' data1$race<-factor(data1$race)
#' data1$cancer<-factor(data1$cancer)
#' data1$diab<-factor(data1$diab)
#' data1$sex<-factor(data1$sex)
#' data1$dementia<-factor(data1$dementia)
#' data1$time<-data1$duration
#' data1$duration<-NULL
#' data1$status<-data1$event
#' data1$event<-NULL
#' trainModel<-deepsurv(data=data1,frac=0.2,
#'                      activation="relu",
#'                      num_nodes=c(64L,64L),
#'                      dropout=0.2,
#'                      early_stopping=TRUE,
#'                      epochs=1000L,
#'                      patience=50L,
#'                      batch_norm = TRUE,
#'                      batch_size=256L,
#'                      shuffle=TRUE)
#'
#' survivalContour(data1,trainModel,"resp")
#' survivalContour(data1,trainModel,"resp",D3=TRUE)
#'
#' trainModel<-deephit(data=data1,frac=0.2,
#'                     activation="relu",
#'                     num_nodes=c(64L,64L),
#'                     dropout=0.2,
#'                     early_stopping=TRUE,
#'                     epochs=1000L,
#'                     patience=50L,
#'                     batch_norm = TRUE,
#'                     batch_size=256L,
#'                     shuffle=TRUE)
#'
#' survivalContour(data1,trainModel,"resp")
#' survivalContour(data1,trainModel,"resp",D3=TRUE)
#'
#' trainModel<-coxtime(data=data1,frac=0.2,
#'                     activation="relu",
#'                     num_nodes=c(64L,64L),
#'                     dropout=0.2,
#'                     early_stopping=TRUE,
#'                     epochs=1000L,
#'                     patience=50L,
#'                     batch_norm = TRUE,
#'                     batch_size=256L,
#'                     shuffle=TRUE)
#'
#' survivalContour(data1,trainModel,"resp")
#' survivalContour(data1,trainModel,"resp",D3=TRUE)
#'
#' trainModel<-loghaz(data=data1,frac=0.2,
#'                    activation="relu",
#'                    num_nodes=c(64L,64L),
#'                    dropout=0.2,
#'                    early_stopping=TRUE,
#'                    epochs=1000L,
#'                    patience=50L,
#'                    batch_norm = TRUE,
#'                    batch_size=256L,
#'                    shuffle=TRUE)
#'
#' survivalContour(data1,trainModel,"resp")
#' survivalContour(data1,trainModel,"resp",D3=TRUE)
#'
#' trainModel<-pchazard(data=data1,frac=0.2,
#'                      activation="relu",
#'                      num_nodes=c(64L,64L),
#'                      dropout=0.2,
#'                      early_stopping=TRUE,
#'                      epochs=1000L,
#'                      patience=50L,
#'                      batch_norm = TRUE,
#'                      batch_size=256L,
#'                      shuffle=TRUE)
#'
#' survivalContour(data1,trainModel,"resp")
#' survivalContour(data1,trainModel,"resp",D3=TRUE)
#'
#' data(veteran,package="randomForestSRC")
#' veteran.grow<-rfsrc(Surv(time,status)~.,veteran,ntree=100)
#' survivalContour(veteran,veteran.grow,"karno")
#' survivalContour(veteran,veteran.grow,"karno",D3=TRUE)

survivalContour<-function(data,model,contCov,D3=FALSE,CI3D=FALSE,contCovName=NULL,nCovEval=30,otherCov=NULL,strataName=NULL){
  Plot<-NULL
  if(identical(class(model),"coxph")){
    # this is a Cox model without interval censored data
    if(!D3){
      Plot<-coxContour(data,model,contCov,contCovName,nCovEval,otherCov)
    }else{
      Plot<-cox3DContour(data,model,contCov,contCovName,nCovEval,otherCov,CI3D)
    }
    if(grepl("strata\\(",paste(deparse(model$formula),collapse=""))){
      # this is a stratified Cox model
      if(!D3){
        Plot<-coxStrataContour(data,model,contCov,contCovName,nCovEval,otherCov,strataName)
      }else{
        Plot<-coxStrata3DContour(data,model,contCov,contCovName,nCovEval,otherCov,strataName,CI3D)
      }
    }
  }else{
    if(identical(class(model),"phreg")){
      if(model$nstrata==1){
        # this is a Cox model with interval censored data
          if(!D3){
            Plot<-coxIntContour(data,model,contCov,contCovName,nCovEval,otherCov)
          }else{
            Plot<-coxIntContour3D(data,model,contCov,contCovName,nCovEval,otherCov,CI3D)
          }
        }else{
        # this is a stratified Cox model with interval censored data
          if(!D3){
            Plot<-coxIntStrataContour(data,model,contCov,contCovName,nCovEval,otherCov,strataName)
          }else{
            Plot<-coxIntStrataContour3D(data,model,contCov,contCovName,nCovEval,otherCov,strataName,CI3D)
          }
      }
    }else{
      if(identical(class(model),"flexsurvreg")){
        # This is a parametric model
        if(!D3){
            Plot<-paraContour(data,model,contCov,contCovName,nCovEval,otherCov)
          }else{
            Plot<-paraContour3D(data,model,contCov,contCovName,nCovEval,otherCov,CI3D)
          }
      }else{
        if(identical(class(model),"FGR")){
          # a Fine and Gray model without interval censored data
          if(!D3){
            Plot<-FGContour(data,model,contCov,contCovName,nCovEval,otherCov)
          }else{
            Plot<-FGContour3D(data,model,contCov,contCovName,nCovEval,otherCov)
          }
        }else{
          if(identical(class(model),"ciregic")){
            # a Fine and Gray model with interval censored data
            if(!D3){
              Plot<-FGIntContour(data,model,contCov,contCovName,nCovEval,otherCov)
            }else{
              Plot<-FGIntContour3D(data,model,contCov,contCovName,nCovEval,otherCov)
            }
          }else{
            if(sum(grepl("pycox",class(model)))){
              # result using deep neural network
              if(!D3){
                Plot<-pycoxContour(data,model,contCov,contCovName,nCovEval,otherCov)
              }else{
                Plot<-pycoxContour3D(data,model,contCov,contCovName,nCovEval,otherCov)
              }
            }else{
              if(sum(grepl("rfsrc",class(model)))){
                if(!D3){
                  Plot<-rfsrcContour(data,model,contCov,contCovName,nCovEval,otherCov)
                }else{
                  Plot<-rfsrcContour3D(data,model,contCov,contCovName,nCovEval,otherCov)
                }
              }else{
                print("The model provided cannot be handled by survivalContour package.")
              }
            }
          }
        }
      }
    }
  }
  Plot
}

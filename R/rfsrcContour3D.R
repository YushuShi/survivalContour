#' @importFrom stats quantile
#' @importFrom dplyr mutate all_of across
#' @import randomForestSRC

rfsrcContour3D<-function(data,trainModel,contCov,contCovName=NULL,nCovEval=30,otherCov=NULL){
  if(is.null(otherCov)){
    newData<-data.frame(data[1,])
    for(i in 1:ncol(data)){
      newData[,i]<-ifelse(is.numeric(data[,i])|is.integer(data[,i]),mean(data[,i]),names(which.max(table(data[,i])))[1])
    }
  }else{
    newData<-otherCov
  }

  data2<-data.frame(rbind(data,newData))
  newData<-data.frame(data2[nrow(data2),])
  newData<-newData[rep(1,nCovEval),]
  newData[,contCov]<-quantile(data[,contCov],0.025)+(quantile(data[,contCov],0.975)-quantile(data[,contCov],0.025))*(0:(nCovEval-1))/(nCovEval-1)
  newData <- newData %>%
    mutate(across(all_of(colnames(newData)[!colnames(newData)%in%trainModel$xvar.names]), as.integer))
  predictOutcome<-predict(trainModel,newData)
  if(is.null(contCovName)){
    contCovName<-contCov
  }
  timeX<-predictOutcome$time.interest
  contourZ<-predictOutcome$survival#x axis is time
  if(min(predictOutcome$time.interest)>0){
    timeX<-c(0,timeX)
    contourZ<-cbind(rep(1,nrow(contourZ)),contourZ)
  }
  predictOutcome$surv<-contourZ
  s<-contour3D(timeX,
               newData[,contCov],
               predictOutcome,contCovName)
  s
}

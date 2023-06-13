#' @importFrom stats quantile
#' @import reticulate
#' @import survivalmodels
pycoxContour3D<-function(data,trainModel,contCov,contCovName=NULL,nCovEval=30,otherCov=NULL){
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
  predictOutcome<-NULL
  predictOutcome$surv<-predict(trainModel,newData,type="survival")
  if(is.null(contCovName)){
    contCovName<-contCov
  }

  s<-contour3D(as.numeric(colnames(predictOutcome$surv)),
               newData[,contCov],
               predictOutcome,contCovName)

  s
}

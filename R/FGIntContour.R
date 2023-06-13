#' @importFrom stats quantile
#' @import intccr
FGIntContour<-function(data,trainModel,contCov,contCovName=NULL,nCovEval=30,otherCov=NULL){
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

  ctime<-seq(0,trainModel$tms[2],length.out=50)
  predMat<-NULL
  y<-newData[,contCov]
  newData<-data.frame(newData[,trainModel$varnames])
  for(i in 1:nCovEval){
    pred <- predict(object = trainModel, covp = as.numeric(newData[i,]), times = ctime)
    predMat<-rbind(predMat,pred$cif1)
  }
  if(is.null(contCovName)){
    contCovName<-contCov
  }
  histData<-data[,contCov]
  s<-survContour(ctime,y,predMat,histData,contCovName,competingIndi=TRUE)
  s
}

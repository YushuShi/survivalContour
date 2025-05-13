#' @importFrom stats quantile
#' @importFrom riskRegression predictRisk

FGContour<-function(data,trainModel,contCov,contCovName=NULL,nCovEval=30,otherCov=NULL, drawHistogram = TRUE){
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

  ctime<-trainModel$response[trainModel$response[,2]==trainModel$cause,1]
  ctime<-unique(ctime)
  ctime<-ctime[order(ctime)]
  if(ctime[1]!=0){
    ctime<-c(0,ctime)
  }
  pred <- predictRisk(object = trainModel, newdata =newData,times = ctime)
  if(is.null(contCovName)){
    contCovName<-contCov
  }
  histData<-data[,contCov]
  s<-survContour(ctime,newData[,contCov],pred,histData,contCovName,competingIndi=TRUE, drawHistogram = drawHistogram)
  s
}

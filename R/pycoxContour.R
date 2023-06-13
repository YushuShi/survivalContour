#' @importFrom stats quantile
#' @import reticulate
#' @import survivalmodels
pycoxContour<-function(data,trainModel,contCov,contCovName=NULL,nCovEval=30,otherCov=NULL){
  if(is.null(otherCov)){
    newData<-data.frame(data[1,])
    for(i in 1:ncol(data)){
      newData[,i]<-ifelse(is.numeric(data[,i])|is.integer(data[,i]),mean(data[,i]),names(which.max(table(data[,i])))[1])
    }
  }else{
    newData<-otherCov
  }

  data2<-data.frame(rbind(data,newData))
  newData2<-data.frame(data2[nrow(data2),])
  newData2<-newData2[rep(1,nCovEval),]
  newData2[,contCov]<-quantile(data[,contCov],0.025)+(quantile(data[,contCov],0.975)-quantile(data[,contCov],0.025))*(0:(nCovEval-1))/(nCovEval-1)
  predictOutcome<-predict(trainModel,newData2,type="survival")
  if(is.null(contCovName)){
    contCovName<-contCov
  }
  histData<-data[,contCov]
  s<-survContour(as.numeric(colnames(predictOutcome)),
                 newData2[,contCov],
                 predictOutcome,histData,contCovName)

  s
}

#' @importFrom stats quantile
#' @import flexsurv
#' @importFrom rlist list.stack

paraContour<-function(data,trainModel,contCov,contCovName=NULL,nCovEval=30,otherCov=NULL){
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

  time<-trainModel$data$Y[trainModel$data$Y[,"status"]>0,"time"]
  time<-unique(time)
  time<-time[order(time)]
  if(time[1]>0){
    time<-c(0,time)
  }
  predictPlot0<-predict(trainModel,newdata = newData,
                        type="survival",conf.int=TRUE,times=time)

  predictPlot<-list.stack(predictPlot0$.pred)
  z<-matrix(predictPlot$.pred_survival,ncol=length(time),byrow = TRUE)
  #z<-cbind(rep(1,nrow(z)),z)

  if(is.null(contCovName)){
    contCovName<-contCov
  }
  histData<-data[,contCov]
  s<-survContour(time,newData[,contCov],z,histData,contCovName)
  s
}

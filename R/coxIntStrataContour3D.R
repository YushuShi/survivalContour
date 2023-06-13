#' @importFrom stats quantile
coxIntStrataContour3D<-function(data,trainModel,contCov,contCovName=NULL,nCovEval=30,otherCov=NULL,strataName=NULL,CI3D=FALSE){
  strataVar<-gsub(".*\\((.*)\\).*", "\\1", trainModel$strata.name)
  strataList<-unique(data[,strataVar])
  plotList<-NULL
  if(is.null(contCovName)){
    contCovName<-contCov
  }
  if(is.null(strataName)){
    strataName<-strataVar
  }
  predictOutcome<-NULL
  contVar<-quantile(data[,contCov],0.025)+(quantile(data[,contCov],0.975)-quantile(data[,contCov],0.025))*(0:(nCovEval-1))/(nCovEval-1)
  for(i in 1:length(strataList)){

    if(is.null(otherCov)){
      newData<-data.frame(data[1,])
      for(j in 1:ncol(data)){
        newData[,j]<-ifelse(is.numeric(data[,j])|is.integer(data[,j]),mean(data[,j]),
                            names(which.max(table(data[,j])))[1])
      }
    }else{
      newData<-otherCov
    }

    data2<-data.frame(rbind(data,newData))
    newData<-data.frame(data2[nrow(data2),])
    newData<-newData[rep(1,nCovEval),]
    newData[,contCov]<-contVar
    newData[,strataVar]<-rep(strataList[i],nCovEval)

    ctime<-as.numeric(unique(trainModel$time))
    if(ctime[1]>0){
      ctime<-c(0,ctime)
    }

    temp<-predictPhreg(trainModel,newdata = newData,times=ctime)
    predictOutcome$time[[i]]<-ctime
    predictOutcome$surv[[i]]<-temp$surv
    predictOutcome$lower[[i]]<-temp$lower
    predictOutcome$upper[[i]]<-temp$upper

  }
  s<-contour3D("spaceHolder",contVar,predictOutcome,contCovName,CI3D,FALSE,TRUE,
               strataName,strataList)
  s
}

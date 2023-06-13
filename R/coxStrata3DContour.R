#' @importFrom stats quantile

coxStrata3DContour<-function(data,trainModel,contCov,contCovName=NULL,nCovEval=30,otherCov=NULL,strataName=NULL,CI3D=FALSE){
  strataVar<-names(trainModel$xlevels)[1]
  strataList<-trainModel$xlevels[[1]]
  plotList<-NULL
  if(is.null(contCovName)){
    contCovName<-contCov
  }
  if(is.null(strataName)){
    strataName<-strataVar
  }
  predictOutcome<-NULL
  if(is.null(otherCov)){
    newData<-data.frame(data[1,])
    for(j in 1:ncol(data)){
      newData[,j]<-ifelse(is.numeric(data[,j])|is.integer(data[,j]),mean(data[,j]),
                          names(which.max(table(data[,j])))[1])
    }
  }else{
    newData<-otherCov
  }
  contVar<-quantile(data[,contCov],0.025)+(quantile(data[,contCov],0.975)-quantile(data[,contCov],0.025))*(0:(nCovEval-1))/(nCovEval-1)
  for(i in 1:length(strataList)){
    data2<-data.frame(rbind(data,newData))
    newData2<-data.frame(data2[nrow(data2),])
    newData2<-newData2[rep(1,nCovEval),]
    newData2[,contCov]<-contVar
    newData2[,strataVar]<-rep(strataList[i],nCovEval)
    temp<-survival::survfit(trainModel,newData2)
    predictOutcome$time[[i]]<-c(0,temp$time)
    predictOutcome$surv[[i]]<-cbind(rep(1,nCovEval),matrix(temp$surv,byrow = TRUE,nrow=nCovEval))
    predictOutcome$lower[[i]]<-cbind(rep(1,nCovEval),matrix(temp$lower,byrow = TRUE,nrow=nCovEval))
    predictOutcome$upper[[i]]<-cbind(rep(1,nCovEval),matrix(temp$upper,byrow = TRUE,nrow=nCovEval))
  }

  s<-contour3D("spaceHolder",contVar,predictOutcome,contCovName,CI3D,FALSE,TRUE,
               strataName,strataList)
  s
}

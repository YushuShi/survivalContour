#' @importFrom stats quantile
#' @importFrom plotly '%>%' hide_colorbar plot_ly add_annotations subplot
coxIntStrataContour<-function(data,trainModel,contCov,contCovName=NULL,nCovEval=30,otherCov=NULL,strataName=NULL){
  strataVar<-gsub(".*\\((.*)\\).*", "\\1", trainModel$strata.name)
  strataList<-unique(data[,strataVar])
  plotList<-NULL
  if(is.null(contCovName)){
    contCovName<-contCov
  }
  if(is.null(strataName)){
    strataName<-strataVar
  }

  for(i in 1:length(strataList)){
    if(is.null(otherCov)){
      newData<-data.frame(data[1,])
      for(j in 1:ncol(data)){
        if(colnames(data)[j]!=strataVar){
          newData[,j]<-ifelse(is.numeric(data[,j])|is.integer(data[,j]),mean(data[,j]),
                              names(which.max(table(data[,j])))[1])
        }
      }
    }else{
      newData<-otherCov
    }
    data2<-data.frame(rbind(data,newData))
    newData<-data.frame(data2[nrow(data2),])
    newData<-newData[rep(1,nCovEval),]
    newData[,contCov]<-quantile(data[,contCov],0.025)+(quantile(data[,contCov],0.975)-quantile(data[,contCov],0.025))*(0:(nCovEval-1))/(nCovEval-1)
    newData[,strataVar]<-rep(strataList[i],nCovEval)

    ctime<-as.numeric(unique(trainModel$time))
    if(ctime[1]>0){
      ctime<-c(0,ctime)
    }

    predictPlot<-predictPhreg(trainModel,newdata = newData,times=ctime)

    temp<-contourPart(ctime,newData[,contCov],predictPlot$surv,contCovName)
    if(i>1){
      temp<-temp %>% hide_colorbar()
    }
    plotList[[2*i-1]]<-temp
    histData<-data[data[,strataVar]==strataList[i],contCov]
    temp2 <- plot_ly(y = histData, type = "histogram",hoverinfo='none',showlegend=FALSE)%>% layout(xaxis=list(title="Count"))
    temp2 <- temp2 %>% add_annotations(x=0.5,y=1.05,
                                       yref = "paper",
                                       xref = "paper",
                                       text=paste(strataName,strataList[i]),
                                       xanchor = "middle",
                                       yanchor = "top",
                                       showarrow=FALSE)
    plotList[[2*i]]<-temp2
  }

  s <- subplot(plotList,
               nrows = length(strataList),
               #ncol=2,
               widths = c(0.8, 0.2),
               margin=c(0,0.01,0.02,0),
               shareX=TRUE,
               shareY=TRUE,titleX = TRUE,titleY = TRUE)

  s

}

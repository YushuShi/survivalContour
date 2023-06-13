#' @importFrom plotly '%>%' layout hide_colorbar

contour3D<-function(x,y,z,contCovName,CI3D=FALSE,competingIndi=FALSE,strata=FALSE,strataName=NULL,strataList=NULL){
  if(!strata){
    if(CI3D){
      fig<-simple3D(x,y,z$surv,contCovName,z$upper,z$lower,competingIndi=competingIndi)
    }else{
      fig<-simple3D(x,y,z$surv,contCovName,competingIndi=competingIndi)
    }
    fig <- fig %>% layout(scene = list(yaxis = list(nticks = 8,
                                                    title=contCovName, range=range(y)),
                                       xaxis = list(nticks = 5,title="Time"),
                                       zaxis=list(title=ifelse(competingIndi,"Predicted CIF","Predicted Survival"))
    ))
  }else{
    fig<-NULL
    for(i in 1:length(z$time)){

        if(CI3D){
          fig[[i]]<-simple3D(z$time[[i]],y,z$surv[[i]],contCovName,z$upper[[i]],z$lower[[i]],competingIndi=competingIndi)
        }else{
          fig[[i]]<-simple3D(z$time[[i]],y,z$surv[[i]],contCovName,competingIndi=competingIndi)
        }

       fig[[i]] <- fig[[i]] %>% layout(scene = list(yaxis = list(nticks = 8,
                                                        title=contCovName, range=range(y)),
                                           xaxis = list(nticks = 5,title="Time"),
                                           zaxis=list(title=ifelse(competingIndi,"Predicted CIF","Predicted Survival")) ))

      fig[[i]]<-fig[[i]] %>% layout(annotations = list(
        #x = (i-0.5)/length(strataList),
        x=0,
        y = 0, text = paste(strataName, strataList[i]), showarrow = F, xref='paper', yref='paper')
      )

      if(i>1){
        fig[[i]]<-fig[[i]] %>% hide_colorbar()

      }
    }
  }
  fig
}

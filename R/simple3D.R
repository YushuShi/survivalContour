#' @importFrom plotly '%>%' layout add_surface

simple3D<-function(x,y,z,contCovName,upper=NULL,lower=NULL,competingIndi=FALSE){
fig <- plot_ly(type = 'surface',
               x = ~x,
               y = ~y,
               z = z,
               colorbar = list(title = ifelse(competingIndi,"Predicted CIF",                                                                       "Predicted survival probability"),
                               titleside='right'),
               hovertemplate = paste('At time %{x:.2f} <br>with',contCovName,
                                     'being %{y:.2f},<br>the predicted',
                                     ifelse(competingIndi,"CIF","survival") ,'is %{z:.2f}<extra></extra>'))
  if(!is.null(upper)){
    fig<-fig %>% add_surface(z =upper,opacity = 0.75,showscale=FALSE)
    fig<-fig %>% add_surface(z =lower,opacity = 0.75,showscale=FALSE)
  }
fig
}

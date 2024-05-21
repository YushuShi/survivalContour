#' @importFrom plotly '%>%' layout plot_ly

contourPart<-function(x,y,z,contCovName,competingIndi=FALSE){
  fig <- plot_ly(x=x,y=y,z=z,type= "contour",
                 colorbar = list(title =                                    ifelse(competingIndi,"Predicted CIF",                                                                              "Predicted survival probability"),
                                 titleside='right'),
                 hovertemplate = paste('At time %{x:.2f} <br>with',contCovName,
                                       'being %{y:.2f},<br>the predicted',
                                       'survival is %{z:.2f}<extra></extra>')
  )
  fig <- fig %>% layout(title=list(text=
                        paste("Contour Plot of the Predicted",
                        ifelse(competingIndi,"Cumulative Incidence Function","Survival Probability")),
                        font=list(size=20),
                        x=0.15),
                        xaxis=list(title="Time",
                                   font=list(size=24),
                                   range=c(0,max(x))),
                        yaxis=list(title=contCovName,
                                   font=list(size=24),
                                   range=range(y)))
  fig
}

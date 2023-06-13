#' @importFrom plotly subplot plot_ly

survContour<-function(x,y,z,histData,contCovName,competingIndi=FALSE){
fig<-contourPart(x,y,z,contCovName,competingIndi)
s <- subplot(fig,
             plot_ly(y = histData, type = "histogram",hoverinfo='none'),
             nrows = 1, widths = c(0.8, 0.2), margin = 0.01,
             shareY = TRUE,
             titleX=TRUE,
             titleY=TRUE)
s
}

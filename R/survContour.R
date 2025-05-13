#' @importFrom plotly subplot plot_ly

survContour<-function(x,y,z,histData,contCovName,competingIndi=FALSE){
fig<-contourPart(x,y,z,contCovName,competingIndi)
if (drawHistogram) {
  s <- subplot(
    fig,
    plot_ly(y = histData, type = "histogram", hoverinfo = 'none') %>% layout(xaxis = list(title = "Count")),
    nrows = 1, widths = c(0.8, 0.2), margin = 0.01,
    shareY = TRUE, titleX = TRUE, titleY = TRUE
  )
} else {
  s <- fig  # no hist
}
s <- s %>% layout(
  margin = list(t = 50,l=5) # Adjust top margin to ensure enough space for the title
)
s
}

#' Plot the estimated or derived value from a TMB model
#'
#' @param est estimated values
#' @param data available data
#' @param name name to print in data frame
#'
#' @return
#' @export
#'
#' @examples
#'
plotValues <- function(est, data, name){

df.plot <- est
df.plot$model <- 'estimated'

data.tmp <- data.frame(value = data$y, SE = NA, min = NA, max = NA, year = data$x, model = 'observed')

df.plot <- rbind(df.plot, data.tmp)

p1 <- ggplot2::ggplot(df.plot, ggplot2::aes(x = year, y = value, color = model))+
  geom_line(data = df.plot[df.plot$model == 'estimated',], size =1, col = 'black')+
  geom_point(data = df.plot[df.plot$model == 'observed',], size =2 )+ theme_bw()+
  geom_ribbon(data = df.plot[df.plot$model == 'estimated',],aes(ymin=min, ymax=max), alpha=0.2, linetype = 0)+
  scale_y_continuous(name)+
  theme(legend.position="none")

p1
return(p1)
}

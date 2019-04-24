plotUncertainty <- function(est, data){

yl = ylimits(est$name, data[is.na(data) == 0])
year <- est$year

plot(year,est$name, ylim = yl, type = 'l', col = 'red', lwd = 2,
     ylab = '', xlab = '')
points(year,data, lwd =2)
polygon(c(year, rev(year)), c(est$max, rev(est$min)), 
        border = NA, col = "#FF000050")


}
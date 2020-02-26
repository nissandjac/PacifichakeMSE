plot_hake_objectives <- function(ls,sim.data, plotfolder = '/Figs/newplots/', plotexp = TRUE){
 
  nms <- names(ls)  
  obj.plot <- list()
  cols <- brewer.pal(6, 'Dark2')
  
  
  for(i in 1:length(nms)){
    obj.plot[[i]] <- hake_objectives(ls[[i]],sim.data$SSB0, move = 1)
    obj.plot[[i]][[2]]$HCR <- nms[i]
  }
  
  
  df.obj <- data.frame(obj.plot[[1]][[2]])
  
  
  for(i in 2:length(nms)){
    df.obj <- rbind(df.obj, obj.plot[[i]][[2]])
  }
  
  indicators <- unique(df.obj$indicator)
 
  pl <- list()
  
  for(i in 1:length(indicators)){
    
    tmp  <- df.obj[df.obj$indicator == indicators[i],]
    ytmp <- range(tmp$value)
    
    pl[[i]] <- ggplot(tmp, aes(x = HCR,y = value))+
      geom_bar(stat = 'identity', aes(fill = HCR))+
      scale_x_discrete(name = '', labels = c('1','2','3'))+  
      scale_y_continuous(name = '')+
      scale_fill_manual(values = cols[1:length(unique(df.obj$HCR))])+
      ggtitle(as.character(indicators[i]))+
      coord_cartesian(ylim = c(ytmp[1]*0.8, ytmp[2]*1.05))+
      theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5), legend.position = 'none')
    
  }
p1 <- plot_grid(plotlist = pl, ncol = 3)
   
if(plotexp == TRUE){
  png(paste(plotfolder,'objective_bars_presentation.png'), width = 20, height =16, res = 400, unit = 'cm')
  print(p1)
  dev.off()
}  




}
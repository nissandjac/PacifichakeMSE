plotViolin <-function(data){
  
  library(ggplot2)
  library(cowplot)
  
  ### load the data 
  load(data)
  df <- obj.plot.v # 
  
  
  cols <- PNWColors::pnw_palette('Starfish',n = length(unique(df$HCR)), type = 'discrete')
  
  # Remove the 5th and 5th percentiles from df
  vars <- unique(df$variable)

  for(i in 1:length(vars)){
    idxtmp <- which(df$variable == vars[i])
    valstmp <- quantile(df$value[idxtmp], probs = c(0.01,0.99))

    rmtmp <- which(df$value[idxtmp] < valstmp[1] | df$value[idxtmp] > valstmp[2])

    df$value[idxtmp][rmtmp] <- NA
  }

  
  ## Do some adjustments to fix the scales 
  
  p.v <- ggplot(df, aes(x = HCR, y = value, fill = HCR))+
    geom_violin()+
    geom_boxplot(width=0.15, col = 'black', outlier.shape = NA)+
    scale_fill_manual(values = cols)+
    facet_wrap(~variable, scales = 'free_y', ncol = 3, dir='v')+
    theme(legend.position = 'none',
          axis.text.x = element_text(angle = 90, vjust = 0.5))+
    scale_x_discrete(name = '')+  
    scale_y_continuous(name = '')#+
  #coord_cartesian(ylim = c(0,1))
  print(p.v)
  
  return(p.v)
}
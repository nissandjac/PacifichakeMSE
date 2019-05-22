#### Run OM model 

## Test the assessment against perfect data 

# Run the hake assessment 
source('load_files.R')
source('load_files_OM.R')
df <- load_data_seasons(nseason = 4, nspace = 2)


sim.data <- run.agebased.true.catch(df)

df.plot <- data.frame(year= rep(df$years,2), SSB = c(sim.data$SSB[,1],sim.data$SSB[,2]), area = rep(c('1','2'), each = df$nyear))

ggplot(df.plot, aes(x = year, y= SSB, color = area))+geom_line()+theme_classic()

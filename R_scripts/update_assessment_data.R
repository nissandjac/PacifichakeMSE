### Update the conditioning data 
# Run the hake assessment 

source('load_files.R')
source('load_data_ss.R')
library(r4ss)
library(dplyr)
library(reshape2)
# Read the assessment data 
mod <- SS_output(paste(getwd(),'/data/SS32018/', sep =''), printstats=FALSE, verbose = FALSE)

df <- load_data_ss(mod)
df$smul <- 0.5

SSB.ss <- mod$derived_quants$Value[grep('SSB_1966', mod$derived_quants$Label):grep('SSB_2018', mod$derived_quants$Label)]
R.ss <- mod$derived_quants$Value[grep('Recr_1966', mod$derived_quants$Label):grep('Recr_2018', mod$derived_quants$Label)]
F0.ss <- df$F0

assessment.export <- data.frame(year = df$years, SSB = SSB.ss, R = R.ss, F0 = F0.ss)


age.ss <- mod$agedbase[mod$agedbase$Fleet == 1,]

years.catch <- unique(age.ss$Yr)
ages <- unique(age.ss$Bin)

age.ss.save <- data.frame(year = years.catch, nTrips = NA)
# Do some stupid magic 
tmp <- as.data.frame(matrix(NA, nrow = length(years.catch), ncol = length(ages)))
names(tmp) <- paste('a',ages, sep ='')
age.ss.save <- cbind(age.ss.save,tmp)

for(i in 1:length(years.catch)){
  
  tmp <- age.ss[age.ss$Yr == years.catch[i],]
  
  age.ss.save$nTrips[i] <- mean(tmp$N) # fix if different for some age classes
  age.ss.save[i,paste('a',ages, sep ='')] <- round(tmp$Obs, digits =3)
  print(sum(tmp$Obs))
}


rowSums(age.ss.save[,paste('a',ages, sep ='')])


write.csv(file ='age_save_2018.csv', age.ss.save, row.names = FALSE)




ac.ss3 <- as.data.frame(t(df$age_catch))
colnames(ac.ss3) <-paste(1:15)
ac.ss3$year <- df$years
ac.ss3.plot <- melt(ac.ss3, id.vars = 'year', variable.name = 'age')
ac.ss3.plot <- ac.ss3.plot[ac.ss3.plot$year %in% df$years[df$flag_catch == 1],]

yr <- 1975

df.N <- data.frame(N = c(as.numeric(N.ss[N.ss$Yr == yr,ix]), vars$N_beg[,which(df$years == yr)]),
                   model = c(rep('ss', df$nage),rep('tmb', df$nage)), age = rep(df$age,2))

ggplot(df.N, aes(x= age, y = N, color = model))+geom_line()+scale_y_log10()


ggplot(ac.tmb.plot[ac.tmb.plot$year == yr,],aes(x= as.numeric(age), y = value))+
  geom_line(col = 'red')+
  geom_point(data =age.ss.plot[age.ss.plot$year == yr,])#+


# Compare the total catch at age 
caa <- mod$catage[mod$catage$Era == 'TIME',]
yr <- 1966
ix <- 11:31


df.c<- data.frame(N = c(as.numeric(caa[caa$Yr == yr,ix]), vars$CatchNAge[,which(df$years == yr)]),
                  model = c(rep('ss', df$nage),rep('tmb', df$nage)), age = rep(df$age,2))


ggplot(df.c, aes(x= age, y = N, color = model))+geom_line()


ggplot(ac.tmb.plot[ac.tmb.plot$year == yr,],aes(x= as.numeric(age), y = value))+
  geom_line(col = 'red')+
  geom_point(data =age.ss.plot[age.ss.plot$year == yr,])#+
# Compare age distribution in survey
ac.tmb <- as.data.frame(t(vars$age_survey_est))
colnames(ac.tmb) <-paste(1:15)
ac.tmb$year <- df$years
ac.tmb.plot <- melt(ac.tmb, id.vars = 'year', variable.name = 'age')
ac.tmb.plot <- ac.tmb.plot[ac.tmb.plot$year %in% df$years[df$flag_survey == 1],]


age.sssurvey <- mod$agedbase[mod$agedbase$Fleet == 2 & mod$agedbase$Yr %in% df$years[df$flag_survey == 1],]
age.ss_survey.plot <- data.frame(year = age.sssurvey$Yr, value = age.sssurvey$Exp, age = age.sssurvey$Bin)


ggplot(ac.tmb.plot,aes(x= as.numeric(age), y = value))+geom_line(col = 'red')+geom_point(data =age.ss_survey.plot)+
  facet_wrap(~year)

ggplot(ac.tmb.plot[ac.tmb.plot$age == 1,], aes(x= year, y = value))
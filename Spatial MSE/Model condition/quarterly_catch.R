# read data file from 2011 model with a approximately monthly time steps
# (9 seasons total, with first 3 and last 2 months grouped together)
#
# files are available on Google Drive:
# https://drive.google.com/drive/folders/1tcN4hCaSjPJ5858b5dnEL_0cuSbyupuj
dat <- SS_readdat('C:/SS/hake/Hake_2011/Candidates_1_13_11_SSv3.24f/Catch_season_age/2011_hake_CSA_data.ss')

# define season groups used in 2011 model: JFM,A,M,J,J,A,S,O,ND
seas.group <- list(1,   #JFM   months  1, 2, 3
                   2:4, #A,M,J months  4, 5, 6
                   5:7, #J,A,S months  7, 8, 9
                   8:9) #O, ND months 10,11,12
# define season groups based on months
seas.group2 <- list(1:3, 
                    4:6, 
                    7:9, 
                    10:12)

# aggregate catch into 2 areas, 4 seasons by country
catch2a4s <- data.frame(CAN=0, US=0, expand.grid(seas=1:4, year=1966:2017)[,2:1])
# aggregate catch across seasons and fleets within each country
for(y in 1966:2010){
  for(s in 1:4){
    tmp.US <- dat$catch[dat$catch$year==y &
                       dat$catch$seas %in% seas.group[[s]], 1:4]
    tmp.CAN <- dat$catch[dat$catch$year==y &
                           dat$catch$seas %in% seas.group[[s]], 5:7]
    if(nrow(tmp.US) > 0){
      catch2a4s$US[catch2a4s$year==y & catch2a4s$seas==s] <- sum(tmp.US)
    }
    if(nrow(tmp.CAN) > 0){
      catch2a4s$CAN[catch2a4s$year==y & catch2a4s$seas==s] <- sum(tmp.CAN)
    }
  } # end seas loop
} # end year loop

# save backup to compare sources for overlapping years
catch2a4s.backup <- catch2a4s

#### get values from more recent years from hake-assessment git repository
data.dir <- "C:/github/hake-assessment/data"
CAN1 <- read.csv(file.path(data.dir, "can-ft-catch-by-month.csv"))
CAN2 <- read.csv(file.path(data.dir, "can-ss-catch-by-month.csv"))
US1 <- read.csv(file.path(data.dir, "us-cp-catch-by-month.csv"))
US2 <- read.csv(file.path(data.dir, "us-ms-catch-by-month.csv"))
US3 <- read.csv(file.path(data.dir, "us-shore-catch-by-month.csv"))
US4 <- read.csv(file.path(data.dir, "us-research-catch-by-month.csv"))

# combine data frames from different sectors within each country
CAN.all <- rbind(CAN1,CAN2)
US.all <- rbind(US1,US2,US3,US4)

#### add recent years to table for the years 2008
for(y in 2008:2017){
  for(s in 1:4){
    tmp.US <- US.all$catch[US.all$year==y &
                             US.all$month %in% seas.group2[[s]]]
    tmp.CAN <- CAN.all$catch[CAN.all$year==y &
                               CAN.all$month %in% seas.group2[[s]]]
    if(length(tmp.US) > 0){
      catch2a4s$US[catch2a4s$year==y & catch2a4s$seas==s] <- round(sum(tmp.US))
    }
    if(length(tmp.CAN) > 0){
      catch2a4s$CAN[catch2a4s$year==y & catch2a4s$seas==s] <- round(sum(tmp.CAN)/1000)
    }
  } # end seas loop
} # end year loop



# convert catches to fractions
catch2a4s.frac <- catch2a4s
for(y in 1966:2017){
  for(s in 1:4){
    sub1 <- catch2a4s.frac$year==y & catch2a4s.frac$seas==s
    sub2 <- catch2a4s.frac$year==y
    catch2a4s.frac$US[sub1] <- round(catch2a4s$US[sub1]/sum(catch2a4s$US[sub2]),3)
    catch2a4s.frac$CAN[sub1] <- round(catch2a4s$CAN[sub1]/sum(catch2a4s$CAN[sub2]),3)
  }
}

# put into matrix format 
catch.frac.mat.US <- matrix(data=NA, nrow=length(1966:2017), ncol=4,
                            dimnames=list(year=1966:2017, seas=1:4))
catch.frac.mat.CAN <- matrix(data=NA, nrow=length(1966:2017), ncol=4,
                            dimnames=list(year=1966:2017, seas=1:4))
for(y in 1966:2017){
  for(s in 1:4){
    sub1 <- catch2a4s.frac$year==y & catch2a4s.frac$seas==s
    catch.frac.mat.US[paste(y),paste(s)] <- catch2a4s.frac$US[sub1]
    catch.frac.mat.CAN[paste(y),paste(s)] <- catch2a4s.frac$CAN[sub1]
  }
}

# calculate 10-year average proportions (all years equally weighted)
CAN.10yr.avg <- round(apply(tail(catch.frac.mat.CAN,10), MARGIN=2, FUN=sum)/10,3)
US.10yr.avg <- round(apply(tail(catch.frac.mat.US,10), MARGIN=2, FUN=sum)/10,3)

CAN.10yr.avg
##     1     2     3     4 
## 0.001 0.188 0.603 0.208 
US.10yr.avg
##     1     2     3     4 
## 0.000 0.317 0.382 0.302

# add average to matrix
catch.frac.mat.CAN <- rbind(catch.frac.mat.CAN, rep(NA,4), avg.10yrs=CAN.10yr.avg)
catch.frac.mat.US <- rbind(catch.frac.mat.US, rep(NA,4), avg.10yrs=US.10yr.avg)

# make barplot
png('C:/SS/hake/MSE/quarterly_catch.png',
    res=300, units='in', width=7, height=7)
par(mfrow=c(2,1), mar=c(3,3,1,1))
colors <- c("blue","green3","yellow","orange")
barplot(t(catch.frac.mat.CAN[-(1:length(1966:1991)),]),
        main="Canada", col=colors, las=1)
axis(side=1, at=par()$usr[2]-1.5, labels="avg.\n'08-'17", tick=FALSE)
barplot(t(catch.frac.mat.US[-(1:length(1966:1991)),]),
        main="U.S.", col=colors, las=1)
axis(side=1, at=par()$usr[2]-1.5, labels="avg.\n'08-'17", tick=FALSE)
dev.off()

# export values
save(catch2a4s, catch2a4s.frac, catch.frac.mat.CAN, catch.frac.mat.CAN,
     CAN.10yr.avg, US.10yr.avg,
     file='C:/SS/hake/MSE/quarterly_catch.Rdata')

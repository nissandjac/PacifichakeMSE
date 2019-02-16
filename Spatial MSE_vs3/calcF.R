calcF <- function(method = 'hybrid', E.temp,B.tmp, N, weight, M, Fsel){
  
# Calculate Btmp  

  
  if(E.temp/B.tmp > 1){
    stop(paste('Catch exceeds available biomass in year:',df$years[yr],' and season', season, 'area', space))
  }
  
  
  
if(E.temp == 0){
  Fseason <- 0
}else{
  if(method == 'hybrid'){
    

    temp <- E.temp/(B.tmp + 0.1*E.temp)
    join <- (1+exp(30*(temp-0.95)))^-1
    temp2 <- join*temp+0.95*(1-join)
    Fout <- -log(1-temp)
    #Fout <- df$F0[yr]
    Fseason <- Fout*Fsel
    
    
    Fnew <- Fout
    for(i in 1:4){
      Z <- M+Fnew*Fsel
      Alpha <- (1-exp(-Z))
      Ctmp <- sum((Fnew/Z)*(N*weight*Fsel)*Alpha)
      
      Zadj <- E.temp/(Ctmp+0.0001)
      
      Zprime <- M+Zadj*(Z-M)
      Alpha <- (1-exp(-Zprime))/(Zprime)
      
      temp <- sum(N*weight*Fsel*Alpha)
      Ftmp <-  E.temp/(temp+0.0001)
      #print(Ftmp)
      j2 <- 1/(1+exp(30*(Ftmp-0.95*1)))
      
      Fnew <- j2*Ftmp+(1-j2)
      #N.tmp <- N.pop*exp(-(Fnew*Fsel+Mseason))
      
      if(i == 4){
        print(Ctmp/E.temp)
      }
    }

  }
  # if(method == 'optimization'){
  #   
  #   getF <- function(par,data){
  #     F0 <- par[1]
  #     sel <- data$Fsel
  #     Z <- M+Fsel
  #     
  #     Ndot <- 
  #     
  #     
  #   }
  #   
  #   
  #   
  #   
  # }
    
}
  
  
return(Fnew)
  
}
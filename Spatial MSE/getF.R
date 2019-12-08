getF <- function(E.temp,B.tmp, Mseason, Fsel, N.tmp, w_catch = w_catch,method = 'Pope'){

# Calculate fishing mortality based on catch
#' @E.temp  catch
#' @B.tmp  Vulnerable biomass 
#' @Mseason Natural mortality
#' @Fsel Selectivity
#' @Ntmp Numbers at age
#' @w_catch Weight at age 
#' @method Pope or Hybrid 
  
  
  
if(E.temp > 0){
  
  
  temp <- E.temp/(B.tmp + 0.1*E.temp)
  join <- (1+exp(30*(temp-0.95)))^-1
  temp2 <- join*temp+0.95*(1-join)
  Fnew <- -log(1-temp2)
  #Fout <- df$F0[yr]
  if (method == 'Hybrid'){
    for(i in 1:4){
      Z <- Mseason+Fnew*Fsel
      Alpha <- (1-exp(-Z))
      Ctmp <- sum((Fnew/Z)*(N.tmp*w_catch*Fsel)*Alpha)
      
      Zadj <- E.temp/(Ctmp+0.0001)
      
      Zprime <- Mseason+Zadj*(Z-Mseason)
      Alpha <- (1-exp(-Zprime))/(Zprime)
      
      temp <- sum(N.tmp*w_catch*Fsel*Alpha)
      Ftmp <-  E.temp/(temp+0.0001)
      #          print(Ftmp)
      j2 <- 1/(1+exp(30*(Ftmp-0.95*1)))
      
      Fnew <- j2*Ftmp+(1-j2)
    }
  }
  
  }else{
  Fnew <- 0
  }
  

  
  
  return(Fnew)
}
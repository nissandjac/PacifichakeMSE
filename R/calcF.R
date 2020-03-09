#' Title
#'
#' @param Fmethod hybrid only viable option right now
#' @param Etemp Catch input
#' @param Btmp estimated available biomass
#' @param N numbers at age
#' @param weight weight at age
#' @param M natural mortality
#' @param Fsel fishing selectivity
#'
#' @return
#' @export
#'
#' @examples
#' calcF(Fmethod = 'hybrid') # Not a working example
calcF <- function(Fmethod = 'hybrid', Etemp,Btmp, N, weight, M, Fsel){

  if(Etemp/Btmp > 1){
    stop(paste('Catch exceeds available biomass in year:',df$years[yr],' and season', season, 'area', space))
  }



if(Etemp == 0){
  Fseason <- 0
}else{
  if(Fmethod == 'hybrid'){


    temp <- Etemp/(Btmp + 0.1*Etemp)
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

      Zadj <- Etemp/(Ctmp+0.0001)

      Zprime <- M+Zadj*(Z-M)
      Alpha <- (1-exp(-Zprime))/(Zprime)

      temp <- sum(N*weight*Fsel*Alpha)
      Ftmp <-  Etemp/(temp+0.0001)
      #print(Ftmp)
      j2 <- 1/(1+exp(30*(Ftmp-0.95*1)))

      Fnew <- j2*Ftmp+(1-j2)
      #N.tmp <- N.pop*exp(-(Fnew*Fsel+Mseason))

      if(i == 4){
        print(Ctmp/Etemp)
      }
    }

  }
  # if(Fmethod == 'optimization'){
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

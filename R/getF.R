#' Calculate the fishing mortality based on catch
#'
#' @param Etemp
#' @param Btemp
#' @param Mseason
#' @param Fsel
#' @param Ntemp
#' @param w_catch
#' @param method
#'
#' @return
#' @export
#'
#' @examples
getF <- function(Etemp,Btemp, Mseason, Fsel, Ntemp, w_catch = w_catch,method = 'Pope'){


if(Etemp > 0){


  temp <- Etemp/(Btemp + 0.1*Etemp)
  join <- (1+exp(30*(temp-0.95)))^-1
  temp2 <- join*temp+0.95*(1-join)
  Fnew <- -log(1-temp2)
  #Fout <- df$F0[yr]
  if (method == 'Hybrid'){
    for(i in 1:4){
      Z <- Mseason+Fnew*Fsel
      Alpha <- (1-exp(-Z))
      Ctmp <- sum((Fnew/Z)*(Ntemp*w_catch*Fsel)*Alpha)

      Zadj <- Etemp/(Ctmp+0.0001)

      Zprime <- Mseason+Zadj*(Z-Mseason)
      Alpha <- (1-exp(-Zprime))/(Zprime)

      temp <- sum(Ntemp*w_catch*Fsel*Alpha)
      Ftmp <-  Etemp/(temp+0.0001)
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

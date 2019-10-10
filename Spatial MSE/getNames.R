getNames <- function(df.out, df){
  x <- names(df.out)
  print(x)
  dim(df.out$N.save)
  years_1 <- c(df$years,max(df$years)+1)

  ls <- list()
  for(i in 1:length(df.out)){
  
    ls[[i]] <- dim(df.out[[i]])
    
  }
  
  
      # 
    # with(df.out,
    #      dimnames(N.save) <- list(age = df$age,
    #                               year = years_1),
    #      dimnames(SSB) <- list(year = df$years,
    #                            space = 1:df$nspace),
    #      dimnames(N.save.age) <- list(age = df$age,
    #                                   year = years_1,
    #                                   space = 1:df$nspace,
    #                                   season = 1:df$nseason),
    #      dimnames(V.save) <- list()
    #        
    #      )
         
  
  
}
ylimits <- function(vec1, vec2){
  
  mins <- min(c(vec1, vec2))
  maxs <- max(c(vec1, vec2))
  
  yl <- c(mins, maxs)
  
  return(yl)
}
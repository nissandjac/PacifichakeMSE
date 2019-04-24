ylimits <- function(vec1, vec2){
  
  mins <- min(c(vec1, vec2), na.rm =T)
  maxs <- max(c(vec1, vec2), na.rm =T)
  
  yl <- c(mins, maxs)
  
  return(yl)
}
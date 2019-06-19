cc <- function(x){
    n = length(x)
    ans <- matrix(NA, n)
    ans[1] = x[1];
    for (i in 2:n) ans[i] = x[i] + ans[i-1];
    return(ans);
  }

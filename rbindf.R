rbindf <- function(...) {
  
  l <- list(...)
  if(length(l) == 1) l <- l[[1]]
  nn <- length(l)
  
  x <- l[[1]]
  if(length(l)>1){
    for(i in 2:nn) {
      y <- l[[i]]
      if(nrow(x) > 0 & nrow(y) > 0) {
        if(!all(yinx <- names(y) %in% names(x))) {
          x[, names(y)[!yinx]] <- NA
        } 
        if(!all(xiny <- names(x) %in% names(y))) {
          y[, names(x)[!xiny]] <- NA
        } 
      }
      x <- rbind(x, y)
    }
  }
  return(x)
}
#functions for the other scripts

majority = function(x){
  y <- names(sort(summary(as.factor(x)), decreasing=TRUE)[2])
  if(!is.na(y) & y[1] == "NA's"){
    return(as.numeric(y[2]))
  }else{
    return(as.numeric(y[1]))
  }
}

dslr = function(X, start){
  last <- start
  init <- T
  DSLR <- rep(NA, nrow(X))
  
  for(i in 1:nrow(X)){
    if(!is.na(X$precip_total[i]) & X$precip_total[i] > 0){
      last <- i
      DSLR[i] <- 0
    }else if(!is.na(X$precip[i]) & X$precip[i] > 0){
      last <- i
      DSLR[i] <- 0
    }else{
      if(init){
        DSLR[i] <- last + i
        init <- FALSE
      }else{
        DSLR[i] <- i-last
      }
    }
  }
  X <- cbind(X, DSLR)
  return(X)
}


last_week_temp = function(X){
  HPW1 <- rep(NA, nrow(X)) #HeatPeriodWeek1 (before current day)
  HPW2 <- rep(NA, nrow(X))
  HPW3 <- rep(NA, nrow(X))
  
  for(i in 8:nrow(X)){ #first week has none of these variables
    if(i<=14){
      HPW1[i] <- sum(X$tavg[(i-7):(i-1)])/7
    }else if(i<=21){
      HPW1[i] <- sum(X$tavg[(i-7):(i-1)])/7
      HPW2[i] <- sum(X$tavg[(i-14):(i-8)])/7
    }else{
      HPW1[i] <- sum(X$tavg[(i-7):(i-1)])/7
      HPW2[i] <- sum(X$tavg[(i-14):(i-8)])/7
      HPW3[i] <- sum(X$tavg[(i-21):(i-15)])/7
    }
  }
  X <- cbind(X, HPW1, HPW2, HPW3)
  return(X)
}

###

maxRows <- 3100
strategyMatrix <- matrix(ncol = 10)
runningDays <- 1000
date <- vector()

cciOverSold <- -100
cciOverBought <- 100

getOrders <- function(store,newRowList,currentPos,info,params) {
  
  allzero  <- rep(0,length(newRowList)) 
  currentPosition <- vector()
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- allzero; pos <- allzero
  
  if (store$iter > params$cciLookback) {
    
    startIndex <-  store$iter - params$cciLookback
    
    # Position Sizing
    maxCl <- 0
    for (i in 1:length(params$series)){
      maxCl <- max(maxCl,newRowList[[params$series[i]]]$Close)
    }
    
    for (i in 1:length(params$series)) {
      
      cl <- newRowList[[params$series[i]]]$Close
      
      cciList <- CCI(store$cl[startIndex:store$iter,i],
                     n=params$cciLookback,c=params$cciMeanDev)
      cci <- last(cciList)
      cciYesterday <- cciList[nrow(cciList)-1,]
      
      if (cci < cciOverSold && !is.na(cci) && !is.na(cciYesterday)) {
        # pos[params$series[i]] <- abs(round(cci-cciYesterday))
        pos[params$series[i]] <- 1*(maxCl/last(cl))*
          (abs(round(cci-cciYesterday)))/last(cl)
      }

      else if (cci > cciOverBought && !is.na(cci) && !is.na(cciYesterday)) {
        # pos[params$series[i]] <- -abs(round(cci-cciYesterday))
        pos[params$series[i]] <- -1*(maxCl/last(cl))*
          (abs(round(cci-cciYesterday)))/last(cl)
      }
      
      # For visualizing strategy operations
      currentPosition <- append(currentPosition,
                                currentPos[params$series[i]])
    }
    
    date <<- append(date,index(newRowList[[1]]))
    strategyMatrix <<- rbind(strategyMatrix,currentPosition)
    
    if(store$iter==runningDays-2){
      strategyMatrix <- strategyMatrix[-1,]
      for(i in 1:length(params$series)){
        png(paste("Graph", toString(i), ".png"),
            width = 1920, height = 1080, units = "px")
        matplot(date,strategyMatrix[,i], 
                ylab="Current Position", type='l')
        dev.off()
      }
    }
  }
  
  marketOrders <- marketOrders + pos
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,
              limitPrices1=allzero,
              limitOrders2=allzero,
              limitPrices2=allzero))
}

initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  return(store)
}
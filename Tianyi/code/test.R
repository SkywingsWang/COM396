# The strategy will be long (short) whenever 

maxRows <- 3100

cciOverSold <- -100
cciOverBought <- 100
# rsiOverSold <- 30

getOrders <- function(store,newRowList,currentPos,info,params) {
  
  allzero  <- rep(0,length(newRowList)) 
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- -currentPos; pos <- allzero
  
  if (store$iter > params$lookback) {
    
    startIndex <-  store$iter - params$lookback
    
    for (i in 1:length(params$series)) {
      
      cl <<- newRowList[[params$series[i]]]$Close
      cci <- last(CCI(store$cl[startIndex:store$iter,i],
                            n=params$lookback,c=params$cciMeanDev))
      
      # rsi <- last(RSI(store$cl[startIndex:store$iter,i], 
      #                 n=14, maType=list(maUp=list(EMA),maDown=list(WMA))))
      
      if (cci < cciOverSold && !is.na(cci)) {
        # if the cci value is below -100, we take long position
        pos[params$series[i]] <- 1
      }
      else if (cci > cciOverBought && !is.na(cci)) {
        pos[params$series[i]] <- -1
      }
      
      # if (rsi < rsiOverSold && !is.na(rsi)){
      #   pos[params$series[i]] <- 1
      # }
      
      # stop loss
      # if () {
      #   pos[params$series[i]] <- 0
      # }
      
    }
  }
  
  if (store$iter > params$macdLookback) {
    
    startIndex <-  store$iter - params$macdLookback
    
    for (i in 1:length(params$series)) {
      
      macd <- last(MACD(store$cl[startIndex:store$iter,i],
                        nFast=params$macdFast, nSlow=params$macdSlow,
                        maType=params$macdMa, percent=TRUE))
     
      if (macd[,"signal"] > macd[,"macd"]) {
        pos[params$series[i]] <- 0
      } 
    }
  }
  
  pos <- pos #check the position sizes
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
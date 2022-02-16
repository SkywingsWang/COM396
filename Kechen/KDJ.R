library(TTR)
maxRows <- 3100


getOrders <- function(store,newRowList,currentPos,info,params) {
  
  allzero  <- rep(0,length(newRowList)) 
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- allzero; 
  pos <- allzero
  limitOrders1=allzero;
  limitPrices1=allzero;
  limitOrders2=allzero;
  limitPrices2=allzero;

  if (store$iter > params$smaLookback) {
    
    startIndex <-  store$iter - params$kdjLookback
    startIndexSMA <-  store$iter - params$smaLookback
    
    for (i in 1:length(params$series)) {
      
      cl <- newRowList[[params$series[i]]]$Close

      #calculate the index
     # print(stoch(store$cl[startIndex:store$iter,i],nFastK = params$nFastK,
     # nFastD = params$nFastD, nSlowD = params$nSlowD, bounded = TRUE,smooth = 1))
      KD <- last(stoch(store$cl[startIndex:store$iter,i],
                      nFastK = params$nFastK, nFastD = params$nFastD, 
                      nSlowD = params$nSlowD, bounded = TRUE,smooth = 1))
      
      #compute the yesterday KD
      KD0<- last(stoch(store$cl[startIndex:store$iter,i],
                 nFastK = params$nFastK, nFastD = params$nFastD, 
                 nSlowD = params$nSlowD, bounded = TRUE,smooth = 1)[-21,])
      
      Jline <- 3*KD[,'fastK']- 2*KD[,'fastD'] 
      Jline0<- 3*KD0[,'fastK']- 2*KD0[,'fastD'] 
      
      sma <- last(SMA(store$cl[startIndexSMA:store$iter,i],params$smaLookback))

      
      Psizing<- 0 
      stoploss<- NA
      
      #make the decision
      if (Jline0 < 0.2 && Jline > 0.2 && !is.na(Jline)) {
        Psizing<- (Jline-0.2)*0.6+1*0.4
        pos[params$series[i]] <- 10*Psizing
        stoploss<- as.numeric(newRowList[[params$series[i]]]$Low)
        
        limitOrders1<- limitOrders1 + -pos[params$series[i]]
        limitPrices1<- limitPrices1 + sma
        
        limitOrders2<- limitOrders2 + -pos[params$series[i]]
        limitPrices2<- limitPrices2 + stoploss
        
      }else if (Jline0 > 0.80 && Jline < 0.8 &&!is.na(Jline)) {
        #dnorm(0.8, mean = 1, sd = 0.1)
        Psizing<- (0.8-Jline)*0.6+1*0.4
        pos[params$series[i]] <- -10*Psizing
        stoploss<- as.numeric(newRowList[[params$series[i]]]$High)
       
        #price hit the ma80 then sell
        limitOrders1<- limitOrders1 + pos[params$series[i]]
        limitPrices1<- limitPrices1 + sma 
        print(class(limitPrices1))
        print(limitPrices1)
        
        #set the stop loss at the Highest price yesterday
        limitOrders2<- limitOrders2 + pos[params$series[i]]
        limitPrices2<-  limitPrices2 + stoploss
        
      }else{
        pos[params$series[i]] <- 0
      }
      
    }
  }    
  pos <- pos #check the position sizes
  marketOrders <- marketOrders + pos
  
  
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,
              limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,
              limitPrices2=limitPrices2))
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

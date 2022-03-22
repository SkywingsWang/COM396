params <- list(series=1:10,
               cciLookback=20, cciMeanDev=0.015, 
               cciOverSold=-130, cciOverBought=130,
               BBLookback=50,BBsd=5,
               kdjLookback=20,nFastK=14,nFastD=3,
               nSlowD=5,Jhigh=0.8,Jlow=0.2,
               DCLookback=19,maLookback=3)

maxRows <- 11000

getOrders <- function(store,newRowList,currentPos,info,params) {
  
  allzero  <- rep(0,length(newRowList)) 
  
  # initialize store at the beginning
  if (is.null(store)){ 
    store <- initStore(newRowList,params$series)    
  }
  
  # update store each transaction day
  store <- updateStore(store, newRowList, params$series)
  
  # initialize market orders and limit orders
  marketOrders <- allzero
  limitOrders <- allzero; limitPos <- allzero; limitPrice <- allzero
  
  # initialize cci-based strategy's market position
  cciPos <- allzero
  # in order to stop all of the cci-based strategy's position
  cciAccumulatePosition <- store$cciAccumulatePosition
  
  momentumPos <- allzero
  momentumLastTran <- store$momentumPos
  
  dcPos <- allzero
  dcLastTran <- store$dcPos
  dcCoefficient <- 50000
  
  if(store$iter > params$DCLookback && store$iter <= 225) {
    
    startIndex <-  store$iter - params$DCLookback
    maxCl <- 0
    
    for (i in 1:length(params$series)){
      maxCl <- max(maxCl,newRowList[[params$series[i]]]$Close)
    }
    
    for (i in 1:length(params$series)) {
      
      cl <- newRowList[[params$series[i]]]$Close
      Merge <- cbind(store$high[startIndex:store$iter,i],store$low[startIndex:store$iter,i],store$cl[startIndex:store$iter,i])
      Merge <- as.data.frame(Merge)
      
      colnames(Merge)[1] <- "High"
      colnames(Merge)[2] <- "Low"
      colnames(Merge)[3] <- "Close"
      
      dc <- last(DonchianChannel(Merge[,c("High","Low")],n=params$DCLookback,include.lag = TRUE))
      movingAverage <- last(SMA(Merge[,c("Close")],n=params$maLookback))
      closePrice <-Merge[,c("Close")]
      
      if (movingAverage< (dc[,3])) {
        #if the moving average is lower than the Donchian Channel Low-bound, long the position
        #replace the first if condition for movingAverage
        ##### if the lowest price of the day is smaller than the Donchian Channel Low-bound, long the position
        
        dcPos[params$series[i]] <- dcCoefficient*(maxCl/cl)*(dc[,3]-movingAverage)/cl
        if (dcPos[params$series[i]]*cl>200000){
          dcPos[params$series[i]] <- 200000/cl
        }
        if(dcLastTran[params$series[i]] != 0) {
          dcLastTran[params$series[i]] <- dcLastTran[params$series[i]] + dcPos[params$series[i]]
          
        } else{
          dcLastTran[params$series[i]] <- dcPos[params$series[i]]
          
        }
      }
      else if (movingAverage > (dc[,1])) {
        #if the moving average is bigger than the Donchian Channel High-bound, short the position
        #replace the first if condition for movingAverage
        ##### if the highest price of the day is bigger than the Donchian Channel High-bound, short the position
        dcPos[params$series[i]] <- -dcCoefficient*(maxCl/cl)*(movingAverage-dc[,1])/cl
        if (dcPos[params$series[i]]*cl< -200000){
          dcPos[params$series[i]] <- -200000/cl
        }
        if(dcLastTran[params$series[i]] != 0) {
          dcLastTran[params$series[i]] <- dcLastTran[params$series[i]] + dcPos[params$series[i]]
          
        } else{
          dcLastTran[params$series[i]] <- dcPos[params$series[i]]
          
        }
      }
      else{
        dcPos[params$series[i]] = -dcLastTran[params$series[i]]
        dcLastTran[params$series[i]] <- 0
        
      }
    }
  }
  
  if(store$iter > 225) {
    
    maxCl <- 0
    
    for (i in 1:length(params$series)){
      maxCl <- max(maxCl,newRowList[[params$series[i]]]$Close)
    }
    
    corr <- store$cor
    
    for (i in 1:length(params$series)) {
      
      startIndex <-  store$iter - params$DCLookback
      startDay = 30*(store$iter%/%30)-90
      endDay = 30*(store$iter%/%30)
      
      if(store$iter %% 30 == 1) {
        if(corr[params$series[i]] > 0.2){
          if(momentumLastTran[params$series[i]] != 0) {
            momentumPos[params$series[i]] <- -momentumLastTran[params$series[i]]
            momentumLastTran[params$series[i]] <- 0
          }
        }
        corr[params$series[i]] = 0
        lookback_return <- c()
        holddays_return <- c()
        
        for (k in 1:90) {
          lookback_return <- c(lookback_return, store$cl[store$iter+k-(225-90+2),i]-store$cl[store$iter+k-225-1,i])
          holddays_return <- c(holddays_return, store$cl[store$iter+k-90-2,i]- store$cl[store$iter+k-(225-90+1),i])
        }
        
        corr[params$series[i]] = cor(lookback_return,holddays_return)
        
        if(cor(lookback_return,holddays_return)>=0.2) {
          if(i != 3){
            if (store$cl[store$iter-1,i] - store$cl[store$iter-90,i] > 0) {
              limitPrice[params$series[i]] <-newRowList[[params$series[i]]]$Close
              limitPos[params$series[i]] <- 100000 %/% newRowList[[params$series[i]]]$Close
              momentumLastTran[params$series[i]] <- limitPos[params$series[i]]
            }
            # if the past lookback period saw a negative return, short
            else {
              limitPrice[params$series[i]] <- newRowList[[params$series[i]]]$Close
              limitPos[params$series[i]] <- -(100000 %/% newRowList[[params$series[i]]]$Close)
              momentumLastTran[params$series[i]] <- limitPos[params$series[i]]
            }
          }
        }
      }
      else if(corr[i]< 0.2) {
        cl <- newRowList[[params$series[i]]]$Close
        Merge <- cbind(store$high[startIndex:store$iter,i],store$low[startIndex:store$iter,i],store$cl[startIndex:store$iter,i])
        Merge <- as.data.frame(Merge)
        
        colnames(Merge)[1] <- "High"
        colnames(Merge)[2] <- "Low"
        colnames(Merge)[3] <- "Close"
        
        dc <- last(DonchianChannel(Merge[,c("High","Low")],n=params$DCLookback,include.lag = TRUE))
        movingAverage <- last(SMA(Merge[,c("Close")],n=params$maLookback))
        closePrice <-Merge[,c("Close")]
        
        if (movingAverage< (dc[,3])) {
          #if the moving average is lower than the Donchian Channel Low-bound, long the position
          #replace the first if condition for movingAverage
          ##### if the lowest price of the day is smaller than the Donchian Channel Low-bound, long the position
          dcPos[params$series[i]] <- dcCoefficient*(maxCl/cl)*(dc[,3]-movingAverage)/cl
          if (dcPos[params$series[i]]*cl>200000){
            dcPos[params$series[i]] <- 200000/cl
          }
          if(dcLastTran[params$series[i]] != 0) {
            dcLastTran[params$series[i]] <- dcLastTran[params$series[i]] + dcPos[params$series[i]]
            
          } else{
            dcLastTran[params$series[i]] <- dcPos[params$series[i]]
            
          }
        }
        
        else if (movingAverage > (dc[,1])) {
          #if the moving average is bigger than the Donchian Channel High-bound, short the position
          #replace the first if condition for movingAverage
          ##### if the highest price of the day is bigger than the Donchian Channel High-bound, short the position
          dcPos[params$series[i]] <- -dcCoefficient*(maxCl/cl)*(movingAverage-dc[,1])/cl
          if (dcPos[params$series[i]]*cl< -200000){
            dcPos[params$series[i]] <- -200000/cl
          }
          if(dcLastTran[params$series[i]] != 0) {
            dcLastTran[params$series[i]] <- dcLastTran[params$series[i]] + dcPos[params$series[i]]
            
          } else{
            dcLastTran[params$series[i]] <- dcPos[params$series[i]]
            
          }
        }
        
        else{
          dcPos[params$series[i]] = -dcLastTran[params$series[i]]
          
          dcLastTran[params$series[i]] <- 0
        }
      }
      else if(corr[i]>=0.2) {
        if(store$iter %% 30 == 2){
          if(momentumLastTran[params$series[i]] > 0){
            if(newRowList[[params$series[i]]]$Low > store$cl[store$iter-1,params$series[i]]){
              momentumLastTran[params$series[i]] <- 0
            }
          }else if(momentumLastTran[params$series[i]] < 0){
            if(newRowList[[params$series[i]]]$High < store$cl[store$iter-1,params$series[i]]){
              momentumLastTran[params$series[i]] <- 0
            }
          }
        }
        if(momentumLastTran[params$series[i]] > 0 && store$cl[endDay,i] >= EMA(store$cl[startDay:endDay,i], 90)[91]){
          if (newRowList[[params$series[i]]]$Close <= EMA(store$cl[startDay:endDay,i], 90)[91]) {
            momentumPos[params$series[i]] <- -(100000 %/% store$cl[endDay+1,params$series[i]])
            momentumLastTran[params$series[i]] <- 0
          } else if(newRowList[[params$series[i]]]$Close >= 1.13 * store$cl[endDay,i]){
            momentumPos[params$series[i]] <- -(100000 %/% store$cl[endDay+1,params$series[i]])
            momentumLastTran[params$series[i]] <- 0
          }
        }
        
        else if (momentumLastTran[params$series[i]] > 0) {
          if(newRowList[[params$series[i]]]$Close >= 1.09 * store$cl[endDay,i]){
            momentumPos[params$series[i]] <- -(100000 %/% store$cl[endDay+1,params$series[i]])
            momentumLastTran[params$series[i]] <- 0
          }
        }
        
        else if (momentumLastTran[params$series[i]] < 0 && store$cl[endDay,i] <= EMA(store$cl[startDay:endDay,i], 90)[91]) {
          if (newRowList[[params$series[i]]]$Close >= EMA(store$cl[startDay:endDay,i], 90)[91]) {
            momentumPos[params$series[i]] <- 100000 %/% store$cl[endDay+1,params$series[i]]
            momentumLastTran[params$series[i]] <- 0
          }else if (newRowList[[params$series[i]]]$Close <= 0.87 * store$cl[endDay,i]) {
            momentumPos[params$series[i]] <- 100000 %/% store$cl[endDay+1,params$series[i]]
            momentumLastTran[params$series[i]] <- 0
          }
        }
        
        else if (momentumLastTran[params$series[i]] < 0) {
          if(newRowList[[params$series[i]]]$Close <= 0.91 * store$cl[endDay,i]){
            momentumPos[params$series[i]] <- 100000 %/% store$cl[endDay+1,params$series[i]]
            momentumLastTran[params$series[i]] <- 0
          }
        }
      }
    }
    
    limitOrders <- limitOrders + limitPos
    store <- updateCorr(store,corr)
  }
  
  # The following if statement contains a strategy 
  # focused on the Commodity Channel Index and the kdj(stochastic oscillator) indicator.
  # The core idea is mean reversion, 
  # when the market declines, it will continue to add positions, and vice versa.
  
  # Buy and sell conditions: CCI() and stoch() functions
  # Position management: performed by the difference between 
  # the cci value and the set benchmark (cciOverSold and cciOverBought).
  # Stop loss: BBands() function. If touched, then clean the position.
  if (store$iter > params$cciLookback) {

    startIndex <-  store$iter - params$cciLookback

    # Get the maximum close price of all the time series, for position sizing.
    maxCl <- 0
    for (i in 1:length(params$series)){
      maxCl <- max(maxCl,newRowList[[params$series[i]]]$Close)
    }
    
    # Loop all the time series
    for (i in 1:length(params$series)) {
      # Get the latest close price
      cl <- newRowList[[params$series[i]]]$Close
      
      # Get the cci value list and KD value list, 
      # for recording today's cci/kdj value and yesterday's value
      cciList <- CCI(store$cl[startIndex:store$iter,i],
                     n=params$cciLookback,c=params$cciMeanDev)
      cci <- last(cciList)
      cciYesterday <- cciList[nrow(cciList)-1,]
      
      KDlist <- stoch(store$cl[startIndex:store$iter,i],
                      nFastK = params$nFastK, nFastD = params$nFastD,
                      nSlowD = params$nSlowD, bounded = TRUE,smooth = 1)
      KD <- last(KDlist)
      KDYesterday<- last(KDlist[-nrow(KDlist),])
      
      # Calculate the J line through KD indicator
      Jline <- 3*KD[,'fastK']- 2*KD[,'fastD']
      JlineYesterday<- 3*KDYesterday[,'fastK']- 2*KDYesterday[,'fastD']
      
      # Buy
      if (cci < params$cciOverSold && !is.na(cci) && !is.na(cciYesterday)
          && JlineYesterday < params$Jlow && Jline > params$Jlow
          && !is.na(Jline)&& !is.na(JlineYesterday)) {
        
        # Add buy operation in market order
        cciPos[params$series[i]] <- 1*(maxCl/last(cl))*
          (abs(round(cci-cciYesterday)))/last(cl)
        
        # Record in store, for stop loss
        cciAccumulatePosition[params$series[i]] <-
          cciAccumulatePosition[params$series[i]] + cciPos[params$series[i]]
      }
      
      # Sell
      else if (cci > params$cciOverBought && !is.na(cci) && !is.na(cciYesterday)
               && JlineYesterday > params$Jhigh && Jline < params$Jhigh
               &&!is.na(Jline)&& !is.na(JlineYesterday)) {
        
        # Add sell operation in market order
        cciPos[params$series[i]] <- -1*(maxCl/last(cl))*
          (abs(round(cci-cciYesterday)))/last(cl)
        
        # Record in store, for stop loss
        cciAccumulatePosition[params$series[i]] <-
          cciAccumulatePosition[params$series[i]] + cciPos[params$series[i]]
      }
      
      # Stop loss
      if (store$iter > params$BBLookback) {
        
        startIndex <-  store$iter - params$BBLookback
        
        # Get the latest close price
        cl <- newRowList[[params$series[i]]]$Close
        
        # Get the Bollinger Band rails
        bbands <- last(BBands(store$cl[startIndex:store$iter,i],
                              n=params$BBLookback,sd=params$BBsd))
        
        # When the close price touches the upper and lower rails 
        # of the Bollinger Band, we clean the position.
        if (cl < bbands[,"dn"]) {
          
          # Reset the position of this strategy
          cciPos[params$series[i]] <- -cciAccumulatePosition[params$series[i]]
          
          # Clear the accumulated value
          cciAccumulatePosition[params$series[i]] <- 0
        }
        else if (cl > bbands[,"up"]) {
          
          # Reset the position of this strategy
          cciPos[params$series[i]] <- -cciAccumulatePosition[params$series[i]]
          
          # Clear the accumulated value
          cciAccumulatePosition[params$series[i]] <- 0
        }
      }
    }
    }
  
  # Store all the values we need
  store <- updateMomentumPos(store,momentumLastTran)
  store <- updateCciPos(store,cciPos,cciAccumulatePosition)
  store <- updateDcPos(store, dcLastTran)
  
  # Set the market order to be the sum of all the strategies
  marketOrders <- marketOrders + momentumPos + dcPos + cciPos
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders,limitPrices1=limitPrice,
              limitOrders2=allzero,limitPrices2=allzero))
}


initStore <- function(newRowList,series) {
  return(list(iter=0,high=initHighStore(newRowList,series),low=initLowStore(newRowList,series),cl=initClStore(newRowList,series)
              , cor = rep(0,10), momentumPos = rep(0,10),
              dcPos = rep(0,10), cciPos = rep(0,10), cciAccumulatePosition = rep(0,10)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$high <- updateHighStore(store$high,newRowList,series,store$iter) 
  store$low <- updateLowStore(store$low,newRowList,series,store$iter) 
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  return(store)
}
updateMomentumPos <- function(store, momentumPos) {
  store$momentumPos <- momentumPos
  return(store)
}
updateDcPos <- function(store, dcPos) {
  store$dcPos <- dcPos
  return(store)
}
updateCciPos <- function(store, cciPos, cciAccumulatePosition) {
  store$cciPos <- cciPos
  store$cciAccumulatePosition <- cciAccumulatePosition
  return(store)
}
updateCorr <- function(store,corr) {
  store$cor <- corr
  return(store)
}
initHighStore  <- function(newRowList,series) {
  HighStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(HighStore)
}
updateHighStore <- function(HighStore, newRowList, series, iter) {
  for (i in 1:length(series))
    HighStore[iter,i] <- as.numeric(newRowList[[series[i]]]$High)
  return(HighStore)
}
initLowStore  <- function(newRowList,series) {
  LowStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(LowStore)
}
updateLowStore <- function(LowStore, newRowList, series, iter) {
  for (i in 1:length(series))
    LowStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Low)
  return(LowStore)
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
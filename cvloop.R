library(forecast)
library(dynlm)
library(dyn)
library(gridExtra)

cvloop <- function(dt, type = "AR", arpdq = c(1,0,0), arpdqs = c(0,0,0)){
  minYr <- as.numeric(format(minDate, '%Y'))
  minWk <- as.numeric(format(minDate + 3, '%U'))
  ts.flupos <- ts(dt$FluPos, frequency = 52, start = c(minYr, minWk))
  ts.gft <- ts(dt$gft, frequency = 52, start = c(minYr, minWk))
  
  # make training-to-test ratio 5 to 1 (diminishes the pandemic effect)
  trsize <- 5
  perr <- NULL
  
  for(i in 0:6){
    # segment data
    ts.learn <- window(ts.flupos, start = c(minYr + i, minWk), 
                       end = c(minYr + trsize + i, minWk - 1))
    ts.learn.gft <- window(ts.gft, start = c(minYr + i, minWk), 
                           end = c(minYr + trsize + i, minWk - 1))
    learn<- data.frame(FluPos = ts.learn, gft = ts.learn.gft)
    
    ts.valid <- window(ts.flupos, start = c(minYr + i + trsize, minWk), 
                       end = c(minYr + trsize + 1 + i, minWk - 1))
    ts.valid.gft <- window(ts.gft, start = c(minYr + i + trsize, minWk), 
                           end = c(minYr + trsize + 1 + i, minWk - 1))
    valid<- data.frame(FluPos = ts.valid, gft = ts.valid.gft)
    
    cat(type, "Iteration: ", i, 
        "\n  Training set: ", min(as.yearmon(time(ts.learn))), 
        " to ", max(as.yearmon(time(ts.learn))),
        "\n  Validation set: ", min(as.yearmon(time(ts.valid))),
        " to ", max(as.yearmon(time(ts.valid))), "\n")
    
    if(type == "AR"){
      ## time-series model
      ar <- arima(ts.learn, arpdq, list(order = arpdqs))  
      
      ## prediction errors
      ar.perr.loop2 <- NULL
      ar.perr.loop3 <- NULL
      ar.perr.loop4 <- NULL
      lsize <- length(ts.learn)
      
      for(k in 1:(length(ts.valid))){
        forec <- forecast(Arima(
          c(ts.learn[min(0,lsize - k): lsize],ts.valid[1:k]), model = ar), h = 4)$mean
        ar.perr.loop2 <- c(ar.perr.loop2, ts.valid[k+2] - forec[2])
        ar.perr.loop3 <- c(ar.perr.loop3, ts.valid[k+3] - forec[3])
        ar.perr.loop4 <- c(ar.perr.loop4, ts.valid[k+4] - forec[4])
      }
      
      ## calculate mean squared error
      ar.perr2 <- mean(ar.perr.loop2^2, na.rm = TRUE)
      ar.perr3 <- mean(ar.perr.loop3^2, na.rm = TRUE)
      ar.perr4 <- mean(ar.perr.loop4^2, na.rm = TRUE)
      
      cat("AR prediction errors: ", ar.perr2, ar.perr3, ar.perr4, "\n")
      
      ar.perr2 <- data.frame(index = i, lag = 2, method = "AR2", MSE = ar.perr2)
      ar.perr3 <- data.frame(index = i, lag = 3, method = "AR3", MSE = ar.perr3)
      ar.perr4 <- data.frame(index = i, lag = 4, method = "AR4", MSE = ar.perr4)
      
      perr <- rbind(perr, rbind(ar.perr2, ar.perr3, ar.perr4))
    } else if(type == "DR"){
      ## dynamic regression time-series model
      dr22 <- dynlm(FluPos ~ L(FluPos, 2) + L(gft, 2) + L(FluPos, 52), data = learn)
      dr33 <- dynlm(FluPos ~ L(FluPos, 3) + L(gft, 3) + L(FluPos, 52), data = learn)
      dr44 <- dynlm(FluPos ~ L(FluPos, 4) + L(gft, 4) + L(FluPos, 52), data = learn)
      dr20 <- dynlm(FluPos ~ L(FluPos, 2) + L(FluPos, 52), data = learn)
      dr30 <- dynlm(FluPos ~ L(FluPos, 3) + L(FluPos, 52), data = learn)
      dr40 <- dynlm(FluPos ~ L(FluPos, 4) + L(FluPos, 52), data = learn)
      
      dr.perr.loop <- NULL
      ### Loop goes here ###
      pred22 <- predict(dr22, newdata = valid)
      pred33 <- predict(dr33, newdata = valid)
      pred44 <- predict(dr44, newdata = valid)
      pred20 <- predict(dr20, newdata = valid)
      pred30 <- predict(dr30, newdata = valid)
      pred40 <- predict(dr40, newdata = valid)
      
      dr.perr22 <- mean((pred22 - valid$FluPos)^2, na.rm = TRUE)
      dr.perr33 <- mean((pred33 - valid$FluPos)^2, na.rm = TRUE)
      dr.perr44 <- mean((pred44 - valid$FluPos)^2, na.rm = TRUE)
      dr.perr20 <- mean((pred20 - valid$FluPos)^2, na.rm = TRUE)
      dr.perr30 <- mean((pred30 - valid$FluPos)^2, na.rm = TRUE)
      dr.perr40 <- mean((pred40 - valid$FluPos)^2, na.rm = TRUE)
      
      cat("DR prediction errors: ", dr.perr20, dr.perr22, dr.perr30, dr.perr33, dr.perr40, dr.perr44, "\n")
      
      dr.perr22 <- data.frame(index = i, lag = 2, method = "DR22", MSE = dr.perr22)
      dr.perr33 <- data.frame(index = i, lag = 3, method = "DR33", MSE = dr.perr33)
      dr.perr44 <- data.frame(index = i, lag = 4, method = "DR44", MSE = dr.perr44)
      dr.perr20 <- data.frame(index = i, lag = 2, method = "DR20", MSE = dr.perr20)
      dr.perr30 <- data.frame(index = i, lag = 3, method = "DR30", MSE = dr.perr30)
      dr.perr40 <- data.frame(index = i, lag = 4, method = "DR40", MSE = dr.perr40)
      
      perr <- rbind(perr, rbind(dr.perr22, dr.perr33, dr.perr44, dr.perr20, dr.perr30, dr.perr40)) 
    }
  }
  means <- as.data.frame(tapply(perr$MSE, perr$method, mean))
  means$method<- rownames(means)
  colnames(means)[1]<- "meanMSE"
  
  perr <- merge(perr, means, by = "method")
  perr$method <- factor(perr$method, levels = c("AR2", "AR3", "AR4", 
                                                "DR20", "DR22", 
                                                "DR30", "DR33", 
                                                "DR40", "DR44"))
  return(perr)
}

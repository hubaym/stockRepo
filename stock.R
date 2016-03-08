install.packages("sqldf")
install.packages("RPostgreSQL")

library(RPostgreSQL)
library(sqldf)

# Establish connection to PoststgreSQL using RPostgreSQL
drv <- dbDriver("PostgreSQL")
#con <- dbConnect(drv)# Simple version (localhost as default)
# Full version of connection seetting
 con <- dbConnect(drv, dbname="stock",host="localhost",port=5432,user="postgres",password="postgres")
 dbWriteTable(con, "AAPL", AAPL, row.names=FALSE)
 
 
 
 library(quantmod)
 rm(AAPLyear, AAPL)
 getSymbols("AAPL", adjust=TRUE)
 AAPLyear <- AAPL["2015"]


 AAPLyear$AAPL.WPR14 <- 0
 AAPLyear$AAPL.WPRbool <- 0
 AAPLyear$AAPL.WPRsign <- 0
 AAPLyear$AAPL.SLval <- 0
 AAPLyear$AAPL.OutSLbool <- 0
 AAPLyear$AAPL.TPval <- 0
 AAPLyear$AAPL.OutTPbool <- 0
 AAPLyear$AAPL.Broker<- 0
 AAPLyear$AAPL.Ret<- 0
 
 AAPLyear$AAPL.WPR14 <- ifelse(is.na(WPR(Cl(AAPLyear), 14)),0,WPR(Cl(AAPLyear), 14))
 AAPLyear$AAPL.WPRbool <- ifelse(AAPLyear$AAPL.WPR14 > 0.9, 1, 0)
 AAPLyear$AAPL.WPRsign <- ifelse(is.na(lag(AAPLyear$AAPL.WPRbool)),0, ifelse((AAPLyear$AAPL.WPRbool-  lag(AAPLyear$AAPL.WPRbool)>0),1,0) )      
 for (i in 2 : nrow(AAPLyear)) {
   AAPLyear$AAPL.SLval[i] <-  ifelse( is.na( AAPLyear$AAPL.SLval[i-1] )  ,0, ifelse(AAPLyear$AAPL.WPRsign[i]>0,AAPLyear$AAPL.Open[i] * 0.98,  AAPLyear$AAPL.SLval[i-1]   ))
 }
 AAPLyear$AAPL.OutSLbool <- ifelse(AAPLyear$AAPL.SLval > AAPLyear$AAPL.Close, 1, 0)
 
 for (i in 2 : nrow(AAPLyear)) {
   AAPLyear$AAPL.TPval[i] <-  ifelse( is.na( AAPLyear$AAPL.TPval[i-1] )  ,0, ifelse(AAPLyear$AAPL.WPRsign[i]>0,AAPLyear$AAPL.Open[i] * 1.06,  AAPLyear$AAPL.TPval[i-1]   ))
 }
 AAPLyear$AAPL.OutTPbool <- ifelse(AAPLyear$AAPL.TPval < AAPLyear$AAPL.Close, 1, 0)
 
 for (i in 2 : nrow(AAPLyear)-1) {
    ifelse(AAPLyear$AAPL.Broker[i]==0,   
          ifelse( AAPLyear$AAPL.WPRsign[i] >0,AAPLyear$AAPL.Broker[i+1]<- 1 , AAPLyear$AAPL.Broker[i+1]<- 0 ),
          ifelse( AAPLyear$AAPL.OutTPbool[i] >0  |  AAPLyear$AAPL.OutSLbool[i] >0,AAPLyear$AAPL.Broker[i+1]<- 0 , AAPLyear$AAPL.Broker[i+1]<- 1)
           )
 }
 AAPLyear$AAPL.Ret<- ifelse( AAPLyear$AAPL.Broker>0 ,  AAPLyear$AAPL.Close -AAPLyear$AAPL.Open,0)
 
 
 sum(AAPLyear$AAPL.Ret)
 
 plot(AAPLyear)
 for (i in 2 : nrow(AAPLyear)-1) {
   if(AAPLyear$AAPL.Broker[i]>0)  {
     
     abline(v= .index(AAPLyear)[i], col="blue")
   } 
   if(AAPLyear$AAPL.WPRsign[i]>0)  {
     
     abline(v= .index(AAPLyear)[i], col="green")
   } 
   if(AAPLyear$AAPL.OutSLbool[i]>0  | AAPLyear$AAPL.OutTPbool[i]>0)  {
     
     abline(v= .index(AAPLyear)[i], col="red")
   }  
   
 }
 
 


 head(AAPLyear)
 AAPLyear
 
 install.packages("ggplot2")
 library('ggplot2')
 
 xx <- 
 yy
 zz
 
 fun <- data.frame(
   x = rep(c(1:100), 100),
   y = sort(  rep(c(1:100), 100) ),
   z = c(1:10000)
 )
 ggplot(data = fun, aes(x = x, y = y, colour = z)) + geom_point(size = 1)
 
 init <- function (x){
   x$AAPL.WPR14 <- 0
   x$AAPL.WPRbool <- 0
   x$AAPL.WPRsign <- 0
   x$AAPL.SLval <- 0
   x$AAPL.OutSLbool <- 0
   x$AAPL.TPval <- 0
   x$AAPL.OutTPbool <- 0
   x$AAPL.Broker<- 0
   x$AAPL.Ret<- 0
   
 }
 
 init(AAPLyear)
 AAPL <- getSymbols("IBM", adjust=TRUE)
 head(YHOO$Close)
 
 
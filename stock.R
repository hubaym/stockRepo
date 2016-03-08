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
 
 install.packages("raster")
 library(raster)
 r <- raster(nrows=10, ncols=10)
 r <- setValues(r, 1:ncell(r))
 r <- as.raster(c(0.5, 1, 0.5))
 r[5]
 seq1 <- seq(1:6)
 mat1 <- matrix(seq1, 2)
 r <- as.raster(mat1)
 plot(r)
 y<-matrix(rnorm(25),5,5)
 y
 plot(y)
 
 df <- data.frame(matrix(ncol = 11, nrow = 10))
 df
 rownames(df) <-c(1:10)
 colnames(df) <-c(1:11)
 for (i in 1:30){
   
   df$`i` <-c(i:i+9)
 }
 c(3:12)
 df$`1` <-c(1:10)
 df$`2` <-c(2:11)
 df$`3` <-c(3:12)
 df$`4` <-c(4:13)
 df$`5` <-c(5:14)
 df$`6` <-c(6:15)
 df$`7` <-c(7:16)
 df$`8` <-c(8:17)
 df$`9` <-c(9:18)
 df$`10` <-c(10:19)
 range01 <- function(x)(x-min(x))/diff(range(x))
 cRamp <- function(x){
   cols <- colorRamp(topo.colors(10))(range01(x))
   apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
 }  

 plot(df)
 df[,c(1,2)]
 
 
 
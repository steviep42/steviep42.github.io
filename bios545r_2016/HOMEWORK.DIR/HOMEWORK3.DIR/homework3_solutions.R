# http://www.businessinsider.com/chart-of-the-day-digital-subscription-prices-2011-3

url <- "http://steviep42.bitbucket.org/bios545r_2016/SUPP.DIR/digital_subscription.csv"
df <- read.csv(url)

sub <- "Annual Digital Subscription for Multi-Device/Cross-Platform"
mtitle <- paste("Digital Subscription Prices",sub,sep="\n")
cols <- c(rep("#6CA560",3),rep("#3E6D98",3),rep("#86508A",3),rep("#D47A19",2),"#CC3D41")
hold <- barplot(df$Price,xaxt="n",ylim=c(0,500),col=cols,main=mtitle,cex.main=0.9)

vals <- c(50,99,120,36,155,180,79,96,96,110,207,455)
cats <- c("Software","Music","Video","News")

mtext(text=df$Name,side=1,at=hold,cex=0.6)
# mtext(text=cats,side=1,at=seq(2,14,4),padj=1.7)
mtext(text=cats,side=1,at=c(2,6,9.3,12.6),padj=1.7)

abline(h=seq(0,500,50),col="gray")
segments(0,-50,1,-50,xpd=TRUE,lwd=1.2)
segments(3.0,-50,5.2,-50,xpd=TRUE,lwd=1.2)
segments(6.7,-50,8.5,-50,xpd=TRUE,lwd=1.2)
segments(10,-50,11.9,-50,xpd=TRUE,lwd=1.2)
segments(13.2,-50,14.5,-50,xpd=TRUE,lwd=1.2)

barplot(df$Price,xaxt="n",ylim=c(0,500),col=cols,add=T)

text(hold,df$Price-15,paste("$",vals,sep=""),col="white",cex=0.8)
legend(0,500,"Source: Michael DeGusta/The Understatment.com (March 2011)",cex=0.6)

# Weather

# Requirements - d1 must be less than d2
# A minumum of 7 days
# The X axis must represent be readable and contain the month and day of the data
# If crossing a year boundary then draw a dotted vertical red line to indicate that



weather <- function(d1,d2) {
  setwd("~/Downloads/history/tester")
  
  # weather("2014-01-15","2014-01-17")
  
  d1 <- as.Date(d1)
  d2 <- as.Date(d2)
  
  mylist <- list()
  myfiles <- list.files()
  
  # file_names <- dir() #where you have your files
  # your_data_frame <- do.call(rbind,lapply(file_names,read.csv))
  
  jj <- 1
  for (ii in myfiles) {
    mylist[[jj]] <- read.csv(ii)
    jj <- jj + 1
  }
  
  weather_df <- do.call(rbind,mylist)
  weather_df$EDT <- as.Date(weather_df$EDT)
  drange <- weather_df[weather_df$EDT >= d1 & weather_df$EDT <= d2,]
  
  
  par(mfrow=c(1,1))
  #  plot(drange$Max.TemperatureF,type="l",main="Max Temp",xaxt="n")
  ylims <- c(min(drange$Min.TemperatureF),max(drange$Max.TemperatureF))
  plot(drange$Max.TemperatureF,type="n",main="Max Temp",xaxt="n",ylim=ylims)
  points(drange$Max.TemperatureF,type="l",col="red")
  #  points(drange$Mean.TemperatureF,type="l",col="blue")
  points(drange$Min.TemperatureF,type="l",col="green")
  pseq <- seq(1,nrow(drange),round(nrow(drange)/6))
  axis(1,at=pseq,labels=drange$EDT[pseq])
  abline(h=pseq,col="black")
  
  # plot(drange$Max.Wind.SpeedMPH,type="l",main="Max Wind Speed",xaxt="n")
  #  grid(col="black")
  #  plot(drange$Max.Sea.Level.PressureIn,type="l",main="Max Sea Level Pressure",xaxt="n")
  #  grid(col="black")
  
  return(drange)
}



weather <- function(d1,d2,wd="~/Downloads/weatherdat") {
  setwd(wd)
  
  # weather("2014-01-15","2014-03-31")
  
  d1 <- as.Date(d1)
  d2 <- as.Date(d2)
  
  myfiles <- list.files()
  
  # file_names <- dir() #where you have your files
  # your_data_frame <- do.call(rbind,lapply(file_names,read.csv))
  
  tempdf <- data.frame()
  for (ii in myfiles) {
    tempdf <- rbind(tempdf,read.csv(ii))
  }
  
  weather_df <- tempdf
  weather_df$EDT <- as.Date(weather_df$EDT)
  drange <- weather_df[weather_df$EDT >= d1 & weather_df$EDT <= d2,]
  drange$EDT <- substr(drange$EDT,6,10)
  
  ylims <- c(min(drange$MeanDew.PointF),max(drange$Mean.TemperatureF))
  ylabs <- "Temperature F"
  xlabs <- "Time Period"
  plot(drange$Max.TemperatureF,type='n',col="blue",main="2014 Weather data for 30322",
       lwd=2,ylim=ylims,xaxt="n",ylab=ylabs,xlab=xlabs)
  
  points(drange$MeanDew.PointF,col="red",lwd=2,type="l")
  points(drange$Mean.TemperatureF,col="blue",lwd=2,type="l")
  mtext("Mean Temperature",side=3,adj=0,col="blue")
  mtext("Dew Point",side=3,adj=1,col="red")
  
  vals <- quantile(1:nrow(drange),probs=seq(0,1,1/6))
  axis(1,at=vals,labels=as.character(drange$EDT[vals]),cex.axis=0.8)
  abline(v=vals,col="black",lty=2) 
  abline(h=seq(ylims[1],ylims[2],10),col="black",lty=2)
  return(drange)
}




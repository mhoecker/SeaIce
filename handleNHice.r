rm(list=ls())
monthDates	<- as.Date(c(
	"2016-01-01",
	"2016-01-15",
	"2016-02-15",
	"2016-03-15",
	"2016-04-15",
	"2016-05-15",
	"2016-06-15",
	"2016-07-15",
	"2016-08-15",
	"2016-09-15",
	"2016-10-15",
	"2016-11-15",
	"2016-12-15",
	"2016-12-31"
	))
Nspan	<- 3
pngsize	<-list(width=1680,height=840)
#
smoothice	<-	function(filename,Nspan){
	ice <- read.csv(file=filename,header=F,skip=2,nrows=-1)
	colnames(ice) <- colnames(read.csv(file=filename,header=T,nrows=2))
	ice$dateString <- sprintf("%04i-%02i-%02i",ice$Year,ice$Month,ice$Day)
	ice$date <- as.Date(ice$dateString)
	ice$JJJ <- as.numeric(strftime(ice$date,format="%j"))
	ice$dExtent <- ice$Extent
	ice$medianExtent <- ice$Extent
	for( i in 1:length(ice$Year)){
		if(i<Nspan){
			ice$dExtent	<-	NA
			ice$medianExtent[i]	<-	NA
		}else if((i>length(ice$Year)-Nspan)){
			ice$dExtent[i]	<-	NA
			ice$medianExtent[i]	<- NA
		}else{
			datarange <- 1:length(ice$Year)
			datarange <- abs(datarange-i) < Nspan+1
			subdata	<- data.frame(
				x=as.numeric(ice$date[datarange]-ice$date[i]),
				y=ice$Extent[datarange]
				)
			fitmodel <-	lm(
				formula= y ~ x,
				data=subdata)
			ice$dExtent[i] <- as.numeric(fitmodel$coefficients[2])
			ice$medianExtent[i] <- as.numeric(fitmodel$coefficients[1])
		}
	}
	return(ice)
}
meanice	<-	function(ice){
	icemeans <-	data.frame(Jday=unique(NHice$JJJ))
	icemeans$Extent <- NA
	icemeans$Extent90 <- NA
	icemeans$Extent10 <- NA
	icemeans$dExtent <- NA
	icemeans$dExtent90 <- NA
	icemeans$dExtent10 <- NA
	icemeans <- icemeans[order(icemeans$Jday),]
	for( j in 1:length(icemeans$Jday)){
		isday	<-	ice$JJJ==icemeans$Jday[j]
		icemeans$Extent[j] <- mean(ice[isday,"Extent"],na.rm=T)
		icemeans$Extent95[j] <- quantile(ice[isday,"Extent"],probs=.95,na.rm=T)
		icemeans$Extent05[j] <- quantile(ice[isday,"Extent"],probs=.05,na.rm=T)
		icemeans$Extent75[j] <- quantile(ice[isday,"Extent"],probs=.75,na.rm=T)
		icemeans$Extent25[j] <- quantile(ice[isday,"Extent"],probs=.25,na.rm=T)
		#
		icemeans$dExtent[j] <- median(ice[isday,"dExtent"],na.rm=T)
		icemeans$dExtent95[j] <- quantile(ice[isday,"dExtent"],probs=.95,na.rm=T)
		icemeans$dExtent05[j] <- quantile(ice[isday,"dExtent"],probs=.05,na.rm=T)
		icemeans$dExtent75[j] <- quantile(ice[isday,"dExtent"],probs=.75,na.rm=T)
		icemeans$dExtent25[j] <- quantile(ice[isday,"dExtent"],probs=.25,na.rm=T)
	}
	return(icemeans)
}
addanomaly	<-	function(ice,icemeans){
		ice$ExtentAnomaly	<-	ice$medianExtent
		ice$dExtentAnomaly	<-	ice$dExtent
		for( j in 1:length(icemeans$Jday)){
			isday	<-	ice$JJJ==icemeans$Jday[j]
			ice$ExtentAnomaly[isday] <- ice$ExtentAnomaly[isday]-icemeans$Extent[j]
			ice$dExtentAnomaly[isday] <- ice$dExtentAnomaly[isday]-icemeans$dExtent[j]
		return(ice)
}
#NHicefile <- 'NH_seaice_extent_final_v2.csv'

NHice <- smoothice(filename='NH_seaice_extent_final_v2.csv',Nspan=Nspan)
NHice2016 <- smoothice(filename='NH_seaice_extent_nrt_v2.csv',Nspan=Nspan)
NHicemeans <- meanice(NHice)
NHice	<- addanomaly(NHice,NHmeans)
NHice2016	<- addanomaly(NHice2016,NHmeans)
#
SHice <- smoothice(filename='SH_seaice_extent_final_v2.csv',Nspan=Nspan)
SHice2016 <- smoothice(filename='SH_seaice_extent_nrt_v2.csv',Nspan=Nspan)
SHicemeans <- meanice(SHice)
SHice	<- addanomaly(SHice,SHmeans)
SHice2016	<- addanomaly(SHice2016,SHmeans)
#
#
rkpal	<-	colorRampPalette(c('black','red'))
Nyears	<-	length(unique(NHice$Year))
NHice$color <- rkpal(Nyears)[as.numeric(cut(NHice$Year,breaks=Nyears))]
#
rkpal	<-	colorRampPalette(c('black','red'))
Nyears	<-	length(unique(SHice$Year))
SHice$color <- rkpal(Nyears)[as.numeric(cut(SHice$Year,breaks=Nyears))]
#
#
png(filename="NHice.png",
	width=pngsize$width,
	height=pngsize$height)
par(mfrow=c(2,1))
plot(
	NHicemeans[,c("Jday","dExtent")],
	pch='.',
	axes=F,
	xlab="Day of the year",
	xlim=c(1,366),
	ylab="Rate of Change in Arctic Sea Ice (10^6 km^2 / day)",
	ylim=c(-.3,.3))
title(sprintf("Rate of change calculated over %i days",2*Nspan+1))
monthString	<- strftime(monthDates,format="%b %d")
monthJdayString	<- strftime(monthDates,format="%j")
monthJdays	<- as.numeric(monthJdayString)
axis(1,
	at=monthJdays,
	label=monthString)
axis(2,
	at=pretty(c(min(NHicemeans$dExtent05),max(NHicemeans$dExtent95))))
polygon(c(NHicemeans$Jday, rev(NHicemeans$Jday)),
	c(NHicemeans$dExtent95, rev(NHicemeans$dExtent05)),
	col = "grey80",
	border = NA)
polygon(c(NHicemeans$Jday, rev(NHicemeans$Jday)),
	c(NHicemeans$dExtent75, rev(NHicemeans$dExtent25)),
	col = "grey60",
	border = NA)
points(NHice[,c("JJJ","dExtent")],col=NHice$color,pch=".")
lines(NHicemeans[,c("Jday","dExtent")],col="white",lw=2)
lines(NHice2016[,c("JJJ","dExtent")],col="blue")
lines(x=NHicemeans$Jday,y=0*NHicemeans$Jday,lty=5,col="black",lw=1)
legend(
	"topleft",
	col=c(
		"white",
		"grey60",
		"grey80",
		"blue"),
	legend=c(
		"1978 to 2015 median",
		"25% to 75% range (1978-2015)",
		" 5% to 95% range (1978-2015)",
		"2016"),
	fill=c(
		"white",
		"grey60",
		"grey80",
		"blue"),
	border="black"
	)
legend(
	"bottomleft",
	col=c(
		rkpal(3)[1],
		rkpal(3)[2],
		rkpal(3)[3]),
	legend=c(
		"1978",
		"to",
		"2015"),
	fill=c(
		rkpal(3)[1],
		rkpal(3)[2],
		rkpal(3)[3]),
	border="black"
	)
#dev.off()
#
plot(
	NHicemeans[,c("Jday","Extent")],
	pch='.',
	axes=F,
	xlab="Day of the year",
	xlim=c(1,366),
	ylab="Arctic Sea Ice (10^6 km^2)",
	ylim=c(min(NHice$Extent,na.rm=T),max(NHice$Extent,na.rm=T)))
title(sprintf("Smoothed over %i days",2*Nspan+1))
monthString	<- strftime(monthDates,format="%b %d")
monthJdayString	<- strftime(monthDates,format="%j")
monthJdays	<- as.numeric(monthJdayString)
axis(1,
	at=monthJdays,
	label=monthString)
axis(2,
	at=pretty(c(min(NHicemeans$Extent05),max(NHicemeans$Extent95))))
polygon(c(NHicemeans$Jday, rev(NHicemeans$Jday)),
	c(NHicemeans$Extent95, rev(NHicemeans$Extent05)),
	col = "grey80",
	border = NA)
polygon(c(NHicemeans$Jday, rev(NHicemeans$Jday)),
	c(NHicemeans$Extent75, rev(NHicemeans$Extent25)),
	col = "grey60",
	border = NA)
points(NHice[,c("JJJ","medianExtent")],col=NHice$color,pch=".")
lines(NHicemeans[,c("Jday","Extent")],col="white",lw=2)
lines(NHice2016[,c("JJJ","medianExtent")],col="blue")
lines(x=NHicemeans$Jday,y=0*NHicemeans$Jday,lty=5,col="black",lw=1)
legend(
	"bottomleft",
	col=c(
		rkpal(3)[1],
		rkpal(3)[2],
		rkpal(3)[3],
		"white",
		"grey60",
		"grey80",
		"blue"),
	legend=c(
		"1978",
		"to",
		"2015",
		"1978 to 2015 median",
		"25% to 75% range (1978-2015)",
		" 5% to 95% range (1978-2015)",
		"2016"),
	fill=c(
		rkpal(3)[1],
		rkpal(3)[2],
		rkpal(3)[3],
		"white",
		"grey60",
		"grey80",
		"blue"),
	border="black"
	)
dev.off()
#
png(filename="SHice.png",
	width=pngsize$width,
	height=pngsize$height)
par(mfrow=c(2,1))
plot(
	SHicemeans[,c("Jday","dExtent")],
	pch='.',
	axes=F,
	xlab="Day of the year",
	xlim=c(1,366),
	ylab="Rate of Change in Antarctic Sea Ice (10^6 km^2 / day)",
	ylim=c(-.3,.3))
title(sprintf("Rate of change calculated over %i days",2*Nspan+1))
monthString	<- strftime(monthDates,format="%b %d")
monthJdayString	<- strftime(monthDates,format="%j")
monthJdays	<- as.numeric(monthJdayString)
axis(1,
	at=monthJdays,
	label=monthString)
axis(2,
	at=pretty(c(min(SHicemeans$dExtent05),max(SHicemeans$dExtent95))))
polygon(c(SHicemeans$Jday, rev(SHicemeans$Jday)),
	c(SHicemeans$dExtent95, rev(SHicemeans$dExtent05)),
	col = "grey80",
	border = NA)
polygon(c(SHicemeans$Jday, rev(SHicemeans$Jday)),
	c(SHicemeans$dExtent75, rev(SHicemeans$dExtent25)),
	col = "grey60",
	border = NA)
points(SHice[,c("JJJ","dExtent")],col=SHice$color,pch=".")
lines(SHicemeans[,c("Jday","dExtent")],col="white",lw=2)
lines(SHice2016[,c("JJJ","dExtent")],col="blue")
lines(x=SHicemeans$Jday,y=0*SHicemeans$Jday,lty=5,col="black",lw=1)
legend(
	"topleft",
	col=c(
		"white",
		"grey60",
		"grey80",
		"blue"),
	legend=c(
		"1978 to 2015 median",
		"25% to 75% range (1978-2015)",
		" 5% to 95% range (1978-2015)",
		"2016"),
	fill=c(
		"white",
		"grey60",
		"grey80",
		"blue"),
	border="black"
	)
legend(
	"bottomleft",
	col=c(
		rkpal(3)[1],
		rkpal(3)[2],
		rkpal(3)[3]),
	legend=c(
		"1978",
		"to",
		"2015"),
	fill=c(
		rkpal(3)[1],
		rkpal(3)[2],
		rkpal(3)[3]),
	border="black"
	)
#dev.off()
#
plot(
	SHicemeans[,c("Jday","Extent")],
	pch='.',
	axes=F,
	xlab="Day of the year",
	xlim=c(1,366),
	ylab="Antarctic Sea Ice (10^6 km^2)",
	ylim=c(min(SHice$Extent,na.rm=T),max(SHice$Extent,na.rm=T)))
title(sprintf("Smoothed over %i days",2*Nspan+1))
monthString	<- strftime(monthDates,format="%b %d")
monthJdayString	<- strftime(monthDates,format="%j")
monthJdays	<- as.numeric(monthJdayString)
axis(1,
	at=monthJdays,
	label=monthString)
axis(2,
	at=pretty(c(min(SHicemeans$Extent05),max(SHicemeans$Extent95))))
polygon(c(SHicemeans$Jday, rev(SHicemeans$Jday)),
	c(SHicemeans$Extent95, rev(SHicemeans$Extent05)),
	col = "grey80",
	border = NA)
polygon(c(SHicemeans$Jday, rev(SHicemeans$Jday)),
	c(SHicemeans$Extent75, rev(SHicemeans$Extent25)),
	col = "grey60",
	border = NA)
points(SHice[,c("JJJ","medianExtent")],col=SHice$color,pch=".")
lines(SHicemeans[,c("Jday","Extent")],col="white",lw=2)
lines(SHice2016[,c("JJJ","medianExtent")],col="blue")
lines(x=SHicemeans$Jday,y=0*SHicemeans$Jday,lty=5,col="black",lw=1)
legend(
	"topleft",
	col=c(
		rkpal(3)[1],
		rkpal(3)[2],
		rkpal(3)[3],
		"white",
		"grey60",
		"grey80",
		"blue"),
	legend=c(
		"1978",
		"to",
		"2015",
		"1978 to 2015 median",
		"25% to 75% range (1978-2015)",
		" 5% to 95% range (1978-2015)",
		"2016"),
	fill=c(
		rkpal(3)[1],
		rkpal(3)[2],
		rkpal(3)[3],
		"white",
		"grey60",
		"grey80",
		"blue"),
	border="black"
	)
dev.off()




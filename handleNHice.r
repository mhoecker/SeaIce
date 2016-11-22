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
#
rkpal	<-	colorRampPalette(c('black','red'))
#
monthString	<- strftime(monthDates,format="%b %d")
monthJdayString	<- strftime(monthDates,format="%j")
monthJdays	<- as.numeric(monthJdayString)
xtics	<-	list(at=monthJdays,label=monthString)

Nspan	<- 3
pngsize	<-list(width=1680,height=1024)
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
	return(ice)
}
meanice	<-	function(ice){
	icemeans <-	data.frame(Jday=unique(ice$JJJ))
	icemeans$Extent	<- NA
	icemeans <- icemeans[order(icemeans$Jday),]
	for( j in 1:length(icemeans$Jday)){
		isday	<-	ice$JJJ==icemeans$Jday[j]
		# calculate Extent quantiles
		icemeans$Extent[j] <- mean(ice[isday,"Extent"],na.rm=T)
		icemeans$Extent95[j] <- quantile(ice[isday,"Extent"],probs=.95,na.rm=T)
		icemeans$Extent05[j] <- quantile(ice[isday,"Extent"],probs=.05,na.rm=T)
		icemeans$Extent75[j] <- quantile(ice[isday,"Extent"],probs=.75,na.rm=T)
		icemeans$Extent25[j] <- quantile(ice[isday,"Extent"],probs=.25,na.rm=T)
		# calculate dExtent quantiles
		icemeans$dExtent[j] <- median(ice[isday,"dExtent"],na.rm=T)
		icemeans$dExtent95[j] <- quantile(ice[isday,"dExtent"],probs=.95,na.rm=T)
		icemeans$dExtent05[j] <- quantile(ice[isday,"dExtent"],probs=.05,na.rm=T)
		icemeans$dExtent75[j] <- quantile(ice[isday,"dExtent"],probs=.75,na.rm=T)
		icemeans$dExtent25[j] <- quantile(ice[isday,"dExtent"],probs=.25,na.rm=T)
	}
	# Calculate Anomaly quantiles
	icemeans$Extent05A	<-	icemeans$Extent05-icemeans$Extent
	icemeans$Extent25A	<-	icemeans$Extent25-icemeans$Extent
	icemeans$Extent75A	<-	icemeans$Extent75-icemeans$Extent
	icemeans$Extent95A	<-	icemeans$Extent95-icemeans$Extent
	# 
	icemeans$dExtent05A	<-	icemeans$dExtent05-icemeans$dExtent
	icemeans$dExtent25A	<-	icemeans$dExtent25-icemeans$dExtent
	icemeans$dExtent75A	<-	icemeans$dExtent75-icemeans$dExtent
	icemeans$dExtent95A	<-	icemeans$dExtent95-icemeans$dExtent
	#
	return(icemeans)
}
addanomaly	<-	function(ice,icemeans){
	ice$ExtentAnomaly	<-	ice$medianExtent
	ice$dExtentAnomaly	<-	ice$dExtent
	for( j in 1:length(icemeans$Jday)){
		isday	<-	ice$JJJ==icemeans$Jday[j]
		ice$ExtentAnomaly[isday] <- ice$ExtentAnomaly[isday]-icemeans$Extent[j]
		ice$dExtentAnomaly[isday] <- ice$dExtentAnomaly[isday]-icemeans$dExtent[j]
	}
	return(ice)
}
plotice	<-	function(ice,icenow,icemeans,icename="",Nspan=NA,xtics=list(at=c(1,3),label=c("Jan 1","Dec 31"))){
Extentlab	<-	paste(icename," Extent (10^6 km^2)")
dExtentlab	<-	paste("Rate of Change in",icename,"(10^6 km^2 / day)")
par(mfrow=c(2,1))
#Upper plot of Rate
plot(
	icemeans[,c("Jday","dExtent")],
	pch=16,
	axes=F,
	xlab="Day of the year",
	xlim=c(1,366),
	ylab=dExtentlab,
	ylim=c(-.4,.3))
	if(!is.na(Nspan)){title(sprintf("Rate of change calculated over %i days",2*Nspan+1))}
axis(1,
	at=xtics$at,
	label=xtics$label)
axis(2,
	at=pretty(c(min(icemeans$dExtent05),max(icemeans$dExtent95))))
polygon(c(icemeans$Jday, rev(icemeans$Jday)),
	c(icemeans$dExtent95, rev(icemeans$dExtent05)),
	col = "grey80",
	border = NA)
polygon(c(icemeans$Jday, rev(icemeans$Jday)),
	c(icemeans$dExtent75, rev(icemeans$dExtent25)),
	col = "grey60",
	border = NA)
lines(icemeans[,c("Jday","dExtent")],col="white",lw=2)
points(ice[,c("JJJ","dExtent")],col=ice$color,pch=16,cex=.3)
lines(icenow[,c("JJJ","dExtent")],col="blue")
lines(x=icemeans$Jday,y=0*icemeans$Jday,lty=5,col="black",lw=1)
legend("bottom",
	bty="n",
	ncol=3,
	border=c(
		rkpal(3)[[1]],
		rkpal(3)[[2]],
		rkpal(3)[[3]],
		"black",
		"grey80",
		"grey60",
		"blue",
		"white",
		"white"
		),
	fill=c(
		ice$color[ice$Year==min(ice$Year)][1],
		ice$color[ice$Year==median(ice$Year)][1],
		ice$color[ice$Year==max(ice$Year)][1],
		"white",
		"grey80",
		"grey60",
		"blue",
		"white",
		"white"
		),
	legend=c(
		min(ice$Year,na.rm=T),
		"to",
		max(ice$Year,na.rm=T),
		"median",
		" 5%-95%",
		"25%-75%",
		icenow$Year[1],
		"",
		""
		)
	)
# Lower Plot of Extent
plot(
	icemeans[,c("Jday","Extent")],
	pch=16,
	axes=F,
	xlab="Day of the year",
	xlim=c(1,366),
	ylab=Extentlab,
	ylim=c(min(pretty(ice$Extent)),max(pretty(ice$Extent))))
title(sprintf("Smoothed over %i days",2*Nspan+1))
monthString	<- strftime(monthDates,format="%b %d")
monthJdayString	<- strftime(monthDates,format="%j")
monthJdays	<- as.numeric(monthJdayString)
axis(1,
	at=xtics$at,
	label=xtics$label)
axis(2,
	at=pretty(c(min(icemeans$Extent05),max(icemeans$Extent95))))
polygon(c(icemeans$Jday, rev(icemeans$Jday)),
	c(icemeans$Extent95, rev(icemeans$Extent05)),
	col = "grey80",
	border = NA)
polygon(c(icemeans$Jday, rev(icemeans$Jday)),
	c(icemeans$Extent75, rev(icemeans$Extent25)),
	col = "grey60",
	border = NA)
points(ice[,c("JJJ","medianExtent")],col=ice$color,pch=16,cex=.3)
lines(icemeans[,c("Jday","Extent")],col="white",lw=2)
lines(icenow[,c("JJJ","medianExtent")],col="blue")
lines(x=icemeans$Jday,y=0*icemeans$Jday,lty=5,col="black",lw=1)
}
ploticeanomaly	<-	function(ice,icenow,icemeans,icename="",Nspan=NA,xtics=list(at=c(1,3),label=c("Jan 1","Dec 31"))){
Extentlab	<-	paste(icename," Extent Anomaly (10^6 km^2)")
dExtentlab	<-	paste("Anomolous Rate of Change in",icename,"(10^6 km^2 / day)")
par(mfrow=c(2,1))
#Upper plot of Rate
plot(
	x=0,
	y=0,
	axes=F,
	xlab="Day of the year",
	xlim=c(1,366),
	ylab=dExtentlab,
	ylim=c(-.15,.15))
	if(!is.na(Nspan)){title(sprintf("Rate of change calculated over %i days",2*Nspan+1))}
axis(1,
	at=xtics$at,
	label=xtics$label)
axis(2,
	at=pretty(c(min(icemeans$dExtent05A),max(icemeans$dExtent95A))))
polygon(c(icemeans$Jday, rev(icemeans$Jday)),
	c(icemeans$dExtent95A, rev(icemeans$dExtent05A)),
	col = "grey80",
	border = NA)
polygon(c(icemeans$Jday, rev(icemeans$Jday)),
	c(icemeans$dExtent75A, rev(icemeans$dExtent25A)),
	col = "grey60",
	border = NA)
points(ice[,c("JJJ","dExtentAnomaly")],col=ice$color,pch=16,cex=.3)
lines(icenow[,c("JJJ","dExtentAnomaly")],col="blue")
lines(x=icemeans$Jday,y=0*icemeans$Jday,lty=5,col="black",lw=1)
legend("bottom",
	bty="n",
	ncol=3,
	border=c(
		rkpal(3)[[1]],
		rkpal(3)[[2]],
		rkpal(3)[[3]],
		"white",
		"grey80",
		"grey60",
		"blue",
		"white",
		"white"
		),
	fill=c(
		ice$color[ice$Year==min(ice$Year)][1],
		ice$color[ice$Year==median(ice$Year)][1],
		ice$color[ice$Year==max(ice$Year)][1],
		"white",
		"grey80",
		"grey60",
		"blue",
		"white",
		"white"
		),
	legend=c(
		min(ice$Year,na.rm=T),
		"to",
		max(ice$Year,na.rm=T),
		"",
		" 5%-95%",
		"25%-75%",
		icenow$Year[1],
		"",
		""
		)
	)
# Lower Plot of Extent
plot(
	x=0,
	y=0,
	axes=F,
	xlab="Day of the year",
	xlim=c(1,366),
	ylab=Extentlab,
	ylim=c(min(pretty(ice$ExtentAnomaly)),max(pretty(ice$ExtentAnomaly))))
title(sprintf("Smoothed over %i days",2*Nspan+1))
axis(1,
	at=xtics$at,
	label=xtics$label)
axis(2,
	at=pretty(c(min(icemeans$Extent05A),max(icemeans$Extent95A))))
polygon(c(icemeans$Jday, rev(icemeans$Jday)),
	c(icemeans$Extent95A, rev(icemeans$Extent05A)),
	col = "grey80",
	border = NA)
polygon(c(icemeans$Jday, rev(icemeans$Jday)),
	c(icemeans$Extent75A, rev(icemeans$Extent25A)),
	col = "grey60",
	border = NA)
points(ice[,c("JJJ","ExtentAnomaly")],col=ice$color,pch=16,cex=.3)
lines(icenow[,c("JJJ","ExtentAnomaly")],col="blue")
lines(x=icemeans$Jday,y=0*icemeans$Jday,lty=5,col="black",lw=1)	
}
# Ingest data sets
NHice <- smoothice(filename='NH_seaice_extent_final_v2.csv',Nspan=Nspan)
NHice2016 <- smoothice(filename='NH_seaice_extent_nrt_v2.csv',Nspan=Nspan)
NHicemeans <- meanice(NHice)
NHice	<- addanomaly(NHice,NHicemeans)
NHice2016	<- addanomaly(NHice2016,NHicemeans)
#
Nyears	<-	length(unique(NHice$Year))
NHice$color <- rkpal(Nyears)[as.numeric(cut(NHice$Year,breaks=Nyears))]
#
png(filename="NHice.png",
	width=pngsize$width,
	height=pngsize$height)
plotice(
	NHice,
	NHice2016,
	NHicemeans,
	icename="Arctic Sea Ice",
	Nspan=Nspan,
	xtics=xtics
	)
dev.off()
png(filename="NHiceAnomaly.png",
	width=pngsize$width,
	height=pngsize$height)
ploticeanomaly(
	NHice,
	NHice2016,
	NHicemeans,
	icename="Arctic Sea Ice",
	Nspan=Nspan,
	xtics=xtics
	)
dev.off()
#
SHice <- smoothice(filename='SH_seaice_extent_final_v2.csv',Nspan=Nspan)
SHice2016 <- smoothice(filename='SH_seaice_extent_nrt_v2.csv',Nspan=Nspan)
SHicemeans <- meanice(SHice)
SHice	<- addanomaly(SHice,SHicemeans)
SHice2016	<- addanomaly(SHice2016,SHicemeans)
#
Nyears	<-	length(unique(SHice$Year))
SHice$color <- rkpal(Nyears)[as.numeric(cut(SHice$Year,breaks=Nyears))]
#
png(filename="SHice.png",
	width=pngsize$width,
	height=pngsize$height)
plotice(
	SHice,
	SHice2016,
	SHicemeans,
	icename="Antarctic Sea Ice",
	Nspan=Nspan,
	xtics=xtics
	)
dev.off()
png(filename="SHiceAnomaly.png",
	width=pngsize$width,
	height=pngsize$height)
ploticeanomaly(
	SHice,
	SHice2016,
	SHicemeans,
	icename="Antarctic Sea Ice",
	Nspan=Nspan,
	xtics=xtics
	)
dev.off()

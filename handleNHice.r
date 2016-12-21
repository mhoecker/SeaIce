source('handleice.r')
# set plot size
pngsize	<-list(width=1536,height=1024)
# Set colors and plot tics
rkpal	<-	colorRampPalette(c(
	rgb(0,0,0,.5),
	rgb(1,0,0,.5),
	rgb(1,1,0,.5)))
# Dates marking midpoints of months and start and end of year
myMonth	<- data.frame(date=as.Date(c(
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
	)))
myMonth$String	<- strftime(myMonth$date,format="%b %d")
myMonth$JdayString	<- strftime(myMonth$date,format="%j")
myMonth$Jdays	<- as.numeric(myMonth$JdayString)
xtics	<-	list(at=myMonth$Jdays,label=myMonth$String)
# Set the extent of smoothing
Nspan	<- 3
probs	<-	c(.125,.25,.75,.875)
# Plot ice summary
plotice	<-	function(
	ice,
	icenow,
	icemeans,
	icename="",
	Nspan=NA,
	xtics=list(
		at=c(1,3),
		label=c("Jan 1","Dec 31")
		),
	probs=c(NA,NA,NA,NA)
		){
histpch	<-	1
histcex	<-	.2
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
	ylim=range(pretty(ice$dExtent))
	)
	if(!is.na(Nspan)){title(sprintf("Rate of change calculated over %i days",2*Nspan+1))}
axis(1,
	at=xtics$at,
	label=xtics$label)
axis(2,
	at=pretty(range(c(icemeans$dExtent1,icemeans$dExtent4))))
polygon(c(icemeans$Jday, rev(icemeans$Jday)),
	c(icemeans$dExtent4, rev(icemeans$dExtent1)),
	col = "grey80",
	border = NA)
polygon(c(icemeans$Jday, rev(icemeans$Jday)),
	c(icemeans$dExtent3, rev(icemeans$dExtent2)),
	col = "grey60",
	border = NA)
# Plot median
lines(icemeans[,c("Jday","dExtent")],col="white",lw=4)
# Plot historical values
points(ice[,c("JJJ","dExtent")],col=ice$color,pch=histpch,cex=histcex)
lines(icenow[,c("JJJ","dExtent")],col="blue",lw=2)
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
		paste(probs[1]*100,"%-",100*probs[4],"%",sep=""),
		paste(probs[2]*100,"%-",100*probs[3],"%",sep=""),
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
	ylim=range(pretty(ice$Extent))
	)
title(sprintf("Smoothed over %i days",2*Nspan+1))
axis(1,
	at=xtics$at,
	label=xtics$label)
axis(2,
	at=pretty(range(c(icemeans$Extent1,icemeans$Extent4))))
polygon(c(icemeans$Jday, rev(icemeans$Jday)),
	c(icemeans$Extent4, rev(icemeans$Extent1)),
	col = "grey80",
	border = NA)
polygon(c(icemeans$Jday, rev(icemeans$Jday)),
	c(icemeans$Extent3, rev(icemeans$Extent2)),
	col = "grey60",
	border = NA)
lines(icemeans[,c("Jday","Extent")],col="white",lw=4)
points(ice[,c("JJJ","medianExtent")],col=ice$color,pch=histpch,cex=histcex)
lines(icenow[,c("JJJ","medianExtent")],col="blue",lw=2)
lines(x=icemeans$Jday,y=0*icemeans$Jday,lty=5,col="black",lw=1)
}
ploticeanomaly	<-	function(
	ice,
	icenow,
	icemeans,
	icename="",
	Nspan=NA,
	xtics=
		list(at=c(1,3),
		label=c("Jan 1","Dec 31")),
	probs=c(NA,NA,NA,NA)){
Extentlab	<-	paste(icename," Extent Anomaly (10^6 km^2)")
dExtentlab	<-	paste("Anomolous Rate of Change in",icename,"(10^6 km^2 / day)")
par(mfrow=c(2,1))
histpch	<-	1
histcex	<-	.2
#Upper plot of Rate
dExtentlims	<-	range(pretty(c(ice$dExtentA,icenow$dExtentA)))
plot(
	x=0,
	y=0,
	axes=F,
	xlab="Day of the year",
	xlim=c(1,366),
	ylab=dExtentlab,
	ylim=dExtentlims)
	if(!is.na(Nspan)){title(sprintf("Rate of change calculated over %i days",2*Nspan+1))}
axis(1,
	at=xtics$at,
	label=xtics$label)
axis(2,
	at=pretty(dExtentlims))
polygon(c(icemeans$Jday, rev(icemeans$Jday)),
	c(icemeans$dExtent4A, rev(icemeans$dExtent1A)),
	col = "grey80",
	border = NA)
polygon(c(icemeans$Jday, rev(icemeans$Jday)),
	c(icemeans$dExtent3A, rev(icemeans$dExtent2A)),
	col = "grey60",
	border = NA)
points(ice[,c("JJJ","dExtentA")],col=ice$color,pch=histpch,cex=histcex)
lines(icenow[,c("JJJ","dExtentA")],col="blue",lw=2)
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
		paste(probs[1]*100,"%-",100*probs[4],"%",sep=""),
		paste(probs[2]*100,"%-",100*probs[3],"%",sep=""),
		icenow$Year[1],
		"",
		""
		)
	)
# Lower Plot of Extent
Extentlims	<-	range(pretty(c(ice$ExtentA,icenow$ExtentA)))
plot(
	x=0,
	y=0,
	axes=F,
	xlab="Day of the year",
	xlim=c(1,366),
	ylab=Extentlab,
	ylim=Extentlims
	)
title(sprintf("Smoothed over %i days",2*Nspan+1))
axis(1,
	at=xtics$at,
	label=xtics$label)
axis(2,
	at=pretty(Extentlims))
polygon(c(icemeans$Jday, rev(icemeans$Jday)),
	c(icemeans$Extent4A, rev(icemeans$Extent1A)),
	col = "grey80",
	border = NA)
polygon(c(icemeans$Jday, rev(icemeans$Jday)),
	c(icemeans$Extent3A, rev(icemeans$Extent2A)),
	col = "grey60",
	border = NA)
points(ice[,c("JJJ","ExtentA")],col=ice$color,pch=histpch,cex=histcex)
lines(icenow[,c("JJJ","ExtentA")],col="blue",lw=2)
lines(x=icemeans$Jday,y=0*icemeans$Jday,lty=5,col="black",lw=1)	
}
# Ingest data sets
NHice	<-	load.ice(file="NHfinal_smooth.dat")
NHice2016	<-	load.ice(file="NHnrt_smooth.dat")
NHicemeans <- load.ice(file="NHsmooth_mean.dat")
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
	xtics=xtics,
	probs=probs
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
	xtics=xtics,
	probs=probs
	)
dev.off()
#
SHice	<-	load.ice(file="SHfinal_smooth.dat")
SHice2016	<-	load.ice(file="SHnrt_smooth.dat")
SHicemeans <- load.ice(file="SHsmooth_mean.dat")
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
	xtics=xtics,
	probs=probs
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
	xtics=xtics,
	probs=probs
	)
dev.off()

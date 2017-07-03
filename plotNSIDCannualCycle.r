# set plot size
pngsize	<-list(width=1536,height=1024)
# Set colors and plot tics
require(RColorBrewer)
rkpal	<-	colorRampPalette(
	rev(brewer.pal(n=11,name="RdYlBu"))
	)
# Dates marking end of weeks in each month end of year
myMonth	<- data.frame(date=as.Date(c(
	"2016-01-01",
#	"2016-01-07",
	"2016-01-14",
#	"2016-01-21",
	"2016-01-28",
#	"2016-01-31",
#	"2016-02-07",
	"2016-02-14",
#	"2016-02-21",
	"2016-02-28",
#	"2016-03-07",
	"2016-03-14",
#	"2016-03-21",
	"2016-03-28",
#	"2016-03-31",
#	"2016-04-07",
	"2016-04-14",
#	"2016-04-21",
	"2016-04-28",
#	"2016-04-30",
#	"2016-05-07",
	"2016-05-14",
#	"2016-05-21",
	"2016-05-28",
#	"2016-05-31",
#	"2016-06-07",
	"2016-06-14",
#	"2016-06-21",
	"2016-06-28",
#	"2016-06-30",
#	"2016-07-07",
	"2016-07-14",
#	"2016-07-21",
	"2016-07-28",
#	"2016-07-31",
#	"2016-08-07",
	"2016-08-14",
#	"2016-08-21",
	"2016-08-28",
#	"2016-08-31",
#	"2016-09-07",
	"2016-09-14",
#	"2016-09-21",
	"2016-09-28",
#	"2016-09-30",
#	"2016-10-07",
	"2016-10-14",
#	"2016-10-21",
	"2016-10-28",
#	"2016-10-31",
#	"2016-11-07",
	"2016-11-14",
#	"2016-11-21",
	"2016-11-28",
#	"2016-11-30",
#	"2016-12-07",
	"2016-12-14",
#	"2016-12-21",
	"2016-12-28",
	"2016-12-31"
#
#
#	"2016-01-01",
#	"2016-01-15",
#	"2016-02-15",
#	"2016-03-15",
#	"2016-04-15",
#	"2016-05-15",
#	"2016-06-15",
#	"2016-07-15",
#	"2016-08-15",
#	"2016-09-15",
#	"2016-10-15",
#	"2016-11-15",
#	"2016-12-15",
#	"2016-12-31"
	)))
myMonth$String	<- strftime(myMonth$date,format="%b %d")
myMonth$JdayString	<- strftime(myMonth$date,format="%j")
myMonth$Jdays	<- as.numeric(myMonth$JdayString)
xtics	<-	list(at=c(myMonth$Jdays-366,myMonth$Jdays,myMonth$Jdays+366),label=c(myMonth$String,myMonth$String,myMonth$String))
# Plot ice summary
plotice	<-	function(
	ice,
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
histcex	<-	.1
histlw	<-	1
Extentlab	<-	paste(icename,"Extent (10^6 km^2)")
dExtentlab	<-	paste(icename,"Change","(10^6 km^2 / day)")
xrange	<-	as.numeric(strftime(Sys.Date(),format="%j"))+c(-178,178)
par(mfrow=c(2,1),cex=1.5)
#Upper plot of Rate
plot(
	icemeans[,c("Jday","dExtent")],
	pch=16,
	axes=F,
	xlab="Day of the year",
	xlim=xrange,
	ylab=dExtentlab,
	ylim=range(pretty(ice$dExtent)),
	col="white"
	)
	if(!is.na(Nspan)){title(sprintf("Rate of change calculated over %i days",2*Nspan+1))}
axis(1,
	at=xtics$at,
	label=xtics$label)
axis(2,
	at=pretty(range(c(icemeans$dExtent1,icemeans$dExtent4))))
# Plot historical values
	for(yr in unique(ice$Year)){
		thelw	<-	max(c(histlw,histlw+(yr+ceiling(Nspan/10)-max(ice$Year))))
		x	<-	c(ice[ice$Year==yr-1,"JJJ"]-366,ice[ice$Year==yr,"JJJ"],ice[ice$Year==yr+1,"JJJ"]+366)
		y	<-	c(ice[ice$Year==yr-1,"dExtent"],ice[ice$Year==yr,"dExtent"],ice[ice$Year==yr+1,"dExtent"])
		#lines(x=x,y=y,col="black",lw=thelw+1)		
		lines(x=x,y=y,col=ice$color[ice$Year==yr],lw=thelw)		
	}
# Plot median
x	<-	c(icemeans[,"Jday"]-366,icemeans[,"Jday"],icemeans[,"Jday"]+366)
y	<-	c(icemeans[,"dExtent"],icemeans[,"dExtent"],icemeans[,"dExtent"])
lines(x=x,y=y,col="black",lw=5)
lines(x=x,y=y,col="white",lw=3)
lines(x=xrange,y=0*xrange,lty=5,col="black",lw=2)
legend("bottom",
	bty="n",
	ncol=4,
	border=c(
		"black",
		"black",
		"black",
		"black"
		),
	fill=c(
		rkpal(3)[[1]],
		rkpal(3)[[2]],
		rkpal(3)[[3]],
		"white"
		),
	legend=c(
		min(ice$Year,na.rm=T),
		"to",
		max(ice$Year,na.rm=T),
		"median"
		)
	)
# Lower Plot of Extent
plot(
	icemeans[,c("Jday","medianExtent")],
	pch=16,
	axes=F,
	xlab="Day of the year",
	xlim=xrange,
	ylab=Extentlab,
	ylim=range(pretty(ice$medianExtent)),
	col="white"
	)
title(sprintf("Smoothed over %i days",2*Nspan+1))
axis(1,
	at=xtics$at,
	label=xtics$label)
axis(2,
	at=pretty(range(c(icemeans$medianExtent1,icemeans$medianExtent4))))
	for(yr in unique(ice$Year)){
		thelw	<-	max(c(histlw,histlw+(yr+ceiling(Nspan/10)-max(ice$Year))))
		x	<-	c(ice[ice$Year==yr-1,"JJJ"]-366,ice[ice$Year==yr,"JJJ"],ice[ice$Year==yr+1,"JJJ"]+366)
		y	<-	c(ice[ice$Year==yr-1,"medianExtent"],ice[ice$Year==yr,"medianExtent"],ice[ice$Year==yr+1,"medianExtent"])
		#lines(x=x,y=y,col="black",lw=thelw+1)		
		lines(x=x,y=y,col=ice$color[ice$Year==yr],lw=thelw)		
	}
#median
x	<-	c(icemeans[,"Jday"]-366,icemeans[,"Jday"],icemeans[,"Jday"]+366)
y	<-	c(icemeans[,"medianExtent"],icemeans[,"medianExtent"],icemeans[,"medianExtent"])
lines(x=x,y=y,col="black",lw=5)
lines(x=x,y=y,col="white",lw=3)
lines(x=xrange,y=0*xrange,lty=5,col="black",lw=2)
}
ploticeanomaly	<-	function(
	ice,
	icemeans,
	icename="",
	Nspan=NA,
	xtics=
		list(at=c(1,3),
		label=c("Jan 1","Dec 31")),
	probs=c(NA,NA,NA,NA)){
	Extentlab	<-	paste(icename,"Extent Anomaly (10^6 km^2)")
	dExtentlab	<-	paste(icename,"Change Anomoly (10^6 km^2 / day)")
	par(mfrow=c(2,1),cex=1.5)
	histpch	<-	1
	histcex	<-	.1
	histlw	<-	1
	#Upper plot of Rate
	dExtentlims	<-	range(pretty(1.3*c(icemeans$dExtent1A,icemeans$dExtent4A)))
	plot(
		x=-366,
		y=0,
		axes=F,
		xlab="Day of the year",
		xlim=c(1,366),
		ylab=dExtentlab,
		ylim=dExtentlims,
		col="white"
		)
	if(!is.na(Nspan)){title(sprintf("Rate of change calculated over %i days\nMedian Seasonal Cycle Removed",2*Nspan+1))}
	axis(1,
		at=xtics$at,
		label=xtics$label)
	axis(2,
		at=pretty(dExtentlims))
	x	<-	c(icemeans$Jday-366,icemeans$Jday,icemeans$Jday+366)
	x	<-	c(x,rev(x))
	y	<-	c(
		icemeans$dExtent4A,
		icemeans$dExtent4A,
		icemeans$dExtent4A,
		rev(icemeans$dExtent1A),
		rev(icemeans$dExtent1A),
		rev(icemeans$dExtent1A))
	polygon(x,
		y,
		col = "grey80",
		border = NA)
	y	<-	c(
		icemeans$dExtent3A,
		icemeans$dExtent3A,
		icemeans$dExtent3A,
		rev(icemeans$dExtent2A),
		rev(icemeans$dExtent2A),
		rev(icemeans$dExtent2A))
	polygon(x,
		y,
		col = "grey60",
		border = NA)
	for(yr in unique(ice$Year)){
		if(yr>max(ice$Year)-1-ceiling(Nspan/10)){
			thelw	<-	max(c(histlw,histlw+(yr+ceiling(Nspan/10)-max(ice$Year))))
			x	<-	ice[ice$Year==yr,"JJJ"]
			x	<-	c(ice[ice$Year==yr-1,"JJJ"]-366,x,ice[ice$Year==yr+1,"JJJ"]+366)
			y	<-	c(ice[ice$Year==yr-1,"dExtentA"],ice[ice$Year==yr,"dExtentA"],ice[ice$Year==yr+1,"dExtentA"])
			#lines(x=x,y=y,col="black",lw=thelw+1)		
			lines(x=x,y=y,col=ice$color[ice$Year==yr],lw=thelw)		
		}
	}
	lines(x=c(-366,2*366),y=c(0,0),lty=5,col="black",lw=2)
	legend("bottom",
		bty="n",
		ncol=2,
		border=c(
			"black",
			"black",
			"black",
			"white",
			"grey80",
			"grey60"
			),
		fill=c(
			rkpal(3)[[1]],
			rkpal(3)[[2]],
			rkpal(3)[[3]],
			"white",
			"grey80",
			"grey60"
			),
		legend=c(
			min(ice$Year,na.rm=T),
			"to",
			max(ice$Year,na.rm=T),
			"",
			paste(probs[1]*100,"%-",100*probs[4],"%",sep=""),
			paste(probs[2]*100,"%-",100*probs[3],"%",sep="")
			)
		)
	# Lower Plot of Extent
	Extentlims	<-	range(pretty(1.3*c(icemeans$medianExtent1A,icemeans$medianExtent4A)))
	plot(
		x=-366,
		y=0,
		axes=F,
		xlab="Day of the year",
		xlim=c(1,366),
		ylab=Extentlab,
		ylim=Extentlims
		)
	title(sprintf("Smoothed over %i days\nMedian Seasonal Cycle Removed",2*Nspan+1))
	axis(1,
		at=xtics$at,
		label=xtics$label)
	axis(2,
		at=pretty(Extentlims))
	x	<-	c(icemeans$Jday-366,icemeans$Jday,icemeans$Jday+366)
	x	<-	c(x,rev(x))
	y	<-	c(icemeans$medianExtent4A,
		icemeans$medianExtent4A,
		icemeans$medianExtent4A,
		rev(icemeans$medianExtent1A),
		rev(icemeans$medianExtent1A),
		rev(icemeans$medianExtent1A))
	polygon(x,
		y,
		col = "grey80",
		border = NA)
	y	<-	c(
		icemeans$medianExtent3A,
		icemeans$medianExtent3A,
		icemeans$medianExtent3A,
		rev(icemeans$medianExtent2A),
		rev(icemeans$medianExtent2A),
		rev(icemeans$medianExtent2A))
	polygon(x,
		y,
		col = "grey60",
		border = NA)
	for(yr in unique(ice$Year)){
		thelw	<-	max(c(histlw,histlw+(yr+ceiling(Nspan/10)-max(ice$Year))))
		x	<-	c(ice[ice$Year==yr-1,"JJJ"]-366,ice[ice$Year==yr,"JJJ"],ice[ice$Year==yr+1,"JJJ"]+366)
		y	<-	c(ice[ice$Year==yr-1,"ExtentA"],ice[ice$Year==yr,"ExtentA"],ice[ice$Year==yr+1,"ExtentA"])
		#lines(x=x,y=y,col="black",lw=thelw+1)
		lines(x=x,y=y,col=ice$color[ice$Year==yr],lw=thelw)		
	}
	lines(x=c(-366,2*366),y=c(0,0),lty=5,col="black",lw=2)	
}
# Plot anomaly by year
plotanomalytrend	<-	function(
	ice,
	icemeans,
	icename="",
	Nspan=NA,
	probs=c(NA,NA,NA,NA)){
	#
	xtics=
		list(
			at=pretty(range(ice$Year)),
			label=pretty(range(ice$Year)),
			range=range(ice$Year)
			)
	Extentlab	<-	paste(icename,"Extent Anomaly (10^6 km^2)")
	Extentlims	<-	range(pretty(ice$ExtentA))
	histpch	<-	16
	histcex	<-	1
	histlw	<-	4
	par(cex=1.5)
	plot(
		x=0,
		y=0,
		axes=F,
		xlab="Year",
		xlim=xtics$range,
		ylab=Extentlab,
		ylim=Extentlims,
		col="white"
		)
	if(!is.na(Nspan)){title(sprintf("Median Seasonal Cycle Removed\n Smoothed over %i days",2*Nspan+1))}
	axis(1,
		at=xtics$at)
	axis(2,
		at=pretty(Extentlims))
	for(yr in unique(ice$Year)){
		thisyr	<-	ice$Year==yr
		x	<-	ice$JJJ[thisyr]
		x	<-	ice$Year[thisyr]+(x-.5)/max(c(x,365))
		xmean	<- yr+(icemeans$Jday-.5)/365
		xmean	<-	c(xmean,rev(xmean))
		polygon(x=xmean,y=c(icemeans$medianExtent1A,rev(icemeans$medianExtent4A)),col="grey80",border = NA)
		polygon(x=xmean,y=c(icemeans$medianExtent2A,rev(icemeans$medianExtent3A)),col="grey60",border = NA)
		lines(x=x,y=ice$medianExtentA[thisyr],col="black",lw=2+histlw)
		lines(x=x,y=ice$medianExtentA[thisyr],col=ice$color[thisyr],lw=histlw)
	}
	legend("bottom",
		bty="n",
		ncol=2,
		border=c(
			"black",
			"black",
			"black",
			"white",
			"grey80",
			"grey60"
			),
		fill=c(
			rkpal(3)[[1]],
			rkpal(3)[[2]],
			rkpal(3)[[3]],
			"white",
			"grey80",
			"grey60"
			),
		legend=c(
			min(ice$Year,na.rm=T),
			"to",
			max(ice$Year,na.rm=T),
			"Percentile Bounds",
			paste(probs[1]*100,"%-",100*probs[4],"%",sep=""),
			paste(probs[2]*100,"%-",100*probs[3],"%",sep="")
			)
		)
}

# Ingest data sets
NHice	<-	load.ice(file="NHsmooth.dat")
NHicemeans <- load.ice(file="NHsmooth_mean.dat")
#
Nyears	<-	length(unique(NHice$Year))
NHice$color <- rkpal(length(unique(NHice$Year)))[as.numeric(cut(NHice$Year,breaks=Nyears))]
#
png(filename="NHice.png",
	width=pngsize$width,
	height=pngsize$height)
plotice(
	NHice,
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
	NHicemeans,
	icename="Arctic Sea Ice",
	Nspan=Nspan,
	xtics=xtics,
	probs=probs
	)
dev.off()
png(filename="NHiceAnomalyTrend.png",
	width=pngsize$width,
	height=pngsize$height)
plotanomalytrend(
	NHice,
	NHicemeans,
	icename="Arctic Sea Ice",
	Nspan=Nspan,
	probs=probs
	)
dev.off()
#
SHice	<-	load.ice(file="SHsmooth.dat")
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
	SHicemeans,
	icename="Antarctic Sea Ice",
	Nspan=Nspan,
	xtics=xtics,
	probs=probs
	)
dev.off()
#
png(filename="SHiceAnomalyTrend.png",
	width=pngsize$width,
	height=pngsize$height)
plotanomalytrend(
	SHice,
	SHicemeans,
	icename="Antarctic Sea Ice",
	Nspan=Nspan,
	probs=probs
	)
dev.off()


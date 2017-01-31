# set plot size
pngsize	<-list(width=1536,height=1024)
# Set colors and plot tics
rkpal	<-	colorRampPalette(c(
	rgb(0.1,0.1,.7),
	rgb(1,1,.4),
	rgb(.7,.1,.1))
	)
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
par(mfrow=c(2,1),cex=1.5)
#Upper plot of Rate
plot(
	icemeans[,c("Jday","dExtent")],
	pch=16,
	axes=F,
	xlab="Day of the year",
	xlim=c(1,366),
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
polygon(c(icemeans$Jday, rev(icemeans$Jday)),
	c(icemeans$dExtent4, rev(icemeans$dExtent1)),
	col = "grey80",
	border = NA)
polygon(c(icemeans$Jday, rev(icemeans$Jday)),
	c(icemeans$dExtent3, rev(icemeans$dExtent2)),
	col = "grey60",
	border = NA)
# Plot historical values
for(yr in unique(ice$Year)){
	if(yr==max(ice$Year)){
		lines(ice[ice$Year==yr,c("JJJ","dExtent")],col=ice$color[ice$Year==yr],lw=3*histlw)
	}else{
		lines(ice[ice$Year==yr,c("JJJ","dExtent")],col=ice$color[ice$Year==yr],lw=histlw)
	}
}
# Plot median
lines(icemeans[,c("Jday","dExtent1")],col="grey80",lw=2)
lines(icemeans[,c("Jday","dExtent4")],col="grey80",lw=2)
lines(icemeans[,c("Jday","dExtent2")],col="grey60",lw=3)
lines(icemeans[,c("Jday","dExtent3")],col="grey60",lw=3)
lines(icemeans[,c("Jday","dExtent")],col="white",lw=4)
lines(ice[ice$Year==max(ice$Year),c("JJJ","dExtent")],col=ice$color[ice$Year==max(ice$Year)],lw=histlw)
lines(x=icemeans$Jday,y=0*icemeans$Jday,lty=5,col="black",lw=1)
legend("bottom",
	bty="n",
	ncol=2,
	border=c(
		rkpal(3)[[1]],
		rkpal(3)[[2]],
		rkpal(3)[[3]],
		"black",
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
		"median",
		paste(probs[1]*100,"%-",100*probs[4],"%",sep=""),
		paste(probs[2]*100,"%-",100*probs[3],"%",sep="")
		)
	)
# Lower Plot of Extent
plot(
	icemeans[,c("Jday","medianExtent")],
	pch=16,
	axes=F,
	xlab="Day of the year",
	xlim=c(1,366),
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
polygon(c(icemeans$Jday, rev(icemeans$Jday)),
	c(icemeans$medianExtent4, rev(icemeans$medianExtent1)),
	col = "grey80",
	border = NA)
polygon(c(icemeans$Jday, rev(icemeans$Jday)),
	c(icemeans$medianExtent3, rev(icemeans$medianExtent2)),
	col = "grey60",
	border = NA)
for(yr in unique(ice$Year)){
	if(yr==max(ice$Year)){
		lines(ice[ice$Year==yr,c("JJJ","medianExtent")],col=ice$color[ice$Year==yr],lw=3*histlw)
	}else{
		lines(ice[ice$Year==yr,c("JJJ","medianExtent")],col=ice$color[ice$Year==yr],lw=histlw)
	}
}
#median
lines(icemeans[,c("Jday","medianExtent1")],col="grey80",lw=2)
lines(icemeans[,c("Jday","medianExtent4")],col="grey80",lw=2)
lines(icemeans[,c("Jday","medianExtent2")],col="grey60",lw=3)
lines(icemeans[,c("Jday","medianExtent3")],col="grey60",lw=3)
lines(icemeans[,c("Jday","medianExtent")],col="white",lw=4)
lines(x=icemeans$Jday,y=0*icemeans$Jday,lty=5,col="black",lw=1)
lines(ice[ice$Year==max(ice$Year),c("JJJ","medianExtent")],col=ice$color[ice$Year==max(ice$Year)],lw=histlw)
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
	dExtentlims	<-	range(pretty(ice$dExtentA))
	plot(
		x=0,
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
	polygon(c(icemeans$Jday, rev(icemeans$Jday)),
		c(icemeans$dExtent4A, rev(icemeans$dExtent1A)),
		col = "grey80",
		border = NA)
	polygon(c(icemeans$Jday, rev(icemeans$Jday)),
		c(icemeans$dExtent3A, rev(icemeans$dExtent2A)),
		col = "grey60",
		border = NA)
	for(yr in unique(ice$Year)){
		if(yr==max(ice$Year)){
			lines(ice[ice$Year==yr,c("JJJ","dExtentA")],col=ice$color[ice$Year==yr],lw=3*histlw)
		}else{
			lines(ice[ice$Year==yr,c("JJJ","dExtentA")],col=ice$color[ice$Year==yr],lw=histlw)		
		}
	}
	lines(x=icemeans$Jday,y=0*icemeans$Jday,lty=5,col="black",lw=1)
	legend("bottom",
		bty="n",
		ncol=2,
		border=c(
			rkpal(3)[[1]],
			rkpal(3)[[2]],
			rkpal(3)[[3]],
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
	Extentlims	<-	range(pretty(ice$medianExtentA))
	plot(
		x=0,
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
	polygon(c(icemeans$Jday, rev(icemeans$Jday)),
		c(icemeans$medianExtent4A, rev(icemeans$medianExtent1A)),
		col = "grey80",
		border = NA)
	polygon(c(icemeans$Jday, rev(icemeans$Jday)),
		c(icemeans$medianExtent3A, rev(icemeans$medianExtent2A)),
		col = "grey60",
		border = NA)
	for(yr in unique(ice$Year)){
		if(yr==max(ice$Year)){
			lines(ice[ice$Year==yr,c("JJJ","medianExtentA")],col=ice$color[ice$Year==yr],lw=3*histlw)
		}else{
			lines(ice[ice$Year==yr,c("JJJ","medianExtentA")],col=ice$color[ice$Year==yr],lw=histlw)
		}
	}
	lines(x=icemeans$Jday,y=0*icemeans$Jday,lty=5,col="black",lw=1)	
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
	histlw	<-	1
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
		lines(x=x,y=ice$medianExtentA[thisyr],col=ice$color[thisyr],lw=histlw)
	}
	legend("bottom",
		bty="n",
		ncol=2,
		border=c(
			rkpal(3)[[1]],
			rkpal(3)[[2]],
			rkpal(3)[[3]],
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


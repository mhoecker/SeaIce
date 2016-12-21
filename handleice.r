#handle ice
# functions for processing NSIDC ice extent .csv flies
#
# Read in the data into a data.frame
read.ice	<-	function(
	file
	){
	ice <- read.csv(file=file,header=F,skip=2,nrows=-1)
	colnames(ice) <- colnames(read.csv(file=file,header=T,nrows=2))
	ice$dateString <- sprintf("%04i-%02i-%02i",ice$Year,ice$Month,ice$Day)
	ice$date <- as.Date(ice$dateString)
	ice$JJJ <- as.numeric(strftime(ice$date,format="%j"))
	return(ice)
}
# Save ice extent to file
save.ice	<-	function(
	ice,
	file=stop("'file' must be specified")
	){
	save('ice',file=file)
}
# Load ice extent from file
load.ice	<-	function(
	file
	){
		load(file=file)
		return(ice)
}
#Read final ice extent into R data files
ingest.final	<-	function(finalfile,
	outname,
	Nspan=3,
	probs=c(.125,.25,.75,.875)
	){
	final	<-	read.ice(finalfile)
	#
	final_mean	<-	meanice(final)
	save.ice(final_mean,file=paste(outname,"mean.dat",sep=""))
	#
	final	<-	addanomaly(final,final_mean)
	save.ice(final,file=paste(outname,"final.dat",sep=""))
	#
	final_smooth	<-	smoothice(final,Nspan=Nspan)
	#
	final_smooth_mean	<-	meanice(final_smooth)
	save.ice(final_smooth_mean,file=paste(outname,"smooth_mean.dat",sep=""))
	#
	final_smooth	<-	addanomaly(final_smooth,final_smooth_mean)
	save.ice(final_smooth,file=paste(outname,"final_smooth.dat",sep=""))
}
ingest.nrt	<- function(nrtfile,
	outname,
	Nspan=3,
	probs=c(.125,.25,.75,.875),
	loadfinal=T
	){
	#
	nrt	<-	read.ice(nrtfile)
	nrt_smooth	<-	smoothice(nrt,Nspan=Nspan)
	# load and incorporate statistics from final data set
	if(loadfinal){
		final_mean	<-	load.ice(paste(outname,"mean.dat",sep="")) 
		final_smooth_mean	<-	load.ice(paste(outname,"smooth_mean.dat",sep=""))
		ls()
		nrt	<-	addanomaly(nrt,final_mean)
		nrt_smooth	<-	addanomaly(nrt_smooth,final_smooth_mean)
	}
	#
	save.ice(nrt,file=paste(outname,"nrt.dat",sep=""))
	save.ice(nrt_smooth,file=paste(outname,"nrt_smooth.dat",sep=""))
}
ingest.ice	<-	function(
	finalfile,
	nrtfile,
	outname,
	Nsapn=3,
	probs=c(.125,.25,.75,.875)
	){
	# do the final file
	ingest.final(finalfile=finalfile,
	outname=outname,
	Nspan=Nspan,
	probs=probs)
	# do the nrt file
	ingest.nrt(nrtfile=nrtfile,
	outname=outname,
	Nspan=Nspan,
	probs=probs
	)
}
#Smooth ice data by fitting a local linear regression
smoothice	<-	function(
	ice,
	Nspan=3){
	ice$dExtent <- ice$Extent
	ice$medianExtent <- ice$Extent
	for( i in 1:length(ice$Year)){
		datarange <- 1:length(ice$Year)
		datarange <- abs(ice$date-ice$date[i]) < Nspan+1
		subdata	<- data.frame(
			x=as.numeric(ice$date[datarange]-ice$date[i]),
			y=ice$Extent[datarange]
			)
		fitmodel <-	lm(
			formula= y ~ poly(x,1,raw=T),
			data=subdata)
		ice$dExtent[i] <- fitmodel$coefficients[2]
		ice$medianExtent[i] <- fitmodel$coefficients[1]
	}
	return(ice)
}
# Interpolate onto a regular Date interval
interpolate.ice	<-	function(
	ice, #data.frame of ice variables (e.g. from read.ice)
	order=3 #order of interpolation 0=constant, 1=linear, 3=spline
	){
	daterange	<-	range(ice$date)
	dates	<-	seq(from=daterange[1],to=daterange[2],by=1)
	if(order==0){
		Iice	<-	approx(x=ice$date,y=ice$Extent,xout=dates,method="constant")
	}else if(order==1){
		Iice	<-	approx(x=ice$date,y=ice$Extent,xout=dates,method="linear")
	}else{
		Iice	<-	spline(x=ice$date,y=ice$Extent,xout=dates)
	}
	names(Iice)	<-	c("date","Extent")
	Iice	<-	data.frame(Iice)
	Iice$date	<-	as.Date(Iice$date,origin="1970-01-01")
	str(Iice)
	Iice$JJJ	<-	as.numeric(strftime(Iice$date,format="%j"))
	Iice$Year	<-	as.numeric(strftime(Iice$date,format="%Y"))
	Iice$Month	<-	as.numeric(strftime(Iice$date,format="%m"))
	Iice$Day	<-	as.numeric(strftime(Iice$date,format="%d"))
	Iice$dateString <- strftime(format="%Y-%m-%d",Iice$date)
	# Preserve all the variable names from original data.frame
	for( var in names(ice)){
		if(!(var %in% names(Iice))){
			Iice[var]	<-	NA
		}
	}
	return(Iice)
}
# Calculate the mean for each day and the probability
meanice	<-	function(
	ice,
	probs=c(.125,.25,.75,.875)
	){
	icemeans <-	data.frame(Jday=unique(ice$JJJ))
	icemeans$Extent	<- NA
	icemeans <- icemeans[order(icemeans$Jday),]
	for( j in 1:length(icemeans$Jday)){
		isday	<-	ice$JJJ==icemeans$Jday[j]
		# calculate Extent quantiles
		icemeans$Extent[j] <- median(ice[isday,"Extent"],na.rm=T)
		icemeans$Extent1[j] <- quantile(ice[isday,"Extent"],probs=probs[1],na.rm=T)
		icemeans$Extent2[j] <- quantile(ice[isday,"Extent"],probs=probs[2],na.rm=T)
		icemeans$Extent3[j] <- quantile(ice[isday,"Extent"],probs=probs[3],na.rm=T)
		icemeans$Extent4[j] <- quantile(ice[isday,"Extent"],probs=probs[4],na.rm=T)
		# calculate dExtent quantiles
		if("dExtent" %in% names(ice)){
			icemeans$dExtent[j] <- median(ice[isday,"dExtent"],na.rm=T)
			icemeans$dExtent1[j] <- quantile(ice[isday,"dExtent"],probs=probs[1],na.rm=T)
			icemeans$dExtent2[j] <- quantile(ice[isday,"dExtent"],probs=probs[2],na.rm=T)
			icemeans$dExtent3[j] <- quantile(ice[isday,"dExtent"],probs=probs[3],na.rm=T)
			icemeans$dExtent4[j] <- quantile(ice[isday,"dExtent"],probs=probs[4],na.rm=T)
		}
	}
	# Calculate Anomaly quantiles
	icemeans$Extent1A	<-	icemeans$Extent1-icemeans$Extent
	icemeans$Extent2A	<-	icemeans$Extent2-icemeans$Extent
	icemeans$Extent3A	<-	icemeans$Extent3-icemeans$Extent
	icemeans$Extent4A	<-	icemeans$Extent4-icemeans$Extent
	# if derivative exists calculate anomalies
	if("dExtent" %in% names(ice)){
		icemeans$dExtent1A	<-	icemeans$dExtent1-icemeans$dExtent
		icemeans$dExtent2A	<-	icemeans$dExtent2-icemeans$dExtent
		icemeans$dExtent3A	<-	icemeans$dExtent3-icemeans$dExtent
		icemeans$dExtent4A	<-	icemeans$dExtent4-icemeans$dExtent
	}
	#
	return(icemeans)
}
# add anomaly 
addanomaly	<-	function(
	ice,
	icemeans
	){
	ice$ExtentA	<-	ice$medianExtent
	if('dExtent' %in% names(ice)){
			ice$dExtentA	<-	ice$dExtent
	}
	for( j in 1:length(icemeans$Jday)){
		isday	<-	ice$JJJ==icemeans$Jday[j]
		ice$ExtentA[isday]	<-	ice$ExtentA[isday]-icemeans$Extent[j]
		if('dExtent' %in% names(icemeans)){
			ice$dExtentA[isday]	<-	ice$dExtentA[isday]-icemeans$dExtent[j]
		}
	}
	return(ice)
}

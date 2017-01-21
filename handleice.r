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
	file=stop("'file' must be specified")
	){
		load(file=file)
		return(ice)
}
#Read ice extent into R data files
ingest.ice	<-	function(iceCSVfile,
	outname,
	Nspan=NA,
	probs=NA
	){
	ice	<-	read.ice(iceCSVfile)
	#
	ice_mean	<-	meanice(ice,probs=probs)
	save.ice(ice_mean,file=paste(outname,"mean.dat",sep=""))
	#
	ice	<-	addanomaly(ice,ice_mean)
	save.ice(ice,file=paste(outname,".dat",sep=""))
	#
	ice_smooth	<-	smoothice(ice,Nspan=Nspan)
	#
	ice_smooth_mean	<-	meanice(ice_smooth,probs=probs)
	save.ice(ice_smooth_mean,file=paste(outname,"smooth_mean.dat",sep=""))
	#
	ice_smooth	<-	addanomaly(ice_smooth,ice_smooth_mean)
	save.ice(ice_smooth,file=paste(outname,"smooth.dat",sep=""))
}
#Smooth ice data by fitting a local linear regression
smoothice	<-	function(
	ice,
	Nspan=NA){
	ice$dExtent <- ice$Extent
	ice$medianExtent <- ice$Extent
	for( i in 1:length(ice$Year)){
		datarange <- abs(ice$date-ice$date[i]) < Nspan+1
		subdata	<- data.frame(
			x=as.numeric(ice$date[datarange]-ice$date[i]),
			y=ice$Extent[datarange]
			)
		weights	<-	abs(subdata$x)
		weights	<-	weights/Nspan
		weights	<-	sqrt(weights)
		weights	<-	1-weights
		if(Nspan>42){
			fitmodel <-	lm(
				formula= y ~ poly(x,2,raw=T),
				weights=weights,
				data=subdata)
		}else{
			fitmodel <-	lm(
				formula= y ~ poly(x,1,raw=T),
				weights=weights,
				data=subdata)
		}
		ice$dExtent[i] <- fitmodel$coefficients[2]
		ice$medianExtent[i] <- fitmodel$coefficients[1]
	}
	return(ice)
}
# Interpolate onto a regular Date interval
interpolate.ice	<-	function(
	ice, #data.frame of ice variables (e.g. from read.ice)
	daystep=1, #
	order=3 #order of interpolation 0=constant, 1=linear, 3=spline
	){
	str(ice)
	daterange	<-	range(ice$date)
	dates	<-	seq(from=daterange[1],to=daterange[2],by=daystep)
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
	Iice$JJJ	<-	as.numeric(strftime(Iice$date,format="%j"))
	Iice$Year	<-	as.numeric(strftime(Iice$date,format="%Y"))
	Iice$Month	<-	as.numeric(strftime(Iice$date,format="%m"))
	Iice$Day	<-	as.numeric(strftime(Iice$date,format="%d"))
	Iice$dateString <- strftime(format="%Y-%m-%d",Iice$date)
	# Preserve all the variable names from original data.frame
	for( var in names(ice)){
		cat(var,"\n")
		if(!(var %in% names(Iice))){
			if(var %in% c("dExtent","ExtentA","dExtentA","medianExtent")){
				if(order==0){
					Ivar	<-	approx(x=ice$date,y=ice[[var]],xout=dates,method="constant")
				}else if(order==1){
					Ivar	<-	approx(x=ice$date,y=ice[[var]],xout=dates,method="linear")
				}else{
					Ivar	<-	spline(x=ice$date,y=ice[[var]],xout=dates)
				}
				Iice[var]	<-	Ivar$y
				rm(Ivar)
			}else{
				Iice[var]	<-	NA
			}
		}
	}
	str(Iice)
	return(Iice)
}
# Calculate the mean for each day and the probability
meanice	<-	function(
	ice,
	probs=NA
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

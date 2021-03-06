NHiceCSV	=	N_seaice_extent_daily_v3.0.csv
SHiceCSV	=	S_seaice_extent_daily_v3.0.csv
NSIDCftp	=	ftp://sidads.colorado.edu/
NHdir		=	DATASETS/NOAA/G02135/north/daily/data/
SHdir		=	DATASETS/NOAA/G02135/south/daily/data/
AllIceCSV	=	$(NHiceCSV) $(SHiceCSV)
Nspan		=	Nspan <- 15
probs		=	probs <- c(.125,.25,.75,.875)
#outputdir	=	/Users/mhoecker/Documents/gnuplot/

# Download ice files

getice	:
	wget -N \
	$(foreach NHiceCSVfile,$(NHiceCSV),$(NSIDCftp)$(NHdir)$(NHiceCSVfile)) \
	$(foreach SHiceCSVfile,$(SHiceCSV),$(NSIDCftp)$(SHdir)$(SHiceCSVfile))

NHsmooth.dat	:	$(NHiceCSV) handleice.r Makefile
	Rscript \
-e "source('handleice.r')" \
-e "$(Nspan)" \
-e "$(probs)" \
-e 'ingest.ice("$(NHiceCSV)","NH",Nspan=Nspan,probs=probs)'

SHsmooth.dat	:	$(SHiceCSV) handleice.r Makefile
	Rscript \
-e "source('handleice.r')" \
-e "$(Nspan)" \
-e "$(probs)" \
-e 'ingest.ice("$(SHiceCSV)","SH",Nspan=Nspan,probs=probs)'

yearplot	:	SHsmooth.dat NHsmooth.dat plotNSIDCannualCycle.r Makefile
	Rscript \
-e "source('handleice.r')" \
-e "$(Nspan)" \
-e "$(probs)" \
-e "source('plotNSIDCannualCycle.r')"

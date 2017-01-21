NHiceCSV	=	NH_seaice_extent_final_v2.csv NH_seaice_extent_nrt_v2.csv
SHiceCSV	=	SH_seaice_extent_final_v2.csv SH_seaice_extent_nrt_v2.csv
AllIceCSV	=	$(NHiceCSV) $(SHiceCSV)
Nspan		=	Nspan <- 3
probs		=	probs <- c(.125,.25,.75,.875)
outputdir	=	/Users/mhoecker/Documents/gnuplot/

# Download ice files

getice	:
	wget -P $(outputdir) -N $(foreach NHiceCSVfile,$(NHiceCSV),\
-N ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/north/daily/data/$(NHiceCSVfile)\
)\
	$(foreach SHiceCSVfile,\
$(SHiceCSV),\
-N ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/south/daily/data/$(SHiceCSVfile)\
)

NHfinal_smooth.dat	:	NH_seaice_extent_final_v2.csv handleice.r Makefile
	Rscript \
-e "source('handleice.r')" \
-e "$(Nspan)" \
-e "$(probs)" \
-e 'ingest.final("$(outputdir)NH_seaice_extent_final_v2.csv","$(outputdir)NH",Nspan=Nspan,probs=probs)'

NHnrt_smooth.dat	:	NHfinal_smooth.dat NH_seaice_extent_nrt_v2.csv handleice.r Makefile
	Rscript \
-e "source('$(outputdir)handleice.r')" \
-e "$(Nspan)" \
-e "$(probs)" \
-e 'ingest.nrt("$(outputdir)NH_seaice_extent_nrt_v2.csv","$(outputdir)NH",Nspan=Nspan,probs=probs)'

SHfinal_smooth.dat	:	SH_seaice_extent_final_v2.csv handleice.r Makefile
	Rscript \
-e "source('handleice.r')" \
-e "$(Nspan)" \
-e "$(probs)" \
-e 'ingest.final("SH_seaice_extent_final_v2.csv","SH",Nspan=Nspan,probs=probs)'

SHnrt_smooth.dat	:	SHfinal_smooth.dat SH_seaice_extent_nrt_v2.csv handleice.r Makefile
	Rscript \
-e "source('handleice.r')" \
-e "$(Nspan)" \
-e "$(probs)" \
-e 'ingest.nrt("$(outputdir)SH_seaice_extent_nrt_v2.csv","$(outputdir)SH",Nspan=Nspan,probs=probs)'

final	:	NHfinal_smooth.dat SHfinal_smooth.dat

nrt	:	SHnrt_smooth.dat NHnrt_smooth.dat

yearplot	:	SHnrt_smooth.dat NHnrt_smooth.dat plotNSIDCannualCycle.r Makefile
	Rscript \
-e "source('$(outputdir)handleice.r')" \
-e "$(Nspan)" \
-e "$(probs)" \
-e "source('$(outputdir)plotNSIDCannualCycle.r')"

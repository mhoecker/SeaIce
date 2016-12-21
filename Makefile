NHiceCSV	=	NH_seaice_extent_final_v2.csv NH_seaice_extent_nrt_v2.csv
SHiceCSV	=	SH_seaice_extent_final_v2.csv SH_seaice_extent_nrt_v2.csv
AllIceCSV	=	$(NHiceCSV) $(SHiceCSV)

# Download ice files

getice	:
	$(foreach NHiceCSVfile,\
		$(NHiceCSV),\
		wget -N ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/north/daily/data/$(NHiceCSVfile)\
	)
	$(foreach NHiceCSVfile,\
		$(NHiceCSV),\
		wget -N ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/south/daily/data/$(sHiceCSVfile)\
	)

NHfinal_smooth.dat	:	NH_seaice_extent_final_v2.csv
	Rscript -e "source('handleice.r')" -e 'ingest.final("NH_seaice_extent_final_v2.csv","NH")'

NHnrt_smooth.dat	:	NHfinal_smooth.dat NH_seaice_extent_nrt_v2.csv
	Rscript -e "source('handleice.r')" -e 'ingest.nrt("NH_seaice_extent_nrt_v2.csv","NH")'

SHfinal_smooth.dat	:	SH_seaice_extent_final_v2.csv
	Rscript -e "source('handleice.r')" -e 'ingest.final("SH_seaice_extent_final_v2.csv","SH")'

SHnrt_smooth.dat	:	SHfinal_smooth.dat SH_seaice_extent_nrt_v2.csv
	Rscript -e "source('handleice.r')" -e 'ingest.nrt("SH_seaice_extent_nrt_v2.csv","SH")'

final	:	NHfinal_smooth.dat SHfinal_smooth.dat

nrt	:	SHnrt_smooth.dat NHnrt_smooth.dat

yearplot	:	SHnrt_smooth.dat NHnrt_smooth.dat
	Rscript handleNHice.r

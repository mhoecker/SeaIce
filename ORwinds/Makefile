dataHTTP	=	http://www.ndbc.noaa.gov/data/
#
historicalHTTP	=	$(dataHTTP)historical/
#
# Historical continuous winds at Stonewall Bank 2015
# http://www.ndbc.noaa.gov/data/historical/cwind/46050c2015.txt.gz
#
realtimeHTTP	=	$(dataHTTP)realtime2/
#
# Past 45 days of continuous winds at Stonewall Bank
# http://www.ndbc.noaa.gov/data/realtime2/46050.cwind
#
# Quality controled files for previous months
# file names are (Station #)(Month single Hex digit)(4 digit year).txt.gz
#
# January of curent year continuous wind data at Stonewall Bank
# http://www.ndbc.noaa.gov/data/cwind/Jan/4605012016.txt.gz
#
# ...
#
# October of curent year continuous wind data at Stonewall Bank
# http://www.ndbc.noaa.gov/data/cwind/Oct/46050a2016.txt.gz
#
datadir		=	$(outputdir)dat/
outputdir	=	/Users/mhoecker/Documents/gnuplot/ORwinds/
#
#
#
YEAR	=	$(shell /bin/date +%Y)
# Station IDs
Stonewall	=	46050
Newport		=	NWPO3 nwpo3
#
Stations	=	$(Stonewall) $(Newport)
historicalWinds	=	$(Stonewall)c2015.txt.gz $(Stonewall)c2014.txt.gz
months	=	Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
# Unzip files
%.txt : %.gz
	gunzip -kf $<

# Download data files

# Realtime Files covering past 45 days
$(datadir)$(Stonewall).cwind : 	
	wget  -P $(datadir) -N $(realtimeHTTP)$(Stonewall).cwind 

realtime	:
	wget  -P $(datadir) -N $(foreach station,$(Stations),$(realtimeHTTP)$(station).cwind)

# Quality controled files for previous months of current calendar year

ThisYear	:	January February March April May June July August September October November December

January	:
	for station in $(Stations); do \
		wget -P $(datadir) -N $(dataHTTP)cwind/Jan/$${station}1$(YEAR).txt.gz && \
		gunzip -kf $(datadir)$${station}1$(YEAR).txt.gz; \
	done

February	:
	for station in $(Stations); do \
		wget -P $(datadir) -N $(dataHTTP)cwind/Feb/$${station}2$(YEAR).txt.gz && \
		gunzip -kf $(datadir)$${station}2$(YEAR).txt.gz; \
	done
	
March	:
	for station in $(Stations); do \
		wget -P $(datadir) -N $(dataHTTP)cwind/Mar/$${station}3$(YEAR).txt.gz && \
		gunzip -kf $(datadir)$${station}3$(YEAR).txt.gz; \
	done

April	:
	for station in $(Stations); do \
		wget -P $(datadir) -N $(dataHTTP)cwind/Apr/$${station}4$(YEAR).txt.gz && \
		gunzip -kf $(datadir)$${station}4$(YEAR).txt.gz; \
	done

May	:	
	for station in $(Stations); do \
		wget -P $(datadir) -N $(dataHTTP)cwind/May/$${station}5$(YEAR).txt.gz &&: \
		gunzip -kf $(datadir)$${station}5$(YEAR).txt.gz; \
	done

June	:	
	for station in $(Stations); do \
		wget -P $(datadir) -N $(dataHTTP)cwind/Jun/$${station}6$(YEAR).txt.gz && \
		gunzip -kf $(datadir)$${station}6$(YEAR).txt.gz; \
	done

July	:	
	for station in $(Stations); do \
		wget -P $(datadir) -N $(dataHTTP)cwind/Jul/$${station}7$(YEAR).txt.gz && \
		gunzip -kf $(datadir)$${station}7$(YEAR).txt.gz; \
	done

August	:	
	for station in $(Stations); do \
		wget -P $(datadir) -N $(dataHTTP)cwind/Aug/$${station}8$(YEAR).txt.gz && \
		gunzip -kf $(datadir)$${station}8$(YEAR).txt.gz; \
	done

September	:	
	for station in $(Stations); do \
		wget -P $(datadir) -N $(dataHTTP)cwind/Sep/$${station}9$(YEAR).txt.gz && \
		gunzip -kf $(datadir)$${station}9$(YEAR).txt.gz; \
	done
October	:	
	for station in $(Stations); do \
		wget -P $(datadir) -N $(dataHTTP)cwind/Oct/$${station}a$(YEAR).txt.gz && \
		gunzip -kf $(datadir)$${station}a$(YEAR).txt.gz; \
	done

November	:	
	for station in $(Stations); do \
		wget -P $(datadir) -N $(dataHTTP)cwind/Nov/$${station}b$(YEAR).txt.gz && \
		gunzip -kf $(datadir)$${station}b$(YEAR).txt.gz; \
	done
# Should December allways fail?
December	:	
	for station in $(Stations); do \
		wget -P $(datadir) -N $(dataHTTP)cwind/Dec/$${station}c$(YEAR).txt.gz; \
		gunzip -kf $(datadir)$${station}c$(YEAR).txt.gz; \
	done

# Historical quality controled winds
2015	:
	for station in $(Stations); do \
		wget -P $(datadir) -N $(historicalHTTP)cwind/$${station}c2015.txt.gz; \
		gunzip -kf $(datadir)$${station}c2015.txt.gz; \
	done

2014	:
	for station in $(Stations); do \
		wget -P $(datadir) -N $(historicalHTTP)cwind/$${station}c2014.txt.gz; \
		gunzip -kf $(datadir)$${station}c2014.txt.gz; \
	done

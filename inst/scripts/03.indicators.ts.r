

bioLibrary( "bio.indicators", "bio.spacetime", "bio.utilities")

setwd( project.datadirectory("bio.indicators", "data") )

# not all are fully refreshed automatically .. they are just place holders for now
groundfish = indicators.ts.db( db="groundfish.timeseries.redo" )
snowcrab = indicators.ts.db( db="snowcrab.timeseries.redo")
climate = indicators.ts.db (db="climate.redo" )
shrimp = indicators.ts.db( db="shrimp.timeseries.redo")

sar = indicators.ts.db( db="species.area.redo" )
nss = indicators.ts.db( db="sizespectrum.redo" )
metab = indicators.ts.db( db="metabolism.redo" )
sc = indicators.ts.db( db="species.composition.redo" )

economics = indicators.ts.db( db="economic.data.redo" )

# hand constructed and updated .. TODO :: find solutions!
#plankton = indicators.ts.db( db="plankton.timeseries.redo" )
human = indicators.ts.db( db="demographics.redo" )
#climate = indicators.ts.db (db="climate.redo" )

#seals = indicators.ts.db( db="seal.timeseries.redo" )
landedvalue = indicators.ts.db( db="landedvalue.annual", ref.year=2008 )
landings = indicators.ts.db( db="landings.annual" )

# refresh the survey data
# DEMOGRAPHICS goto:: http://www.gov.ns.ca/finance/communitycounts/dataview.asp?gnum=pro9012&gnum2=pro9012&chartid=&whichacct=&year2=&mapid=&ptype=&gtype=&yearid=2006&acctype=0&gname=&dcol=&group=&group1=&group2=&group3=&gview=3&table=table_d17&glevel=pro


# require( xlsReadWrite )
# data = read.xls( "mydata.xls", sheet="Sheet1" )
#
# for ( y in
# http://www.gov.ns.ca/finance/communitycounts/export.asp?bExcel=1&page=table_d17&dgroup=&dgroup1=&dgroup2=&dgroup3=&dgroup4=&yearid=2011&gnum=pro9012&gname=Nova%20Scotia&range=
#
# require( XLConnect )
# fn = "~/Downloads/estimates.xls"
# wb <- loadWorkbook( fn)
# data <- readWorksheet(wb)


indic = indicators.ts.db( db="bio.indicators.all.glue" )  # glue all time-series together
# indic = indicators.ts.db( db="bio.indicators.all" ) # load the glued version

# indic$data$Nwells.drilled = cumsum.jae(indic$data$Nwells.drilled)
# indic$data$seismic.2D = cumsum.jae(indic$data$seismic.2D)
# indic$data$seismic.3D = cumsum.jae(indic$data$seismic.3D)

t0 = 1960
t1 = 2015

# ordination of selected key factors
indic = indicators.ts.db( db="bio.indicators.all" )

#   d = subset( indic, type="keyfactors" )
#   save( d, file="/home/adam/tmp/ordin.rdata", compress=TRUE )

Y = pca.analyse.data(indic, t0, t1, fname=file.path(project.datadirectory("bio.indicators"), "keyfactors" ) )


sub = indic$data[, c("T_bottom_misaine", "SST_halifax", "ice_coverage.km.2", "Gulf.Stream.front.Ref.62lon", "T_sable_annual", "snowcrab.bottom.habitat.area", "snowcrab.kriged.R0.mass", "snowcrab.fishery.landings", "snowcrab.fishery.cpue", "groundfish.stratified.mean.temp" )]

write.table(sub, file=file.path( project.datadirectory( "bio.snowcrab"), "research", "environ.management", "data.csv"), sep=";")


inn = names (indic$data)
for (i in .keyfactors) {
  if ( i %in% inn ) next()
  print (i)
}


## smaller subsets
# human
.human = c(indic$landings.totals.NS, indic$landedvalue.totals.NS, indic$human )
.human = setdiff( .human, "No.Fish.processors" ) # this has no data yet
Y = pca.analyse.data( indic$data, .human, t0, t1, fname=file.path(project.datadirectory("bio.indicators"), "human") )

# fishery landings
.fishery = c(indic$landings.NS, indic$landings.totals.NS )
Y = pca.analyse.data( indic$data, .fishery, t0, t1, fname=file.path(project.datadirectory("bio.indicators"), "fishery" ))

# fishery landed value
.fishery.value = c(indic$landedvalue.NS, indic$landedvalue.totals.NS )
Y = pca.analyse.data( indic$data, .fishery.value, t0, t1, fname=file.path(project.datadirectory("bio.indicators"), "landedvalue" ))

# fishery -- overall
.fishery = c(indic$landedvalue.NS, indic$landedvalue.totals.NS, indic$landings.NS, indic$landings.totals.NS )
Y = pca.analyse.data( indic$data, .fishery, t0=1970, t1, fname=file.path(project.datadirectory("bio.indicators"), "fishery.overall" ))

# snowcrab
.snowcrab = c(indic$snowcrab, "groundfish.stratified.mean.totwgt.snowcrab", "groundfish.stratified.mean.totno.snowcrab" )
Y = pca.analyse.data(indic$data, .snowcrab, t0, t1, fname=file.path(project.datadirectory("bio.indicators"), "snowcrab" ))

# climate
.climate = c( indic$climate )
Y = pca.analyse.data(indic$data, .climate, t0, t1, fname=file.path(project.datadirectory("bio.indicators"), "climate" ))

# ecosystem
.ecosystem = c( indic$ecosystem )
Y = pca.analyse.data(indic$data, .ecosystem, t0, t1, fname=file.path(project.datadirectory("bio.indicators"), "ecosystem" ))





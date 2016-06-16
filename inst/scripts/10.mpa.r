
# code used to generate stats and figures for St Anns Bank assessment
# this is the main calling program
# new comment

p = list( project.name="mpa" )
p = bio.indicators::indicators.parameters( DS=p$project.name, p=p )

polys = mpa.db( p=p, DS="polygons.redo" ) # obtain and save a local cache of polygons of the mpa/aoi
# polys = mpa.db( p=p, DS="polygons" )

# 1. close-up map of area of interest:
figure.mpa.closeup(p)

# 2. map of area of interest:
figure.mpa.aoi(p )

# 3. survey.db trawl data summaries
pdf( file=file.path(p$project.outdir.root, "trawl.time.density.pdf") )
  ss = survey.db( DS="set" )
  dscols = c( snowcrab="green", groundfish="orange" )
  plot( jitter( dyear) ~ jitter(yr), ss, pch=".", col=dscols[ss$data.source], xlab="Year", ylab="Fractional year", cex=1.5 )
dev.off()

pdf( file=file.path(p$project.outdir.root, "maps", "trawl.spatial.density.pdf") )
  ss = survey.db( DS="set" )
  dscols = c( snowcrab="green", groundfish="orange" )
  figure.trawl.density(p=p, ss=ss, dscols=dscols )
dev.off()

# 4. net mensuration related figures
p$scanmar.dir = file.path( project.datadirectory("bio.groundfish"), "data", "nets", "Scanmar" )
p$marport.dir = file.path( project.datadirectory("bio.groundfish"), "data", "nets", "Marport" )
figures.netmensuration( DS="all", p=p, outdir=p$project.outdir.root  )  # for latex a copy here





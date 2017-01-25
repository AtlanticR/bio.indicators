figure.bio.map.survey.locations = function( p ) {
  require( bio.coastline )
  set = survey.db( DS="set", p=p )
  # redo coast to get unclipped version
  coast = coastline.db( DS=" gshhg coastline full redo ",
        xlim=p$corners$lon, ylim=p$corners$lat, no.clip=TRUE, level=1 )
  plot( coast, col="transparent", border="steelblue2" ,
     xlim=c(-68,-55), ylim=c(41,48),
     xaxs="i", yaxs="i", axes=TRUE )  # ie. coastline

  dscols=c( snowcrab="green", groundfish="orange" )
  points( set$lon, set$lat, pch=20, cex=0.5, col=dscols[set$data.source]  )

  sp::compassRose( sab.loc[1]+2.3 , sab.loc[2]-4.3, cex= 0.7 )
  maps::map.scale( sab.loc[1]+1 , sab.loc[2]-5.1, ratio=FALSE, cex=0.8 )
  box()

}



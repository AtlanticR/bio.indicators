


  variable.list.expand = function(component="all.data") {
    
    V = switch( EXPR=component,
      
    sp.list = c( 
    "10"  , "11"  , "12"  , "13"  , "14"  , "15"  , "16"  , "17"  , "18"  , "19"  , "23"  , "30"  , "31"  , "40"  , "41"  , "42"  , "43"  , "44"  , "46"  , "47"  , "50"  , "51"  , "52"  , "59"  , "60",  
    "61"  , "62"  , "64"  , "70"  , "86"  , "94"  , "111" , "112" , "114" , "117" , "123" , "142" , "143" , "150" , "160" , "162" , "193" , "200" , "201" , "202" , "203" , "204" , "205" , "207" , "211", 
    "216" , "219" , "220" , "240" , "241" , "300" , "301" , "303" , "304" , "307" , "310" , "311" , "317" , "320" , "321" , "336" , "337" , "340" , "350" , "351" , "352" , "385" , "400" , "406" , "410", 
    "411" , "414" , "500" , "501" , "502" , "505" , "511" , "520" , "542" , "565" , "570" , "580" , "590" , "595" , "597" , "598" , "599" , "600" , "612" , "619" , "621" , "622" , "623" , "625" , "626", 
    "630" , "637" , "640" , "641" , "642" , "644" , "645" , "647" , "660" , "701" , "704" , "712" , "720" , "742" , "771" , "845" , "946" , "980" , "1100", "1224", "1510", "1701", "1810", "1821", "1823",
    "1827", "1840", "1845", "1900", "2000", "2100", "2210", "2211", "2212", "2310", "2311", "2313", "2316", "2410", "2411", "2413", "2415", "2416", "2417", "2511", "2513", "2515", "2519", "2520", "2521",
    "2522", "2523", "2527", "2532", "2550", "2558", "2559", "2565", "2606", "2800", "2901", "2980", "2990", "2999", "3000", "3100", "3123", "3200", "3451", "3550", "3600", "4000", "4110", "4200", "4210",
    "4216", "4220", "4221", "4300", "4301", "4304", "4310", "4312", "4315", "4316", "4317", "4321", "4322", "4330", "4332", "4340", "4342", "4343", "4353", "4354", "4355", "4400", "4430", "4431", "4500",
    "4501", "4510", "4511", "4514", "4521", "4522", "4621", "5100", "6006", "6100", "6111", "6113", "6115", "6117", "6119", "6121", "6123", "6127", "6128", "6129", "6200", "6300", "6400", "6411", "6412",
    "6413", "6500", "6511", "6600", "6611", "6719", "8100", "8200", "8300", "8313", "8318", "8322", "8332", "8400", "8500", "8520", "8600", "9300"
    ), 
    oldsp.list = c( 
      "forage.fish", "all", "allfish", "elasmobranchs", "gadoid", "flatfish",
      "demersal", "large.demersal", "small.demersal",
      "pelagic", "large.pelagic", "small.pelagic",
      "commercial", "noncommercial", 
      "cod", "haddock", "american.plaice", "silver.hake", "white.hake", 
      "capelin", "herring", "mackerel", "sandlance", "redfish", "wolffish",
      "winter.flounder", 
      "spiny.dogfish",  "thornyskate",
      "crabs", "snowcrab", "northernshrimp", "squid" 
    ), 
    multispecies = c( "all", "elasmobranchs", "demersal", "large.demersal", "small.demersal",
      "pelagic", "large.pelagic", "small.pelagic", "flatfish", "commercial", "noncommercial"
    ),
    days = c("all.1km.10day", "all.50km.10day", "all.1km.50day", "all.50km.50day" 
    ),

    all = c(
      paste( "ms.no", variable.list.expand("sp.list"), sep="." ),
      paste( "ms.mass", variable.list.expand("sp.list"),  sep="." ),
      paste( "ntaxa", variable.list.expand("multispecies"),  sep="." ),
      paste( "rmean", variable.list.expand("sp.list"),  sep="." ),
      paste( "pmean", variable.list.expand("sp.list"),  sep="." ),
      paste( "mmean", variable.list.expand("sp.list"),  sep="." ),
#     paste( "lmean", variable.list.expand("sp.list"),  sep="." ),
      paste( "nss.rsquared", variable.list.expand("days"), sep="."),
      paste( "nss.df", variable.list.expand("days"), sep="."),
      paste( "nss.b0", variable.list.expand("days"), sep="."),
      paste( "nss.b1", variable.list.expand("days"), sep="."),
      paste( "nss.shannon", variable.list.expand("days"), sep="."),
      paste( "nss.evenness", variable.list.expand("days"), sep="."),
      paste( "nss.Hmax", variable.list.expand("days"), sep="."),
      paste( "ntaxa", "annual",c(1,seq(20,200,20)), sep="."),
      "C", "Z", "sar.rsq", "Npred", 
      "mr", "mrT", "smr", "smrT", "mrPvalue", "mrPvalueT",
      "ca1", "ca2", "shannon", "evenness", "Hmax",
      "sdepth", "temp", "sal", "oxyml", "julian"
    ),

      catch.summary = variable.list.expand("sp.list") ,
  
               
       physical = c("z", "t", "julian"),
       
       males.general = c(
        "totmass.male.com", "totno.male.com", "totno.male.mat", "totno.male.imm", "totno.male", 
        "R0.mass", "R0.no","R1.no", "R2.no", "R3.no", "R4.no", "R5p.no",
        "male.large.mass", "male.small.mass", "male.large.no", "male.small.no", "dwarf.no", "totno.male.skip.moulter"
      ),
      males.CC = c( 
        "totno.male.com.CC1", "totno.male.com.CC2", "totno.male.com.CC3", "totno.male.com.CC4", "totno.male.com.CC5",
        "totno.male.com.CC1to2", "totno.male.com.CC3to4",
        "totmass.male.com.CC1", "totmass.male.com.CC2", "totmass.male.com.CC3", "totmass.male.com.CC4", "totmass.male.com.CC5",
        "totmass.male.com.CC1to2", "totmass.male.com.CC3to4"
      ),
      males.instar = c( 
        "mi123.no", "mi4.no", "mi5.no", "mi6.no", "mi7.no", "mi8.no","mi9.no", "mi10.no", "mi11.no", "mi12.no",
        "mi9.skip.moulter.no", "mi10.skip.moulter.no","mi11.skip.moulter.no","mi12.skip.moulter.no",
        "ma9.no","ma10.no","ma11.no","ma12.no","ma13.no",
        "ma9.CC1to2.no","ma10.CC1to2.no","ma11.CC1to2.no","ma12.CC1to2.no","ma13.CC1to2.no",
        "ma9.CC3to4.no","ma10.CC3to4.no","ma11.CC3to4.no","ma12.CC3to4.no","ma13.CC3to4.no",
        "ma9.CC5.no","ma10.CC5.no","ma11.CC5.no","ma12.CC5.no","ma13.CC5.no" 
      ),
      females.instar = c(
        "fi1234.no", "fi5.no", "fi6.no", "fi7.no", "fi8.no", "fi9.no", "fi10.no",
        "fi6.adolescent.no","fi7.adolescent.no","fi8.adolescent.no","fi9.adolescent.no","fi10.adolescent.no",
        "fi6.preprimiparous.no","fi7.preprimiparous.no", "fi8.preprimiparous.no", "fi9.preprimiparous.no","fi10.preprimiparous.no",
        "fa7.no","fa8.no","fa9.no","fa10.no",
        "fa7.berried.no","fa8.berried.no","fa9.berried.no","fa10.berried.no",
        "fa7.primiparous.no","fa8.primiparous.no","fa9.primiparous.no","fa10.primiparous.no",
        "fa7.multiparous.no","fa8.multiparous.no","fa9.multiparous.no","fa10.multiparous.no",
        "fa7.senile.no","fa8.senile.no","fa9.senile.no","fa10.senile.no" 
      ),
      females.general = c(
        "totno.female.berried","totno.female.imm", "totno.female.mat", "totno.female.primiparous","totno.female.multiparous", "fecundity", 
        "female.large.mass", "female.small.mass", "female.large.no", "female.small.no", "totno.female"
      ),
      snowcrab.general = c(
        "totno.all", "totmass.all"
      ),
      snowcrab.unused = c(
        "totmass.male", "totmass.female",
        "totmass.male.imm", "totmass.female.imm",
        "totmass.male.mat", "totmass.female.mat",
        "totmass.female.berried", 
        "totmass.female.primiparous", "totmass.female.multiparous",
        "totno.male.ncom",
        "totmass.male.ncom",
        "totmass.male.skip.moulter",
        "pre.recruit.no", "pre.recruit.mass",
        "mi123.mass", "mi4.mass", "mi5.mass", "mi6.mass", "mi7.mass", "mi8.mass", "mi9.mass", "mi10.mass", "mi11.mass", "mi12.mass", 
        "fi1234.mass", "fi5.mass", "fi6.mass", "fi7.mass", "fi8.mass", "fi9.mass", "fi10.mass",
        "m7.no", "f7.no",
        "m8.no", "f8.no",
        "m9.no", "f9.no",
        "m10.no", "f10.no",
        "totmass.female.CC3", "totmass.female.CC4",
        "totno.female.CC3", "totno.female.CC4", 
        "totno.female.CC1to2", "totno.female.CC3to4", "totno.female.CC5", 
        "totmass.female.CC1to2", "totmass.female.CC3to4", "totmass.female.CC5", 
        "R1.mass", "R2.mass", "R3.mass", "R4.mass", "R5p.mass", "dwarf.mass" 
      ),
      snowcrab.bycatch = c( 
           paste( "ms.no", variable.list.expand("sp.list"), sep="." ),
            paste( "ms.mass", variable.list.expand("sp.list"),  sep="." )
            ),

      snowcrab.indicators = c(      
        "sexratio.all", "sexratio.mat", "sexratio.imm" 
      ),
      snowcrab.cw = c(
        "cw.mean", "cw.comm.mean", "cw.notcomm.mean", "cw.fem.mat.mean", "cw.fem.imm.mean",
        "cw.comm.var", "cw.notcomm.var", "cw.fem.mat.var", "cw.fem.imm.var", "cw.var",
        "cw.male.mat.mean", "cw.male.imm.mean", "cw.male.mat.var", "cw.male.imm.var", "cw", "ch", "aw"
      ),
      
      all.data = c(
        variable.list.expand("physical"),
        variable.list.expand("males.general"),
        variable.list.expand("males.CC"),
        variable.list.expand("males.instar"),
        variable.list.expand("females.instar"),
        variable.list.expand("females.general"),
        variable.list.expand("snowcrab.general"),
        variable.list.expand("snowcrab.unused"),
        #variable.list.expand("snowcrab.bycatch"),
        variable.list.expand("snowcrab.indicators"),
        variable.list.expand("snowcrab.cw"),
         "landings", "cpue",  "effort"
      ),
     
      all.to.model = c( 
        variable.list.expand("males.general"),
        variable.list.expand("males.CC"),
        variable.list.expand("males.instar"),
        variable.list.expand("females.instar"),
        variable.list.expand("females.general")
      ),

      scaled.centered =c ("dummyvariable"), # swtich does not like a null vector

      log.transform = c(
        paste( "ms.no", variable.list.expand("sp.list"), sep="." ),
        paste( "ms.mass", variable.list.expand("sp.list"),  sep="." ),
        "Npred", "mr", "mrT",
        variable.list.expand("males.general"),
        variable.list.expand("males.CC"),
        variable.list.expand("males.instar"),
        variable.list.expand("females.instar"),
        variable.list.expand("females.general"),
        variable.list.expand("snowcrab.general"),
        variable.list.expand("snowcrab.unused"),
        variable.list.expand("snowcrab.bycatch"),
        "landings", "dZ", "ddZ"
      )
    ) # end switch
    
    V = sort( unique(  V ) )
    return (V)
  }



  indicators_selection_criteria = function( dat, selection=NULL ) {
    
    out = 1:nrow(dat)

    if (is.null(selection)) return(out)

    if (exists("spec_bio", selection)) {
      out =  intersect( out, which( dat$spec_bio==selection$spec_bio ) ) 
    }

    if (exists("spec", selection)) {
      out =  intersect( out, which( dat$spec==selection$spec ) ) 
    }

    if (exists("mat", selection)) {
      out =  intersect( out, which( dat$mat==selection$mat ) ) 
    }

    if (exists("sex", selection)) {
      out =  intersect( out, which( dat$set==selection$set ) ) 
    }

    if (exists("len", selection)) {
      out =  intersect( out, which( dat$len > selection$len[1] & dat$len < selection$len[2] ) ) 
    }

    if (exists("mass", selection)) {
      out =  intersect( out, which( dat$mass > selection$mass[1] & dat$mass < selection$mass[2] ) ) 
    }

    return(out)
  }

#function for depth and thickness of stagnic properties

f.logg_g <- function (unic_point_data, minPg,thickg){
  for (li in 1:length(unic_point_data[,1])){
    if (!is.na(unic_point_data$O2_MANGEL1[li]) &  grepl("g",unic_point_data$O2_MANGEL1[li])&!grepl("gg",unic_point_data$O2_MANGEL1[li])){
      if (unic_point_data$TIEFE_VON[li] < minPg) {
        minPg = unic_point_data$TIEFE_VON[li]
        thickg = unic_point_data$TIEFE_BIS[li]-unic_point_data$TIEFE_VON[li]
      }
    }
  }
  return(c(minPg, thickg))
}
  
    
f.logg_gg <- function (unic_point_data, minPgg,thickgg){
  for (li in 1:length(unic_point_data[,1])){
    if (!is.na(unic_point_data$O2_MANGEL1[li]) &  grepl("gg",unic_point_data$O2_MANGEL1[li])){
      if (unic_point_data$TIEFE_VON[li] < minPgg) {
        minPgg = unic_point_data$TIEFE_VON[li]
        thickgg = unic_point_data$TIEFE_BIS[li]-unic_point_data$TIEFE_VON[li]
      }
    }
  }
  return(c(minPgg, thickgg))
}

  
f.logg_r <- function (unic_point_data, minPr,thickr){
  for (li in 1:length(unic_point_data[,1])){
    if (!is.na(unic_point_data$O2_MANGEL1[li]) &  grepl("r",unic_point_data$O2_MANGEL1[li])){
      if (unic_point_data$TIEFE_VON[li] < minPr) {
        minPr = unic_point_data$TIEFE_VON[li]
        thickr = unic_point_data$TIEFE_BIS[li]-unic_point_data$TIEFE_VON[li]
      }
    } 
  }
  return(c(minPr, thickr))
}
#code R for peat processing analysis
f.peat <- function (unic_point_data, vpostsum,sumOST,thickT,thickT80, minT, thickMu80, nbpeathoriz){
for (li in 1:length(unic_point_data[,1])){
  #analysis of T horizont, minimal deapth, number of not consecutive T, thickness all and to 80 cm
  if (!is.na(unic_point_data$HORIZONT1[li]) &  grepl("T",unic_point_data$HORIZONT1[li])& !grepl("TC",unic_point_data$HORIZONT1[li])){
    vpostsum = vpostsum+as.numeric(substr(sondH$ZERSETZUNG[li], 2,3))*(unic_point_data$TIEFE_BIS[li]-unic_point_data$TIEFE_VON[li])
    sumOST=sumOST+sondH$OS_FELD[li]*(unic_point_data$TIEFE_BIS[li]-unic_point_data$TIEFE_VON[li])
    thickT=thickT+unic_point_data$TIEFE_BIS[li]-unic_point_data$TIEFE_VON[li]
    if (unic_point_data$TIEFE_VON[li]<80 & unic_point_data$TIEFE_BIS[li]<=80){
      thickT80=thickT80+unic_point_data$TIEFE_BIS[li]-unic_point_data$TIEFE_VON[li] #thickness to 80cm
    }
    if (unic_point_data$TIEFE_VON[li]<80 & unic_point_data$TIEFE_BIS[li]>80){
      thickT80=thickT80+80-unic_point_data$TIEFE_VON[li]
    }
    if (unic_point_data$TIEFE_VON[li] < minT) { #minimal deapth
      minT = unic_point_data$TIEFE_VON[li]
    }
  }
  #thickness of Mudde in the first 80 cm
  if (!is.na(unic_point_data$HORIZONT1[li]) &  grepl("C",unic_point_data$HORIZONT1[li])& grepl("MU",unic_point_data$tab02_schicht_AUSGANGSMAT[li])){
    if (unic_point_data$TIEFE_VON[li]<80 & unic_point_data$TIEFE_BIS[li]<=80){
      thickMu80=thickMu80+unic_point_data$TIEFE_BIS[li]-unic_point_data$TIEFE_VON[li]
    }
    if (unic_point_data$TIEFE_VON[li]<80 & unic_point_data$TIEFE_BIS[li]>80){
      thickMu80=thickMu80+80-unic_point_data$TIEFE_VON[li]
    }
  }
  
  #count of the peat horizons
  if (!is.na(unic_point_data$HORIZONT1[li]) &  grepl("T",unic_point_data$HORIZONT1[li]) & !grepl("T",unic_point_data$HORIZONT1[li+1])& !grepl("CT",unic_point_data$HORIZONT1[li+1])){ ##CT hinzugef?gt
    nbpeathoriz = nbpeathoriz +1
  }
}
  
  return(c(vpostsum,sumOST,thickT,thickT80, minT, thickMu80, nbpeathoriz))
}
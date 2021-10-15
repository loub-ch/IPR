library(tidyverse)
library(readr)
data_ssbv<- read_delim("data_net_complet_X_Y_v2_wgs84_CodGeo.csv", 
                                          ";", escape_double = FALSE, trim_ws = TRUE)
bv<-readRDS("bv")
data_ipr<-data_net_complet%>%
  left_join(y=data_ssbv%>%select(CdEauSsBassi:CdEuBassin))%>%
  left_join(y=bv)

saveRDS(data_ipr,"donnees sortie/data_ipr")


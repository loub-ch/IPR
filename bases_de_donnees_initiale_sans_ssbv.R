############################Chargement des packages et des données############################
devtools::install_github("PascalIrz/aspe")
library(aspe)
library(sqldf)
library(tidyverse)
load(file = "donnees entree/toutes_tables_aspe_sauf_mei.RData")
##############################################################################################
#Pour simplifier notre demarche on va suivre ces étapes:
#Construction de tableau de correspondance des clés primaires
#donnees
source("pass2.R")
passerelle <- mef_creer_passerelle2()
data <- mef_creer_passerelle2() %>%
  
  mef_ajouter_libelle() %>%
  
  mef_ajouter_ipr() %>%
  
  mef_ajouter_ope_date() %>%
  
  left_join(y = station %>%
              
              select(sta_id, sta_code_sandre))

data_2010_2020<-sqldf('select * from data where annee between "2010" and "2020"')
data_2010_2020<-sqldf('select * from data_2010_2020 where ipr is not null')
data_2010_2020<-data_2010_2020%>%filter(qualif !=c("3","4"))
data_net<-sqldf('select distinct(pop_id),sta_id,avg(ipr) as ipr,annee, ope_date  from data_2010_2020 group by annee,ope_date,pop_id')

data_net_complet<-data_net%>% 
  left_join(y = station %>%select(sta_id, sta_code_sandre,sta_libelle_sandre,sta_com_code_insee,sta_coordonnees_x,sta_coordonnees_y))%>%
   
  left_join(y = commune %>%select(sta_com_code_insee=com_code_insee,dep_code_insee=com_dep_code_insee,com_libelle))%>%

  left_join(y = departement %>%select(dep_code_insee, reg_code_insee=dep_reg_code_insee,dep_libelle))%>%
  
  left_join(y = region %>%select(reg_code_insee,reg_libelle))%>%filter(sta_code_sandre!="NA")
 

data_net_complet<-sqldf('SELECT *,case when ipr <="7" then "Tres bon"
                 when ipr  >"5" and ipr <= "16" then "Bon"
                when ipr >"16" and  ipr <= "25" then "Moyen"
                when ipr >"25" and ipr<= "36" then "Mediocre"
                else "Mauvais"
                end as classe_ipr FROM data_net_complet')

#Exporter cette table en excel pour demender le recoupement geomatique pour les ssbv et le rajout des coodonnees
#X ,Y en WGS84
write.csv2(data_net_complet, file = "data_net_complet.csv",
           row.names = FALSE, na = "")

saveRDS(data_net_complet,"data_net_complet")

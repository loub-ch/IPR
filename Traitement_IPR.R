##############################################################################################
#
#Date de debut: 18/05/2021
#Encadrant de travail: CERISIER-AUGER Alexis/JOASSARD Irenee/IRZ Pascal
##############################################################################################

###########################################Objectif###########################################
#sortir les données par station, cours d'eau, département, région, bassin versant, national
#dresser la carte par année suivant les 5 classes par station
##############################################################################################

##################################Demarche a suivre###########################################
#partir de la base ASPE 
#reprendre les scripts R récemment élaborés disponibles sous https://rpubs.com/kamoke/713491
#respecter les conditions suivantes:
#prendre réseaux pérennes uniquement 2,3,4,5,6 (voire 7) 
#prendre pêche complète uniquement
#prendre pêche à 1, 2 ou 3 passage(s)
#commencer par réaliser le traitement sur les petits cours d'eau (faire le même traitement sur 
#les grands cours d'eau dans un second temps)
##############################################################################################
#####
# Dans un premier temps je ne prend aucune condition en compte juste je cree la 
# passerelle
#####

############################Chargement des packages et des données############################
devtools::install_github("PascalIrz/aspe")
library(aspe)
library(sqldf)
library(tidyverse)
load(file = "donnees brutes/toutes_tables_aspe_sauf_mei.RData")
##############################################################################################

##################################L'analyse de la base########################################

#######################Structure de la base#######################

#les principaux informations dans cette base:
#station
#point_prelevement
#operation
#prelevement_elementaire
#lot_poissons
#mesure_individuelle

##################################################################


#############################Principe#############################

#Pour simplifier notre demarche on va suivre ces étapes:
#Construction de tableau de correspondance des clés primaires
passerelle <- mef_creer_passerelle()

################################Resolution d'un probleme#####################################
#En essayant de reproduire les graphique les couleurs pour la qualité d'eau (mediocre te tres 
#mauvais on la detecte pas ) pour resoudre ce probleme je propose d'enlever les accents et les 
#espaces pour ces deux qualite
classe_ipr_plus$cip_libelle<-c("Tres Bon","Bon","Moyen","Mediocre","Mauvais")

################################Bilan des IPR par station#####################################

#On utilise la passerelle pour assurer le lien entre les informations dans differentes tables
ipr <- mef_ajouter_ipr(passerelle) %>%
  mef_ajouter_ope_date() %>%
  filter(ope_date > lubridate::dmy("01/01/2010"),
         ope_date < lubridate::dmy("31/12/2020")) %>%
  droplevels()

##Export de la table IPR pour Alexis
write.csv2(ipr[1:1048575,] , file = "donnees elaborees/traitement gen/IPR1.csv",
           row.names = FALSE, na = "")
write.csv2(ipr[1048576:1798740,] , file = "donnees elaborees/traitement gen/IPR2.csv",
           row.names = FALSE, na = "")
#Extraire les 10 premiere lignes du tableau
ipr %>% head() %>% DT::datatable()

#Nombre de stations totale par an
n_ipr_par_an <- ipr %>%
  group_by(annee) %>%
  summarise(n_pts = n_distinct(pop_id)) %>%
  ungroup()
##Export de la table nbre de stations par an pour Alexis
write.csv2(n_ipr_par_an, file = "donnees elaborees/traitement gen/nombre de stations.csv",
           row.names = FALSE, na = "")
#IPR de station par an 
ipr_1c_par_an <- ipr_pivoter_1colonne_par_an(ipr_df = ipr) 

##Export de la table IPR stations par an pour Alexis
write.csv2(ipr_1c_par_an, file = "donnees elaborees/traitement gen/IPR station par an.csv",
           row.names = FALSE, na = "")

#extraire les IPR par station de l'annee 2010
test1<-sqldf('select distinct(pop_id),sta_id,ipr,annee from ipr where annee=2010 group by pop_id')

##Export de la table IPR stations 2010 pour Alexis
write.csv2(test1, file = "donnees elaborees/traitement gen/IPR station 2010.csv",
           row.names = FALSE, na = "")


#extraire les IPR par station de l'annee 2010
test2<-sqldf('select distinct(pop_id),sta_id,ipr,annee from ipr where annee=2011 group by pop_id')

##Export de la table IPR stations 2011 pour Alexis
write.csv2(test2, file = "donnees elaborees/IPR station 2011.csv",
           row.names = FALSE, na = "")

#extraire les IPR par station de l'annee 2010
test10<-sqldf('select distinct(pop_id),sta_id,ipr,annee from ipr where annee=2020 group by pop_id')

##Export de la table IPR stations 2011 pour Alexis
write.csv2(test10, file = "donnees elaborees/traitement gen/IPR station 2020.csv",
           row.names = FALSE, na = "")


#Etape avant la modelisation
#Pour simplifier notre demarche on va suivre ces étapes:
#Construction de tableau de correspondance des clés primaires

passerelle <- mef_creer_passerelle2()

##Selection des operation de 2 a 7
  passerelle <- passerelle %>%
    mef_select_obj(objectif = 2:7)
#selection des protocoles de pêches
  # 1 = "Pêche complète à un ou plusieurs passages",
  # 2 = "Pêche partielle par points (grand milieu)",
  # 3 = "Pêche par ambiances"
  
#Ici je prend que "Pêche complète à un ou plusieurs passages"
passerelle <- passerelle %>%
  mef_select_proto(protocole =1)

################################Bilan des IPR par station#####################################
library(lubridate)
  
#filterer les ipr d'aout à oct
ipr_fl<- mef_ajouter_ipr(passerelle) %>%
 mef_ajouter_ope_date()%>%filter(qualif !=c("4","3") & ipr!="NA" & annee>=2010)
ipr_fl<-ipr_fl%>%mutate(mois=month(ope_date),jours_annee=yday(ope_date))%>%filter(mois>=8 & mois<=10)

#si on a des ipr dans la meme journée en prend la moy
ipr_fl<-ipr_fl%>%left_join(y = data_net_complet%>%select(sta_id,BV,NomSsBassi))

ipr_fl<-sqldf('select distinct(pop_id),sta_id,avg(ipr) as ipr ,ope_date,annee from ipr_fl group by annee,pop_id,sta_id,ope_date')
#transformation du log ipr
ipr_filtre<-ipr_fl%>%mutate(logIPR=max (log (1 +ipr)) - log (1 + ipr))


# suppression des valeurs au-delà de moyenne + ou - 3 écarts-type sur la distribution générale
ipr_filtre<-ipr_filtre%>%mutate(moy=mean(logIPR),var=sd(logIPR))
ipr_filtre<- subset (ipr_filtre, abs (ipr_filtre$logIPR - moy) < 3 * var)


# ajout de colonnes pour les nb de valeurs, moyennes et écarts-types des notes de qualité
tab2 <- aggregate(ipr_filtre[, "logIPR"], by=list(ipr_filtre$pop_id), FUN=length, simplify=T)
colnames(tab2) <- c("pop_id", "nb.notes.qualite")
tab3 <- aggregate(ipr_filtre[, "logIPR"], by=list(ipr_filtre$pop_id), FUN=mean, simplify=T)
colnames(tab3) <- c("pop_id", "moy.notes.qualite")
tab4 <- aggregate(ipr_filtre[, "logIPR"], by=list(ipr_filtre$pop_id), FUN=sd, simplify=T)
colnames(tab4) <- c("pop_id", "var.notes.qualite")
ipr_filtre<- merge (x=ipr_filtre, y=tab2, by="pop_id", all.x=T)
ipr_filtre<- merge (x=ipr_filtre, y=tab3, by="pop_id", all.x=T)
ipr_filtre<- merge (x=ipr_filtre, y=tab4, by="pop_id", all.x=T)

#Moins de 3 obs suppr on selectionne les stat avec 3 donnes IPr
res <- subset (ipr_filtre, nb.notes.qualite > 2)
res$pop_id <- droplevels(as.factor(res$pop_id))

#élimination des notes au-delà de 3 écarts-types pour chaque station
# calcul de l'écart à la moyenne de la station pour chaque note de qualité et suppression au-delà du seuil
res$ecart.moy <- abs ((res$logIPR-res$moy.notes.qualite)/res$var.notes.qualite)
res <- subset (res, res$ecart.moy <= 3)

# re-sélection des stations avec plus de 3 données IPR
res<- subset (res, nb.notes.qualite > 2)
res$pop_id <- droplevels (res$pop_id)
n3 <- nrow (tab2[ which (tab2$nb.notes.qualite > 3),])
library(lubridate)
res$jour<- day(res$ope_date)
res$jour_annee <- yday (res$ope_date)

# recherche des pseudo-doublons - 2 notes IPR sur un même site à même date - moyenne si plusieurs IPR
vars <- as.list(names(ipr_filtre))
vars <- vars [vars !="logIPR"]
resb <- aggregate(res["logIPR"], by=list(res$pop_id, res$ope_date), FUN=mean, simplify=T)
colnames(resb) [1:2] <- c("pop_id", "ope_date")

#visualisation des faux doublons éventuels
sub4 <- merge (x=resb, y=res, by=c("pop_id", "ope_date"), all.y=T)
sub4$diff <- sub4$logIPR.x - sub4$logIPR.y
min(sub4$diff)
max(sub4$diff)
sub4b <- subset (sub4, !sub4$logIPR.x==sub4$logIPR.y)
rm(sub4b, sub4)

# suppression des faux doublons éventuels
res <- merge (x=resb, y=res, all.x=T)
res <- droplevels(res)
rm(tab2, resb, n3)
res
bv_ssbv<-data_net_complet%>%select(sta_id,BV,NomSsBassi)%>%distinct()
res_bv<-res%>%left_join(y=bv_ssbv)

# res<- sqldf('select distinct(pop_id),sta_id,avg(ipr) as ipr ,ope_date,annee from res group by annee,pop_id,sta_id,ope_date')
# res<-res%>%filter(pop_id!="1024")
data_tendance<-res_bv
saveRDS(data_tendance,"donnees sortie/data_tendance")

gam<-function(df){
  Mod3_ssbv<-gam (logIPR ~ as.factor(annee) + s(jour_annee, bs= "cc") + s(pop_id, bs = "re"),data=d)
  
  par(mar = c(5.1, 6, 4.1, 2.1), mgp = c(4, 1, 0))
  coef.table <- as.data.frame(summary(Mod3_ssbv)$p.table)
  colnames(coef.table)[2] <- "SE"
  coef.table$annee <- as.numeric(substr( row.names(coef.table), nchar( row.names(coef.table))-3, nchar(
    row.names(coef.table))))
  coef.table$annee[1] <- min(na.omit(d$annee))
  coef.table$Estimate[1] <- 0
  coef.table$ind100 <- 100*(1+coef.table$Estimate)
  coef.table$SE100 <- 100*coef.table$SE
  coef.table$SE100 [1]<- 0
  coef.table$yminus<-coef.table$ind100-1.96*coef.table$SE100
  coef.table$yplus<-coef.table$ind100+1.96*coef.table$SE100
  coef.table
  
}
tend_nat<-gam(data_tendance)
saveRDS(tend_nat,"donnes sortie/tend_nat")
#plot tendace national
p<-ggplot(data =tend_nat,
          aes(x = annee, y = ind100))+labs(title ='Tendance national de L\'IPR') +scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''),
                                                                                                     breaks = 2010:2020,
          )+
  geom_point() +
  geom_smooth()+theme(plot.background=element_rect(fill="transparent",colour=NA))+labs(x =paste("Ann\U00E9\U0065"), y = "Valeur de l'indice IPR (base 100)")
TREND = ggplot_build(p)$data[[2]][["y"]]
#taux d'evolution
d<-mean(TREND[1:8])
f<-mean(TREND[40:48])
taux_evol_15<-100*((f/d)-1)


d<-mean(TREND[48:56])
f<-mean(TREND[64:80])
taux_evol_20<-100*((f/d)-1)

d<-mean(TREND[1:16])
f<-mean(TREND[64:80])
taux<-100*((f/d)-1)


#taux pas bassin
ab<-c("RHONE-MEDITERRANEE", "LOIRE-BRETAGNE", "RHIN-MEUSE", "SEINE-NORMANDIE",
      "ADOUR-GARONNE", "ARTOIS-PICARDIE")
bv<-function(i){
  gam_bv(data_tendance,ab[i])
}
taux_bv<-function(i){
  TREND <-ggplot_build(bv(i))$data[[2]][["y"]]
  d<-mean(TREND[1:16])
  f<-mean(TREND[64:80])
  taux<-100*((f/d)-1)
  taux
}
taux_bv(1)
num<-round(c(taux_bv(1),taux_bv(2),taux_bv(3),taux_bv(4),taux_bv(5),taux_bv(6)),1)
mean(tableau_bv$y)
tableau_bv <- data.frame(x = ab, y = num) 
tableau_bv<-tableau_bv%>%arrange(y)
tableau_bv$ordre<-order(sort(tableau_bv$y),decreasing = T)
saveRDS(tableau_bv,"donnees sortie/tableaubv")

SSBV<-sqldf::sqldf('select distinct(NomSsBassi) from data_tendance')
SSBV[1,1]

#taux par ssbv
p<-function(i){
  gam_ssbv(data_tendance,SSBV[1,i])
}

taux<-function(i){
  TREND <-ggplot_build(p(i))$data[[2]][["y"]]
  d<-mean(TREND[1:16])
  f<-mean(TREND[64:80])
  taux<-100*((f/d)-1)
  taux
}

for (i in 1:length(a)){
  p[i]<-round(taux(i),1)
  p
}
mean(tableau_bv$y)
tableau_ssbv <- data.frame(x = SSBV, y = p) 
tableau_ssbv<-tableau_ssbv%>%arrange(y)
tableau_ssbv$ordre<-order(sort(tableau_ssbv$y),decreasing = T)
saveRDS(tableau_bv,"donnees sortie/tableau_ssbv")




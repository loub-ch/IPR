##########Recodage de la table station pour la transformer en coord sp
## Cette partie est bien détaillee sur https://rpubs.com/kamoke/716322
## Ici je l'adapte en fonction de mes besoins
##########
load(file = "donnees brutes/toutes_tables_aspe_sauf_mei.RData")

##Reprojection des stations
##Pour les traitements geographiques, faut avoir les objets manipule
#dans le meme CRS

crs_stations <- station %>% 
  rename(typ_id = sta_typ_id) %>% 
  left_join(y = ref_type_projection) %>% 
  pull(typ_libelle_sandre) %>% 
  as.character() %>% 
  table() %>% 
  as.data.frame() %>% 
  purrr::set_names(c("crs", "Nombre de stations"))

crs_points <- point_prelevement %>% 
  rename(typ_id = pop_typ_id) %>% 
  left_join(y = ref_type_projection) %>% 
  pull(typ_libelle_sandre) %>% 
  as.character() %>% 
  table() %>% 
  as.data.frame() %>% 
  purrr::set_names(c("crs", "Nombre de points"))


crs_tot <- full_join(x = crs_points, y = crs_stations)

DT::datatable(crs_tot, width = 600, rownames = FALSE)

#La table ref type projection contien les crs utilises pour les stations 
#et les point de prelevements

ref_type_projection %>%
  filter(typ_libelle_sandre %in% (crs_tot %>% pull(crs))) %>% 
  as.data.frame() %>% 
  DT::datatable()

#on complete la table ref_type_projection car le  code EPSG (27572) est manquant 
ref_type_projection <- ref_type_projection %>%
  mutate(typ_code_epsg = ifelse((is.na(typ_code_epsg) & typ_libelle_sandre == "Lambert II Etendu"),
                                yes = 27572,
                               no = typ_code_epsg))
#Verification 
ref_type_projection %>%
  filter(typ_libelle_sandre %in% (crs_tot %>% pull(crs))) %>% 
  as.data.frame() %>% 
  DT::datatable()

#Ajout à la table station des codes EPSG de chacune des stations
station <- station %>%
  left_join(y = ref_type_projection,
            by = c("sta_typ_id" = "typ_id"))
#ajout de la var pop_id
station <- station %>%
  left_join(y = point_prelevement,
            by = c("sta_id" = "pop_sta_id"))
#Reprojection et extraction des coordonnées des stations en WGS84
coords_wgs84 <- geo_convertir_coords_df(df = station,
                                        var_x = "sta_coordonnees_x",
                                        var_y = "sta_coordonnees_y",
                                        var_crs_initial = "typ_code_epsg",
                                        crs_sortie = 4326) %>%
  rename(x_wgs84 = X, y_wgs84 = Y)

#Ajout des coordonnées WGS84 et suppression des colonnes qui ne serviront plus :
station <- station %>%
  bind_cols(coords_wgs84) %>%
  select(-c(sta_geometrie:typ_code_epsg,pop_code_sandre:pop_sus_id,pop_lieu_dit,
            pop_distance_mer:pop_uti_id))

#Création d’un objet géographique pour les stations
station_geo <- station %>%
  sf::st_as_sf(coords = c("x_wgs84", "y_wgs84"), crs = 4326)

mapview(station_geo)



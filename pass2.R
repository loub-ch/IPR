mef_creer_passerelle2 <- function() {
  lot_poissons %>% select(lop_id,
                          pre_id = lop_pre_id) %>%
    left_join(y = prelevement_elementaire %>%
                select(pre_id,
                       ope_id = pre_ope_id)) %>%
    left_join(y = operation %>%
                select(ope_id,qualif=ope_niq_id,
                       pop_id = ope_pop_id,
                       pro_id = ope_pro_id,
                       poste_wama = ope_poste_wama)) %>%
    left_join(y = operation_objectif %>%
                select(ope_id = opo_ope_id,
                       obj_id = opo_obj_id)) %>%
    left_join(y = point_prelevement %>%
                select (sta_id = pop_sta_id,
                        pop_id,
                        sta_id = pop_sta_id)) %>%
    select(sta_id,
           pop_id,
           ope_id,
           obj_id,
           pro_id,
           poste_wama,
           pre_id,
           lop_id,qualif)
}

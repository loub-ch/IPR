ipr_periode <- function(df,premiere_annee = NA,
                                  derniere_annee = NA)
  
{
 
df<-df %>% group_by(pop_id,sta_id,annee,ope_date) %>% filter(premiere_annee<= annee,derniere_annee >=annee)
}


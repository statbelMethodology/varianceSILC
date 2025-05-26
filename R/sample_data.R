#' example_eusilc
#'
#' Création d'un data.frame jck pour eusilc du package laeken
#'
#' @name example_eusilc
#'
#' @returns data.frame
#' @import dplyr
#' @import tidyr
#' @export
#'
#' @examples
#' # Simplement appeler cette fonction pour créer un jeu de données pour eusilc
#' df_test_jck <- example_eusilc()
#' str(df_test_jck)
example_eusilc <- function(){

  # On va supposer que dans chaque DB040, il y a deux ou trois PSU
  set.seed(123)
  df_psu <- eusilc %>%
    select(db040) %>%
    distinct() %>%
    mutate(n_psu = sample(2:3,nrow(.),replace = TRUE)) %>%
    mutate(psu_start = 1+ cumsum(lag(n_psu,default = 0)),
           psu_end = psu_start+n_psu-1)

  set.seed(123)
  df_eusilc <- eusilc %>%
    left_join(df_psu,by="db040") %>%
    mutate(scale = (n_psu-1)/n_psu) %>%
    group_by(db030) %>%
    mutate(psu = sample(first(psu_start):first(psu_end),1))

  set.seed(123)
  df_jck <- unique(df_eusilc$psu) %>% purrr::map_df(~{

    strate_jck <- df_eusilc %>% filter(psu == .x) %>% pull(db040) %>% unique()
    scale_jck <- df_eusilc %>% filter(psu == .x) %>% pull(scale) %>% unique()

    df_eusilc %>%
      select(db030,db040,psu,rb050,scale) %>%
      group_by(db030) %>% mutate(ALEA = sample(-25:25,1)) %>% ungroup() %>%
      mutate(
        PSU_JCK = .x,
        STRATE_JCK = strate_jck,
        SCALE_JCK = scale_jck,
        rb050 = case_when(
          db040 != STRATE_JCK  ~ rb050 + ALEA,
          psu != PSU_JCK ~ rb050*1/SCALE_JCK + ALEA,
          TRUE ~ 0
        ))
  })

  df_jck <- df_jck %>%
    mutate(ID_JCK = as.numeric(factor(PSU_JCK))) %>%
    group_by(STRATE_JCK,PSU_JCK) %>%
    mutate(rb050 = rb050*(sum(eusilc$rb050))/sum(rb050)) %>%
    ungroup() %>%
    distinct() %>%
    as.data.frame()

  return(df_jck)
}


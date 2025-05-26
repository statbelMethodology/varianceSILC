#' loop_svymean
#'
#' Boule d'estimations de variances v2 (package survey)
#'
#' @name loop_svymean
#'
#' @param design objet svrepdesign
#' @param vars_indics vecteur d'indicateurs
#' @param vars_domain vecteur de domaines
#' @param vars_breaks vecteur de breakdowns
#'
#' @returns data.frame, résultats de variances
#' @import dplyr
#' @import tidyr
#' @export
#'
#' @examples
#' # Nous devons reproduire les étapes de transformation en données larges pour
#' # obtenir la matrice des poids répliqués, et obtenir le vecteur des facteurs
#' # de corrections. Nous avons stocké eusilc_jck sous forme d'une liste de
#' # ménages*réplications, afin de limiter l'espace disque. Les commandes du
#' # package survey impose de générer la matrice des poids répliqués pour les
#' # individus, nous devons donc coupler préalablement à eusilc pour obtenir un
#' # fichier individus.
#'
#' repweights_matrix <- eusilc_jck %>%
#'   left_join(eusilc %>% select(db030,rb030),
#'             by="db030",relationship = "many-to-many") %>%
#'   select(db030, rb030, PSU_JCK, rb050) %>%
#'   arrange(PSU_JCK) %>%
#'   pivot_wider(
#'     id_cols = c(db030,rb030),
#'     names_from = PSU_JCK,
#'     values_from = rb050,
#'     names_prefix = "WEI_REP_",
#'     values_fill = 0
#'   ) %>%
#'   select(starts_with("WEI_REP_")) %>%
#'   as.matrix()
#'
#' scale_vector <- eusilc_jck %>% select(PSU_JCK,SCALE_JCK) %>%
#'   arrange(PSU_JCK) %>% distinct() %>% pull(SCALE_JCK)
#'
#' eusilc_design <- survey::svrepdesign(
#'   data = eusilc,
#'   weights = ~rb050,
#'   repweights = repweights_matrix,
#'   scale = 1,
#'   rscales = scale_vector,
#'   type="JKn"
#' )

#' survey::svymean(~eqIncome,eusilc_design)
#' survey::svyby(~eqIncome,~rb090,eusilc_design,survey::svymean)
#'
#' # Uniquement des variables d'intérêt
#' loop_survey(eusilc_design,
#'             c("eqIncome","py120n"))
#'
#' # Ajout de variables de sous-population
#' loop_survey(eusilc_design,
#'             c("eqIncome","py120n"),
#'             c("pl030","rb090"))
#'
#' # Ajout de variables de domaine
#' loop_survey(eusilc_design,
#'             c("eqIncome","py120n"),
#'             c("pl030","rb090"),
#'             "hsize")
loop_svymean <- function(design,
                        vars_indics,
                        vars_breaks = NULL,
                        vars_domain = NULL) {

  # Estimation sans breakdown
  for_indics <- as.formula(paste("~",paste(vars_indics,collapse = "+")))
  step_1 <- survey::svymean(for_indics,design,na.rm = TRUE) %>%
    as.data.frame() %>%
    rename(se=SE) %>%
    mutate(indic = rownames(.))
  step_1[,vars_breaks] <- NA
  step_1[,vars_domain] <- NA
  step_1$step <- "indic"
  step_1 <- step_1 %>% select(step,indic,!!!syms(vars_domain),
                              !!!syms(vars_breaks),mean,se)

  results <- list()
  results[["step1"]] <- step_1

  # Transformer les variables breakdown NA en visible
  for (var in c(vars_breaks,vars_domain)) {
    design$variables[[var]] <- as.character(design$variables[[var]])
    design$variables[[var]][is.na(design$variables[[var]])] <- "NA"
  }

  # Estimation pour chaque breakdown et domaine isolé
  results[["step2"]] <- vars_indics %>% purrr::map_df(~{
    var_indic <- .x
    c(vars_breaks,vars_domain) %>% purrr::map_df(~{
      survey::svyby(as.formula(paste0("~",var_indic)),
                    as.formula(paste0("~",.x)),
                    subset(design, !is.na(get(var_indic))),
                    survey::svymean,na.rm = TRUE) %>%
        as.data.frame() %>%
        mutate(indic = var_indic,step = "break and dom") %>%
        rename(mean = !!var_indic)
    })
  })

  if (!is.null(vars_breaks)){
    # Estimation pour chaque combinaison de breakdown
    results[["step3"]] <- vars_indics %>% purrr::map_df(~{
      var_indic <- .x
      vars_breaks <- paste(vars_breaks,collapse = "+")
      survey::svyby(as.formula(paste0("~",var_indic)),
                    as.formula(paste("~",vars_breaks)),
                    subset(design, !is.na(get(var_indic))),
                    survey::svymean,na.rm = TRUE) %>%
        as.data.frame() %>%
        mutate(indic = var_indic,step = "comb of break") %>%
        rename(mean = !!var_indic)
    })

    if (!is.null(vars_domain)){
      # Estimation pour chaque combinaison de breakdown par domain
      results[["step4"]] <- vars_indics %>% purrr::map_df(~{
        var_indic <- .x
        vars_domain %>% purrr::map_df(~{
          vars_breaks_domain <- paste(c(.x,vars_breaks),collapse = "+")
          survey::svyby(as.formula(paste0("~",var_indic)),
                        as.formula(paste("~",vars_breaks_domain)),
                        subset(design, !is.na(get(var_indic))),
                        survey::svymean,na.rm = TRUE) %>%
            as.data.frame() %>%
            mutate(indic = var_indic,step = "comb of break by dom") %>%
            rename(mean = !!var_indic)
        })
      })
    }
  }

  results <- purrr::map_df(results,rbind) %>%
    add_pop_to_results(vars_domain,vars_breaks)

  return(results)
}


#' loop_my_svymean
#'
#' Boule d'estimations de variance v1 (Approche Statbel)
#'
#' @name loop_my_svymean
#'
#' @param design objet my_svrepdesign
#' @param vars_indics vecteur d'indicateurs
#' @param vars_domain vecteur de domaines
#' @param vars_breaks vecteur de breakdowns
#'
#' @returns data.frame, résultats de variances
#' @import dplyr
#' @import tidyr
#' @export
#'
#' @examples
#' eusilc_design <- my_svyrepdesign(data=eusilc,
#'   data_jck = eusilc_jck,
#'   var_poids = "rb050",
#'   var_strate = "db040",
#'   var_facteur = "SCALE_JCK",
#'   var_id = "db030",
#'   var_JCK = "ID_JCK")
#'
#' my_svymean(eusilc_design,~eqIncome)
#' my_svymean(eusilc_design,~eqIncome,~rb090)
#'
#' # Uniquement des variables d'intérêt
#' loop_my_svymean(eusilc_design,
#'                 c("eqIncome","py120n"))
#'
#' # Ajout de variables de sous-population
#' loop_my_svymean(eusilc_design,
#'                 c("eqIncome","py120n"),
#'                 c("pl030","rb090"))
#'
#' # Ajout de variables de domaine
#' loop_my_svymean(eusilc_design,
#'                c("eqIncome","py120n"),
#'                c("pl030","rb090"),
#'                "hsize")
loop_my_svymean <- function(design,
                            vars_indics,
                            vars_breaks = NULL,
                            vars_domain = NULL) {

  # Estimation sans breakdown
  for_indics <- as.formula(paste("~",paste(vars_indics,collapse = "+")))
  step_1 <- design %>%
    my_svymean(for_indics) %>%
    as.data.frame()
  step_1[,vars_breaks] <- NA
  step_1[,vars_domain] <- NA
  step_1$step <- "indic"
  step_1 <- step_1 %>% select(step,indic,!!!syms(vars_domain),
                              !!!syms(vars_breaks),N,mean,se)

  results <- list()
  results[["step1"]] <- step_1

  # Estimation pour chaque breakdown et domain isolé
  results[["step2"]] <- c(vars_domain,vars_breaks) %>% purrr::map_df(~{
    design %>%
      my_svymean(for_indics,as.formula(paste0("~",.x))) %>%
      as.data.frame() %>%
      mutate(step = "break and dom")
  })

  if (!is.null(vars_breaks)){
    # Estimation pour chaque combinaison de breakdown
    for_breaks <- as.formula(paste("~",paste(vars_breaks,collapse = "+")))
    results[["step3"]] <- design %>%
      my_svymean(for_indics,for_breaks) %>%
      mutate(step = "comb of break")

    if (!is.null(vars_domain)){
      results[["step4"]] <- vars_domain %>% purrr::map_df(~{
        vars_breaks_domain <- paste(c(.x,vars_breaks),collapse = "+")
        design %>%
          my_svymean(for_indics,as.formula(paste("~",vars_breaks_domain))) %>%
          mutate(step = "comb of break by dom")
      })
    }
  }

  results <- purrr::map_df(results,rbind) %>%
    add_pop_to_results(vars_domain,vars_breaks)

  return(results)
}

#' add_pop_to_results
#'
#' Ajout d'une variable de population pour décrire l'estimation de la variance (usage interne)
#'
#' @name add_pop_to_results
#'
#' @param results objet results provenant de loop_svymean or loop_my_svymean
#' @param vars_domain vecteur de domaines
#' @param vars_breaks vecteur de breakdowns
#'
#' @returns results
#' @import stringr
add_pop_to_results <- function(results,vars_domain,vars_breaks){
  results <- results %>% mutate(pop = "")

  for (var in c(vars_domain,vars_breaks)){
    if (!("pop" %in% colnames(results))){
      results <- results %>%
        mutate(pop = if_else(
          is.na(!!sym(var)),"",paste0(var," = ",!!sym(var))))
    }else{
      results <- results %>%
        mutate(pop = if_else(
          is.na(!!sym(var)),pop,paste0(pop," ; ",var," = ",!!sym(var))))
    }
  }
  results <- results %>%
    mutate(pop = stringr::str_remove(pop,"^ *; *"),
           pop = stringr::str_remove(pop," *; *$"))

  return(results)
}

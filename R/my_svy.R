#' my_svyrepdesign
#'
#' Génère un design de sondage spécifique pour un jackknife stratifié
#'
#' @name my_svyrepdesign
#'
#' @param data data.frame avec les données d'enquête
#' @param data_jck data.frame avec les poids répliqués
#' @param var_poids variable de poids
#' @param var_strate variable de pseudo-strate
#' @param var_facteur variable de facteur de correction
#' @param var_id variable d'identification de l'individus/ménage
#' @param var_JCK variable d'identification de la réplication
#'
#' @returns object my_svyrepdesign
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
my_svyrepdesign <- function(data,
                            data_jck,
                            var_poids = "RB050",
                            var_strate = "STRATUM_JCK",
                            var_facteur = "FACTEUR_H",
                            var_id = "DB030",
                            var_JCK = "ID_JCK") {

  # Mise en majuscules pour homogénéiser
  # colnames(data) <- toupper(colnames(data))
  # colnames(data_jck)   <- toupper(colnames(data_jck))
  # var_poids   <- toupper(var_poids)
  # var_strate  <- toupper(var_strate)
  # var_facteur <- toupper(var_facteur)
  # var_id   <- toupper(var_id)
  # var_JCK     <- toupper(var_JCK)

  # Forcer les strates, psu et id_jck en charactère
  # data <- data %>% mutate(across(any_of(
  #   c(var_strate,var_JCK,var_id)),as.character))
  # data <- data %>% mutate(across(any_of(
  #   c(var_strate,var_JCK,var_id)),as.character))
  # data_jck <- data_jck %>% mutate(across(any_of(var_strate),as.character))

  # On crée la version "complète" (id_jck == 0) à partir des données
  data_jck_run0 <- data %>%
    select(all_of(var_id), all_of(var_poids)) %>%
    mutate(!!var_JCK   := NA,
           !!var_strate:= NA,
           !!var_facteur:= NA)

  # On fusionne : d'abord les réplicats (data_jck) et ensuite les observations complètes
  t0_jck <- bind_rows(data_jck, data_jck_run0)

  # On joint avec data pour récupérer toutes les variables d'intérêt
  # On prépare les données à coupler
  vec_vars <- t0_jck %>% select(-all_of(var_id)) %>% colnames

  design_data <- t0_jck %>%
    inner_join(data %>% select(-any_of(vec_vars)), by = var_id) %>%
    distinct()

  # Extraction du vecteur de facteurs de correction (scale_vector)
  # On suppose ici que data_jck contient les colonnes var_JCK et var_facteur
  scale_vector <- data_jck %>%
    select(all_of(var_JCK), all_of(var_facteur)) %>%
    distinct() %>%
    arrange(!!sym(var_JCK)) %>%
    pull(!!sym(var_facteur))

  obj <- list(call = match.call(),
              # data = as.data.table(design_data),
              data = design_data,
              var_poids = var_poids,
              var_strate = var_strate,
              var_facteur = var_facteur,
              var_id = var_id,
              var_JCK = var_JCK,
              scale_vector = scale_vector)
  class(obj) <- "my_svyrepdesign"
  return(obj)
}

#' my_svymean
#'
#' Cette fonction calcule la moyenne (et son écart-type) pour un indicateur,
#' éventuellement par groupe (breakdown). Elle s'inspire de la méthode jackknife :
#' on calcule l'estimateur de l'échantillon, les estimateurs pour chaque
#' réplique puis la variance
#'
#' @name my_svymean
#'
#' @param design objet my_svyrepdesign
#' @param formula Indicateur à calculer
#' @param by Variable de breakdown
#'
#' @returns  data.frame, résultats de variances
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
my_svymean <- function(design, formula, by = NULL) {
  # Extraire le nom des indicateurs
  var_indic <- all.vars(formula)

  # Récupérer les noms de variables à partir de l'objet design
  wgt_var <- design$var_poids
  jck_var <- design$var_JCK
  var_strate <- design$var_strate

  data <- design$data

  # Si une variable de breakdown est spécifiée, on la récupère
  if (!is.null(by)) {
    group_var <- paste(paste0("~", by),collapse = " ") %>%
      as.formula() %>% all.vars()
  }else{
    group_var <- "GRP_VIDE_"
    data$GRP_VIDE_ <- TRUE
  }

  # Ne conserver que les données utiles
  data <- data %>%
    select(all_of(wgt_var), all_of(jck_var), all_of(var_strate),
           all_of(group_var), all_of(var_indic))

  # Transformer les variables breakdown NA en visible
  for (var in group_var) {
    data[[var]] <- as.character(data[[var]])
    data[[var]][is.na(data[[var]])] <- "NA"
  }

  # Modifier le nom des indics s'ils sont présents dans le group
  info_modif <- NULL
  for (indic in var_indic){
    if (indic %in% group_var){
      var_indic[which(var_indic==indic)] <- paste0(indic,"_")
      data[,paste0(indic,"_")] <- data[,indic]
      info_modif <- c(info_modif,indic)
    }
  }

  # Données avec ou sans les réplicats
  full_sample <- data %>% filter(is.na(!!sym(jck_var)))
  rep_sample <- data %>% filter(!is.na(!!sym(jck_var)))

  # Estimation pour la moyenne
  full_est <- full_sample %>%
    group_by(!!!syms(group_var)) %>%
    summarise(
      N = n(),
      across(
        .cols = all_of(var_indic),
        .fns = ~ weighted.mean(.x,!!sym(wgt_var), na.rm = TRUE)
      )
      , .groups = "drop") %>%
    pivot_longer(
      cols = -c(N,!!!syms(group_var)),
      names_to = "indic",values_to = "mean"
    ) %>%
    ungroup()

  # Estimation pour la moyenne des réplications
  rep_est <- rep_sample %>%
    group_by(!!sym(var_strate), !!sym(jck_var), !!!syms(group_var)) %>%
    summarise(
      across(
        .cols = all_of(var_indic),
        .fns = ~ weighted.mean(.x,!!sym(wgt_var), na.rm = TRUE)
      )
      , .groups = "drop") %>%
    pivot_longer(
      cols = -c(!!sym(var_strate), !!sym(jck_var), !!!syms(group_var)),
      names_to = "indic",values_to = "mean_rep"
    ) %>%
    ungroup()

  # Jointure des estimateurs réplicats avec l'estimateur complet
  joined <- rep_est %>% left_join(full_est, by = c("indic",group_var))

  # On doit maintenant associer, pour chaque réplicat, le facteur de
  # correction correspondant. On suppose que les identifiants de réplicats
  # (jck_var) sont des nombres non nuls.
  rep_ids <- sort(unique(joined[[jck_var]]))
  scale_df <- tibble(rep_id = rep_ids,scale = design$scale_vector)
  # Renommer la variable jck_var dans joined pour la jointure
  joined <- joined %>% rename(rep_id = !!sym(jck_var)) %>%
    left_join(scale_df, by = "rep_id")

  # Calcul de la variance jackknife :
  # Si breakdown, pour chaque groupe :
  # variance = sum_r scale*(mean_rep - mean)^2
  strate_by <- joined %>%
    group_by(indic,!!!syms(group_var),!!sym(var_strate)) %>%
    summarise(mean_strate = mean(mean_rep, na.rm = TRUE), .groups = "drop")

  variance_by <- strate_by %>%
    left_join(joined %>%
                select(indic,!!!syms(group_var),
                       !!sym(var_strate),scale,mean_rep) %>%
                distinct()
              ,by=c("indic",group_var,var_strate)) %>%
    group_by(indic,!!!syms(group_var)) %>%
    summarise(var = sum(scale * (mean_rep - mean_strate)^2, na.rm = TRUE)
              ,.groups = "drop") %>%
    ungroup()

  result <- full_est %>%
    left_join(variance_by, by = c("indic",group_var)) %>%
    mutate(se = sqrt(var))

  result <- result %>%
    select(indic,!!!syms(group_var),N,mean,se)


  for (info in info_modif){
    result[result$indic == paste0(info,"_"),'indic'] <- info
  }

  if (is.null(by)) {
    result <- result %>% select(-GRP_VIDE_)
  }

  return(result)
}


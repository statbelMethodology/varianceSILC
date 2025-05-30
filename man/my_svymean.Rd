% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_svy.R
\name{my_svymean}
\alias{my_svymean}
\title{my_svymean}
\usage{
my_svymean(design, formula, by = NULL)
}
\arguments{
\item{design}{objet my_svyrepdesign}

\item{formula}{Indicateur à calculer}

\item{by}{Variable de breakdown}
}
\value{
data.frame, résultats de variances
}
\description{
Cette fonction calcule la moyenne (et son écart-type) pour un indicateur,
éventuellement par groupe (breakdown). Elle s'inspire de la méthode jackknife :
on calcule l'estimateur de l'échantillon, les estimateurs pour chaque
réplique puis la variance
}
\examples{
eusilc_design <- my_svyrepdesign(data=eusilc,
  data_jck = eusilc_jck,
  var_poids = "rb050",
  var_strate = "db040",
  var_facteur = "SCALE_JCK",
  var_id = "db030",
  var_JCK = "ID_JCK")

my_svymean(eusilc_design,~eqIncome)
my_svymean(eusilc_design,~eqIncome,~rb090)
}

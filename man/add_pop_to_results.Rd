% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loop.R
\name{add_pop_to_results}
\alias{add_pop_to_results}
\title{add_pop_to_results}
\usage{
add_pop_to_results(results, vars_domain, vars_breaks)
}
\arguments{
\item{results}{objet results provenant de loop_svymean or loop_my_svymean}

\item{vars_domain}{vecteur de domaines}

\item{vars_breaks}{vecteur de breakdowns}
}
\value{
results
}
\description{
Ajout d'une variable de population pour décrire l'estimation de la variance (usage interne)
}

---
editor_options: 
  markdown: 
    wrap: 72
---

# varianceSILC

<!-- badges: start -->

<!-- badges: end -->

Estimation de la variance dans les enquêtes SILC - Méthode du jackknife
stratifé et approche par Statbel

## Présentation

Le package `varianceSILC` fournit une implémentation dédiée à
l’estimation de la variance dans les enquêtes complexes telles que SILC
(Statistics on Income and Living Conditions), en s’appuyant sur une
méthode par poids répliqués construits via la technique du jackknife
stratifié. Il est conçu pour reproduire le plan de sondage utilisé par
Statbel, tout en proposant une alternative plus fidèle que l’approche
générique du package `survey`.

## Pourquoi varianceSILC ?

Le package `survey` de R permet une estimation de variance à partir de
poids répliqués. Toutefois, son implémentation repose sur une formule
dite "globale" (v2) qui peut surestimer la variance dans certains cas.
Le package `varianceSILC`, quant à lui : - Implémente la formule locale
(v1) recommandée dans les manuels spécialisés (Wolter, Jones), - Tient
compte de l’organisation des pseudo-strates créées pour simuler un
tirage systématique, - Est compatible avec les poids répliqués diffusés
avec les données SILC de Statbel.

## Fonctionnalités principales

-   `my_svyrepdesign()` : construit un design d’enquête avec poids
    répliqués au format jackknife stratifié.
-   `my_svymean()` : calcule la moyenne d’indicateurs, avec estimation
    robuste de la variance.
-   `loop_my_svymean()` : estime efficacement la variance d’un ensemble
    d’indicateurs sur plusieurs croisements de sous-populations.
-   Données d’exemple intégrées : `eusilc` et `eusilc_jck`, pour tester
    les méthodes sur une base synthétique inspirée de SILC.

## Installation

``` r
# En local depuis GitHub
# remotes::install_github("tdelc/varianceSILC")
```

## Exemple

Voici un exemple simple d'utilisation du package :

``` r
library(tidyverse)
library(varianceSILC)

# Construction du design avec poids répliqués (jackknife stratifié)
design <- my_svyrepdesign(
  data = eusilc,
  data_jck = eusilc_jck,
  var_poids = "rb050",
  var_strate = "db040",
  var_facteur = "SCALE_JCK",
  var_id = "db030",
  var_JCK = "ID_JCK"
)

# Estimation de la moyenne pondérée avec erreur-type
my_svymean(design, ~eqIncome)
my_svymean(design, ~eqIncome, ~rb090)

# Ajout de variables de sous-population
result <- loop_my_svymean(eusilc_design,"eqIncome",c("pl030","rb090"))

# Ajout de variables de domaine
result <- loop_my_svymean(eusilc_design,"eqIncome",c("pl030","rb090"),"hsize")
                            
```

## Comparaison avec le package `survey`

Une analyse de la littérature met en évidence plusieurs variantes
possibles du calcul de la variance. `varianceSILC` implémente la formule
'originelle', qui est :

-   Sans biais,
-   Plus précise dans les cas d’hétérogénéité entre strates,
-   Alignée avec les macros de production officielles de Statbel.

Le package `survey`, en revanche, utilise une variante, avec la variance
autour de la moyenne des réplications, plus simple mais potentiellement
plus conservatrice.

## Données disponibles

Le package intègre :

-   `eusilc` : données simulées au format `laeken`,

-   `eusilc_jck` : poids répliqués simulés selon le protocole jackknife.

Certains utilisateur·rices disposant d'un accès aux micro-données
pouvant faire une demande pour obtenir le fichier des poids répliqués.

## Auteurs

Développé par Thomas Delclite, Service Méthodologie, Statbel, l'office
National de Statistique, Belgique

## Licence

Ce package est fourni à titre expérimental pour usage interne et
académique. Merci de contacter Statbel pour toute utilisation
officielle. Il est interdit de diffuser les poids répliqués.

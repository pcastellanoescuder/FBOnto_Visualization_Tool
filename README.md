# Welcome to fobitoolsGUI! <img src='pix/logo.png' align="right" height="139" />

<!-- badges: start --> 

[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Last Commit](https://img.shields.io/github/last-commit/pcastellanoescuder/fobitoolsGUI.svg)](https://github.com/pcastellanoescuder/fobitoolsGUI/commits/master)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![DOI](https://img.shields.io/badge/DOI-https%3A%2F%2Fdoi.org%2F10.1093%2Fdataba%2Fbaaa033-blue)](https://doi.org/10.1093/databa/baaa033)  

<!-- badges: end -->

## Overview

`fobitoolsGUI` is a web-based tool based on the [`fobitools` R package](https://github.com/pcastellanoescuder/fobitools). This user-friendly web interface provides a set of tools for interacting with [FOBI (Food-Biomarker Ontology)](https://github.com/pcastellanoescuder/FoodBiomarkerOntology). A collection of basic manipulation tools for biological significance analysis, graph visualization and text mining strategies for annotating nutritional data are provided here:

  - FOBI graph static visualization
  - FOBI graph dynamic visualization
  - Extract FOBI information in a downloadable table
  - Compound ID conversion (among metabolite names, FOBI, ChemSpider, KEGG, PubChemCID, InChIKey, InChICode and HMDB IDs)
  - Biological significance analysis using ORA and MSEA methods:
    - Chemical class enrichment analysis: ORA and MSEA using FOBI chemical classes as metabolite sets
    - **Food enrichment analysis**: ORA and MSEA using FOBI food groups as metabolite sets
  - Text mining algorithm for annotating free-text dietary data

`fobitoolsGUI` is hosted at our own server, freely available at http://webapps.nutrimetabolomics.com/fobitoolsGUI.

## Run fobitoolsGUI locally

### Step 1: Install package dependencies

Open a [RStudio](https://rstudio.com) console and run:

``` r
# CRAN packages

installifnot <- function(pckgName){
  if (!(require(pckgName, character.only = TRUE))) {
    install.packages(pckgName, dep = TRUE)
    require(pckgName, character.only = TRUE)
  }
}

pk1 <- c("shiny", "DT", "dplyr", "readxl", "readr", "ontologyIndex", "igraph",
         "ggplot2", "shinythemes", "shinyWidgets", "networkD3", "BiocManager")

for (i in 1:length(pk1)){
  installifnot(pk1[i])
}

# Bioconductor packages

BiocManager::install(c("fobitools", "BioNet"))
```

### Step 2: Launch fobitoolsGUI locally

Once all dependencies have been installed run the following command and enjoy the fobitoolsGUI!  

``` r
shiny::runGitHub("pcastellanoescuder/fobitoolsGUI")
```

## Citation

**Pol Castellano-Escuder, Raúl González-Domínguez, David S Wishart, Cristina Andrés-Lacueva, Alex Sánchez-Pla**, _FOBI: an ontology to represent food intake data and associate it with metabolomic data_, Database, Volume 2020, 2020, baaa033. DOI: [https://doi.org/10.1093/databa/baaa033](https://doi.org/10.1093/databa/baaa033)   

## Code of Conduct

Please note that the 'fobitoolsGUI' project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.  


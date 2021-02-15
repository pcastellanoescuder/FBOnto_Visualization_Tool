# Welcome to fobitoolsGUI!

<!-- badges: start --> 

[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Last Commit](https://img.shields.io/github/last-commit/pcastellanoescuder/fobitoolsGUI.svg)](https://github.com/pcastellanoescuder/fobitoolsGUI/commits/master)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![DOI](https://img.shields.io/badge/DOI-https%3A%2F%2Fdoi.org%2F10.1093%2Fdataba%2Fbaaa033-blue)](https://doi.org/10.1093/databa/baaa033)  

<!-- badges: end -->

## Overview

`fobitoolsGUI` is a web-based tool based on the [`fobitools` R package](https://github.com/pcastellanoescuder/fobitools). This user-friendly web interface is focused on the [FOBI](https://github.com/pcastellanoescuder/FoodBiomarkerOntology) ontology visualization and the **food enrichment analysis** for nutrimetabolomic studies. Several functionalities for interacting with FOBI are provided here:

  - FOBI graph static visualization
  - FOBI graph dynamic visualization
  - Extract FOBI information in a downloadable table
  - Compound ID conversion (among metabolite names, FOBI, ChemSpider, KEGG, PubChemCID, InChIKey, InChICode and HMDB IDs)
  - Biological significance analysis via a classical over representation analysis:
      - Chemical class enrichment analysis: Over representation analysis using FOBI chemical classes as sets (tables and plots provided)
      - **Food enrichment analysis**: Over representation analysis using FOBI food groups as sets (tables and plots provided)
  - Text mining function for annotating dietary data

`fobitoolsGUI` is hosted at our own server and it's available at http://webapps.nutrimetabolomics.com/fobitoolsGUI.

## Run fobitoolsGUI locally

### Step 1: Install package dependencies

Open a [RStudio](https://rstudio.com) console and run:

```
# CRAN packages

installifnot <- function(pckgName){
  if (!(require(pckgName, character.only = TRUE))) {
    install.packages(pckgName, dep = TRUE)
    require(pckgName, character.only = TRUE)
  }
}

pk1 <- c("shiny", "DT", "tidyverse", "ggraph", "tidygraph", "readxl",
         "ggrepel", "shinythemes", "shinyWidgets", "networkD3", "BiocManager")
         
for (i in 1:length(pk1)){
  installifnot(pk1[i])
}

# Bioconductor packages

BiocManager::install(version = "devel")
BiocManager::install("fobitools")
```

### Step 2: Launch fobitoolsGUI locally :tada:

Once all dependencies have been installed run the following command and enjoy the fobitoolsGUI!  

```
shiny::runGitHub("pcastellanoescuder/fobitoolsGUI")
```    

## Run fobitoolsGUI Docker container image

### Step 1: Pull Docker image

Pull the fobitoolsGUI Docker container image hosted at [Docker Hub](https://hub.docker.com/repository/docker/pcastellanoescuder/fobitoolsgui) by running the following command in the terminal.

```
docker pull pcastellanoescuder/fobitoolsgui
```

### Step 2: Run Docker image

Run the container on your terminal once it has been pulled.   

```
docker run -d --rm -p 3838:3838 fobitoolsgui
```

### Step 3: Run fobitoolsGUI in your browser

Open your browser and paste `http://0.0.0.0:3838`. Then, enjoy the fobitoolsGUI!   

## Citation

**Pol Castellano-Escuder, Raúl González-Domínguez, David S Wishart, Cristina Andrés-Lacueva, Alex Sánchez-Pla**, _FOBI: an ontology to represent food intake data and associate it with metabolomic data_, Database, Volume 2020, 2020, baaa033. DOI: [https://doi.org/10.1093/databa/baaa033](https://doi.org/10.1093/databa/baaa033)   

## Code of Conduct

Please note that the 'fobitoolsGUI' project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.  


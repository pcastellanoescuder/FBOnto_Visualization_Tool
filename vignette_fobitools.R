
library(dplyr)
library(metabolomicsWorkbenchR)
library(POMA)
library(rvest)
library(SummarizedExperiment)
library(fobitools)

data_negative_mode <- do_query(
  context = "study",
  input_item = "analysis_id",
  input_value = "AN000062",
  output_item = "SummarizedExperiment")

data_positive_mode <- do_query(
  context = "study",
  input_item = "analysis_id",
  input_value = "AN000063",
  output_item = "SummarizedExperiment")

#####

metaboliteNamesURL <- "https://www.metabolomicsworkbench.org/data/show_metabolites_by_study.php?STUDY_ID=ST000041&SEARCH_TYPE=KNOWN&STUDY_TYPE=MS&RESULT_TYPE=1"
metaboliteNames <- metaboliteNamesURL %>% 
  read_html() %>% 
  html_nodes(".datatable")

metaboliteNames_negative <- metaboliteNames %>%
  .[[1]] %>%
  html_table() %>%
  dplyr::select(`Metabolite Name`, PubChemCompound_ID, `Kegg Id`)

metaboliteNames_positive <- metaboliteNames %>%
  .[[2]] %>%
  html_table() %>%
  dplyr::select(`Metabolite Name`, PubChemCompound_ID, `Kegg Id`)

metaboliteNames <- bind_rows(metaboliteNames_negative, metaboliteNames_positive) %>%
  rename(names = 1, PubChem = 2, KEGG = 3) %>%
  mutate(KEGG = ifelse(KEGG == "-", "UNKNOWN", KEGG),
         PubChem = ifelse(PubChem == "-", "UNKNOWN", PubChem)) %>%
  filter(!duplicated(PubChem))

##### 
  
features_negative <- assay(data_negative_mode) %>%
  dplyr::slice(-n(), -(n()-1))
rownames(features_negative) <- rowData(data_negative_mode)$metabolite[1:(length(rowData(data_negative_mode)$metabolite)-2)]

##

features_positive <- assay(data_positive_mode) %>%
  dplyr::slice(-n(), -(n()-1))
rownames(features_positive) <- rowData(data_positive_mode)$metabolite[1:(length(rowData(data_positive_mode)$metabolite)-2)]

##

pdata <- colData(data_negative_mode) %>% # or data_positive_mode
  as.data.frame() %>%
  tibble::rownames_to_column("ID")

##

features <- bind_rows(features_negative, features_positive) %>%
  tibble::rownames_to_column("names") %>%
  right_join(metaboliteNames, by = "names") %>%
  select(-names, -KEGG) %>%
  tibble::column_to_rownames("PubChem")

#####

data_msnset <- PomaMSnSetClass(target = pdata, features = t(features))

data_preprocessed <- data_msnset %>%
  PomaImpute(ZerosAsNA = TRUE, cutoff = 70, method = "rf") %>%
  PomaNorm(method = "log_pareto") %>%
  PomaOutliers(coef = 3)

limma_res <- data_preprocessed %>%
  PomaLimma(contrast = "Control-NAFLD", adjust = "fdr") %>%
  tibble::rownames_to_column("PubChemCID")

FOBI_names <- limma_res %>%
  dplyr::pull("PubChemCID") %>%
  fobitools::id_convert()

left_join(limma_res, FOBI_names, by = "PubChemCID")








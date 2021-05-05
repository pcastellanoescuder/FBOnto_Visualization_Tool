
library(tidyverse)
library(metabolomicsWorkbenchR)
library(SummarizedExperiment)
library(POMA)
library(fobitools)

##

data <- do_query(
  context = "study",
  input_item = "analysis_id",
  input_value = "AN000961",
  output_item = "SummarizedExperiment")

##

features <- assay(data)
rownames(features) <- rowData(data)$metabolite

##

pdata <- colData(data) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("ID") %>%
  relocate(grouping, .before = Sodium.level) %>%
  mutate(grouping = case_when(grouping == "A(salt sensitive)" ~ "A",
                              grouping == "B(salt insensitive)" ~ "B"))

##

data_msnset <- PomaMSnSetClass(target = pdata, features = t(features))

data_preprocessed <- data_msnset %>%
  PomaImpute(ZerosAsNA = TRUE, cutoff = 20, method = "knn") %>%
  PomaNorm(method = "log_pareto") %>%
  PomaOutliers(coef = 3)

limma_res <- data_preprocessed %>%
  PomaLimma(contrast = "A-B", adjust = "fdr") %>%
  tibble::rownames_to_column("ID")

# cat(limma_res$ID, sep = "\n")
# ST000629_MetaboAnalyst_MapNames <- readr::read_delim("https://www.metaboanalyst.ca/MetaboAnalyst/resources/users/guest2496630745059799380tmp/name_map.csv", delim = ",")

annotated_limma <- ST000629_MetaboAnalyst_MapNames %>%
  dplyr::rename(ID = Query) %>%
  right_join(limma_res, by = "ID")
  
limma_FOBI_names <- annotated_limma %>% 
  dplyr::pull("KEGG") %>%
  fobitools::id_convert(to = "FOBI") %>%
  right_join(annotated_limma, by = "KEGG") %>%
  dplyr::select(FOBI, KEGG, ID, logFC, P.Value, adj.P.Val) %>%
  dplyr::arrange(-dplyr::desc(adj.P.Val))

##

metaboliteList <- limma_FOBI_names$FOBI[limma_FOBI_names$adj.P.Val < 0.05]
metaboliteUniverse <- limma_FOBI_names$FOBI

fobitools::ora(metaboliteList = metaboliteList,
               metaboliteUniverse = metaboliteUniverse,
               pvalCutoff = 1)




data <- readr::read_csv2("csv/200226_fobi-export.csv")

fobi <- data[!is.na(data$Type), ]
fobi$Entity <- gsub("'", '', fobi$Entity)
fobi$`Superclass(es)` <- gsub("'", '', fobi$`Superclass(es)`)
fobi$BiomarkerOf <- gsub("'", "", fobi$BiomarkerOf, fixed = T)
fobi$Contains <- gsub("'", "", fobi$Contains, fixed = T)
fobi <- data.table(fobi)

#### Superclasses

superclasses <- fobi %>% dplyr::select(Entity, `Superclass(es)`) %>% mutate(Property = "is_a")
colnames(superclasses)[1:2] <- c("from", "to")

names <- superclasses$from

#### BiomarkerOf

biomarkerof <- fobi[, list(from = Entity,
                           to = unlist(strsplit(BiomarkerOf, '\t'))), 
                    by = 1:nrow(fobi)]
biomarkerof <- biomarkerof %>% dplyr::select(-nrow) %>% mutate(Property = "BiomarkerOf")
biomarkerof <- biomarkerof %>% na.omit

#### Contains

contains <- fobi[, list(from = Entity,
                        to = unlist(strsplit(Contains, '\t'))), 
                 by = 1:nrow(fobi)]
contains <- contains %>% dplyr::select(-nrow) %>% mutate(Property = "Contains")
contains <- contains %>% na.omit

####

graph_table <- rbind(superclasses, biomarkerof, contains)


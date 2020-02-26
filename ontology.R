
path <- "ontology/fobi_old.obo"

ontology <- get_ontology(path, extract_tags = "everything")

biomarker_of <- ontology$`FOBI:00422` %>%
  map(as_tibble) %>%
  bind_rows(.id = "parent") %>%
  add_column(type = "BiomarkerOf") 

is_a <- ontology$is_a %>%
  map(as_tibble) %>%
  bind_rows(.id = "parent") %>%
  add_column(type = "is_a")

ingredient_of <- ontology$`FOBI:00424` %>%
  map(as_tibble) %>%
  bind_rows(.id = "parent") %>%
  add_column(type = "IsIngredientOf") 

names <- ontology$name %>%
  map(as_tibble) %>%
  bind_rows(.id = "ID") %>%
  rename(value = value)

names2 <- names[!(names$value %in% c("Foods", "Biomarkers", "BiomarkerOf", "hasBiomarker", "Contains", "IsIngredientOf")) ,]

BIOMARKERS <- get_descendants(ontology, "FOBI:01501")
FOODS <- get_descendants(ontology, "FOBI:0001")

BIOMARKERSc <- get_term_property(ontology, "FOBI:01501", property_name = "children")
FOODSc <- get_term_property(ontology, "FOBI:0001",  property_name = "children")



path<-"data/FBOnto2018_6.obo"

ontology <- get_ontology(path, extract_tags = "everything")

biomarker_of <- ontology$`FB_00422` %>%
  map(as_tibble) %>%
  bind_rows(.id = "parent") %>%
  add_column(type = "BiomarkerOf") 

is_a <- ontology$is_a %>%
  map(as_tibble) %>%
  bind_rows(.id = "parent") %>%
  add_column(type = "is_a")

ingredient_of <- ontology$`FB_00424` %>%
  map(as_tibble) %>%
  bind_rows(.id = "parent") %>%
  add_column(type = "IsIngredientOf") 

names <- ontology$name %>%
  map(as_tibble) %>%
  bind_rows(.id = "ID") %>%
  rename(value = value)

names2 <- names[!(names$value %in% c("Foods", "Biomarkers", "BiomarkerOf", "hasBiomarker", "Contains", "IsIngredientOf")) ,]

BIOMARKERS <- get_descendants(ontology, "FB_01501")
FOODS <- get_descendants(ontology, "FB_0001")

BIOMARKERSc <- get_term_property(ontology, "FB_01501", property_name = "children")
FOODSc <- get_term_property(ontology, "FB_0001",  property_name = "children")




load("data/201106_fobi.RData") # obtained via `fobitools::parse_fobi()`
load("data/foods.rda") # obtained via `fobitools::parse_fobi(terms = "FOBI:0001", get = "des")`

names <- fobi %>%
  pull(name)
  
contains <- fobi %>%
  mutate(Property = ifelse(!is.na(Contains), "Contains", NA)) %>%
  filter(!is.na(Property)) %>%
  select(name, Contains, Property) %>%
  rename(from = 1, to = 2, Property = 3)
  
biomarkerof <- fobi %>%
  mutate(Property = ifelse(!is.na(BiomarkerOf), "BiomarkerOf", NA)) %>%
  filter(!is.na(Property)) %>%
  select(name, BiomarkerOf, Property) %>%
  rename(from = 1, to = 2, Property = 3)

is_a <- fobi %>%
  select(name, is_a_name) %>%
  mutate(Property = "is_a") %>%
  filter(!duplicated(name)) %>%
  rename(from = 1, to = 2, Property = 3)

####

graph_table <- rbind(is_a, biomarkerof, contains)


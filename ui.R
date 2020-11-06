source("helpers.R")
source("get_relations.R")

tagList(
  
  navbarPage(
    title = "Food-Biomarker Ontology Visualization Tool",
    theme = shinytheme("spacelab"),
    
    ## ==================================================================================== ##
    ## GRAPH TAB
    ## ==================================================================================== ## 
    
    tabPanel("Graph", 
             sidebarPanel(width = 3,
                          
                          selectizeInput("FOBI_name", 
                                         label = "Entity:",
                                         multiple = TRUE,
                                         choices = names, 
                                         selected = c("4,5-dicaffeoylquinic acid", "Quinic acids and derivatives", "Cyclitols and derivatives")
                          ), 
                          
                          selectizeInput("property", 
                                         label = "Property:",
                                         multiple = TRUE,
                                         choices = c("is_a", "BiomarkerOf", "Contains"),
                                         selected = c("is_a", "BiomarkerOf")
                          ),
                          
                          selectInput("layout", "Layout:", 
                                      choices = c('fr', 'kk', 'lgl', 'graphopt', 'drl', 'linear')
                          ),
                          
                          checkboxInput("plotnames", "Network Labels", 
                                        value = TRUE
                          ),
                          
                          conditionalPanel("input.plotnames",
                                           
                                           radioButtons("labeltext",
                                                        "Label type:",
                                                        c("Label" = "label",
                                                          "Text" = "text")
                                           ),
                                           sliderInput("labelsize", 
                                                       "Label size", 
                                                       min = 1, 
                                                       max = 10, 
                                                       value = 5,
                                                       step = 1
                                           )
                          ),
                          
                          sliderInput("a_node", "Nodes alfa", min = 0, max = 1, value = 1, step = 0.1),
                          
                          sliderInput("a_edge", "Edges alfa", min = 0, max = 1, value = 1, step = 0.1)
                          
             ),
             
             mainPanel(
               plotOutput("ontologyplot", height = "750px", width = "1000px")
               )
             ),
    
    ## ==================================================================================== ##
    ## TABLE TAB
    ## ==================================================================================== ## 
    
    tabPanel("Table", DT::dataTableOutput("ontologytable")
             ),
    
    ## ==================================================================================== ##
    ## ENRICHMENT ANALYSIS TAB
    ## ==================================================================================== ## 
    
    tabPanel("Food Enrichment Analysis", 
             
             sidebarPanel(width = 3,
                          
                          textAreaInput("ora_metabolites", 
                                        label = "Enter your metabolites here:",
                                        value = "FOBI:030318
FOBI:030653
FOBI:030663
FOBI:030342
FOBI:030325
FOBI:030375
FOBI:030421
FOBI:030431
FOBI:030450
FOBI:030629",
                                        # cat("FOBI:030318", "FOBI:030653", "FOBI:030663", "FOBI:030342", "FOBI:030325", "FOBI:030375",
                                        #             "FOBI:030421", "FOBI:030431", "FOBI:030450", "FOBI:030629", sep = "\n"),
                                        height = "220px"
                          ),
                          
                          helpText(HTML("Note: can use metabolite names, FOBI, ChemSpider, KEGG, PubChemCID, InChIKey, InChICode and HMDB IDs")),
                          
                          selectInput("correction_method_ora",
                                      "Correction method",
                                      choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr"),
                                      selected = "fdr"
                                      )
                          
             ),
             
             mainPanel(
               tabsetPanel(
                 
                 tabPanel("Table",
                          DT::dataTableOutput("oratable")
                          ),
                 tabPanel("Plot",
                          plotOutput("oraplot")
                          )
                 )
               )
             )
    ),
  
  ## ==================================================================================== ##
  ## FOOTER
  ## ==================================================================================== ##              
  footer = p(hr(), p("ShinyApp created by ", a(HTML("<b>Pol Castellano Escuder</b>"), href = "https://pcastellanoescuder.github.io"), align = "center", width = 4),
             p(("Statistics and Bioinformatics Lab and Biomarkers and Nutritional & Food Metabolomics Lab from "), align = "center", width = 4),
             p(("University of Barcelona"), align = "center", width = 4),
             p(("Copyright (C) 2020, code licensed under GPLv3"), align = "center", width = 4),
             p(("Code available on Github:"), a("https://github.com/pcastellanoescuder/FOBI_Visualization_Tool", 
                                                href = "https://github.com/pcastellanoescuder/FOBI_Visualization_Tool"), align = "center", width = 4),
             p(("Please cite:"), a("https://doi.org/10.1093/databa/baaa033", href = "https://doi.org/10.1093/databa/baaa033"), align = "center", width = 4)
  )

  # tags$head(includeScript("google-analytics.js"))
  
  )


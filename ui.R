source("helpers.R")
source("get_relations.R")

tagList(
  
  navbarPage(
    title = "Food-Biomarker Ontology Visualization Tool",
    theme = shinytheme("spacelab"),
    
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
               plotOutput("ontologyplot", height = "600px")
             )
    ),
    
    tabPanel("Table", DT::dataTableOutput("ontologytable")
    ),
    
    tabPanel("Enrichment Analysis")
    
  ),
  
  ## ==================================================================================== ##
  ## FOOTER
  ## ==================================================================================== ##              
  footer = p(hr(), p("ShinyApp created by Pol Castellano Escuder",align="center",width=4),
             p(("University of Barcelona"),align="center",width=4),
             p(("Copyright (C) 2020, code licensed under GPLv3"),align="center",width=4),
             p(("Code available on Github:"),a("https://github.com/pcastellanoescuder/FOBI_Visualization_Tool",href="https://github.com/pcastellanoescuder/FOBI_Visualization_Tool"),align="center",width=4),
             p(("Cite:"),a("https://github.com/pcastellanoescuder/FoodBiomarkerOntology",href="https://github.com/pcastellanoescuder/FoodBiomarkerOntology"),align="center",width=4)
  )
  ## ==================================================================================== ##
  ## end
  ## ==================================================================================== ## 
  # tags$head(includeScript("google-analytics.js"))
  
  )


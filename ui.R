source("helpers.R")
source("ontology.R")

navbarPage("Food-Biomarker Ontology Visualization Tool", id="nav", 
           
           theme = shinytheme("spacelab"),
           
           tabPanel("Network",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                        ),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 325, height = "auto",
                                      
                                      h2("Network parameters"),
                                      
                                      selectizeInput("FBOnto_name", label = "FOBI object:",
                                                  choices = names2$value, selected = "Vegetables"),
                                      
                                      selectInput("level", "Level:", 
                                                  choices = c('all', 'ancestors', 'descendants'), selected = 'descendants'),
                                      
                                      checkboxGroupInput("property", "Properties:",
                                                         c("is_a" = "is_a",
                                                           "BiomarkerOf" = "BiomarkerOf",
                                                           "IsIngredientOf" = "IsIngredientOf"),
                                                         selected = "is_a"),
                                      
                                      selectInput("layout", "Layout:", 
                                                  choices = c('fr', 'kk', 'lgl', 'graphopt', 'drl', 'linear')),
                                      
                                      checkboxInput("plotnames", "Network Labels"),
                                      
                                      conditionalPanel("input.plotnames",
                                                       radioButtons("labeltext",
                                                                    "Label type:",
                                                                    c("Label" = "label",
                                                                      "Text" = "text")),
                                                       sliderInput("labelsize", "Label size", 1, 10, 6, step = 1)),
                                      
                                      sliderInput("a_node", "Nodes alfa", 0,1,1, step = 0.1),
                                      
                                      sliderInput("a_edge", "Edges alfa", 0,1,1, step = 0.1)

                                      ),
                        
                    br(),br(),

                    plotOutput("ontologyplot", width="85%", height="95%")),
                    
                    br(),br(),br(),br(),br(),
                    
                    ## FOOTER
                    
                    tags$div(id="cite", tags$a(href="https://github.com/pcastellanoescuder/FoodBiomarkerOntology", 
                                               tags$em('Food-Biomarker Ontology')),
                             ' by Pol Castellano-Escuder et al. (2019).',
                             ' Code available on Github: ', tags$a(href="https://github.com/pcastellanoescuder/FBOnto_Visualization_Tool", 
                                                           tags$em('https://github.com/pcastellanoescuder/FBOnto_Visualization_Tool'))),
                    br()
                    ),
          
            tabPanel("Table",

                     DT::dataTableOutput("ontologytable")
            ),
           
           br(),
           
           ## FOOTER
           
           tags$div(id="cite", tags$a(href="https://github.com/pcastellanoescuder/FoodBiomarkerOntology", 
                                      tags$em('Food-Biomarker Ontology')),
                    ' by Pol Castellano-Escuder et al. (2019).',
                    ' Code available on Github: ', tags$a(href="https://github.com/pcastellanoescuder/FOBI_Visualization_Tool", 
                                                          tags$em('https://github.com/pcastellanoescuder/FOBI_Visualization_Tool'))),
           br()
)


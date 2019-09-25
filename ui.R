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
                                      width = 330, height = "auto",
                                      
                                      h2("Network parameters"),
                                      
                                      selectizeInput("FBOnto_name", label = "FBOnto object:",
                                                  choices = names2$value, selected = "Vegetables"),
                                      
                                      selectInput("level", "Level", 
                                                  choices = c('all', 'ancestors', 'descendants'), selected = 'descendants'),
                                      
                                      checkboxGroupInput("property", "Properties",
                                                         c("is_a" = "is_a",
                                                           "BiomarkerOf" = "BiomarkerOf",
                                                           "IsIngredientOf" = "IsIngredientOf"),
                                                         selected = "is_a"),
                                      
                                      selectInput("layout", "Layout", 
                                                  choices = c('fr', 'kk', 'lgl', 'graphopt', 'drl', 'linear')),
                                      
                                      checkboxInput("plotnames", "Network Labels"),
                                      
                                      sliderInput("a_node", "Nodes alfa", 0,1,1, step = 0.1),
                                      
                                      sliderInput("a_edge", "Edges alfa", 0,1,1, step = 0.1)

                                      ),
                        
                        br(),
                        #downloadButton("download_plot", "Download Plot"),
                        br(),

                        plotOutput("ontologyplot", width="100%", height="100%")
                        )#,
                        
                        #tags$div(id="cite", tags$em('Food-Biomarker Ontology'), ' by Pol Castellano-Escuder et al. (2019).'
                        #)
                    ),
          
            tabPanel("Table",

                     DT::dataTableOutput("ontologytable")
            )
)


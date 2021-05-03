source("helpers.R")

tagList(
  
  navbarPage(
    title = "fobitoolsGUI",
    theme = shinytheme("spacelab"),
    
    ## ==================================================================================== ##
    ## HOME TAB
    ## ==================================================================================== ## 
    
    tabPanel("Home", includeMarkdown("README.md")
    ),
    
    ## ==================================================================================== ##
    ## GRAPH TAB
    ## ==================================================================================== ## 
    
    tabPanel("Graph", 
             sidebarPanel(width = 3,
                          
                          selectizeInput("FOBI_name",
                                         label = "Entity:",
                                         multiple = TRUE,
                                         choices = NULL, 
                                         selected = NULL
                          ), 
                          
                          selectizeInput("get_graph", 
                                         label = "Get:",
                                         multiple = FALSE,
                                         choices = c('None' = "NULL", 
                                                     'Descendants' = "des", 
                                                     'Ancestors' = "anc"),
                                         selected = "NULL"
                          ),
                          
                          selectizeInput("property", 
                                         label = "Property:",
                                         multiple = TRUE,
                                         choices = c("is_a", "BiomarkerOf", "Contains"),
                                         selected = c("is_a", "BiomarkerOf")
                          ),
                          
                          checkboxInput("plotnames", "Network labels", value = TRUE
                          ),
                          
                          downloadButton("downloadPlot", "Download plot"),
                          
                          downloadButton("downloadXGMML", HTML("Download XGMML<sup>*</sup>")),
                          
                          br(),
                          
                          helpText(HTML("<sup>*</sup>Extensible Graph Markup and Modelling Language (XGMML) is a network file format supported by Cytoscape")),
                          
                          prettySwitch("showadvanced", "Advanced settings", fill = TRUE, status = "primary"),

                          conditionalPanel("input.showadvanced",

                                           conditionalPanel("input.plotnames",

                                                            sliderInput("labelsize",
                                                                        "Label size",
                                                                        min = 1,
                                                                        max = 10,
                                                                        value = 5,
                                                                        step = 1
                                                            )
                                           ),

                                           selectInput("layout", "Layout:",
                                                       choices = c('sugiyama', 'lgl')
                                                       ),
                                           
                                           sliderInput("pointSize", "Node size", min = 1, max = 15, value = 4, step = 1),
                                           
                                           prettySwitch("curved", "Curved edges", fill = TRUE, status = "primary"),
                                           
                                           prettySwitch("legend", "Show legend", fill = TRUE, status = "primary"),

                                           conditionalPanel("input.legend",
                                           
                                                            selectInput("legendPos", "Legend position",
                                                                        choices = c('bottom', 'top', 'right', 'left')
                                                                        ),
                                           
                                                            sliderInput("legendSize", "Legend size", min = 12, max = 20, value = 16, step = 1)
                                                            )
                          )
                          ),
             
             mainPanel(
               plotOutput("ontologyplot", height = "650px", width = "1000px")
               )
             ),
    
    ## ==================================================================================== ##
    ## INTERACTIVE GRAPH TAB
    ## ==================================================================================== ## 
    
    tabPanel("Interactive Graph", 
             sidebarPanel(width = 3,
                          
                          knobInput("SizeFontD3", "Label size:", min = 10, max = 30, value = 15),
                          
                          knobInput("net_charge", "Network charge:", min = -500, max = -1, value = -200),
                          
                          helpText("This value represents the strength", 
                                   "of the node repulsion")
                          ),
             mainPanel(
               
               networkD3::simpleNetworkOutput("fobiD3graph", height = "600px")
             )
             ),
    
    ## ==================================================================================== ##
    ## TABLE TAB
    ## ==================================================================================== ## 
    
    tabPanel("Table", 
             
             checkboxInput("inverse_food_rel", HTML("Include '<i>HasBiomarker</i>' property"), value = FALSE
             ),
             
             DT::dataTableOutput("ontologytable")
             ),
    
    ## ==================================================================================== ##
    ## CONVERT ID TAB
    ## ==================================================================================== ## 
    
    tabPanel("Convert IDs", 
             
             sidebarPanel(width = 3,
                          
                          textAreaInput("convId_metabolites", 
                                        label = "Enter your metabolites here:",
                                        value = "TYNQWWGVEGFKRU-AJDPQWBVSA-N
DMASLKHVQRHNES-FKKUPVFPSA-N
PHIQHXFUZVPYII-LURJTMIESA-N
QVWAEZJXDYOKEH-UHFFFAOYSA-N
PFTAWBLQPZVEMU-DZGCQCFKSA-N
FEWJPZIEWOKRBE-JCYAYHJZSA-N
UYPYRKYUKCHHIB-UHFFFAOYSA-N
CQOVPNPJLQNMDC-ZETCQYMHSA-N
PQBAWAQIRZIWIV-UHFFFAOYSA-N
IQPNAANSBPBGFQ-UHFFFAOYSA-N",
                                        height = "220px",
                                        resize = "none"
                          ),
                          
                          helpText(HTML("Note<sup>1</sup>: can use metabolite names, FOBI, ChemSpider, KEGG, PubChemCID, InChIKey, InChICode and HMDB IDs")),
                          
                          helpText(HTML("Note<sup>2</sup>: if you're only checking one metabolite, please click 'Enter' to create an empty line below")),
                          
                          checkboxInput("exampleID", "Use example data", value = FALSE),
                          
                          selectInput("convTo",
                                      "Convert IDs to:",
                                      choices = c("metaboliteNames", "FOBI", "ChemSpider", "KEGG", "PubChemCID", "InChIKey", "InChICode", "HMDB"),
                                      selected = "FOBI"
                          )
                          
             ),
             
             mainPanel(DT::dataTableOutput("IDtable")
                       )
    ),
    
    ## ==================================================================================== ##
    ## ENRICHMENT ANALYSIS TAB
    ## ==================================================================================== ## 
    
    tabPanel("Enrichment Analysis", 
             
             sidebarPanel(width = 3,
                          
                          radioButtons("enrich_method", 
                                       label = "Enrichment analysis method:",
                                       choices = c("ORA" = 'ora',
                                                   "MSEA" = 'msea'),
                                       selected = "ora",
                                       inline = TRUE
                          ),
                          
                          conditionalPanel(condition = "input.enrich_method == 'ora'",
                                           
                                           textAreaInput("metaboliteList", 
                                                         label = "Enter your metabolite list here:",
                                                         height = "220px",
                                                         resize = "none"
                                           ),
                                           
                                           textAreaInput("metaboliteUniverse", 
                                                         label = "Enter your metabolite universe here:",
                                                         height = "220px",
                                                         resize = "none"
                                           ),
                                           
                                           checkboxInput("exampleORA", "Use example data", value = FALSE)
                          ),
                          
                          conditionalPanel(condition = "input.enrich_method == 'msea'",
                                           
                                           fileInput("msea_data", "Select an xlsx file:",
                                                     accept = c(".xlsx")
                                           ),
                                           
                                           helpText("Note: Input must be a two column ranked data frame. First column must contain",
                                                    "FOBI IDs and the second column must contain metabolite-level stats used to rank the list"),
                                           
                                           checkboxInput("exampleMSEA", "Use example data", value = FALSE)
                          ),
                          
                          selectInput("subOntology",
                                      "Select a FOBI sub-ontology",
                                      choices = c("Food" = 'food',
                                                  "Biomarker" = 'biomarker'),
                                      selected = 'food'),
                          
                          numericInput("pvalcutoff", "p-value cutoff", min = 0, max = 1, value = 0.01)
                          
             ),
             
             mainPanel(
               tabsetPanel(
                 
                 tabPanel("Table",
                          conditionalPanel(condition = "input.enrich_method == 'ora'",
                                           DT::dataTableOutput("oratable")
                          ),
                          conditionalPanel(condition = "input.enrich_method == 'msea'",
                                           DT::dataTableOutput("mseatable")
                          )
                          ),
                 tabPanel("Plot",
                          conditionalPanel(condition = "input.enrich_method == 'ora'",
                                           plotlyOutput("oraplot")
                          ),
                          conditionalPanel(condition = "input.enrich_method == 'msea'",
                                           plotlyOutput("mseaplot")
                          )
                          )
                 )
               )
             ),
    
    ## ==================================================================================== ##
    ## FOOD ANNOTATION TAB
    ## ==================================================================================== ## 
    
    tabPanel("Dietary Text Annotation", 
             
             sidebarPanel(width = 3,
                          
                          fileInput("raw_foods", "Select an xlsx file:",
                                    accept = c(".xlsx")
                          ),
                          
                          helpText("Note: Input must be a two column data frame. First column must contain IDs",
                                   "and the second column must contain food items (it can be a word or a string)"),
                          
                          # checkboxInput("exampleANNO", "Use example data", value = FALSE),
                          
                          sliderInput("similarity", "Similarity", min = 0, max = 1, value = 0.85),
                          
                          helpText("This value indicates the semantic similarity cutoff used at the last layer", 
                                   "of the text mining pipeline. 1 = exact match; 0 = very poor match.",
                                   "Values below 0.85 are not recommended")
                          
                          ),
             
             mainPanel(
               tabsetPanel(
                 
                 tabPanel("Input data",
                          DT::dataTableOutput("raw_foods_file")
                 ),
                 tabPanel("Annotated data",
                          DT::dataTableOutput("annotated_foods_file")
                 ),
                 tabPanel("Unannotated data",
                          DT::dataTableOutput("unannotated_foods_file")
                 )
               )
             )
             ),
    
    ## ==================================================================================== ##
    ## CONTACT TAB
    ## ==================================================================================== ## 
    
    tabPanel("Contact", includeMarkdown("contact.md")
    )
    
    ),
  
  ## ==================================================================================== ##
  ## FOOTER
  ## ==================================================================================== ##  
  
  footer = p(hr(), p(a(HTML("<b>Pol Castellano Escuder</b>"), href = "https://pcastellanoescuder.github.io"), ",",
                     a("Cristina Andrés Lacueva", href = "http://www.nutrimetabolomics.com/team/cristina"), "and", 
                     a("Alex Sánchez Pla", href = "https://webgrec.ub.edu/webpages/000011/cat/asanchez.ub.edu.html"), align = "center", width = 4),
             p(("Statistics and Bioinformatics Lab and Biomarkers and Nutritional & Food Metabolomics Lab"), align = "center", width = 4),
             p(("University of Barcelona"), align = "center", width = 4),
             p(("Copyright (C) 2021, code licensed under GPLv3"), align = "center", width = 4),
             p(("Code available on Github:"), a("https://github.com/nutrimetabolomics/fobitoolsGUI", 
                                                href = "https://github.com/nutrimetabolomics/fobitoolsGUI"), align = "center", width = 4),
             p(("Please cite:"), a("https://doi.org/10.1093/databa/baaa033", href = "https://doi.org/10.1093/databa/baaa033"), align = "center", width = 4)
  )#,
  
  # tags$head(includeScript("google-analytics.js"))
  
  )


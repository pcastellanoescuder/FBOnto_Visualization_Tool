source("helpers.R")
source("get_relations.R")

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
                                         choices = names, 
                                         selected = c("4,5-dicaffeoylquinic acid", "Quinic acids and derivatives", "Cyclitols and derivatives")
                          ), 
                          
                          selectizeInput("property", 
                                         label = "Property:",
                                         multiple = TRUE,
                                         choices = c("is_a", "BiomarkerOf", "Contains"),
                                         selected = c("is_a", "BiomarkerOf")
                          ),
                          
                          checkboxInput("plotnames", "Network Labels", 
                                        value = TRUE
                          ),
                          
                          downloadButton("downloadPlot","Download Plot"),
                          
                          br(),
                          br(),
                          
                          prettySwitch("showadvanced", "Advanced settings", fill = TRUE, status = "primary"),

                          conditionalPanel("input.showadvanced",

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

                                           selectInput("layout", "Layout:",
                                                       choices = c('sugiyama', 'fr', 'kk', 'lgl', 'graphopt', 'drl', 'linear')
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
    
    tabPanel("Table", DT::dataTableOutput("ontologytable")
             ),
    
    ## ==================================================================================== ##
    ## CONVERT ID TAB
    ## ==================================================================================== ## 
    
    tabPanel("Convert IDs", 
             
             sidebarPanel(width = 3,
                          
                          textAreaInput("convId_metabolites", 
                                        label = "Enter your metabolites here:",
                                        value = "PHIQHXFUZVPYII-LURJTMIESA-N
QVWAEZJXDYOKEH-UHFFFAOYSA-N
PFTAWBLQPZVEMU-DZGCQCFKSA-N
FEWJPZIEWOKRBE-JCYAYHJZSA-N
UYPYRKYUKCHHIB-UHFFFAOYSA-N
CQOVPNPJLQNMDC-ZETCQYMHSA-N
IQPNAANSBPBGFQ-UHFFFAOYSA-N
XOAAWQZATWQOTB-UHFFFAOYSA-N
OAIJSZIZWZSQBC-GYZMGTAESA-N
HCAJEUSONLESMK-UHFFFAOYSA-N",
                                        height = "220px",
                                        resize = "none"
                          ),
                          
                          helpText(HTML("Note: can use metabolite names, FOBI, ChemSpider, KEGG, PubChemCID, InChIKey, InChICode and HMDB IDs")),
                          
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
    
    tabPanel("Food Enrichment Analysis", 
             
             sidebarPanel(width = 3,
                          
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
                          
                          helpText("Note: can use metabolite names, FOBI, ChemSpider, KEGG, PubChemCID, InChIKey, InChICode and HMDB IDs"),
                          
                          selectInput("subOntology",
                                      "Select a FOBI sub-ontology",
                                      choices = c("Food" = 'food',
                                                  "Biomarker" = 'biomarker'),
                                      selected = 'food'),
                          
                          numericInput("pvalcutoff", "p-value cutoff", min = 0, max = 1, value = 0.01),
                          
                          selectInput("adj_pval",
                                      "p-value adjustment method",
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
  footer = p(hr(), p("ShinyApp created by ", a(HTML("<b>Pol Castellano Escuder</b>"), href = "https://pcastellanoescuder.github.io"), align = "center", width = 4),
             p(("Statistics and Bioinformatics Lab and Biomarkers and Nutritional & Food Metabolomics Lab from "), align = "center", width = 4),
             p(("University of Barcelona"), align = "center", width = 4),
             p(("Copyright (C) 2020, code licensed under GPLv3"), align = "center", width = 4),
             p(("Code available on Github:"), a("https://github.com/pcastellanoescuder/FOBI_Visualization_Tool", 
                                                href = "https://github.com/pcastellanoescuder/FOBI_Visualization_Tool"), align = "center", width = 4),
             p(("Please cite:"), a("https://doi.org/10.1093/databa/baaa033", href = "https://doi.org/10.1093/databa/baaa033"), align = "center", width = 4)
  )#,
  
  # tags$head(includeScript("google-analytics.js"))
  
  )


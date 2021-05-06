
function(input, output, session) {

observe({
  
  names <- fobitools::fobi %>%
    pull(name)
  
  updateSelectizeInput(session, "FOBI_name", 
                       choices = names, 
                       selected = c("4,5-dicaffeoylquinic acid",
                                    "Quinic acids and derivatives",
                                    "Cyclitols and derivatives"),
                       server = TRUE)
})
  
#### PLOT
  
fobi_network <- reactive({
  
  validate(need(!is.null(input$FOBI_name), "Select one or more entities."))
  validate(need(!is.null(input$property), "Select one or more properties."))
  
  terms_code <- fobitools::fobi %>%
    filter(name %in% input$FOBI_name) %>%
    pull(id_code)
  
  get_graph <- input$get_graph
  
  if(get_graph == "NULL") {
    get_graph <- NULL
  }

  networkplot <- fobitools::fobi_graph(
    terms = terms_code,
    get = get_graph,
    property = input$property,
    layout = input$layout,
    labels = input$plotnames,
    labelsize = input$labelsize,
    legend = input$legend,
    legendSize = input$legendSize,
    legendPos = input$legendPos,
    curved = input$curved,
    pointSize = input$pointSize)
  
  return(networkplot)
    
  })

## PLOT OUTPUT

output$ontologyplot <- renderPlot({fobi_network()})

## DOWNLOAD PLOT

output$downloadPlot <- downloadHandler(
  filename = function(){paste0(Sys.Date(), "_FOBI_network", ".png")},
  content = function(file){
    ggsave(file, plot = fobi_network(), device = "png", dpi = 200, width = 15, height = 10)
    }
  )

## TABLE GENERATION

TABLE_GEN <- reactive({
  
  terms_code <- fobitools::fobi %>%
    filter(name %in% input$FOBI_name) %>%
    pull(id_code)
  
  get_graph <- input$get_graph
  
  if(get_graph == "NULL") {
    get_graph <- NULL
  }
  
  if (!is.null(get_graph)) {
    if (get_graph == "des") {
      fobi_des <- fobitools::fobi_terms %>%
        ontologyIndex::get_descendants(roots = terms_code, exclude_roots = TRUE)
      
      fobiGraph <- fobi %>%
        filter(id_code %in% fobi_des) %>%
        filter(!is.na(is_a_code))
    }
    else {
      fobi_anc <- fobitools::fobi_terms %>%
        ontologyIndex::get_ancestors(terms = terms_code)
      
      fobiGraph <- fobi %>%
        filter(id_code %in% fobi_anc) %>%
        filter(!is.na(is_a_code))
    }
  }
  else {
    fobiGraph <- fobitools::fobi %>%
      filter(id_code %in% terms_code) %>%
      filter(!is.na(is_a_code))
  }
  
  contains <- fobiGraph %>%
    mutate(Property = ifelse(!is.na(Contains), "Contains", NA)) %>%
    filter(!is.na(Property)) %>%
    select(name, Contains, Property) %>%
    rename(from = 1, to = 2, Property = 3)
  
  biomarkerof <- fobiGraph %>%
    mutate(Property = ifelse(!is.na(BiomarkerOf), "BiomarkerOf", NA)) %>%
    filter(!is.na(Property)) %>%
    select(name, BiomarkerOf, Property) %>%
    rename(from = 1, to = 2, Property = 3)
  
  is_a <- fobiGraph %>%
    select(name, is_a_name) %>%
    mutate(Property = "is_a") %>%
    filter(!duplicated(name)) %>%
    rename(from = 1, to = 2, Property = 3)
  
  fobi_table <- rbind(is_a, biomarkerof, contains) %>%
    filter(Property %in% input$property)
  
  return(fobi_table)
  
})

## DOWNLOAD XGMML FORMAT

output$downloadXGMML <- downloadHandler(
  filename = function(){paste0(Sys.Date(), "_FOBI_network.xgmml")},
  content = function(file){

    fobi_links <- TABLE_GEN()
    fobi_igr <- igraph::graph_from_data_frame(fobi_links)
    BioNet::saveNetwork(fobi_igr, name = "FOBI_XGMML_network", file = file, type = "XGMML")
    # showNotification(ui = "Network saved as XGMML file in the app directory", duration = 5, closeButton = TRUE, type = "message")
    
  }
)

## INTERACTIVE PLOT

output$fobiD3graph <- networkD3::renderSimpleNetwork({
  
  validate(need(!is.null(input$FOBI_name), "Select one or more entities."))
  validate(need(!is.null(input$property), "Select one or more properties."))

  fobi_links <- TABLE_GEN()

  validate(need(nrow(fobi_links) > 0, "There aren't connections between selected entities and properties."))

  simpleNetwork(fobi_links, fontSize = input$SizeFontD3, zoom = TRUE, charge = input$net_charge, height = "800px")
  
})

#### TABLE OUTPUT

output$ontologytable <- DT::renderDataTable({
  
  validate(need(!is.null(input$FOBI_name), "Select one or more entities."))
  validate(need(!is.null(input$property), "Select one or more properties."))
  
  sub_table <- fobitools::fobi %>%
    filter(name %in% TABLE_GEN()$from)
  
  if(input$inverse_food_rel) {
    
    inverse_rel <- fobitools::fobi %>%
      filter(id_BiomarkerOf %in% sub_table$id_code)
    
    sub_table <- bind_rows(sub_table, inverse_rel)
  
  }
    
  sub_table <- sub_table %>%
    select(-ChemSpider, -KEGG, -PubChemCID, -InChIKey, -InChICode, -alias, -HMDB) %>%
    dplyr::relocate(FOBI, .before = name) %>%
    rename("FOBI ID" = FOBI)

  if (!("Contains" %in% input$property)){		
    sub_table <- sub_table %>%		
      select(-id_Contains, -Contains)		
  }
  
  if (!("BiomarkerOf" %in% input$property)){		
    sub_table <- sub_table %>%
      select(-id_BiomarkerOf, -BiomarkerOf)
  }
  
  if (!("is_a" %in% input$property)){		
    sub_table <- sub_table %>%
      select(-is_a_code, -is_a_name)
  }
  
  sub_table <- sub_table %>%
    filter(!duplicated(.))
  
  validate(need(nrow(sub_table) > 0, "No terms with these characteristics."))
  
  DT::datatable(sub_table,
                filter = 'none',extensions = 'Buttons',
                escape=FALSE,  rownames=FALSE, class = 'cell-border stripe',
                options = list(
                  dom = 'Bfrtip',
                  buttons =
                    list("copy", "print", list(
                      extend="collection",
                      buttons=list(list(extend = "csv",
                                        filename = paste0(Sys.Date(), "_FOBI_table")),
                                   list(extend = "excel",
                                        filename = paste0(Sys.Date(), "_FOBI_table")),
                                   list(extend = "pdf",
                                        filename = paste0(Sys.Date(), "_FOBI_table"))),
                      text = "Dowload")),
                  order=list(list(2, "desc")),
                  pageLength = nrow(sub_table)))
  })

#### CONVERT ID

observe({
  
  if (input$exampleID){
    updateTextAreaInput(session, "convId_metabolites", value = paste(fobitools::idmap$InChIKey[1:10], collapse = "\n"))
  } 
  else {
    updateTextAreaInput(session, "convId_metabolites", value = "")
  }
  
})

##

output$IDtable <- DT::renderDataTable({
  
  validate(need(input$convId_metabolites != "", "Select one or more entities."))
  
  res <- readr::read_delim(input$convId_metabolites, delim = "\n", col_names = FALSE) %>%
    pull(1) %>%
    fobitools::id_convert(to = input$convTo)
    
  DT::datatable(res,
                filter = 'none',extensions = 'Buttons',
                escape=FALSE,  rownames=TRUE, class = 'cell-border stripe',
                options = list(
                  dom = 'Bfrtip',
                  buttons =
                    list("copy", "print", list(
                      extend="collection",
                      buttons=list(list(extend = "csv",
                                        filename = paste0(Sys.Date(), "_FOBI_ConvertID")),
                                   list(extend = "excel",
                                        filename = paste0(Sys.Date(), "_FOBI_ConvertID")),
                                   list(extend = "pdf",
                                        filename = paste0(Sys.Date(), "_FOBI_ConvertID"))),
                      text = "Dowload")),
                  order = list(list(2, "desc")),
                  pageLength = nrow(res)))
  })

#### ORA

observe({
  
  if (input$exampleORA){
    updateTextAreaInput(session, "metaboliteList", value = paste(fobitools::idmap$FOBI[1:70], collapse = "\n"))
    updateTextAreaInput(session, "metaboliteUniverse", value = paste(fobitools::idmap$FOBI[1:200], collapse = "\n"))
  } 
  else {
    updateTextAreaInput(session, "metaboliteList", value = "")
    updateTextAreaInput(session, "metaboliteUniverse", value = "")
  }
  
})

##

ora_enrichment <- reactive({
  
  validate(need(input$metaboliteList != "", "Select one or more entities for metaboliteList."))
  validate(need(input$metaboliteUniverse != "", "Select one or more entities for metaboliteUniverse."))
  
  metaboliteList <- readr::read_delim(input$metaboliteList, delim = "\n", col_names = FALSE) %>% 
    pull(1) %>%
    fobitools::id_convert(to = "FOBI") %>%
    pull(FOBI) 
  
  metaboliteUniverse <- readr::read_delim(input$metaboliteUniverse, delim = "\n", col_names = FALSE) %>% 
    pull(1) %>%
    fobitools::id_convert(to = "FOBI") %>%
    pull(FOBI)
  
  res <- fobitools::ora(metaboliteList,
                        metaboliteUniverse,
                        subOntology = input$subOntology,
                        pvalCutoff = input$pvalcutoff) %>%
    dplyr::arrange(-dplyr::desc(padj))
  
  return(res)
  
})

##

output$oratable <- DT::renderDataTable({
  
  res <- ora_enrichment()
  
  DT::datatable(res,
                filter = 'none',extensions = 'Buttons',
                escape = FALSE,  rownames = FALSE, class = 'cell-border stripe',
                options = list(
                  dom = 'Bfrtip',
                  buttons =
                    list("copy", "print", list(
                      extend="collection",
                      buttons=list(list(extend = "csv",
                                        filename = paste0(Sys.Date(), "_FOBI_Enrichment_Analysis_ORA")),
                                   list(extend = "excel",
                                        filename = paste0(Sys.Date(), "_FOBI_Enrichment_Analysis_ORA")),
                                   list(extend = "pdf",
                                        filename = paste0(Sys.Date(), "_FOBI_Enrichment_Analysis_ORA"))),
                      text = "Dowload")),
                  order=list(list(2, "desc")),
                  pageLength = nrow(res)))
  
  })

##

output$oraplot <- renderPlotly({
  
  res <- ora_enrichment()
  
  ora_plot <- ggplot(res, aes(x = -log10(pval), y = reorder(className, -log10(pval)), fill = -log10(pval))) +
    xlab("-log10(P-value)") +
    ylab("") +
    geom_col() +
    theme_bw() +
    theme(legend.position = "none",
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 15))
  
  plotly::ggplotly(ora_plot) %>% 
    plotly::config(
      toImageButtonOptions = list(format = "png"),
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("sendDataToCloud", "zoom2d", "select2d", "lasso2d", 
                                 "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian")
      )
})

#### MSEA

mseaInput <- reactive({
  
  if(input$exampleMSEA) {
    
    data_msea <- readxl::read_xlsx("data/ranked_list_ST000291.xlsx") %>%
      dplyr::rename(FOBI = 1, stats = 2)
    
    data_msea_vec <- data_msea$stats
    names(data_msea_vec) <- data_msea$FOBI
    
    return(data_msea_vec)
  }
  
  else {
    
    infile <- input$msea_data
    
    if (is.null(infile)){
      return(NULL)
    }
    
    else {
      
      data_msea <- readxl::read_xlsx(infile$datapath)
      
      validate(need(ncol(data_msea) == 2, "Input must be a two column ranked data frame."))
      
      data_msea <- data_msea %>%
        dplyr::rename(FOBI = 1, stats = 2)
      
      validate(need(all(data_msea$FOBI %in% fobitools::fobi$FOBI), "Identifiers not found in FOBI."))
      
      data_msea_vec <- data_msea$stats
      names(data_msea_vec) <- data_msea$FOBI
      
      return(data_msea_vec)
    }
  }
  })

##

msea_enrichment <- reactive({
  
  data_msea_vec <- mseaInput()
  
  validate(need(!is.null(data_msea_vec), "Upload a ranked list."))
  
  res <- fobitools::msea(data_msea_vec,
                         subOntology = input$subOntology,
                         pvalCutoff = input$pvalcutoff) %>%
    dplyr::arrange(-dplyr::desc(padj))
  
  return(res)
  
})

##

output$mseatable <- DT::renderDataTable({
  
  res <- msea_enrichment()
  
  DT::datatable(res,
                filter = 'none',extensions = 'Buttons',
                escape = FALSE,  rownames = FALSE, class = 'cell-border stripe',
                options = list(
                  dom = 'Bfrtip',
                  buttons =
                    list("copy", "print", list(
                      extend="collection",
                      buttons=list(list(extend = "csv",
                                        filename = paste0(Sys.Date(), "_FOBI_Enrichment_Analysis_MSEA")),
                                   list(extend = "excel",
                                        filename = paste0(Sys.Date(), "_FOBI_Enrichment_Analysis_MSEA")),
                                   list(extend = "pdf",
                                        filename = paste0(Sys.Date(), "_FOBI_Enrichment_Analysis_MSEA"))),
                      text = "Dowload")),
                  order=list(list(2, "desc")),
                  pageLength = nrow(res)))
  
})

##

output$mseaplot <- renderPlotly({
  
  res <- msea_enrichment() %>%
    mutate(overlap = length(leadingEdge))
  
  msea_plot <- ggplot(res, aes(x = -log10(pval), y = NES, color = NES, size = classSize, label = className)) +
    xlab("-log10(P-value)") +
    ylab("NES (Normalized Enrichment Score)") +
    geom_point() +
    theme_bw() +
    theme(legend.position = "none",
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 15))
  
  plotly::ggplotly(msea_plot) %>% 
    plotly::config(
      toImageButtonOptions = list(format = "png"),
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("sendDataToCloud", "zoom2d", "select2d", "lasso2d", 
                                 "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian")
    )
})

#### FOOD ANNOTATION

output$raw_foods_file <- DT::renderDataTable({
  
  inFile <- input$raw_foods
  
  if(is.null(inFile))
    return(NULL)
  
  file.rename(inFile$datapath, paste(inFile$datapath, ".xlsx", sep = ""))
  raw_fods <- read_excel(paste(inFile$datapath, ".xlsx", sep = ""), 1)

  DT::datatable(raw_fods,
                filter = 'none',extensions = 'Buttons',
                escape = FALSE,  rownames = FALSE, class = 'cell-border stripe',
                options = list(pageLength = 10))
  
})

##

food_annotation <- reactive({
  
  inFile <- input$raw_foods
  
  if(is.null(inFile))
    return(NULL)
  
  file.rename(inFile$datapath, paste(inFile$datapath, ".xlsx", sep = ""))
  annotated_foods <- read_excel(paste(inFile$datapath, ".xlsx", sep = ""), 1) %>%
    fobitools::annotate_foods(similarity = input$similarity)
  
  unannotated_foods <- annotated_foods$unannotated
  annotated_foods <- annotated_foods$annotated
  
  return(list(annotated_foods = annotated_foods, unannotated_foods = unannotated_foods))
  
})

##

output$annotated_foods_file <- DT::renderDataTable({
  
  annotated_fods <- food_annotation()$annotated_foods
  
  DT::datatable(annotated_fods,
                filter = 'none',extensions = 'Buttons',
                escape = FALSE,  rownames = FALSE, class = 'cell-border stripe',
                options = list(
                  dom = 'Bfrtip',
                  buttons =
                    list("copy", "print", list(
                      extend="collection",
                      buttons=list(list(extend = "csv",
                                        filename = paste0(Sys.Date(), "_FOBI_annotated_foods")),
                                   list(extend = "excel",
                                        filename = paste0(Sys.Date(), "_FOBI_annotated_foods")),
                                   list(extend = "pdf",
                                        filename = paste0(Sys.Date(), "_FOBI_annotated_foods"))),
                      text = "Dowload")),
                  order=list(list(2, "desc")),
                  pageLength = nrow(annotated_fods)))
  })

##

output$unannotated_foods_file <- DT::renderDataTable({
  
  unannotated_foods <- food_annotation()$unannotated_foods
  
  DT::datatable(unannotated_foods,
                filter = 'none',extensions = 'Buttons',
                escape = FALSE,  rownames = TRUE, class = 'cell-border stripe',
                options = list(
                  dom = 'Bfrtip',
                  buttons =
                    list("copy", "print", list(
                      extend="collection",
                      buttons=list(list(extend = "csv",
                                        filename = paste0(Sys.Date(), "_FOBI_unannotated_foods")),
                                   list(extend = "excel",
                                        filename = paste0(Sys.Date(), "_FOBI_unannotated_foods")),
                                   list(extend = "pdf",
                                        filename = paste0(Sys.Date(), "_FOBI_unannotated_foods"))),
                      text = "Dowload")),
                  order=list(list(2, "desc")),
                  pageLength = nrow(unannotated_foods)))
})

}

 
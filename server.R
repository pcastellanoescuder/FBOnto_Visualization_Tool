
function(input, output, session) {
  
#### PLOT
  
fobi_network <- reactive({
  
  validate(need(!is.null(input$FOBI_name), "Select one or more entities."))
  validate(need(!is.null(input$property), "Select one or more properties."))
  
  entity <- input$FOBI_name
  
  graph_table <- graph_table %>% 
    filter(from %in% entity | to %in% entity) %>% 
    filter(Property %in% input$property)
  
  validate(need(nrow(graph_table) > 1, "There aren't connections between selected entities and properties."))
  
  graph <- as_tbl_graph(graph_table) %>%
    mutate(subOntology = ifelse(name %in% foods$name, "Food", "Biomarker"),
           subOntology = ifelse(name == "Foods", "Food", subOntology))
  
  cols_nodes <- c("Biomarker" = "#440154FF", 
                  "Food" = "#FDE725FF") # viridis palette
  
  cols_edges <- c("is_a" = "#287C8EFF", 
                  "BiomarkerOf" = "#75D054FF", 
                  "Contains" = "#E3E418FF") # viridis palette
  
  networkplot <- ggraph(graph, layout = input$layout) +
    {if(!input$curved) geom_edge_link(aes(color = Property), end_cap = circle(2.5, "mm"), 
                                arrow = arrow(length = unit(2.5, "mm"), type = "closed"),
                                show.legend = input$legend)} +
    {if(input$curved) geom_edge_arc(aes(color = Property), end_cap = circle(2.5, "mm"),
                              arrow = arrow(length = unit(2.5, "mm"), type = "closed"),
                              strength = 0.1, show.legend = input$legend)} +
    geom_node_point(aes(color = subOntology, shape = subOntology), size = input$pointSize, show.legend = FALSE) +
    {if(input$plotnames & input$labeltext == "label") geom_node_label(aes(label = name), color = 'black', size = input$labelsize, repel = TRUE, show.legend = FALSE)} +
    {if(input$plotnames & input$labeltext != "label") geom_node_text(aes(label = name), color = 'black', size = input$labelsize, repel = TRUE, show.legend = FALSE)} +
    scale_color_manual(values = cols_nodes, guide = "none") +
    scale_shape_manual(values = c("Biomarker" = 16, "Food" = 15), guide = "none") +
    scale_edge_color_manual(values = cols_edges) +
    theme_graph(foreground = "white", fg_text_colour = "white") + 
    theme(legend.title = element_blank(),
          legend.text = element_text(size = input$legendSize),
          legend.position = input$legendPos)
  
  return(networkplot)
    
  })

output$ontologyplot <- renderPlot({fobi_network()})

output$downloadPlot <- downloadHandler(
  filename = function(){paste0(Sys.Date(), "_FOBI_network", ".png")},
  content = function(file){
    ggsave(file, plot = fobi_network(), device = "png", dpi = 200, width = 15, height = 10)
    }
  )

#### INTERACTIVE PLOT

output$fobiD3graph <- networkD3::renderSimpleNetwork({
  
  entity <- input$FOBI_name
  
  fobi_links <- graph_table %>% 
    filter(from %in% entity | to %in% entity) %>% 
    filter(Property %in% input$property)
  
  validate(need(nrow(fobi_links) > 1, "There aren't connections between selected entities and properties."))
  
  simpleNetwork(fobi_links, fontSize = input$SizeFontD3, zoom = TRUE, charge = input$net_charge, height = "800px")
  
})

#### TABLE

output$ontologytable <- DT::renderDataTable({
  
  entity <- input$FOBI_name
  
  sub_table <- fobi %>% 
    filter(name %in% entity) %>% # | is_a_name %in% entity | BiomarkerOf %in% entity | Contains %in% entity
    select(-ChemSpider, -PubChemCID, -KEGG, -HMDB, -InChIKey, -InChICode, -alias) %>%
    dplyr::relocate(FOBI, .before = name) %>%
    rename("ID" = id_code,
           "FOBI ID" = FOBI,
           "Name" = name,
           "SuperClass ID" = is_a_code,
           "SuperClass" = is_a_name,
           "BiomarkerOf ID" = id_BiomarkerOf,
           "Contains ID" = id_Contains)
  
  if(!("Contains" %in% input$property)){
    sub_table <- sub_table %>%
      select(-`Contains ID`, -Contains)
  }
  if(!("BiomarkerOf" %in% input$property)){
    sub_table <- sub_table %>%
      select(-`BiomarkerOf ID`, -BiomarkerOf)
  }
  if(!("is_a" %in% input$property)){
    sub_table <- sub_table %>%
      select(-`SuperClass ID`, -SuperClass)
  }
  
  sub_table <- sub_table %>%
    filter(!duplicated(.))
  
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
  
  res <- read_delim(input$convId_metabolites, delim = "\n", col_names = FALSE) %>%
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

food_enrichment <- reactive({
  
  validate(need(input$metaboliteList != "", "Select one or more entities for metaboliteList."))
  validate(need(input$metaboliteUniverse != "", "Select one or more entities for metaboliteUniverse."))
  
  metaboliteList <- read_delim(input$metaboliteList, delim = "\n", col_names = FALSE) %>% 
    pull(1) %>%
    fobitools::id_convert(to = "FOBI") %>%
    pull(FOBI) 
  
  metaboliteUniverse <- read_delim(input$metaboliteUniverse, delim = "\n", col_names = FALSE) %>% 
    pull(1) %>%
    fobitools::id_convert(to = "FOBI") %>%
    pull(FOBI)
  
  res <- fobitools::ora(metaboliteList,
                        metaboliteUniverse,
                        subOntology = input$subOntology,
                        pvalCutoff = input$pvalcutoff,
                        adjust = input$adj_pval) %>%
    arrange(-desc(pvalueAdj))
  
  return(res)
  
})

##

output$oratable <- DT::renderDataTable({
  
  res <- food_enrichment()
  
  DT::datatable(res,
                filter = 'none',extensions = 'Buttons',
                escape = FALSE,  rownames = FALSE, class = 'cell-border stripe',
                options = list(
                  dom = 'Bfrtip',
                  buttons =
                    list("copy", "print", list(
                      extend="collection",
                      buttons=list(list(extend = "csv",
                                        filename = paste0(Sys.Date(), "_FOBI_Enrichment_Analysis")),
                                   list(extend = "excel",
                                        filename = paste0(Sys.Date(), "_FOBI_Enrichment_Analysis")),
                                   list(extend = "pdf",
                                        filename = paste0(Sys.Date(), "_FOBI_Enrichment_Analysis"))),
                      text = "Dowload")),
                  order=list(list(2, "desc")),
                  pageLength = nrow(res)))
  
  })

##

output$oraplot <- renderPlot({
  
  res <- food_enrichment()
  
  ggplot(res, aes(x = -log10(pvalue), y = reorder(className, -log10(pvalue)), fill = -log10(pvalue), label = classId)) +
    xlab("-log10(P-value)") +
    ylab("") +
    geom_col() +
    theme_bw() +
    theme(legend.position = "none",
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 15))
  
})

}

 
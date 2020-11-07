
function(input, output, session) {
 
#### PLOT
  
output$ontologyplot <- renderPlot({
  
  entity <- input$FOBI_name
  
  graph_table <- graph_table %>% 
    filter(from %in% entity | to %in% entity) %>% 
    filter(Property %in% input$property)
  
  graph <- as_tbl_graph(graph_table) %>%
      mutate(Interactions = centrality_degree(mode = 'in'))
  
  if(input$plotnames){
    
    if (input$labeltext == "label"){
      
      ggraph(graph, layout = input$layout) +
        geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE) +
        geom_edge_link(aes(colour = Property), alpha = input$a_edge) +
        geom_node_point(aes(size = Interactions), colour = 'snow4', alpha = input$a_node) +
        theme_graph(foreground = 'white', fg_text_colour = 'white') + 
        geom_node_label(aes(label = name), color = 'black', size = input$labelsize, repel = TRUE) +
        theme(legend.title = element_text(size = 18),
              legend.text = element_text(size = 16),
              legend.position = "bottom")
      
    } else {
      
      ggraph(graph, layout = input$layout) +
        geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE) +
        geom_edge_link(aes(colour = Property), alpha = input$a_edge) +
        geom_node_point(aes(size = Interactions), colour = 'snow4', alpha = input$a_node) +
        theme_graph(foreground = 'white', fg_text_colour = 'white') + 
        geom_node_text(aes(label = name), color = 'black', size = input$labelsize, repel = TRUE) +
        theme(legend.title = element_text(size = 18),
              legend.text = element_text(size = 16),
              legend.position = "bottom")
      
    }
    
  } else {
    
    ggraph(graph, layout = input$layout) +
      geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE) +
      geom_edge_link(aes(colour = Property), alpha = input$a_edge) +
      geom_node_point(aes(size = Interactions), colour = 'snow4', alpha = input$a_node) +
      theme_graph(foreground = 'white', fg_text_colour = 'white') +
      theme(legend.title = element_text(size = 18),
            legend.text = element_text(size = 16),
            legend.position = "bottom")
    
  }
  
  })

#### TABLE

output$ontologytable <- DT::renderDataTable({
  
  entity <- input$FOBI_name
  
  graph_table <- graph_table %>% 
    filter(from %in% entity | to %in% entity) %>% 
    filter(Property %in% input$property) %>%
    rename(Source = 1, Target = 2)
  
  DT::datatable(graph_table, 
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
                  pageLength = nrow(graph_table)))
  })

#### CONVERT ID

output$IDtable <- DT::renderDataTable({
  
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
                  order=list(list(2, "desc")),
                  pageLength = nrow(res)))
})

#### ORA

output$oratable <- DT::renderDataTable({
  
  res <- read_delim(input$ora_metabolites, delim = "\n", col_names = FALSE) %>% 
    pull(1) %>%
    fobitools::id_convert(to = "FOBI") %>%
    pull(FOBI) %>%
    fobitools::ora(method = input$correction_method_ora) %>%
    mutate(pvalue = round(pvalue, 4),
           pvalueAdj = round(pvalueAdj, 4)) %>%
    arrange(!desc(pvalueAdj))
  
  DT::datatable(res,
                filter = 'none',extensions = 'Buttons',
                escape=FALSE,  rownames=FALSE, class = 'cell-border stripe',
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

output$oraplot <- renderPlot({
  
  res <- read_delim(input$ora_metabolites, delim = "\n", col_names = FALSE) %>%
    pull(1) %>%
    fobitools::id_convert(to = "FOBI") %>%
    pull(FOBI) %>%
    fobitools::ora(method = input$correction_method_ora) %>%
    mutate(pvalue = round(pvalue, 4),
           pvalueAdj = round(pvalueAdj, 4)) %>%
    arrange(!desc(pvalueAdj))
  
  ggplot(res, aes(x = -log10(pvalue), y = reorder(description, -log10(pvalue)), fill = -log10(pvalue), label = classId)) +
    xlab("-log10(P-value)") +
    ylab("") +
    geom_col() +
    theme_bw() +
    theme(legend.position = "none",
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 15))
  
})

}

 
source("helpers.R")
source("ontology.R")

function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  
  output$ontologyplot <- renderPlot({

      item <- input$FBOnto_name
      code <- as.character(names[names$value == item ,1])

      ##
      
      ancestors <- get_term_property(ontology, code, property_name = "ancestors")
      descendants <- get_descendants(ontology, code)
      
      if(input$level == "all"){

      item_group <- c(ancestors, descendants)
      
      } 
      
      else if(input$level == "ancestors"){

        item_group <- ancestors
      }
      
      else if(input$level == "descendants"){
        
        item_group <- descendants
      }

      ##

      graph_table <- bind_rows(biomarker_of, is_a, ingredient_of) %>%
        setNames(.,c("from", "to", "Property"))

      graph_table <- graph_table[graph_table$from %in% item_group | graph_table$to %in% item_group ,]

      ##

      BIOMARKERScc <- BIOMARKERSc[!BIOMARKERSc %in% ancestors]
      FOODScc <- FOODSc[!FOODSc %in% ancestors]

      if (code %in% BIOMARKERS){
        graph_table <- graph_table[!(graph_table$from %in% BIOMARKERScc | graph_table$to %in% BIOMARKERScc) ,]
      }

      if (code %in% FOODS){
        graph_table <- graph_table[!(graph_table$from %in% FOODScc | graph_table$to %in% FOODScc) ,]
      }

      ##

      for (i in 1:nrow(graph_table)){
        new_name_from <- names[names$ID == graph_table$from[i] , 2]
        new_name_to <- names[names$ID == graph_table$to[i] , 2]

        graph_table$from[i] <- new_name_from
        graph_table$to[i] <- new_name_to
      }

      ##
      
      graph_table <- graph_table[graph_table$Property %in% input$property ,]

      ##

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
          
        } else{
          
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
    
    item <- input$FBOnto_name
    code <- as.character(names[names$value == item , 1])
    
    ##
    
    ancestors <- get_term_property(ontology, code, property_name = "ancestors")
    descendants <- get_descendants(ontology, code)
    
    if(input$level == "all"){
      
      item_group <- c(ancestors, descendants)
      
    } 
    
    else if(input$level == "ancestors"){
      
      item_group <- ancestors
    }
    
    else if(input$level == "descendants"){
      
      item_group <- descendants
    }
    
    ##
    
    graph_table <- bind_rows(biomarker_of, is_a, ingredient_of) %>%
      setNames(.,c("from", "to", "Property"))
    
    graph_table <- graph_table[graph_table$from %in% item_group | graph_table$to %in% item_group ,]
    
    ##
    
    BIOMARKERScc <- BIOMARKERSc[!BIOMARKERSc %in% ancestors]
    FOODScc <- FOODSc[!FOODSc %in% ancestors]
    
    if (code %in% BIOMARKERS){
      graph_table <- graph_table[!(graph_table$from %in% BIOMARKERScc | graph_table$to %in% BIOMARKERScc) ,]
    }
    
    if (code %in% FOODS){
      graph_table <- graph_table[!(graph_table$from %in% FOODScc | graph_table$to %in% FOODScc) ,]
    }
    
    ##
    
    for (i in 1:nrow(graph_table)){
      new_name_from <- names[names$ID == graph_table$from[i] , 2]
      new_name_to <- names[names$ID == graph_table$to[i] , 2]
      
      graph_table$from[i] <- new_name_from
      graph_table$to[i] <- new_name_to
    }
    
    ##
    
    graph_table <- graph_table[graph_table$Property %in% input$property ,]
    
    colnames(graph_table)[1:2] <- c("From", "To")
    
    DT::datatable(graph_table, 
              filter = 'none',extensions = 'Buttons',
              escape=FALSE,  rownames=FALSE, class = 'cell-border stripe',
              options = list(
                dom = 'Bfrtip',
                buttons = 
                  list("copy", "print", list(
                    extend="collection",
                    buttons=list(list(extend="csv",
                                      filename=paste0("FBOnto_network_table_",input$FBOnto_name)),
                                 list(extend="excel",
                                      filename=paste0("FBOnto_network_table_",input$FBOnto_name)),
                                 list(extend="pdf",
                                      filename=paste0("FBOnto_network_table_",input$FBOnto_name))),
                    text="Dowload")),
                order=list(list(2, "desc")),
                pageLength = nrow(graph_table)))
    
  })
 }
  

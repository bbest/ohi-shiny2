shinyServer(function(input, output) {
  
  ## get_selected() ----
  get_selected = reactive({
    req(input$sel_type)
    
    switch(input$sel_type,
           
      # case: output     
      output = {
        req(input$sel_output_goal)
        req(input$sel_output_goal_dimension)
        
        list(
          data = scores %>%
            filter(
              goal      == input$sel_output_goal,
              dimension == input$sel_output_goal_dimension) %>%
            select(
              rgn_id = region_id, 
              value  = score) %>%
            select(rgn_id, value),
          label = sprintf('%s - %s', input$sel_output_goal, input$sel_output_goal_dimension),
          description = dims %>%
            filter(dimension == input$sel_output_goal_dimension)  %>%
            markdownToHTML(text = .$description, fragment.only=T)) },
      
      # case: input     
      input = {
        req(input$sel_input_target_layer)
        
        fld_category = filter(layers, layer==input$sel_input_target_layer) %>% .$fld_category
        fld_year     = filter(layers, layer==input$sel_input_target_layer) %>% .$fld_year
        
        # get data
        data = d_lyrs %>%
            filter(layer == input$sel_input_target_layer) %>%
            mutate(
              rgn_id = fld_id_num, 
              value  = fld_val_num)

        # if layer has category, filter
        if (!is.na(fld_category)){
          req(input$sel_input_target_layer_category)
          data = data %>%
            filter(fld_category == input$sel_input_target_layer_category)
        }
        
        # if layer has category year, filter
        if (!is.na(fld_year)){
          req(input$sel_input_target_layer_category_year)
          data = data %>%
            filter(fld_year == input$sel_input_target_layer_category_year)
        }
        
        
        # return list
        list(
          data = data %>%
            select(rgn_id, value),
          label = input$sel_input_target_layer,
          description = layers %>%
            filter(layer == input$sel_input_target_layer) %>%
            markdownToHTML(text = .$description, fragment.only=T))
      })
    })
  
  ## output$ui_sel_output ---- 
  output$ui_sel_output <- renderUI({
    req(input$sel_output_goal)
    
    selectInput(
      'sel_output_goal_dimension', 
      label    = '3. Choose dimension:', 
      choices  = scores %>%
        filter(goal == input$sel_output_goal) %>%
        distinct(dimension) %>%
        .$dimension, 
      selected = 'score')
    
    })

  # output$ui_sel_input ----
  output$ui_sel_input <- renderUI({
    req(input$sel_input_target)
    
    target_layers = with(
      layers_by_target %>%
        filter(target == input$sel_input_target) %>%
        mutate(label = sprintf('%s: %s', layer, name)),
      setNames(layer, label))
    
    ui = tagList(selectInput(
      'sel_input_target_layer',
      label    = '3. Choose layer:',
      choices  = target_layers,
      selected = ifelse(
        is.null(input$sel_input_target_layer), 
        target_layers[1],
        input$sel_input_target_layer)))
    
    if (!is.null(input$sel_input_target_layer)){
      
      fld_category = filter(layers, layer==input$sel_input_target_layer) %>% .$fld_category
      
      if (!is.na(fld_category)){
        
        categories = d_lyrs %>%
          filter(layer == input$sel_input_target_layer) %>%
          distinct(fld_category) %>%
          .$fld_category
        
        ui = tagList(ui, selectInput(
          'sel_input_target_layer_category',
          label    = sprintf('4. Choose %s:', fld_category),
          choices  = categories,
          selected = ifelse(
            is.null(input$sel_input_target_layer_category), 
            categories[1],
            input$sel_input_target_layer_category)))
        
        fld_year = filter(layers, layer == input$sel_input_target_layer) %>% .$fld_year
        
        if (!is.na(fld_year) & !is.null(input$sel_input_target_layer_category)){
          years = d_lyrs %>%
            filter(
              layer        == input$sel_input_target_layer,
              fld_category == input$sel_input_target_layer_category) %>%
            distinct(fld_year) %>%
            .$fld_year
  
          ui = tagList(ui, selectInput(
          'sel_input_target_layer_category_year',
          label    = '5. Choose year:',
          choices  = years,
          selected = ifelse(
            is.null(input$sel_input_target_layer_category_year), 
            years[1],
            input$sel_input_target_layer_category_year)))  }}}
          
   return(ui) })
  
  # output$var_description ----
  # update description of input layer or output goal dimension
  output$var_description = renderText({ 
    get_selected()$description })
  
  # output$map1 ----
  
  # input$map_shape_mouseover gets updated a lot, even if the id doesn't change.
  # We don't want to update the polygons and stateInfo except when the id
  # changes, so use v$hi_id to insulate the downstream reactives (as 
  # writing to v$hi_id doesn't trigger reactivity unless the new value 
  # is different than the previous value).
  v <- reactiveValues(hi_id = 0, msg = '') # set default to GLOBAL = 0
  area_global <- round(sum(rgns@data$area_km2))
  
  output$map1 <- renderLeaflet({
    
    # get data from selection (get_selected() is the reactive function defined above)
    selected = get_selected()
    
    # debug
    isolate(v$msg <- paste(now_s(), '-- renderLeaflet()', br(), v$msg))

    # set color palette
    pal = colorNumeric(
      palette = 'RdYlBu',
      domain = selected$data$value)
    
    # plot map with rgns$id to lookup data$value
    pal = colorNumeric(
      palette = 'RdYlBu',
      domain = selected$data$value)
    id2col = function(ids, d=selected$data, col_id='rgn_id', col_val='value'){
      pal(d[match(ids, d[[col_id]]), col_val]) 
    }
    leaflet() %>%
      addPolygons(
        data = rgns,
        color = ~id2col(rgn_id))
    leaflet() %>%
      addProviderTiles('Stamen.TonerLite', options=tileOptions(noWrap=TRUE)) %>%
      setView(0,0,2) %>%
      addPolygons(
        data = rgns, group = 'regions', layerId = rgns@data$rgn_id,
        stroke = TRUE, fillOpacity = 0.5, smoothFactor = 0.5,
        color = ~id2col(rgn_id)) %>%
      addLegend(
        "bottomright", pal = pal, opacity = 0.5,
        values = selected$data$value, title = selected$label)
        
  })
  
  # aster hover ----
  
  # handle mouseover/mouseout per leaflet::examples/shiny.R style
  observeEvent(input$map1_shape_mouseover, {
    v$hi_id <- as.integer(sub('_hi', '', as.character(input$map1_shape_mouseover$id)))
    isolate(v$msg <- paste(now_s(), '-- map1_shape_mouseover | hi_id=', v$hi_id, br(), v$msg))
  })
  observeEvent(input$map1_shape_mouseout, {
    v$hi_id <- 0
    isolate(v$msg <- paste(now_s(), '-- map1_shape_mouseout | hi_id=', v$hi_id, br(), v$msg))
  })
  
  # add shape on hover
  observeEvent(v$hi_id,{
    
    #req(input$map1) # otherwise [JS console error `Couldn't find map with id map`](https://github.com/rstudio/leaflet/issues/242) 
    
    # clean previously highlighted shape
    leafletProxy("map1") %>% 
      clearGroup("highlight")
    
    # return if GLOBAL
    if (v$hi_id == 0) return()
    
    # add shape
    leafletProxy("map1") %>% 
      addPolygons(
        data = rgns[match(v$hi_id, rgns$rgn_id),], 
        group = "highlight", layerId = sprintf('%d_hi', v$hi_id),
        stroke = T, color = "gray", weight = 5, fillColor = "transparent")
  })
  
  # aster plot
  output$aster = renderAster({

    req(input$sel_type, input$sel_output_goal, input$sel_output_goal_dimension)
    
    # if default input Index score, show aster
    if (input$sel_type=='output' & input$sel_output_goal=='Index' & input$sel_output_goal_dimension=='score'){
      aster(
        data = scores %>%
          filter(
            region_id == v$hi_id,
            dimension == 'score') %>%
          left_join(goals, by='goal') %>%
          filter(is.na(parent), !is.na(order_color)) %>%
          arrange(order_color) %>%
          mutate(label=NA) %>%
          select(id=goal, order=order_color, score, weight, color, label),
        background_color = "transparent",
        font_color = "black", stroke = "blue", font_size_center = "12px", font_size = "8px",
        margin_top=5, margin_right=5, margin_bottom=5, margin_left=5)
    }
  })
  
  output$rgnInfo = renderText({

    if (v$hi_id == 0){
      txt = strong('Global')
    } else {
      req(get_selected())
      
      # if hover, show region
      txt = paste(
        strong(
          subset(rgns@data, rgn_id==v$hi_id, rgn_name)), ':',
        get_selected()$data %>%
          filter(rgn_id == v$hi_id) %>%
          .$value)
    }
    format(txt)
  })
  
  output$hoverText <- renderText({
    if (v$hi_id == 0){
      sprintf("Global: %s km2", format(area_global, big.mark =','))
    } else {
      sprintf(
        "%s: %s km2", 
        subset(rgns@data, rgn_id==v$hi_id, rgn_name, drop=T), 
        format(round(subset(rgns@data, rgn_id==v$hi_id, area_km2, drop=T)), big.mark =','))
    }
  })
  
  
  # elements tab ----
  
  output$network <- renderVisNetwork({
    
    # http://datastorm-open.github.io/visNetwork/options.html
    nb <- 10
    nodes <- data.frame(id = 1:nb, label = paste("Label", 1:nb),
                        group = sample(LETTERS[1:3], nb, replace = TRUE), value = 1:nb,
                        title = paste0("<p>", 1:nb,"<br>Tooltip !</p>"), stringsAsFactors = FALSE)
    
    edges <- data.frame(from = c(8,2,7,6,1,8,9,4,6,2),
                        to = c(3,7,2,7,9,1,5,3,2,9),
                        value = rnorm(nb, 10), label = paste("Edge", 1:nb),
                        title = paste0("<p>", 1:nb,"<br>Edge Tooltip !</p>"))
  
    visNetwork(nodes, edges, height = "500px", width = "100%") %>% 
      visOptions(
        highlightNearest = TRUE, nodesIdSelection = TRUE,
        selectedBy = "group") %>%
      visLayout(randomSeed = 123) %>% 
      # http://datastorm-open.github.io/visNetwork/interaction.html
      visInteraction(
        navigationButtons = TRUE, 
        keyboard = TRUE, tooltipDelay = 0)
  })
  
  observeEvent(input$network_selected, {
    v$msg <- paste("network_selected", input$network_selected)
  })
  observeEvent(input$network_selectedBy, {
    v$msg <- paste("network_selected", input$network_selected)
  })
  
  output$message <- renderText(v$msg)
})

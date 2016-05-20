shinyServer(function(input, output) {
  
  ## get_selected() ----
  ## take geojson's attribute table (rgns@data) and join with scores, filtered for user selection
  get_selected = reactive({
    req(input$sel_type)
    
    switch(input$sel_type,
           
      # case: output     
      output = {
        req(input$sel_output_goal)
        req(input$sel_output_goal_dimension)
        
        #browser()
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
            select(
              rgn_id = fld_id_num, 
              value  = fld_val_num)

        # if layer has category, filter
        if (!is.na(fld_category)){
          req(input$sel_input_target_layer_category_year)
          data = data %>%
            filter(fld_category == input$sel_input_target_layer_category_year)
        }
        
        # if layer has year, filter
        if (!is.na(fld_year)){
          req(input$sel_input_target_layer_category)
          data = data %>%
            filter(fld_category == input$sel_input_target_layer_category)
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
    
   ui = tagList(selectInput(
      'sel_input_target_layer',
      label    = '3. Choose layer:',
      choices  = with(
        layers_by_target %>%
          filter(target == input$sel_input_target) %>%
          mutate(label = sprintf('%s: %s', layer, name)),
        setNames(layer, label))))
    
    if (!is.null(input$sel_input_target_layer)){
      
      fld_category = filter(layers, layer==input$sel_input_target_layer) %>% .$fld_category
      if (!is.na(fld_category)){
        ui = tagList(ui, selectInput(
          'sel_input_target_layer_category',
          label    = sprintf('4. Choose %s:', fld_category),
          choices  = d_lyrs %>%
            filter(layer == input$sel_input_target_layer) %>%
            distinct(fld_category) %>%
            .$fld_category))
        
        fld_year = filter(layers, layer==input$sel_input_target_layer) %>% .$fld_year
        if (!is.na(fld_year) & !is.null(input$sel_input_target_layer_category)){
          ui = tagList(ui, selectInput(
          'sel_input_target_layer_category_year',
          label    = '5. Choose year:',
          choices  = d_lyrs %>%
            filter(
              layer        == input$sel_input_target_layer,
              fld_category == input$sel_input_target_layer_category) %>%
            distinct(fld_year) %>%
            .$fld_year))  }}}
          
   # TODO: fix problem with this not returning extra drop-downs for category and year
   # layers %>% filter(!is.na(fld_category) & !is.na(fld_year)) %>% select(targets, layer, name, fld_id_num, fld_category, fld_year, fld_val_num)
   # MAR | mar_harvest_tonnes         | species_code
   # NP  | np_harvest_tonnes          | product
   # NP  | np_harvest_tonnes_relative | product
   # layers %>% filter(!is.na(fld_category) & is.na(fld_year))
   # cs_habitat_extent
   #cat(file=stderr(), format(ui))
   #cat(file=stderr(), '\n----\n')
   #browser()
   return(ui) })
  
  # output$var_description ----
  # update description of input layer or output goal dimension
  output$var_description = renderText({ 
    get_selected()$description })
  
  # output$map1 ----
  output$map1 <- renderLeaflet({
    
    # get data from selection (get_selected() is the reactive function defined above)
    selected = get_selected()
    # # drop value in rgns spatial data frame if exists
    # rgns@data = rgns@data[,names(rgns@data) != 'value'] # JA/JL not sure what this is doing; 'value' column not in rgns@data geojson
    # # merge value to rgns
    # rgns@data = rgns@data %>%
    #   left_join(
    #     selected$data,
    #     by='rgn_id')
    
    # set color palette
    pal = colorNumeric(
      palette = 'RdYlBu',
      #domain = rgns$value)
      domain = selected$data$value)
    
    # # debug ----
    # data = scores %>%
    #   filter(
    #     goal      == 'Index',
    #     dimension == 'score') %>%
    #   select(
    #     rgn_id = region_id,
    #     value  = score) %>%
    #   select(rgn_id, value)
    # pal = colorNumeric(
    #   palette = 'RdYlBu',
    #   domain = data$value)
    
    # plot map with rgns$id to lookup data$value
    pal = colorNumeric(
      palette = 'RdYlBu',
      #domain = data$value)
      domain = selected$data$value)
    #id2col = function(ids, d=data, col_id='rgn_id', col_val='value'){
    id2col = function(ids, d=selected$data, col_id='rgn_id', col_val='value'){
      pal(d[match(ids, d[[col_id]]), col_val]) 
    }
    leaflet() %>%
      addPolygons(
        data = rgns,
        color = ~id2col(rgn_id))
    leaflet() %>%
      # TODO: add click() and hover() responsiveness? 
      #   see <http://rstudio.github.io/leaflet/shiny.html>,
      #   ["Highlight" polygon on hover? #195](https://github.com/rstudio/leaflet/issues/195)
      addProviderTiles('Stamen.TonerLite', options=tileOptions(noWrap=TRUE)) %>%
      setView(0,0,2) %>%
      addPolygons(
        data = rgns, group = 'regions', layerId = rgns@data$rgn_id,
        #stroke = 'gray', fillOpacity = 0.5, smoothFactor = 0.5,
        stroke = TRUE, fillOpacity = 0.5, smoothFactor = 0.5,
        #color = ~pal(selected$data$value)) %>%
        color = ~id2col(rgn_id)) %>%
      # addPolygons(
      #   group = "highlight", 
      #   data = rgns[match(16, rgns$rgn_id),], 
      #   stroke = T, color = "gray", weight = 5, fillColor = "transparent")
      # TODO: why does legend disappear when choosing type: Input Layer?
      addLegend(
        "bottomright", pal = pal, opacity = 0.5,
        #values = data$value, title = 'Test')
        values = selected$data$value, title = selected$label)
        
  })
  
  # aster hover ----
  
  # input$map_shape_mouseover gets updated a lot, even if the id doesn't change.
  # We don't want to update the polygons and stateInfo except when the id
  # changes, so use v$hi_id to insulate the downstream reactives (as 
  # writing to v$hi_id doesn't trigger reactivity unless the new value 
  # is different than the previous value).
  v <- reactiveValues(hi_id = 0) # set default to GLOBAL = 0
  area_global <- round(sum(rgns@data$area_km2))
  
  observe({
    
    if (length(input$map1_shape_mouseover$id) > 0){

      if (is.character(input$map1_shape_mouseover$id)){
        # strip highlight ("_hi") from id
        v$hi_id <- as.integer(sub('_hi', '', input$map1_shape_mouseover$id))
      } else {
        v$hi_id <- input$map1_shape_mouseover$id
      }
        
      if (length(input$map1_shape_mouseout$id) > 0 && input$map1_shape_mouseout$id == input$map1_shape_mouseover$id){
        v$hi_id <- 0
      }
    } else {
      v$hi_id = 0
    } 
  })
  
  # add shape on hover
  observeEvent(v$hi_id,{
    
    #req(input$map1) # otherwise [error `Couldn't find map with id map`](https://github.com/rstudio/leaflet/issues/242) 
    
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

    req(input$sel_type, input$sel_output_goal, input$sel_output_goal_dimension, v$hi_id)
    
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
  
})

shinyServer(function(input, output) {
  
  # ui_sel_output ----
  output$ui_sel_output <- renderUI({
    req(input$sel_output_goal)
    
    selectInput(
      'sel_output_goal_dimension', 
      label    = '3. Choose dimension:', 
      choices  = scores %>%
        filter(goal == input$sel_output_goal) %>%
        distinct(dimension) %>%
        .$dimension, 
      selected = 'score')})

  # ui_sel_input ----
  output$ui_sel_input <- renderUI({
    req(input$sel_input_target)
    
   sel_input_target_layer = selectInput(
      'sel_input_target_layer',
      label    = '3. Choose layer:',
      choices  = with(
        layers_by_target %>%
          filter(target == input$sel_input_target) %>%
          mutate(label = sprintf('%s: %s', layer, name)),
        setNames(layer, label)))
    
    if (!is.null(input$sel_input_target_layer)){
      
      fld_category = filter(layers, layer==input$sel_input_target_layer) %>% .$fld_category
      if (!is.na(fld_category)){
        sel_input_target_layer_category = selectInput(
          'sel_input_target_layer_category',
          label    = sprintf('4. Choose %s:', fld_category),
          choices  = d_lyrs %>%
            filter(layer == input$sel_input_target_layer) %>%
            distinct(fld_category) %>%
            .$fld_category)
        
        # debug: trying out layers with category and year fields
        # layers %>% filter(!is.na(fld_category) & !is.na(fld_year)) %>% select(targets, layer, name, fld_id_num, fld_category, fld_year, fld_val_num)
        # MAR | mar_harvest_tonnes         | species_code
        # NP  | np_harvest_tonnes          | product
        # NP  | np_harvest_tonnes_relative | product
        
        fld_year = filter(layers, layer==input$sel_input_target_layer) %>% .$fld_year
        if (!is.na(fld_year) & !is.null(input$sel_input_target_layer_category)){
          sel_input_target_layer_category_year = selectInput(
          'sel_input_target_layer_category_year',
          label    = '5. Choose year:',
          choices  = d_lyrs %>%
            filter(
              layer        == input$sel_input_target_layer,
              fld_category == input$sel_input_target_layer_category) %>%
            distinct(fld_year) %>%
            .$fld_year)
          # TODO: fix problem with this not returning extra drop-downs for category and year
          cat(file=stderr(), "\n!OUTPUT layer, category, year\n")
          #browser()
          cat(file=stderr(), format(tagList(
            sel_input_target_layer,
            sel_input_target_layer_category,
            sel_input_target_layer_category_year)))
          return(tagList(
            sel_input_target_layer,
            sel_input_target_layer_category,
            sel_input_target_layer_category_year))
        } else { # is.na(fld_year)
          # TODO: fix problem with this not returning extra drop-down for category
          cat(file=stderr(), "\n!OUTPUT layer, category\n")
          #browser()
          cat(file=stderr(), format(tagList(
            sel_input_target_layer,
            sel_input_target_layer_category)))
          return(tagList(
            sel_input_target_layer,
            sel_input_target_layer_category))
        }
      } else { # is.na(fld_category)
        cat(file=stderr(), "\nOUTPUT layer (1)\n")
        cat(file=stderr(), format(sel_input_target_layer))
        return(sel_input_target_layer)
      }
    } else { # is.null(input$sel_input_target_layer)
      cat(file=stderr(), "\nOUTPUT layer (2)\n")
      cat(file=stderr(), format(sel_input_target_layer))
      return(sel_input_target_layer)
    }})
  
  # get_selected ----
  get_selected = reactive({
    req(input$sel_type)
    if (input$sel_type == 'output'){
      req(input$sel_output_goal)
      req(input$sel_output_goal_dimension)
      list(
        data = rgns@data %>%
          left_join(
            scores %>%
              filter(
                goal      == input$sel_output_goal,
                dimension == input$sel_output_goal_dimension) %>%
              select(
                rgn_id = region_id, 
                value  = score),
            by='rgn_id') %>%
          select(rgn_id, value),
        label = sprintf('%s - %s', input$sel_output_goal, input$sel_output_goal_dimension))
    } else { # presume input$type == 'input'
      req(input$sel_input_target_layer)
      d = d_lyrs %>%
        filter(layer == input$sel_input_target_layer)
      # TODO: add filter for input$sel_input_target_layer_category
      # TODO: add filter for input$sel_input_target_layer_category_year
      list(
        data = rgns@data %>%
          left_join(
            d %>%
            select(
              rgn_id = fld_id_num, 
              value  = fld_val_num),
            by='rgn_id') %>%
          select(rgn_id, value),
        label = input$sel_input_target_layer)
    }
  })

  # map1 ----
  output$map1 <- renderLeaflet({
    
    # get data from selection
    selected = get_selected()
    # drop value in rgns spatial data frame if exists
    rgns@data = rgns@data[,names(rgns@data) != 'value']
    # merge value to rgns
    rgns@data = rgns@data %>%
      left_join(
        selected$data,
        by='rgn_id')
    
    # set color palette
    pal = colorNumeric(
      palette = 'RdYlBu',
      domain = rgns$value)
    
    # plot map
    leaflet(rgns) %>%
      # TODO: deal with nowrap around dateline?
      # TODO: add click() and hover() responsiveness? 
      #   see <http://rstudio.github.io/leaflet/shiny.html>,
      #   ["Highlight" polygon on hover? #195](https://github.com/rstudio/leaflet/issues/195)
      addProviderTiles('Stamen.TonerLite') %>%
      setView(0,0,2) %>%
      addPolygons(
        stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
        color = ~pal(value)) %>%
      # TODO: why does legend disappear when choosing type: Input Layer?
      addLegend(
        "bottomright", pal = pal, opacity = 0.5,
        values = rgns$value, title = selected$label)
  })
  
})

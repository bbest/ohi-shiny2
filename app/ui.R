dashboardPage(
  
  dashboardHeader(title="Ocean Health Index"),
  
  dashboardSidebar(
    
    sidebarMenu(
      id = 'sidebarmenu',
      
      menuItem("Introduction",tabName = 'intro',icon=icon("info-circle",lib='font-awesome')),
      
      menuItem("Explore Data", tabName='explore',icon=icon("globe",lib='font-awesome')),
      
      conditionalPanel(
        "input.sidebarmenu === 'explore'",
    
        selectInput(
          'sel_type', 
          label='1. Choose type:', 
          choices=c('Input Layer'='input', 'Output Score'='output'), 
          selected='output'),
        
        conditionalPanel(
          condition = "input.sel_type == 'output'",
          
          selectInput(
            'sel_output_goal',
            label    = '2. Choose goal:', 
            choices  = output_goals, 
            selected = 'Index'),
          
          uiOutput('ui_sel_output')),
      
        conditionalPanel(
          condition = "input.sel_type == 'input'",
          
          selectInput(
            'sel_input_target',
            label    = '2. Choose target:', 
            choices  = with(layer_targets, setNames(target, target_label))),
          
          uiOutput('ui_sel_input')),
        
        htmlOutput('var_description', class='shiny-input-container') ))),
  
  dashboardBody(
    tabItems(
      
      tabItem(
        tabName='intro',
        includeMarkdown("data/introduction.md")),
      
      tabItem(
        tabName='explore',
        h2("Explore Data"),

        fluidRow(
          box(
            collapsible=T, width=12, 
            div(
              position='relative',
              
              # leaflet map
              leafletOutput('map1', height = 550),
              
              # hover text showing info on hover area
              absolutePanel(
                bottom=10, left=10, style='background-color:white',
                textOutput('hoverText')),
              
              # aster chart
              absolutePanel(
                top=10, right=100,
                asterOutput(outputId = "aster", width='150px', height='150px')) )))))))
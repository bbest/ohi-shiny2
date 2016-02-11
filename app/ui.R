dashboardPage(
  dashboardHeader(title="Ocean Health Index"),
  dashboardSidebar(

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
      uiOutput('ui_sel_input'))),
  
  dashboardBody(
    fluidRow(
      box(collapsible = TRUE, width=12, 
        leafletOutput('map1'))))
)

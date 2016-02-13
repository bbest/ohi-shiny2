suppressPackageStartupMessages({
  library(rgdal)
  library(leaflet)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(shiny)
  library(shinydashboard)
  library(markdown)
  library(htmlwidgets) # devtools::install_github('FrissAnalytics/ohi-aster', subdir='asterHTMLwidget')
  library(jsonlite)
  library(aster)
})
  
#options(shiny.reactlog=TRUE)
#setwd('app')

# load rdata for faster startup
rdata = 'data/default.Rdata'
if (!file.exists(rdata)){
  # TODO: enable drop-down for selecting alternate scenarios
  # TODO: could have github dir, check for latest updates, and enable selection by commit

  # get selectable layers ----
  dir_scenario = './data/ohi-global/eez2015'
  
  # read goals and layers
  layers = read_csv(file.path(dir_scenario, 'layers.csv'))
  goals  = read_csv(file.path(dir_scenario, 'conf/goals.csv'))
  scores = read_csv(file.path(dir_scenario, 'scores.csv')) # AO,BD,CP,CS,CW,ECO,FIS,FP,HAB,ICO,Index,LE,LIV,LSP,MAR,NP,SP,SPP,TR
  dims   = read_csv(file.path(dir_scenario, 'conf/dimensions.csv'))
  
  # read spatial, custom simplified using rmapshaper
  # TODO: set the path spatial for rgn_id in config.R, which should be converted to YAML with spatials registered
  # skipping other spatial fields: 'saup_id','fao_id' # note: 'cntry_key' for www2013, 'country_id' for nature2012
  rgns = readOGR('./data/rgn_offshore_gcs_mapshaper-simplify_x2_eez-only.geojson', 'OGRGeoJSON', verbose = F)
  topoData = readLines('data/rgn_offshore_gcs_mapshaper-simplify_x2_eez-only.topojson.json')
  goals_colors = colorRampPalette(RColorBrewer::brewer.pal(10, 'Spectral'), space='Lab')(nrow(goals))
  goals = goals %>%
    arrange(order_color) %>%
    mutate(color = goals_colors)

  #     wts = get_wts(input)
  #     goals.wts = names(wts)
  #     cols.wts = cols.goals.all[goals.wts]
  #     
  #     # get data from results, so far assuming first line of results/regions_goals.csv
  #     # TODO: add dropdowns for: 1) different regions, 2) different schemes (ie weights)
  #     x = subset(scores, dimension=='score' & region_id==0)
  #     scores.wts  = x$score[match(names(wts), as.character(x$goal))] # regions_goals[1, goals.wts]
  #     index.score = weighted.mean(scores.wts, wts)
  #     
  #     # plot aster
  #     aster(lengths=scores.wts,
  #           max.length=100,
  #           widths=wts,
  #           disk=0.4,
  #           main='Global',
  #           fill.col=cols.wts,
  #           center=round(index.score),
  #           labels=paste(goals.wts, round(scores.wts), sep='\n'),
  #           label.cex=1.0,
  #           label.offset=0.1,
  #           cex=2.2,
  #           cex.main=2.5)
  
  
  # prep output score data ----
  output_goals = c(
    '0 Index'='Index', 
    setNames(
      goals$goal, 
      sprintf('%g %s (%s)', goals$order_hierarchy, goals$name, goals$goal)))
  
  # prep input layer data ----
  
  # filter to layers having rgn_id and numeric values
  layers = layers %>%
    filter(!is.na(fld_val_num)) %>%     # TODO: fix to use fld_val_chr for layers (n=5, ICO spatial): ico_spp_extinction_status, ico_spp_popn_trend, rgn_georegion_labels, rgn_global, rgn_labels
    filter(!is.na(fld_id_num)) %>%      # TODO: fix to use fld_id_chr=='cntry_key'(n=24, LE MAR TR) } or fld_id_chr=='fao_saup_id'(n=1, FIS)
    filter(fld_id_num == 'rgn_id') %>%  # TODO: fix to use other fld_id_num. so far only layers (n=2, FS): fis_b_bmsy, fis_proparea_saup2rgn
    arrange(layer)
  
  # get layers by target goal
  layers_by_target = layers %>%
    select(layer, targets) %>%
    separate(targets, c('target1','target2','target3'), ' ', fill='right') %>%
    gather(target_num, target, target1:target3) %>%
    select(layer, target) %>%
    filter(!is.na(target)) %>%
    left_join(layers, by='layer')
  
  # get available targets
  layer_targets = layers_by_target %>%
    distinct(target) %>%
    select(target) %>%
    left_join(
      bind_rows(
        goals %>%
          select(target = goal, order_hierarchy, name),
        data.frame(
          target = c('pressures','resilience','spatial'),
          order_hierarchy = c(100, 101, 102),
          name = c('pressures','resilience','spatial'))),
      by='target') %>%
    arrange(order_hierarchy, target) %>%
    select(target, order_hierarchy, name) %>%
    mutate(target_label = sprintf('%g %s (%s)', order_hierarchy, name, target))
  
  # paste data together for later selecting appropriate category and year values
  for (i in 1:nrow(layers)){ # i=74
    # layers[i,] %>% select(-description, -fld_id_chr, -fld_val_chr)
    
    # read in data layer
    d = read_csv(file.path(dir_scenario, 'layers', layers$filename[i]))
    
    # convert to lower names
    names(d) = tolower(names(d))
    
    # rename fld_id_num, fld_val_num
    d = rename_(d, 'fld_id_num' = layers$fld_id_num[i], 'fld_val_num' = layers$fld_val_num[i])
    
    # rename fld_category
    if(!is.na(layers$fld_category[i])){
      d = rename_(d, 'fld_category' = layers$fld_category[i])
      d$fld_category = as.character(d$fld_category)
    } else {
      d$fld_category = NA
    }
    
    # rename fld_year
    if(!is.na(layers$fld_year[i])){
      d = rename_(d, 'fld_year' = layers$fld_year[i])
    } else {
      d$fld_year = NA
    }
    
    # set layer name
    d = d %>%
      mutate(layer = layers$layer[i]) %>%
      select(layer, fld_category, fld_year, fld_id_num, fld_val_num) %>%
      arrange(layer, fld_category, fld_year, fld_id_num)
    
    # bind rows
    if (exists('d_lyrs')){
      d_lyrs = bind_rows(d_lyrs, d)
    } else {
      d_lyrs = d
    }
  }

  # save to rdata
  save.image(rdata)
  
} else {
  load(rdata)
}

topojson <- readLines('data/rgn_offshore_gcs_mapshaper-simplify_x2_eez-only.topojson.json', warn = FALSE) # %>%
  #paste(collapse = "\n") %>%
  #jsonlite::fromJSON(simplifyVector=F)

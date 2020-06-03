library(ggplot2)
library(maps)
library(rgdal)# R wrapper around GDAL/OGR
library(sp)
# require("RPostgreSQL")
library(RPostgreSQL)
library(postGIStools)
library(plyr)
library(dplyr)
library(viridis)
library(scales)
library(rjson)
# library(jsonlite)
require(RColorBrewer)
library(glue)
# library(ggpubr)
library(cowplot)
library(stringi)
library(gridBase)
library(grid)
library(gridSVG)
library(gridExtra) #load Grid
library(extrafont)
library(sf)
library(rasterVis)
library(ggthemes) # theme_map()
library(ggpmisc)




library("ggpubr")
library(readr)
library(SciViews)

options(scipen=999)

# load fonts - every session
loadfonts(device = "win", quiet = TRUE)



############### clean slate ###################################################

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.

#####kill the postgres connections each time run script
killDbConnections <- function () {

  all_cons <- dbListConnections(PostgreSQL())

  print(all_cons)

  for(con in all_cons)
    +  dbDisconnect(con)

  print(paste(length(all_cons), " connections killed."))

}

killDbConnections()



# ###### link to json files #################################################
figure_json = 'C:\\Users\\Bougie\\Desktop\\scripts\\projects\\usxp\\stages\\deliverables\\scatterplots\\json\\figure_json.json'
figure_obj<- fromJSON(file=figure_json)



######################################################################
##########  create postgres connections ##########################################
#########################################################################
user <- "mbougie"
host <- '144.92.235.105'
port <- '5432'
password <- 'Mend0ta!'

con_nass <- dbConnect(PostgreSQL(), dbname = 'nass', user = user, host = host, port=port, password = password)
con_nri <- dbConnect(PostgreSQL(), dbname = 'nri', user = user, host = host, port=port, password = password)
con_usxp_deliverables <- dbConnect(PostgreSQL(), dbname = 'usxp_deliverables', user = user, host = host, port=port, password = password)
con_usxp <- dbConnect(PostgreSQL(), dbname = 'usxp', user = user, host = host, port=port, password = password)


######################################################################
##########  import featureclasses ##########################################
#########################################################################
states <- readOGR(dsn='H:\\e_drive\\data\\general\\shapefiles.gdb',layer="states_102003")
counties <- readOGR(dsn='H:\\e_drive\\data\\general\\shapefiles.gdb',layer="counties_102003")

fc <- sf::st_read("H:\\e_drive\\data\\nass\\districts\\asds.gdb", layer = "asd_2012_20m_102003")

######################################################################
##########  create dataframe ##########################################
#########################################################################

createDF_usxp <- function(cp){
  
  p = fp$datasets_usxp[[cp$datasets_usxp]]
  con = eval(parse(text = p$con))
  query= read_file(paste(path_sql, eu, p$query, sep = "\\"))
  
  
  ####read in dataset from postgres
  df_init <- dbGetQuery(con_usxp, query)
  
  df<-df_init %>% inner_join(fc, c("district_unique" = "STASD_A"))

  df$acres_usxp_per_year_per_eu = (df$acres_usxp_per_year/df$acres) * 100

  return(st_as_sf(df))
}

createDF <- function(name, map){
  # name="NLCD"
  cp <- fp$datasets[[name]]
  ########### create intial DF and join dataframes together ##################
  df_usxp = createDF_usxp(cp)
  
  con = eval(parse(text = cp$con))
  query= read_file(paste(path_sql, eu, cp$query, sep = "\\"))
  
  ####read in dataset from postgres
  df_init <- dbGetQuery(con, query)
  
  ####join usxp dataframe together specific dataframe
  df  <- df_usxp %>% inner_join(df_init , by = "district_unique")

  df$acres_per_year_per_eu = (df$acres_per_year/df$acres) * 100

  ####create difference of acres per year between dataframes
  df$diff_acres = (df$acres_per_year_per_eu - df$acres_usxp_per_year_per_eu)
  # ####the difference per year relative to acres of usxp per year
  df$diff_ratio = (df$diff_acres/(df$acres_usxp_per_year_per_eu))
  
  ########### apply saturation for map figure ##################################
  
  #-------apply saturation to the column of interest
  df$col_sat = df[[fp$maps[[map]]$current_col]]
  df$col_sat[df$col_sat <= -fp$maps[[map]]$sat] <- -fp$maps[[map]]$sat
  df$col_sat[df$col_sat >= fp$maps[[map]]$sat] <- fp$maps[[map]]$sat

  # ########### transform data for scatterplot figure #################################

  df$x_log1p = log1p(df$acres_usxp_per_year)
  df$y_log1p = log1p(df$acres_per_year)
  

  ### Note need to use the st_as_sf function after appending columns or geom_sf won't recognize object!!!!
  return(st_as_sf(df))
}

##################################################################################
######## qaqc table to check dataframe creation and get the ranges of scatterplots ###############################
#############################################################################################

qaqc <- function(df, fileout){
  test <- st_drop_geometry(df)
  
  csv_out0 = paste0('I:\\projects\\usxp\\series\\s35\\deliverables\\scatterplot\\csv\\', fileout, '_raw.csv')
  write.csv(test, csv_out0)
  
  test <- test %>% dplyr::select(acres_usxp_per_year_per_eu, acres_per_year_per_eu , diff_acres, diff_ratio, x_log1p, y_log1p)
  df_qaqc <- data.frame(min=sapply(test,min), max=sapply(test,max), min=sapply(test[Reduce(`&`, lapply(test, is.finite)),],min), max=sapply(test[Reduce(`&`, lapply(test, is.finite)),],max))
  
  csv_out1 = paste0('I:\\projects\\usxp\\series\\s35\\deliverables\\scatterplot\\csv\\', fileout, '_qaqc.csv')
  write.csv(df_qaqc, csv_out1)
  
}

##################################################################
#### map function #########################################
##################################################################

createMap <- function(df, map, plot.title){

  
  d = ggplot() + 
    
  geom_polygon(
      data=states,
      aes(x=long,y=lat,group=group),
      fill='#7e7e7e'
      # fill='red'
  ) +
    

  
  ######## main dataframe ###########
  geom_sf(data= df, aes(fill = col_sat)) + 
    
    
  geom_polygon(
      data=states,
      aes(x=long,y=lat,group=group),
      fill=NA,
      # color='#D3D3D3',
      color='#383838',
      size=1.5
    ) +
    
  labs(title = plot.title) + 
    
  theme(
    #### nulled attributes ##################
    axis.text.x = element_blank(),
    axis.title.x=element_blank(),
    axis.text.y = element_blank(),
    axis.title.y=element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    
    # panel.border= element_rect(size = 2, fill = NA),
    # panel.background = element_rect(fill = 'blue', color = 'purple'),
    panel.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),
    
    legend.text=element_text(size=25),
    legend.text.align = 0,
    legend.title=element_text(size=30),
    legend.key.width = unit(4,"cm"),
    legend.key.height = unit(1,"cm"),
    
    # plot.background = element_rect(fill = 'green', color = 'red'),
    plot.background = element_rect(fill = NA, color = NA),
    plot.title = element_text(size= 35),
    plot.margin = unit(c(t=0, r=0, b=0, l=0), "cm")
  ) + 
 
  # coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") + 
  scale_fill_gradientn(colors = rev(brewer.pal(11,fp$maps[[map]]$palette)),
                       values = rescale(c(-fp$maps[[map]]$sat, 0, fp$maps[[map]]$sat)),
                       limits=c(-fp$maps[[map]]$sat, fp$maps[[map]]$sat),
                       guide = guide_colourbar(title = fp$maps[[map]]$legend.title,
                                               direction = "horizontal",
                                               # title.vjust=1,
                                               title.position = 'top'
                       ))
  return(d)
}


###################################################################################
##### hexbin function ######################################################
###################################################################################

hexBinRegPlot = function(df, dataset){
  
  
  minVal = min(df$acres_usxp_per_year, df$acres_per_year, na.rm = T) # the na.rm = T here and below is important!
  print(minVal)
  maxVal = max(df$acres_usxp_per_year, df$acres_per_year, na.rm = T)
  print(maxVal)
  
  
  my.formula <- y ~ x
  # x = fp$datasets[[dataset]]$x
  # y = fp$datasets[[dataset]]$y
  
  # p <- ggplot(data=df,aes(x = .data[[x]], y = .data[[y]])) +
  p <- ggplot(data=df,aes(x = acres_usxp_per_year, y = acres_per_year)) +
    geom_point(colour = "#5c8a8a", aes(size=20,alpha=0.7)) +
    # geom_hex(bins = 35) +
    theme_bw()+
    guides(fill = guide_colourbar(ticks.colour = NA,
                                  frame.colour = "black",
                                  title="Count"))+

    labs(y=bquote(.(dataset) ~ ('acres' ~ yr^-1)), x=bquote('This Study' ~ ('acres' ~ yr^-1))) + 
    theme(
      panel.border= element_rect(size = 2),
      text = element_text(size=30),
      
      axis.title.x = element_text(face = 'bold'),
      axis.title.y = element_text(face = 'bold'),
      axis.title = element_text(size = 25),
      
      legend.text=element_text(size=25),
      legend.text.align = 0,
      legend.title=element_text(size=30),
      legend.key.width = unit(1, "cm"),
      legend.key.height = unit(2, "cm"),
      legend.position="none",
      
      
      # panel.background = element_rect(fill = 'green', color = 'red'),
      panel.background = element_rect(fill = NA, color = NA),
      ###extend bottom margin of plot to accomidate legend and grob annotation
      # plot.background = element_rect(fill = 'green', color = 'red'),
      plot.background = element_rect(fill = NA, color = NA),
      plot.margin = unit(c(t=2, r=0, b=2, l=0), "cm")
    )+
    
    geom_abline(slope = 1, intercept = 0, colour="black",size=2, linetype = "dashed") +
    geom_smooth(method = "lm", se = FALSE, formula = my.formula, size=2, colour="black") +
    stat_poly_eq(formula = my.formula,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 size=9,
                 parse = TRUE) +
    

    scale_y_continuous(trans = 'log1p',
                       breaks = breaks_log(base = 10),
                       labels = function(x) comma(x),
                       limits = c(minVal, maxVal))+
    scale_x_continuous(trans = 'log1p',
                       breaks = breaks_log(base = 10),
                       labels = function(x) comma(x),
                       limits = c(minVal, maxVal))+

    scale_fill_gradientn(colors = rev(brewer.pal(11,"Spectral")))
  
  
  p
  return(p)
}





hexBinRegPlot_nass = function(df, dataset){
  minVal = min(df$acres_usxp_per_year, df$acres_per_year, na.rm = T) # the na.rm = T here and below is important!
  print(minVal)
  maxVal = max(df$acres_usxp_per_year, df$acres_per_year, na.rm = T)
  print(maxVal)
  
  my.formula <- y ~ x
  # x = fp$datasets[[dataset]]$x
  # y = fp$datasets[[dataset]]$y
  
  # p <- ggplot(data=df,aes(x = .data[[x]], y = .data[[y]])) +
  p <- ggplot(data=df,aes(x = acres_usxp_per_year, y = acres_per_year)) +
    geom_point(colour = "#5c8a8a", aes(size=20,alpha=0.7)) +
    # geom_hex(bins = 35) +
    theme_bw()+
    guides(fill = guide_colourbar(ticks.colour = NA,
                                  frame.colour = "black",
                                  title="Count"))+
    
    labs(y=bquote(.(dataset) ~ ('acres' ~ yr^-1)), x=bquote('This Study' ~ ('acres' ~ yr^-1))) + 
    theme(
      panel.border= element_rect(size = 2),
      text = element_text(size=30),
      
      axis.title.x = element_text(face = 'bold'),
      axis.title.y = element_text(face = 'bold'),
      axis.title = element_text(size = 25),
      
      legend.text=element_text(size=25),
      legend.text.align = 0,
      legend.title=element_text(size=30),
      legend.key.width = unit(1, "cm"),
      legend.key.height = unit(2, "cm"),
      legend.position="none",
      
      # panel.background = element_rect(fill = 'green', color = 'red'),
      panel.background = element_rect(fill = NA, color = NA),
      ###extend bottom margin of plot to accomidate legend and grob annotation
      # plot.background = element_rect(fill = 'green', color = 'red'),
      plot.background = element_rect(fill = NA, color = NA),
      plot.margin = unit(c(t=2, r=0, b=2, l=0), "cm")
    )+
    
    geom_abline(slope = 1, intercept = 0, colour="black",size=2, linetype = "dashed") +
    geom_smooth(method = "lm", se = FALSE, formula = my.formula, size=2, colour="black") +
    stat_poly_eq(formula = my.formula,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 size=9,
                 parse = TRUE) +
    

    scale_x_continuous(limits = c(minVal, maxVal))+
    scale_y_continuous(limits = c(minVal, maxVal))+
    scale_fill_gradientn(colors = rev(brewer.pal(11,"Spectral")))
  
  
  p
  return(p)
}




########################################################################################
###### ~~~~~~~~~~~~~~~~~~~~~~~~queries~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----########
########################################################################################

eu='districts'
fp = figure_obj[[eu]]
path_sql = 'C:\\Users\\Bougie\\Desktop\\scripts\\projects\\usxp\\stages\\deliverables\\scatterplots\\sql'

########## USXP ##############################################################




########################################################################################
###### ~~~~~~~~~~~~~~~~~~~~~~~~ RUN SCRIPT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###############################################################################################


  for(name in names(fp$datasets)){
    print(name)

    df1 = createDF(name, map="map1")
    fp$datasets[[name]]$df1 = df1
    ###export df to csv
    qaqc(df=df1, name)
    map1 = createMap(df=df1, map="map1", plot.title=name)
    fp$datasets[[name]]$map1 = map1
    
    df2 = createDF(name, map="map2")
    fp$datasets[[name]]$df2 = df2
    ###export df to csv
    qaqc(df=df2, name)
    map2 = createMap(df=df2,map="map2", plot.title=name)
    fp$datasets[[name]]$map2 = map2
    
    df3 = createDF(name, map="map3_diff_acres")
    fp$datasets[[name]]$df3 = df3
    ###export df to csv
    qaqc(df=df3, name)
    map3 = createMap(df=df3,map="map3_diff_acres", plot.title='')
    fp$datasets[[name]]$map3 = map3
    
    if(name=="NASS"){
      sp = hexBinRegPlot_nass(df=df2, dataset=name)
      fp$datasets[[name]]$sp = sp
    }else{
      sp = hexBinRegPlot(df=df2, dataset=name)
      fp$datasets[[name]]$sp = sp
    }
  }






########################################################################################
###### ~~~~~~~~~~~~~~~~~~~~~~~~ Arrange panels ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###############################################################################################
nri_obj=fp$datasets[['NRI']]
nlcd_obj=fp$datasets[['NLCD']]
nass_obj=fp$datasets[['NASS']]

# c0 = ggarrange(nri_obj$map1, nlcd_obj$map1, nass_obj$map1, ncol = 1, nrow = 3, common.legend = TRUE, legend="bottom")
c2 = ggarrange(nri_obj$map2, nlcd_obj$map2, nass_obj$map2, ncol = 1, nrow = 3, common.legend = TRUE, legend="bottom")
c3 = ggarrange(nri_obj$map3, nlcd_obj$map3, nass_obj$map3, ncol = 1, nrow = 3, common.legend = TRUE, legend="bottom")
c4 = ggarrange(nri_obj$sp, nlcd_obj$sp, nass_obj$sp, ncol = 1, nrow = 3)

map = ggarrange(c2, c3, c4, ncol = 3, nrow = 1)

fileout = paste0('I:\\projects\\usxp\\series\\s35\\deliverables\\scatterplot\\figures\\',eu,'.png')
ggplot2::ggsave(fileout, width = 34, height = 25, dpi = 500)





















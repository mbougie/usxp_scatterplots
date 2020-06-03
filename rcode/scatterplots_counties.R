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
con_usxp <- dbConnect(PostgreSQL(), dbname = 'usxp', user = user, host = host, port=port, password = password)
con_usxp_deliverables <- dbConnect(PostgreSQL(), dbname = 'usxp_deliverables', user = user, host = host, port=port, password = password)



######################################################################
##########  import featureclasses ##########################################
#########################################################################
states <- readOGR(dsn='H:\\e_drive\\data\\general\\shapefiles.gdb',layer="states_102003")
counties <- readOGR(dsn='H:\\e_drive\\data\\general\\shapefiles.gdb',layer="counties_102003")

# fc <- sf::st_read("H:\\e_drive\\data\\nass\\districts\\asds.gdb", layer = "asd_2012_20m_102003")
fc <- sf::st_read("H:\\e_drive\\data\\general\\shapefiles.gdb", layer="counties_102003")
######################################################################
##########  create dataframe ##########################################
#########################################################################
createDF_usxp <- function(cp){
  
  p = fp$datasets_usxp[[cp$datasets_usxp]]
  con = eval(parse(text = p$con))
  query= read_file(paste(path_sql, eu, p$query, sep = "\\"))
  
  ####read in dataset from postgres
  df <- dbGetQuery(con, query)
  
  return(df)
}


createDF <- function(name, map){
  # name= "NLCD"
  ###current parameters########################
  cp <- fp$datasets[[name]]

  ########### create intial DF and join dataframes together ##################
  df_usxp = createDF_usxp(cp)
  
  con = eval(parse(text = cp$con))
  query= read_file(paste(path_sql, eu, cp$query, sep = "\\"))
 
   ####read in dataset from postgres
  df_init <- st_read(con, query=query)
  
  ####join usxp dataframe together specific dataframe
  df  <- df_usxp %>% inner_join(df_init , by = "atlas_name")

  
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
  
  # df$x_log1p = log1p(df$acres_usxp_per_year)
  # df$y_log1p = log1p(df$acres_per_year)
  
  
  
  
  

  # ########### transform data for scatterplot figure #################################
  # df$x = df[[fp$scatterplots$perc_eu$x]]
  # df$y = df[[fp$scatterplots$perc_eu$y]]
  # 
  # df$x_log10 = log(df[[fp$scatterplots$perc_eu$x]], base=10)
  # df$y_log10 = log(df[[fp$scatterplots$perc_eu$y]], base=10)


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
  
  # test <- test %>% dplyr::select(acres_usxp_per_year_per_eu, acres_per_year_per_eu , x_log10, y_log10, x_log10_plus1, y_log10_plus1)
  # df_qaqc <- data.frame(min=sapply(test,min), max=sapply(test,max), min=sapply(test[Reduce(`&`, lapply(test, is.finite)),],min), max=sapply(test[Reduce(`&`, lapply(test, is.finite)),],max))
  # 
  # csv_out1 = paste0('I:\\projects\\usxp\\series\\s35\\deliverables\\scatterplot\\csv\\', fileout, '_qaqc.csv')
  # write.csv(df_qaqc, csv_out1)
  
  # csv_out2 = paste0('I:\\projects\\usxp\\series\\s35\\deliverables\\scatterplot\\csv\\', fileout, '.csv')
  # write.csv(test, csv_out2)
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
    

  
  #   ### main dataframe ###########
  # geom_polygon(
  #   data= mapa.df,
  #   aes(y=lat, x=long, group=group, fill = acres_per_year)
  # ) + 
    
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
  my.formula <- y ~ x
  
  
  # p <- ggplot(data=df,aes(x = .data[[fp$log_trans[1]]], y = .data[[fp$log_trans[2]]])) +
  # p <- ggplot(data=df,aes(x = x_log10, y = y_log10)) +
    # p <- ggplot(data=df,aes(x = .data[[x]], y = .data[[y]])) +
  p <- ggplot(data=df,aes(x = acres_usxp_per_year, y = acres_per_year)) +
    # geom_point() +
    geom_hex(bins = 35) +
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
      
      
      # panel.background = element_rect(fill = 'green', color = 'red'),
      panel.background = element_rect(fill = NA, color = NA),
      ###extend bottom margin of plot to accomidate legend and grob annotation
      # plot.background = element_rect(fill = 'green', color = 'red'),
      plot.background = element_rect(fill = NA, color = NA),
      plot.margin = unit(c(t=4, r=0, b=0, l=0), "cm")
    )+
    
    geom_abline(slope = 1, intercept = 0, colour="black",size=2, linetype = "dashed") +
    geom_smooth(method = "lm", se = FALSE, formula = my.formula, size=2, colour="black") +
    # stat_poly_eq(formula = my.formula,
    #              aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    #              size=9,
    #              parse = TRUE) +
    stat_poly_eq(formula = my.formula,
                 aes(label = ..rr.label..),
                 size=9,
                 parse = TRUE) + 
    # scale_x_continuous(trans='log10') +
    # scale_y_continuous(trans='log10') +
    scale_fill_gradientn(colors = rev(brewer.pal(11,"Spectral")))
  
  
  p
  return(p)
}




########################################################################################
###### ~~~~~~~~~~~~~~~~~~~~~~~~queries~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----########
########################################################################################

eu='states'
fp = figure_obj[[eu]]
path_sql = 'C:\\Users\\Bougie\\Desktop\\scripts\\projects\\usxp\\stages\\deliverables\\scatterplots\\sql'



########################################################################################
###### ~~~~~~~~~~~~~~~~~~~~~~~~ RUN SCRIPT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###############################################################################################


for(name in names(fp$datasets)){
    print(name)

    #####name="NLCD"

    df1 = createDF(name, map="map1")
    qaqc(df=df1, name)
    fp$datasets[[name]]$df1 = df1
    map1 = createMap(df=df1, map="map1", plot.title=name)
    fp$datasets[[name]]$map1 = map1

    df2 = createDF(name, map="map2")
    qaqc(df=df2, name)
    fp$datasets[[name]]$df2 = df2
    map2 = createMap(df=df2,map="map2", plot.title=name)
    fp$datasets[[name]]$map2 = map2

    df3 = createDF(name, map="map3")
    qaqc(df=df3, name)
    fp$datasets[[name]]$df3 = df3
    map3 = createMap(df=df3,map="map3", plot.title='')
    fp$datasets[[name]]$map3 = map3

    sp = hexBinRegPlot(df=df2, dataset=name)
    fp$datasets[[name]]$sp = sp

  
  }






########################################################################################
###### ~~~~~~~~~~~~~~~~~~~~~~~~ Arrange panels ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###############################################################################################
nri_obj=fp$datasets[['NRI']]
nlcd_obj=fp$datasets[['NLCD']]
nass_obj=fp$datasets[['NASS']]

# c1 = ggarrange(nri_obj$map1, nlcd_obj$map1, nass_obj$map1, ncol = 1, nrow = 3, common.legend = TRUE, legend="bottom")
c2 = ggarrange(nri_obj$map2, nlcd_obj$map2, nass_obj$map2, ncol = 1, nrow = 3, common.legend = TRUE, legend="bottom")
c3 = ggarrange(nri_obj$map3, nlcd_obj$map3, nass_obj$map3, ncol = 1, nrow = 3, common.legend = TRUE, legend="bottom")
c4 = ggarrange(nri_obj$sp, nlcd_obj$sp, nass_obj$sp, ncol = 1, nrow = 3)

map = ggarrange(c2, c3, c4, ncol = 3, nrow = 1)

fileout = paste0('I:\\projects\\usxp\\series\\s35\\deliverables\\scatterplot\\figures\\',eu,'.png')
ggplot2::ggsave(fileout, width = 34, height = 25, dpi = 500)



nri_df2 = figure_obj[["counties"]][["datasets"]][["NLCD"]][["df2"]]





















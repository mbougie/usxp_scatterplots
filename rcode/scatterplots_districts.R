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
figure_json = 'C:\\Users\\Bougie\\Desktop\\scripts\\projects\\usxp\\stages\\deliverables\\scatterplots\\json\\figure_json_v2.json'
figure_obj<- fromJSON(file=figure_json)



###get postgres connections ############################
user <- "mbougie"
host <- '144.92.235.105'
port <- '5432'
password <- 'Mend0ta!'

con_nass <- dbConnect(PostgreSQL(), dbname = 'nass', user = user, host = host, port=port, password = password)
con_nri <- dbConnect(PostgreSQL(), dbname = 'nri', user = user, host = host, port=port, password = password)
con_usxp_deliverables <- dbConnect(PostgreSQL(), dbname = 'usxp_deliverables', user = user, host = host, port=port, password = password)
con_usxp <- dbConnect(PostgreSQL(), dbname = 'usxp', user = user, host = host, port=port, password = password)


#### bring in featureclasses for context in map ##################################
states <- readOGR(dsn='H:\\e_drive\\data\\general\\shapefiles.gdb',layer="states_102003")
counties <- readOGR(dsn='H:\\e_drive\\data\\general\\shapefiles.gdb',layer="counties_102003")

fc <- sf::st_read("H:\\e_drive\\data\\nass\\districts\\asds.gdb", layer = "asd_2012_20m_102003")



sql_nri = 'C:\\Users\\Bougie\\Desktop\\scripts\\projects\\usxp\\stages\\deliverables\\scatterplots\\sql\\districts\\nri.sql'
nri <- read_file(sql_nri)
xx = dbGetQuery(con_nri,nri)

######################################################################
##########  create dataframe ##########################################
#########################################################################
createDF_usxp <- function(query){
  
  ########### create intial DF and join dataframes together ##################
  
  ####read in dataset from postgres
  df_init <- dbGetQuery(con_usxp, query)
  
  df<-df_init %>% inner_join(fc, c("district_unique" = "STASD_A"))

  df$acres_usxp_per_year_per_eu = (df$acres_usxp_per_year/df$acres) * 100

  return(st_as_sf(df))
}



createDF <- function(con, query, usxp_query, map){
  
  ########### create intial DF and join dataframes together ##################
  df_usxp = createDF_usxp(query=usxp_query)
  ####read in dataset from postgres
  df_init <- dbGetQuery(con, query)
  
  ####join usxp dataframe together specific dataframe
  df  <- df_usxp %>% inner_join(df_init , by = "district_unique")

  df$acres_per_year_per_eu = (df$acres_per_year/df$acres) * 100

  # ####create difference of acres per year between dataframes
  # df$diff_acres = (df$acres_usxp_per_year_per_eu - df$acres_per_year_per_eu)
  # # ####the difference per year relative to acres of usxp per year
  # df$diff_perc = (df$diff_acres/(df$acres_usxp_per_year_per_eu)*100)
  
  
  ####create difference of acres per year between dataframes
  df$diff_acres = (df$acres_usxp_per_year-df$acres_per_year)
  
  # ####the difference per year relative to acres of usxp per year
  df$diff_perc = (df$diff_acres/(df$acres_usxp_per_year)*100)
  
  ########### apply saturation for map figure ##################################
  
  #-------apply saturation to the column of interest
  df$col_sat = df[[fp$maps[[map]]$current_col]]
  df$col_sat[df$col_sat <= -fp$maps[[map]]$sat] <- -fp$maps[[map]]$sat
  df$col_sat[df$col_sat >= fp$maps[[map]]$sat] <- fp$maps[[map]]$sat

  # ########### transform data for scatterplot figure #################################
  df$x = df[[fp$scatterplots$perc_eu$x]]
  df$y = df[[fp$scatterplots$perc_eu$y]]

  df$x_log10 = log(df[[fp$scatterplots$perc_eu$x]], base=10)
  df$y_log10 = log(df[[fp$scatterplots$perc_eu$y]], base=10)


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
  
  test <- test %>% dplyr::select(acres_usxp_per_year_per_eu, acres_per_year_per_eu , x_log10, y_log10, x_log10_plus1, y_log10_plus1)
  df_qaqc <- data.frame(min=sapply(test,min), max=sapply(test,max), min=sapply(test[Reduce(`&`, lapply(test, is.finite)),],min), max=sapply(test[Reduce(`&`, lapply(test, is.finite)),],max))
  
  csv_out1 = paste0('I:\\projects\\usxp\\series\\s35\\deliverables\\scatterplot\\csv\\', fileout, '_qaqc.csv')
  write.csv(df_qaqc, csv_out1)
  
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
  p <- ggplot(data=df,aes(x = x, y = y)) +
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
    stat_poly_eq(formula = my.formula,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 size=9,
                 parse = TRUE) +
    # scale_x_continuous(trans='log10') +
    # scale_y_continuous(trans='log10') +
    scale_fill_gradientn(colors = rev(brewer.pal(11,"Spectral")))
  
  
  p
  return(p)
}




########################################################################################
###### ~~~~~~~~~~~~~~~~~~~~~~~~create queries~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###############################################################################################


########## USXP ##############################################################

# query_usxp_exp = "SELECT
# c.district_unique,
# b.state_name,
# sum((a.acres/8)) as acres_usxp_per_year
# --((a.acres/8)/b.acres_calc * 100) as acres_usxp_per_year_per_eu
# --st_union(st_snaptogrid(b.wkb_geometry, 0.0001::double precision)) AS geom
# FROM
# zonal_stats.s35_mtr_counties as a INNER JOIN spatial.counties_102003 as b
# USING(atlas_stco) INNER JOIN
# misc.district_lookup as c on b.atlas_stco=c.fips 
# WHERE label in ('3')
# group by b.state_name, c.district_unique"

query_usxp_exp = read_file('C:\\Users\\Bougie\\Desktop\\scripts\\projects\\usxp\\stages\\deliverables\\scatterplots\\sql\\districts\\query_usxp_exp.sql')
df_usxp_exp <- createDF_usxp(con=con_usxp, query=query_usxp_exp)




# query_usxp_net = "SELECT
# c.district_unique,
# county.state_name,
# sum((mtr3.acres - mtr4.acres)/8) as acres_usxp_per_year
# ---(((mtr3.acres - mtr4.acres)/8 )/county.acres_calc * 100) as acres_usxp_per_year_per_eu
# --county.wkb_geometry as geom
# FROM 
# 
# (SELECT 
# t1.atlas_stco,
# t1.acres as acres
# FROM 
# zonal_stats.s35_mtr_counties as t1 
# WHERE label in ('3')) as mtr3
# 
# 
# INNER JOIN
# 
# 
# (SELECT 
# t1.atlas_stco, 
# t1.acres as acres
# FROM 
# zonal_stats.s35_mtr_counties as t1 
# WHERE label in ('4')) as mtr4
# 
# 
# USING(atlas_stco)
# 
# INNER JOIN
# spatial.counties_102003 as county USING(atlas_stco) 
# INNER JOIN
# misc.district_lookup as c on county.atlas_stco=c.fips 
# group by county.state_name, c.district_unique"

query_usxp_net = read_file('C:\\Users\\Bougie\\Desktop\\scripts\\projects\\usxp\\stages\\deliverables\\scatterplots\\sql\\districts\\query_usxp_net.sql')
df_usxp_net <- createDF_usxp(con=con_usxp, query=query_usxp_net)






########## NRI ##############################################################

# query_nri =  "SELECT
# c.district_unique,
# b.state_name,
# sum(a.expansion/8) as acres_per_year
# --st_union(st_snaptogrid(b.wkb_geometry, 0.0001::double precision)) AS geom
# FROM main.nri_2015_analysis as a INNER JOIN 
# spatial.counties_102003 as b USING(atlas_stco)
# INNER JOIN
# misc.district_lookup as c on b.atlas_stco=c.fips 
# --WHERE a.expansion IS NOT NULL
# group by b.state_name, c.district_unique"

query_nri = read_file('C:\\Users\\Bougie\\Desktop\\scripts\\projects\\usxp\\stages\\deliverables\\scatterplots\\sql\\districts\\query_nri.sql')


########## NLCD ##############################################################

# query_nlcd = "SELECT
# c.district_unique,
# b.state_name,
# sum(a.acres/8) as acres_per_year
# ----st_union(st_snaptogrid(b.wkb_geometry, 0.0001::double precision)) AS geom
# FROM
# choropleths.combine_nlcd08_16_histo as a INNER JOIN
# spatial.counties_102003 as b
# USING(atlas_stco) INNER JOIN
# misc.district_lookup as c on b.atlas_stco=c.fips 
# WHERE a.label = '4'
# group by b.state_name, c.district_unique"

query_nlcd = read_file('C:\\Users\\Bougie\\Desktop\\scripts\\projects\\usxp\\stages\\deliverables\\scatterplots\\sql\\districts\\query_nlcd.sql')





#####################  NASS #####################################################

# query_nass="SELECT
# c.district_unique,
# b.state_name,
# sum(a.diff/10) as acres_per_year
# --d.wkb_geometry AS geom
# ---t_union(st_snaptogrid(b.wkb_geometry, 0.0001::double precision)) AS geom
# FROM ag_census.ag_census_expansion as a INNER JOIN
# spatial.counties_102003 as b USING(atlas_stco) INNER JOIN
# misc.district_lookup as c on b.atlas_stco=c.fips 
# ---spatial.asd_2012_20m_102003 as d on c.district_unique=d.stasd_a
# group by b.state_name, c.district_unique
# order by state_name"

query_nass = read_file('C:\\Users\\Bougie\\Desktop\\scripts\\projects\\usxp\\stages\\deliverables\\scatterplots\\sql\\districts\\query_nass.sql')






########################################################################################
###### ~~~~~~~~~~~~~~~~~~~~~~~~ main function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###############################################################################################

# main <- function(){
#   df1_list =list()
#   map1_list = list()
#   df2_list =list()
#   map2_list = list()
#   scatterplot_list = list()
#   for(name in names(dfParams)){
#     
#     print(dfParams[[name]])
#     obj = (dfParams[[name]])
#     
#     ####create dataframe
#     df1 = createDF(con=obj$con, query=obj$query, df_usxp=obj$df_uxsp, current_col="acres_per_year_per_eu", sat=1.0)
#     df1_list <- append(df1_list, list(df1))
#     ###create map1
#     map1 = createMap(df=df1,  plot.title="obj$plot.title", palette="RdBu", sat=1.0)
#     map1_list <- append(map1_list, list(map1))
# 
# 
#     ####create dataframe
#     df2 = createDF(con=obj$con, query=obj$query, df_usxp=obj$df_uxsp, current_col="diff_perc", sat=200)
#     df2_list <- append(df2_list, list(df2))
#     ###create map2
#     map2 = createMap(df=df2, plot.title="", palette="RdBu", sat=200)
#     map2_list <- append(map2_list, list(map2))
#     
# 
#     ###create scatterplot
#     scatterplot = hexBinRegPlot_2(df=df2)
#     scatterplot_list <- append(scatterplot_list, list(scatterplot))
#   }
#   
#   finalList <- list("df1"=df1_list, "map1"=map1_list, "df2"=df2_list, "map2"=map2_list, "scatterplot"=scatterplot_list)
#   return(finalList)
# }


########################################################################################
###### ~~~~~~~~~~~~~~~~~~~~~~~~ RUN SCRIPT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###############################################################################################

eu='districts'
fp = figure_obj[[eu]]



  for(name in names(fp$datasets)){

    con=eval(parse(text = fp$datasets[[name]]$con))
    query=eval(parse(text = fp$datasets[[name]]$query))
    usxp_query=eval(parse(text = fp$datasets[[name]]$usxp_query))
   
    df1 = createDF(con=con, query=query, usxp_query=usxp_query, map="map1")
    figure_obj[[eu]]$datasets[[name]]$df1 = df1
    map1 = createMap(df=df1, map="map1", plot.title=name)
    figure_obj[[eu]]$datasets[[name]]$map1 = map1
  
    df2 = createDF(con=con, query=query, usxp_query=usxp_query, map="map2")
    figure_obj[[eu]]$datasets[[name]]$df2 = df2
    map2 = createMap(df=df2,map="map2", plot.title='')
    figure_obj[[eu]]$datasets[[name]]$map2 = map2
    
    sp = hexBinRegPlot(df=df2, dataset=name)
    figure_obj[[eu]]$datasets[[name]]$sp = sp
  }






########################################################################################
###### ~~~~~~~~~~~~~~~~~~~~~~~~ Arrange panels ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###############################################################################################
nri_obj=figure_obj[[eu]]$datasets[['NRI']]
nlcd_obj=figure_obj[[eu]]$datasets[['NLCD']]
nass_obj=figure_obj[[eu]]$datasets[['NASS']]

c1 = ggarrange(nri_obj$map1, nlcd_obj$map1, nass_obj$map1, ncol = 1, nrow = 3, common.legend = TRUE, legend="bottom")
c2 = ggarrange(nri_obj$map2, nlcd_obj$map2, nass_obj$map2, ncol = 1, nrow = 3, common.legend = TRUE, legend="bottom")
c3 = ggarrange(nri_obj$sp, nlcd_obj$sp, nass_obj$sp, ncol = 1, nrow = 3)

map = ggarrange(c1, c2, c3, ncol = 3, nrow = 1)

fileout = paste0('I:\\projects\\usxp\\series\\s35\\deliverables\\scatterplot\\figures\\test.png')
ggplot2::ggsave(fileout, width = 34, height = 25, dpi = 500)

























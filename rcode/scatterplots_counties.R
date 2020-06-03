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
figure_json = 'C:\\Users\\Bougie\\Desktop\\scripts\\projects\\usxp\\stages\\deliverables\\scatterplots\\json\\figure_json.json'
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


###################################################################################################
####################################################################################################
###############  Arguments #####################################################################
###################################################################################################
####################################################################################################

eu='counties'
current_map='diff_perc_year_eu'
current_scatterplot='raw_acres'

fp_map = figure_obj[[eu]][['maps']][[current_map]]
fp_sp = figure_obj[[eu]][['scatterplots']][[current_scatterplot]]

fp = append(fp_map, fp_sp)

log_trans_array = c("no_log", "log10", "log10_plus1")
log_trans = log_trans_array[1]

x = fp$log_trans[[log_trans]][1]
y = fp$log_trans[[log_trans]][2]


output_file = paste(fp$name_map, fp$name_sp, log_trans, sep='_')






######################################################################
##########  create dataframe ##########################################
#########################################################################


createDF <- function(con, query, df_usxp, sat){
  
  ########### create intial DF and join dataframes together ##################
  
  ####read in dataset from postgres
  df_init <- st_read(con, query=query)
  
  ####join usxp dataframe together specific dataframe
  df  <- df_usxp %>% inner_join(df_init , by = "atlas_stco")
  
  
  ########## create columns ####################################
  
  ####create difference of acres per year between dataframes
  df$diff_acres = (df$acres_usxp_per_year-df$acres_per_year)
  
  # ####the difference per year relative to acres of usxp per year
  df$diff_perc = (df$diff_acres/(df$acres_usxp_per_year)*100)
  # 
  # ####the difference per year relative to acres of usxp per year
  # df$perc_year = (df$acres_usxp_per_year/(df$acres_per_year))
  
  
  ####the difference per year relative to acres of usxp per year
  df$perc_year_eu = (df$acres_per_year_per_eu/(df$acres_usxp_per_year_per_eu)*100)
  
  
  df$diff_perc_year_eu = df$acres_usxp_per_year_per_eu - df$acres_per_year_per_eu 
  
  ####remove records that have negative infinity in the per column
  # df <- df[is.finite(df$perc_year),]
  
  
  
  ########### apply saturation for map figure ##################################
  
  #-------apply saturation to the column of interest
  df$col_sat = df[[fp$col]]
  df$col_sat[df$col_sat <= sat[1]] <- sat[1]
  df$col_sat[df$col_sat >= sat[2]] <- sat[2]
  
  
  
  ########### transform data for scatterplot figure #################################
  ########### transform data for scatterplot figure #################################
  df$x = df[[fp$x]]
  df$y = df[[fp$y]]
  
  
  df$x_no_log = df[[fp$x]]
  df$y_no_log = df[[fp$y]]
  
  df$x_log10 = log(df[[fp$x]], base=10)
  df$y_log10 = log(df[[fp$y]], base=10)

  
  df$x_log10_plus1 = log((df[[fp$x]]+1), base=10)
  df$y_log10_plus1 = log((df[[fp$y]]+1), base=10)
  
  
  


  # linearMod <- lm(y_log10 ~ x_log10, data=df)
  # print(linearMod)
  
  

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

createMap <- function(df, current_col, plot.title){

  
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
    
  geom_sf(data= df, aes(fill = current_col)) + 
    
    
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
    legend.key.width = unit(1,"cm"),
    legend.key.height = unit(2,"cm"),
    
    # plot.background = element_rect(fill = 'green', color = 'red'),
    plot.background = element_rect(fill = NA, color = NA),
    plot.title = element_text(size= 35),
    plot.margin = unit(c(t=1, r=4, b=1, l=0), "cm")
  ) + 
 
  guides(fill = guide_colourbar(title=fp$legend.title)) +
  # 
  # coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") + 
  scale_fill_gradientn(colors = rev(brewer.pal(11,"RdBu")))
  return(d)
}


###################################################################################
##### hexbin function ######################################################
###################################################################################

hexBinRegPlot_2 = function(df, dataset){
  my.formula <- y ~ x
  
  
  # p <- ggplot(data=df,aes(x = .data[[fp$log_trans[1]]], y = .data[[fp$log_trans[2]]])) +
  # p <- ggplot(data=df,aes(x = .data[[fp$x_no_log]], y = .data[[fp$y_no_log]])) +
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
    # stat_poly_eq(formula = my.formula,
    #              aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    #              size=9,
    #              parse = TRUE) +
    # scale_x_continuous(trans='log10') +
    # scale_y_continuous(trans='log10') +
    scale_fill_gradientn(colors = rev(brewer.pal(11,"Spectral")))
  
  
  p
  return(p)
}






###############################################################################
########## USXP ##############################################################
########################################################################
query_usxp_exp = "SELECT
b.atlas_stco,
b.atlas_name,
(a.acres/8) as acres_usxp_per_year,
((a.acres/8)/b.acres_calc * 100) as acres_usxp_per_year_per_eu
--b.wkb_geometry as geom
FROM
zonal_stats.s35_mtr_counties as a INNER JOIN spatial.counties_102003 as b
USING(atlas_stco)
WHERE label in ('3')"
df_usxp_exp <- dbGetQuery(con_usxp, query_usxp_exp)




query_usxp_net = "SELECT
county.atlas_stco,
county.atlas_name,
(mtr3.acres - mtr4.acres)/8 as acres_usxp_per_year,
(((mtr3.acres - mtr4.acres)/8 )/county.acres_calc * 100) as acres_usxp_per_year_per_eu
--county.wkb_geometry as geom
FROM 

(SELECT 
t1.atlas_stco,
t1.acres as acres
FROM 
zonal_stats.s35_mtr_counties as t1 
WHERE label in ('3')) as mtr3


INNER JOIN


(SELECT 
t1.atlas_stco, 
t1.acres as acres
FROM 
zonal_stats.s35_mtr_counties as t1 
WHERE label in ('4')) as mtr4


USING(atlas_stco)

INNER JOIN
spatial.counties_102003 as county USING(atlas_stco)"

df_usxp_net <- dbGetQuery(con_usxp, query_usxp_net)





###############################################################################
########## NRI ##############################################################
########################################################################

query_nri =  "SELECT
b.atlas_name,
b.atlas_stco,
a.expansion as acres,
a.expansion/8 as acres_per_year,
((a.expansion/8)/b.acres_calc * 100) as acres_per_year_per_eu,
b.acres_calc,
b.wkb_geometry as geom
FROM main.nri_2015_analysis as a INNER JOIN 
spatial.counties_102003 as b USING(atlas_stco)
WHERE a.expansion IS NOT NULL"

df_init = st_read(con_nri ,query=query_nri)

# df <- df_nri
# df$x_log10 = log(df[[fp$x]], base=10)
# df$y_log10 = log(df[[fp$y]], base=10)
# df[sapply(df, is.infinite)] <- NA
df_nri = createDF(con=con_nri, query=query_nri, df_usxp=df_usxp_exp, sat=fp$sat$nri)

###perform a qaqc of the dataframe
qaqc(df_nri, 'df_nri')

#### create instance of a map  #############
d_nri = createMap(df_nri, df_nri$acres_usxp_per_year_per_eu, plot.title='NRI')

#### create instance of a map  #############
d_nri_rel = createMap(df_nri, df_nri$col_sat, plot.title='NRI')

###create instance of a hex bin  ################
p_nri = hexBinRegPlot_2(df_nri, dataset='NRI')

h_nri = ggplot(data=df_nri, aes(col_sat)) + geom_histogram() + theme(legend.text=element_text(size=20))
h_nri

###############################################################################
########## NLCD ##############################################################
########################################################################

####create dataframe #######################
query_nlcd = "SELECT
b.atlas_name,
b.atlas_stco,
a.acres as acres,
a.acres/8 as acres_per_year,
((a.acres/8)/b.acres_calc * 100) as acres_per_year_per_eu,
b.acres_calc,
b.wkb_geometry as geom
FROM
choropleths.combine_nlcd08_16_histo as a INNER JOIN
spatial.counties_102003 as b
USING(atlas_stco)
WHERE a.label = '4'"



df_nlcd = createDF(con=con_usxp_deliverables, df_usxp=df_usxp_exp, query=query_nlcd, sat=fp$sat$nlcd)
###perform a qaqc of the dataframe
qaqc(df_nlcd, 'df_nlcd')

### create instance of a map  #############
d_nlcd = createMap(df_nlcd, df_nlcd$acres_per_year_per_eu, plot.title='NLCD')

#### create instance of a map  #############
d_nlcd_rel = createMap(df_nlcd, df_nlcd$col_sat, plot.title='')

###create instance of a hex bin  ################
p_nlcd = hexBinRegPlot_2(df_nlcd, dataset='NLCD')
p_nlcd

h_nlcd = ggplot(data=df_nlcd, aes(col_sat)) + geom_histogram() + theme(legend.text=element_text(size=20))
h_nlcd







#################################################################################
#####################  NASS #####################################################
#################################################################################

####create dataframe #######################
query_nass="SELECT
b.atlas_name,
b.atlas_stco,
a.diff/10 as acres_per_year,
((a.diff/10)/b.acres_calc) * 100 as acres_per_year_per_eu,
b.acres_calc,
b.wkb_geometry as geom
FROM ag_census.ag_census_expansion as a INNER JOIN
spatial.counties_102003 as b USING(atlas_stco)"

df_init = st_read(con_nass ,query=query_nass)
df_nass = createDF(con=con_nass, df_usxp=df_usxp_net, query=query_nass,sat=fp$sat$nass)

###perform a qaqc of the dataframe
qaqc(df_nass, 'df_nass')

### create instance of a map  #############
d_nass = createMap(df_nass, df_nass$acres_per_year_per_eu, plot.title='NASS')

### create instance of a map  #############
d_nass_rel = createMap(df_nass, df_nass$col_sat, plot.title='NASS')

###create instance of a hex bin  ################
p_nass = hexBinRegPlot_2(df_nass, dataset='NASS')
p_nass

h_nass = ggplot(data=df_nass, aes(col_sat)) + geom_histogram() + theme(legend.text=element_text(size=20))
h_nass






#################################################################################
##########  Create layout ######################################################
#######################################################################################


# ###create a matrix that will be filled with the plots above
# lay <- rbind(c(rep(1:2, each=4)),
#              c(rep(3:4, each=4)),
#              c(rep(5:6, each=4)))

# ###create a matrix that will be filled with the plots above
# lay <- rbind(c(rep(1:3, times = c(4, 4, 3))),
#              c(rep(4:6, times = c(4, 4, 3))),
#              c(rep(7:9,times = c(4, 4, 3))))
# 
# 
# print(lay)


c1 = ggarrange(d_nri, d_nlcd, d_nass, ncol = 1, nrow = 3)
c2 = ggarrange(d_nri, d_nlcd, d_nass, ncol = 1, nrow = 3)
c3 = ggarrange(d_nri, d_nlcd, d_nass, ncol = 1, nrow = 3)

map = ggarrange(c1, c2, c3, ncol = 3, nrow = 1, align = 'hv')
legend = ggarrange(legend_panels[[1]], legend_panels[[2]],legend_panels[[3]], ncol = 1, nrow = 3, align = 'hv')




final = ggarrange(map,legend, ncol = 2, nrow = 1, widths = c(4,1), align = 'hv')
fileout = paste0('I:\\projects\\usxp\\series\\s35\\deliverables\\scatterplot\\figures\\test.png')
ggplot2::ggsave(fileout, width = 34, height = 25, dpi = 500)

#
# #merge all three plots within one grid (and visualize this)

# margin = theme(plot.margin = unit(c(2,2,2,2), "cm"))
# # g <- arrangeGrob(d_nri,p_nri,h_nri,d_nlcd,p_nlcd,h_nlcd,d_nass,p_nass,h_nass, layout_matrix = lay)
# g <- arrangeGrob(d_nri, d_nri_rel, p_nri, d_nlcd, d_nlcd_rel, p_nlcd, d_nass, d_nass_rel, p_nass, layout_matrix = lay)
# 
# 
# fileout = paste0('I:\\projects\\usxp\\series\\s35\\deliverables\\scatterplot\\figures\\counties_', output_file, '.png')
# # ggplot2::ggsave(fileout, width = 34, height = 25, dpi = 500)
# ggplot2::ggsave(fileout, width = 27, height = 25, dpi = 500, g)





















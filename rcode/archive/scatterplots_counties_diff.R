library(ggplot2)
library(maps)
library(rgdal)# R wrapper around GDAL/OGR
library(sp)
require("RPostgreSQL")
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

library(dplyr)

library(gridBase)
library(grid)
library(gridSVG)
library(gridExtra) #load Grid


library(extrafont)




library(ggplot2)
library(maps)
library(rgdal)# R wrapper around GDAL/OGR
library(sp)
library(plyr)
# library(dplyr)
library(viridis)
library(scales)
require(RColorBrewer)
library(glue)
# library(ggpubr)
library(cowplot)
library(RPostgreSQL)
library(postGIStools)
#
#
#
library(rasterVis)

library(grid)
library(scales)
library(viridis)  # better colors for everyone
library(ggthemes) # theme_map()

library(ggpmisc)

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


###get postgres connections ############################
user <- "mbougie"
host <- '144.92.235.105'
port <- '5432'
password <- 'Mend0ta!'

con_nass <- dbConnect(PostgreSQL(), dbname = 'nass', user = user, host = host, port=port, password = password)
con_nri <- dbConnect(PostgreSQL(), dbname = 'nri', user = user, host = host, port=port, password = password)
con_usxp_deliverables <- dbConnect(PostgreSQL(), dbname = 'usxp_deliverables', user = user, host = host, port=port, password = password)
con_usxp <- dbConnect(PostgreSQL(), dbname = 'usxp', user = user, host = host, port=port, password = password)


query_counties =  "select atlas_stco,acres_calc FROM spatial.counties"

df_counties <- dbGetQuery(con_usxp_deliverables, query_counties)
#### bring in shapefile for context in map ##################################
states <- readOGR(dsn = "H:\\e_drive\\data\\general\\sf", layer = "states")
counties <- readOGR(dsn = "H:\\e_drive\\data\\general\\sf", layer = "counties")
counties@data  <- counties@data %>% mutate(ATLAS_STCO = tolower(ATLAS_STCO))

#### create df_usxp_exp dataframe ###########################
query_usxp_exp =  "SELECT 
                t1.atlas_stco, 
                (t1.acres/8) as acres_usxp_per_year
                FROM 
                zonal_stats.s35_mtr_counties as t1 
                WHERE label in ('3')"
df_usxp_exp <- dbGetQuery(con_usxp, query_usxp_exp)




######################################################################
##########  create dataframe ##########################################
#########################################################################

createDF <- function(con, query, sat){

  df <- dbGetQuery(con, query)
  
  ###join with usxp_exp
  df <- df_usxp_exp %>% inner_join(df, by = "atlas_stco")
  df$diff = (df$acres_usxp_per_year-df$acres_per_year)
  
  ####
  # df_nri$perc = (df_nri$diff/(df_nri$acres_usxp_per_year+0.00001)*100)
  df$perc = (df$diff/(df$acres_usxp_per_year)*100)
  
  ### reomve records that have negaitve infinity in the per column
  # df_nlcd = na.omit(df_nlcd)
  df <- df[is.finite(df$perc),]
 
  
  
   print (table(df$perc))
  plot(df$perc)
  

  #-------saturate
  df$diff_sat = df$diff
  df$diff_sat[df$diff_sat <= sat[1]] <- sat[1]
  df$diff_sat[df$diff_sat >= sat[2]] <- sat[2]
  
  
  
  df$perc_sat = df$perc
  df$perc_sat[df$perc_sat <= sat[1]] <- sat[1]
  df$perc_sat[df$perc_sat >= sat[2]] <- sat[2]
  
  
  
  
  
  # print (table(df$perc_sat))
  # plot(df$perc_sat)
  # hist(df$perc_sat,breaks = 50)
  # summary(df$perc_sat)
  

  df$ATLAS_STCO = df$atlas_stco
  
  return(df)
}



##################################################################
#### map function #########################################
##################################################################

createMap <- function(df, plot.title){
  mapa <- merge(counties, df)

  mapa.df <- fortify(mapa)
  mapa@data$id <- rownames(mapa@data)
  
  #merge the attributes of mapa@data to the fortified dataframe with id column
  mapa.df <- join(mapa.df, mapa@data, by="id")
  
  
  d = ggplot() + 
    
  geom_polygon(
      data=states,
      aes(x=long,y=lat,group=group),
      fill='#7e7e7e'
      # fill='red'
  ) +
    

  
    ### main dataframe ###########
  geom_polygon(
    data= mapa.df,
    aes(y=lat, x=long, group=group, fill = diff_sat)
  ) + 
    
    
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
 
  guides(fill = guide_colourbar(title="Acres difference")) +
  # 
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") + 
  scale_fill_gradientn(colors = rev(brewer.pal(11,"RdBu")))
  return(d)
}


###################################################################################
##### hexbin function ######################################################
###################################################################################
hexBinRegPlot_2 = function(df, labs.y){
my.formula <- y ~ x
  p <- ggplot(data=df,aes(x = acres_usxp_per_year, y = acres_per_year)) +
    # geom_point() +
    geom_hex(bins = 35) +
    theme_bw()+
    guides(fill = guide_colourbar(ticks.colour = NA,
                                  frame.colour = "black",
                                  title="Count"))+
    
    labs(y=labs.y, x = "USXP acres converted per year (log10)") + 
    
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
    scale_x_continuous(trans='log10') +
    scale_y_continuous(trans='log10') +
    scale_fill_gradientn(colors = rev(brewer.pal(11,"Spectral")))
  
  
  p
  return(p)
}





###############################################################################
########## NRI ##############################################################
########################################################################



####create dataframe #######################
query_nri =  "SELECT
              'nri' as dataset,
              atlas_stco,
              expansion/8 as acres_per_year
              FROM main.nri_2015_analysis as a
              WHERE expansion IS NOT NULL"

df_nri = createDF(con=con_nri, query=query_nri, sat=c(-2000, 2000))


#### create instance of a map  #############
d_nri = createMap(df_nri, plot.title='NRI')
d_nri

###create instance of a hex bin  ################
p_nri = hexBinRegPlot_2(df_nri, labs.y='NRI acres converted per year (log10)')
# p_nri

h_nri = ggplot(data=df_nri, aes(diff_sat)) + geom_histogram(bins = 1000) + theme(legend.text=element_text(size=20))


###############################################################################
########## NLCD ##############################################################
########################################################################

####create dataframe #######################
query_nlcd =  "SELECT
               'nlcd' as dataset,
              atlas_stco,
              a.acres/8 as acres_per_year
              FROM
              choropleths.combine_nlcd08_16_histo as a
              WHERE a.label = '4' AND a.acres IS NOT NULL"


df_nlcd = createDF(con=con_usxp_deliverables, query=query_nlcd, sat=c(-500, 500))
# df_nlcd = na.omit(df_nlcd)
# df_test <- df_nlcd[is.finite(df_nlcd$perc),]

#### create instance of a map  #############
d_nlcd = createMap(df_nlcd, plot.title='NLCD')
d_nlcd

###create instance of a hex bin  ################
p_nlcd = hexBinRegPlot_2(df_nlcd, labs.y='NLCD acres converted per year (log10)')
p_nlcd



h_nlcd = ggplot(data=df_nlcd, aes(diff_sat)) + geom_histogram() + theme(legend.text=element_text(size=20))
h_nlcd







#################################################################################
#####################  NASS #####################################################
#################################################################################

####create dataframe #######################
query_nass =  "SELECT
              'nass' as dataset,
              atlas_stco,
              a.diff/10 as acres_per_year
              FROM ag_census.ag_census_expansion as a
              WHERE a.diff IS NOT NULL"


df_nass = createDF(con=con_nass, query=query_nass, sat=c(-2000, 2000))

#### create instance of a map  #############
d_nass = createMap(df_nass, plot.title='NASS')
d_nass

###create instance of a hex bin  ################
p_nass = hexBinRegPlot_2(df_nass, labs.y='NASS acres converted per year (log10)')
p_nass



h_nass = ggplot(data=df_nass, aes(diff_sat)) + geom_histogram() + theme(legend.text=element_text(size=20))
h_nass






#################################################################################
##########  Create layout ######################################################
#######################################################################################


# ###create a matrix that will be filled with the plots above
# lay <- rbind(c(rep(1:2, each=4)),
#              c(rep(3:4, each=4)),
#              c(rep(5:6, each=4)))

# ###create a matrix that will be filled with the plots above
lay <- rbind(c(rep(1:2, times = c(4, 3))),
             c(rep(3:4, times = c(4, 3))),
             c(rep(5:6,times = c(4, 3))))


print(lay)

#
# #merge all three plots within one grid (and visualize this)

# margin = theme(plot.margin = unit(c(2,2,2,2), "cm"))
# g <- arrangeGrob(d_nri,p_nri,h_nri,d_nlcd,p_nlcd,h_nlcd,d_nass,p_nass,h_nass, layout_matrix = lay)
g <- arrangeGrob(d_nri,p_nri,d_nlcd,p_nlcd,d_nass,p_nass, layout_matrix = lay)


fileout = paste0('I:\\projects\\usxp\\series\\s35\\deliverables\\scatterplot\\acatterplot_diff.png')
# ggplot2::ggsave(fileout, width = 34, height = 25, dpi = 500)
ggplot2::ggsave(fileout, width = 27, height = 25, dpi = 500, g)














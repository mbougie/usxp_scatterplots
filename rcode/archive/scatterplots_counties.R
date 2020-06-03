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




###############################################################################
########## NRI ##############################################################
########################################################################
query_nri =  "SELECT 
              'nri' as dataset,
              atlas_stco,
              expansion/8 as acres_per_year
              FROM main.nri_2015_analysis as a
              WHERE expansion IS NOT NULL"

df_nri_init <- dbGetQuery(con_nri, query_nri)

###join with usxp_exp
df_nri <- df_usxp_exp %>% inner_join(df_nri_init, by = "atlas_stco")
df_nri$diff = (df_nri$acres_usxp_per_year-df_nri$acres_per_year)
# df_nri$perc = (df_nri$diff/(df_nri$acres_usxp_per_year+0.00001)*100)
df_nri$perc = (df_nri$diff/(df_nri$acres_usxp_per_year)*100)
print (table(df_nri$perc))
plot(df_nri$perc)


##clip and saturate the map
#-------clip
# r_df$value[r_df$value<fp$raster.value.clip] <- NA
#-------saturate
df_nri$perc_sat = df_nri$perc
df_nri$perc_sat[df_nri$perc_sat<=-100] <- -100
print (table(df_nri$perc_sat))
plot(df_nri$perc_sat)
hist(df_nri$perc_sat,breaks = 50)
summary(df_nri$perc_sat)




df_nri$perc_log10 <- df_nri$perc * log(df_nri$perc, 10)




df_nri$ATLAS_STCO = df_nri$atlas_stco
mapa_nri <- merge(counties, df_nri)
# # mapa@data <- na.omit(mapa@data)
# print(summary(mapa$diff))
# mapa$fill = cut(mapa$diff, c(-20000, -1000, -500, -250, 0, 250, 500, 1000, 10000))
# print (table(mapa$fill))


mapa_nri.df <- fortify(mapa_nri)
mapa_nri@data$id <- rownames(mapa_nri@data)

#merge the attributes of mapa@data to the fortified dataframe with id column
mapa_nri.df <- join(mapa_nri.df, mapa_nri@data, by="id")



# mapa$fill = cut(mapa$current_field, fp$bin_breaks)
# print(summary(mapa.df$diff))
# mapa.df$fill = cut(mapa.df$diff,  c(-20000, -1000, -500, -250, 0, 250, 500, 1000, 10000))
# print (table(mapa.df$fill))


#### create map
d_nri = ggplot() + 
  
geom_polygon(
    data=states,
    aes(x=long,y=lat,group=group),
    fill='#7e7e7e'
    # fill='red'
) +
  ### county boundary strokes ###########
geom_polygon(
  data=mapa_nri.df,
  aes(y=lat, x=long, group=group, fill = perc_sat)
)
d_nri

# d + scale_fill_manual(values = brewer.pal(8, name="PRGn"))
# d + scale_fill_gradientn(colors = rev(brewer.pal(11,"Spectral")), trans = "log")
d_nri <- d_nri + scale_fill_gradientn(colors = rev(brewer.pal(11,"RdBu"))) + theme(legend.text=element_text(size=20))
d_nri








# DATA is your data frame
# XVAR is the name of your x-axis variable
# YVAR is the name of your y-axis variable
# LIMITS is a two element vector with the (i) min and (ii) max values of the legend color bar 
# BREAKS is a vector containing the values of the lengend color bar at which you want labels
# LABELS is a vector containing the labels that correspond with BREAKS
# MAIN is the title of the plot (see the attached example in which MAIN is either "Corn", "Soybeans", or "Wheat")
# XLAB is the text label for the x-axis
# YLAB is the text label for the y-axis

########################################################################
#### define hexbin function ############################################
########################################################################
hexBinRegPlot = function(DATA, XVAR, YVAR, LIMITS, BREAKS, LABELS, MAIN, XLAB, YLAB){
  print(data)
  p = ggplot(DATA, aes(XVAR, YVAR)) +
    geom_hline(yintercept=0, size = 1)+
    geom_hex(bins = 35)+
    geom_smooth(method = "lm", se = F)+
    scale_fill_gradientn(colors = rev(brewer.pal(11,"Spectral")),
                         name = 'Count',
                         limits = LIMITS,
                         breaks = BREAKS,
                         labels = LABELS,
                         oob = squish)+
    theme_bw()+
    labs(title = MAIN,
         x = XLAB,
         y = YLAB)+
    guides(fill = guide_colourbar(ticks.colour = NA,
                                  frame.colour = "black",
                                  barwidth = 0.5))+
    theme(panel.border= element_rect(size = 1),
          axis.title.x = element_text(face = 'bold'),
          axis.title.y = element_text(face = 'bold'),
          axis.title = element_text(size = 8))
  
  return(p)
}






















hexBinRegPlot_2 = function(df){
my.formula <- y ~ x
p_nri <- ggplot(data=df,aes(x = acres_usxp_per_year, y = acres_per_year)) +
  # geom_point() +
  geom_hex(bins = 35) +
  theme_bw()+
  guides(fill = guide_colourbar(ticks.colour = NA,
                                frame.colour = "black",
                                barwidth = 0.9))+
  theme(panel.border= element_rect(size = 2),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        axis.title = element_text(size = 8),
        legend.text=element_text(size=15),
        legend.key.width = unit(0.35, "cm"), 
        legend.key.height = unit(1.0, "cm"))+
  geom_abline(slope = 1, intercept = 0, colour="black",size=2) +
  geom_smooth(method = "lm", se = FALSE, formula = my.formula, size=2) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  scale_fill_gradientn(colors = rev(brewer.pal(11,"Spectral")))


p_nri
return(p_nri)
}


p_nri = hexBinRegPlot(DATA=df_nri, 
                      XVAR=df_nri$acres_usxp_per_yea, 
                      YVAR=df_nri$acres_per_year, 
                      LIMITS = c(100,500),
                      BREAKS = c(10,20,30,40,50),
                      LABELS = c('10','20','30','40','50'),
                      MAIN='rerere', 
                      XLAB='xlab', 
                      YLAB='ylab')

p_nri

DATA=df_nri
XVAR=df_nri$acres_usxp_per_year
YVAR=df_nri$acres_per_year
# LIMITS = c('red','blue')
LIMITS = c(100,500)
BREAKS = c(10,20,30,40,50)
LABELS = c('10','20','30','40','50')
MAIN='NRI'
XLAB='xlab'
YLAB='ylab'





p_nri_2 = ggplot(data=DATA, aes(x=XVAR, y=YVAR)) +
geom_hline(yintercept=0, size = 1)+
geom_hex(bins = 35)+
geom_smooth(method = "lm", se = F)+
scale_fill_gradientn(colors = rev(brewer.pal(11,"Spectral")),
                     name = 'Count',
                     limits = LIMITS,
                     breaks = BREAKS,
                     labels = LABELS,
                     oob = squish)+
scale_x_continuous(trans='log10') +
scale_y_continuous(trans='log10') +
theme_bw()+
labs(title = MAIN,
     x = XLAB,
     y = YLAB)+
guides(fill = guide_colourbar(ticks.colour = NA,
                              frame.colour = "black",
                              barwidth = 0.5))+
theme(panel.border= element_rect(size = 1),
      axis.title.x = element_text(face = 'bold'),
      axis.title.y = element_text(face = 'bold'),
      axis.title = element_text(size = 8))

p_nri_2

p_nri = hexBinRegPlot_2(df_nri)
p_nri

#### create scatterplot
my.formula <- y ~ x
p_nlcd <- ggplot(data=df_nlcd,aes(x = acres_usxp_per_year, y = acres_per_year)) +
  # geom_point() +
  geom_hex(bins = 20) + 
  geom_abline(slope = 1, intercept = 0, colour="red",size=1) + 
  geom_smooth(method = "lm", se = FALSE, formula = my.formula) + 
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  scale_fill_gradientn(colors = rev(brewer.pal(11,"RdBu")))
p_nlcd = p_nlcd + theme(legend.text=element_text(size=20))

p_nlcd = hexBinRegPlot_2(df_nlcd)
p_nlcd




# h_nri = ggplot(data=df_nri, aes(perc_sat)) + geom_histogram(bins = 30) + theme(legend.text=element_text(size=20))




###############################################################################
########## NLCD ##############################################################
########################################################################


query_nlcd =  "SELECT
                  'nlcd' as dataset,
                  atlas_stco,
                  a.acres/8 as acres_per_year
                  FROM
                  choropleths.combine_nlcd08_16_histo as a
                  WHERE a.label = '4' AND a.acres IS NOT NULL"

df_nlcd_init <- dbGetQuery(con_usxp_deliverables, query_nlcd)


###join with usxp_exp
df_nlcd <- df_usxp_exp %>% inner_join(df_nlcd_init, by = "atlas_stco")
df_nlcd$diff = (df_nlcd$acres_usxp_per_year-df_nlcd$acres_per_year)

#### why did I do a fundge factor here !!!!!!!!!!!!!!
# df_nlcd$perc = (df_nlcd$diff/(df_nlcd$acres_usxp_per_year+0.00001)*100)
df_nlcd$perc = (df_nlcd$diff/(df_nlcd$acres_usxp_per_year)*100)
print (table(df_nlcd$perc))
plot(df_nlcd$perc)


##clip and saturate the map
#-------clip
# r_df$value[r_df$value<fp$raster.value.clip] <- NA
#-------saturate
df_nlcd$perc_sat = df_nlcd$perc
df_nlcd$perc_sat[df_nlcd$perc_sat<=-100] <- -100
print (table(df_nlcd$perc_sat))
plot(df_nlcd$perc_sat)
hist(df_nlcd$perc_sat,breaks = 50)
summary(df_nlcd$perc_sat)


df_nlcd$ATLAS_STCO = df_nlcd$atlas_stco
mapa <- merge(counties, df_nlcd)
# # mapa@data <- na.omit(mapa@data)
# print(summary(mapa$diff))
# mapa$fill = cut(mapa$diff, c(-20000, -1000, -500, -250, 0, 250, 500, 1000, 10000))
# print (table(mapa$fill))


mapa.df <- fortify(mapa)
mapa@data$id <- rownames(mapa@data)

#merge the attributes of mapa@data to the fortified dataframe with id column
mapa.df <- join(mapa.df, mapa@data, by="id")



# mapa$fill = cut(mapa$current_field, fp$bin_breaks)
# print(summary(mapa.df$diff))
# mapa.df$fill = cut(mapa.df$diff,  c(-20000, -1000, -500, -250, 0, 250, 500, 1000, 10000))
# print (table(mapa.df$fill))


#### create map
d_nlcd = ggplot() + 
  
  geom_polygon(
    data=states,
    aes(x=long,y=lat,group=group),
    fill='#7e7e7e'
    # fill='red'
  ) +
  ### county boundary strokes ###########
geom_polygon(
  data=mapa.df,
  aes(y=lat, x=long, group=group, fill = perc_sat)
)
d_nlcd

# d + scale_fill_manual(values = brewer.pal(8, name="PRGn"))
# d + scale_fill_gradientn(colors = rev(brewer.pal(11,"Spectral")), trans = "log")
d_nlcd <- d_nlcd + scale_fill_gradientn(colors = rev(brewer.pal(11,"RdBu"))) + theme(legend.text=element_text(size=20))
d_nlcd







#### create scatterplot
my.formula <- y ~ x
p_nlcd <- ggplot(data=df_nlcd,aes(x = acres_usxp_per_year, y = acres_per_year)) +
  # geom_point() +
  geom_hex(bins = 20) + 
  geom_abline(slope = 1, intercept = 0, colour="red",size=1) + 
  geom_smooth(method = "lm", se = FALSE, formula = my.formula) + 
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  scale_fill_gradientn(colors = rev(brewer.pal(11,"RdBu")))
p_nlcd = p_nlcd + theme(legend.text=element_text(size=20))



h_nlcd = ggplot(data=df_nlcd, aes(perc_sat)) + geom_histogram() + theme(legend.text=element_text(size=20))
h_nlcd


#################################################################################
#####################  NASS #####################################################
#################################################################################


query_nass = "SELECT
              'nass' as dataset,
              atlas_stco,
              a.diff/10 as acres_per_year
              FROM ag_census.ag_census_expansion as a
              WHERE a.diff IS NOT NULL"


df_nass_init <- dbGetQuery(con_nass, query_nass)


###join with usxp_exp
df_nass <- df_usxp_exp %>% inner_join(df_nass_init, by = "atlas_stco")
df_nass$diff = (df_nass$acres_usxp_per_year-df_nass$acres_per_year)
# df_nass$perc = (df_nass$diff/(df_nass$acres_usxp_per_year+0.00001)*100)
df_nass$perc = (df_nass$diff/(df_nass$acres_usxp_per_year)*100)
print (table(df_nass$perc))
plot(df_nass$perc)
hist(df_nass$perc,breaks = 50)

##clip and saturate the map
#-------clip
# r_df$value[r_df$value<fp$raster.value.clip] <- NA
#-------saturate
df_nass$perc_sat = df_nass$perc
df_nass$perc_sat[df_nass$perc_sat<=-500] <- -500
df_nass$perc_sat[df_nass$perc_sat>=500] <- 500
print (table(df_nass$perc_sat))
plot(df_nass$perc_sat)
hist(df_nass$perc_sat,breaks = 50)
summary(df_nass$perc_sat)


df_nass$ATLAS_STCO = df_nass$atlas_stco
mapa <- merge(counties, df_nass)
# # mapa@data <- na.omit(mapa@data)
# print(summary(mapa$diff))
# mapa$fill = cut(mapa$diff, c(-20000, -1000, -500, -250, 0, 250, 500, 1000, 10000))
# print (table(mapa$fill))


mapa.df <- fortify(mapa)
mapa@data$id <- rownames(mapa@data)

#merge the attributes of mapa@data to the fortified dataframe with id column
mapa.df <- join(mapa.df, mapa@data, by="id")



# mapa$fill = cut(mapa$current_field, fp$bin_breaks)
# print(summary(mapa.df$diff))
# mapa.df$fill = cut(mapa.df$diff,  c(-20000, -1000, -500, -250, 0, 250, 500, 1000, 10000))
# print (table(mapa.df$fill))


#### create map
d_nass = ggplot() + 
  
  geom_polygon(
    data=states,
    aes(x=long,y=lat,group=group),
    fill='#7e7e7e'
    # fill='red'
  ) +
  ### county boundary strokes ###########
geom_polygon(
  data=mapa.df,
  aes(y=lat, x=long, group=group, fill = perc_sat)
)


d_nass

# d + scale_fill_manual(values = brewer.pal(8, name="PRGn"))
# d + scale_fill_gradientn(colors = rev(brewer.pal(11,"Spectral")), trans = "log")
d_nass <- d_nass + scale_fill_gradientn(colors = rev(brewer.pal(11,"RdBu"))) + theme(legend.text=element_text(size=20))
d_nass







#### create scatterplot
my.formula <- y ~ x
p_nass <- ggplot(data=df_nass,aes(x = acres_usxp_per_year, y = acres_per_year)) +
  # geom_point() +
  geom_hex(bins = 20) + 
  geom_abline(slope = 1, intercept = 0, colour="red",size=1) + 
  geom_smooth(method = "lm", se = FALSE, formula = my.formula) + 
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  scale_fill_gradientn(colors = rev(brewer.pal(11,"RdBu")))
p_nass


h_nass = ggplot(data=df_nass, aes(perc_sat)) + geom_histogram()
h_nass




# # ###create a matrix that will be filled with the plots above
# lay <- rbind(c(1,1,1,1,2,2,2,2,3,3,3,3),
#              c(1,1,1,1,2,2,2,2,3,3,3,3),
#              c(4,4,4,4,5,5,5,5,6,6,6,6),
#              c(4,4,4,4,5,5,5,5,6,6,6,6),
#              c(7,7,7,7,8,8,8,8,9,9,9,9),
#              c(7,7,7,7,8,8,8,8,9,9,9,9))


# ###create a matrix that will be filled with the plots above
lay <- rbind(c(rep(1:2, each=4)),
             c(rep(3:4, each=4)),
             c(rep(5:6, each=4)))

print(lay)

#
# #merge all three plots within one grid (and visualize this)

# margin = theme(plot.margin = unit(c(2,2,2,2), "cm"))
# g <- arrangeGrob(d_nri,p_nri,h_nri,d_nlcd,p_nlcd,h_nlcd,d_nass,p_nass,h_nass, layout_matrix = lay)
g <- arrangeGrob(d_nri,p_nri,d_nlcd,p_nlcd,d_nass,p_nass, layout_matrix = lay)


fileout = paste0('C:\\Users\\Bougie\\Desktop\\temp\\test.png')
# ggplot2::ggsave(fileout, width = 34, height = 25, dpi = 500)
ggplot2::ggsave(fileout, width = 34, height = 25, dpi = 500, g)




















# 
# 
# 
# 
# 
# 
# # ####inner join these datafraems
# # df_nass_w_counties <- df_nass %>% full_join(df_counties, by = "atlas_stco")
# # 
# # df_nass_w_counties$perc = (df_nass_w_counties$acres_per_year/df_nass_w_counties$acres_calc * 100)
# 
# 
# 
# 
# 
# 
# 
# 
# ### stacked the expansion only datasets
# df_nri_nass = dplyr::bind_rows(df_nri, df_nlcd)
# 
# # ####inner join these datafraems
# # y_axis_dfs_exp_w_counties <- y_axis_dfs_exp %>% full_join(df_counties, by = "atlas_stco")
# # 
# # y_axis_dfs_exp_w_counties$perc = (y_axis_dfs_exp_w_counties$acres_per_year/y_axis_dfs_exp_w_counties$acres_calc * 100)
# 
# 
# #### create df_usxp_exp dataframe ###########################
# query_usxp_exp =  "SELECT 
#                 t1.atlas_stco, 
#                 (t1.acres/8) as acres_usxp_per_year
#                 FROM 
#                 zonal_stats.s35_mtr_counties as t1 
#                 WHERE label in ('3')"
# df_usxp_exp <- dbGetQuery(con_usxp, query_usxp_exp)
# 
# # ### attach county dataframe so can divide by arces_calc column
# # df_usxp_exp_w_counties <- df_usxp_exp %>% inner_join(df_counties, by = "atlas_stco")
# # df_usxp_exp_w_counties$usxp_perc = (df_usxp_exp_w_counties$acres_usxp_per_year/df_usxp_exp_w_counties$acres_calc * 100)
# 
# 
# 
# ###join datasets together 
# df_exp <- df_usxp_exp %>% inner_join(df_nri_nlcd, by = "atlas_stco")
# 
# # ###join datasets together 
# # df_exp_w_counties <- df_usxp_exp_w_counties %>% inner_join(y_axis_dfs_exp_w_counties, by = "atlas_stco")
# 
# 
# 
# 
# 
# 
# #### create df_usxp_net dataframe ###########################
# query_usxp_net =  "SELECT
#                     mtr3.atlas_stco, 
#                     ((mtr3.acres - mtr4.acres)/8) as acres_usxp_per_year 
#                     FROM 
#                     
#                     (SELECT 
#                     t1.atlas_stco, 
#                     t1.acres as acres
#                     FROM 
#                     zonal_stats.s35_mtr_counties as t1 
#                     WHERE label in ('3')) as mtr3
#                     
#                     
#                     INNER JOIN
#                     
#                     
#                     (SELECT 
#                     t1.atlas_stco, 
#                     t1.acres as acres
#                     FROM 
#                     zonal_stats.s35_mtr_counties as t1 
#                     WHERE label in ('4')) as mtr4
#                     
#                     
#                      USING(atlas_stco)"
# df_usxp_net <- dbGetQuery(con_usxp, query_usxp_net)
# 
# # ### attach county dataframe so can divide by arces_calc column
# # df_usxp_net_w_counties <- df_usxp_net %>% inner_join(df_counties, by = "atlas_stco")
# # df_usxp_net_w_counties$usxp_perc = (df_usxp_net_w_counties$acres_usxp_per_year/df_usxp_net_w_counties$acres_calc * 100)
# 
# 
# 
# 
# 
# ###join datasets together 
# df_net <- df_usxp_net %>% inner_join(df_nass, by = "atlas_stco")
# 
# 
# 
# # ###join datasets together 
# # df_net_w_counties <- df_usxp_net_w_counties %>% inner_join(y_axis_df_nass_w_counties, by = "atlas_stco")
# # 
# # ### remove null values from alalysis (these are the values where NASS had no values for a county)
# # df_net_w_counties = na.omit(df_net_w_counties)
# 
# 
# 
# 
# 
# #### final dataframes #################
# df = dplyr::bind_rows(df_net, df_exp)
# df$diff = (df$acres_usxp_per_year-df$acres_per_year)
# df$ATLAS_STCO = df$atlas_stco
# # df_w_counties <- dplyr::inner_join(counties@data, df, by = c("ATLAS_STCO"="atlas_stco"))
# 
# 
# # df_w_counties = dplyr::bind_rows(df_net_w_counties, df_exp_w_counties)
# 
# # df_w_counties <- dplyr::inner_join(counties@data, df, by = c("ATLAS_STCO"="atlas_stco"))
# # 
# # 
# # ##This function turns a map into a dataframe that can more easily be plotted with ggplot2.
# # mapa.df <- fortify(counties)
# # 
# # #fortify() creates zany attributes so need to reattach the values from intial dataframe
# # #creates a numeric index for each row in dataframe
# # counties@data$id <- rownames(counties@data)
# # 
# # #merge the attributes of mapa@data to the fortified dataframe with id column
# # mapa.df <- join(counties@data, df, by="id")
# # 
# # 
# # 
# # df3 <- dplyr::left_join(df1, df2, by=c("name1" = "name3", "name2" = "name4"))
# 
# 
# mynewspdf <- merge(counties, df)
# 
# 
# df_vv = data.frame(counties@data, df[match(counties@data[,by], df[,by]),])
# 
# 
# my.formula <- y ~ x
# p <- ggplot(data=df,aes(x = acres_usxp_per_year, y = acres_per_year)) +
#   # geom_point() +
#   geom_hex(bins = 20) + 
#   geom_abline(slope = 1, intercept = 0, colour="red",size=1) + 
#   geom_smooth(method = "lm", se = FALSE, formula = my.formula) + 
#   stat_poly_eq(formula = my.formula, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE) +
#   # scale_x_continuous(trans='log') +
#   # scale_y_continuous(trans='log') +
#   scale_fill_gradientn(colors = rev(brewer.pal(11,"Spectral")), trans = "log")
# p + facet_wrap(~dataset, scales = "free")
# 
# 
# # p + geom_text(x = 25, y = 300, label = lm_eqn(yo), parse = TRUE)
# # p + facet_wrap(~metric, scales = "free")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # ##### link to module scripts #####################################################
# # modalpath = 'C:\\Users\\Bougie\\Desktop\\scripts\\modules\\r_maps'
# # source(paste(modalpath, 'legendLabelCreator_bins_v2.R', sep='\\'))
# # 
# # 
# # ##### link to scripts #####################################################
# # rootpath = 'C:\\Users\\Bougie\\Desktop\\scripts\\projects\\usxp\\stages\\deliverables\\choropleths\\rcode'
# # source(paste(rootpath, 'usxp_maps_getDF_bins_v2.R', sep='\\'))
# # source(paste(rootpath, 'choropleth_legends2.R', sep='\\'))
# 
# 
# # ###### link to json files #################################################
# # figure_json = 'C:\\Users\\Bougie\\Desktop\\Gibbs\\scripts\\projects\\usxp\\stages\\deliverables\\json\\figure_json.json'
# # figure_obj<- fromJSON(file=figure_json)
# 
# 
# # getDF <- function(current_obj){
# # 
# #   if(current_obj$dataset == 'nass'){
# # 
# #     # query_nass = "SELECT atlas_stco, perc_conv_county as perc, geom FROM ag_census.ag_census_expansion WHERE perc_conv_county > 0.5"
# #     query_nass = "SELECT atlas_stco, (perc_conv_county/10) as perc, geom FROM ag_census.ag_census_expansion"
# #     df <- get_postgis_query(con_nass, query_nass, geom_name = "geom")
# # 
# # 
# #     #### note: modify the datafreame here for cartography ############
# #     df$perc[df$perc < 0]  <- 0.0001
# # 
# #     ###attach df to specific object in json
# #     return(df)
# #   }
# #   else if(current_obj$dataset == 'nri'){
# #     # query_nri = "SELECT a.fips, a.ratio_expansion * 100 as perc, b.geom FROM main.nri_2015_analysis as a INNER JOIN spatial.counties as b USING(fips) WHERE a.ratio_expansion > 0.005"
# #     query_nri = "SELECT a.fips, (a.ratio_expansion/8) * 100 as perc, b.geom FROM main.nri_2015_analysis as a INNER JOIN spatial.counties as b USING(fips) where a.ratio_expansion IS NOT NULL"
# #     df <- get_postgis_query(con_nri, query_nri, geom_name = "geom")
# # 
# #     ###attach df to specific object in json
# #     return(df)
# #   }
# # 
# #   else if(current_obj$dataset == 'nlcd'){
# # 
#     # query_nlcd="SELECT
#     #             combine_nlcd08_16_histo.atlas_stco,
#     #             ((combine_nlcd08_16_histo.acres/counties.acres_calc)/8)*100 as perc,
#     #             geom
#     #             FROM
#     #             choropleths.combine_nlcd08_16_histo INNER JOIN
#     #             spatial.counties
#     #             ON
#     #             counties.atlas_stco = combine_nlcd08_16_histo.atlas_stco
#     #             WHERE label = '4'"
# #     df <- get_postgis_query(con_usxp_deliverables, query_nlcd, geom_name = "geom")
# # 
# #     return(df)
# #   }
# #   else if(current_obj$dataset == 'usxp'){
# #     query_usxp="SELECT atlas_stco, (perc_conv_county/8) as perc, geom FROM choropleths.s35_perc_conv_county"
# #     df <- get_postgis_query(con_usxp_deliverables, query_usxp, geom_name = "geom")
# # 
# #     ###attach df to specific object in json
# #     return(df)
# #   }
# # }
# # 
# # # query_nass = "SELECT atlas_stco, perc_conv_county as perc, geom FROM ag_census.ag_census_expansion WHERE perc_conv_county > 0.5"
# # query_nass = "SELECT atlas_stco, (perc_conv_county/10) as perc, geom FROM ag_census.ag_census_expansion"
# # df <- get_postgis_query(con_nass, query_nass, geom_name = "geom")
# 
# 
# 
# 
# # 
# # runMain <- function(figure_params){
# # 
# #   merge_ggplotlists <- list()
# # 
# #   for(i in figure_params$panels){
# #     print('start--------------------------------')
# #     df <- getDF(i)
# #     fp = append(figure_params$core, i)
# #     print('*********************** yo ********************************************')
# #     print(fp)
# #     ggplot_object <- createMap(fp, df)
# #     merge_ggplotlists <- append(merge_ggplotlists, list(ggplot_object))
# #   }
# # 
# #   return(merge_ggplotlists)
# # 
# # }
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # #####################################################################
# # ######## run script #################################################
# # #####################################################################
# # parent="main"
# # figure="choropleths"
# # 
# # ###### link to json files #################################################
# # figure_json = 'C:\\Users\\Bougie\\Desktop\\scripts\\projects\\usxp\\stages\\deliverables\\json\\figure_json.json'
# # figure_obj<- fromJSON(file=figure_json)
# # 
# # 
# # ####DEFINE THE FIGURE OBJECT HERE!!!!!!!!!!!!!!!!!!!
# # figure_params = figure_obj[[parent]][[figure]]
# # legend <- createLegend(figure_params)
# # 
# # 
# # 
# # map_panels <- runMain(figure_params)
# # 
# # 
# # 
# # # 
# # # 
# # # 
# # # 
# # # ###create a matrix that will be filled with the plots above
# # lay <- rbind(c(1,1,1,1,2,2,2,2),
# #              c(1,1,1,1,2,2,2,2),
# #              c(3,3,3,3,4,4,4,4),
# #              c(3,3,3,3,4,4,4,4),
# #              c(5,5,5,5,5,5,5,5))
# # #
# # # #merge all three plots within one grid (and visualize this)
# # 
# # # margin = theme(plot.margin = unit(c(2,2,2,2), "cm"))
# # g <- arrangeGrob(map_panels[[1]],map_panels[[2]],map_panels[[3]],map_panels[[4]],legend, layout_matrix = lay)
# # 
# # 
# # 
# # fileout = paste0('I:\\projects\\usxp\\series\\s35\\deliverables\\figures\\main\\',figure_params$core$output)
# # # ggplot2::ggsave(fileout, width = 34, height = 25, dpi = 500)
# # ggplot2::ggsave(fileout, width = 34, height = 25, dpi = 500, g)
# # 
# # 
# # 
























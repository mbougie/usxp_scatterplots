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


query_states =  "select atlas_name as state_name, acres_calc FROM spatial.states"
df_states <- dbGetQuery(con_usxp_deliverables, query_states)



query_nri =  "SELECT 
              'nri' as dataset, 
              state_name, 
              sum(expansion) as acres,
              sum(expansion)/8 as acres_per_year
              FROM main.nri_2015_analysis 
              GROUP BY state_name
              ORDER BY sum(expansion) DESC"

df_nri <- dbGetQuery(con_nri, query_nri)

query_nlcd =  "SELECT
                  'nlcd' as dataset,
                  b.state_name,
                  sum(a.acres) as acres,
                  sum(a.acres)/8 as acres_per_year
                  FROM
                  choropleths.combine_nlcd08_16_histo as a INNER JOIN
                  spatial.counties as b
                  USING(atlas_stco)
                  WHERE a.label = '4'
                  GROUP BY b.state_name"

df_nlcd <- dbGetQuery(con_usxp_deliverables, query_nlcd)







query_nass = "SELECT
              'nass' as dataset,
              c.atlas_name as state_name,
              sum(a.diff) as acres,
              sum(a.diff)/10 as acres_per_year
   
              
              FROM ag_census.ag_census_expansion as a 
              
              
              INNER JOIN
              spatial.counties as b USING(atlas_stco)
              
              INNER JOIN
              spatial.states as c USING(atlas_st)
              
              GROUP BY
              c.atlas_name"
df_nass <- dbGetQuery(con_nass, query_nass)

####inner join these datafraems
y_axis_df_nass_w_states <- df_nass %>% full_join(df_states, by = "state_name")

y_axis_df_nass_w_states$perc = (y_axis_df_nass_w_states$acres_per_year/y_axis_df_nass_w_states$acres_calc * 100)





### stacked the expansion only datasets
y_axis_dfs_exp = dplyr::bind_rows(df_nri, df_nlcd)

####inner join these datafraems
y_axis_dfs_exp_w_states <- y_axis_dfs_exp %>% full_join(df_states, by = "state_name")

y_axis_dfs_exp_w_states$perc = (y_axis_dfs_exp_w_states$acres_per_year/y_axis_dfs_exp_w_states$acres_calc * 100)




query_usxp_exp =  "SELECT
               atlas_name as state_name,
               expansion as acres_usxp,
               expansion/8 as acres_usxp_per_year
               FROM choropleths.s35_perc_conv_states"
df_usxp_exp <- dbGetQuery(con_usxp_deliverables, query_usxp_exp)


### attach county dataframe so can divide by arces_calc column
df_usxp_exp_w_states <- df_usxp_exp %>% inner_join(df_states, by = "state_name")
df_usxp_exp_w_states$usxp_perc = (df_usxp_exp_w_states$acres_usxp_per_year/df_usxp_exp_w_states$acres_calc * 100)





###join datasets together 
df_exp <- df_usxp_exp_w_states %>% inner_join(y_axis_dfs_exp_w_states, by = "state_name")










query_usxp_net = "SELECT
            state_name,
            sum((mtr3.acres - mtr4.acres)/8) as acres_usxp_per_year
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
            spatial.counties USING(atlas_stco)
            
            GROUP BY state_name"

df_usxp_net <- dbGetQuery(con_usxp, query_usxp_net)
df_usxp_net_w_states <- df_usxp_net %>% inner_join(df_states, by = "state_name")
df_usxp_net_w_states$usxp_perc = (df_usxp_net_w_states$acres_usxp_per_year/df_usxp_net_w_states$acres_calc * 100)
 

###join datasets together 
df_net <- df_usxp_net_w_states %>% inner_join(y_axis_df_nass_w_states, by = "state_name")

### remove null values from alalysis (these are the values where NASS had no values for a county)
df_net = na.omit(df_net)

df = dplyr::bind_rows(df_net, df_exp)





# p <- ggplot(data = df, aes(x = x, y = y)) +
#   geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
#   stat_poly_eq(formula = my.formula, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE) +         
#   geom_point()
# p
# 
# 

print(hi)



my.formula <- y ~ x
p <- ggplot(data=df,aes(x = usxp_perc, y = perc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = my.formula) + 
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)
p + facet_wrap(~dataset, scales = "free")


# p + geom_text(x = 25, y = 300, label = lm_eqn(yo), parse = TRUE)
# p + facet_wrap(~metric, scales = "free")








# ##### link to module scripts #####################################################
# modalpath = 'C:\\Users\\Bougie\\Desktop\\scripts\\modules\\r_maps'
# source(paste(modalpath, 'legendLabelCreator_bins_v2.R', sep='\\'))
# 
# 
# ##### link to scripts #####################################################
# rootpath = 'C:\\Users\\Bougie\\Desktop\\scripts\\projects\\usxp\\stages\\deliverables\\choropleths\\rcode'
# source(paste(rootpath, 'usxp_maps_getDF_bins_v2.R', sep='\\'))
# source(paste(rootpath, 'choropleth_legends2.R', sep='\\'))


# ###### link to json files #################################################
# figure_json = 'C:\\Users\\Bougie\\Desktop\\Gibbs\\scripts\\projects\\usxp\\stages\\deliverables\\json\\figure_json.json'
# figure_obj<- fromJSON(file=figure_json)


# getDF <- function(current_obj){
# 
#   if(current_obj$dataset == 'nass'){
# 
#     # query_nass = "SELECT atlas_stco, perc_conv_county as perc, geom FROM ag_census.ag_census_expansion WHERE perc_conv_county > 0.5"
#     query_nass = "SELECT atlas_stco, (perc_conv_county/10) as perc, geom FROM ag_census.ag_census_expansion"
#     df <- get_postgis_query(con_nass, query_nass, geom_name = "geom")
# 
# 
#     #### note: modify the datafreame here for cartography ############
#     df$perc[df$perc < 0]  <- 0.0001
# 
#     ###attach df to specific object in json
#     return(df)
#   }
#   else if(current_obj$dataset == 'nri'){
#     # query_nri = "SELECT a.fips, a.ratio_expansion * 100 as perc, b.geom FROM main.nri_2015_analysis as a INNER JOIN spatial.counties as b USING(fips) WHERE a.ratio_expansion > 0.005"
#     query_nri = "SELECT a.fips, (a.ratio_expansion/8) * 100 as perc, b.geom FROM main.nri_2015_analysis as a INNER JOIN spatial.counties as b USING(fips) where a.ratio_expansion IS NOT NULL"
#     df <- get_postgis_query(con_nri, query_nri, geom_name = "geom")
# 
#     ###attach df to specific object in json
#     return(df)
#   }
# 
#   else if(current_obj$dataset == 'nlcd'){
# 
    # query_nlcd="SELECT
    #             combine_nlcd08_16_histo.atlas_stco,
    #             ((combine_nlcd08_16_histo.acres/counties.acres_calc)/8)*100 as perc,
    #             geom
    #             FROM
    #             choropleths.combine_nlcd08_16_histo INNER JOIN
    #             spatial.counties
    #             ON
    #             counties.atlas_stco = combine_nlcd08_16_histo.atlas_stco
    #             WHERE label = '4'"
#     df <- get_postgis_query(con_usxp_deliverables, query_nlcd, geom_name = "geom")
# 
#     return(df)
#   }
#   else if(current_obj$dataset == 'usxp'){
#     query_usxp="SELECT atlas_stco, (perc_conv_county/8) as perc, geom FROM choropleths.s35_perc_conv_county"
#     df <- get_postgis_query(con_usxp_deliverables, query_usxp, geom_name = "geom")
# 
#     ###attach df to specific object in json
#     return(df)
#   }
# }
# 
# # query_nass = "SELECT atlas_stco, perc_conv_county as perc, geom FROM ag_census.ag_census_expansion WHERE perc_conv_county > 0.5"
# query_nass = "SELECT atlas_stco, (perc_conv_county/10) as perc, geom FROM ag_census.ag_census_expansion"
# df <- get_postgis_query(con_nass, query_nass, geom_name = "geom")




# 
# runMain <- function(figure_params){
# 
#   merge_ggplotlists <- list()
# 
#   for(i in figure_params$panels){
#     print('start--------------------------------')
#     df <- getDF(i)
#     fp = append(figure_params$core, i)
#     print('*********************** yo ********************************************')
#     print(fp)
#     ggplot_object <- createMap(fp, df)
#     merge_ggplotlists <- append(merge_ggplotlists, list(ggplot_object))
#   }
# 
#   return(merge_ggplotlists)
# 
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #####################################################################
# ######## run script #################################################
# #####################################################################
# parent="main"
# figure="choropleths"
# 
# ###### link to json files #################################################
# figure_json = 'C:\\Users\\Bougie\\Desktop\\scripts\\projects\\usxp\\stages\\deliverables\\json\\figure_json.json'
# figure_obj<- fromJSON(file=figure_json)
# 
# 
# ####DEFINE THE FIGURE OBJECT HERE!!!!!!!!!!!!!!!!!!!
# figure_params = figure_obj[[parent]][[figure]]
# legend <- createLegend(figure_params)
# 
# 
# 
# map_panels <- runMain(figure_params)
# 
# 
# 
# # 
# # 
# # 
# # 
# # ###create a matrix that will be filled with the plots above
# lay <- rbind(c(1,1,1,1,2,2,2,2),
#              c(1,1,1,1,2,2,2,2),
#              c(3,3,3,3,4,4,4,4),
#              c(3,3,3,3,4,4,4,4),
#              c(5,5,5,5,5,5,5,5))
# #
# # #merge all three plots within one grid (and visualize this)
# 
# # margin = theme(plot.margin = unit(c(2,2,2,2), "cm"))
# g <- arrangeGrob(map_panels[[1]],map_panels[[2]],map_panels[[3]],map_panels[[4]],legend, layout_matrix = lay)
# 
# 
# 
# fileout = paste0('I:\\projects\\usxp\\series\\s35\\deliverables\\figures\\main\\',figure_params$core$output)
# # ggplot2::ggsave(fileout, width = 34, height = 25, dpi = 500)
# ggplot2::ggsave(fileout, width = 34, height = 25, dpi = 500, g)
# 
# 
# 
























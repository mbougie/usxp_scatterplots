SELECT
c.district_unique,
county.state_name,
sum((mtr3.acres - mtr4.acres)/8) as acres_usxp_per_year
---(((mtr3.acres - mtr4.acres)/8 )/county.acres_calc * 100) as acres_usxp_per_year_per_eu
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
spatial.counties_102003 as county USING(atlas_stco) 
INNER JOIN
misc.district_lookup as c on county.atlas_stco=c.fips 
group by county.state_name, c.district_unique
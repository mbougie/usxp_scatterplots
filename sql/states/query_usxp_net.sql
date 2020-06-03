SELECT
a.atlas_name,
a.acres_usxp_per_year,
((a.acres_usxp_per_year)/b.acres_calc * 100) as acres_usxp_per_year_per_eu 
FROM 

(SELECT
state_name as atlas_name,
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

GROUP BY state_name) as a

INNER JOIN 

spatial.states_102003 as b USING(atlas_name)
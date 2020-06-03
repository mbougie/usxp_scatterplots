SELECT
b.atlas_name,
sum(a.expansion) as acres,
sum(a.expansion)/8 as acres_per_year,
--b.acres_calc,
((sum(a.expansion)/8)/b.acres_calc * 100) as acres_per_year_per_eu,
b.wkb_geometry as geom
FROM main.nri_2015_analysis as a INNER JOIN 
spatial.states_102003 as b USING(st_abbrev)
GROUP BY b.atlas_name, b.acres_calc, b.wkb_geometry
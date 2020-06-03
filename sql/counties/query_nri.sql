SELECT
b.atlas_name,
b.atlas_stco,
a.expansion as acres,
a.expansion/8 as acres_per_year,
((a.expansion/8)/b.acres_calc * 100) as acres_per_year_per_eu,
b.acres_calc,
b.wkb_geometry as geom
FROM main.nri_2015_analysis as a INNER JOIN 
spatial.counties_102003 as b USING(atlas_stco)
WHERE a.expansion IS NOT NULL
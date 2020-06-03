
SELECT
b.atlas_name,
b.atlas_stco,
a.diff/10 as acres_per_year,
((a.diff/10)/b.acres_calc) * 100 as acres_per_year_per_eu,
b.acres_calc,
b.wkb_geometry as geom
FROM ag_census.ag_census_expansion as a INNER JOIN
spatial.counties_102003 as b USING(atlas_stco)
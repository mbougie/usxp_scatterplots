SELECT
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
WHERE a.label = '4'
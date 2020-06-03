SELECT
b.atlas_stco,
b.atlas_name,
(a.acres/8) as acres_usxp_per_year,
((a.acres/8)/b.acres_calc * 100) as acres_usxp_per_year_per_eu
--b.wkb_geometry as geom
FROM
zonal_stats.s35_mtr_counties as a INNER JOIN spatial.counties_102003 as b
USING(atlas_stco)
WHERE label in ('3')
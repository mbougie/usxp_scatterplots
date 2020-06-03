SELECT
c.atlas_name,
sum(a.acres) as acres,
sum(a.acres)/8 as acres_per_year,
((sum(a.acres)/8)/c.acres_calc * 100) as acres_per_year_per_eu,
c.wkb_geometry as geom
FROM
choropleths.combine_nlcd08_16_histo as a INNER JOIN
spatial.counties_102003 as b
USING(atlas_stco)
INNER JOIN spatial.states_102003 as c ON c.atlas_name=b.state_name
WHERE a.label = '4'
GROUP BY b.state_name,
c.acres_calc,
c.wkb_geometry,
c.atlas_name
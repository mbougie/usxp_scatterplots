SELECT
c.atlas_name,
sum(a.diff/10) as acres_per_year,
((sum(a.diff/10))/c.acres_calc * 100) as acres_per_year_per_eu,
c.wkb_geometry as geom
FROM ag_census.ag_census_expansion as a INNER JOIN
spatial.counties_102003 as b USING(atlas_stco)
INNER JOIN spatial.states_102003 as c USING (st_abbrev)
WHERE a.diff IS NOT NULL
GROUP BY c.atlas_name,c.acres_calc,c.wkb_geometry
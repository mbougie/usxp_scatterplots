SELECT
c.district_unique,
b.state_name,
sum((a.acres/8)) as acres_usxp_per_year
--((a.acres/8)/b.acres_calc * 100) as acres_usxp_per_year_per_eu
--st_union(st_snaptogrid(b.wkb_geometry, 0.0001::double precision)) AS geom
FROM
zonal_stats.s35_mtr_counties as a INNER JOIN spatial.counties_102003 as b
USING(atlas_stco) INNER JOIN
misc.district_lookup as c on b.atlas_stco=c.fips 
WHERE label in ('3')
group by b.state_name, c.district_unique
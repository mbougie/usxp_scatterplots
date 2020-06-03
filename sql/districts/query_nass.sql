SELECT
c.district_unique,
b.state_name,
sum(a.diff/10) as acres_per_year
--d.wkb_geometry AS geom
---t_union(st_snaptogrid(b.wkb_geometry, 0.0001::double precision)) AS geom
FROM ag_census.ag_census_expansion as a INNER JOIN
spatial.counties_102003 as b USING(atlas_stco) INNER JOIN
misc.district_lookup as c on b.atlas_stco=c.fips 
---spatial.asd_2012_20m_102003 as d on c.district_unique=d.stasd_a
group by b.state_name, c.district_unique
order by state_name
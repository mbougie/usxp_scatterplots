SELECT
c.district_unique,
b.state_name,
sum(a.acres/8) as acres_per_year
----st_union(st_snaptogrid(b.wkb_geometry, 0.0001::double precision)) AS geom
FROM
choropleths.combine_nlcd08_16_histo as a INNER JOIN
spatial.counties_102003 as b
USING(atlas_stco) INNER JOIN
misc.district_lookup as c on b.atlas_stco=c.fips 
WHERE a.label = '4'
group by b.state_name, c.district_unique
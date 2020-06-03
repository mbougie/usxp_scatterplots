SELECT
a.atlas_name,
a.expansion as acres_usxp,
a.expansion/8 as acres_usxp_per_year,
((a.expansion/8)/b.acres_calc * 100) as acres_usxp_per_year_per_eu 
FROM choropleths.s35_perc_conv_states as a INNER JOIN
spatial.states_102003 as b USING(atlas_name)
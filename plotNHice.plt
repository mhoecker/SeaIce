reset
plot 'NH_seaice_extent_final_v2.csv' \
	using ($2/12.0+$3):4 \
	every ::2 \
	notitle

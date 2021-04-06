./setup magnetoHD/UCLA-BB-Marissa -2d -auto \
        -nxb=16 -nyb=16 +hdf5typeio +usm3t  \
        species=cham,targ +mtmmmt +laser    \
	ed_maxPulses=1 ed_maxBeams=1 ed_maxPulseSections=4 \
        +mgd mgd_meshgroups=6               \
	-parfile=peenplume3.par             \
        -site=mbpadams -objdir=UCLA_BB_Marissa-Test

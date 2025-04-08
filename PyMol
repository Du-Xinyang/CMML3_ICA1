######### Hydrogen bond visualization #########
(taking 280K as an example, the interface residues were obtained from PISA results, needs to be changed when temperature changes)

# load the model
load final_280K
select chainA, chain A
select chainB, chain B
select interface_A, chain A and resi 32+33+62+64+96+97+99+100+101+102+103+104+107+125+126+127+128+129+130+152+153+154+155+156+188+189+209+217+218+219+220+221+222
select interface_B, chainB and resi 10+11+13+14+15+16+17+18+19+20+21+32+33+34+35+36+43+55

# Interface hydrogen bonds visualization
distance inter_hbonds, chainA, chainB, cutoff=4.0, angle=120, mode=2

# Format settings
set dash_radius, 0.1
set dash_gap, 0.2
hide everything, not interface_A and not interface_B
set label_size, -1
set label_color, forest, inter_hbonds


############# AF model alignment ############
load final_280K
load fold_af3_1r0r_model_1
align fold_af3_1r0r_model_1, final_280K  # Record the RMSD value

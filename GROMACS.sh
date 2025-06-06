# Locate the software
alias gmx="/software/gromacs-2024.3/build/bin/gmx_mpi"

# Run GROMACS
# Generate topology file, position restraint file and the post-processed structure file   #Choose amber99sb-ildn
    gmx pdb2gmx -f 1r0r_clean.pdb -o complex_processed.gro -water spce  
# Define the box
    gmx editconf -f complex_processed.gro -o complex_box.gro -c -d 1.0 -bt cubic
# Fill it with solvent (water)
    gmx solvate -cp complex_box.gro -cs spc216.gro -o complex_solv.gro -p topol.top
# Adding ions (Cl-1)
    gmx grompp -f ions.mdp -c complex_solv.gro -p topol.top -o ions.tpr 
# genion  # Choose SOL
    gmx genion -s ions.tpr -o complex_solv_ions.gro -p topol.top -pname NA -nname CL -neutral 
# Energy minimization
    gmx grompp -f minim.mdp -c complex_solv_ions.gro -p topol.top -o em.tpr
# Run MD
    gmx mdrun -v -deffnm em -nb gpu -ntomp 8
# Check if the potential energy converges (Fmax<1000 kJ/mol/nm in em. log)
# Energy analysis  # 10 0
    gmx energy -f em.edr -o potential.xvg 
# Equilibration with solvent, Applying a position restraining force
    gmx grompp -f nvt.mdp -c em.gro -r em.gro -p topol.top -o nvt.tpr
# NVT equilibration
    gmx mdrun -deffnm nvt -nb gpu -ntomp 8
# Analysis of the temperature progression, using energy  #16 0
    gmx energy -f nvt.edr -o temperature.xvg 
# NPT Equilibration 
    gmx grompp -f npt.mdp -c nvt.gro -r nvt.gro -t nvt.cpt -p topol.top -o npt.tpr
# NPT equilibration
    gmx mdrun -deffnm npt -nb gpu -ntomp 8
# Analysis of pressure progression, using energy   #24 0
    gmx energy -f npt.edr -o density.xvg 
# Analysis of pressure progression, using energy    # 18 0
    gmx energy -f npt.edr -o pressure.xvg
# Release the position restraints and run production MD
    gmx grompp -f md_300K_50ns.mdp -c npt.gro -t npt.cpt -p topol.top -o md_300K_50ns.tpr
# 50ns simulation
    gmx mdrun -deffnm md_300K_50ns -nb gpu -ntomp 8

# MD results analysis
# To account for any periodicity in the system   # Choose "Protein", choose "System" as output
    gmx trjconv -s md_300K_50ns.tpr -f md_300K_50ns.xtc -o md_300K_50ns_noPBC.xtc -pbc mol -center     
# Structural stability analysis. Relative to the structure present in the minimized, equilibrated system.     # Choose "Protein", choose "System" as output
    gmx rms -s md_300K_50ns.tpr -f md_300K_50ns_noPBC.xtc -o rmsd_300K_50ns.xvg -tu ns   
# Structural stability analysis. Relative to the crystal structure.    #Choose "Backbone", choose "System" as output
    gmx rms -s em.tpr -f md_300K_50ns_noPBC.xtc -o rmsd_xtal.xvg      
# The radius of gyration     # Choose "Protein"
    gmx gyrate -s md_300K_50ns.tpr -f md_300K_50ns_noPBC.xtc -o gyrate_300K_50ns.xvg    
# Extract the last frame structure (assuming a total simulation duration of 50 ns)     #Choose "Protein"
    gmx trjconv -s md_300K_50ns.tpr -f md_300K_50ns_noPBC.xtc -o final_300K.pdb -dump 50000      
# Extract structures within a specific time range (such as the last 10 ns)     #Choose "Protein"
    gmx trjconv -s md_300K_50ns.tpr -f md_300K_50ns_noPBC.xtc -b 40000 -e 50000 -o last10ns_300K.pdb# -b 40 (ns) -e 50 (ns)

# Running DSSP
    gmx dssp -f md_320K_50ns_noPBC.xtc -s md_320K_50ns.tpr -o ss_320K_50ns.dat -num dssp.xvg

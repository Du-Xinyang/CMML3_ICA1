# SC-OMTKY3 complex stability analysis
## Software Requirements
1. GROMACS 2024.3
1. PyMOL 2.5.7
1. R 4.3.3
1. PISA 2.0.6
## Molecular Dynamics Simulation ##
Code tempelate for running a MD simulation under a certain temperature was provided in the **GROMACS.sh**. The input is the 1r0r.pdb file obtained from PDB database and that has removed water molecule, which can be done in PyMol by: 
```bash
load 1r0r.pdb
remove resn HOH
save 1r0r_clean.pdb, 1r0r
```
Files including md_short.mdp, nvt.mdp, and npt.mdp should be modified to corresponding temperature during each simulation.
## Visualisation & Stability Analysis ## 
### PISA Typical Workflow ###
1. Upload MD pdb structure at each temperature to PISA web interface: https://www.ebi.ac.uk/msd-srv/prot_int/pistart.html
2. ΔG and hydrogen bond numbers were obtained from **Interfaces**.
3. Residues that form hydrogen bonds and their buried surface area were obtained from **Details**.
### PyMol Typical Workflow ###
Code is deposited in the **PyMol.sh**.
1. Visualize hydrogen bond. 
2. Alignment of MD model and AF3 model. AF3 model obtained by inputting sequence of SC-OMTKY3 at AlphaFoldServer website: https://alphafoldserver.com/welcome
## Statistical analysis & Visualisation ##
### Reproducibility validation ###
Code is deposited in the **R/Reproducibility Validation.R**.
Validating reproducibility through analyzing processes including energy minimization, temperature equilibration, pressure equilibration, and density equilibration.
### RMSD and Rg Comparison ###
Code is deposited in the **R/RMSD_Rg.R**.
1. For the RMSD comparison, code for generating RMSD for the replicates at each temperature and for comparing RMSD at different temperatures in a limited time period from 20 to 25 ns were provided.
2. For the Rg comparison, code for generating RMSD for the replicates at each temperature and for comparing Rg at different temperatures in the whole 50ns or in a limited time period from 15 to 25 ns were provided.
### PISA result comparison ###
Code for comparing ΔG values, hydrogen bond number, and buried surface area were deposited in **R/PISA.R**. Bar chart and line chart are used for visualization of relevant values.
### DSSP analysis and result visualisation ###
Code for doing DSSP analysis was deposited in **GROMACS.sh**, analysis requires GROMACS 2024.3. 
Code for generating the stacked bar chart of secondary structureis deposited in the **R/DSSP.R**. The number variation across 50 ns of all secondary structures of the SC-OMTKY3 complex was visualized. Input requires dssp.xvg obtained from GROMACS. 



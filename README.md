# SC-OMTKY3 complex stability analysis
## Requirements
1. GROMACS 2024.3
1. PyMOL 2.5.7
1. R 4.3.3
1. PISA 2.0.6
## Molecular Dynamics Simulation ##
Code tempelate for running a MD simulation under a certain temperature was provided in the **GROMACS** folder. Files including md_short.mdp, nvt.mdp, and npt.mdp should be modified to corresponding temperature during each simulation.
## Visualisation & Stability Analysis ## 
### PISA Typical Workflow ###
1. Upload structure at each temperature to PISA web interface: https://www.ebi.ac.uk/msd-srv/prot_int/pistart.html
2. ΔG and hydrogen bond numbers were obtained from **Interfaces**.
3. Residues that form hydrogen bonds and their buried surface area were obtained from **Details**.
### PyMol Typical Workflow ###
Code are deposited in the **PyMol** folder.
1. Visualize hydrogen bond. 
2. Alignment of MD model and AF3 model.
## Statistical analysis & Visualisation ##
### Reproducibility validation ###

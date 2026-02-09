[![DOI](https://zenodo.org/badge/18552364.svg)](https://zenodo.org/badge/latestdoi/18552364)

# GCAM-TD-CMM
Repository for generating T&D CMM input files ready to use in gcamdata.

## Instructions
Users can source the main processing script TD_data_proc.R to generate all files. There are two output files from this workflow, which are included in the `output` folder.
1) `A26.td_mineral_coef.csv` (T&D technologies mineral intensity information for GCAM32 regions and years 2015-2100)
2) `A26.td_nonenergy_cost_nonmineral.csv` (non-energy, non-mineral costs for the T&D technologies for GCAM32 regions and years 2015-2100).

These files are to be utilized as gcamdata input files, and can be used in the CMM multi-sectoral demand branch associated with Qiu et al. (under review), 
_Critical Mineral and Material Supply Availability and Energy System Development: A Multisector Analysis and Implications_.

Note: the files generated here do not include the standard gcamdata input file header information. These will need to be added by the user.

## Citation
Brinda Yarlagadda. (2026). brinday/GCAM-TD-CMM: v1.0 (v1.0). Zenodo. https://doi.org/10.5281/zenodo.18552364

## References
Qiu et al. (under review), _Critical Mineral and Material Supply Availability and Energy System Development: A Multisector Analysis and Implications_.

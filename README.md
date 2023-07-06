# AAV-Perturb-seq

These notebooks contain code reproducing the single-cell analyses from:

Santinha, A.J., Klingler, E., Kuhn, M., Farouni, R., Lagler, S., Kalamakis, G., Lischetti, U., Jabaudon, D., & Platt, RJ., "AAV-mediated single-nucleus CRISPR screening of DiGeorge syndrome in vivo", Nature, 2023

Download the R objects from GEO to use the notebooks: GSE236519.

Count tables for gRNA expression libraries are generated as shown in Hill et al 2018 (https://github.com/shendurelab/single-cell-ko-screens.git)

Scripts in this repo:

* `gRNA_nucleus_association.Rmd` contains code for gRNA QC, filtering, and integration with gene expression data

* `pooled_screen_analysis.Rmd` accepts scRNA-seq screen data containing multiple cell types and perturbations
  - Filter non-perturbed cells with LDA
  - Create pseudobulks of perturbed cells
  - Calculate differential expression with edgeR using pseudobulks as input
  - Plot cell type-specific UMAPs with perturbed cells
 
* `zigosity_analysis.Rmd` contains code to separate perturbed cells by zygosity states

* `lgdel_data_analysis.Rmd` contains code to analyze the LgDel dataset and compare individual perturbations to the full deletion

* `plot_functions.R` contains support functions for plots

* `dif.exp_functions.R` contains support functions for filtering, differential expression, and data visualization


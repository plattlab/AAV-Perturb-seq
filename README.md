# AAV-Perturb-seq

These notebooks contain code reproducing the single-cell analyses from:

Santinha, A.J., Klingler, E., Kuhn, M., Farouni, R., Lagler, S., Kalamakis, G., Lischetti, U., Jabaudon, D., & Platt, RJ., "AAV-mediated single-nucleus CRISPR screening of DiGeorge syndrome in vivo", Nature, 2023

Download the R objects from GEO to use the notebooks: GSE236519.

* `gRNA_nucleus_association.Rmd` constain code for gRNA QC, filtering, and integration with gene expression data

* `pooled_screen_analysis.Rmd` accepts scRNA-seq screen data containing multiple cell types and perturbations
  - Filter non-perturbed cells with LDA
  - Create pseudobulks of perturbed cells
  - Calculate differential expression with edgeR using pseudobulks as input
  - Plot cell type-specific UMAPs with perturbed cells


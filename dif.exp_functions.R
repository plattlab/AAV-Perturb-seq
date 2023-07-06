pseudoBulk_screen <- function(object, assay, ct_col_meta, cell_type, perturbation_col_meta, perturb_info, sample_meta, min_features){
  #' @param object the seurat object already with cluster cell types
  #' @param assay select counts or data
  #' @param ct_col_meta column with cell type labels
  #' @param cell_type cell type of interest
  #' @param perturbation_col_meta column with perturbation labels
  #' @param lane_meta column with lane info.
  #' @param min_features min number of genes.
  
  # Isolate cell type
  Idents(object) <- object@meta.data[,ct_col_meta]
  object <- subset(object, idents = cell_type)
  
  
  # Isolate perturbation data
  Idents(object) <- object@meta.data[,perturbation_col_meta]
  object <- subset(object, idents = perturb_info)
  
  # Change Safe_H to Safe
  object@meta.data[,perturbation_col_meta][object@meta.data[,perturbation_col_meta] == "Safe_H"] <- "Safe"
  
  # Isolate data and metadata
  meta <- object@meta.data
  
  expr <- switch(assay, 
                 counts = object@assays$RNA@counts,
                 data = object@assays$RNA@data)
  
  # Change names metadata columns, just in cae
  names(meta)[names(meta) == sample_meta] <- "sample"
  names(meta)[names(meta) == perturbation_col_meta] <- "perturbation"
  
  # Process data
  mm <- model.matrix(~0 + perturbation:sample, data = meta)
  mat_mm <- expr %*% mm
  keep_genes <- rowSums(mat_mm > 0) > min_features
  
  mat_mm <- mat_mm[keep_genes, ] %>% as.data.frame()
  mat_mm <- as.data.frame(mat_mm)
  
  colnames(mat_mm) = gsub("perturbation|sample|", "", colnames(mat_mm))
  
  # drop empty columns
  keep_samples = colSums(mat_mm) > 0
  mat_mm <- mat_mm[, keep_samples]
  return(mat_mm)
}


pseudoDE <- function(counts, metadata, groups, de_test, de_subtest = NULL, control = "Safe"){
  #' @param counts
  #' @param metadata
  #' @param groups perturbations to analyze. Don't need to include Safe, as it is already assumed.
  #' @param de_test
  #' @param de_subtest
  
  # Filter perturbations to analyze
  m <- metadata[metadata$per_gene %in% c(control, groups),]
  counts <- counts[,names(counts) %in% m$pseudobulk]
  
  # Create design matrix
  design <- model.matrix(pseudobulk~per_gene, data = m)
  design <- design[,colSums(design) !=0]
  
  DE <- switch(de_test, 
               edgeR = {
                 y <- DGEList(counts = counts, group = m$per_gene) %>%
                   calcNormFactors(method = 'TMM') %>%
                   estimateDisp(design)
                 test <- switch (de_subtest,
                                 QLF = {
                                   fit = glmQLFit(y, design)
                                   test = glmQLFTest(fit, coef = -1)
                                 },
                                 LRT = {
                                   fit = glmFit(y, design = design)
                                   test = glmLRT(fit)
                                 })
                 res = topTags(test, n = Inf) %>%
                   as.data.frame() %>%
                   rownames_to_column('gene') %>%
                   # flag metrics in results
                   mutate(de_family = 'pseudobulk',
                          de_method = de_test,
                          de_type = de_subtest)
               })
  return(DE)
}


umap_perturbations <- function(object, assay, ct_col_meta, cell_type, perturbation_col_meta, perturbations, features, total_deg, n_neighbors = 10, metric = "cosine", spread = 10, min_dist = 5){
  #' @param object seurat object with screen data
  #' @param assay which matrix to use, "counts" or "data" or "scale.data"
  #' @param ct_col_meta cell type column in metadata
  #' @param cell_type whcih cell type to analyze. Should be present in "ct_col_meta"
  #' @param perturbation_col_meta perturbation column in metadata
  #' @param perturbations which perturbations to analyze
  #' @param features which features to use to construct the UMAP representation 
  #' at assumes cell names are in metadata column called "cell_name"
  
  
  # Select cells
  Idents(object) <- object@meta.data[,ct_col_meta]
  object <- subset(object, idents = cell_type)
  
  Idents(object) <- object@meta.data[,perturbation_col_meta]
  object <- subset(object, idents = perturbations)
  
  
  # Extract matrix from Seurat object
  expr <- switch(assay, 
                 
                 counts = data.frame(t(object@assays$RNA@counts[features,])),
                 
                 data = data.frame(t(object@assays$RNA@data[features,])),
                 
                 scale.data = {
                   object <- ScaleData(object, features = features)
                   scd <- data.frame(t(object@assays$RNA@scale.data[features,]))
                 })
  
  meta <- object@meta.data[,c("cell_name", ct_col_meta, perturbation_col_meta)]
  
  # Prepare data for UMAP
  expr$cell_name <- rownames(expr)
  pre_umap <- merge(expr, meta, by = "cell_name")
  
  # Prepare all genes data for UMAP metadata
  object <- ScaleData(object, features = total_deg)
  gene_exp <- data.frame(t(object@assays$RNA@scale.data[total_deg,]))
  gene_exp$cell_name <- rownames(gene_exp)
  gene_exp <- merge(gene_exp, meta, by = "cell_name")
  
  # Run UMAP
  umap_data <- uwot::umap(X = pre_umap[,!names(pre_umap) %in% c("cell_name", ct_col_meta, perturbation_col_meta)],
                          n_neighbors = n_neighbors,
                          metric = metric,
                          spread = spread,
                          min_dist = min_dist)
  
  umap_data <- data.frame(umap_data)
  colnames(umap_data) <- c("umap_x", "umap_y")
  umap_data <- cbind(gene_exp, umap_data)
  
  return(umap_data)
}


violin_plot <- function(object, ct_col_meta, cell_type, group_col_meta, group, assay = "data", feature, boxp = FALSE, plot_colors){
  # Get specific cell type
  Idents(object) <- object@meta.data[ ,ct_col_meta]
  object <- subset(object, idents = cell_type)
  
  
  # Get only the desired groups
  Idents(object) <- object@meta.data[ ,group_col_meta]
  object <- subset(object, idents = group)
  
  # Prepare metadata from in use cells
  meta_cells <- object@meta.data
  
  # Get data from desired assay
  if (assay == "scale.data"){
    object <- data.frame(object@assays$RNA@scale.data[feature,])
    colnames(object) <- "gene"
    object$cell_name <- rownames(object)
    
  }
  
  if (assay == "data"){
    object <- data.frame(object@assays$RNA@data[feature,])
    colnames(object) <- "gene"
    object$cell_name <- rownames(object)
    
  }
  
  if (assay == "counts"){
    object <- data.frame(object@assays$RNA@counts[feature,])
    colnames(object) <- "gene"
    object$cell_name <- rownames(object)
  }
  
  # Merge metadata with counts data
  object <- merge(meta_cells, object, by = "cell_name")
  names(object)[names(object) == group_col_meta] <- "porra"
  
  
  # Plot data
  if (boxp == FALSE){
    plt <- ggplot(object, aes(x = porra, y = gene, fill = porra)) +
      plot_def() +
      geom_violin(color = NA) +
      scale_fill_manual(values=plot_colors) + theme(legend.position="none") +
      geom_jitter(shape = 16, position = position_jitter(0.1), color = "grey58", size = 0.0001) +
      scale_x_discrete(limits = group)
  }
  
  if (boxp == TRUE) {
    plt <- ggplot(object, aes(x = porra, y = gene, fill = porra)) +
      plot_def() +
      geom_violin(color = NA) +
      scale_fill_manual(values=plot_colors) + theme(legend.position="none") +
      geom_boxplot(fill='gray90', width=0.1) +
      scale_x_discrete(limits = group)
  }
  
  return(plt)
}


detectPerturbations <- function(data, cell_types, ct_column, pert_column, ctrl_cond, perturbations, ct_col_cell_name = "cell_name", logfc.thre = 0.15, min.pc = 0.25, test = "LR", min_de_genes = 4, max_de_genes = 20){
  
  # Start column to bring metadata
  data$new_label <- "hold"
  data$new_label[data@meta.data[,pert_column] %in% "Safe_H"] <- "Safe_H"
  
  for (y in cell_types){
    print(y)
    # Isolate cell type
    Idents(data) <- data@meta.data[, ct_column]
    object <- subset(data, idents = y)
    
    # Run analysis for each perturbation specifically
    de_data <- list()
    for (i in perturbations){
      # Isolate perturbation and control cells
      Idents(object) <- object@meta.data[,pert_column]
      ind <- subset(object, idents = c(ctrl_cond, i))
      
      # Perform DE analysis
      de <- FindMarkers(object = ind,
                        ident.1 = i,
                        ident.2 = ctrl_cond,
                        logfc.threshold = logfc.thre,
                        min.pct = min.pc,
                        test.use = test)
      
      # keep DE data
      data@tools[[y]][[i]] <- de
      
      # Get data with the de genes
      genes <- rownames(de[de$p_val_adj < 0.05,])
      
      # If there are less than "min_de_genes", exclude perturbation already
      if (length(genes) < min_de_genes){
        print(paste(i, " didn't lead to a significant transcriptional phenotype", sep = ""))
        
        # Add metadata
        data$new_label[data@meta.data[,ct_col_cell_name] %in% data@meta.data[,ct_col_cell_name][ind@meta.data[,pert_column] %in% i]] <- paste(i, "NP", sep = "_")
      }
      
      # Else
      if (length(genes) >= min_de_genes){
        print(paste(i, " has more than ", min_de_genes, " significant DE genes", sep = ""))
        print("continue")
        
        # Define the number of total genes to use on the LDA model
        if (length(genes) > max_de_genes){
          genes <- de %>% top_n(-max_de_genes, p_val_adj) %>% rownames(.)
        }
        
        # Get expression and meta data to input into the model
        exp <- data.frame(t(ind@assays$RNA@data[genes,]))
        exp$cell_name <- rownames(exp)
        
        meta <- ind@meta.data[,c(ct_col_cell_name, pert_column)]
        names(meta) <- c("cell_name", "per_gene")
        
        exp <- merge(exp, meta, by = "cell_name")
        exp <- exp %>% column_to_rownames("cell_name")
        
        
        # Run lda
        model <- MASS::lda(factor(per_gene)~., data = exp)
        
        pred <- predict(model, newdata = exp[,!names(exp) %in% "per_gene"])
        post <- data.frame(pred$class)
        
        exp <- cbind(exp, post)
        
        # Add new labels
        exp$new_label <- ctrl_cond
        exp$new_label[exp$per_gene == i & exp$pred.class == i] <- paste(i, "KO", sep = "_")
        exp$new_label[exp$per_gene == i & exp$pred.class == ctrl_cond] <- paste(i, "NP", sep = "_")
        
        
        # Add metadata to original file
        exp$cell_name <- rownames(exp)
        data$new_label[data@meta.data[,ct_col_cell_name] %in% exp$cell_name[exp$new_label %in% paste(i, "KO", sep = "_")]] <- paste(i, "KO", sep = "_")
        data$new_label[data@meta.data[,ct_col_cell_name] %in% exp$cell_name[exp$new_label %in% paste(i, "NP", sep = "_")]] <- paste(i, "NP", sep = "_")
        
        
        # Print some info
        av <- nrow(exp[exp$new_label %in% paste(i, "KO", sep = "_"),])/nrow(exp[exp$per_gene %in% i,])*100
        print(paste(as.character(round(av)), "% of cells show a significant perturbation for ", i, sep = ""))
      }
      
    }
  }
  
  return(data)
}


pseudo_Bulk_array <- function(object, assay, ct_col_meta, cell_type, perturbation_col_meta, perturb_info, min_features){
  #' @param object the seurat object already with cluster cell types
  #' @param assay select raw or data
  #' @param ct_col_meta column with cell type labels
  #' @param cell_type cell type of interest
  #' @param perturbation_col_meta column with perturbation labels
  #' @param perturbed_or_not information from music, accepts "perturbed", "nonperturbed" or both as a vector
  #' @param lane_meta column with lane info.
  #' @param min_features min number of genes.
  
  # Isolate cell type
  Idents(object) <- object@meta.data[,ct_col_meta]
  object <- subset(object, idents = cell_type)
  
  # Isolate perturbation data
  Idents(object) <- object@meta.data[,perturbation_col_meta]
  object <- subset(object, idents = perturb_info)
  
  # Change wt_cas9 to Safe
  object@meta.data[,perturbation_col_meta][object@meta.data[,perturbation_col_meta] == "Safe_H"] <- "Safe"
  
  # Isolate data and metadata
  meta <- object@meta.data
  
  expr <- switch(assay, 
                 counts = object@assays$RNA@counts,
                 data = object@assays$RNA@data)
  
  
  # Add random subsample
  rn = floor(runif(n = nrow(meta), min=1, max=4))
  rn <- paste("lane_", rn, sep="")
  meta$lane <- rn
  meta$lane <- as.character(meta$lane)
  
  # Change names
  names(meta)[names(meta) == perturbation_col_meta] <- "per_gene"
  
  
  # Process data
  mm <- model.matrix(~0 + per_gene:lane, data = meta)
  mat_mm <- expr %*% mm
  keep_genes <- rowSums(mat_mm) > min_features
  
  mat_mm <- mat_mm[keep_genes, ] %>% as.data.frame()
  
  colnames(mat_mm) = gsub("per_gene|lane|:", "", colnames(mat_mm))
  
  # drop empty columns
  keep_samples = colSums(mat_mm) > 0
  mat_mm <- mat_mm[, keep_samples]
  return(mat_mm)
}


# Pseudobulk DEG for LgDel
pseudoDE_lgdel <- function(counts, metadata, groups, de_test, de_subtest = NULL){
  #' @param counts
  #' @param metadata
  #' @param groups perturbations to analyze. Don't need to include Safe, as it is already assumed.
  #' @param de_test
  #' @param de_subtest
  
  # Filter perturbations to analyze
  m <- metadata[metadata$per_gene %in% c("wt", groups),]
  counts <- counts[,names(counts) %in% m$pseudobulk]
  
  # Create design matrix
  design <- model.matrix(pseudobulk~per_gene, data = m)
  design <- design[,colSums(design) !=0]
  
  DE <- switch(de_test, 
               edgeR = {
                 y <- DGEList(counts = counts, group = m$per_gene) %>%
                   calcNormFactors(method = 'TMM') %>%
                   estimateDisp(design)
                 test <- switch (de_subtest,
                                 QLF = {
                                   fit = glmQLFit(y, design)
                                   test = glmQLFTest(fit, coef = -1)
                                 },
                                 LRT = {
                                   fit = glmFit(y, design = design)
                                   test = glmLRT(fit)
                                 })
                 res = topTags(test, n = Inf) %>%
                   as.data.frame() %>%
                   rownames_to_column('gene') %>%
                   # flag metrics in results
                   mutate(de_family = 'pseudobulk',
                          de_method = de_test,
                          de_type = de_subtest)
               })
  return(DE)
}


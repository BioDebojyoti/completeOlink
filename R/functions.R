list_of_packages2 <- c(
  "comprehenr",
  "dplyr",
  "stringr",
  "tidyverse",
  "tidyr",
  "factoextra",
  "ggtext"
)

for(pkg2 in list_of_packages2){
  library(pkg2, character.only = TRUE)
}

not_sample_label <- c(
  "SampleID",
  "UniProt",
  "OlinkID",
  "Assay",
  "MissingFreq",
  "LOD",
  "NPX",
  "Assay_Warning",
  "Normalization",
  "ExploreVersion"
)

not_variable_label <- c(
  "SampleID",
  "UniProt",
  "OlinkID",
  "Assay",
  "MissingFreq",
  "LOD",
  "NPX",
  "Assay_Warning",
  "Normalization",
  "ExploreVersion"
)

list_of_outlier_detection_methods <- c(
  "olink_median_iqr_outlier"
)

list_of_outlier_plots <- c(
  "olink_qc_plot",
  "olink_pca_plot",
  "olink_umap_plot"
)

statistical_test_list <- c(
  "olink_ttest",
  "olink_wilcox",
  "olink_anova",
  "olink_one_non_parametric",
  "olink_ordinalRegression",
  "olink_lmer"
)

statistical_test_posthoc_list <- c(
  "olink_anova_posthoc",
  "olink_one_non_parametric_posthoc",
  "olink_ordinalRegression_posthoc",
  "olink_lmer_posthoc"
)

pval_correction_methods <- c(
  "holm",
  "hochberg", 
  "hommel", 
  "bonferroni",
  "BH",
  "BY",
  "fdr", 
  "none"
  )

post_hoc_padjust_method_list1 <- c("tukey", "sidak", "bonferroni", "none")
post_hoc_padjust_method_list2 <- c("kruskal", "friedman")

plot_ggplot_msg <- function(msg) {
  return(
    ggplot() +
      geom_richtext(
        aes(x = 0.5, y = 0.5, label = msg),
        size = 6, # Adjust size if needed
        hjust = 0.5,
        vjust = 0.5,
        fill = NA, # No background fill
        label.color = NA # No border
      ) +
      theme_void()
  )
}

plot_plotly_msg <- function(msg) {
  plotly::plot_ly() %>%
    plotly::layout(
      annotations = list(
        text = msg,
        x = 0.5,
        y = 0.5,
        showarrow = FALSE,
        font = list(size = 16), # Adjust size if needed
        xref = "paper",
        yref = "paper"
      ),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    )
}

# panel specific detection measures
data_detection_dist <- function(npx_data, panel) {
  if(panel != "all"){
    npx_data <- npx_data %>%
    dplyr::filter(Panel == panel)
  }
  panel_data_detection <- npx_data %>%
    dplyr::filter(!stringr::str_detect(SampleID, stringr::regex("control|ctrl", ignore_case = TRUE))) %>%
    dplyr::filter(!stringr::str_detect(Assay, stringr::regex("control|ctrl", ignore_case = TRUE))) %>%
    dplyr::mutate(Detection = ifelse(NPX >= LOD, "Above LOD", "Below LOD"))
  return(panel_data_detection)
}

# assay-wise detection for specific panel (detection column available)
panel_assay_detection_percentage <- function(panel_data_detection) {
  panel_wise_assay_detection <- panel_data_detection %>%
      dplyr::group_by(Assay) %>%
      dplyr::count(Detection) %>%
      summarise(
        total_samples = sum(n),
        samples_above_LOD = sum(if_else(Detection == "Above LOD", n, 0)),
        protein_detection_percentage = round(samples_above_LOD / total_samples * 100, 2)
      ) %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      dplyr::ungroup()
  
  return(panel_wise_assay_detection)
}

panel_wise_metrics_for_assay_detection <- function(npx_data, panel="Inflammation") {
  panel_data_detection <- data_detection_dist(npx_data, panel)
  panel_wise_assay_detection <- panel_assay_detection_percentage(panel_data_detection)
  return(panel_wise_assay_detection)
}


group_specific_panel_assay_detection_percentage <- function(npx_data, panel, group){
  panel_data_detection <- data_detection_dist(npx_data, panel)
  group_specific_panel_assay_detection <- panel_data_detection %>%
      dplyr::group_by(Assay, !!sym(group)) %>%
      dplyr::count(Detection) %>%
      summarise(
        total_samples = sum(n),
        samples_above_LOD = sum(if_else(Detection == "Above LOD", n, 0)),
        protein_detection_percentage = round(samples_above_LOD/total_samples *100,2)
      )
  return(group_specific_panel_assay_detection)
}


pca_plot <- function(
    data_to_use,
    panel,
    group,
    comp = c(1,2),
    geom = "point",
    pointsize=3,
    labelsize=0,
    addEllipse = TRUE,
    ellipse.level=0.95,
    mean.point = FALSE,
    palette = "Dark2"
    ){
  
  if(panel != "all"){
    data_to_use <- data_to_use %>% 
      dplyr::filter(Panel == panel)
  }
  
  data4pca <- data_to_use %>% 
    dplyr::filter(!grepl("control|ctrl", SampleID, ignore.case = TRUE)) %>%
    dplyr::filter(!grepl("control|ctrl", Assay,ignore.case = TRUE)) %>%
    dplyr::select(SampleID, Assay, NPX, !!sym(group))
  
  if (group %in% c("QC_Warning")) {
    data4pca <- data4pca %>%
      dplyr::group_by(SampleID) %>%
      dplyr::mutate(!!sym(group) := ifelse(any(!!sym(group) %in% c("Warning", "WARN")), "WARN", "PASS")) %>%
      dplyr::ungroup()
  }
  

  
  sample_dict <- data4pca %>% dplyr::distinct(SampleID,!!sym(group))  

  data4pca_wide <- as.data.frame(
    data4pca %>% 
      dplyr::select(-all_of(c(group))) %>%
      tidyr::pivot_wider(names_from = "SampleID", values_from = "NPX")
    )
  
  row.names(data4pca_wide) <- data4pca_wide$Assay
  

  data4pca_wide <- data4pca_wide %>% dplyr::select(-Assay)
    
  data_for_pca <- as.data.frame(t(data4pca_wide[complete.cases(data4pca_wide), ]))
 
  # Match sample_dict to active individuals
  retained_samples <- row.names(data_for_pca)
  sample_dict <- sample_dict %>% 
    dplyr::filter(SampleID %in% retained_samples)
  
  if (dim(sample_dict)[1] != dim(data_for_pca)[1]) {
    msg = paste0("Is <span style='color:red;'>", group, "</span> a sample label?")
      return(plot_msg(msg))
  }

  res.pca <- prcomp(data_for_pca, scale = TRUE)
  
  pca_plot <- factoextra::fviz_pca_ind(
    res.pca,
    habillage = sample_dict[[group]],
    addEllipses = addEllipse,
    ellipse.level = ellipse.level,
    pointsize = pointsize,
    labelsize = labelsize,
    repel = TRUE,
    geom = geom,
    palette = palette,
    mean.point = mean.point
  ) 
  
  pca_plot$labels$x <- gsub("Dim","PC", pca_plot$labels$x)
  pca_plot$labels$y <- gsub("Dim","PC", pca_plot$labels$y)
  
  pca_plot <- pca_plot +  
    theme(
      legend.position = "right",
      plot.title = element_blank(),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 20)
    )
  return(pca_plot)
}

qq_ploter <- function(data_to_use, panel, assay){
  
  if(panel != "all"){
    data_to_use <- data_to_use %>%
      dplyr::filter(Panel == panel)
  }
  
  assay_data <- data_to_use %>%
    dplyr::filter(!grepl("control|ctrl", SampleID, ignore.case = TRUE)) %>%
    dplyr::filter(!grepl("control|ctrl", Assay, ignore.case = TRUE)) %>%
    dplyr::filter(Assay == assay) %>%
    dplyr::select(NPX) 
  
  return(
    ggplot2::ggplot(assay_data, aes(sample = NPX)) +
      ggplot2::stat_qq(color = "#0000FF") +
      ggplot2::stat_qq_line(color = "#FFA500") +
      ggplot2::labs(
        x = "theoretical",
        y = "observed",
        title = paste0("q-q plot: ", assay)
      ) +
      ggplot2::theme_minimal(base_size = 15) +
      ggplot2::theme(
        plot.title = element_text(
          size = 24,             
          face = "bold",         
          hjust = 0.5,           
          margin = margin(b = 20) 
        ),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(color = "#000000", size = 14),
        axis.text.y = element_text(color = "#000000", size = 14),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20) 
      )
  )
}


npxCheck <- function(df) {
  # Extract column names
  df_colnames <- colnames(df)
  
  # Check whether df contains NPX or QUANT ----
  if ("NPX" %in% df_colnames) {
    data_type <- "NPX"
  } else if ("Quantified_value" %in% df_colnames) {
    data_type <- "Quantified_value"
  } else {
    stop("Neither NPX or Quantified_value column present in the data")
  }
  
  # Check whether df contains recognized OIDs ----
  if (!("OlinkID" %in% df_colnames)) {
    stop("OlinkID column not present in the data")
  } else {
    non_conforming_OID <- df |>
      dplyr::distinct(OlinkID) |>
      dplyr::filter(stringr::str_detect(OlinkID,
                                        "OID[0-9]{5}",
                                        negate = TRUE)) |>
      dplyr::pull(OlinkID)
  }
  
  # Check for duplicates in SampleID ----
  
  duplicate_ids <- df |>
    dplyr::select(SampleID, OlinkID) |>
    duplicated()
  
  # Check if any duplicates are found
  duplicate_samples <- character(0)
  if (any(duplicate_ids)) {
    duplicate_samples <- unique(df$SampleID[duplicate_ids])
    message(
      "Duplicate SampleID(s) detected:\n ",
      paste(duplicate_samples, collapse = "\n ")
    )
  }
  
  # Identify assays that have only NAs ----
  all_nas <- df |>
    dplyr::group_by(OlinkID) |>
    dplyr::summarise(n = dplyr::n(),
                     n_na = sum(is.na(!!rlang::ensym(data_type))),
                     .groups = "drop") |>
    dplyr::filter(n == n_na) |>
    dplyr::pull(OlinkID)
  
  if (length(all_nas) > 0) {
    warning(
      paste0(
        "The assays ",
        paste(all_nas, collapse = ", "),
        " have NPX = NA for all samples. They will be excluded from the analysis"
      ),
      call. = FALSE
    )
  }
  
  # Identify samples that have all NAs for an assay ----
  sample_all_nas <- df |>
    dplyr::group_by(SampleID) |>
    dplyr::summarise(n = dplyr::n(),
                     n_na = sum(is.na(!!rlang::ensym(data_type))),
                     .groups = "drop") |>
    dplyr::filter(n == n_na) |>
    dplyr::pull(SampleID)
  
  if (length(sample_all_nas) > 0) {
    warning(
      paste0(
        "The samples ",
        paste(sample_all_nas, collapse = ", "),
        " have NPX = NA for all assays. They will be excluded from the analysis"
      ),
      call. = FALSE
    )
  }
  
  # Identify the assays with QC warning ----
  # Identify the assay_warning column
  assay_warning <-
    df |> dplyr::select(
      tidyselect::any_of(
        c("Assay_Warning", "AssayQC")
      )) |>
    names()
  
  if (length(assay_warning) > 0) {
    assays_with_warning <- df |>
      dplyr::select(OlinkID,
                    !!rlang::ensym(assay_warning)) |>
      dplyr::filter(grepl("(?i)warn",
                          !!rlang::ensym(assay_warning))) |>
      dplyr::distinct(OlinkID) |>
      dplyr::pull()
    message(
      paste(length(assays_with_warning),
            " assay(s) exhibited assay QC warning. For more information see the ",
            assay_warning," column.", sep = "")
    )
  }
  
  return(
    list(
      data_type = data_type,
      non_conforming_OID = non_conforming_OID,
      all_nas = all_nas,
      sample_all_nas = sample_all_nas,
      duplicate_samples = duplicate_samples
    )
  )
}


statistical_test <- function(
  df,
  panel_col,
  test_col,
  variable,
  pair_id_val = NULL,
  covariate_val = NULL,
  model_formula_text = NULL,
  random_effect = NULL,
  dependence_val = FALSE,
  subject_val = NULL,
  return_covariates = FALSE  
){
  
  
  if(panel_col != "all"){
    df <- df %>% dplyr::filter(Panel == panel_col)
  }
  
  df <- df %>%
    dplyr::filter(!(grepl("control|ctrl", SampleID, ignore.case = TRUE))) %>%
    dplyr::filter(!(grepl("control|ctrl", Assay, ignore.case = TRUE)))
  
  if(test_col == "olink_ttest"){
    
    levels_variable <- levels(factor(df[[variable]]))
    df[[variable]] <- factor(df[[variable]], levels = rev(levels_variable))
    
    if(is.null(pair_id_val) || pair_id_val == "null"){
      verbose_msg <- capture.output(
      out <- OlinkAnalyze::olink_ttest(df, variable),
      type = "message"
      )
    } else {
      verbose_msg <- capture.output(
      out <- OlinkAnalyze::olink_ttest(df, variable, pair_id = pair_id_val),
      type = "message"
      )
    }
  }
    
  if(test_col == "olink_wilcox"){
    
    levels_variable <- levels(factor(df[[variable]]))
    df[[variable]] <- factor(df[[variable]], levels = rev(levels_variable))
    
    if(is.null(pair_id_val) || pair_id_val == "null"){
      verbose_msg <- capture.output(
      out <- OlinkAnalyze::olink_wilcox(df, variable),
      type = "message"
      )
    } else {
      verbose_msg <- capture.output(
      out <- OlinkAnalyze::olink_wilcox(df, variable, pair_id = pair_id_val),
      type = "message"
      )
    }    
  }
  
  if(test_col == "olink_anova"){
    
    if(is.null(model_formula_text) | model_formula_text == ""){
      verbose_msg <- capture.output(
      out <- OlinkAnalyze::olink_anova(
        df = df, 
        variable = variable, 
        covariates = covariate_val,
        return.covariates = return_covariates,
        verbose = TRUE),
      type = "message"
      )
    } else {
      verbose_msg <- capture.output(
      out <- OlinkAnalyze::olink_anova(
        df = df, 
        model_formula = model_formula_text,
        return.covariates = return_covariates,
        verbose = TRUE),
      type = "message"
      )
    }
    
    }
    
  if(test_col == "olink_lmer"){
    if(is.null(model_formula_text) | model_formula_text == ""){
      verbose_msg <- capture.output(
      out <- OlinkAnalyze::olink_lmer(
        df = df,
        variable = variable,
        random = random_effect,
        covariates = covariate_val,
        return.covariates = return_covariates,
        verbose = TRUE),
      type = "message"
      )
    } else {
      verbose_msg <- capture.output(
      out <- OlinkAnalyze::olink_lmer(
        df = df,
        model_formula = model_formula_text,
        return.covariates = return_covariates,
        verbose = TRUE),
      type = "message"
      )
    }
    
  }
  
  if(test_col == "olink_ordinalRegression"){
    
    if (is.vector(variable)){
      for(v in variable){
        df <- df %>%
          dplyr::filter(
            !is.na(!!sym(v))
          )
      }
    }
    
    verbose_msg <- capture.output(
    out <- OlinkAnalyze::olink_ordinalRegression(
      df,
      variable = variable,
      covariates = covariate_val,
      return.covariates = return_covariates,
      verbose = TRUE),
    type = "message"
    )
    
  }
  
  if(test_col == "olink_one_non_parametric"){
    if(dependence_val){
      verbose_msg <- capture.output(
      out <- OlinkAnalyze::olink_one_non_parametric(
        df,
        variable = variable,
        dependence = dependence_val,
        subject = subject_val,
        verbose = TRUE),
      type = "message"
      )
    } else {
      verbose_msg <- capture.output(
      out <- OlinkAnalyze::olink_one_non_parametric(
        df,
        variable = variable,
        verbose = TRUE),
      type = "message"
      )
      
    }
    
  }
  
  return(list(out, verbose_msg))
  
}

statistical_test_plot <- function(test_result, variable, method2use){
  test_result <- test_result %>%
    dplyr::mutate(
      Adjusted_pval = ifelse(Adjusted_pval == 0, 0.0001, Adjusted_pval),
      text_new = paste0("Assay: ", Assay)
      ) %>%
    dplyr::mutate(
      Threshold_new = ifelse(
        Adjusted_pval < 0.001, "***",
        ifelse(Adjusted_pval < 0.01, "**", 
               ifelse(Adjusted_pval < 0.05, "*", "ns")))
    )
  

  test_result$Threshold_new <- factor(test_result$Threshold_new,
                                       levels = c("ns","***", 
                                                  "**", "*"))
  
  test_volcano <- plot_ly(
    test_result,
    x = ~ estimate,
    y = ~ -log10(Adjusted_pval),
    type = "scatter",
    mode = "markers",
    marker = list(size = 6),    
    text = ~I(text_new),
    split = ~ Threshold_new,
    customdata = ~ Threshold,
    hovertemplate = paste(
      "<b>%{text}</b><br><br>",
      "%{yaxis.title.text}: %{y:.3f}<br>",
      "%{xaxis.title.text}: %{x:.3f}<br>",
      "Significance: %{customdata}",
      "<extra></extra>"
    )
  ) %>%
    layout(
      legend = list(title = list(text = paste0(
        "p-value (", method2use, ")"
      ))),
      xaxis = list(
        title = "log2[FC]",
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff'
      ),
      yaxis = list(
        title = "-log10[p-value]",
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff'
      ),
      plot_bgcolor = '#e5ecf6'
    )
  
  
  return(test_volcano)
}

######

modified_olink_volcano_plot <- function(p.val_tbl, 
                                        x_lab = "Estimate", 
                                        pval_col = "Adjusted_pval",
                                        olinkid_list = NULL, 
                                        pval_cutoff = 0.05,
                                        ...) 
{
  p.val_tbl <- p.val_tbl %>%
    dplyr::mutate(
      Threshold = ifelse(
        !!sym(pval_col) < as.numeric(pval_cutoff), 
        "Significant", "Non-significant")
    ) %>% 
    as.data.frame() %>%
    # dplyr::mutate(across(where(is.numeric), ~ round(., 3))) %>%
      dplyr::mutate(
        Threshold = ifelse(
          Threshold == "Non-significant",
          "Non-significant",
          ifelse(estimate <0, "Down", "Up"))
      )
  # Define colors
  color_palette <- c(
    "Non-significant" = "#898989",
    "Up" = "#ff3232",
    "Down" = "#0021f3"
    # "Significant" = "#99ff99"
  )
  
  if (length(list(...)) > 0) {
    ellipsis_variables <- names(list(...))
    if (length(ellipsis_variables) == 1) {
      if (!(ellipsis_variables == "coloroption")) {
        stop(paste0("The ... option only takes the coloroption argument. ... currently contains the variable ", 
                    ellipsis_variables, "."))
      }
    }
    else {
      stop(paste0("The ... option only takes one argument. ... currently contains the variables ", 
                  paste(ellipsis_variables, collapse = ", "), "."))
    }
  }
  if (is.null(olinkid_list)) {
    olinkid_list <- p.val_tbl %>% 
      dplyr::filter(!!sym(pval_col) < pval_cutoff) %>% 
      dplyr::pull(OlinkID)
  }
  
  plot_min <- min( p.val_tbl$estimate, na.rm = TRUE)
  plot_max <- max( p.val_tbl$estimate, na.rm = TRUE)
  plot_limits <- abs(max(abs(plot_min),plot_max))
  
  volcano_plot <- p.val_tbl %>%
    ggplot2::ggplot(ggplot2::aes(x = estimate, y = -log10(!!sym(pval_col)), color = Threshold)) + 
    ggplot2::geom_point() + 
    ggplot2::labs(x = x_lab, y = "-log10(p-value)") + 
    ggplot2::xlim(-plot_limits,plot_limits) +
    ggrepel::geom_label_repel(
      data = subset(p.val_tbl, OlinkID %in% olinkid_list),
      ggplot2::aes(label = Assay, color = Threshold),
      max.overlaps=nrow(subset(p.val_tbl, OlinkID %in% olinkid_list)),
      box.padding = 1,
      show.legend = FALSE) + 
    ggplot2::geom_hline(yintercept = -log10(pval_cutoff), linetype = "dotted") + 
    OlinkAnalyze::set_plot_theme() +
    ggplot2::scale_color_manual(values = color_palette, drop = FALSE) +
    ggplot2::theme(
      axis.title = element_text(size = 12),       # Axis title size
      axis.text = element_text(size = 12),        # Axis tick label size
      legend.text = element_text(size = 12),      # Legend text size
      legend.title = element_text(size = 12)     # Legend title size
    )
  
  return(volcano_plot)
}

######

posthoc_statistics <- function(
  df,
  posthoc_effect = NULL,
  posthoc_effect_formula = NULL,  
  panel_col = "all",
  test_col = NULL,
  variable = NULL,
  olink_list = NULL,
  covariate_val =  NULL,
  posthoc_model_formula = NULL,
  random_list = NULL,
  return_mean = FALSE,
  posthoc_padj_method = NULL,
  posthoc_test = NULL
){
  
  df <- as.data.frame(df)
  
  
  if(panel_col != "all"){
    df <- df %>% dplyr::filter(Panel == panel_col)
  }
  
  df <- df %>%
    dplyr::filter(!(grepl("control|ctrl", SampleID, ignore.case = TRUE))) %>%
    dplyr::filter(!(grepl("control|ctrl", Assay, ignore.case = TRUE)))
  
  if(test_col == "olink_anova"){
    
    if(is.null(posthoc_model_formula) | posthoc_model_formula == ""){
      if(is.null(posthoc_effect)){
        # print("anova option: 1")
        verbose_msg <- capture.output(
          out <- OlinkAnalyze::olink_anova_posthoc(
          df = df,
          olinkid_list = olink_list,
          variable = variable,
          covariates = covariate_val,
          effect_formula = posthoc_effect_formula,
          mean_return = as.logical(return_mean),
          post_hoc_padjust_method = posthoc_padj_method,
          verbose = TRUE
        ),
        type = "message"
      )
      } else {
        # print("anova option: 2")
        verbose_msg <- capture.output(
          out <- OlinkAnalyze::olink_anova_posthoc(
          df = df,
          olinkid_list = olink_list,
          variable = variable,
          covariates = covariate_val,
          effect = posthoc_effect,
          mean_return = as.logical(return_mean),
          post_hoc_padjust_method = posthoc_padj_method,
          verbose = TRUE
        ),
        type = "message"
        )
        
      }
    } else {
      if(is.null(posthoc_effect)){
        # print("anova option: 3")
        verbose_msg <- capture.output(
        out <- OlinkAnalyze::olink_anova_posthoc(
          df = df,
          olinkid_list = olink_list,
          model_formula = posthoc_model_formula,
          effect_formula = posthoc_effect_formula,
          mean_return = as.logical(return_mean),
          post_hoc_padjust_method = posthoc_padj_method,
          verbose = TRUE
        ),
        type = "message"
        )
      } else {
        # print("anova option: 4")
        verbose_msg <- capture.output(
          out <- OlinkAnalyze::olink_anova_posthoc(
          df = df,
          olinkid_list = olink_list,
          model_formula = posthoc_model_formula,
          effect = posthoc_effect,
          mean_return = as.logical(return_mean),
          post_hoc_padjust_method = posthoc_padj_method,
          verbose = TRUE
        ),
        type = "message"
        )
        
      }
    }
    
  }
  
  if(test_col == "olink_lmer"){
    
    if(is.null(posthoc_model_formula) | posthoc_model_formula == ""){
      if(is.null(posthoc_effect)){
        # print("lmer option: 1")
        verbose_msg <- capture.output(
        out <- OlinkAnalyze::olink_lmer_posthoc(
          df = df,
          olinkid_list = olink_list,
          variable = variable,
          covariates = covariate_val,
          random = random_list,
          effect_formula = posthoc_effect_formula,
          mean_return = as.logical(return_mean),
          post_hoc_padjust_method = posthoc_padj_method,
          verbose = TRUE
        ),
        type = "message"
        )
      } else {
        # print("lmer option: 2")
        verbose_msg <- capture.output(
        out <- OlinkAnalyze::olink_lmer_posthoc(
          df = df,
          olinkid_list = olink_list,
          variable = variable,
          covariates = covariate_val,
          random = random_list,
          effect = posthoc_effect,
          mean_return = as.logical(return_mean),
          post_hoc_padjust_method = posthoc_padj_method,
          verbose = TRUE
        ),
        type = "message"
        )
        
      }
    } else {
      if(is.null(posthoc_effect)){
        # print("lmer option: 3")
        verbose_msg <- capture.output(
          out <- OlinkAnalyze::olink_lmer_posthoc(
          df = df,
          olinkid_list = olink_list,
          model_formula = posthoc_model_formula,
          random = random_list,
          effect_formula = posthoc_effect_formula,
          mean_return = as.logical(return_mean),
          post_hoc_padjust_method = posthoc_padj_method,
          verbose = TRUE
        ),
        type = "message"
        )

      } else {
        # print("lmer option: 4")
        verbose_msg <- capture.output(
        out <- OlinkAnalyze::olink_lmer_posthoc(
          df = df,
          olinkid_list = olink_list,
          model_formula = posthoc_model_formula,
          random = random_list,
          effect = posthoc_effect,
          mean_return = as.logical(return_mean),
          post_hoc_padjust_method = posthoc_padj_method,
          verbose = TRUE
        ),
        type = "message"
        )
      }
    }
    
  }
  
  if(test_col == "olink_ordinalRegression"){
    
    if(is.null(posthoc_effect)){
      # print("ordinalRegression option: 1")
      verbose_msg <- capture.output(
      out <- OlinkAnalyze::olink_ordinalRegression_posthoc(
        df = df,
        olinkid_list = olink_list,
        variable = variable,
        covariates = covariate_val,
        effect_formula = posthoc_effect_formula,
        mean_return = as.logical(return_mean),
        post_hoc_padjust_method = posthoc_padj_method,
        verbose = TRUE
      ),
      type = "message"
      )

    } else {
      # print("ordinalRegression option: 2")
      verbose_msg <- capture.output(
      out <- OlinkAnalyze::olink_ordinalRegression_posthoc(
        df = df,
        olinkid_list = olink_list,
        variable = variable,
        covariates = covariate_val,
        effect = posthoc_effect,
        mean_return = as.logical(return_mean),
        post_hoc_padjust_method = posthoc_padj_method,
        verbose = TRUE
      ),
      type = "message"
      )
    }
  }
  
  if(test_col == "olink_one_non_parametric"){
    
      # print("one_non_parametric option: 1")
    verbose_msg <- capture.output(
      out <- OlinkAnalyze::olink_one_non_parametric_posthoc(
        df = df,
        olinkid_list = olink_list,
        variable = variable,
        test = posthoc_test,
        verbose = TRUE
      ),
      type = "message"
      )
  }
  
  
  return(list(out, verbose_msg))
  
}

outlier_detection_plot <- function(
    method2use,
    df2check4outlier,
    panel = "all",
    color = "QC_Warning",
    x_value = 1L,
    y_value = 2L,
    label_samples_logical = FALSE,
    drop_assays_logical = FALSE,
    drop_samples_logical = FALSE,
    byPanel_logical = FALSE,
    outlierDefX_val = as.numeric(NA),
    outlierDefY_val = as.numeric(NA),
    outlierLines_logical = FALSE,
    label_outliers_logical = TRUE,
    IQR_outlierDef_val = 3,
    median_outlierDef_val = 3,
    facetNrow_val = NULL,
    facetNcol_val = NULL
){
  
  if(panel != "all"){
    df2check4outlier <- df2check4outlier %>% dplyr::filter(Panel == panel)
  }
  
  df2check4outlier <- df2check4outlier %>% 
    dplyr::filter(!(grepl("control|ctrl", SampleID, ignore.case = TRUE))) %>%
    dplyr::filter(!(grepl("control|ctrl", Assay, ignore.case = TRUE)))

  set.seed(1234)
  
  if(method2use == "olink_umap_plot"){
    plot_out <- OlinkAnalyze::olink_umap_plot(
      df = df2check4outlier,
      color_g = as.character(color),
      x_val = as.integer(x_value),
      y_val = as.integer(y_value),
      label_samples = as.logical(label_samples_logical),
      drop_assays = as.logical(drop_assays_logical),
      drop_samples = as.logical(drop_samples_logical),
      byPanel = as.logical(byPanel_logical),
      outlierDefX = as.numeric(outlierDefX_val),
      outlierDefY = as.numeric(outlierDefY_val),
      outlierLines = as.logical(outlierLines_logical),
      label_outliers = as.logical(label_outliers_logical),
      quiet = TRUE
    )
    
    plot_out <- patchwork::wrap_plots(plot_out) + plot_layout(guides = "collect")
    
  }  
  
  # print(str(df2check4outlier))
  
  if(method2use == "olink_pca_plot"){
    plot_out <- OlinkAnalyze::olink_pca_plot(
      df = df2check4outlier,
      color_g = as.character(color),
      x_val = as.integer(x_value),
      y_val = as.integer(y_value),
      label_samples = as.logical(label_samples_logical),
      drop_assays = as.logical(drop_assays_logical),
      drop_samples = as.logical(drop_samples_logical),
      byPanel = as.logical(byPanel_logical),
      outlierDefX = as.numeric(outlierDefX_val),
      outlierDefY = as.numeric(outlierDefY_val),
      outlierLines = as.logical(outlierLines_logical),
      label_outliers = as.logical(label_outliers_logical),
      quiet = TRUE
    )
    plot_out <- patchwork::wrap_plots(plot_out) + plot_layout(guides = "collect")

  }  
      
  if(method2use == "olink_qc_plot"){
    plot_out <- OlinkAnalyze::olink_qc_plot(
      df = df2check4outlier,
      color_g = !!sym(color),
      plot_index = FALSE,
      label_outliers = as.logical(label_outliers_logical),
      IQR_outlierDef = as.numeric(IQR_outlierDef_val),
      median_outlierDef = as.numeric(median_outlierDef_val),
      outlierLines = as.logical(outlierLines_logical),
      facetNrow = facetNrow_val,
      facetNcol = facetNcol_val
    )
  }
  
  if(method2use %in% c("olink_pca_plot", "olink_umap_plot")){
    if (is.list(plot_out)) {
      p_out_table <- data.frame()
      for (p in seq(1, length(plot_out))) {
        curr_outlier_table <- plot_out[[p]]$data %>% dplyr::filter(Outlier == 1)
        if (nrow(curr_outlier_table) > 0) {
          p_out_table <- rbind(p_out_table, curr_outlier_table)
        }
      }
    }
  } else {
    p_out_table <- plot_out$data %>% dplyr::filter(Outlier==1)
  }
  
  if((panel != "all") && (!("Panel" %in% names(p_out_table)))){
    p_out_table["Panel"] <- panel
  }
  
  
  
return(list(plot_out, p_out_table))
  
}

clean_yticks <- function(p, m){
  
  p_details <- ggplot2::ggplot_build(p)
  ybreaks <- p_details$layout$panel_params[[1]]$y$breaks
  new_ybreaks <- comprehenr::to_vec(for(b in ybreaks) gsub("_", " ", b))
  if(m == "olink_pathway_heatmap"){
    return(p + scale_y_discrete(labels = new_ybreaks))
  } else {
    return(p + scale_x_discrete(labels = new_ybreaks))
  }

  
}



generate_complex_heatmap <- function(
    data, 
    group = "Treatment", 
    panel = "Olink Inflammation", 
    scale_rows = TRUE, 
    cluster_rows = TRUE,
    cluster_columns = FALSE,
    show_row_names = TRUE,
    show_column_names = FALSE,
    color_palette = c("#2166ac", "white", "#b2182b")) {
 
  data <- data %>%
    dplyr::filter(Panel == panel) %>%
    dplyr::select(-Panel) %>%
    as.data.frame() %>% 
    dplyr::filter(!grepl("control|ctrl",SampleID, ignore.case = TRUE))  %>%
    dplyr::arrange(!!sym(group))
  
  
  levels_of_group <- levels(factor(data[[group]]))
  data[[group]] <- factor(data[[group]])

  if (!all(c("SampleID", "Assay", group, "NPX") %in% colnames(data))) {
    stop("Data must contain 'SampleID', 'Assay', and 'NPX'",group," columns.")
  }
  

  
  # Pivot data into wide format (genes as rows, samples as columns)
  heatmap_data <- data %>%
    dplyr::select(SampleID, Assay, NPX) %>%
    tidyr::pivot_wider(names_from = SampleID, values_from = NPX) %>%
    as.data.frame()
  
  row.names(heatmap_data) <- heatmap_data[["Assay"]]
  
  heatmap_data <- heatmap_data %>%
    dplyr::select(-Assay)
  
  # Extract sample groups for annotation
  sample_groups <- data %>%
    dplyr::distinct(SampleID, !!sym(group)) %>%
    dplyr::arrange(!!sym(group))

  
  # Convert to matrix
  mat <- as.matrix(heatmap_data%>% na.omit())
  
  # Scale rows if specified
  if (scale_rows) {
    mat <- t(scale(t(mat)))
  }
  
  heatmap_colors <- circlize::colorRamp2(
    c(min(mat, na.rm = TRUE), 0, max(mat, na.rm = TRUE)),
    color_palette)
  
  # Define color function
  group_levels <- levels_of_group
  group_colors <- setNames(
    RColorBrewer::brewer.pal(max(3,length(group_levels)), "Set2")[seq_len(length(group_levels))],
    group_levels
  )

  
  col_anno <- HeatmapAnnotation(
    group = sample_groups[[group]],
    name = group,
    col = list(group = group_colors),
    annotation_name_side = "left"
    )
    
  # Generate heatmap
  Heatmap(
    mat,
    name = "NPX",
    cluster_rows = cluster_rows,
    cluster_columns = cluster_columns,
    show_row_names = show_row_names,
    show_column_names = show_column_names,
    col = heatmap_colors,
    top_annotation = col_anno
  )
}

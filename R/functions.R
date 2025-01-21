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

plot_msg <- function(msg) {
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
    dplyr::filter(!stringr::str_detect(SampleID, stringr::regex("control|ctrl", ignore_case = TRUE))) %>%
    dplyr::filter(!stringr::str_detect(Assay, stringr::regex("control|ctrl", ignore_case = TRUE))) %>%
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
    dplyr::filter(!stringr::str_detect(SampleID, stringr::regex("control|ctrl", ignore_case = TRUE))) %>%
    dplyr::filter(!stringr::str_detect(Assay, stringr::regex("control|ctrl", ignore_case = TRUE))) %>%
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
      theme(
        plot.title = element_text(size = 24),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(color = "#000000", size = 18),
        axis.text.y = element_text(color = "#000000", size = 18)
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
    dplyr::select(SampleID,
                  OlinkID) |>
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
  print(test_col)
  
  if(panel_col != "all"){
    df <- df %>% dplyr::filter(Panel == panel_col)
  }
  
  df <- df %>%
    dplyr::filter(!(grepl("control|ctrl", SampleID, ignore.case = TRUE))) %>%
    dplyr::filter(!(grepl("control|ctrl", Assay, ignore.case = TRUE)))
  
  if(test_col == "olink_ttest"){
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




rm(list = ls())

list_of_packages1 <- c(
  "shiny",
  "DT",
  "comprehenr",
  "dplyr",
  "readr",
  "stringr",
  "tidyverse",
  "tidyr",
  "ggplot2", 
  "plotly",
  "webshot",
  "this.path",
  "shinyBS",
  "bslib",
  "plotly",
  "OlinkAnalyze",
  "circlize",
  "ComplexHeatmap",
  "tools",
  "FSA",
  "gbRd",
  "msigdbr",
  "patchwork"
)


for(pkg1 in list_of_packages1){
  library(pkg1, character.only = TRUE)
}


source("R/functions.R")

options(shiny.maxRequestSize=160*1024^2)

ui <- fluidPage(
  uiOutput("olink_data"),
  tags$footer(
    div(
      style = "position: fixed; bottom: 0; width: 100%; background-color: #f8f9fa; padding: 10px; text-align: center; border-top: 1px solid #ddd;",
      "© 2025 Debojyoti Das Bioinformactics Unit, Core Facility & Clinical Genomics Linköping, Linköping University"
    )
  )
)


server <- function(input, output, session) {
  
  output$olink_data <- renderUI({
    navbarPage(" ",
               id = "olink_data",
               tabPanel("Olink View", fluidPage(
                 page_sidebar(
                   "",
                   sidebar = sidebar(
                         bsCollapse(id = "sidebar_collapse_tab1", open = c("Data Uploading"), multiple = TRUE,
                         bsCollapsePanel("Data Uploading", style = "primary",
                           div(
                             style = "display: flex; align-items: center;",
                             radioButtons("use_demo_data", "Use demo data", selected = "Yes", choices = c("No" = FALSE, "Yes" = TRUE), inline = TRUE)
                           ),
                           conditionalPanel(
                             condition = "input.use_demo_data == 'FALSE'",
                               div(
                                 style = "display: flex; align-items: center;",
                                 fileInput("file", "Upload data", placeholder = "NPX data", accept = ".csv"),
                                 actionButton("info_btn", label = "i", style = "padding: 2px 2px; font-size: 12px; line-height: 1; border: 1px solid #ccc; border-radius: 4px; background-color: #f9f9f9; color: #333; margin-left: 5px; cursor: pointer;")
                               )
                           ),
                           conditionalPanel(
                             condition = "input.use_demo_data == 'FALSE'",
                           div(
                             style = "display: flex; align-items: center;",
                             fileInput("meta_file", "Upload meta data", placeholder = "sample manifest", accept = ".csv"),
                             actionButton("info_btn_meta", label = "m", style = "padding: 2px 2px; font-size: 12px; line-height: 1; border: 1px solid #ccc; border-radius: 4px; background-color: #f9f9f9; color: #333; margin-left: 5px; cursor: pointer;")
                            ),
                           shinyBS::bsPopover(id = "info_btn", title = "OLINK File Format", placement = "left", trigger = "click", options = list(container = "body"))
                           ),
                           div(style = "display: flex; gap: 0px; margin-top: 10px; margin-left: 0px;", actionButton("process_data", "Process uploaded data")),
                           uiOutput("panel_col_ui"),
                           uiOutput("group_col_ui")
                         )
                       )
                   ),
                   tabsetPanel(
                     tabPanel("Data table: raw", DT::dataTableOutput("data_uploaded")),
                     tabPanel("Data table: meta", DT::dataTableOutput("meta_uploaded")),
                     tabPanel("Data table: complete", DT::dataTableOutput("full_uploaded")),
                     tabPanel("Data table: panel metrics", DT::dataTableOutput("panel_metrics")),
                     tabPanel("Data table: group-wise panel metrics", DT::dataTableOutput("group_panel_metrics")),                     
                     tabPanel("About/ Citation", uiOutput("citation_text")),
                   )
                 )
               )),
               tabPanel("Outlier Detection", fluidPage(
                 page_sidebar(
                   "",
                   sidebar = sidebar(
                     div(style = "overflow-y: auto; max-height: 90vh;", 
                         bsCollapse(id = "sidebar_collapse_tab2", open = c("Detection Method", "Method-specific options"), multiple = TRUE,
                                    bsCollapsePanel("Detection Method", style = "primary",
                                                    uiOutput("plot_method_col_ui"),
                                                   uiOutput("outlier_color_ui"),
                                                   uiOutput("outlier_panel_ui"),
                                                   uiOutput("run_plot_ui"),                                   
                                                   uiOutput("run_plot_msg_ui")
                     ),
                     bsCollapsePanel("Method-specific options", style = "success",
                                     uiOutput("by_panel_logical_ui"),
                                     uiOutput("outlierLines_logical_ui"),
                                     uiOutput("label_samples_logical_ui"),
                                     uiOutput("label_outliers_logical_ui"),
                                     uiOutput("drop_assays_logical_ui"),
                                     uiOutput("drop_sample_logical_ui"),
                                     uiOutput("outlier_x_val_ui"),
                                     uiOutput("outlier_y_val_ui"),
                                     uiOutput("outlierDefX_val_ui"),
                                     uiOutput("outlierDefY_val_ui"),
                                     uiOutput("IQR_outlierDef_val_ui"),
                                     uiOutput("median_outlierDef_val_ui"),
                                     uiOutput("facetNrow_val_ui"),
                                     uiOutput("facetNcol_val_ui")
                     ),
                     bsCollapsePanel("Download options", style = "danger",
                                     uiOutput("run_plot_msg_ui"),
                                     uiOutput("download_options_outlier_ui"),
                                     uiOutput("download_width_outlier_ui"),
                                     uiOutput("download_height_outlier_ui"),
                                     uiOutput("download_type_outlier_ui"),
                                     uiOutput("download_plot_outlier_ui")
                                    )
                              )
                           )
                   ),
                   tabsetPanel(
                     tabPanel("Exploratory View", 
                              div(
                                style = "height: 100vh; width: 100vw; display: flex; align-items: center; justify-content: center;",
                                plotOutput("exploratory_output", width = "80%", height = "80%")
                                )
                              ),             
                     tabPanel("Outlier Table", DT::dataTableOutput("outlier_table_output")),
                     tabPanel("Documentation/ Help", htmlOutput("exploratory_help"))
                   )
                 )
               )
               ),
               tabPanel("Filter Data", fluidPage(
                 page_sidebar(
                   "",
                   sidebar = sidebar(
                         bsCollapse(id = "sidebar_collapse_tab3", open = c("Filter activation"), multiple = TRUE,
                                    bsCollapsePanel("Filter activation", style = "primary",
                                      uiOutput("filter_data_button_ui"),
                                      uiOutput("assay_ui"),
                                      uiOutput("qq_ui"),
                                      uiOutput("filter_sample_id_logical_ui"),
                                      uiOutput("filter_assay_logical_ui"),
                                      uiOutput("filter_sample_id_list_ui"),
                                      uiOutput("filter_assay_list_ui")
                                   )
                                 )
                   ),
                   tabsetPanel(
                     tabPanel("Quantile-Quantile plot", 
                              div(
                                style = "height: 100vh; width: 100vw; display: flex; align-items: center; justify-content: center;",
                                plotOutput("qq_out", width = "80%", height = "80%")
                                )
                              ),
                     tabPanel("Filtered Data", DT::dataTableOutput("filtered_data_table_output")),
                     tabPanel("Documentation/ Help", htmlOutput("filter_help"))
                   )
                 )
               )
               ),               
               tabPanel("Statistics", fluidPage(
                   page_sidebar(
                     "",
                     sidebar = sidebar(
                       div(style = "overflow-y: auto; max-height: 90vh;", 
                           bsCollapse(id = "sidebar_collapse_tab4", open = c("Method", "Parameters", "Run"), multiple = TRUE,
                                      bsCollapsePanel("Method", style = "primary",
                                                      uiOutput("use_filtered_data_logical_ui"),
                                                      uiOutput("stats_panel_col_ui"),
                                                      uiOutput("variable_col_ui"),
                                                      uiOutput("test_col_ui")
                                      ),
                                      # Model Parameters Section
                                      bsCollapsePanel("Parameters", style = "info",
                                                      uiOutput("result_pval_cutoff_ui"),
                                                      uiOutput("result_pval_col_ui"),
                                                      uiOutput("pair_id_ui"),
                                                      uiOutput("model_formula_ui"),
                                                      uiOutput("return_covariate_ui"),
                                                      uiOutput("random_effects_ui"),
                                                      uiOutput("dependence_ui"),
                                                      uiOutput("subject_ui"),
                                                      conditionalPanel(
                                                        condition = 'input.test_col %in% c("olink_ttest","olink_wilcox")',
                                                        uiOutput("pair_id_col_ui")
                                                      ),
                                                      conditionalPanel(
                                                        condition = 'input.test_col %in% c("olink_anova")',
                                                        uiOutput("covariate_col_ui")
                                                      ),
                                                      uiOutput("run_button_ui")
                                      ),
                                      # Run and Plot Options
                                      bsCollapsePanel("Plot", style = "warning",
                                                      uiOutput("volcano_pval_cutoff_ui"),
                                                      uiOutput("volcano_pval_col_ui"),
                                                      uiOutput("volcano_alternate_x_label_ui"),
                                                      uiOutput("volcano_olink_specific_logical_ui"),
                                                      uiOutput("volcano_olink_specific_list_ui"),
                                                      uiOutput("plot_button_lmer_ui"),
                                                      uiOutput("plot_olinkid_list_ui"),
                                                      uiOutput("plot_x_axis_variable_ui"),
                                                      uiOutput("plot_col_variable_ui"),
                                                      uiOutput("plot_number_of_proteins_per_plot_ui")
                                      ),
                                      # Run and Plot Options
                                      bsCollapsePanel("Heatmap", style = "success",
                                                      uiOutput("complex_heatmap_panel_col_ui"),
                                                      uiOutput("complex_heatmap_group_ui"),
                                                      uiOutput("complex_heatmap_use_significant_assay_logical_ui"),
                                                      uiOutput("complex_heatmap_cluster_rows_logical_ui"),
                                                      uiOutput("complex_heatmap_cluster_cols_logical_ui"),
                                                      uiOutput("complex_heatmap_use_rownames_logical_ui"),
                                                      uiOutput("complex_heatmap_use_colnames_logical_ui"),
                                                      uiOutput("complex_heatmap_ui")
                                      ),
                                      # Download Options
                                      bsCollapsePanel("Download Options", style = "danger",
                                                      uiOutput("stats_download_options_ui")
                                      )
                           )
                       )
                     ),     
                     #########
                     tabsetPanel(id="statistics_active_tab",
                       tabPanel("Statistical test result", DT::dataTableOutput("test_result")),
                       tabPanel("Statistical test plot", 
                                div(
                                  style = "height: 100vh; width: 100vw; display: flex; align-items: center; justify-content: center;",
                                  plotOutput("statistical_test_plot_out", height = "80%", width = "80%")
                                ),
                       ),
                       tabPanel("Statistical test log", uiOutput("test_log")),
                       tabPanel("Documentation/ Help", htmlOutput("test_help")),
                       tabPanel("Heatmap", 
                                div(
                                  style = "height: 100vh; width: 100vw; display: flex; align-items: center; justify-content: center;",
                                  plotOutput("complex_heatmap_output", width = "80%", height = "80%")),
                               ),
                       tabPanel("ComplexHeatmap Documentation/ Help", htmlOutput("complex_heatmap_help")),
                       tabPanel("lmer visualization", 
                                  div(
                                    style = "height: 100vh; width: 100vw; display: flex; align-items: center; justify-content: center;",
                                    plotOutput("olink_lmer_plot_output", width = "80%", height = "80%")
                                    )
                                  )
                         
                     )
                   )
                 )
               ),
               tabPanel("Post-hoc Statistics", fluidPage(
                 page_sidebar(
                   "",
                   sidebar = sidebar(
                     div(style = "overflow-y: auto; max-height: 90vh;", 
                         bsCollapse(id = "sidebar_collapse_tab5", multiple = TRUE,
                                    bsCollapsePanel("posthoc options", style = "primary",   
                                                    uiOutput("run_posthoc_ui"),
                                                    uiOutput("posthoc_variable_ui"),
                                                    uiOutput("use_olink_ids_ui"),
                                                    uiOutput("filter_term_olink_ids_ui"),
                                                    uiOutput("olink_ids_ui"),
                                                    uiOutput("posthoc_random_ui"),
                                                    uiOutput("posthoc_effect_ui"),
                                                    uiOutput("posthoc_covariates_ui"),
                                                    uiOutput("posthoc_mean_return_ui"),
                                                    uiOutput("posthoc_padjt_method_ui"),
                                                    uiOutput("posthoc_test_ui")
                                    ),
                                    bsCollapsePanel("pass formula (optional)", style = "danger",                     
                                                    uiOutput("posthoc_model_formula_ui"),
                                                    uiOutput("posthoc_effect_formula_ui")
                                    )
                         )
                     )
                   ),
                   tabsetPanel(
                     tabPanel("Posthoc Statistical test result", DT::dataTableOutput("posthoc_test_result")),
                     tabPanel("Posthoc Statistical test log", uiOutput("posthoc_test_log")),
                     tabPanel("Documentation/ Help", htmlOutput("posthoc_test_help"))
                   )
                 )
               )
               ),
               tabPanel("Pathway Enrichment", fluidPage(
                 page_sidebar(
                   "",
                   sidebar = sidebar(
                     div(style = "overflow-y: auto; max-height: 90vh;",
                         bsCollapse(id = "sidebar_collapse_tab6", open = c("enrichment options","visualization options"), multiple = TRUE,
                                    bsCollapsePanel("enrichment options", style = "primary",
                                                    uiOutput("run_pathway_enrichment_ui"),
                                                    uiOutput("pathway_enrichment_method_ui"),
                                                    uiOutput("pathway_enrichment_ontology_ui"),
                                                    uiOutput("pathway_enrichment_organism_ui"),
                                                    uiOutput("pathway_enrichment_contrast_ui"),
                                                    uiOutput("pathway_enrichment_pvalue_cutoff_ui"),
                                                    uiOutput("pathway_enrichment_estimate_cutoff_ui")
                                    ),
                                    bsCollapsePanel("visualization options", style = "success",
                                                    uiOutput("pathway_visual_mode_ui"),
                                                    uiOutput("pathway_visual_keyword_ui"),
                                                    uiOutput("pathway_visual_number_of_terms_ui"),
                                                    uiOutput("pathway_visual_run_ui")
                                    ),
                                    bsCollapsePanel("Download options", style = "danger",
                                                    uiOutput("download_pathway_visual_options_ui"),
                                                    uiOutput("download_pathway_visual_width_ui"),
                                                    uiOutput("download_pathway_visual_height_ui"),
                                                    uiOutput("download_pathway_visual_type_ui"),
                                                    uiOutput("download_pathway_visual_plot_ui")
                                    )
                                    
                         )
                     )
                   ),
                   tabsetPanel(
                     tabPanel("Pathway Enrichment Result", DT::dataTableOutput("pathway_enrichment_result")),
                     tabPanel("Pathway Enrichment Log", uiOutput("pathway_enrichment_log")),
                     tabPanel("Documentation/ Help", htmlOutput("pathway_enrichment_help")),
                     tabPanel("Pathway Enrichment Plot", plotOutput("pathway_visual_plot")),
                     tabPanel("Plot Documentation/ Help", htmlOutput("pathway_visual_help"))
                   )
                   )
                 )
               ),
               tabPanel("Additional Visualization", fluidPage(
                 page_sidebar(
                   "",
                   sidebar = sidebar(
                     div(style = "overflow-y: auto; max-height: 90vh;", 
                         bsCollapse(id = "sidebar_collapse_tab7", open = c("plot options"), multiple = TRUE,
                            bsCollapsePanel("Plot Options", style = "primary",                        
                              uiOutput("statistical_test_boxplot_variable_ui"),
                              uiOutput("statistical_test_boxplot_olink_ui"),
                              uiOutput("statistical_test_boxplot_olink_use_test_result_ui"),
                              uiOutput("statistical_test_boxplot_number_ui"), 
                              uiOutput("statistical_test_boxplot_run_ui")
                         ),
                        bsCollapsePanel("Download options", style = "danger",                        
                            uiOutput("download_options_ui"),
                            uiOutput("download_width_ui"),
                            uiOutput("download_height_ui"),
                            uiOutput("download_type_ui"),
                            uiOutput("download_plot_ui")
                        )
                       )
                      )
                   ),
                   tabsetPanel(
                     tabPanel("Boxplot", 
                              div(
                                style = "height: 100vh; width: 100vw; display: flex; align-items: center; justify-content: center;",
                                plotOutput("statistical_test_boxplot_out", height = "80%", width = "80%")
                              )
                     ),
                     tabPanel("Documentation/ Help", htmlOutput("boxplot_help"))
                   )
                 )
               )
               )
    )
  })
  
  # info button
  observeEvent(req(input$info_btn), {
    showModal(modalDialog(
      title = "OLINK File Format",
      HTML(
        "<b>Expected columns:</b><br>
        -SampleID<br>
        -Sample_Type<br>
        -Index<br>
        -OlinkID<br>
        -UniProt<br>
        -Assay<br>
        -MissingFreq<br>
        -Panel<br>
        -Panel_Lot_Nr<br>
        -PlateID<br>
        -QC_Warning<br>
        -LOD<br>
        -NPX<br>
        -Normalization<br>
        -Assay_Warning<br>
        -ExploreVersion"
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(req(input$info_btn_meta), {
    showModal(modalDialog(
      title = "Meta data File Format",
      HTML(
        "<b>Expected columns:</b><br>
        -SampleID (mandatory)<br>
        -Variable1(string without special character and space)<br>
        -Variable2(string without special character and space)<br>
        -Variable3(string without special character and space)<br>
        .<br>
        .<br>
        .<br>
        -Variablen(string without special character and space)"
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  output$citation_text <- renderUI({
    HTML("
      <p><strong>This shiny app is based on the OlinkAnalyze R package 4.0.1.</strong> Please cite the original work (latest version at the time of release of this app) if you use this application for your research work.</p>
      
      <hr>

      <h4>Citation:</h4>
      <p>To cite package <em>OlinkAnalyze</em> in publications use:</p>
      <p><strong>Nevola K, Sandin M, Guess J, Forsberg S, Cambronero C, Pucholt P, Zhang B, Sheikhi M, Diamanti K, Kar A, Conze L, Chin K, Topouza D (2025).</strong> <em>OlinkAnalyze: Facilitate Analysis of Proteomic Data from Olink</em>. R package version 4.1.2, <a href='https://github.com/olink-proteomics/olinkrpackage' target='_blank'>https://github.com/olink-proteomics/olinkrpackage</a>.</p>

      <hr>

      <h4>BibTeX Entry:</h4>
      <pre>
@Manual{,
  title = {OlinkAnalyze: Facilitate Analysis of Proteomic Data from Olink},
  author = {Kathleen Nevola and Marianne Sandin and Jamey Guess and
    Simon Forsberg and Christoffer Cambronero and Pascal Pucholt and
    Boxi Zhang and Masoumeh Sheikhi and Klev Diamanti and Amrita Kar
    and Lei Conze and Kristyn Chin and Danai Topouza},
  year = {2025},
  note = {R package version 4.1.2},
  url = {https://github.com/olink-proteomics/olinkrpackage},
}
      </pre>
    ")
  })
  
  # Reactive expression to read the uploaded file
  data <- reactive({
    req(input$use_demo_data)
    if (as.logical(input$use_demo_data)) {
      OlinkAnalyze::read_NPX("data/test_data.csv")
    } else {
      req(input$file,input$process_data)
      OlinkAnalyze::read_NPX(input$file$datapath)
    }
  })

  
  # Generate and display data tables
  output$data_uploaded <- DT::renderDataTable({
    req(input$use_demo_data)
    if(as.logical(input$use_demo_data)){
      req(data())      
    } else {
      req(input$file, input$process_data, data())      
    }

    validate(
      need(npxCheck(data()), 'Check input NPX file!'),
    )
    datatable(
      data(),
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        filter = 'top' # Enable column filters
      ),
      filter = 'top'
    )
  })
  
  meta_data <- reactive({
    req(input$use_demo_data)
    if (as.logical(input$use_demo_data)) {
      readr::read_csv("data/test_manifest.csv", col_types = c(SampleID = "character"))
    } else {
    req(input$meta_file, input$process_data)
    readr::read_csv(input$meta_file$datapath, col_types = c(SampleID = "character"))
    }
  })
  
  # Generate and display data tables
  output$meta_uploaded <- DT::renderDataTable({
    req(input$use_demo_data)
    if(as.logical(input$use_demo_data)){
      req(meta_data())      
    } else {
      req(input$meta_file, input$process_data, meta_data())      
    }

    datatable(
      meta_data(),
      filter = "top", 
      extensions = 'Buttons', 
      options = list(
        dom = 'Bflrtip',
        buttons = list('copy', 'csv', 'excel', list(
          extend = "collection",
          text = 'Show All',
          action = DT::JS("function ( e, dt, node, config ) {
                                    dt.page.len(-1);
                                    dt.ajax.reload();
                                }")
         )
        )
      )
    )
  }) 
  
  full_data <- reactive({
    req(input$use_demo_data)
    if(as.logical(input$use_demo_data)){
      req(data(), meta_data())      
    } else {
      req(input$file, input$meta_file, input$process_data, data(), meta_data())      
    }
    dplyr::left_join(data(), meta_data(), by = "SampleID")
  })
  
  output$hasData <- reactive({
    req(full_data())
    !is.null(full_data()) && ncol(full_data()) > 0
  })
  
  output$qq_ui <- renderUI({
    req(full_data())
    actionButton("go_qq", "Plot QQ")
  })
  
  
  output$full_uploaded <- DT::renderDataTable({
    req(full_data(), input$process_data)
    validate(
      need(npxCheck(full_data()), 'Check input NPX file!'),
    )
    datatable(
      full_data(),
      filter = "top", 
      extensions = 'Buttons', 
      options = list(
        dom = 'Bflrtip',
        buttons = list('copy', 'csv', 'excel', list(
          extend = "collection",
          text = 'Show All',
          action = DT::JS("function ( e, dt, node, config ) {
                                    dt.page.len(-1);
                                    dt.ajax.reload();
                                }")
        )
        )
      )
      # options = list(
      #   pageLength = 10,
      #   autoWidth = TRUE,
      #   filter = 'top' # Enable column filters
      # ),
      # filter = 'top'
    )
  }) 
  
  
  panel_metrics <- reactive({
    req(full_data(), input$panel_col)
    panel_wise_metrics_for_assay_detection(full_data(), input$panel_col)
  })
  
  output$panel_metrics <- DT::renderDataTable({
    req(panel_metrics(), input$panel_col)
    datatable(
      panel_metrics(),
      caption = paste0(input$panel_col," panel"),
      filter = "top", 
      extensions = 'Buttons', 
      options = list(
        dom = 'Bflrtip',
        buttons = list('copy', 'csv', 'excel', list(
          extend = "collection",
          text = 'Show All',
          action = DT::JS("function ( e, dt, node, config ) {
                                    dt.page.len(-1);
                                    dt.ajax.reload();
                                }")
        )
        )
      )
    )
  }) 
  
  group_specific_panel_assay_metrics <- reactive({
    req(full_data(), input$panel_col, input$group_col)
    group_specific_panel_assay_detection_percentage(full_data(), input$panel_col, input$group_col)
  })
  
  output$group_panel_metrics <- DT::renderDataTable({
    req(group_specific_panel_assay_metrics(), input$panel_col, input$group_col)
    datatable(
      group_specific_panel_assay_metrics(),
      caption = paste0("panel: ",input$panel_col,"\nGroup: ",input$group_col),
      filter = "top", 
      extensions = 'Buttons', 
      options = list(
        dom = 'Bflrtip',
        buttons = list('copy', 'csv', 'excel', list(
          extend = "collection",
          text = 'Show All',
          action = DT::JS("function ( e, dt, node, config ) {
                                    dt.page.len(-1);
                                    dt.ajax.reload();
                                }")
        )
        )
      )
    )
  }) 
  

  
  # Generate UI for selecting panel
  output$panel_col_ui <- renderUI({
    req(full_data())
    panels <- full_data() %>% dplyr::distinct(Panel) %>% dplyr::pull(Panel)
    panels <- na.omit(panels)  
    selectInput("panel_col", "Select panel", choices = c("all", panels))
  })
  
  output$group_col_ui <- renderUI({
    req(full_data())
    selectInput("group_col", "Select group column", choices = setdiff(colnames(full_data()),not_sample_label))
  })
  

  # Outlier detection page
  output$plot_method_col_ui <- renderUI({
    req(full_data())
    selectInput("outlier_plot_method", "Select plot", choices = list_of_outlier_plots)
  })
  
  output$run_plot_ui <- renderUI({
    req(full_data(), input$outlier_plot_method)
    actionButton("run_outlier_plot", paste0("Run ", input$outlier_plot_method))
  })
  
  output$run_plot_msg_ui <- renderUI({
    req(full_data(), input$outlier_plot_method == "olink_umap_plot")
      hr("UMAP is a stochastic method -each run might be different")
  })
  
  
  output$outlier_color_ui <- renderUI({
    req(full_data())
    selectInput("outlier_color", "Select color", choices = setdiff(names(full_data()), not_sample_label), selected = "QC_Warning")
  })
  
  output$outlier_panel_ui <- renderUI({
    req(full_data(), input$outlier_plot_method)
    selectInput("outlier_panel", "Select panel", choices = c("all", full_data() %>% dplyr::distinct(Panel) %>% dplyr::pull(Panel)))
  })
  
  output$outlierLines_logical_ui <- renderUI({
    req(full_data(), input$outlier_plot_method)
    radioButtons("outlierLines_logical", "Draw outlier Lines", choices = c(FALSE, TRUE))
  })
  
  output$label_samples_logical_ui <- renderUI({
    req(full_data(), input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot"))
    radioButtons("label_samples_logical", "Use SampleID", choices = c(FALSE, TRUE))
  })
  
  output$label_outliers_logical_ui <- renderUI({
    req(full_data(), input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot", "olink_qc_plot"))
    radioButtons("label_outliers_logical", "Label outliers", choices = c(TRUE, FALSE))
  })
  
  output$drop_assays_logical_ui <- renderUI({
    req(full_data(), input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot"))
    radioButtons("drop_assays_logical", "Drop assay with missing values", choices = c(FALSE, TRUE))
  })
  
  output$drop_sample_logical_ui <- renderUI({
    req(full_data(), input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot"))
    radioButtons("drop_sample_logical", "Drop sample with missing values", choices = c(FALSE, TRUE))
  })
  
  output$by_panel_logical_ui <- renderUI({
    req(full_data(), input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot"))
    radioButtons("by_panel_logical", paste0(gsub("_"," ", input$outlier_plot_method)," per panel"), choices = c(TRUE,FALSE))
  })
  
  output$outlier_x_val_ui <- renderUI({
    req(full_data(), input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot"))
    selectInput("outlier_x_val", "Select x-axis", choices = seq(1L:10L), selected = 1L)
  })
  
  output$outlier_y_val_ui <- renderUI({
    req(full_data(), input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot"))
    selectInput("outlier_y_val", "Select y-axis", choices = seq(1L:10L), selected = 2L)
  })
  
  output$outlierDefX_val_ui <- renderUI({
    req(full_data(), input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot"))
    numericInput("outlierDefX_val", "number standard deviations along the PC on x-axis", value = 2, min = 0, max = 20)
  })
  
  output$outlierDefY_val_ui <- renderUI({
    req(full_data(), input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot"))
    numericInput("outlierDefY_val", "number standard deviations along the PC on y-axis", value = 2, min = 0, max = 20)
  })
  
  output$IQR_outlierDef_val_ui <- renderUI({
    req(full_data(), input$outlier_plot_method %in% c("olink_qc_plot"))
    numericInput("IQR_outlierDef_val", "Std. from IQR", value = 3, min = 0, max = 20)
  })
  
  output$median_outlierDef_val_ui <- renderUI({
    req(full_data(), input$outlier_plot_method %in% c("olink_qc_plot"))
    numericInput("median_outlierDef_val", "Std. from mean", value = 3, min = 0, max = 20)
  })
  
  output$facetNrow_val_ui <- renderUI({
    req(full_data(), input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot", "olink_qc_plot"))
    numericInput("facetNrow_val", "rows for panel", value = 1, min = 1, max = 10)
  })
  
  output$facetNcol_val_ui <- renderUI({
    req(full_data(), input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot", "olink_qc_plot"))
    numericInput("facetNcol_val", "columns for panel", value = 2, min = 1, max = 10)
  })

  observeEvent(input$run_outlier_plot, {})
  
  # Reactive values to track the plot
  rv_outlier <- reactiveValues(plot = NULL)
  
  outlier_detection_plot_output <- reactive({
      req(
      full_data(), 
      input$outlier_plot_method, 
      input$outlier_color,  
      input$outlier_panel, 
      input$run_outlier_plot,
      input$outlierLines_logical,
      input$label_outliers_logical
    )
    if(input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot")){
      req(
        input$outlier_x_val, 
        input$outlier_y_val, 
        input$drop_assays_logical, 
        input$drop_sample_logical, 
        input$by_panel_logical, 
        input$outlierDefX_val,
        input$outlierDefY_val,    
        input$label_samples_logical
      )
    } 
    if(input$outlier_plot_method %in% c("olink_qc_plot")){
      req(
        input$IQR_outlierDef_val,
        input$median_outlierDef_val,
        input$facetNrow_val,
        input$facetNcol_val
      )
    }     
    
    outlier_detection_plot(
      method2use = input$outlier_plot_method,
      df2check4outlier = full_data(),
      panel = input$outlier_panel,
      color = input$outlier_color,
      x_value = if(input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot")) input$outlier_x_val else 1L,
      y_value = if(input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot")) input$outlier_y_val else 2L,
      label_samples_logical = if(input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot")) input$label_samples_logical else FALSE,
      drop_assays_logical = if(input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot")) input$drop_assays_logical else FALSE,
      drop_samples_logical = if(input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot")) input$drop_assays_logical else FALSE,
      byPanel_logical = if(input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot")) input$by_panel_logical else FALSE,
      outlierDefX_val = if(input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot")) input$outlierDefX_val else as.numeric(NA),
      outlierDefY_val = if(input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot")) input$outlierDefY_val else as.numeric(NA),
      outlierLines_logical = input$outlierLines_logical,
      label_outliers_logical = input$label_outliers_logical,
      IQR_outlierDef_val = if(input$outlier_plot_method %in% c("olink_qc_plot")) input$IQR_outlierDef_val else 3,
      median_outlierDef_val = if(input$outlier_plot_method %in% c("olink_qc_plot")) input$median_outlierDef_val else 3,
      facetNrow_val = input$facetNrow_val,
      facetNcol_val = input$facetNcol_val
    )
    })
    
    output$exploratory_output <- renderPlot({
    req(outlier_detection_plot_output())
      
    rv_outlier$plot <- outlier_detection_plot_output()[[1]] 
    outlier_detection_plot_output()[[1]]
  })
  
  outlier_table <- reactive({
    req(outlier_detection_plot_output())
    outlier_detection_plot_output()[[2]]
  })
  
  output$outlier_table_output <- DT::renderDataTable({
    req(outlier_table())
    datatable(
      outlier_table() %>% 
          as.data.frame() %>%
          dplyr::mutate(across(where(is.numeric), ~ round(., 3))),
        caption = gsub("_"," ",input$test_col),
        filter = "top", 
        extensions = 'Buttons', 
        options = list(
          dom = 'Bflrtip',
          buttons = list('copy', 'csv', 'excel', list(
            extend = "collection",
            text = 'Show All',
            action = DT::JS("function ( e, dt, node, config ) {
                                    dt.page.len(-1);
                                    dt.ajax.reload();
                                }")
           )
          )
        )
      )
    }) 
  
  assay_list <- reactive({
    req(full_data(), input$panel_col)
    df_raw <- full_data()
    if(input$panel_col != "all"){
      df_raw <- df_raw %>% dplyr::filter(Panel == input$panel_col)
    }
    unique(
      df_raw %>%
        dplyr::filter((!(grepl("control|ctrl", SampleID, ignore.case=TRUE)))  & (!(grepl("control|ctrl", Assay, ignore.case=TRUE)))) %>%
        dplyr::distinct(Assay) %>%
        dplyr::pull(Assay)
    )
  })
  
  output$assay_ui <- renderUI({
    req(full_data(), input$panel_col, assay_list())
    selectInput("curr_assay","Select assay",choices = assay_list(), selected = NULL,multiple = FALSE)
  })
  
  

  
  # outlier detection plot download options
  output$download_options_outlier_ui <- renderUI({
    validate(need(is.list(outlier_detection_plot_output()), "Run outlier detection!"))
    validate(need(is.ggplot(outlier_detection_plot_output()[[1]]), "Check input parameters."))
    h4("Download Options")
  })
  
  output$download_width_outlier_ui <- renderUI({
    validate(need(is.list(outlier_detection_plot_output()), "Run outlier detection!"))
    validate(need(is.ggplot(outlier_detection_plot_output()[[1]]), "Check input parameters."))
    numericInput("download_width_outlier", "Width (inches)", value = 8, min = 1)
  })
  
  output$download_height_outlier_ui <- renderUI({
    validate(need(is.list(outlier_detection_plot_output()), "Run outlier detection!"))
    validate(need(is.ggplot(outlier_detection_plot_output()[[1]]), "Check input parameters."))
    numericInput("download_height_outlier", "Height (inches)", value = 6, min = 1)
  })
  
  output$download_type_outlier_ui <- renderUI({
    validate(need(is.list(outlier_detection_plot_output()), "Run outlier detection!"))
    validate(need(is.ggplot(outlier_detection_plot_output()[[1]]), "Check input parameters."))
    selectInput("download_type_outlier", "File Type", choices = c("pdf", "png", "jpg"))
  })
  
  output$download_plot_outlier_ui <- renderUI({
    validate(need(is.list(outlier_detection_plot_output()), "Run outlier detection!"))
    validate(need(is.ggplot(outlier_detection_plot_output()[[1]]), "Check input parameters."))
    downloadButton("download_plot_outlier", "Download Plot")
  })
  
  # Download handler
  output$download_plot_outlier <- downloadHandler(
    filename = function() {
      paste0(input$outlier_plot_method,"_", Sys.Date(), ".", input$download_type_outlier)
    },
    content = function(file) {
      ggsave(
        filename = file,
        plot = rv_outlier$plot, # Use the stored plot
        device = input$download_type_outlier,
        width = input$download_width_outlier, 
        height = input$download_height_outlier
      )
    }
  )
  
  output$exploratory_help <- renderText({
    req(input$outlier_plot_method, full_data())
    temp = Rd2HTML(Rd_fun(input$outlier_plot_method),
                   out = tempfile("docs"))
    content = read_file(temp)
    file.remove(temp)
    content
  })
    
  # Filter Data Page
  output$filter_assay_logical_ui <- renderUI({
    req(full_data())
    radioButtons("filter_assay_logical", "Filter Assay(s)", choices = c(FALSE, TRUE))
  })
  
  output$filter_assay_list_ui <- renderUI({
    req(full_data(), input$filter_assay_logical == TRUE)
    selectInput("filter_assay_list", "Select assay(s)", choices = assay_list(), multiple = TRUE)
  })

  output$filter_sample_id_logical_ui <- renderUI({
    req(full_data(), outlier_table())
    radioButtons("filter_sample_id_logical", "Filter SampleID(s)", choices = c(FALSE, TRUE))
  })
  
  output$filter_sample_id_list_ui <- renderUI({
    req(full_data(), outlier_table(), input$filter_sample_id_logical == TRUE)
    selectInput("filter_sample_id_list", "Select SampleID(s)", choices = outlier_table() %>% dplyr::pull(SampleID), multiple = TRUE)
  })
  
  output$filter_data_button_ui <- renderUI({
    req(full_data())

    if (!is.null(input$filter_sample_id_logical) && input$filter_sample_id_logical == TRUE) {
        req(!is.null(input$filter_sample_id_list)) 
    }
    
    if (!is.null(input$filter_assay_logical) && input$filter_assay_logical == TRUE) {
      req(!is.null(input$filter_assay_list))  
    }
    if((!is.null(input$filter_assay_list)) ||(!is.null(input$filter_sample_id_list))){
      actionButton("filter_data_button", "Filter data")
    }
    
  })
  
  output$qq_out <- renderPlot({
    req(full_data(), input$go_qq, input$panel_col,input$curr_assay)
    qq_ploter(
      full_data(),
      panel = input$panel_col,
      assay = input$curr_assay
    )
  }) 

  # statistical page
  # Generate UI for selecting variable for statistical test
  
  output$filter_help <- renderText({
    req(full_data())
    temp = Rd2HTML(Rd_fun(help("filter", package = "dplyr")), out = tempfile("docs"))
    content = read_file(temp)
    file.remove(temp)
    content
  })
  
  
  filtered_data <- reactive({
    req(full_data(), input$filter_data_button)
    clean_data <- full_data()
    
    if (as.logical(input$filter_sample_id_logical)) {
      req(input$filter_sample_id_list)

      if(length(input$filter_sample_id_list) > 0){
        clean_data <- clean_data %>% dplyr::filter(!(SampleID %in% input$filter_sample_id_list))
      }
    }
    
    if (as.logical(input$filter_assay_logical)) {
      req(input$filter_assay_list)  

      if(length(input$filter_assay_list) > 0){
        clean_data <- clean_data %>% dplyr::filter(!(Assay %in% input$filter_assay_list))
      }
      
    }
    
    clean_data
    
  })
  
  output$filtered_data_table_output <- DT::renderDataTable({
    req(filtered_data())
    datatable(filtered_data() %>% as.data.frame(),
      caption = "clead data",
      filter = "top", 
      extensions = 'Buttons', 
      options = list(
        dom = 'Bflrtip',
        buttons = list('copy', 'csv', 'excel', list(
          extend = "collection",
          text = 'Show All',
          action = DT::JS("function ( e, dt, node, config ) {
                                    dt.page.len(-1);
                                    dt.ajax.reload();
                                }")
        )
        )
      )
    )
  })
  
  output$use_filtered_data_logical_ui <- renderUI({
    req(full_data())
    radioButtons("use_filtered_data_logical", "Use filtered data", choices = c(FALSE, TRUE))
  })
  
  output$stats_panel_col_ui <- renderUI({
    req(full_data(), input$use_filtered_data_logical)
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
      df <- filtered_data() 
    } else {
      req(full_data())  
      df <- full_data() 
    }
    selectInput("stats_panel_col", "Select panel", choices = c("all",unique(df[["Panel"]])))
  })
  
  output$test_col_ui <- renderUI({
    req(full_data(),input$use_filtered_data_logical)
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
    } else {
      req(full_data())      
    }
    selectInput("test_col", "Select test", selected = NULL, choices = statistical_test_list)
  })   
  
  
  column_types <- reactive({
    req(input$stats_panel_col, input$use_filtered_data_logical)
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
      df <- filtered_data() 
    } else {
      req(full_data())  
      df <- full_data() 
    }
    
    df <- df %>%
      dplyr::filter(!grepl("control|ctrl", SampleID, ignore.case = TRUE)) %>%
      dplyr::filter(!grepl("control|ctrl", Assay, ignore.case = TRUE))
    
    if(input$stats_panel_col != "all"){
      df <- df %>%
        dplyr::filter(Panel == input$stats_panel_col)
    }
    
    column_classes <- sapply(df, class)
    valid_columns <- names(column_classes[column_classes %in% c("character", "factor")])
    
    valid_columns <- valid_columns[sapply(valid_columns, function(col_name) {
      nrow(df %>% dplyr::distinct(!!sym(col_name))) == 2
    })]
    # Return valid column names
    valid_columns
  })
  
  output$variable_col_ui <- renderUI({
    req(input$test_col, input$use_filtered_data_logical)
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
      df <- filtered_data() 
    } else {
      req(full_data())
      df <- full_data()
    }
    
    if (input$test_col %in% c("olink_ttest", "olink_wilcox")) {
      selectInput("variable_col", "Select variable", choices = setdiff(column_types(),not_variable_label), multiple = FALSE)
    } else {
      selectInput("variable_col", "Select variable", choices = setdiff(names(df),not_variable_label), multiple = TRUE)
    }
  })  
  
  output$pair_id_ui <- renderUI({
    req(input$test_col %in% c("olink_ttest", "olink_wilcox"), input$use_filtered_data_logical) 
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
    } else {
      req(full_data())
    }
    radioButtons("pair_id", "Pair-id", selected = FALSE, choices =  c(TRUE, FALSE))
  })
  
  output$result_pval_cutoff_ui <- renderUI({
    req(
      input$variable_col
      # input$test_col %in% c("olink_ttest", "olink_wilcox")
    )
    numericInput("result_pval_cutoff", "P-value cutoff", min = 0, max = 1, value = 0.05)
  })
  
  output$result_pval_col_ui <- renderUI({
    req(
      input$variable_col 
      # input$test_col %in% c("olink_ttest", "olink_wilcox")
    )
    selectInput("result_pval_col", "P-value column to use", choices = c("Adjusted_pval", "p.value"))
  })
  
  output$pair_id_col_ui <- renderUI({
    req(input$test_col %in% c("olink_ttest","olink_wilcox"), input$pair_id == TRUE, input$use_filtered_data_logical) 
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
      df <- filtered_data()
    } else {
      req(full_data())
      df <- full_data()
    }
    selectInput("pair_id_col", "Select pair-id column", choices = setdiff(colnames(df), not_variable_label))
  })

  output$covariate_col_ui <- renderUI({
    req(input$test_col %in% c("olink_anova", "olink_lmer", "olink_ordinalRegression"), input$use_filtered_data_logical) 
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
      df <- filtered_data()
    } else {
      req(full_data())
      df <- full_data()
    }
    
    selectInput(
      "covariate_col", 
      "Select covarite column", 
      choices = setdiff(colnames(df), not_variable_label), 
      multiple = TRUE
    )
  })
  
  output$model_formula_ui <- renderUI({
    req(input$test_col %in% c("olink_anova", "olink_lmer"), input$use_filtered_data_logical)
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
    } else {
      req(full_data())
    }
    
    textInput(
      "model_formula_text", 
      "Provide model formula", 
      value = "")
  })
  
  output$return_covariate_ui <- renderUI({
    req(input$test_col %in% c("olink_anova", "olink_lmer", "olink_ordinalRegression"), input$use_filtered_data_logical) 
    
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
    } else {
      req(full_data())
    }
    
    radioButtons("return_covariates", "Return covariates", choices = c(FALSE, TRUE))
  })
  
  output$dependence_ui <- renderUI({
    req(input$test_col %in% c("olink_one_non_parametric"), input$use_filtered_data_logical) 
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
    } else {
      req(full_data())
    }
    
    radioButtons("dependence_text", "dependence", choices = c(FALSE, TRUE))
  })
  
  output$subject_ui <- renderUI({
    req(input$test_col %in% c("olink_one_non_parametric"), input$dependence_text == TRUE, input$use_filtered_data_logical)
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
      df <- filtered_data()
    } else {
      req(full_data())
      df <- full_data()
    }
    selectInput("subject_text", "Select subject", choices = setdiff(names(df), not_variable_label), multiple = TRUE)
  })
  
  output$random_effects_ui <- renderUI({
    req(input$test_col %in% c("olink_lmer"), input$use_filtered_data_logical) 
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
      df <- filtered_data()
    } else {
      req(full_data())
      df <- full_data()
    }
    selectInput("random_effect", "Select random effects", choices = setdiff(names(df), not_variable_label), multiple = TRUE)
  })
  
  output$run_button_ui <- renderUI({
    req(input$test_col, input$use_filtered_data_logical)
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
      if(!is.null(filtered_data()) &&  !is.null(input$test_col)){
        actionButton("run_test",paste0("Run ", gsub("_"," ",input$test_col)))
      } 
    } else {
      req(full_data())
      if(!is.null(full_data()) &&  !is.null(input$test_col)){
        actionButton("run_test",paste0("Run ", gsub("_"," ",input$test_col)))
      } 
    }

  })
  
  test_output <- reactive({
    req(input$run_test)
    req(
      input$stats_panel_col,
      input$test_col, 
      input$use_filtered_data_logical
    )
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
      df <- filtered_data()
    } else {
      req(full_data())
      df <- full_data()
    }
    
    # Conditional requirements based on test_col
    if (input$test_col %in% c("olink_ttest", "olink_wilcox")) {
      req(input$variable_col, input$pair_id)  # Pair ID is required for these tests
    } else if (input$test_col %in% c("olink_anova", "olink_lmer")){
      validate(need(!is.null(input$variable_col) || input$model_formula_text != "", "No inputs provide for model creatation"))
      req(input$return_covariates)
    } else if (input$test_col %in% c("olink_ordinalRegression")){
      req(input$variable_col, input$return_covariates)
    } else if (input$test_col == "olink_one_non_parametric") {
      req(input$variable_col, input$dependence_text)
    }
    
  
    
    test_result_output <- tryCatch(
      {
        statistical_test(
          df = df,
          panel_col = input$stats_panel_col,
          test_col = input$test_col,
          variable = input$variable_col,
          pair_id_val = if (input$test_col %in% c("olink_ttest", "olink_wilcox") & input$pair_id == TRUE) input$pair_id_col else NULL,
          covariate_val = if (input$test_col %in% c("olink_anova", "olink_lmer")) input$covariate_col else NULL,
          model_formula_text = if (input$test_col %in% c("olink_anova", "olink_lmer")) input$model_formula_text else NULL,
          random_effect = if (input$test_col %in% c("olink_lmer")) input$random_effect else NULL,
          dependence_val = if (input$test_col %in% c("olink_one_non_parametric")) input$dependence_text else FALSE,
          subject_val = if (input$test_col %in% c("olink_one_non_parametric")  & input$dependence_text == TRUE) input$subject_text else NULL,
          return_covariates = if (input$test_col %in% c("olink_lmer")) input$return_covariates else FALSE
        ) 
        
      },
    error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return(NULL)  # Return NULL to prevent further errors
      }
    )
    
    return(test_result_output)

  
  })
  
  output$test_result <- DT::renderDataTable({
    req(test_output(),input$test_col, input$run_test)
    validate(need(is.list(test_output()), "check input"))
    dr <- test_output()[[1]] %>%
      dplyr::mutate(
        Threshold = ifelse(
          !!sym(input$result_pval_col) < as.numeric(input$result_pval_cutoff), 
          "Significant", "Non-significant")
      ) %>% 
      as.data.frame() %>%
      dplyr::mutate(across(where(is.numeric), ~ round(., 3)))
    
    if(input$test_col %in% c("olink_ttest", "olink_wilcox")){
      dr <- dr %>%
        dplyr::mutate(
          Threshold = ifelse(
            Threshold == "Non-significant",
            "Non-significant",
            ifelse(estimate <0, "Down", "Up"))
        )
    }
    
    
    datatable(
      dr,
      caption = gsub("_"," ",input$test_col),
      filter = "top", 
      extensions = 'Buttons', 
      options = list(
        dom = 'Bflrtip',
        buttons = list('copy', 'csv', 'excel', list(
          extend = "collection",
          text = 'Show All',
          action = DT::JS("function ( e, dt, node, config ) {
                                    dt.page.len(-1);
                                    dt.ajax.reload();
                                }")
        )
        )
      )
    ) %>%
    formatStyle(
      columns = "Threshold", 
      backgroundColor = styleEqual(
        c("Significant", "Non-significant", "Up", "Down"), 
        c("#99ff99", "#808080","#ff9999", "#99ccff")  
      ),
      # target = "cell",
      fontWeight = "bold"
    ) %>%
    formatStyle(
      columns = input$result_pval_col, 
      fontWeight = "bold"
    )
  }) 
  
  output$test_log <- renderUI({
    req(input$run_test, test_output())
    
    message_items <- lapply(test_output()[[2]], function(msg) {
      tags$p(msg)
    })
    do.call(tagList, message_items)
 
  })
  # statistical test plot
  
  output$volcano_alternate_x_label_ui <- renderUI({
    req(input$test_col %in% c("olink_ttest", "olink_wilcox"))
    req(
      test_output(), 
      input$variable_col
    )
    textInput("volcano_alternate_x_label", "Alternate x-axis label", value = "")
  })
  
  output$volcano_olink_specific_logical_ui <- renderUI({
    req(input$test_col %in% c("olink_ttest", "olink_wilcox"))
    req(
      test_output(), 
      input$variable_col
    )
    radioButtons("volcano_olink_specific_logical", "Use specific OlinkID(s)", choices = c(FALSE,TRUE))
  })
  
  output$volcano_pval_cutoff_ui <- renderUI({
    req(input$test_col %in% c("olink_ttest", "olink_wilcox"))
    req(
      test_output(), 
      input$variable_col
    )
    numericInput("volcano_pval_cutoff", "Volcano p-value cutoff", min = 0, max = 1, value = 0.05)
  })
  
  volcano_pval_list <- reactive({
    req(input$test_col %in% c("olink_ttest", "olink_wilcox"))
    req(test_output(), input$variable_col, input$result_pval_col)
    if (input$result_pval_col == "Adjusted_pval"){
      return(c("Adjusted_pval", "p.value"))
    } else {
      return(c("p.value"))}
  })
  output$volcano_pval_col_ui <- renderUI({
    req(input$test_col %in% c("olink_ttest", "olink_wilcox"))
    req(test_output(), input$variable_col)
    selectInput("volcano_pval_col", 
                "Volcano p-value column", 
                selected = input$result_pval_col,
                choices = volcano_pval_list())
  })
  
  output$volcano_olink_specific_list_ui <- renderUI({
    req(input$test_col%in% c("olink_ttest", "olink_wilcox"))
    req(test_output(), input$variable_col, as.logical(input$volcano_olink_specific_logical) == TRUE)
    olinks_available <- test_output()[[1]] %>% dplyr::distinct(OlinkID) %>% dplyr::pull(OlinkID)
    selectInput("volcano_olink_specific_list", "Use specific OlinkID(s)", choices = olinks_available, multiple = TRUE)
  })
  
  statistical_test_plot_output <- reactive({
    req(
      test_output(), 
      input$variable_col, 
      input$test_col, 
      input$volcano_olink_specific_logical
    )
    if(input$test_col %in% c("olink_anova", "olink_lmer", "olink_ordinalRegression", "olink_one_non_parametric")){
      plot_to_return <- plot_ggplot_msg(paste0("Try <span style='color:green;'>", paste0(input$test_col, "_posthoc", collapse = ""),"</span> analysis"))
    } else {
      x_label <- ifelse(input$volcano_alternate_x_label == "", "Estimate", input$volcano_alternate_x_label)
      if(as.logical(input$volcano_olink_specific_logical)){
        req(input$volcano_olink_specific_list)
        plot_to_return <- modified_olink_volcano_plot(
          test_output()[[1]]  %>%
            dplyr::mutate(
              Threshold = ifelse(
                Threshold == "Non-significant",
                "Non-significant",
                ifelse(estimate <0, "Down", "Up"))
            ), 
          pval_col = input$volcano_pval_col,
          olinkid_list = input$volcano_olink_specific_list, 
          pval_cutoff = input$volcano_pval_cutoff, 
          x_lab = x_label
          ) 
        # plot_to_return <- OlinkAnalyze::olink_volcano_plot(test_output()[[1]], olinkid_list = input$volcano_olink_specific_list, x_lab = x_label)
        
      } else {
        plot_to_return <- modified_olink_volcano_plot(
          test_output()[[1]]  %>%
            dplyr::mutate(
              Threshold = ifelse(
                Threshold == "Non-significant",
                "Non-significant",
                ifelse(estimate <0, "Down", "Up"))
            ), 
          pval_col = input$volcano_pval_col,
          pval_cutoff = input$volcano_pval_cutoff, 
          x_lab = x_label
        ) 
        # plot_to_return <- OlinkAnalyze::olink_volcano_plot(test_output()[[1]], x_lab = x_label)
      }
      # plot_to_return$layers[[3]] <- NULL
    }
    plot_to_return 
  })
  
  output$statistical_test_plot_out <- renderPlot({
      req(statistical_test_plot_output())
      statistical_test_plot_output()
  })   
  
  
  # download volcano plot
  output$download_statistical_test_plot_options_ui <- renderUI({
    req(statistical_test_plot_output(), input$test_col %in% c("olink_ttest", "olink_wilcox"))
    h4("Download Volcano")
  })
  
  output$download_statistical_test_plot_width_ui <- renderUI({
    req(statistical_test_plot_output(), input$test_col %in% c("olink_ttest", "olink_wilcox"))
    numericInput("download_statistical_test_plot_width", "Width (inches)", value = 12, min = 1)
  })
  
  output$download_statistical_test_plot_height_ui <- renderUI({
    req(statistical_test_plot_output(), input$test_col %in% c("olink_ttest", "olink_wilcox"))
    numericInput("download_statistical_test_plot_height", "Height (inches)", value = 8, min = 1)
  })
  
  output$download_statistical_test_plot_type_ui <- renderUI({
    req(statistical_test_plot_output(), input$test_col %in% c("olink_ttest", "olink_wilcox"))
    selectInput("download_statistical_test_plot_type", "File Type", choices = c("pdf", "png", "jpg"))
  })
  
  output$download_statistical_test_plot_plot_ui <- renderUI({
    req(statistical_test_plot_output(), input$test_col %in% c("olink_ttest", "olink_wilcox"))
    downloadButton("download_statistical_test_plot_plot", "Download Plot")
  })
  
  # Download handler
  output$download_statistical_test_plot_plot <- downloadHandler(
    filename = function() {
      paste0("olink_volcano_plot_", Sys.Date(), ".", input$download_statistical_test_plot_type)
    },
    content = function(file) {
      ggsave(
        filename = file,
        plot = statistical_test_plot_output(), # Use the stored plot
        device = input$download_statistical_test_plot_type,
        width = input$download_statistical_test_plot_width, 
        height = input$download_statistical_test_plot_height
      )
    }
  )
  
  
  output$test_help <- renderText({
    req(input$test_col)
    temp = Rd2HTML(Rd_fun(input$test_col),out = tempfile("docs"))
    content = read_file(temp)
    file.remove(temp)
    content
  })
  
  output$use_olink_ids_ui <- renderUI({
    req(input$test_col %in% c("olink_anova", "olink_lmer", "olink_ordinalRegression", "olink_one_non_parametric"), test_output()) 
    radioButtons( 
      "use_olink_ids", 
      "Use specific OlinkIDs", 
      selected = FALSE,
      choices =  c(TRUE, FALSE))
  })
  
  output$filter_term_olink_ids_ui <- renderUI({
    req(test_output(), input$test_col %in% c("olink_anova", "olink_lmer", "olink_ordinalRegression", "olink_one_non_parametric"), input$use_olink_ids == TRUE)
    selectInput("filter_term_olink_ids", "Term to filter on", choices = test_output()[[1]] %>% dplyr::distinct(term) %>% dplyr::pull(term), multiple = FALSE)
  })
  
  olink_ids_to_use <- reactive({
    req(test_output(), input$test_col %in% c("olink_anova", "olink_lmer", "olink_ordinalRegression", "olink_one_non_parametric"), input$use_olink_ids == TRUE)
    if(is.null(input$filter_term_olink_ids) || input$filter_term_olink_ids == ""){
      test_output()[[1]] %>% 
        dplyr::filter(Threshold == 'Significant') %>%
        dplyr::distinct(OlinkID) %>%
        dplyr::pull(OlinkID)
    } else {
      test_output()[[1]] %>% 
        dplyr::filter(Threshold == 'Significant' & term == input$filter_term_olink_ids) %>%
        dplyr::distinct(OlinkID) %>%
        dplyr::pull(OlinkID)
    }
  })
  
  output$olink_ids_ui <- renderUI({
    req(test_output(), input$test_col %in% c("olink_anova", "olink_lmer", "olink_one_non_parametric", "olink_ordinalRegression"), input$use_olink_ids == TRUE)
    selectInput("posthoc_olinkid_list", "Select OlinkIDs", choices = olink_ids_to_use(), selected = olink_ids_to_use(), multiple = TRUE)
  })
  
  output$run_posthoc_ui <- renderUI({
    req(full_data(), input$use_filtered_data_logical)
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
      df <- filtered_data()
    } else {
      req(full_data())
      df <- full_data()
    }
    if(!is.null(df) &&  !is.null(input$test_col) && input$test_col %in% c("olink_anova", "olink_one_non_parametric", "olink_ordinalRegression", "olink_lmer") && !is.null(test_output())){
      actionButton("run_posthoc", paste0("Run ",input$test_col,"_posthoc"))
    } 
  })
  
  output$posthoc_effect_ui <- renderUI({
    req(input$test_col %in% c( "olink_anova", "olink_lmer", "olink_ordinalRegression"), test_output(), input$use_filtered_data_logical) 
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
      df <- filtered_data()
    } else {
      req(full_data())
      df <- full_data()
    }
    selectInput("posthoc_effect", "Term (effect) to perform post-hoc on", choices = test_output()[[1]] %>% dplyr::distinct(term) %>% dplyr::pull(term), multiple = FALSE)
  })
  
  output$posthoc_effect_formula_ui <- renderUI({
    req(input$test_col %in% c("olink_anova", "olink_lmer", "olink_ordinalRegression"), test_output(),input$use_filtered_data_logical)
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
      df <- filtered_data()
    } else {
      req(full_data())
      df <- full_data()
    }
    textInput("posthoc_effect_formula_text", "Effect formula for post-hoc", value = NULL)
  })
  
  output$posthoc_mean_return_ui <- renderUI({
    req(input$test_col %in% c("olink_anova", "olink_lmer", "olink_ordinalRegression"), test_output(),input$use_filtered_data_logical) 
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
      df <- filtered_data()
    } else {
      req(full_data())
      df <- full_data()
    }
    radioButtons("posthoc_mean_return", "Return mean of each factor level", choices = c(FALSE, TRUE))
  })
  
  output$posthoc_padjt_method_ui <- renderUI({
    req(!is.null(input$test_col) && input$test_col %in% c("olink_anova", "olink_ordinalRegression", "olink_lmer") && !is.null(test_output()))
    selectInput("posthoc_padjt_method","post-hoc padj method", choices = post_hoc_padjust_method_list1)
  })
  
  output$posthoc_test_ui <- renderUI({
    req(!is.null(input$test_col) && input$test_col %in% c("olink_one_non_parametric") && !is.null(test_output()))
    selectInput("posthoc_test","post-hoc test", choices = post_hoc_padjust_method_list2)
  })
  
  posthoc_output <- reactive({
    req(
      input$run_test,
      input$run_posthoc,
      test_output(),
      input$stats_panel_col,
      input$test_col,
      # input$posthoc_olinkid_list,
      input$use_filtered_data_logical
    )
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
      df <- filtered_data()
    } else {
      req(full_data())
      df <- full_data()
    }
    if (input$test_col %in% c("olink_anova", "olink_lmer")){
      req(!is.null(input$variable_col) | !is.null(input$model_formula_text) | input$model_formula_text != "", input$return_covariates, !is.null(input$posthoc_effect) | !is.null(input$posthoc_effect_formula_text))
    } else if (input$test_col %in% c("olink_ordinalRegression")){
      req(input$variable_col, input$return_covariates)
    } else if (input$test_col == "olink_one_non_parametric") {
      req(input$variable_col, input$dependence_text)
    }
    
    if(!is.null(input$posthoc_olinkid_list)){
      list2pass <- input$posthoc_olinkid_list
    } else {
      list2pass <- NULL
    }
    
    tryCatch({
        posthoc_statistics(
          df = df,
          posthoc_effect = if (input$test_col %in% c("olink_lmer","olink_anova", "olink_ordinalRegression")) input$posthoc_effect else NULL,
          posthoc_effect_formula = if (input$test_col %in% c("olink_lmer","olink_anova", "olink_ordinalRegression")) input$posthoc_effect_formula_text else NULL,
          panel_col = input$stats_panel_col,
          test_col = input$test_col,
          variable = input$variable_col,
          olink_list = list2pass,
          covariate_val = if (input$test_col %in% c("olink_anova", "olink_lmer", "olink_ordinalRegression")) input$covariate_col else NULL,
          posthoc_model_formula = if (input$test_col %in% c("olink_anova", "olink_lmer")) input$model_formula_text else NULL,
          random_list = if (input$test_col %in% c("olink_lmer")) input$random_effect else NULL,
          return_mean = if (input$test_col %in% c("olink_lmer","olink_anova", "olink_ordinalRegression")) input$posthoc_mean_return else FALSE,
          posthoc_padj_method = if (input$test_col %in% c("olink_lmer","olink_anova", "olink_ordinalRegression")) input$posthoc_padjt_method else NULL,
          posthoc_test = if (input$test_col %in% c("olink_one_non_parametric")) input$posthoc_test else NULL
          )
      },
      error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
          return(NULL)  # Return NULL to prevent further errors
        }
      )
  
  })
  
  output$posthoc_test_result <- DT::renderDataTable({
    req(input$run_posthoc, posthoc_output())
    datatable(
      posthoc_output()[[1]] %>% 
        as.data.frame() %>%
        dplyr::mutate(across(where(is.numeric), ~ round(., 3))),
      caption = paste0(input$test_col, "_posthoc"),
      filter = "top", 
      extensions = 'Buttons', 
      options = list(
        dom = 'Bflrtip',
        buttons = list('copy', 'csv', 'excel', list(
          extend = "collection",
          text = 'Show All',
          action = DT::JS("function ( e, dt, node, config ) {
                                    dt.page.len(-1);
                                    dt.ajax.reload();
                                }")
        )
        )
      )
    )  %>%
      formatStyle(
        columns = "Threshold", 
        backgroundColor = styleEqual(
          c("Significant", "Non-significant", "Up", "Down"), 
          c("#99ff99", "#808080","#ff9999", "#99ccff")  
        ),
        # target = "cell",
        fontWeight = "bold"
      ) %>%
      formatStyle(
        columns = "Adjusted_pval", 
        fontWeight = "bold"
      )
  }) 
  
  output$posthoc_test_log <- renderUI({
    req(input$run_posthoc, posthoc_output())
    message_items <- lapply(posthoc_output()[[2]], function(msg) {
      tags$p(msg)
    })
    do.call(tagList, message_items)
  })
  
  
  output$statistical_test_boxplot_variable_ui <- renderUI({
    req(input$stats_panel_col,input$use_filtered_data_logical)
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
      df <- filtered_data()
    } else {
      req(full_data())
      df <- full_data()
    }
    
    if (input$test_col %in% c("olink_ttest", "olink_wilcox")) {
      selectInput("boxplot_variable_list", "Variable(s) for box plot", choices = setdiff(column_types(),not_variable_label), multiple = FALSE)
    } else {
      selectInput("boxplot_variable_list", "Variable(s) for box plot", choices = setdiff(names(df),not_variable_label), multiple = TRUE)
    }
  })
  
  statistical_test_boxplot_olink_list <- reactive({
    req(input$test_col, input$stats_panel_col, input$use_filtered_data_logical)
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
      df <- filtered_data()
    } else {
      req(full_data())
      df <- full_data()
    }
    
    if(input$test_col == "olink_ttest" & !is.null(test_output())){
      df2use <- test_output()[[1]]
      list2pass <- df2use %>%
        dplyr::distinct(OlinkID) %>%
        dplyr::pull()
    } else if (input$test_col == "olink_anova" & !is.null(test_output())){
      df2use <- posthoc_output()[[1]]
      list2pass <- df2use %>%
        dplyr::distinct(OlinkID) %>%
        dplyr::pull(OlinkID)
    } else {
      df2use <- df
      
      if(input$stats_panel_col != "all"){
        df2use <- df2use %>% dplyr::filter(Panel == input$stats_panel_col)
      }
      
      list2pass <- df2use %>% 
        dplyr::distinct(OlinkID) %>%
        dplyr::pull(OlinkID)
    }
    
    list2pass
  })

  output$statistical_test_boxplot_olink_use_test_result_ui <- renderUI({
    req(input$test_col, input$stats_panel_col, input$use_filtered_data_logical)
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
      df <- filtered_data()
    } else {
      req(full_data())
      df <- full_data()
    }

    radioButtons("use_test_results", "Use ttest/anova posthoc results", choices = c(FALSE, TRUE))
  })
  
  
  
  output$statistical_test_boxplot_olink_ui <- renderUI({
    req(input$test_col, input$stats_panel_col, input$use_filtered_data_logical)
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
      df <- filtered_data()
    } else {
      req(full_data())
      df <- full_data()
    }
    selectInput("boxplot_olink_list", "olinkID(s) for box plot", choices = statistical_test_boxplot_olink_list(), multiple = TRUE)
  })
  
  output$statistical_test_boxplot_number_ui <- renderUI({
    req(input$test_col, input$stats_panel_col, input$use_filtered_data_logical)
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
      df <- filtered_data()
    } else {
      req(full_data())
      df <- full_data()
    }
    numericInput("boxplot_number", "number of proteins per plot", value = 6, min = 1, max = 100)
  })
  
  output$statistical_test_boxplot_run_ui <- renderUI({
    req(input$test_col, input$stats_panel_col, input$boxplot_variable_list,input$boxplot_olink_list, input$use_filtered_data_logical)
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
    } else {
      req(full_data())
    }
    actionButton("generate_boxplot", "Show boxplot")
  })
  
  observeEvent(input$generate_boxplot, {})
  
  # Reactive values to track the plot
  rv <- reactiveValues(plot = NULL)
  
  output$statistical_test_boxplot_out <- renderPlot({
    req(input$stats_panel_col, input$boxplot_variable_list,input$boxplot_olink_list, input$use_filtered_data_logical, input$use_test_results)
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
      df2plot <- filtered_data()
    } else {
      req(full_data())
      df2plot <- full_data()
    }
    
    if(as.logical(input$use_test_results)){
      req(input$test_col)
      if(input$test_col == "olink_ttest"){
        validate(need(is.list(test_output()), "Run olink_tttest first!"))
        validate(need(is.data.frame(test_output()[[1]]), "check input before running olink_tttest!"))
        req(input$generate_boxplot)
      }
      if(input$test_col == "olink_anova"){
        validate(need(is.list(posthoc_output()), paste0("Run",input$test_col,"_posthoc first!")))     
        validate(need(is.data.frame(posthoc_output()[[1]]), "check input contrast before running olink_anova_posthoc!"))
        validate(need(length(input$boxplot_variable_list)==1,"must select exactly one column to use olink_anova_posthoc results."))
        req(input$generate_boxplot)
        
      }      
    } else {
      req(input$generate_boxplot)
    }
    
    
    if(input$stats_panel_col != "all"){
      df2plot <- df2plot %>% dplyr::filter(Panel == input$stats_panel_col)
    }
    
    df2plot <- df2plot %>% 
      dplyr::filter(!(grepl("control|ctrl", SampleID, ignore.case = TRUE))) %>%      
      dplyr::filter(!(grepl("control|ctrl", Assay, ignore.case = TRUE))) %>%
      dplyr::filter(!is.na(NPX))
    
    if (as.logical(input$use_test_results) && input$test_col == "olink_ttest"){
      ttest_results <- test_output()[[1]]
    }  else {
      ttest_results <- NULL
    }
    if (as.logical(input$use_test_results) && input$test_col == "olink_anova"){
      posthoc_results <- posthoc_output()[[1]]
    }  else {
      posthoc_results <- NULL
    }
    
    p <- OlinkAnalyze::olink_boxplot(
      df = df2plot,
      variable = input$boxplot_variable_list,
      olinkid_list = input$boxplot_olink_list,
      verbose = FALSE,
      number_of_proteins_per_plot = input$boxplot_number,
      posthoc_results = posthoc_results,
      ttest_results = ttest_results
    )
    
    rv$plot <- p[[1]]
    
    p[[1]]
    
  })
  
  output$download_options_ui <- renderUI({
    req(input$test_col, input$stats_panel_col, input$boxplot_variable_list,input$boxplot_olink_list, input$generate_boxplot, input$use_filtered_data_logical)
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
    } else {
      req(full_data())
    }
    h4("Download Options")
  })
  
  output$download_width_ui <- renderUI({
    req(input$test_col, input$stats_panel_col, input$boxplot_variable_list,input$boxplot_olink_list, input$generate_boxplot, input$use_filtered_data_logical)
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
    } else {
      req(full_data())
    }
    numericInput("download_width", "Width (inches)", value = 8, min = 1)
  })
  
  output$download_height_ui <- renderUI({
    req(input$test_col, input$stats_panel_col, input$boxplot_variable_list,input$boxplot_olink_list, input$generate_boxplot, input$use_filtered_data_logical)
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
    } else {
      req(full_data())
    }
    numericInput("download_height", "Height (inches)", value = 6, min = 1)
  })
  
  output$download_type_ui <- renderUI({
    req(input$test_col, input$stats_panel_col, input$boxplot_variable_list,input$boxplot_olink_list, input$generate_boxplot, input$use_filtered_data_logical)
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
    } else {
      req(full_data())
    }
    selectInput("download_type", "File Type", choices = c("pdf", "png", "jpg"))
  })
  
  output$download_plot_ui <- renderUI({
    req(input$test_col, input$stats_panel_col, input$boxplot_variable_list,input$boxplot_olink_list, input$generate_boxplot, input$use_filtered_data_logical)
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
    } else {
      req(full_data())
    }
    downloadButton("download_plot", "Download Plot")
  })
  
  # Download handler
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("boxplot_", Sys.Date(), ".", input$download_type)
    },
    content = function(file) {
      ggsave(
        filename = file,
        plot = rv$plot, # Use the stored plot
        device = input$download_type,
        width = input$download_width, 
        height = input$download_height
      )
    }
  )
  
  output$boxplot_help <- renderText({
    req(input$stats_panel_col, input$boxplot_variable_list, input$use_filtered_data_logical, input$use_test_results)
    temp = Rd2HTML(Rd_fun("olink_boxplot"),
                   out = tempfile("docs"))
    content = read_file(temp)
    file.remove(temp)
    content
  })

  output$posthoc_test_help <- renderText({
    req(!(input$test_col %in% c("olink_ttest", "olink_wilcox")), test_output())
    temp = Rd2HTML(Rd_fun(paste0(input$test_col,"_posthoc", collapse = "")),
                   out = tempfile("docs"))
    content = read_file(temp)
    file.remove(temp)
    content
  })
  
  # Pathway Enrichment Page
  output$pathway_enrichment_help <- renderText({
    req(input$test_col)
    if(input$test_col %in% c("olink_ttest", "olink_wilcox")){
      req(test_output())
    } else {
      req(posthoc_output())
    }
    
    temp = Rd2HTML(Rd_fun("olink_pathway_enrichment"),
                   out = tempfile("docs"))
    content = read_file(temp)
    file.remove(temp)
    content
  })
  
  output$run_pathway_enrichment_ui <- renderUI({
    req(input$test_col)
    if(input$test_col %in% c("olink_ttest", "olink_wilcox")){
      req(test_output())
      actionButton("run_pathway_enrichment", "Run Pathway Enrichment")
    } else {
      req(posthoc_output())
      actionButton("run_pathway_enrichment", "Run Pathway Enrichment")
    }
  })
  
  output$pathway_enrichment_method_ui <- renderUI({
    req(input$test_col)
    if(input$test_col %in% c("olink_ttest", "olink_wilcox")){
      req(test_output(), input$run_test)
      selectInput("pathway_enrichment_method", "Select Method", choices = c("GSEA", "ORA"))
    } else {
      req(posthoc_output(), input$run_posthoc)
      selectInput("pathway_enrichment_method", "Select Method", choices = c("GSEA", "ORA"))
    }
  })
  
  output$pathway_enrichment_ontology_ui <- renderUI({
    req(input$test_col)
    if(input$test_col %in% c("olink_ttest", "olink_wilcox")){
      req(test_output(), input$run_test)
      selectInput("pathway_enrichment_ontology", "Select Ontology", choices = c("MSigDb", "KEGG", "GO", "Reactome"))
    } else {
      req(posthoc_output(), input$run_posthoc)
      selectInput("pathway_enrichment_ontology", "Select Ontology", choices = c("MSigDb", "KEGG", "GO", "Reactome"))
    }
  })
  
  output$pathway_enrichment_organism_ui <- renderUI({
    req(input$test_col)
    if(input$test_col %in% c("olink_ttest", "olink_wilcox")){
      req(test_output(), input$run_test)
      selectInput("pathway_enrichment_organism", "Select Organism", choices = c("human", "mouse"))
    } else {
      req(posthoc_output(), input$run_posthoc)
      selectInput("pathway_enrichment_organism", "Select Organism", choices = c("human", "mouse"))
    }
  })
  
  
  output$pathway_enrichment_contrast_ui <- renderUI({
    req(input$test_col)
    if(!(input$test_col %in% c("olink_ttest", "olink_wilcox"))){
      req(posthoc_output(), input$run_posthoc)
      contrast_list <- posthoc_output()[[1]] %>% dplyr::distinct(contrast) %>% dplyr::pull(contrast)
      selectInput("pathway_enrichment_contrast", "Select contrast", choices = contrast_list)
    }
  })
  
  output$pathway_enrichment_pvalue_cutoff_ui <- renderUI({
    req(input$test_col)
    if(!(input$test_col %in% c("olink_ttest", "olink_wilcox"))){
      req(posthoc_output(), input$run_posthoc, input$pathway_enrichment_method == "ORA")
      numericInput("pathway_enrichment_pvalue_cutoff", "Select p-value cutoff", value = 0.05, min = 0, max = 1)
    } else {
      req(test_output(), input$run_test, input$pathway_enrichment_method == "ORA")
      numericInput("pathway_enrichment_pvalue_cutoff", "Select p-value cutoff", value = 0.05, min = 0, max = 1)
      
    }
  })
  output$pathway_enrichment_estimate_cutoff_ui <- renderUI({
    req(input$test_col)
    
    if(!(input$test_col %in% c("olink_ttest", "olink_wilcox"))){
      req(posthoc_output(), input$run_posthoc, input$pathway_enrichment_method == "ORA")
      numericInput("pathway_enrichment_estimate_cutoff", "Select estimate cutoff", value = 0.0, min = 0, max = 10)
    } else {
      req(test_output(), input$run_test, input$pathway_enrichment_method == "ORA")
      numericInput("pathway_enrichment_estimate_cutoff", "Select estimate cutoff", value = 0.0, min = 0, max = 10000)
    }
  })
  
  pathway_enrichment_result_output <- reactive({
    req(
      input$pathway_enrichment_organism, 
      input$pathway_enrichment_ontology, 
      input$pathway_enrichment_method,
      input$use_filtered_data_logical,
      input$run_pathway_enrichment
    )
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
      df <- filtered_data() 
    } else {
      req(full_data())
      df <- full_data()
    }
    
    if(input$test_col %in% c("olink_ttest", "olink_wilcox")){
      req(test_output())
      test_results <- test_output()[[1]]
    } else {
      req(posthoc_output())
      test_results <- posthoc_output()[[1]] %>% 
          dplyr::filter(contrast == input$pathway_enrichment_contrast)
    }
    
    if(input$pathway_enrichment_method == "ORA"){
      req(input$pathway_enrichment_pvalue_cutoff, input$pathway_enrichment_estimate_cutoff)
    }
    
    if(input$stats_panel_col != "all"){
      df <- df %>% dplyr::filter(Panel == input$stats_panel_col)
    }
    
    df <- df %>% 
      dplyr::filter(!grepl("control|ctrl", SampleID, ignore.case = TRUE)) %>% 
      dplyr::filter(!grepl("control|ctrl", Assay, ignore.case = TRUE))
    

    tryCatch({
            verbose_msg <- capture.output(
              pathway_enrichment <- OlinkAnalyze::olink_pathway_enrichment(
              data = df,
              test_results = test_results ,
              method = input$pathway_enrichment_method,
              ontology = input$pathway_enrichment_ontology,
              organism = input$pathway_enrichment_organism,
              pvalue_cutoff = if (input$pathway_enrichment_method == "ORA") input$pathway_enrichment_pvalue_cutoff else 0.05,
              estimate_cutoff = if (input$pathway_enrichment_method == "ORA") input$pathway_enrichment_estimate_cutoff else 0
             ),
             type = "message"
            )
        },
        error = function(e) {
            showNotification(paste("Error:", e$message), type = "error")
            return(NULL)  # Return NULL to prevent further errors
          }
        )
  
    
    return(list(pathway_enrichment, verbose_msg, test_results))
    
  })
  
  output$pathway_enrichment_result <- DT::renderDataTable({
    req(input$run_pathway_enrichment, pathway_enrichment_result_output())
    if(input$test_col == "olink_ttest"){
      caption_out <- paste0("pathway enrichment on ",input$test_col, " results")
    } else {
      caption_out <- paste0("pathway enrichment on ",input$test_col, "_posthoc results")
    }
    datatable(
      pathway_enrichment_result_output()[[1]] %>% 
        as.data.frame() %>%
        dplyr::mutate(across(where(is.numeric), ~ round(., 3))),
      caption = caption_out,
      filter = "top", 
      extensions = 'Buttons', 
      options = list(
        dom = 'Bflrtip',
        buttons = list('copy', 'csv', 'excel', list(
          extend = "collection",
          text = 'Show All',
          action = DT::JS("function ( e, dt, node, config ) {
                                    dt.page.len(-1);
                                    dt.ajax.reload();
                                }")
         )
        )
      )
    )  
    # %>%
      # formatStyle(
      #   columns = "Threshold", 
      #   backgroundColor = styleEqual(
      #     c("Significant", "Non-significant", "Up", "Down"), 
      #     c("#99ff99", "#808080","#ff9999", "#99ccff")  
      #   ),
      #   # target = "cell",
      #   fontWeight = "bold"
      # ) %>%
      # formatStyle(
      #   columns = input$result_pval_col, 
      #   fontWeight = "bold"
      # )
  })
  
  output$pathway_enrichment_log <- renderUI({
    req(input$run_pathway_enrichment, pathway_enrichment_result_output())
    message_items <- lapply(pathway_enrichment_result_output()[[2]], function(msg) {
      tags$p(msg)
    })
    do.call(tagList, message_items)
  })
  
  output$pathway_visual_mode_ui <- renderUI({
    validate(need(!is.null(pathway_enrichment_result_output()),"No enrichment terms."))
    selectInput("pathway_visual_mode", "Select plot type", choices = c("olink_pathway_heatmap", "olink_pathway_visualization"))
  })
  
  output$pathway_visual_help <- renderText({
    req(input$pathway_visual_mode)
    temp = Rd2HTML(Rd_fun(input$pathway_visual_mode),out = tempfile("docs"))
    content = read_file(temp)
    file.remove(temp)
    content
  })
  
  output$pathway_visual_keyword_ui <- renderUI({
    validate(need(!is.null(pathway_enrichment_result_output()),"No enrichment terms."))
    textInput("pathway_visual_keyword", "Keyword to filter enrichment results", value = '')
  })
  
  output$pathway_visual_number_of_terms_ui <- renderUI({
    validate(need(!is.null(pathway_enrichment_result_output()),"No enrichment terms."))
    numericInput("pathway_visual_number_of_terms", "Number of terms", value = 20, min = 1, max = 100)
  })
  
  output$pathway_visual_run_ui <- renderUI({
    validate(need(!is.null(pathway_enrichment_result_output()),"No enrichment terms."))
    req(input$pathway_visual_mode, input$pathway_visual_number_of_terms)
    actionButton("pathway_visual_run", "Plot enrichment results")
  })
  
  observeEvent(input$pathway_visual_run, {})
  
  # Reactive values to track the plot
  rv_pathway_visual <- reactiveValues(plot = NULL)
  
  pathway_visual_output <- reactive({
    req(
      pathway_enrichment_result_output(), 
      input$pathway_visual_mode, 
      input$pathway_visual_number_of_terms,
      input$pathway_visual_run
    )
    validate(need(nrow(pathway_enrichment_result_output()[[1]])>0, "Enrichment returned empty data.frame"))
    if(input$pathway_visual_mode =="olink_pathway_heatmap"){
      tryCatch({
      verbose_msg <- capture.output(
        pathway_plot <- olink_pathway_heatmap(
        enrich_results = pathway_enrichment_result_output()[[1]],
        test_results = pathway_enrichment_result_output()[[3]],
        method = input$pathway_enrichment_method,
        keyword = input$pathway_visual_keyword,
        number_of_terms = input$pathway_visual_number_of_terms), type = "message")
          },
          error = function(e) {
            showNotification(paste("Error:", e$message), type = "error")
            return(NULL)  # Return NULL to prevent further errors
          }
        )
    } else {
      tryCatch({
      verbose_msg <- capture.output(
        pathway_plot <- olink_pathway_visualization(
        enrich_results = pathway_enrichment_result_output()[[1]],
        method = input$pathway_enrichment_method,
        keyword = input$pathway_visual_keyword,
        number_of_terms = input$pathway_visual_number_of_terms), 
        type = "message")
          },
          error = function(e) {
            showNotification(paste("Error:", e$message), type = "error")
            return(NULL)  # Return NULL to prevent further errors
          }
          )
    }
    if(is.ggplot(pathway_plot)){
      pathway_plot <- clean_yticks(pathway_plot, input$pathway_visual_mode)
    }
    return(list(pathway_plot, verbose_msg))
  })
  
  output$pathway_visual_plot <- renderPlot({
    req(pathway_visual_output(), input$pathway_visual_run)
    rv_pathway_visual$plot <- pathway_visual_output()[[1]] 
    
    pathway_visual_output()[[1]]
  })
  
  output$pathway_visual_log <- renderUI({
    req(pathway_visual_output(), input$pathway_visual_run)
    message_items <- lapply(pathway_visual_output()[[2]], function(msg) {
      tags$p(msg)
    })
    do.call(tagList, message_items)
  })
  
  # Download Enrichment Visualization
  
  output$download_pathway_visual_options_ui <- renderUI({
    req(pathway_visual_output(), input$pathway_visual_run)
    h4("Download Options")
  })
  
  output$download_pathway_visual_width_ui <- renderUI({
    req(pathway_visual_output(), input$pathway_visual_run)
    numericInput("download_pathway_visual_width", "Width (inches)", value = 8, min = 1)
  })
  
  output$download_pathway_visual_height_ui <- renderUI({
    req(pathway_visual_output(), input$pathway_visual_run)
    numericInput("download_pathway_visual_height", "Height (inches)", value = 8, min = 1)
  })
  
  output$download_pathway_visual_type_ui <- renderUI({
    req(pathway_visual_output(), input$pathway_visual_run)
    selectInput("download_pathway_visual_type", "File Type", choices = c("pdf", "png", "jpg"))
  })
  
  output$download_pathway_visual_plot_ui <- renderUI({
    req(pathway_visual_output(), input$pathway_visual_run)
    downloadButton("download_pathway_visual_plot", "Download Plot")
  })
  
  # Download handler
  output$download_pathway_visual_plot <- downloadHandler(
    filename = function() {
      paste0(input$pathway_enrichment_method,"_", Sys.Date(), ".", input$download_pathway_visual_type)
    },
    content = function(file) {
      ggsave(
        filename = file,
        plot = rv_pathway_visual$plot, # Use the stored plot
        device = input$download_pathway_visual_type,
        width = input$download_pathway_visual_width, 
        height = input$download_pathway_visual_height
      )
    }
  )
  
# lmer plot
  x_axis_variable_list <- reactive({
    req(
      input$run_test,
      test_output(),
      input$test_col == "olink_lmer"
    )
    
    unique(
      comprehenr::to_vec(
        for(i in test_output()[[1]] %>% distinct(term) %>% dplyr::pull(term))
          if(grepl(":",i)) str_split(i,":")[[1]] else i)
    )
  })
  
  
  
  output$plot_olinkid_list_ui <- renderUI({
    req(
      input$run_test,
      input$test_col == "olink_lmer",
      input$stats_panel_col,
      input$use_filtered_data_logical
    )
    if(as.logical(input$use_filtered_data_logical)){
      req(filtered_data())
      df <- filtered_data()
    } else {
      req(full_data())
      df <- full_data()
    }
    
    if(input$stats_panel_col != "all"){
      df <- df %>% dplyr::filter(Panel == input$stats_panel_col)
    }
    
    olinkid_list_to_plot <- df %>% 
      dplyr::distinct(OlinkID) %>% 
      dplyr::pull(OlinkID)
    
    selectInput("plot_olinkid_list", "proteins (by OlinkID) to plot", 
                choices = olinkid_list_to_plot, multiple = TRUE)
  })
  
  output$plot_x_axis_variable_ui <- renderUI({
    req(
      input$run_test,
      input$test_col == "olink_lmer"
    )
    if(input$test_col == "olink_lmer"){
      req(test_output())
    }
    selectInput("plot_x_axis_variable", "main effect to use as x-axis in the plot", choices = x_axis_variable_list())
  })
  
  
  output$plot_col_variable_ui <- renderUI({
    req(
      input$run_test,
      input$test_col == "olink_lmer"
    )
    if(input$test_col == "olink_lmer"){
      req(test_output())
    }
    selectInput("plot_col_variable", "interaction effect", choices = rev(x_axis_variable_list()))
  })
  
  output$plot_number_of_proteins_per_plot_ui <- renderUI({
    req(
      input$run_test,
      input$test_col == "olink_lmer"
    )
    if(input$test_col == "olink_lmer"){
      req(test_output())
    }
    numericInput("plot_number_of_proteins_per_plot", "Number of plots", value = 6, min = 1, max = 20)
  })
  
 output$plot_button_lmer_ui <- renderUI({
   req(input$test_col == "olink_lmer")
   req(
     input$run_test,
     test_output(),
     input$stats_panel_col,
     input$variable_col, 
     input$random_effect,
     input$plot_x_axis_variable,
     input$plot_col_variable, 
     input$use_filtered_data_logical
   )
   if(as.logical(input$use_filtered_data_logical)){
     req(filtered_data())
   } else {
     req(full_data())
   }   
   actionButton("plot_button_lmer", "olink_lmer_plot")
 })

 observeEvent(input$plot_button_lmer, {})
 
 # Reactive values to track the plot
 rv_lmer <- reactiveValues(plot = NULL)
 
 olink_lmer_plot_out <- reactive({
   req(
     input$use_filtered_data_logical,
     input$run_test,
     test_output(),
     input$stats_panel_col,
     input$test_col == "olink_lmer",
     input$variable_col, 
     input$random_effect,
     input$plot_olinkid_list,
     input$plot_x_axis_variable,
     input$plot_col_variable,
     input$plot_button_lmer, 
     input$use_filtered_data_logical
   )
   
   if(as.logical(input$use_filtered_data_logical)){
     req(filtered_data())
     df <- filtered_data()
   } else {
     req(full_data())
     df <- full_data()
   }
   if(input$stats_panel_col != "all"){
     df <- df %>% dplyr::filter(Panel == input$stats_panel_col)
   }
   df <- df %>% 
     dplyr::filter(!grepl("control|ctrl",SampleID, ignore.case=TRUE)) %>%
     dplyr::filter(!grepl("control|ctrl",Assay, ignore.case=TRUE)) 

   p_out <- olink_lmer_plot(
     df = df,
     variable = input$variable_col,
     outcome = "NPX",
     random = input$random_effect,
     olinkid_list = input$plot_olinkid_list,
     covariates =  input$covariate_col,
     x_axis_variable = input$plot_x_axis_variable,
     col_variable = input$plot_col_variable,
     number_of_proteins_per_plot = input$plot_number_of_proteins_per_plot
   )
   
   rv_lmer$plot <-p_out[[1]] 
   p_out[[1]]
  
 })

 output$olink_lmer_plot_output <- renderPlot({
   req(olink_lmer_plot_out())
   olink_lmer_plot_out()
 })
 
 # download lmer plot
 output$download_lmer_options_ui <- renderUI({
   req(olink_lmer_plot_out())
   h4("Download Options")
 })
 
 output$download_lmer_width_ui <- renderUI({
   req(olink_lmer_plot_out())
   numericInput("download_lmer_width", "Width (inches)", value = 12, min = 1)
 })
 
 output$download_lmer_height_ui <- renderUI({
   req(olink_lmer_plot_out())
   numericInput("download_lmer_height", "Height (inches)", value = 8, min = 1)
 })
 
 output$download_lmer_type_ui <- renderUI({
   req(olink_lmer_plot_out())
   selectInput("download_lmer_type", "File Type", choices = c("pdf", "png", "jpg"))
 })
 
 output$download_lmer_plot_ui <- renderUI({
   req(olink_lmer_plot_out())
   downloadButton("download_lmer_plot", "Download Plot")
 })
 
 # Download handler
 output$download_lmer_plot <- downloadHandler(
   filename = function() {
     paste0("olink_lmer_plot_", Sys.Date(), ".", input$download_lmer_type)
   },
   content = function(file) {
     ggsave(
       filename = file,
       plot = rv_lmer$plot, # Use the stored plot
       device = input$download_lmer_type,
       width = input$download_lmer_width, 
       height = input$download_lmer_height
     )
   }
 )
 
 output$complex_heatmap_panel_col_ui <- renderUI({
   req(input$use_filtered_data_logical)
   if(as.logical(input$use_filtered_data_logical)){
     req(filtered_data())
     df <- filtered_data()
   } else {
     req(full_data())
     df <- full_data()
   }
   selectInput("complex_heatmap_panel_col", "Heatmap data panel", selected = input$stats_panel_col, choices = unique(df$Panel))
 })
 
 output$complex_heatmap_use_significant_assay_logical_ui <- renderUI({
   req(input$use_filtered_data_logical)
   if(as.logical(input$use_filtered_data_logical)){
     req(filtered_data())
     df <- filtered_data()
   } else {
     req(full_data())
     df <- full_data()
   }
   radioButtons("complex_heatmap_use_significant_assay_logical", "Significant assay only", choices = c(FALSE, TRUE))
 })
 
 output$complex_heatmap_use_colnames_logical_ui <- renderUI({
   req(input$use_filtered_data_logical)
   if(as.logical(input$use_filtered_data_logical)){
     req(filtered_data())
     df <- filtered_data()
   } else {
     req(full_data())
     df <- full_data()
   }
   radioButtons("complex_heatmap_use_colnames_logical", "Use column names", choices = c(FALSE, TRUE))
 })
 
 output$complex_heatmap_use_rownames_logical_ui <- renderUI({
   req(input$use_filtered_data_logical)
   if(as.logical(input$use_filtered_data_logical)){
     req(filtered_data())
     df <- filtered_data()
   } else {
     req(full_data())
     df <- full_data()
   }
   radioButtons("complex_heatmap_use_rownames_logical", "Use row names", choices = c(FALSE, TRUE))
 })
 
 output$complex_heatmap_group_ui <- renderUI({
   req(input$use_filtered_data_logical)
   if(as.logical(input$use_filtered_data_logical)){
     req(filtered_data())
     df <- filtered_data()
   } else {
     req(full_data())
     df <- full_data()
   }
   selectInput("complex_heatmap_group", "Heatmap data group", choices = setdiff(names(df),not_variable_label))
 })
 
 output$complex_heatmap_ui <- renderUI({
   req(input$complex_heatmap_panel_col != "all", input$use_filtered_data_logical)
   req(input$complex_heatmap_use_significant_assay_logical)
   if(as.logical(input$complex_heatmap_use_significant_assay_logical)){
     validate(need(is.data.frame(test_output()[[1]]), paste0("Run ", input$test_col," first.")))
     validate(need(test_output()[[1]] %>% dplyr::filter(Threshold %in% c("Significant", "Up", "Down")) %>% nrow() > 0, paste0("No significant Assays found for ", input$test_col,"!")))
   }
   req(input$complex_heatmap_group)
   actionButton("plot_complex_heatmap", "Plot heatmap")
   
 })
 
 output$complex_heatmap_cluster_rows_logical_ui <- renderUI({
   req(input$use_filtered_data_logical)
   if(as.logical(input$use_filtered_data_logical)){
     req(filtered_data())
     df <- filtered_data()
   } else {
     req(full_data())
     df <- full_data()
   }
   radioButtons("complex_heatmap_cluster_rows_logical", "Cluster rows", choices = c(FALSE, TRUE))
 })
 
 output$complex_heatmap_cluster_cols_logical_ui <- renderUI({
   req(input$use_filtered_data_logical)
   if(as.logical(input$use_filtered_data_logical)){
     req(filtered_data())
     df <- filtered_data()
   } else {
     req(full_data())
     df <- full_data()
   }
   radioButtons("complex_heatmap_cluster_cols_logical", "Cluster columns", choices = c(FALSE, TRUE))
 })
 
 observeEvent(input$plot_complex_heatmap, {})
 
 # Reactive values to track the plot
 rv_complex_heatmap <- reactiveValues(plot = NULL)
   
 complex_heatmap <- reactive({
   
   req(input$complex_heatmap_panel_col != "all", input$use_filtered_data_logical)
   
   if(as.logical(input$use_filtered_data_logical)){
     req(filtered_data())
     df <- filtered_data()
   } else {
     req(full_data())
     df <- full_data()
   }
   
   req(input$complex_heatmap_use_significant_assay_logical)
   if(as.logical(input$complex_heatmap_use_significant_assay_logical)){
     validate(need(is.data.frame(test_output()[[1]]), paste0("Run ", input$test_col," first.")))
     validate(need(test_output()[[1]] %>% dplyr::filter(Threshold %in% c("Significant", "Up", "Down")) %>% nrow() > 0, paste0("No significant Assays found for ", input$test_col,"!")))
     significant_assays_for_heatmap <- test_output()[[1]] %>%
       dplyr::mutate(
         Threshold = ifelse(
           !!sym(input$result_pval_col) < as.numeric(input$result_pval_cutoff), 
           "Significant", "Non-significant")
       ) %>% 
       as.data.frame() %>% 
       dplyr::filter(Threshold %in% c("Significant", "Up", "Down")) %>% 
       dplyr::distinct(Assay) %>% 
       dplyr::pull(Assay)
     
     df <- df %>% dplyr::filter(Assay %in% significant_assays_for_heatmap)
   }
   
   req(input$complex_heatmap_group, input$plot_complex_heatmap)
   
   complex_heatmap_output <- generate_complex_heatmap(
     data = df,
     group =  input$complex_heatmap_group, 
     panel =  input$complex_heatmap_panel_col,
     scale_rows = TRUE,
     cluster_rows = as.logical(input$complex_heatmap_cluster_rows_logical),
     cluster_columns = as.logical(input$complex_heatmap_cluster_cols_logical),
     show_row_names = as.logical(input$complex_heatmap_use_rownames_logical),
     show_column_names = as.logical(input$complex_heatmap_use_colnames_logical),
   )
   
   rv_complex_heatmap$plot <-complex_heatmap_output
   
   return(complex_heatmap_output)
   
 })
 

 output$complex_heatmap_output <- renderPlot({
   validate(need(complex_heatmap(), "check heatmap input"))
   complex_heatmap()
 })
 # , height = plot_height, width = plot_width)
 
 # download heatmap plot
 output$download_complex_heatmap_options_ui <- renderUI({
   req(complex_heatmap())
   h4("Download heatmap")
 })
 
 output$download_complex_heatmap_width_ui <- renderUI({
   req(complex_heatmap())
   numericInput("download_complex_heatmap_width", "Width (inches)", value = 12, min = 1)
 })
 
 output$download_complex_heatmap_height_ui <- renderUI({
   req(complex_heatmap())
   numericInput("download_complex_heatmap_height", "Height (inches)", value = 8, min = 1)
 })
 
 output$download_complex_heatmap_type_ui <- renderUI({
   req(complex_heatmap())
   selectInput("download_complex_heatmap_type", "File Type", choices = c("pdf", "png", "jpg"))
 })
 
 output$download_complex_heatmap_plot_ui <- renderUI({
   req(complex_heatmap())
   downloadButton("download_complex_heatmap_plot", "Download Plot")
 })
 
 # Download handler
 output$download_complex_heatmap_plot <- downloadHandler(
   filename = function() {
     paste0("complex_heatmap_plot_", Sys.Date(), ".", input$download_complex_heatmap_type)
   },
   content = function(file) {
     # Open the appropriate graphics device
     if (input$download_complex_heatmap_type == "pdf") {
       pdf(file, width = input$download_complex_heatmap_width, height = input$download_complex_heatmap_height)
     } else if (input$download_complex_heatmap_type == "png") {
       png(file, width = input$download_complex_heatmap_width * 100, height = input$download_complex_heatmap_height * 100, res = 300)
     } else if (input$download_complex_heatmap_type == "tiff") {
       tiff(file, width = input$download_complex_heatmap_width * 100, height = input$download_complex_heatmap_height * 100, res = 300)
     } else {
       stop("Unsupported file format.")
     }
     
     ComplexHeatmap::draw(rv_complex_heatmap$plot)
     dev.off()
   }
 )
 
 output$complex_heatmap_help <- renderText({
   req(test_output())
   help_file <- help("Heatmap", package = "ComplexHeatmap")
   if (is.null(help_file)) return("Documentation not found.")
   
   temp <- tempfile("docs")
   tools::Rd2HTML(utils:::.getHelpFile(help_file), out = temp)
   content <- readLines(temp)
   file.remove(temp)
   paste(content, collapse = "\n")
 })
 
 
 observeEvent(input$statistics_active_tab, {
   if (input$statistics_active_tab == "Statistical test plot") {
     output$stats_download_options_ui <- renderUI({
       tagList(
         uiOutput("download_statistical_test_plot_options_ui"),
         uiOutput("download_statistical_test_plot_width_ui"),
         uiOutput("download_statistical_test_plot_height_ui"),
         uiOutput("download_statistical_test_plot_type_ui"),
         uiOutput("download_statistical_test_plot_plot_ui")
       )
     })
   } else if (input$statistics_active_tab == "Heatmap") {
     output$stats_download_options_ui <- renderUI({
       tagList(
         uiOutput("download_complex_heatmap_options_ui"),
         uiOutput("download_complex_heatmap_width_ui"),
         uiOutput("download_complex_heatmap_height_ui"),
         uiOutput("download_complex_heatmap_type_ui"),
         uiOutput("download_complex_heatmap_plot_ui")
       )
     })
   } else if (input$statistics_active_tab == "lmer visualization") {
     output$stats_download_options_ui <- renderUI({
       tagList(
         uiOutput("download_lmer_options_ui"),
         uiOutput("download_lmer_width_ui"),
         uiOutput("download_lmer_height_ui"),
         uiOutput("download_lmer_type_ui"),
         uiOutput("download_lmer_plot_ui")
       )
     })
   } else {
     output$stats_download_options_ui <- renderUI({ NULL })  # Hide options when no relevant tab is selected
   }
 })
  
}


shiny::shinyApp(ui = ui, server = server)


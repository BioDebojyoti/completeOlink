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
  "tools",
  "FSA",
  "gbRd",
  "patchwork"
)

for(pkg1 in list_of_packages1){
  library(pkg1, character.only = TRUE)
}


source("R/functions.R")

options(shiny.maxRequestSize=160*1024^2)

ui <- fluidPage(
  uiOutput("olink_data")
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
                             fileInput("file", "Upload data", accept = ".csv"),
                             actionButton("info_btn", label = "i", style = "padding: 2px 2px; font-size: 12px; line-height: 1; border: 1px solid #ccc; border-radius: 4px; background-color: #f9f9f9; color: #333; margin-left: 5px; cursor: pointer;")
                           ),
                           div(
                             style = "display: flex; align-items: center;",
                             fileInput("meta_file", "Upload meta data", accept = ".csv"),
                             actionButton("info_btn_meta", label = "m", style = "padding: 2px 2px; font-size: 12px; line-height: 1; border: 1px solid #ccc; border-radius: 4px; background-color: #f9f9f9; color: #333; margin-left: 5px; cursor: pointer;")
                            ),
                           shinyBS::bsPopover(id = "info_btn", title = "OLINK File Format", placement = "left", trigger = "click", options = list(container = "body")),
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
                     tabPanel("Data table: group-wise panel metrics", DT::dataTableOutput("group_panel_metrics"))
                   )
                 )
               )),
               tabPanel("Outlier Detection", fluidPage(
                 page_sidebar(
                   "",
                   sidebar = sidebar(
                     div(style = "overflow-y: auto; max-height: 90vh;", 
                         bsCollapse(id = "sidebar_collapse_tab2", open = c("Detection Method"), multiple = TRUE,
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
                     tabPanel("Exploratory View", plotOutput("exploratory_output")),             
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
                     tabPanel("Quantile-Quantile plot", div(style = "display: margin-top: 40px; height: calc(100vh - 100px);", plotOutput("qq_out", height = "100%"))),
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
                           bsCollapse(id = "sidebar_collapse", open = c("General Settings"), multiple = TRUE,
                                      # General Settings Section
                                      bsCollapsePanel("General Settings", style = "primary",
                                                      uiOutput("use_filtered_data_logical_ui"),
                                                      uiOutput("stats_panel_col_ui"),
                                                      uiOutput("variable_col_ui"),
                                                      uiOutput("test_col_ui")
                                      ),
                                      # Model Parameters Section
                                      bsCollapsePanel("Parameters", style = "info",
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
                                                      )
                                      ),
                                      # Run and Plot Options
                                      bsCollapsePanel("Run", style = "success",
                                                      uiOutput("run_button_ui"),
                                                      hr(),
                                                      uiOutput("plot_button_lmer_ui"),
                                                      uiOutput("plot_olinkid_list_ui"),
                                                      uiOutput("plot_x_axis_variable_ui"),
                                                      uiOutput("plot_col_variable_ui"),
                                                      uiOutput("plot_number_of_proteins_per_plot_ui")
                                      ),
                                      
                                      # Download Options
                                      bsCollapsePanel("Download Options", style = "danger",
                                                      uiOutput("download_lmer_options_ui"),
                                                      uiOutput("download_lmer_width_ui"),
                                                      uiOutput("download_lmer_height_ui"),
                                                      uiOutput("download_lmer_type_ui"),
                                                      uiOutput("download_lmer_plot_ui")
                                      )
                           )
                       )
                     ),                     
                     tabsetPanel(
                       tabPanel("Statistical test result", DT::dataTableOutput("test_result")),
                       tabPanel("Statistical test plot", 
                                div(
                                  style = "height: 100vh; width: 100vw; display: flex; align-items: center; justify-content: center;",
                                  plotlyOutput("statistical_test_plot_out", height = "800px", width = "800px")
                                ),
                       ),
                       tabPanel("Statistical test log", uiOutput("test_log")),
                       tabPanel("Documentation/ Help", htmlOutput("test_help")),
                       tabPanel("Additional visualization", plotOutput("olink_lmer_plot_output"))
                     )
                   )
                 )
               ),
               tabPanel("Post-hoc Statistics", fluidPage(
                 page_sidebar(
                   "",
                   sidebar = sidebar(
                     div(style = "overflow-y: auto; max-height: 90vh;", 
                         bsCollapse(id = "sidebar_collapse_tab5", open = c("posthoc options"), multiple = TRUE,
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
               tabPanel("Additional Visualization", fluidPage(
                 page_sidebar(
                   "",
                   sidebar = sidebar(
                     div(style = "overflow-y: auto; max-height: 90vh;", 
                         bsCollapse(id = "sidebar_collapse_tab6", open = c("plot options"), multiple = TRUE,
                            bsCollapsePanel("Plot Options", style = "primary",                        
                              uiOutput("statistical_test_boxplot_variable_ui"),
                              uiOutput("statistical_test_boxplot_olink_ui"),
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
                                plotOutput("statistical_test_boxplot_out", height = "800px", width = "800px")
                              )
                     )
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
  
  # Reactive expression to read the uploaded file
  data <- reactive({
    req(input$file)
    OlinkAnalyze::read_NPX(input$file$datapath)
  })

  
  # Generate and display data tables
  output$data_uploaded <- DT::renderDataTable({
    req(data(), input$process_data)
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
    req(input$meta_file)
    read.csv(input$meta_file$datapath, sep=",")
  })
  
  # Generate and display data tables
  output$meta_uploaded <- DT::renderDataTable({
    req(meta_data(), input$process_data)
    datatable(
      meta_data(),
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        filter = 'top' # Enable column filters
      ),
      filter = 'top'
    )
  }) 
  
  full_data <- reactive({
    req(data(), meta_data())
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
    req(data(), meta_data(), input$process_data)
    datatable(
      full_data(),
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        filter = 'top' # Enable column filters
      ),
      filter = 'top'
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
      caption = paste0(input$panel_col," panel")
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
      caption = paste0("panel: ",input$panel_col,"\nGroup: ",input$group_col)
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
    numericInput("facetNrow_val", "rows for panel", value = 1L, min = 1L, max = 10L)
  })
  
  output$facetNcol_val_ui <- renderUI({
    req(full_data(), input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot", "olink_qc_plot"))
    numericInput("facetNcol_val", "columns for panel", value = 2L, min = 1L, max = 10L)
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
      df = full_data(),
      panel = input$outlier_panel,
      color = input$outlier_color,
      x_value = if(input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot")) input$outlier_x_val else 1L,
      y_value = if(input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot")) input$outlier_y_val else 2L,
      label_samples_logical = if(input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot")) input$label_samples_logical else FALSE,
      drop_assays_logical = if(input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot")) input$drop_assays_logical else FALSE,
      drop_samples_logical = if(input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot")) input$drop_assays_logical else FALSE,
      byPanel_logical = if(input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot")) input$by_panel_logical else FALSE,
      outlierDefX_val = if(input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot")) input$outlierDefX_val else NA,
      outlierDefY_val = if(input$outlier_plot_method %in% c("olink_pca_plot", "olink_umap_plot")) input$outlierDefY_val else NA,
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
    rv_outlier$plot <- outlier_detection_plot_output() # Store the plot in reactive values
    
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
    req(full_data(), input$outlier_plot_method, input$outlier_color, input$outlier_panel, input$run_outlier_plot, input$outlierLines_logical, input$label_samples_logical)
    h4("Download Options")
  })
  
  output$download_width_outlier_ui <- renderUI({
    req(full_data(), input$outlier_plot_method, input$outlier_color, input$outlier_panel, input$run_outlier_plot, input$outlierLines_logical, input$label_samples_logical)
    numericInput("download_width_outlier", "Width (inches)", value = 8, min = 1)
  })
  
  output$download_height_outlier_ui <- renderUI({
    req(full_data(), input$outlier_plot_method, input$outlier_color, input$outlier_panel, input$run_outlier_plot, input$outlierLines_logical, input$label_samples_logical)
    numericInput("download_height_outlier", "Height (inches)", value = 6, min = 1)
  })
  
  output$download_type_outlier_ui <- renderUI({
    req(full_data(), input$outlier_plot_method, input$outlier_color, input$outlier_panel, input$run_outlier_plot, input$outlierLines_logical, input$label_samples_logical)
    selectInput("download_type_outlier", "File Type", choices = c("pdf", "png", "jpg"))
  })
  
  output$download_plot_outlier_ui <- renderUI({
    req(full_data(), input$outlier_plot_method, input$outlier_color, input$outlier_panel, input$run_outlier_plot, input$outlierLines_logical, input$label_samples_logical)
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
  filtered_data <- reactive({
    req(full_data(), input$filter_data_button)
    clean_data <- full_data()
    
    if (input$filter_sample_id_logical) {
      req(input$filter_sample_id_list)

      if(length(input$filter_sample_id_list) > 0){
        clean_data <- clean_data %>% dplyr::filter(!(SampleID %in% input$filter_sample_id_list))
      }
    }
    
    if (input$filter_assay_logical) {
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
    if(input$use_filtered_data_logical){
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
    if(input$use_filtered_data_logical){
      req(filtered_data())
    } else {
      req(full_data())      
    }
    selectInput("test_col", "Select test", selected = NULL, choices = statistical_test_list)
  })   
  
  
  column_types <- reactive({
    req(input$stats_panel_col, input$use_filtered_data_logical)
    if(input$use_filtered_data_logical){
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
    if(input$use_filtered_data_logical){
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
    if(input$use_filtered_data_logical){
      req(filtered_data())
    } else {
      req(full_data())
    }
    radioButtons("pair_id", "Pair-id", selected = FALSE, choices =  c(TRUE, FALSE))
  })
  
  output$pair_id_col_ui <- renderUI({
    req(input$test_col %in% c("olink_ttest","olink_wilcox"), input$pair_id == TRUE, input$use_filtered_data_logical) 
    if(input$use_filtered_data_logical){
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
    if(input$use_filtered_data_logical){
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
    if(input$use_filtered_data_logical){
      req(filtered_data())
    } else {
      req(full_data())
    }
    
    textInput(
      "model_formula_text", 
      "Provide model formula", 
      value = NULL)
  })
  
  output$return_covariate_ui <- renderUI({
    req(input$test_col %in% c("olink_anova", "olink_lmer", "olink_ordinalRegression"), input$use_filtered_data_logical) 
    
    if(input$use_filtered_data_logical){
      req(filtered_data())
    } else {
      req(full_data())
    }
    
    radioButtons("return_covariates", "Return covariates", choices = c(FALSE, TRUE))
  })
  
  output$dependence_ui <- renderUI({
    req(input$test_col %in% c("olink_one_non_parametric"), input$use_filtered_data_logical) 
    if(input$use_filtered_data_logical){
      req(filtered_data())
    } else {
      req(full_data())
    }
    
    radioButtons("dependence_text", "dependence", choices = c(FALSE, TRUE))
  })
  
  output$subject_ui <- renderUI({
    req(input$test_col %in% c("olink_one_non_parametric"), input$dependence_text == TRUE, input$use_filtered_data_logical)
    if(input$use_filtered_data_logical){
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
    if(input$use_filtered_data_logical){
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
    if(input$use_filtered_data_logical){
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
    req(
      input$run_test,
      input$stats_panel_col,
      input$test_col, 
      input$use_filtered_data_logical
    )
    if(input$use_filtered_data_logical){
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
      req(!is.null(input$variable_col) | !is.null(input$model_formula_text) |input$model_formula_text != "", input$return_covariates)
    } else if (input$test_col %in% c("olink_ordinalRegression")){
      req(input$variable_col, input$return_covariates)
    } else if (input$test_col == "olink_one_non_parametric") {
      req(input$variable_col, input$dependence_text)
    }
    
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
  })
  
  output$test_result <- DT::renderDataTable({
    req(test_output(),input$test_col, input$run_test)
    datatable(
      test_output()[[1]] %>% 
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
  
  output$test_log <- renderUI({
    req(input$run_test, test_output())
    message_items <- lapply(test_output()[[2]], function(msg) {
      tags$p(msg)
    })
    do.call(tagList, message_items)
  })
  # statistical test plot
  
  statistical_test_plot_output <- reactive({
    req(
      test_output(), 
      input$variable_col, 
      input$test_col 
    )
    if(input$test_col %in% c("olink_anova", "olink_lmer", "olink_ordinalRegression", "olink_one_non_parametric")){
      plot_msg(paste0("Try <span style='color:green;'>", paste0(input$test_col, "_posthoc", collapse = ""),"</span> analysis"))
    } else {
      statistical_test_plot(test_output()[[1]], input$variable_col, input$test_col)
    }
    
  })
  
  output$statistical_test_plot_out <- renderPlotly({
    req(statistical_test_plot_output())
    statistical_test_plot_output()
  })   
  
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
    if(is.null(input$filter_term_olink_ids) | input$filter_term_olink_ids == ""){
      test_output()[[1]] %>% 
        dplyr::filter(Threshold == 'Significant') %>%
        dplyr::select(OlinkID) %>%
        dplyr::distinct() %>%
        dplyr::pull()
    } else {
      test_output()[[1]] %>% 
        dplyr::filter(Threshold == 'Significant' & term == input$filter_term_olink_ids) %>%
        dplyr::select(OlinkID) %>%
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
    if(input$use_filtered_data_logical){
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
    if(input$use_filtered_data_logical){
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
    if(input$use_filtered_data_logical){
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
    if(input$use_filtered_data_logical){
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
      input$posthoc_olinkid_list,
      input$use_filtered_data_logical
    )
    if(input$use_filtered_data_logical){
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
    
    posthoc_statistics(
      df = df,
      posthoc_effect = if (input$test_col %in% c("olink_lmer","olink_anova", "olink_ordinalRegression")) input$posthoc_effect else NULL,
      posthoc_effect_formula = if (input$test_col %in% c("olink_lmer","olink_anova", "olink_ordinalRegression")) input$posthoc_effect_formula_text else NULL,
      panel_col = input$stats_panel_col,
      test_col = input$test_col,
      variable = input$variable_col,
      olink_list = input$posthoc_olinkid_list,
      covariate_val = if (input$test_col %in% c("olink_anova", "olink_lmer", "olink_ordinalRegression")) input$covariate_col else NULL,
      posthoc_model_formula = if (input$test_col %in% c("olink_anova", "olink_lmer")) input$model_formula_text else NULL,
      random_list = if (input$test_col %in% c("olink_lmer")) input$random_effect else NULL,
      return_mean = if (input$test_col %in% c("olink_lmer","olink_anova", "olink_ordinalRegression")) input$posthoc_mean_return else FALSE,
      posthoc_padj_method = if (input$test_col %in% c("olink_lmer","olink_anova", "olink_ordinalRegression")) input$posthoc_padjt_method else NULL,
      posthoc_test = if (input$test_col %in% c("olink_one_non_parametric")) input$posthoc_test else NULL
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
    req(input$test_col, input$stats_panel_col,input$use_filtered_data_logical)
    if(input$use_filtered_data_logical){
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
    if(input$use_filtered_data_logical){
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
      
      if(input$panel != "all"){
        df2use <- df2use %>% dplyr::filter(Panel == input$stats_panel_col)
      }
      
      list2pass <- df2use %>% 
        dplyr::distinct(OlinkID) %>%
        dplyr::pull()
    }
    
    list2pass
  })
  
  output$statistical_test_boxplot_olink_ui <- renderUI({
    req(input$test_col, input$stats_panel_col, input$use_filtered_data_logical)
    if(input$use_filtered_data_logical){
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
    if(input$use_filtered_data_logical){
      req(filtered_data())
      df <- filtered_data()
    } else {
      req(full_data())
      df <- full_data()
    }
    numericInput("boxplot_number", "number of proteins per plot", value = 4, min = 1, max = 12)
  })
  
  output$statistical_test_boxplot_run_ui <- renderUI({
    req(input$test_col, input$stats_panel_col, input$boxplot_variable_list,input$boxplot_olink_list, input$use_filtered_data_logical)
    if(input$use_filtered_data_logical){
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
    req(input$stats_panel_col, input$boxplot_variable_list,input$boxplot_olink_list, input$generate_boxplot, input$use_filtered_data_logical)
    if(input$use_filtered_data_logical){
      req(filtered_data())
      df2plot <- filtered_data()
    } else {
      req(full_data())
      df2plot <- full_data()
    }
    
    
    if(input$stats_panel_col != "all"){
      df2plot <- df2plot %>% dplyr::filter(Panel == input$stats_panel_col)
    }
    
    df2plot <- df2plot %>% 
      dplyr::filter(!(grepl("control|ctrl", SampleID, ignore.case = TRUE))) %>%      
      dplyr::filter(!(grepl("control|ctrl", Assay, ignore.case = TRUE))) %>%
      dplyr::filter(!is.na(NPX))
    
    p <- OlinkAnalyze::olink_boxplot(
      df = df2plot,
      variable = input$boxplot_variable_list,
      olinkid_list = input$boxplot_olink_list,
      verbose = FALSE,
      number_of_proteins_per_plot = input$boxplot_number
    )
    
    rv$plot <- p[[1]] # Store the plot in reactive values
    
    p[[1]]
    
  })
  
  output$download_options_ui <- renderUI({
    req(input$test_col, input$stats_panel_col, input$boxplot_variable_list,input$boxplot_olink_list, input$generate_boxplot, input$use_filtered_data_logical)
    if(input$use_filtered_data_logical){
      req(filtered_data())
    } else {
      req(full_data())
    }
    h4("Download Options")
  })
  
  output$download_width_ui <- renderUI({
    req(input$test_col, input$stats_panel_col, input$boxplot_variable_list,input$boxplot_olink_list, input$generate_boxplot, input$use_filtered_data_logical)
    if(input$use_filtered_data_logical){
      req(filtered_data())
    } else {
      req(full_data())
    }
    numericInput("download_width", "Width (inches)", value = 8, min = 1)
  })
  
  output$download_height_ui <- renderUI({
    req(input$test_col, input$stats_panel_col, input$boxplot_variable_list,input$boxplot_olink_list, input$generate_boxplot, input$use_filtered_data_logical)
    if(input$use_filtered_data_logical){
      req(filtered_data())
    } else {
      req(full_data())
    }
    numericInput("download_height", "Height (inches)", value = 6, min = 1)
  })
  
  output$download_type_ui <- renderUI({
    req(input$test_col, input$stats_panel_col, input$boxplot_variable_list,input$boxplot_olink_list, input$generate_boxplot, input$use_filtered_data_logical)
    if(input$use_filtered_data_logical){
      req(filtered_data())
    } else {
      req(full_data())
    }
    selectInput("download_type", "File Type", choices = c("pdf", "png", "jpg"))
  })
  
  output$download_plot_ui <- renderUI({
    req(input$test_col, input$stats_panel_col, input$boxplot_variable_list,input$boxplot_olink_list, input$generate_boxplot, input$use_filtered_data_logical)
    if(input$use_filtered_data_logical){
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
  
  
  output$posthoc_test_help <- renderText({
    req(input$test_col, test_output())
    temp = Rd2HTML(Rd_fun(paste0(input$test_col,"_posthoc", collapse = "")),
                   out = tempfile("docs"))
    content = read_file(temp)
    file.remove(temp)
    content
  })

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
    if(input$use_filtered_data_logical){
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
   req(
     input$run_test,
     test_output(),
     input$stats_panel_col,
     input$test_col == "olink_lmer",
     input$variable_col, 
     input$random_effect,
     input$plot_x_axis_variable,
     input$plot_col_variable, 
     input$use_filtered_data_logical
   )
   if(input$use_filtered_data_logical){
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
   if(input$use_filtered_data_logical){
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
  
}

options(shiny.port = 9999)
shiny::shinyApp(ui = ui, server = server)


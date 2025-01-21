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
  "EnhancedVolcano",
  "ggplot2", 
  "plotly",
  "webshot",
  "this.path",
  "shinyBS",
  "bslib",
  "OlinkAnalyze",
  "plotly"
)

for(pkg1 in list_of_packages1){
  library(pkg1, character.only = TRUE)
}


source("R/functions.R")

options(shiny.maxRequestSize=80*1024^2)

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
                     div(
                       style = "display: flex; align-items: center;",
                       fileInput("file", "Upload data", accept = ".csv"),
                       actionButton(
                         "info_btn",
                         label = "i",
                         style = "padding: 2px 2px;
                       font-size: 12px;
                       line-height: 1;
                       border: 1px solid #ccc;
                       border-radius: 4px;
                       background-color: #f9f9f9;
                       color: #333;
                       margin-left: 5px;
                       cursor: pointer;"
                       )
                     ),
                     div(
                       style = "display: flex; align-items: center;",
                       fileInput("meta_file", "Upload meta data", accept = ".csv"),
                       actionButton(
                         "info_btn_meta",
                         label = "i",
                         style = "padding: 2px 2px;
                       font-size: 12px;
                       line-height: 1;
                       border: 1px solid #ccc;
                       border-radius: 4px;
                       background-color: #f9f9f9;
                       color: #333;
                       margin-left: 5px;
                       cursor: pointer;"
                       )
                     ),
                     # Tooltip content for the info button
                     shinyBS::bsPopover(
                       id = "info_btn",
                       title = "OLINK File Format",
                       placement = "left",
                       trigger = "click",
                       options = list(container = "body")
                     ),
                     uiOutput("panel_col_ui"),
                     uiOutput("group_col_ui"),
                     uiOutput("assay_ui"),
                     fluidRow(
                       div(
                         style = "display: flex; gap: 10px; margin-top: 10px; margin-left: 20px;",
                         actionButton("process_data", "Process uploaded data")
                       ),
                       div(style = "display: flex; gap: 20px; margin-top: 10px; margin-left: 20px;", actionButton("go_pca", "Plot PCA"), ),
                       div(style = "display: flex; gap: 20px; margin-top: 10px; margin-left: 20px;", actionButton("go_qq", "Plot QQ"))
                     ),
                     div(
                       style = "display: flex; gap: 20px; margin-top: 10px; margin-left: 20px;",
                       sliderInput(
                         "pointsize_pca",
                         "point-size PCA",
                         min = 0,
                         max = 12,
                         value = 3,
                         step = 0.5
                       )
                     ),
                     div(
                       style = "display: flex; gap: 20px; margin-top: 10px; margin-left: 20px;",
                       sliderInput(
                         "labelsize_pca",
                         "label-size PCA",
                         min = 0,
                         max = 12,
                         value = 3,
                         step = 0.5
                       )
                     ),
                     div(
                       style = "display: flex; gap: 20px; margin-top: 10px; margin-left: 20px;",
                       radioButtons(
                         "addEllipse_pca",
                         "enable ellipse on PCA",
                         choices = c(TRUE, FALSE),
                         selected = FALSE
                       ),
                     )
                   ),
                   tabsetPanel(
                     tabPanel("Data table: raw", DT::dataTableOutput("data_uploaded")),
                     tabPanel("Data table: meta", DT::dataTableOutput("meta_uploaded")),
                     tabPanel(
                       "Data table: complete",
                       DT::dataTableOutput("full_uploaded")
                     ),
                     tabPanel(
                       "Data table: panel metrics",
                       DT::dataTableOutput("panel_metrics")
                     ),
                     tabPanel(
                       "Data table: group-wise panel metrics",
                       DT::dataTableOutput("group_panel_metrics")
                     ),
                     tabPanel(
                       "PCA plot",
                       div(style = "height: calc(100vh - 100px);", # Adjust for navbar and margins
                           plotOutput("pca_out", height = "90%"))
                     ),
                     tabPanel(
                       "Quantile-Quantile plot",
                       div(style = "height: calc(100vh - 100px);", # Adjust for navbar and margins
                           plotOutput("qq_out", height = "100%"))
                     )
                   )
                 )
               )),
               tabPanel("Statistics", fluidPage(
                 page_sidebar(
                   "",
                   sidebar = sidebar(
                     uiOutput("variable_col_ui"),
                     uiOutput("test_col_ui"),
                     uiOutput("pair_id_ui"),
                     uiOutput("model_formula_ui"),
                     uiOutput("return_covariate_ui"),
                     uiOutput("random_effects_ui"),
                     uiOutput("dependence_ui"),
                     uiOutput("subject_ui"),
                     conditionalPanel(condition = 'input.test_col %in% c("olink_ttest","olink_wilcox")', uiOutput("pair_id_col_ui")),
                     conditionalPanel(condition = 'input.test_col %in% c("olink_anova")', uiOutput("covariate_col_ui")),
                     uiOutput("run_button_ui")
                   ),
                   tabsetPanel(
                     tabPanel("Statistical test result", DT::dataTableOutput("test_result")),
                     tabPanel("Statistical test log", uiOutput("test_log"))
                   )
                 )
               )
              ),
              tabPanel("Post-hoc Statistics", fluidPage(
                page_sidebar(
                  "",
                  sidebar = sidebar(
                    uiOutput("posthoc_variable_ui"),
                    uiOutput("use_olink_ids_ui"),
                    uiOutput("filter_term_olink_ids_ui"),
                    uiOutput("olink_ids_ui"),
                    uiOutput("posthoc_random_ui"),
                    uiOutput("posthoc_effect_ui"),
                    uiOutput("posthoc_model_formula_ui"),
                    uiOutput("posthoc_effect_formula_ui"),
                    uiOutput("posthoc_covariates_ui"),
                    uiOutput("posthoc_mean_return_ui"),
                    uiOutput("posthoc_padjt_method_ui"),
                    uiOutput("posthoc_test_ui"),
                    uiOutput("run_posthoc_ui")
                  ),
                  tabsetPanel(
                    tabPanel("Posthoc Statistical test result", DT::dataTableOutput("posthoc_test_result")),
                    tabPanel("Posthoc Statistical test log", uiOutput("posthoc_test_log"))
                  )
                )
              )
              ),
              tabPanel("Visualization", fluidPage(
                page_sidebar(
                  "",
                  sidebar = sidebar(
                    uiOutput("statistical_test_boxplot_variable_ui"),
                    uiOutput("statistical_test_boxplot_olink_ui"),
                    uiOutput("statistical_test_boxplot_number_ui"), 
                    uiOutput("statistical_test_boxplot_run_ui"),
                    hr(),
                    # Conditional download options
                    conditionalPanel(
                      condition = "input.generate_boxplot > 0",
                      h4("Download Options"),
                      numericInput("download_width", "Width (inches)", value = 8, min = 1),
                      numericInput("download_height", "Height (inches)", value = 6, min = 1),
                      selectInput("download_type", "File Type", choices = c("pdf", "png", "jpg")),
                      downloadButton("download_plot", "Download Plot")
                    )
                  ),
                  tabsetPanel(
                    tabPanel("Statistical test plot", 
                             div(
                               style = "height: 100vh; width: 100vw; display: flex; align-items: center; justify-content: center;",
                               plotlyOutput("statistical_test_plot_out", height = "1000px", width = "1200px")
                             )
                    ),
                    tabPanel("Boxplot", 
                             div(
                               style = "height: 100vh; width: 100vw; display: flex; align-items: center; justify-content: center;",
                               plotOutput("statistical_test_boxplot_out", height = "1000px", width = "1000px")
                               )
                    )
                  )
                )
              )
              )
            )
  })
  
  # info button
  observeEvent(req( input$info_btn), {
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
  # Reactive expression to read the uploaded file
  data <- reactive({
    req(input$file)
    OlinkAnalyze::read_NPX(input$file$datapath)
  })
  # read.csv(input$file$datapath, sep=";")
  
  
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
    dplyr::left_join(
      data(), 
      meta_data() %>%
        dplyr::mutate_if(is.numeric,as.character), by = "SampleID")
  })
  
  output$hasData <- reactive({
    req(full_data())
    !is.null(full_data()) && ncol(full_data()) > 0
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
    req( full_data(), input$panel_col)
    panel_wise_metrics_for_assay_detection(full_data(), input$panel_col)
  })
  
  output$panel_metrics <- DT::renderDataTable({
    req( panel_metrics(), input$panel_col)
    datatable(
      panel_metrics(),
      caption = paste0(input$panel_col," panel")
    )
  }) 
  
  group_specific_panel_assay_metrics <- reactive({
    req( full_data(), input$panel_col, input$group_col)
    group_specific_panel_assay_detection_percentage(full_data(), input$panel_col, input$group_col)
  })
  
  output$group_panel_metrics <- DT::renderDataTable({
    req( group_specific_panel_assay_metrics(), input$panel_col, input$group_col)
    datatable(
      group_specific_panel_assay_metrics(),
      caption = paste0("panel: ",input$panel_col,"\nGroup: ",input$group_col)
    )
  }) 

  pca_plot_output <- reactive({
    req(
      
      full_data(),
      input$go_pca,
      input$panel_col,
      input$group_col,
      input$pointsize_pca,
      input$labelsize_pca,
      input$addEllipse_pca     
    )
    pca_plot(
      full_data(),
      panel = input$panel_col,
      group = input$group_col,
      pointsize = input$pointsize_pca,
      labelsize = input$labelsize_pca,
      addEllipse = input$addEllipse_pca
    )
  })
  
  output$pca_out <- renderPlot({
    req(
      
      full_data(),
      input$go_pca,
      input$panel_col,
      input$group_col,
      input$pointsize_pca,
      input$labelsize_pca,
      input$addEllipse_pca, 
      pca_plot_output()
    )
    pca_plot_output()
  })
  
  # Generate UI for selecting log2FC and p-value columns
  output$panel_col_ui <- renderUI({
    req( full_data())
    selectInput("panel_col", "Select panel", choices = c("all",unique(full_data()[["Panel"]])))
  })
  
  output$group_col_ui <- renderUI({
    req( full_data())
    selectInput("group_col", "Select group column", choices = setdiff(colnames(full_data()),not_sample_label))
  })
  
  assay_list <- reactive({
    req( full_data(), input$panel_col)
    unique(
      full_data() %>%
        dplyr::filter(Panel == input$panel_col &
                        Sample_Type != "CONTROL")  %>%
        dplyr::distinct(Assay) %>%
        dplyr::pull(Assay)
    )
  })
  
  output$assay_ui <- renderUI({
    req( full_data(), input$panel_col, assay_list())
    selectInput(
      "curr_assay",
      "Select assay",
      choices = assay_list(),
      selected = NULL,
      multiple = FALSE
    )
  })
  
  
  output$qq_out <- renderPlot({
    req(
      
      full_data(),   
      input$go_qq,
      input$panel_col,
      input$curr_assay
      )
    qq_ploter(
      full_data(),
      panel = input$panel_col,
      assay = input$curr_assay
    )
  })
  
  # statistical page
  # Generate UI for selecting variable for statistical test
  output$test_col_ui <- renderUI({
    req( full_data())
    selectInput("test_col", "Select test", selected = NULL, choices = statistical_test_list)
  })   
  
  
  column_types <- reactive({
    req(full_data(), input$panel_col)
    
    df <- full_data() %>%
      dplyr::filter(
        !stringr::str_detect(SampleID, stringr::regex("control|ctrl", ignore_case = TRUE))
      ) %>%
      dplyr::filter(
        !stringr::str_detect(Assay, stringr::regex("control|ctrl", ignore_case = TRUE))
      )
    
    if(input$panel_col != "all"){
      df <- df %>%
        dplyr::filter(Panel == input$panel_col)
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
    req(full_data(), input$test_col)
    if (input$test_col %in% c("olink_ttest", "olink_wilcox")) {
      selectInput("variable_col", "Select variable", choices = setdiff(column_types(),not_variable_label), multiple = FALSE)
    } else {
      selectInput("variable_col", "Select variable", choices = setdiff(names(full_data()),not_variable_label), multiple = TRUE)
    }
  })  
  
  output$pair_id_ui <- renderUI({
    req(input$test_col %in% c("olink_ttest", "olink_wilcox"), full_data()) # Ensure full_data() is available
    radioButtons( 
      "pair_id", 
      "Pair-id", 
      selected = FALSE,
      choices =  c(TRUE, FALSE))
  })
  
  output$pair_id_col_ui <- renderUI({
    req(input$test_col %in% c("olink_ttest","olink_wilcox"), input$pair_id == TRUE, full_data()) # Ensure full_data() is available
    selectInput(
      "pair_id_col", 
      "Select pair-id column", 
      choices = setdiff(colnames(full_data()), not_variable_label)
    )
  })
  # choices = c("null", setdiff(colnames(full_data()), not_variable_label))
  
  output$covariate_col_ui <- renderUI({
    req(input$test_col %in% c("olink_anova", "olink_lmer", "olink_ordinalRegression"), full_data()) # Ensure full_data() is available
    selectInput(
      "covariate_col", 
      "Select covarite column", 
      choices = setdiff(colnames(full_data()), not_variable_label), 
      multiple = TRUE
    )
  })
  
  output$model_formula_ui <- renderUI({
    req(input$test_col %in% c("olink_anova", "olink_lmer"), full_data()) # Ensure full_data() is available
    textInput(
      "model_formula_text", 
      "Provide model formula", 
      value = NULL)
  })
  
  output$return_covariate_ui <- renderUI({
    req(input$test_col %in% c("olink_anova", "olink_lmer", "olink_ordinalRegression"), full_data()) # Ensure full_data() is available
    radioButtons( 
      "return_covariates", 
      "Return covariates", 
      selected = FALSE,
      choices =  c(TRUE, FALSE))
  })
  
  output$dependence_ui <- renderUI({
    req(input$test_col %in% c("olink_one_non_parametric"), full_data()) # Ensure full_data() is available
    radioButtons( 
      "dependence_text", 
      "dependence", 
      selected = FALSE,
      choices =  c(TRUE, FALSE))
  })

  output$subject_ui <- renderUI({
    req(input$test_col %in% c("olink_one_non_parametric"), input$dependence_text == TRUE, full_data()) # Ensure full_data() is available
    selectInput("subject_text", "Select subject", choices = setdiff(names(full_data()), not_variable_label), multiple = TRUE)
  })
  
  output$random_effects_ui <- renderUI({
    req(input$test_col %in% c("olink_lmer"), full_data()) # Ensure full_data() is available
    selectInput("random_effect", "Select random effects", choices = setdiff(names(full_data()), not_variable_label), multiple = TRUE)
  })
  
  output$run_button_ui <- renderUI({
    if(!is.null(full_data()) &&  !is.null(input$test_col)){
      actionButton("run_test",paste0("Run ", gsub("_"," ",input$test_col)))
    } 
  })
  
  test_output <- reactive({
    req(
      input$run_test,
      full_data(),
      input$panel_col,
      input$test_col
      )
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
      df = full_data(),
      panel_col = input$panel_col,
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
      # plot_msg("Try ",input$test_col,"_posthoc analysis for visualization")
      plot_msg(
        paste0("Try <span style='color:green;'>", input$test_col, "_posthoc</span> analysis")
        )
    } else {
      statistical_test_plot(test_output()[[1]], input$variable_col, input$test_col)
    }

  })
  
  output$statistical_test_plot_out <- renderPlotly({
    req(statistical_test_plot_output())
    statistical_test_plot_output()
  })   
  
  
  output$use_olink_ids_ui <- renderUI({
    req(input$test_col %in% c("olink_anova", "olink_lmer", "olink_ordinalRegression", "olink_one_non_parametric"), test_output()) # Ensure full_data() is available
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
    if(!is.null(full_data()) &&  !is.null(input$test_col) && input$test_col %in% c("olink_anova", "olink_one_non_parametric", "olink_ordinalRegression", "olink_lmer") && !is.null(test_output())){
      actionButton("run_posthoc", paste0("Run ",input$test_col,"_posthoc"))
    } 
  })

  output$posthoc_effect_ui <- renderUI({
    req(input$test_col %in% c( "olink_anova", "olink_lmer", "olink_ordinalRegression"), full_data(), test_output()) 
    selectInput("posthoc_effect", "Term (effect) to perform post-hoc on", choices = test_output()[[1]] %>% dplyr::distinct(term) %>% dplyr::pull(term), multiple = FALSE)
  })
  
  output$posthoc_effect_formula_ui <- renderUI({
    req(input$test_col %in% c("olink_anova", "olink_lmer", "olink_ordinalRegression"), full_data(), test_output()) # Ensure full_data() is available
    textInput(
      "posthoc_effect_formula_text", 
      "Effect formula for post-hoc", 
      value = NULL)
  })
  
  output$posthoc_mean_return_ui <- renderUI({
    req(input$test_col %in% c("olink_anova", "olink_lmer", "olink_ordinalRegression"), full_data(), test_output()) # Ensure full_data() is available
    radioButtons( 
      "posthoc_mean_return", 
      "Return mean of each factor level", 
      selected = FALSE,
      choices =  c(TRUE, FALSE))
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
      full_data(),
      input$run_test,
      input$run_posthoc,
      test_output(),
      input$panel_col,
      input$test_col,
      input$posthoc_olinkid_list
    )
    if (input$test_col %in% c("olink_anova", "olink_lmer")){
      req(!is.null(input$variable_col) | !is.null(input$model_formula_text) | input$model_formula_text != "", input$return_covariates, !is.null(input$posthoc_effect) | !is.null(input$posthoc_effect_formula_text))
    } else if (input$test_col %in% c("olink_ordinalRegression")){
      req(input$variable_col, input$return_covariates)
    } else if (input$test_col == "olink_one_non_parametric") {
      req(input$variable_col, input$dependence_text)
    }
    
    posthoc_statistics(
      df = full_data(),
      posthoc_effect = if (input$test_col %in% c("olink_lmer","olink_anova", "olink_ordinalRegression")) input$posthoc_effect else NULL,
      posthoc_effect_formula = if (input$test_col %in% c("olink_lmer","olink_anova", "olink_ordinalRegression")) input$posthoc_effect_formula_text else NULL,
      panel_col = input$panel_col,
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
    req(full_data(), input$test_col, input$panel_col)
    if (input$test_col %in% c("olink_ttest", "olink_wilcox")) {
      selectInput("boxplot_variable_list", "Variable(s) for box plot", choices = setdiff(column_types(),not_variable_label), multiple = FALSE)
    } else {
      selectInput("boxplot_variable_list", "Variable(s) for box plot", choices = setdiff(names(full_data()),not_variable_label), multiple = TRUE)
    }
  })
  
  statistical_test_boxplot_olink_list <- reactive({
    req(full_data(), input$test_col, input$panel_col)
    
    if(input$test_col == "olink_ttest" & !is.null(test_output())){
      df2use <- test_output()[[1]]
      list2pass <- df2use %>%
        dplyr::distinct(OlinkID) %>%
        dplyr::pull()
    } else if (input$test_col == "olink_anova" & !is.null(test_output())){
      df2use <- posthoc_output()[[1]]
      list2pass <- df2use %>%
        dplyr::distinct(OlinkID) %>%
        dplyr::pull()
    } else {
      df2use <- full_data() 
      
      if(input$panel != "all"){
        df2use <- df2use %>% dplyr::filter(Panel == input$panel_col)
      }
      
      list2pass <- df2use %>% 
        dplyr::distinct(OlinkID) %>%
        dplyr::pull()
    }
    
    list2pass
  })
    
  output$statistical_test_boxplot_olink_ui <- renderUI({
    req(full_data(), input$test_col, input$panel_col)
    selectInput("boxplot_olink_list", 
                "olinkID(s) for box plot", 
                choices = statistical_test_boxplot_olink_list(), 
                multiple = TRUE)
  })
  
  output$statistical_test_boxplot_number_ui <- renderUI({
    req(full_data(), input$test_col, input$panel_col)
    numericInput("boxplot_number",
                "number of proteins per plot", 
                value = 4,
                min = 1,
                max = 12
                )
  })
  
  output$statistical_test_boxplot_run_ui <- renderUI({
    req(full_data(), input$test_col, input$panel_col, input$boxplot_variable_list,input$boxplot_olink_list)
    actionButton("generate_boxplot", "Show boxplot")
  })
  
  # Reactive values to track the plot
  rv <- reactiveValues(plot = NULL)
  
  output$statistical_test_boxplot_out <- renderPlot({
    req(full_data(), input$panel_col, input$boxplot_variable_list,input$boxplot_olink_list, input$generate_boxplot)

    df2plot <- full_data()
    
    if(input$panel_col != "all"){
      df2plot <- df2plot %>% dplyr::filter(Panel == input$panel_col)
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

}

shiny::shinyApp(ui = ui, server = server)

# ,
# posthoc_results = ifelse(input$test_col == "olink_anova" & !is.null(posthoc_output()), posthoc_output()[[1]], NULL),
# ttest_results = ifelse(input$test_col == "olink_ttest" & !is.null(test_output()), test_output()[[1]], NULL)

## Improve: accept both  Machine Death Age (Days) as well as Age at Death (d) in the time of death column
## make line examples longer in ggplot legend
## ensure that the factor ordering (e.g. in the figure legends) is correct.

# source functions
source("./helper.R")
library(shinyDND)


#### UI

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Welcome", tabName = "Welcome", icon = icon("dashboard")),
    menuItem("Inputs", icon = icon("cog"), tabName = "Inputs_tab",
             badgeLabel = "required", badgeColor = "red",selected = TRUE),
    menuItem("Diagnostics", icon = icon("check-circle"), tabName = "Diagnostics_tab",
             badgeLabel = "plots", badgeColor = "green"),
    menuItem("Lifespan", icon = icon("th-large"), tabName = "Lifespan_tab",
             badgeLabel = "plots", badgeColor = "green"),
    menuItem("Download / Export", icon = icon("send"), tabName = "Export_tab"),
    menuItem("Contact", icon = icon("envelope"), tabName = "Contact_tab")
  )
)



body <- dashboardBody(
  useShinyjs(),
  # overall title
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  ############################################## Welcome ######################################
  tabItems(
    tabItem(tabName = "Welcome",
            div(img(src = "landing.png",height = 1547, width = 964),style="text-align: center;")
            ),

    
    ############################################## Inputs_tab ######################################
    tabItem(tabName = "Inputs_tab",
            h2("Imput, dataset upload"),
            setSliderColor(c("", "#f45342", "", ""), c(2)),
            
            fluidRow(
              # upload box
              box(title = "Upload lifespan files",width = 4,status = "primary",
              fileInput("data_upload", "Choose CSV File",
                        accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv",
                          ".xlsx")),
              
              # Input: Select Manual vs. lifespan machine
              radioButtons("data_origin", "Please select input mode",
                           choices = c(`LM detailed output` = "detailed_LM",
                                       Manually_measured = "manual",
                                       Official_Score_Sheet = "official_scoresheet",
                                       other = NULL),
                           selected = "detailed_LM"),
              
              # explorative analysis (should be directly determined from data)
              radioButtons("full_vs_explorative", "Please select analysis mode",
                           choices = c(Explorative_analysis = "explorative_analysis",
                                       Full_Analysis = "full_analysis"),
                           selected = "full_analysis"),
              
              actionButton("import_settings", "Apply import settings")
              ),
              
              
              box(title = "Please order the factor levels",width = 8,status = "primary",
                  uiOutput("UI_condition_ordering"),
                  verbatimTextOutput('condition_factor_order'),
                  tags$hr(),
                  uiOutput("UI_strain_ordering"),
                  verbatimTextOutput('strain_factor_order'),
                  tags$hr(),
                  actionButton("update_order", "Update the order of dataset")
            )
            ),
            
            fluidRow(
                box(title = "Specify column combination",width = 4,status = "primary",
                    radioButtons("machine_vs_human", "Please select whether to use human or machine data",
                                 choices = c("Only use machine information" = "machine_only",
                                             "Only use human information" = "human_only",
                                             "Use human and if incomplete use machine data" = "combine_human_first"),
                                 selected = "combine_human_first")
                )
            ),
            
            
            fluidRow(
              column(width = 4,
              # Infobox describing the uploaded data
              box(title = "Imported",width = 12,status = "primary",
                  infoBoxOutput("info_rows_upload_table")
              ),
              tags$br(),
              box(title = "Filtered & Frequency adjusted",width = 12,status = "primary",
                  infoBoxOutput("info_rows_processed_table")
              )
              ),
              
              column(width = 8,
              box(title = "Imported table",width = 12, status = "primary",
                 dataTableOutput("data_upload_table")
                 )
            )
            )
      
    ),
    
        
    ############################################## Lifespan_tab ######################################
    tabItem(tabName = "Lifespan_tab",
            h2("Lifespan_tab content"),
            fluidRow(
              box(title = "Purpose",width = 8,"The purpose of this tab is to ...                    
                  On this tab you can observe the lifespan distributions:",
                  tags$ul(
                    tags$li(tags$em("Strain")), 
                    tags$li(tags$em("Strain & Condition (if available)")), 
                    tags$li(tags$em("Device effects"))
                  )
              ),
              box(title = "Options",width = 4,
                  "Click here to modify", tags$strong("plotting"), "options:",
                  actionButton("toggle_lifespan_plot_options","Lifespan plot options")
                  )
              
            ),
            fluidRow(
              hidden(div(id = "settings_lifespan_plot_options",
                         box(title = "Plate options",width = 12,status = "primary",solidHeader = TRUE,
                             radioButtons(inputId = "show_censored",label = "Plot censored deaths:",choiceNames = c("TRUE","FALSE"),choiceValues = c(TRUE,FALSE),selected = TRUE),
                             radioButtons(inputId = "show_ls_conf_int",label = "Show confidence intervals on lifespan plots:",choiceNames = c("TRUE","FALSE"),choiceValues = c(TRUE,FALSE),selected = FALSE),
                             radioButtons(inputId = "facet_by_device",label = "Display curves per device:",choiceNames = c("all-in-one","by Device"),choiceValues = c(FALSE,TRUE),selected = FALSE),
                             uiOutput("UI_time_window")
                         )))
            ),
            
            
            
            
            conditionalPanel(condition = "input.full_vs_explorative == 'explorative_analysis'",
            fluidRow(
              box(title = "Overall lifespan", width = 12,status = "primary",solidHeader = TRUE,
                 plotOutput({"initial_plate_rank_plot"}))
            ),
            fluidRow(
              
              box(title = "Test text",width = 6,status = "primary",solidHeader = TRUE, 
                  div(textOutput("cond")), style = "font-size:40%"),
              
              box(title = "Test text",width = 6,status = "primary",solidHeader = TRUE, 
                  div(tableOutput("initial_plate_rank_table")), style = "font-size:40%")
            )
            ),
            
            conditionalPanel(condition = "input.full_vs_explorative == 'full_analysis'",
            fluidRow(
              box(title = "Survival by Strain",width = 12,status = "primary",solidHeader = TRUE, 
                plotOutput("survival_strain",height = "600px")
              )
            ),
            fluidRow(
              box(title = "Statistics on individual strata",width = 12,solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                  div(dataTableOutput("survival_strain_tab")), style = "font-size:70%")),
            fluidRow(
              box(title = "Log-Rank comparison between strata",width = 12,solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                  div(dataTableOutput("survival_strain_pval_matrix")), style = "font-size:70%")),
            fluidRow(
              div(class = "fig_desc",
                  box(title = "Additional information", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,width = 12,
                      textOutput("survival_strain_descr"))
              )
            ),
            
            #make conditional: it is only evaluated when data$Condition.1 has more than one value
            conditionalPanel(condition = "output.selected_test != '1'",
            fluidRow(
              box(title = "Survival by Strain & Condition",width = 12,status = "primary",solidHeader = TRUE, 
                  plotOutput("survival_strain_condition",height = "600px")
              )
            ),
            fluidRow(
              box(title = "Statistics on individual strata",width = 12,solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                  div(dataTableOutput("survival_strain_condition_tab")), style = "font-size:70%")
              ),
            fluidRow(
              box(title = "Log-Rank comparison between strata",width = 12,solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                  div(dataTableOutput("survival_strain_condition_pval")), style = "font-size:70%")
            ),
            fluidRow(
              div(class = "fig_desc",
                  box(title = "Additional information", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,width = 12,
                      textOutput("survival_strain_condition_descr"))
              )
            )
            ),
            
            
            fluidRow(
              box(title = "survival_by_plate",width = 12,status = "primary",solidHeader = TRUE, 
                  plotOutput("survival_by_plate",height = "600px")
              )
            ),
            fluidRow(
              box(title = "Statistics on individual strata",width = 12,solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                  div(dataTableOutput("survival_by_plate_tab")), style = "font-size:70%")
            ),
            fluidRow(
              div(class = "fig_desc",
                  box(title = "Additional information", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,width = 12,
                      textOutput("survival_by_plate_tab_descr"))
              )
            )
            )
            
            
    ),
    
    ############################################## Diagnostics tab ######################################
    tabItem(tabName = "Diagnostics_tab",
            h2("Diagnostic visualizations"),
              fluidRow(
                box(title = "Purpose",width = 8,"The purpose of this tab is to assess potential biases in the dataset. Only after the quality is assured the lifespan analysis can be interpreted.
                    On this tab you can explore if your dataset is:",
                    tags$ul(
                      tags$li(tags$em("homogenous")), 
                      tags$li(tags$em("bias-free")), 
                      tags$li(tags$em("interpetable"))
                    )
                ),
                box(title = "Options",width = 4,
                  "Click here to modify", tags$strong("plate"), "options:",
                  actionButton("toggle_plate_options","Plate options"),
                  br(),tags$hr(),
                  "Click here to modify", tags$strong("color"), " options:",
                  actionButton("toggle_plot_color_options","Visualization options")
                )

              ),
            
              fluidRow(
                
                hidden(div(id = "settings_diagnostics_tab_plate",
                  column(width = 6,
                  box(title = "Moderate dataset operations",width = 12, height = 300,background = "olive",
                      sliderInput("min_max_individuals_per_plate","Min & Max individuals per plate:",min=0,max=200,value = c(5,60)),
                      uiOutput("UI_exclude_plates")
                      )),
                  column(width = 6,
                  box(title = "Dangerous dataset operations",width = 12, height = 300,background = "yellow",
                      sliderInput("early_death_exclusion","Specify timepoint up to which all death are removed",min=0,max=10,value = c(0))
                  ))
                ))
              ),
            fluidRow(
              hidden(div(id = "settings_diagnostics_tab_color",
                  box(title = "Color & plotting options",width = 12,status = "primary",solidHeader = TRUE,
                      # textInput(inputId = "ctr.strain.col",label = "color for control population",value = "black"),
                      colourInput("ctr.strain.col", "Choose colour", "black"),
                      # textInput(inputId = "col_option",label = "color palette",value = "viridis"),
                      radioButtons(inputId = "col_option",inline = TRUE,label = "color palette",choiceNames = c("viridis","magma","plasma","inferno","cividis"),choiceValues = c("viridis","magma","plasma","inferno","cividis"),selected = "viridis"),
                      sliderInput("start_end_color","Color range per plate:",min=0,max=1,value = c(0.3,0.8)),
                      radioButtons(inputId = "palette_direction",label = "Color palette direction:",choiceNames = c("forward","reverse"),choiceValues = c(1,-1),selected = 1)
                   )
              ))
            ),
            
               fluidRow(
                 box(title = "Number of observed death events per plate", width = 12,status = "primary",solidHeader = TRUE,
                     plotOutput({"QC_bar_obs_deaths"},  height = "1000px"))

               ),
               fluidRow(
                 div(class = "fig_desc",
                     box(title = "Additional information", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,width = 12,
                         textOutput("QC_bar_obs_deaths_descr"))
                 )
               ),

               
               
               fluidRow(
                 box(title = "Correlation analysis: Crowding ~ lifespan",width = 12,status = "primary",solidHeader = TRUE,
                     plotOutput("QC_corr_numb_vs_ls")
                 )
               ),
               fluidRow(
                 box(width = 12,status = "primary",solidHeader = FALSE, 
                     div(dataTableOutput("QC_corr_numb_vs_ls_tab")), style = "font-size:70%")
               ),
               fluidRow(
                 div(class = "fig_desc",
                     box(title = "Additional information", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,width = 12,
                         textOutput("QC_corr_numb_vs_ls_descr"))
                 )
               ),
               
               conditionalPanel("input.data_origin != 'manual'",
               fluidRow(
                 box(title = "QC_scanner_loading",width = 12,status = "primary",solidHeader = TRUE,
                     plotOutput("QC_scanner_loading", height = "700px")
                 )
               ),
               fluidRow(
                 div(class = "fig_desc",
                     box(title = "Additional information", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,width = 12,
                         textOutput("QC_scanner_loading_descr"))
                 )
               )
               ),
               
               conditionalPanel("FALSE",
               fluidRow(
                 box(title = "QC_movement_with_age",width = 12,status = "primary",solidHeader = TRUE,collapsible = TRUE, collapsed = TRUE,
                     plotOutput("QC_movement_with_age")
                 )
               ),
               fluidRow(
                 div(class = "fig_desc",
                     box(title = "Additional information", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,width = 12,
                         textOutput("QC_movement_with_age_descr"))
                 )
               ),
               
               fluidRow(
                 box(title = "QC_box_plate_position",width = 12,status = "primary",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                     plotOutput("QC_box_plate_position")
                 )
               ),
               fluidRow(
                 div(class = "fig_desc",
                     box(title = "Additional information", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,width = 12,
                         textOutput("QC_box_plate_position_descr"))
                 )
               )
               
               )
               
    ),
    
    ############################################## Download & Export ######################################
      tabItem(tabName = "Export_tab",
              h2("Export content"),

              box(title = "Purpose",width = 8,"The purpose of this tab is to batch export the plots and tables you generated during this session.
                  On this tab you can export the following data:",
                  tags$ul(
                    tags$li(tags$em("Diagnostic plots")), 
                    tags$li(tags$em("Lifespan plots")), 
                    tags$li(tags$em("Tables"))
                  )
              ),
              box(title = "Select output to be exported", width = 4,
                  "Here the buttons for the selected downloads will be implemented shortly, in the mean time please use right click or print screen to download the content of the website."),
              
              fluidRow(
                box(title = "Project name",width = 8,status = "primary",
                    textInput("report_name","Name of Project","Lifespan analysis"),
                    textInput("report_author","Name of Author & Affiliation","Cyril Statzer, ETH Zürich")
                ),
                box(title = "Export results",solidHeader = TRUE,background = "olive",
                    downloadButton("report","Generate Report"))
              ),
              fluidRow(
                tableOutput("strain_condition_kable_Strain")
              ),
              fluidRow(
                tableOutput("strain_condition_kable_Strain_Cond")
              )
              
              
      ),
  
    ############################################## Contact ######################################
      tabItem(tabName = "Contact_tab",
          h1(tags$strong("Ewald lab, ETH Zurich")),
          # group image
          fluidRow(
            column(width = 4,
                div(img(src = "ewaldlogo.png",height = 180, width = 350),style="text-align: center;")
            ),
            column(width = 3,
                   box(title = "Contact us online",width = 12,
                   tags$a(href="https://ewaldlab.com","Group website")
                   # br(), tags$hr(),
                   # # twitter button
                   # tags$a(href="https://ewaldlab.com", "Tweet", class="twitter-share-button"),
                   # includeScript("http://platform.twitter.com/widgets.js")
                   )
            ),
            column(width = 3,
                   box(title = "Adress",width = 12,
                       "ETH Zurich",tags$br(),
                       "Extracellular Matrix Regeneration Laboratory",tags$br(),
                       "Schorenstrasse 16",tags$br(),
                        "8607 Schwerzenbach",tags$br(),
                       tags$strong("Switzerland")
                   )
                   )
          ),
          tags$br(),
          tags$br(),
          fluidRow(
            column(width = 4,
                  div(img(src = "cyril.jpg",height = 350, width = 350),style="text-align: center;")
            ),
            column(width = 3,
                   box(title = "Contact me directly",width = 12,
                       tags$em(
                         tags$span("Contact email: "),
                         tags$a("Cyril Statzer", href = "mailto:cyrilstatzer@gmx.ch"),
                         tags$br(), tags$br()
                       )
                   )
            )
          )
          )

  
  
    
    
    
  )
)




# Put them together into a dashboardPage
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Lifespan Tools"),
  sidebar,
  body
)






### SERVER


server <- function(input, output) {
  options(shiny.maxRequestSize=30*1024^2) # increase the maximum upload size
  ############################################## Inputs_tab ######################################
  upload <- function(input_list_accession){
    inFile <- input[["data_upload"]]
    if (is.null(inFile)) return(NULL)
    # test if excel file
    if ( grepl(x = inFile$name,pattern = ".xlsx") ){
      print("Excel file upload selected")
      excel_file <- readxl::read_xlsx(path = inFile$datapath)
      # if (ncol(excel_file) == 1) excel_file <- sapply(excel_file, function(x) str_split(x,pattern = ";") )
      return(excel_file)
    }
    # If not excel file test how the csv is separated
    firstline <- readLines(inFile$datapath, n = 1)
    sep <- ifelse( str_count(pattern = ",",string = firstline) > str_count(pattern = ";", string = firstline),   "comma",    "semicolon")
    # read csv using separator
    # csv_file <- try(read.csv(inFile$datapath,stringsAsFactors = FALSE,sep = sep)) # try to read uploaded file # original line before using readr function
    if (sep == "comma") csv_file <- try(read_csv(inFile$datapath)) # try to read uploaded file
    if (sep == "semicolon") csv_file <- try(read_csv2(inFile$datapath)) # try to read uploaded file
    print(paste("CSV file upload selected, with sep =", sep))
    if(is(csv_file,"try-error"))  return("Error when importing the .csv file, please make sure you fulfill all import criteria described on the landing page: header as illustrated, .csv file (commas preferred), no empty lines or cells.")
    return(csv_file)
  }

  
  
  # input dataset for app
  values <- reactiveValues(input_data = NULL,autocomp_data = NULL, data = NULL,
                           pal = NULL,Condition.1.col = NULL, Condition.1.alpha = NULL,
                           strains = NULL, conditions = NULL,
                           strain_factor_order = NULL, condition_factor_order = NULL,device_factor_order = NULL,
                           Initial_Strain_order = NULL, Initial_Condition_order = NULL,
                           status_color = "info", temp_input_data_room = NULL, temp_input_data_incubator = NULL,
                           fit_tab = NULL, plots = NULL, tables = NULL
                           )
  
  
  # temperature_code_block
  observeEvent(input$temp_input_data_room, {
    out_time_series <- load_time_series(input_list_accession = "temp_input_data_room")
    values$temp_input_data_room <- out_time_series$xts_table
    print(paste("nrow(values$temp_input_data_room): ",nrow(values$temp_input_data_room)))
  })
  observeEvent(input$temp_input_data_incubator, {
    out_time_series <- load_time_series(input_list_accession = "temp_input_data_incubator")
    values$temp_input_data_incubator <- out_time_series$xts_table
    print(paste("nrow(values$temp_input_data_incubator): ",nrow(values$temp_input_data_incubator)))
  })
  
  ###### STEP 1: Upload data - TRIGGERED by pressing the Apply import button.
  observeEvent(input$import_settings, {
    upload <- upload(input_list_accession = "data_upload")
    print("UPLOAD")
    print(paste("nrow(upload)",nrow(upload)))
    upload <- import_rename_columns(upload)
    values$input_data <- upload

    if (input$data_origin == "detailed_LM") {
      autocomp_data <- exclude_autocomplete(data = values$input_data, 
                                            machine_vs_human = input$machine_vs_human)
      print(table(autocomp_data$Censored))
      print(paste('input$data_origin == "detailed_LM", autocomp_data contains',nrow(autocomp_data),"rows"))
    }
    if (input$data_origin == "manual") {
      autocomp_data <- exclude_autocomplete_manual(data = values$input_data)
      # stats <- ls_stats(data = autocomp_data)
      #data_s <- stats$data_s
      print(paste('input$data_origin == "manual", autocomp_data contains',nrow(autocomp_data),"rows"))
    }
    if (input$data_origin == "official_scoresheet") {
      autocomp_data <- exclude_autocomplete_scoresheet(data = values$input_data)
      # stats <- ls_stats(data = autocomp_data)
      #data_s <- stats$data_s
      print(paste('input$data_origin == "official_scoresheet", autocomp_data contains',nrow(autocomp_data),"rows"))
    }
    values$autocomp_data <- autocomp_data
    print("values$autocomp_data")
    print(values$autocomp_data)
    
    # print(dim(values$autocomp_data))
    # print(head(values$autocomp_data['data_s']))
    # generate the initial factor levels in the uploaded dataset:
    print("print(values$Initial_Strain_order)")
    print(values$Initial_Strain_order)
    values$Initial_Strain_order <- unique(values$autocomp_data$Strain)[!is.na(unique(values$autocomp_data$Strain))]
    print(values$Initial_Strain_order)
    values$Initial_Condition_order <- na.omit(unique(values$autocomp_data$Condition.1))
  })
  

    
    

  
  # used for data reordering
  observeEvent(c(input$update_order), {
    values$data <- apply_factor_levels(data = values$autocomp_data, 
                                          levels_Strain = input$strain_factor_order,
                                          levels_Condition.1 = input$condition_factor_order,
                                          levels_Device = input$device_factor_order)
  })
  
  

  
  
  
  # color settings
  observeEvent(c(values$data,input$start_end_color,input$ctr.strain.col),{
  values$pal <- create_base_palettes(data = values$data,ctr.strain.col = input$ctr.strain.col, color_range = input$start_end_color,col_option = input$col_option, direction = input$palette_direction) 
  print(values$pal)
  values$Condition.1.col <- values$pal$Condition.1.col
  values$Condition.1.alpha <- values$pal$Condition.1.alpha
  # strains <- unique(values$data$Strain)
  # values$strains <- na.omit(strains)
  # conditions <- unique(values$data$Condition.1)
  # values$conditions <- na.omit(conditions)
  # devices <- unique(values$data$Device)
  # values$devices <- na.omit(devices)
  })
  
  # plate exclusion - by plate ID
  observeEvent(input$exclude_plates,
               values$data <- values$data[!values$data$Plate.Name %in% input$exclude_plates,]
               )
  # plate exclusion - by Min & Max
  observeEvent(input$min_max_individuals_per_plate,
               values$data <- Min_Max_plate_thr(data = values$data,min_max_individuals_per_plate = input$min_max_individuals_per_plate),ignoreInit = TRUE
  )

  
  ####### factor ordering ########
  # displayed in order box
  output$strain_factor_order <- renderPrint({ input$strain_factor_order })
  output$condition_factor_order <- renderPrint({ input$condition_factor_order }) 
  output$UI_strain_ordering <- renderUI({
    orderInput(inputId = 'strain_factor', label = 'Strain order (Levels)', items = values$Initial_Strain_order, placeholder = 'Drag items here...')
  })
  output$UI_condition_ordering <- renderUI({
    orderInput(inputId = 'condition_factor', label = 'Condition order (Levels)', items = values$Initial_Condition_order, placeholder = 'Drag items here...')
  })
  
  # time window
  output$UI_time_window <- renderUI({
    max_time <- values$data$Death %>% max(na.rm = TRUE) %>% round(0)
    sliderInput(inputId = 'time_window', label = 'Range of survival to examine:', min=0, max=max_time, value = c(0,60))
  })
  
  #test Text
  output$test_text <- renderText(levels(values$data$Condition.1))

  #### exclude plates
  output$UI_exclude_plates <- renderUI({

    checkboxGroupInput(inputId = "exclude_plates",label = "Plates_to_exclude",inline = TRUE, 
                       h3("Checkbox group"),
                       choices = unique(values$data$Plate.Name))

    })
  
  

  
  output$data_upload_table <- renderDataTable({ 
    show_cols <- c("Device","Strain","Condition.1","Censored","Excluded","Experiment","Frequency")
    values$data[,colnames(values$data) %in% show_cols]
    })
  
  
  output$info_rows_upload_table <- renderInfoBox({
    rows <- nrow(values$input_data)
    infoBox(title = "Import",value = paste0(rows,"# Observations") , icon = icon("table"),
            color = "light-blue")
  })
  output$info_rows_processed_table <- renderInfoBox({
    rows <- nrow(values$autocomp_data)
    infoBox(title = "Filtred",value = paste0(rows,"# Observations") , icon = icon("table"),
            color = "aqua")
  })
  
  
  # should be moved to UI later
  title <- "set title"
  start_col = 0.2
  end_col = 0.85
  # col_option = "viridis"
  direction = 1
  # ctr.strain.col <- "black"
  # max_individuals_per_plate <- 1000
  # min_individuals_per_plate <- 0
  
  # lifespan preview plot
  # output$lifespan_preview_plot <- renderPlot({
  #   fit <- ls_fit(data = values$data,yvar = 'Surv(Death,1-Censored)',xvars = c("Strain") )
  #   pal <- mix_pal_SCDPlates_fit(data = values$data,fit = fit,strain.col = values$pal$strain.col)
  #   lty <- mix_lty_fit(data = values$data,strata =  pal$strata) # what line to dash
  #   ggsurv <- ggsurvplot(fit = fit,data = values$data ,conf.int = F,legend = "top",title ="Strain",pval = FALSE,surv.median.line = "hv",censor =FALSE,risk.table = TRUE,tables.height = 0.2, xlab = "Time [days]",size = 2,linetype = lty,palette = pal$col.pal,
  #                        xlim = input$time_window)
  #   ggsurv
  # })
  

  ############################################## Diagnostics tab ######################################
  # QC_bar_obs_deaths
  observeEvent(input$toggle_plate_options,{
    toggle("settings_diagnostics_tab_plate",anim = TRUE)
  })
  observeEvent(input$toggle_plot_color_options,{
    toggle("settings_diagnostics_tab_color",anim = TRUE)
  })

  output$QC_bar_obs_deaths <- renderPlot({
    bar_obs_deaths <- plot_bar_obs_deaths(data = values$data,
                                          levels_Condition.1 = input$condition_factor_order, 
                                          strain.col = values$pal$strain.col,
                                          Condition.1.col = values$Condition.1.col,
                                          Condition.1.alpha = values$Condition.1.alpha
                                          )
    values$status_color <- ifelse(sd(values$data$Death) > 10,"info","warning") # improve this status diagnostic
    bar_obs_deaths + facet_grid(Trial ~ Device)
  })
  output$QC_bar_obs_deaths_descr <- renderText({
    "This figure depicts the animal loading pattern across plates by grouping the number of observed death events (including censored events) for each plate. The plate color refers to the used strain and the outline indicates the condition which was applied in the experiment.
    If multiple measurement devices are used the output is vertically aligned to make trends easily detectable. If for example the plates in a certain location are prone to fail (e.g. through desiccation) this pattern will be observable across devices.
    For standard plates (5-6 cm diameter) an animal number of 10 - 50 individuals is expected. Higher numbers bear the risk of dietary restriction, dauer pheromone secretion and can indicate a loss of sterility while lower animal numbers can indicate a general plate failure like reduced detection due to contamination.
    In both cases, animal loading numbers below 10 and above 50 should be handled with care and the corresponding plates potentially either manually excluded (see 'Inputs'-tab, plate exclusion) or a global individual number to be defined (see 'Inputs'-tab, animal number slider).
    When the animal loading is near the mentioned threshold values the correlation analysis in the next panel should be carefully studied.
    "
  })
  

  # QC_corr_numb_vs_ls
  output$QC_corr_numb_vs_ls <- renderPlot({
    dot_corr_numb_vs_ls <- plot_dot_corr_numb_vs_ls(data = values$data,
                                                    levels_Condition.1 = input$condition_factor_order,
                                                    strain.col = values$pal$strain.col,
                                                    Condition.1.col = values$Condition.1.col,
                                                    Condition.1.alpha = values$Condition.1.alpha
                                                    )
    values$fit_tab <- dot_corr_numb_vs_ls$fit_tab
    dot_corr_numb_vs_ls$g + facet_grid(. ~ Device)
  })
  output$QC_corr_numb_vs_ls_tab <- DT::renderDataTable({ 
    DT::datatable(data = values$fit_tab,options = list(lengthMenu = c(5,10,25,50,100,500), pageLength = 5, dom = 'tl'))
  })
  output$QC_corr_numb_vs_ls_descr <- renderText({
    "This figure depicts the observation of each plate as a dot plot two dimensions. The first dimension is the median lifespan observed per plate (x-axis: survival) and the number of observed death on each plate (y-axis, crowding).
      The purpose of the plot is to assess whether there is a correlation between these two variables. As an aid, a linear model and its confidence interval is shown.
      Be careful if you can observe a strong correlation (steep slope, slim confidence intervals) between the number of animals and the measured lifespan per plate (see above explanation).
      If you observe such correlations the experimental conditions have to be improved and likely Dauer pheromone signalling or food shortage affected the population survival.
    "
  })
  
  
  # Plate loading
  output$QC_scanner_loading <- renderPlot({
      row_col_filled_data <- mutate_row_col_fill(data = values$data)
      g <- plot_scanner_loading(row_col_filled_data = row_col_filled_data,
                                levels_Condition.1 = input$condition_factor_order,
                                strain.col = values$pal$strain.col,
                                Condition.1.col = values$Condition.1.col,
                                Condition.1.alpha = values$Condition.1.alpha
                                )
      g + facet_grid(Trial ~ Device)
  })
  output$QC_scanner_loading_descr <- renderText({
    "How where the scanners loaded? This plot provides a 'top-down' view on the scanner flatbed as it was loaded at the beginning of the experiment. Are the plates holding the different samples shuffled sufficiently?
    I recommend that any given sample should be assessed in each device to reduce (and be able to identify) scanner biases. Within each device the plates can be either randomly assorted or placed in the form of a chessboard pattern.
    Additionally, this plot provides a visual aid to see which locations in which scanners are prone to fail (no plate symbol visible).
    "
  })
  
  
  # Movement with age
  output$QC_movement_with_age <- renderPlot({
    movement_with_age <- plot_movement_with_age(data = values$data,
                                                levels_Condition.1 = input$condition_factor_order,
                                                strain.col = values$pal$strain.col,
                                                Condition.1.col = values$Condition.1.col,
                                                Condition.1.alpha = values$Condition.1.alpha
                                                )    
    movement_with_age + facet_grid(. ~ Strain)
  })
  output$QC_movement_with_age_descr <- renderText({
    "Here, each dot represents the life of one individual. On the x-axis, the lifespan is displayed and on the y-axis the duration the animal spent moving slowly before its death (minor movements like head movement and small displacements).
    While the movement cannot be quantified over the lifespan over the animal, the duration it spent slow moving at the very end of its life offers an initial insight into its behavior as it aged.
    To give an example of how to read this plot: 'A value on the x-axis of 20 and on the y-axis of 5 describes an animal which died at the age of 20 days and spent its last 5 days only moving slowly.
    "
  })
  
  
  # Movement with age
  output$QC_box_plate_position <- renderPlot({
    box_plate_position <- plot_box_plate_position(data = values$data,
                                                   strain.col = values$pal$strain.col,
                                                   Condition.1.col = values$Condition.1.col,
                                                   Condition.1.alpha = values$Condition.1.alpha
                                                  )
    box_plate_position + facet_grid(Trial ~ Device)
  })
    
  output$QC_box_plate_position_descr <- renderText({
    " In this plot the death times are shown for each plate in the form of a boxplot to illustrate their distribution across all strains, conditions and devices and make them easily visually comparable.
    "
  })
  
  

  

  

  
  
  
  


  ############################################## Lifespan_tab ######################################
  observeEvent(input$toggle_lifespan_plot_options,{
    toggle("settings_lifespan_plot_options",anim = TRUE)
  })
  
  
  # dummy plot
  output$plot1 <- renderPlot({
    plot(mtcars$wt, mtcars$mpg)
  })
  
  # initial plate rank 
  output$initial_plate_rank_plot <- renderPlot({
    out <- initial_plate_rank_plot(data = values$data)
    out$plot
    })
  output$initial_plate_rank_table <- renderTable({
    out <- initial_plate_rank_plot(data = values$data)
    out$plate_order
  })
  
  # test script
  output$selected_test <- renderText(length(unique(values$data$Condition.1)) )
  outputOptions(output, "selected_test", suspendWhenHidden = FALSE)
  
  # lifespan by strain
  output$survival_strain <- renderPlot({
    base_condition <- levels(values$data$Condition.1)[1]
    data <- values$data %>% filter(Condition.1 %in% base_condition)
    fit <- ls_fit(data = data,yvar = 'Surv(Death,1-Censored)',xvars = c("Strain") )
    col.pal <- make_col_palette(data = data,fit = fit,colname_to_color = "Strain", base_col = values$pal$strain.col)
    lty <- 1
    ggsurv <- ggsurvplot(fit = fit,data = data ,conf.int = as.logical(input$show_ls_conf_int), censor =  TRUE, legend = "top", title =paste0("Survival by strain (only for ",base_condition,")"), xlim = input$time_window, pval = TRUE, pval.method = TRUE, surv.median.line = "hv",censor.size = 12,risk.table = TRUE,tables.height = 0.3, xlab = "Time [days]",size = 2,linetype = lty,palette = col.pal)
    ggsurv$plot <- ggsurv$plot +  theme(legend.key.width = unit(4, "cm")) 
    # values$plots$survival_strain <- ggsurv
    ggsurv
  })
  output$survival_strain_tab <- DT::renderDataTable({ 
    base_condition <- levels(values$data$Condition.1)[1]
    data <- values$data %>% filter(Condition.1 %in% base_condition)
    out <- kable_table_assemble(force_merge_trials = TRUE, data = data,xvars = c("Strain"),round_digits = 2,facilitate_names = TRUE)
    table_display_reactive(single_table = out$stat_tables$Combined)
        })
  # overall pval matrix (to put at the end)
  output$survival_strain_pval_matrix <- DT::renderDataTable({ 
    base_condition <- levels(values$data$Condition.1)[1]
    data <- values$data %>% filter(Condition.1 %in% base_condition)
    pval_matrix <- pval_matrix(data = data,Strain = TRUE)
    DT::datatable(data = pval_matrix,options = list(pageLength = 50, dom = 't'))
  })
  
  output$survival_strain_descr <- renderText({
    "The survival curve is depicted for each strain. On the x-axis the time is depicted in days and on the y-axis the fraction of the population that is alive (1 refers to 100% alive).
    Underneath the figure a risk table is shown indicating the number of individuals that were at risk of dying at selected timepoints over the course of the experiment.
    The different colors refer to the different strains and can be retrieved from the figure legend at the top of the graph. Vertical dashed lines highlight the median survival timepoint (intersecting with the horizontal dashed line at 0.5 survival).
    The p-value is displayed at the bottom left together with the comparison method used (Log-rank).
    "
  })
  
  
  
  
  
  # lifespan by strain & condition
  output$survival_strain_condition <- renderPlot({
    if(length(unique(values$data$Condition.1)) <= 1) return(NULL)
    fit <- ls_fit(data = values$data,yvar = 'Surv(Death,1-Censored)',xvars = c("Strain","Condition.1") )
    col.pal <- make_col_palette(data = values$data,fit = fit,colname_to_color = "Strain", base_col = values$pal$strain.col)
    lty <- make_lty_palette(data = values$data, fit = fit, colname_to_set_lty = "Condition.1") # what line to dash
    print(lty)
    ggsurv <- ggsurvplot(fit = fit,data = values$data ,conf.int = as.logical(input$show_ls_conf_int),legend = "top",title ="Strain & Condition",xlim = input$time_window,pval = TRUE,pval.method = TRUE,surv.median.line = "hv",censor = as.logical(input$show_censored),censor.size = 12,risk.table = TRUE,tables.height = 0.4, xlab = "Time [days]",size = 2,linetype = lty,palette = col.pal)
    ggsurv$plot <- ggsurv$plot +  theme(legend.key.width = unit(4, "cm"))   
    ggsurv
  })
  output$survival_strain_condition_tab <- DT::renderDataTable({ 
    out <- kable_table_assemble(force_merge_trials = TRUE, data = values$data,xvars = c("Strain","Condition.1"),round_digits = 2,facilitate_names = TRUE)
    table_display_reactive(single_table = out$stat_tables$Combined)
  })
  output$survival_strain_condition_descr <- renderText({
    "The survival curve is depicted for each combination of strain and condition. The strains are displayed in different colors while the different conditions are referred to by a variety of dashed linetypes (the control is always represented as a solid line).
    The legend referring to both is provided at the top of the plot.
    "
  })
  # Condition and Strain
  output$survival_strain_condition_pval <- DT::renderDataTable({ 
    out <- kable_table_assemble(data = values$data,xvars = c("Strain","Condition.1"),round_digits = 2,facilitate_names = TRUE)
    pval <- simplify_pvals(df = out$pval_mats$Combined, modification = "cutoff")
    table_display_reactive(single_table = pval,plot_cols = "all")
  })
  
  
  
  # lifespan by Plate
  output$survival_by_plate <- renderPlot({
    xvars <- ifelse(length(unique(values$data$Condition.1)) > 1,"Condition.1",NA) %>% c(.,c("Strain","Plate.Name","Device","Trial")) %>% na.trim()
    fit <- ls_fit(data = values$data,yvar = 'Surv(Death,1-Censored)',xvars = xvars )
    col.pal <- make_col_palette(data = values$data,fit = fit,colname_to_color = "Strain", base_col = values$pal$strain.col)
    lty <- make_lty_palette(data = values$data, fit = fit, colname_to_set_lty = "Condition.1") # what line to dash
    ggsurv <- ggsurvplot(fit = fit,data = values$data ,conf.int = as.logical(input$show_ls_conf_int),legend = "top",title ="Strain & Condition",xlim = input$time_window,pval = FALSE,pval.method = TRUE,censor = as.logical(input$show_censored),censor.size = 12,risk.table = TRUE,tables.height = 0.4, xlab = "Time [days]",size = 2,linetype = lty,palette = col.pal)
    ggsurv$plot <- ggsurv$plot + theme(legend.position = "none")
    ggsurv$plot + facet_grid(Trial ~ Device)
  })
  
  output$survival_by_plate_tab <- DT::renderDataTable({ 
    xvars <- ifelse(length(unique(values$data$Condition.1)) > 1,"Condition.1",NA) %>% c(.,c("Strain","Plate.Name","Device","Trial")) %>% na.trim()
    out <- kable_table_assemble(data = values$data,xvars = xvars,round_digits = 2,facilitate_names = TRUE)
    table_display_reactive(single_table = out$stat_tables$Combined)
  })
  
  output$survival_by_plate_tab_descr <- renderText({
    "Unlike the previous lifespan curves, this analysis is not meant to be directly interpreted. The purpose of this graph is to summarize overall how consistent the individual measurements are.
    Ideally, the pattern of strain and or condition survival curves is the same across plates and also devices - this illustrates a robust lifespan effect.
    "
  })
  
  
  ############################################## Download & Export ######################################
  output$report <- downloadHandler(
    filename = "report.pdf",
    content = function(file){
      tempReport <- file.path(tempdir(),"report.Rmd") # Select report type: report.Rmd or report_nobyplate.Rmd
      file.copy("report.Rmd", tempReport,overwrite = TRUE) # save first to temp dir
      rmarkdown::render(tempReport,output_file = file) # set new environment (child) to isolate the markdown code
    }
  )
  
  # # kable table (mnake a proper function)
  # output$strain_condition_kable <- function() {
  #   xvars <- c("Strain","Condition.1")
  #   experiments <- unique(values$data$Experiment)
  #   round_digits <- 2
  #   
  #   # compute stats for all experiments
  #   stat_tables <- list()
  #   for (exp in experiments) {
  #     fit <-  ls_fit(data = values$data[values$data$Experiment == exp,],yvar = 'Surv(Death,1-Censored)',xvars = xvars)
  #     stat_table <- lifespan_summary_stats(data = values$data,fit = fit)
  #     Strata <- rownames(stat_table)
  #     stat_tables[[exp]] <- cbind(Strata,stat_table)
  #   }
  # 
  #   # bind the individual experiment stats to one dataframe (and remove the experiment column)
  #   df <- plyr::ldply(stat_tables, data.frame)
  #   data.table::setnames(x = df,old = c(".id","Strata","n.start","n.end","mean","median","X25th.percentile","X75th.percentile"),new = c("Experiment","Strata","n start", "n end","mean","median","25th per.", "75th per."))
  #   df <- df[,2:ncol(df)]
  #   df[,c("mean","median","25th per.", "75th per.")] <- apply(df[,c("mean","median","25th per.", "75th per.")] , 2, function(x) as.numeric(as.character(x)))
  #   df[,c("mean","median","25th per.", "75th per.")] <- round( df[,c("mean","median","25th per.", "75th per.")] ,round_digits)
  #   
  #   # How many rows are in each experiment
  #   num_rows <- map(1:length(names(stat_tables)), function(x){nrow(stat_tables[[x]]) }) %>% unlist()
  #   
  #   # show table
  #   df %>%
  #     kable("html", align = "c") %>%
  #     kable_styling("striped", full_width = F) %>%
  #     add_header_above(c(" ", "study size" = 2, "Population statistics" = 4)) %>%
  #     kable_styling(full_width = T) %>%
  #     group_rows(index = setNames(num_rows, names(stat_tables))) 
  #     
  # }
  
  output$strain_condition_kable_Strain <- function(){
                      out <- kable_table_assemble(data = values$data,xvars = c("Strain"),round_digits = 2)
                      # add footnote markers
                      # names(out$df)[c(2)] <- paste0(names(out$df)[2], footnote_marker_symbol(1))
                      # names(out$df)[c(5)] <- paste0(names(out$df)[5], footnote_marker_symbol(2))
                      df <- kable_table_plot_html(df = out$df, num_rows = out$num_rows, stat_tables = out$stat_tables)
                      df
  }
  
  output$strain_condition_kable_Strain_Cond <- function(){
                      out <- kable_table_assemble(data = values$data,xvars = c("Strain"),round_digits = 2)
                      saveRDS(reactiveValuesToList(values),"values.rds")
                      saveRDS(reactiveValuesToList(input),"input.rds")
                      df <- kable_table_plot_html(df = out$df, num_rows = out$num_rows, stat_tables = out$stat_tables)
                      df %>%
                        footnote(general = "This is the lifespan table. ")
  }

  
  
}

### APP
shinyApp(ui, server) 


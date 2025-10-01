library(shinyWidgets)
library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)
library(arrow)
library(janitor)
options(shiny.maxRequestSize = 300 * 1024^2)
library(bslib)
library(glue)

# Functions to Prepare Data and the UI
## Functions for Working With Data
# Getting String from Data-Frame
prepText <- function(x){
  x <- as.character(x)
  x <- str_replace_all(x, '\n', '<br>')
  return(x)}

getString <- function(data, id, template, keyword, pattern) {
  # Get the row as a named list
  row <- as.list(data[id, , drop = FALSE])
  # Apply glue template
  text <- glue_data(row, template)
  # Apply prepText for line breaks
  text <- prepText(text)
  # Keyword highlighting
  text <- str_replace_all(text, keyword, pattern)
  # Return as HTML string
  return(as.character(text))
}

getPalavraChave <- function(palavraChave){
  if(is.null(palavraChave) | palavraChave == '')
  {return(regex('(KAMDSJNAJSBDASJ)'))} else {
    palavraChave <- str_replace_all(palavraChave, '^[^\\d\\w-]+', '')
    palavraChave <- str_replace_all(palavraChave, '[^\\d\\w-]+$', '')
    palavraChave <- str_split(palavraChave, '[^\\d\\w-]+')[[1]]
    palavraChave <- str_c(palavraChave, collapse = '|')
    palavraChave <- regex(paste0('(', palavraChave, ')'), ignore_case = TRUE)
    return(palavraChave)}}

## Variables for Application Use
make_ui <- function(x, var){
  if(is.numeric(x)){
    x <- as.character(x)
    levs = c(levels(factor(x)), NA)
    selectizeInput(var, var, choices = levs, 
                   selected = NULL, multiple = FALSE, options = list(maxOptions = 10))
  } else if (is.character(x)){
    levs = c(levels(factor(x)), NA)
    selectizeInput(var, var, choices = levs, 
                   selected = NULL, multiple = FALSE, options = list(maxOptions = 10))
  } else {
    NULL
  }
}

make_edit_ui <- function(x, var, current_value){
  n_levels <- length(unique(x))
  
  if(inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")){
    valid_date <- suppressWarnings(as.Date(current_value))
    if (is.na(valid_date)) valid_date <- Sys.Date()
    dateInput(paste0("edit_", var), var, value = valid_date, width = '100%')
    
  } else if(is.numeric(x)){
    valid_value <- suppressWarnings(as.numeric(current_value))
    numericInput(paste0("edit_", var), var, value = valid_value, width = '100%')
    
  } else if(is.factor(x) && n_levels <= 10){
    levs <- levels(x)
    selectizeInput(
      paste0("edit_", var), var, choices = levs,
      selected = current_value, multiple = FALSE,
      options = list(maxOptions = 10, create = TRUE), width = '100%'
    )
  } else if(is.character(x)){
    char_levels <- unique(x[!is.na(x)])
    if(length(char_levels) <= 10){
      selectizeInput(
        paste0("edit_", var), var, choices = char_levels,
        selected = current_value, multiple = FALSE,
        options = list(maxOptions = 10, create = TRUE), width = '100%'
      )
    } else {
      textInput(paste0("edit_", var), var, value = current_value, width = '100%')
    }
    
    # Fallback for other types
  } else {
    textInput(paste0("edit_", var), var, value = as.character(current_value), width = '100%')
  }
}

filter_var <- function(x, val){
  if(is.numeric(x)){
    x <- as.character(x)
    val <- as.character(val)
    if('NA' %in% val){val <- c(val, NA)}
    x %in% val
  } else if(is.character(x)){
    if('NA' %in% val){val <- c(val, NA)}
    x %in% val
  } else {
    TRUE
  }
}

sidePanelCard <- sidebar(width = '350px',
  
  span('✦ Input Panel', style = 'font-size: 18px'),
  span('Basic Input', style = 'font-size: 16px'),


  fileInput('file', 'File Input', 
              accept = c('.csv', '.tsv', '.xlsx', '.xls', '.parquet')),
  
  selectizeInput('filter1', 'Filter by', choices = NULL, selected = 'Sem Filtro'),
  
  uiOutput('filter11'),
  
  selectizeInput('arrange', 'Arrange by', choices = NULL,
                 selected = NULL, multiple = TRUE, width = '100%'),
  
  hr(style = "border-top: 1px solid #000000;"),
  
  span('Advanced Text Filtering', style = 'font-size: 16px'),
  textInput('text_to_filter', 'Text Filter', value = '', width = '100%'),
  p('This only works when there is no filter selected in the basic input panel.', style = 'font-size: 0.8rem; margin-top: 0px'),
  
  hr(style = "border-top: 1px solid #000000;"),
  
  span('Export Options', style = 'font-size: 16px'),
  selectInput("export_ext", "File Type", choices = c("Excel (.xlsx)" = ".xlsx", "CSV (.csv)" = ".csv", "TSV (.tsv)" = ".tsv", "Parquet (.parquet)" = ".parquet"), selected = ".xlsx"),
  downloadButton("export_data", "Export Dataset", class = "btn-success")
  
)

mainPanelCard <- div(class = 'main-panel',
    
     accordion(
       width = '100%',
       open = FALSE,
       accordion_panel(
         "Keywords Highlighting",
         textInput('palavraChave', 'Keywords', value = NULL, width = '100%'),
         htmlOutput('nFound')
       )),               
     
   accordion(
     width = '100%',
     open = FALSE,
     accordion_panel(
       "Data Template",
       textAreaInput('dataTemplate', 'Template', value = NULL, width = '100%', rows = 5),
       p('Paste the template to review the data below.', style = 'font-size: 0.8rem; margin-top: 0px')
     )), 
   

   
   div(class = 'nav-button-container',
       
       actionButton('btprevious100', '', icon=icon('backward'), style="text-align: center; background-color: #95a5a6; border: none", class = "btn-info btn-nav"),
       actionButton('btprevious1', '', icon=icon('arrow-left'), style="text-align: center; background-color: #95a5a6; border: none", class = "btn-info btn-nav"),
       div(class='n-current', htmlOutput('nCurrent')),
       actionButton('btnext1', '', icon=icon('arrow-right'), style="text-align: center; border: none", class = "btn-primary btn-nav"),
       actionButton('btnext100', '', icon=icon('forward'), style="text-align: center; border: none", class = "btn-primary btn-nav")
       
   ),
   
  div(style = 'width: 100%',
    card(
      card_header('Data Report'),
      htmlOutput('templateDisplay', width='100%')
    )
  )
)

navSetCardUnderline <- navset_card_underline(
  title = "Data Viewer",
  nav_panel("View",
      div(style = "margin-bottom: 10px;",
          selectizeInput('view_columns', 'Variables', 
                         choices = NULL, selected = NULL, multiple = TRUE, 
                         width = '100%',
                         options = list(placeholder = 'Select variables to view'))
      ),
      tableOutput('view_table')
            ),
  nav_panel("Edit",
            uiOutput('edit_inputs')
  ),
  nav_panel("Dictionary",
        card_body(
            tableOutput("dictionary_table")
        )
    )
)

# Define UI for application that draws a histogram
ui <- page_sidebar(
  # Loading Necessary Libraries
  shinyjs::useShinyjs(),
  sidebar_width = 600,
  # Loading Styles
  tags$script(src = "script.js"),
  tags$link(href = "style.css", rel = "stylesheet", type = "text/css"),
  
  title = tagList(
    div(icon("check-circle", class = "text-success", style = "margin-right: 8px;"), "Data Validation UI")
  ),
  sidebar = sidePanelCard,
  fillable = FALSE,
  
  theme = bs_theme(
    bootswatch = "flatly",
    base_font = font_google("Inter"),
    navbar_bg = "#2c3e50",
    version = 5
  ),
  
  layout_columns(
    col_widths = c(7, 5),
    card(card_header('Main Panel'),
         mainPanelCard),
    navSetCardUnderline
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  data_rv <- reactiveValues(data = NULL)
  
  observeEvent(input$file, {
    ext <- tools::file_ext(input$file$name)
    raw_data <- switch(ext,
                       csv = read.table(input$file$datapath, sep = ';'),
                       tsv = read.table(input$file$datapath, sep = '\t'),
                       xlsx = read_excel(input$file$datapath),
                       xls = read_excel(input$file$datapath),
                       parquet = read_parquet(input$file$datapath),
                       validate('Invalid file; Please upload a .csv or .tsv or Excel file'))
    raw_data$row_id <- 1:nrow(raw_data)
    data_rv$data <- raw_data
  })
  
  data <- reactive({
    req(data_rv$data)
    data_rv$data
  })
  
  ## Filtering the Data
  df <- reactive(
    if(!(is.null(input$arrange))){
      data() %>% filter(selected()) %>% arrange(across(all_of(input$arrange)))
    } else {
      data() %>% filter(selected())
    })
  
  selected <- reactive({
    if(input$filter1 %in% colnames(data())){filter_var(data()[[input$filter1]], input[[input$filter1]])}
    else if(input$text_to_filter != ''){
      grepl(regex(input$text_to_filter), ignore.case = T,
            data() %>% mutate_all(as.character) %>% unite('all', all_of(colnames(data())), sep = ' ') %>% pull('all'))}
    else{rep(TRUE, nrow(data()))}
  })
  
  # Updating the Order Inputs
  reactive_vals <- reactiveValues()
  n <- reactiveVal(1)
  
  observeEvent(input$btnext1, {n(min(n() + 1, nrow(df())))})
  observeEvent(input$btnext100, {n(min(n() + 100, nrow(df())))})
  observeEvent(input$btprevious1, {n(max(n() - 1, 1))})
  observeEvent(input$btprevious100, {n(max(n() - 100, 1))})
  
  observeEvent(input$arrange, { n(1) })
  observeEvent(input$filter1, { n(1) })
  
  # Creating Intermediate Variables
  reactive_vals$palavraChave <- reactive({getPalavraChave(input$palavraChave)})
  reactive_vals$text <- reactive(getString(df(), n(), input$dataTemplate, reactive_vals$palavraChave(), reactive_vals$chavePattern()))
  reactive_vals$chavePattern <- reactive('<span class="highlighted-text">\\1</span>')
  reactive_vals$nFound <- reactive({sum(str_count(reactive_vals$text(), 'highlighted-text'))})
  
  # Creating Outputs
  observeEvent(input$file, {
    updateSelectizeInput(inputId = 'view_columns', choices = setdiff(names(data()), "row_id"), selected = setdiff(names(data()), "row_id"), server = TRUE)
    updateSelectizeInput(inputId = 'filter1', choices = c('No Filters', colnames(data())), server = TRUE)
    updateSelectizeInput(inputId = 'arrange', choices = colnames(data()), server = TRUE)
    updateTextAreaInput(
      inputId = 'dataTemplate',
      value = paste0("#", setdiff(names(data()), "row_id"), ": {`", setdiff(names(data()), "row_id"), "`}", collapse = "\n")
    )
    output$filter11 <- renderUI(make_ui(data()[[input$filter1]], input$filter1))
    })
  output$templateDisplay <- renderText({HTML(reactive_vals$text())})
  output$nFound <- renderText({
    HTML(paste0('<span class="found-words">Number of words found: ', 
                '<b> ', reactive_vals$nFound(),'</b></span>'))})
  output$nCurrent <- renderText(
    HTML(paste0('<b style="font-size: 30px; text-align: center;">', 
                as.character(n()), ' / ', as.character(nrow(df())), '</b></span>')))
  
  # Creating View Panel
  output$view_table <- renderTable({
    req(input$view_columns)
    cols <- setdiff(input$view_columns, "row_id")
    current_row <- df()[n(), cols, drop = FALSE]
    data.frame(
      Variable = cols,
      Value = as.character(t(current_row))
    )
  }, rownames = FALSE, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # Creating Edit Panel
  output$edit_inputs <- renderUI({
    req(input$view_columns)
    req(nrow(df()) > 0)
    
    current_row <- df()[n(), input$view_columns, drop = FALSE]
    
    edit_ui_list <- lapply(setdiff(input$view_columns, "row_id"), function(col_name) {
      col_value <- current_row[[col_name]]
      col_data <- data()[[col_name]]
      make_edit_ui(col_data, col_name, col_value) 
    })
    
    div(
      style = "padding: 15px;",
      edit_ui_list,
      hr(),
      actionButton('save_edit', 'Save Changes', class = 'btn-primary', icon = icon('save'))
    )
  })
  
  # Creating Dictionary Nav Panel
  output$dictionary_table <- renderTable({
    req(data())
    
    dict <- data.frame(
      Variable = setdiff(names(data()), "row_id"),
      Type = sapply(data()[setdiff(names(data()), "row_id")], function(x) class(x)[1]),
      N_Unique = sapply(data()[setdiff(names(data()), "row_id")], function(x) length(unique(x))),
      N_Missing = sapply(data()[setdiff(names(data()), "row_id")], function(x) sum(is.na(x))),
      Example = sapply(data()[setdiff(names(data()), "row_id")], function(x) as.character(head(x, 1)))
    )
    
    dict
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  observeEvent(input$save_edit, {
    req(nrow(df()) > 0)
    current_row_id <- df()[n(), "row_id", drop = TRUE]
    original_row_index <- which(data_rv$data$row_id == current_row_id)
    str(df())
    str(data_rv$data)
    print(current_row_id)
    
    for(col_name in input$view_columns) {
      if(col_name != "row_id") {
        new_value <- input[[paste0("edit_", col_name)]]
        if(inherits(data_rv$data[[col_name]], "Date") || inherits(data_rv$data[[col_name]], "POSIXct")){
          new_value <- as.Date(new_value)
        } else if(is.numeric(data_rv$data[[col_name]])){
          new_value <- as.numeric(new_value)
        } else if(new_value == ""){
          new_value <- NA
        }
        data_rv$data[[col_name]][original_row_index] <- new_value
      }
    }
    showNotification("Changes saved successfully!", type = "message", duration = 3)
  })
  
  output$export_data <- downloadHandler(
    filename = function() {
      original <- if (!is.null(input$file)) tools::file_path_sans_ext(input$file$name) else "dataset"
      dt <- format(Sys.time(), "%Y%m%d_%H%M%S")
      ext <- if (!is.null(input$export_ext)) input$export_ext else ".xlsx"
      original <- sub("_[0-9]{8}_[0-9]{6}$", "", original)
      paste0(original, "_", dt, ext)
    },
    content = function(file) {
      rio::export(data_rv$data[, setdiff(names(data_rv$data), "row_id"), drop = FALSE], file)
    }
  )

  
}

# Run the application 
shinyApp(ui = ui, server = server)

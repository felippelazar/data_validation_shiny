library(shinyWidgets)
library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)
library(arrow)
library(janitor)
library(bslib)
library(glue)
options(shiny.maxRequestSize = 300 * 1024^2)

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


getPalavraChave <- function(palavraChave) {
  if (is.null(palavraChave) || palavraChave == "" || nchar(palavraChave) == 1) {
    return(regex("(KAMDSJNAJSBDASJ)"))
  }
  terms <- str_extract_all(palavraChave, '"[^"]+"|\\S+')[[1]]
  terms <- str_replace_all(terms, '^"|"$', "")
  regex <- regex(paste0("(", paste(terms, collapse = "|"), ")"), ignore_case = TRUE)
  return(regex)
}

## Variables for Application Use
## Variables for Application Use
make_ui <- function(x, var){
  if(is.numeric(x)){
    uniq <- sort(unique(x[!is.na(x)]))
    # If all differences between values are 1
    if(length(uniq) > 1 && all(diff(uniq) == 1)) {
      numericInput(
        var, var,
        value = min(uniq), # selected value is min
        min = min(uniq),
        max = max(uniq),
        step = 1
      )
    } else {
      x_chr <- as.character(x)
      levs <- levels(factor(x_chr))
      choices <- setNames(levs, levs)
      # Add "Missing" label only if there are missing values
      if(any(is.na(x))){
        choices <- c(choices, "Missing" = "NA")
      }
      selectizeInput(var, var, choices = choices, 
                     selected = NULL, multiple = FALSE, options = list(maxOptions = 15))
    }
  } else if (is.character(x)){
    levs <- levels(factor(x))
    choices <- setNames(levs, levs)
    # Add "Missing" label only if there are missing values
    if(any(is.na(x))){
      choices <- c(choices, "Missing" = "NA")
    }
    selectizeInput(var, var, choices = choices, 
                   selected = NULL, multiple = FALSE, options = list(maxOptions = 15))
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
    
  } else if(is.factor(x) && n_levels <= 20){
    levs <- levels(x)
    selectizeInput(
      paste0("edit_", var), var, choices = levs,
      selected = current_value, multiple = FALSE,
      options = list(maxOptions = 20, create = TRUE), width = '100%'
    )
  } else if(is.character(x)){
    char_levels <- unique(x[!is.na(x)])
    if(length(char_levels) <= 20){
      selectizeInput(
        paste0("edit_", var), var, choices = char_levels,
        selected = current_value, multiple = FALSE,
        options = list(maxOptions = 20, create = TRUE), width = '100%'
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

  fileInput('file', 'File Input', 
              accept = c('.csv', '.tsv', '.xlsx', '.xls', '.parquet')),
  
  uiOutput("progressUI"),
  
  span('Basic Filtering', style = 'font-size: 16px'),
  
  div(
    class='d-flex flex-column gap-1',
    div(
      selectizeInput('filter1', 'Filter by', choices = NULL, selected = 'Sem Filtro'),
      uiOutput('filter11')
    ),
    
    div(style = 'display:none',
        selectizeInput('filter2', 'Filter by', choices = NULL, selected = 'Sem Filtro'),
        uiOutput('filter22'),
    ),
    
    selectizeInput('arrange', 'Arrange by', choices = NULL,
                   selected = NULL, multiple = TRUE, width = '100%'),
  ),

  
  hr(style = "border-top: 1px solid #000000; margin: 2px 0px"),
  
  span('Advanced Configurations', style = 'font-size: 16px'),
  
  bslib::input_switch(id = "edit_all_slice", label = "Edit all Sliced (Filtered) Data", value = FALSE),
  textInput('text_to_filter', 'Text Filter', value = '', width = '100%'),
  
  hr(style = "border-top: 1px solid #000000; margin: 2px 0px"),
  
  span('Export Options', style = 'font-size: 16px'),
  selectInput("export_ext", "File Type", choices = c("Excel (.xlsx)" = ".xlsx", "CSV (.csv)" = ".csv", "TSV (.tsv)" = ".tsv", "Parquet (.parquet)" = ".parquet"), selected = ".xlsx"),
  downloadButton("export_data", "Export Dataset", class = "btn-success")
  
)

configAccordion <- accordion(
  id = "main",
  width = "100%",
  open = FALSE,
  
  accordion_panel(
    "Configurations",
    
    # Nested accordion with 2 panels
    accordion(
      id = "config_nested",
      width = "100%",
      open = TRUE,
      
      accordion_panel(
        "Keywords Highlighting",
        textInput("palavraChave", "Keywords", value = NULL, width = "100%"),
        htmlOutput("nFound")
      ),
      
      accordion_panel(
        "Data Template",
        textAreaInput(
          "dataTemplate",
          "Template",
          value = NULL,
          width = "100%",
          rows = 7
        ),
        p("Paste the template to review the data below.",
          style = "font-size: 0.8rem; margin-top: 0px")
      )
    )
  )
)

mainPanelCard <- div(class = 'main-panel',
    
   configAccordion,
   
   div(class = 'nav-button-container',
       
       div(
         actionButton('btprevious100', '', icon = icon('backward'), style = "text-align: center;", class = "btn-nav btn-light", type = 'button', `data-bs-toggle` = "tooltip", `data-bs-placement` = "left", title = "Back 100"),
         actionButton('btprevious10', '', icon = icon('angle-double-left'), style = "text-align: center;", class = "btn-nav btn-light", `data-bs-toggle` = "tooltip", `data-bs-placement` = "left", title = "Back 10"),
         actionButton('btprevious1', '', icon = icon('arrow-left'), style = "text-align: center;", class = "btn-nav btn-light", `data-bs-toggle` = "tooltip", `data-bs-placement` = "left", title = "Back 1"),
       ),
       
       div(class = 'n-current', htmlOutput('nCurrent')),
       
       div(
         actionButton('btnext1', '', icon = icon('arrow-right'), style = "text-align: center;", class = "btn-nav btn-light", `data-bs-toggle` = "tooltip", `data-bs-placement` = "right", title = "Next 1"),
         actionButton('btnext10', '', icon = icon('angle-double-right'), style = "text-align: center;", class = "btn-nav btn-light", `data-bs-toggle` = "tooltip", `data-bs-placement` = "right", title = "Next 10"),
         actionButton('btnext100', '', icon = icon('forward'), style = "text-align: center;", class = "btn-nav btn-light", `data-bs-toggle` = "tooltip", `data-bs-placement` = "right", title = "Next 100")
       ),
       
   ),
   
  div(style = 'width: 100%',
    card(
      style = "max-height: 65vh; overflow-y: auto;", 
      card_header('Data Report'),
      htmlOutput('templateDisplay', width='100%')
    )
  )
)

navSetCardUnderline <- navset_card_underline(
  title = "Data Viewer",
  nav_panel("View",
            style = "max-height: 82vh; overflow-y: auto;",
      card_body(
        div(style = "margin-bottom: 10px;",
            selectizeInput('view_columns', 'Variables', 
                           choices = NULL, selected = NULL, multiple = TRUE, 
                           width = '100%',
                           options = list(placeholder = 'Select variables to view'))
        ),
        tableOutput('view_table')
      ),
            ),
  nav_panel("Edit",
            style = "max-height: 82vh; overflow-y: auto;",
            uiOutput('edit_inputs')
  ),
  nav_panel("Dictionary",
            style = "max-height: 82vh; overflow-y: auto;",
        card_body(
            tableOutput("dictionary_table")
        )
    )
)

# Define UI for application that draws a histogram
ui <- page_sidebar(
  # Loading Necessary Libraries
  shinyjs::useShinyjs(),
  sidebar_width = 650,
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
  # bslib::bs_themer()
  data_rv <- reactiveValues(data = NULL)
  file_raw <- reactiveVal(1)
  
  n_rows <- reactiveVal(1)
  n_validated <- reactiveVal(1)
  
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
    
    validation_levels <- c("validated", "unvalidated")
    if(!'data_validated' %in% names(raw_data)) raw_data$data_validated <- 'unvalidated'
    if(!all(unique(raw_data$data_validated) %in% validation_levels)) raw_data$data_validated <- 'unvalidated'
    raw_data <- dplyr::relocate(raw_data, 'data_validated', .before = 1)
    
    # Getting the Number of Rows for Progress Bar
    n_rows(nrow(raw_data))
    n_validated(sum(raw_data$data_validated == 'validated'))
    
    data_rv$data <- raw_data
    file_raw(raw_data)
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
  
  last_selected <- reactiveVal(NULL)
  
  text_filter_selected <- reactive({
    dat <- data()
    nrows <- nrow(dat)
    sanitized <- gsub("[^A-Za-z0-9| ]", "", input$text_to_filter)
    if(sanitized != "" && nchar(sanitized) >= 3) {
      sapply(1:nrows, function(i) {
        grepl(
          sanitized,
          paste(dat[i, setdiff(colnames(dat), "row_id")], collapse = " "),
          ignore.case = TRUE
        )
      })
    } else {
      rep(TRUE, nrows)
    }
  })
  
  selected <- reactive({
    dat <- data()
    nrows <- nrow(dat)
    
    sel1 <- if(input$filter1 %in% colnames(dat) && input$filter1 != 'No Filters') {
      filter_var(dat[[input$filter1]], input[[input$filter1]])
    } else {
      rep(TRUE, nrows)
    }
    
    sel2 <- if(input$filter2 %in% colnames(dat) && input$filter2 != 'No Filters') {
      filter_var(dat[[input$filter2]], input[[input$filter2]])
    } else {
      rep(TRUE, nrows)
    }
    
    if(sum((sel1 & sel2 & text_filter_selected()) != last_selected()) >= 2) n(1)
    
    last_selected(sel1 & sel2 & text_filter_selected())
    sel1 & sel2 & text_filter_selected()
    
  })
  
  # Updating the Order Inputs
  reactive_vals <- reactiveValues()
  n <- reactiveVal(1)
  
  observeEvent(list(input$filter1, input$filter2, input$filter2, input$filter2), {
    n(1)
  })
  
  observeEvent(input$btnext1, {n(min(n() + 1, nrow(df())))})
  observeEvent(input$btnext10, {n(min(n() + 10, nrow(df())))})
  observeEvent(input$btnext100, {n(min(n() + 100, nrow(df())))})
  observeEvent(input$btprevious1, {n(max(n() - 1, 1))})
  observeEvent(input$btprevious10, {n(max(n() - 10, 1))})
  observeEvent(input$btprevious100, {n(max(n() - 100, 1))})
  
  # Creating Intermediate Variables
  reactive_vals$palavraChave <- reactive({getPalavraChave(input$palavraChave)})
  reactive_vals$text <- reactive(getString(df(), n(), input$dataTemplate, reactive_vals$palavraChave(), reactive_vals$chavePattern()))
  reactive_vals$chavePattern <- reactive('<span class="highlighted-text">\\1</span>')
  reactive_vals$nFound <- reactive({sum(str_count(reactive_vals$text(), 'highlighted-text'))})
  
  
  # Creating Outputs
  observeEvent(input$file, {
    updateSelectizeInput(inputId = 'view_columns', choices = setdiff(names(file_raw()), "row_id"), selected = head(setdiff(names(file_raw()), "row_id"), 10), server = TRUE)

    updateSelectizeInput(inputId = 'arrange', choices = colnames(file_raw()), server = TRUE)
    updateTextAreaInput(
      inputId = 'dataTemplate',
      value = paste0("#", setdiff(names(file_raw()), "row_id"), ": {`", setdiff(names(file_raw()), "row_id"), "`}", collapse = "\n")
    )
    
    updateSelectizeInput(inputId = 'filter1', choices = c('No Filters', setdiff(names(file_raw()), c("row_id"))), selected = input$filter1, server = TRUE)
    updateSelectizeInput(inputId = 'filter2', choices = c('No Filters', setdiff(names(file_raw()), c("row_id"))), selected = input$filter2, server = TRUE)
    
    })
  
  observe({
    req(input$file)
    if(input$filter1 == 'data_validated') output$filter11 <- renderUI(selectizeInput('data_validated', 'data_validated', choices = c('validated', 'unvalidated'), selected = NULL)) else output$filter11 <- renderUI(make_ui(file_raw()[[input$filter1]], input$filter1))
    if(input$filter2 == 'data_validated') output$filter22 <- renderUI(selectizeInput('data_validated', 'data_validated', choices = c('validated', 'unvalidated'), selected = NULL)) else output$filter22 <- renderUI(make_ui(file_raw()[[input$filter2]], input$filter2))
  })
  
  
  output$templateDisplay <- renderText({HTML(reactive_vals$text())})
  output$nFound <- renderText({
    HTML(paste0('<span class="found-words">Number of words found: ', 
                '<b> ', reactive_vals$nFound(),'</b></span>'))})
  
  observe({
    if(n() > nrow(df())) n(1)
    if(nrow(df()) == 0) {
      output$nCurrent <- renderText(HTML(paste0('<b style="font-size: 30px; text-align: center;"> 0/0 </b></span>')))
    } else {
      output$nCurrent <- renderText(
        HTML(paste0('<b style="font-size: 30px; text-align: center;">  ', 
                    as.character(n()), ' / ', as.character(nrow(df())), '  </b></span>')))
    }
  })
  
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

    current_row <- df()[n(), c(input$view_columns, 'data_validated'), drop = FALSE]

    validated_value <- current_row[['data_validated']] == 'validated'
    
    edit_ui_list <- lapply(setdiff(input$view_columns, c("row_id", 'data_validated')), function(col_name) {
      col_value <- current_row[[col_name]]
      col_data <- data()[[col_name]]
      make_edit_ui(col_data, col_name, col_value)
    })
    
    # Create vector of input IDs
    edit_input_ids <- paste0("edit_", setdiff(input$view_columns, c("row_id", "data_validated")))
    
    # Save to a reactive value for use outside renderUI
    # You might want to use a reactiveVal or reactiveValues for this
    rv_edit$edit_input_ids <- edit_input_ids
    rv_edit$n_changes <- 0
    
    div(
      style = "padding: 15px;",
      actionButton('save_edit', 'Save Changes', class = 'btn-primary', icon = icon('save')),
      hr(),
      bslib::input_switch(id = "data_validated_input", label = "Data Validated?", value = validated_value),
      edit_ui_list
    )
  })
  
  # ReactiveValues to store input IDs
  rv_edit <- reactiveValues(edit_input_ids = NULL, n_changes = NULL)
  
  # Observe changes to any edit input
  observeEvent(
    lapply(rv_edit$edit_input_ids, function(id) input[[id]]),
    {
      if(rv_edit$n_changes <= 1) {
        # First event after creation: just clear the flag, don't update switch
        rv_edit$n_changes <- rv_edit$n_changes + 1
      } else {
        # All subsequent changes: update switch
        update_switch(id = "data_validated_input", value = TRUE)
      }
    },
    ignoreInit = TRUE
  )
  
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
    
    for(col_name in setdiff(input$view_columns, 'data_validated')) {
      if(col_name != "row_id") {
        new_value <- input[[paste0("edit_", col_name)]]
        if(inherits(data_rv$data[[col_name]], "Date") || inherits(data_rv$data[[col_name]], "POSIXct")){
          new_value <- as.Date(new_value)
        } else if(is.numeric(data_rv$data[[col_name]])){
          new_value <- as.numeric(new_value)
        } else if(new_value == ""){
          new_value <- NA
        }
        if(input$edit_all_slice) data_rv$data[[col_name]][selected()] <- new_value else data_rv$data[[col_name]][original_row_index] <- new_value
      }
    }
    
    if(input$edit_all_slice) data_rv$data[['data_validated']][selected()] <- ifelse(input[['data_validated_input']], 'validated', 'unvalidated') else data_rv$data[['data_validated']][original_row_index] <- ifelse(input[['data_validated_input']], 'validated', 'unvalidated')
    n_validated(sum(data_rv$data[['data_validated']] == 'validated'))
    
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

  output$progressUI <- renderUI({
    req(nrow(df()) > 0)
    
    pct <- round(100 * n_validated() / n_rows())
    
    div(class = 'd-flex flex-column gap-0',
        p('Validation Progress Bar'),
        div(
          class = "progress",
          style = "height: 25px; position: relative;",
          
          # Progress bar
          div(
            class = "progress-bar bg-success",
            role = "progressbar",
            style = sprintf("width: %d%%;", pct)
          ),
          
          # Centered text overlay
          div(
            style = "
            position: absolute;
            top: 0; left: 0;
            width: 100%; height: 100%;
            display: flex;
            align-items: center;
            justify-content: center;
            color: black;
            font-weight: 500;
          ",
            sprintf("%d / %d (%d%% complete)", n_validated(), n_rows(), pct)
          )
        )
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

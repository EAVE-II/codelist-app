library(shiny)
library(data.table)
library(readxl)
library(dplyr)
library(tidyr)
library(DT)
library(shinythemes)
library(httr)
library(writexl)

########################## PREPARE DATA #####################################

# Read from github
url <- 'https://github.com/EAVE-II/read-code-app/raw/master/data/Adverse%20events%20-%20study%20design%20and%20codes_vB.xlsx'

GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
excel <- read_excel(tf, 2L)

# Read from local directory
#df <-read_excel("Adverse events - study design and codes_vB.xlsx", sheet = 'Codes')

df <- excel[-7]

# cols1 is names of column in the main dataframe
# cols1View is how the column names will appear in the web app
cols1 <- c('Category', 'AE', 'viewCodes')
cols1View <- c('Category', 'Adverse event', '')

#rename columns
df <- rename(df,  'AE' = 'Adverse event',
             'readCodes' = 'Read codes',
             'icd10Codes' = 'ICD-10 codes',
             'readCodesSource' =  'Source: Read codes',
             'icd10CodesSource' = 'Source: ICD-10 code')

df <- fill(df, c('Category', 'AE', 'readCodesSource', 'icd10CodesSource'), .direction = 'down')

df <- group_by(df, AE) %>% mutate( readCodes = list(readCodes),
                                   icd10Codes = list(icd10Codes)) %>% unique()

df <- as.data.frame(df)

df <- df[order(df$AE),] 

rownames(df) <- 1:nrow(df)


# # Create ID_list. This only needs to be done once initially; after that it self updates
# ID_list <- df[c('AE')]
# 
# ID_list['ID'] <- rownames(ID_list)
# 
# write.csv(ID_list, '../data/ID_list.csv', row.names = FALSE)

#Update ID_list
ID_list <- read.csv('../data/ID_list.csv')

ID_list <- left_join(df['AE'], ID_list)

ID_list['ID'] <- 1:nrow(new)

# Merge ID's with df
df <- left_join(df, ID_list)

# This makes a series of action buttons
shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}

################################# UI ####################################

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel('Codes'),
  uiOutput("header"),
  uiOutput('table1'),
  uiOutput('table2'),
  uiOutput('buttons')
)

############################## SERVER #####################################

server <- function(input, output) {
  
  # Create main table    
  output$data <- renderDataTable({
    
    data <- mutate(df, viewCodes =
                     shinyInput(actionButton, nrow(df), 'button_', label = "View", class = "btn-primary",
                                onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )) %>% select(cols1)
    
    
    table <- datatable(data[cols1], escape = FALSE, selection = 'none',
                       colnames = cols1View,  filter = 'top',
                       options = list(columnDefs = list( list(targets = 3, searchable = FALSE),
                                                         list(width = '80px', targets = 3),
                                                         list(width = '2000px', targets =c(1,2)))))
    
    table
  })
  
  # Render main table in UI
  output$table1 <- renderUI({DT::dataTableOutput("data")})
  
  # This creates a download link button
  output$downloadAll <- downloadHandler(
    filename = function() {paste( 'Adverse_events_codes_', Sys.Date(), '.csv', sep='')},
    content = function(file) { file.copy('../data/Adverse events - study design and codes_vB.xlsx') }
  )
  
  # Render back and download buttons to UI
  output$buttons <- renderUI({
    tagList( actionButton("return", "Back to all codes", class = "btn-primary"),
             downloadButton("downloadAll", "Download excel file", class = "btn-primary"))
  })
  
  
  
  
  
  # This is what happens when you press one of the "View" action buttons in
  # the main table
  observeEvent(input$select_button, {
    
    # This is the row of the action button that was clicked
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    
    # Create data frames with all codes, and read/ICD-10 codes separately
    allCodes <- data.frame(readCodes = unlist(df[selectedRow, 'readCodes']),
                           icd10Codes = unlist(df[selectedRow, 'icd10Codes']))
    
    allCodes[] <- lapply(allCodes, as.character)
    
    readCodes <- na.omit(allCodes['readCodes'])
    
    icd10Codes <- na.omit(allCodes['icd10Codes'])
    
    # Replace NA with space, for download
    allCodes[ is.na(allCodes)] <- ''
    
    # Category ad adverse event names
    Cat <- df[selectedRow, 'Category']
    AE <- df[selectedRow, 'AE']
    
    # Render header to UI
    output$header <- renderUI({
      tagList(tags$h4( paste("Category:", Cat, sep=" ")),
              tags$h4("Adverse Event:", AE, sep=" "))
    })
    
    
    # Create the read code and icd-10 code tables
    output$readCodes= DT::renderDataTable(
      readCodes, colnames = 'Read Codes',
      server = FALSE)
    
    output$icd10Codes= DT::renderDataTable(
      icd10Codes, colnames = 'ICD-10 codes',
      server = FALSE)
    
    # Render read code and icd-10 code tables to UI
    output$table1 <- renderUI({
      div(style="padding: 30px 0px", DT::dataTableOutput("icd10Codes"))
    })
    
    output$table2 <- renderUI({
      div(style="padding: 30px 0px", DT::dataTableOutput("readCodes"))})
    
    
    # This creates a download link button
    output$download <- downloadHandler(
      filename = function() {paste( AE, "_codes_", Sys.Date(), '.csv', sep='')},
      content = function(file) {write.csv(allCodes, file)}
    )
    
    # Render back and download buttons to UI
    output$buttons <- renderUI({
      tagList( actionButton("return", "Back to all codes", class = "btn-primary"),
               downloadButton("download", "Download CSV", class = "btn-primary"))
    })
    
  })
  
  # This is what happens when you click the "Back to all codes" button.  
  observeEvent(input$return, {
    
    # Get rid of header
    output$header <- renderUI({ })
    
    # Render main table in UI
    output$table1 <- renderUI({DT::dataTableOutput("data")})
    
    # Get rid of table2
    output$table2 <- renderUI({})
    
    # Get rid of buttons
    output$buttons <- renderUI({})
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


###################################################################### 

## Code author: Steven Kerr

## Description: This is the code for a read/ICD-10 code browsing app.
# It is currently hosted at https://argoshare.is.ed.ac.uk/content/537/
###################################################################### 

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

# Read from github. Better this way in case you forget to pull.
url <- 'https://github.com/EAVE-II/read-code-app/raw/master/code/Adverse%20event%20codes_v1.xlsx'

GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
excel <- read_excel(tf, 3L)

#rename columns
excel <- rename(excel,  'AE' = 'Adverse event',
             'read_codes' = 'Read codes',
             'read_code_desc' = 'Read code description',
             'icd10_codes' = 'ICD-10 codes',
             'icd10_code_desc' = 'ICD-10 code description',
             'read_code_source' =  'Source: Read codes',
             'icd10_code_source' = 'Source: ICD-10 code',
             'owner' = 'Owner',
             'date' = 'Date modified')

excel$date <- as.Date(excel$date)

excel <- fill(excel, c('Category', 'AE', 'Projects', 'read_code_source', 'icd10_code_source', 'owner', 'date'), .direction = 'down')

# df will be used to fill the main table. excel will be used to populate sub-tables.
df <- select(excel, c('AE', 'Category', 'Projects', 'owner', 'date')) %>% unique()

df <- as.data.frame(df)

df <- df[order(df$AE),] 

rownames(df) <- 1:nrow(df)

#Create ID_list. This only needs to be done once initially; after that it self updates
# ID_list <- df[c('AE')]
# 
# ID_list['ID'] <- rownames(ID_list)
# 
# write.csv(ID_list, './ID_list.csv', row.names = FALSE)

#Update ID_list
ID_list <- read.csv('./ID_list.csv')

ID_list <- left_join(df['AE'], ID_list)

ID_list['ID'] <- 1:nrow(ID_list)

write.csv(ID_list, './ID_list.csv', row.names = FALSE)

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

# cols is names of column in the main dataframe
# colsView is the column name that will appear in the app
cols <- c('AE', 'Category', 'Projects', 'owner', 'ID', 'date', 'viewCodes')
cols_view <- c('Adverse event', 'Category', 'Projects', 'Owner', 'ID', 'Last modified', '')

# These are the columns of the sub-tables
table_cols <- c('read_codes', 'read_code_desc', 'icd10_codes', 'icd10_code_desc')

################################# UI ####################################

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel('Code Library'),
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
                                onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )) %>% select(cols)
    
    table <- datatable(data[cols], escape = FALSE, selection = 'none',
                       colnames = cols_view,  filter = 'top', rownames = FALSE,
                       options = list(columnDefs = list( list(searchable = FALSE, targets = 6),
                                                         list(width = '120px', targets = c(3,6)),
                                                         list(width = '60px', targets = 4),
                                                         list(width = '2000px', targets =c(0,1,2, 3)),
                                                         list(className = 'dt-center', targets = 4))))
    
    table
  })
  
  # Render main table in UI
  output$table1 <- renderUI({DT::dataTableOutput("data")})
  
  # This creates a download link button
  output$download_all <- downloadHandler(
    filename = function() {paste( 'Adverse_events_codes_', Sys.Date(), '.xlsx', sep='')},
    content = function(file) { GET(url, write_disk(file)) }
  )
  
  # Render back and download buttons to UI
  output$buttons <- renderUI({
    tagList(downloadButton("download_all", "Download excel file", class = "btn-primary"))
  })
  
  # This defines what happens when you press one of the "View" action buttons in
  # the main table
  observeEvent(input$select_button, {
    
    # This is the row of the action button that was clicked
    selected_row <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    
    # Select relevant rows and columns from original excel file
    all_codes <- filter(excel, AE == df[selected_row, 'AE']) %>% select(table_cols)
    
    read_codes <- na.omit(all_codes[c('read_codes', 'read_code_desc')])

    icd10_codes <- na.omit(all_codes[c('icd10_codes', 'icd10_code_desc')])
    
    # Replace NA with space, for download
    all_codes[ is.na(all_codes)] <- ''
    
    # Render header to UI
    output$header <- renderUI({
      tagList(tags$h4("Adverse Event:", df[selected_row, 'AE'], sep=" "),
              tags$h4( paste("Category:", df[selected_row, 'Category'], sep=" ")) ,
              tags$h4( paste("Owner:", df[selected_row, 'owner'], sep=" ")) ,
              tags$h4( paste("ID:", df[selected_row, 'ID'], sep=" ")))
    })
    
   
    # Create the read code and icd-10 code tables
    output$read_codes= DT::renderDataTable(
      read_codes, colnames = c('Read Codes', 'Description'),
      server = FALSE)

    output$icd10_codes= DT::renderDataTable(
      icd10_codes, colnames = c('ICD-10 Codes', 'Description'),
      server = FALSE)
    
    # Render read code and icd-10 code tables to UI
    output$table1 <- renderUI({
      div(style="padding: 30px 0px", DT::dataTableOutput("icd10_codes"))
    })
    
    output$table2 <- renderUI({
      div(style="padding: 30px 0px", DT::dataTableOutput("read_codes"))})
    
    # This creates a download link button
    output$download <- downloadHandler(
      filename = function() {paste( df[selected_row, 'AE'], "_codes_", Sys.Date(), '.csv', sep='')},
      content = function(file) {write.csv(all_codes, file)}
    )
    
    # Render back and download buttons to UI
    output$buttons <- renderUI({
      tagList( actionButton("return", "Back to all codes", class = "btn-primary"),
               downloadButton("download", "Download CSV", class = "btn-primary"))
    })
    
  })
  
  # This defines what happens when you click the "Back to all codes" button.  
  observeEvent(input$return, {
    
    # Get rid of header
    output$header <- renderUI({ })
    
    # Render main table in UI
    output$table1 <- renderUI({DT::dataTableOutput("data")})
    
    # Get rid of table2
    output$table2 <- renderUI({})
    
    # Put download excel file button back
    output$buttons <- renderUI({
      tagList(downloadButton("download_all", "Download excel file", class = "btn-primary"))
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

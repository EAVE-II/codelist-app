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

# cols is names of column in the main dataframe
# colsView is how the column names will appear in the web app
cols <- c('Category', 'AE', 'viewCodes')
colsView <- c('Category', 'Adverse event', '')

# For after update. 
#cols <- c('AE', 'Cateogry', 'Owner', 'ID', 'viewCodes')
#colsView <- c('Adverse event', 'Category', 'Owner', 'ID', '')

#rename columns
df <- rename(df,  'AE' = 'Adverse event',
             'read_codes' = 'Read codes',
             'icd10_codes' = 'ICD-10 codes',
             'read_codesSource' =  'Source: Read codes',
             'icd10_codesSource' = 'Source: ICD-10 code')

df <- fill(df, c('Category', 'AE', 'read_codesSource', 'icd10_codesSource'), .direction = 'down')

# For after update
# df <- group_by(df, AE) %>% mutate( read_codes = list(read_codes),
#                                    read_codeDesc = list(read_codeDesc),
#                                    icd10_codes = list(icd10_codes),
#                                    icd10_codeDesc = list(read_codeDesc)) %>% unique()

df <- group_by(df, AE) %>% mutate( read_codes = list(read_codes),
                                   icd10_codes = list(icd10_codes)) %>% unique()

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

ID_list['ID'] <- 1:nrow(ID_list)

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
                                onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )) %>% select(cols)
    
    # For after update
    # table <- datatable(data[cols], escape = FALSE, selection = 'none',
    #                    colnames = colsView,  filter = 'top',
    #                    options = list(columnDefs = list( list(targets = 5, searchable = FALSE),
    #                                                      list(width = '80px', targets = 5),
    #                                                      list(width = '2000px', targets =c(1,2,3,4)))))
    
    
    table <- datatable(data[cols], rownames = FALSE, escape = FALSE, selection = 'none',
                       colnames = colsView,  filter = 'top',
                        options = list(columnDefs = list( list(targets = 2, searchable = FALSE),
                                                          list(width = '80px', targets = 2),
                                                          list(width = '2000px', targets =c(0,1)))
                    ))

    
    table
  })
  
  # Render main table in UI
  output$table1 <- renderUI({DT::dataTableOutput("data")})
  
  # This creates a download link button
  output$downloadAll <- downloadHandler(
    filename = function() {paste( 'Adverse_events_codes_', Sys.Date(), '.xlsx', sep='')},
    content = function(file) { GET(url, write_disk(file)) }
  )
  
  # Render back and download buttons to UI
  output$buttons <- renderUI({
    tagList(downloadButton("downloadAll", "Download excel file", class = "btn-primary"))
  })
  
  

  # This is what happens when you press one of the "View" action buttons in
  # the main table
  observeEvent(input$select_button, {
    
    # This is the row of the action button that was clicked
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    
    # For after update
    # # Create data frames with all codes, and read/ICD-10 codes separately
    # allCodes <- data.frame(read_codes = unlist(df[selectedRow, 'read_codes']),
    #                        read_codes_desc = unlist(df[selectedRow, 'read_codes_desc'])
    #                        icd10_codes = unlist(df[selectedRow, 'icd10_codes']),
    #                        icd10_codes_desc = unlist(df[selectedRow, 'icd10_codes_desc']) )
    
    # Create data frames with all codes, and read/ICD-10 codes separately
    allCodes <- data.frame(read_codes = unlist(df[selectedRow, 'read_codes']),
                           icd10_codes = unlist(df[selectedRow, 'icd10_codes']))
    
    allCodes[] <- lapply(allCodes, as.character)

# For after update    
#    read_codes <- na.omit(allCodes[c('read_codes', 'read_codes_desc')])
#
#    icd10_codes <- na.omit(allCodes[c('icd10_codes', 'icd10_codes_desc')])
    
    read_codes <- na.omit(allCodes['read_codes'])
    
    icd10_codes <- na.omit(allCodes['icd10_codes'])
    
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
    
   
    # For after update
    # # Create the read code and icd-10 code tables
    # output$read_codes= DT::renderDataTable(
    #   read_codes, colnames = c('Read Codes', 'Description'),
    #   server = FALSE)
     
    # Create the read code and icd-10 code tables
    output$read_codes= DT::renderDataTable(
      read_codes, colnames = 'Read Codes',
      server = FALSE)
    
    # For after update
    # # Create the read code and icd-10 code tables
    # output$read_codes= DT::renderDataTable(
    #   icd10_codes, colnames = c('ICD-10 Codes', 'Description'),
    #   server = FALSE)
    
    output$icd10_codes= DT::renderDataTable(
      icd10_codes, colnames = 'ICD-10 codes',
      server = FALSE)
    
    # Render read code and icd-10 code tables to UI
    output$table1 <- renderUI({
      div(style="padding: 30px 0px", DT::dataTableOutput("icd10_codes"))
    })
    
    output$table2 <- renderUI({
      div(style="padding: 30px 0px", DT::dataTableOutput("read_codes"))})
    
    
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
    
    # Put download excel file button back
    output$buttons <- renderUI({
      tagList(downloadButton("downloadAll", "Download excel file", class = "btn-primary"))
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


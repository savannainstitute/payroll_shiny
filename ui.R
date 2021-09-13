
library(shiny)
# UI


fluidPage(
  titlePanel("Savanna Institute Payroll-Files Generator"),
  sidebarLayout(
    
    sidebarPanel(
      fileInput('gusto', 'Select Gusto .csv file:',
                accept = c("csv")),
      fileInput('tch', 'Select TCH .csv file:',
                accept = c("csv")),
      fileInput('salary', 'Select Salary Allocation .csv file:',
                accept = c("csv")),
      fileInput('qsehra', 'Select QSEHRA .csv file:',
                accept = c("csv")),
      
      dateInput("MINDATE", "Select start date (yyyy-mm-dd):", value = "2021-01-01"),
      dateInput("MAXDATE", "Select end date (yyyy-mm-dd):", value = "2021-07-30"),
      
      numericInput("rows", "Rows to preview:", 10, min = 1),
      
      textInput('archiveName',"Name of .zip archive:", value = "payroll-files"),
      
      div(style="display:inline-block;width:99%;text-align: center;", downloadButton('download', "Download .zip archive of payroll Excel files"))
      
      
      
    ),
    
    mainPanel(
      # downloadButton("downloadData", "Download")),
      tabsetPanel(
        tabPanel("Gusto file preview", tableOutput("gustoHead")), 
        tabPanel("TCH file preview", tableOutput("tchHead")),
        tabPanel("Salary Allocation file preview", tableOutput("salaryHead")),
        tabPanel("QSEHRA file preview", tableOutput("qsehraHead"))
        )
      )
    )
  )
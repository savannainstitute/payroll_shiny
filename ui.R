
library(shiny)
# UI


fluidPage(
  titlePanel("SI Payroll App"),
  sidebarLayout(
    
    sidebarPanel(
      fileInput('gusto', 'Select Gusto .csv file',
                accept = c("csv")),
      fileInput('tch', 'Select TCH .csv file',
                accept = c("csv")),
      fileInput('salary', 'Select Salary allocation .csv file',
                accept = c("csv")),
      fileInput('qsehra', 'Select QSEHRA .csv file',
                accept = c("csv")),
      
      dateInput("MINDATE", "Select start date (yyyy-mm-dd):", value = "2021-01-01"),
      dateInput("MAXDATE", "Select end date (yyyy-mm-dd):", value = "2021-07-30"),
      
      textInput('archiveName',"Name of .zip archive", value = "payroll"),
      
      downloadButton('download', "Download .zip archive of payroll files", align = "center")
      
    ),
    
    mainPanel(
      # downloadButton("downloadData", "Download")),
      tabsetPanel(
        tabPanel("Gusto file preview", tableOutput("gustoHead")), 
        tabPanel("TCH file preview", tableOutput("tchHead")),
        tabPanel("Salary file preview", tableOutput("salaryHead")),
        tabPanel("QSEHRA file preview", tableOutput("qsehraHead"))
        )
      )
    )
  )
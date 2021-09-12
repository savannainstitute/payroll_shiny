
library(shiny)
# UI


fluidPage(
  titlePanel("SI Payroll App"),
  sidebarLayout(
    
    sidebarPanel(
      fileInput('gusto', 'Choose Gusto CSV file',
                accept = c("csv")),
      fileInput('tch', 'Choose TCH CSV file',
                accept = c("csv")),
      fileInput('salary', 'Choose Salary allocation CSV file',
                accept = c("csv")),
      fileInput('qsehra', 'Choose QSEHRA CSV file',
                accept = c("csv")),
      
      dateInput("MINDATE", "Min date (yyyy-mm-dd):", value = "2021-01-01"),
      dateInput("MAXDATE", "Max date (yyyy-mm-dd):", value = "2021-07-30"),
      
      textInput('archiveName',"Name of .zip archive", value = "payroll"),
      
    ),
    
    mainPanel(
      # downloadButton("downloadData", "Download")),
      tabsetPanel(
        tabPanel("Gusto preview", tableOutput("gustoHead")), 
        tabPanel("TCH preview", tableOutput("tchHead")),
        tabPanel("Salary preview", tableOutput("salaryHead")),
        tabPanel("QSEHRA preview", tableOutput("qsehraHead")),
        
        tabPanel("Preview of first output file", tableOutput("preview2")),
        
        
        tabPanel("Download", downloadButton('download', "Download payroll files"))
        )
      )
    )
  )
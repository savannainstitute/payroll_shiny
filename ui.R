
library(shiny)
# UI


fluidPage(
  titlePanel("Savanna Institute Payroll JE Generator"),
  sidebarLayout(
    
    sidebarPanel(
      fileInput('gusto', 'Select Gusto .csv file:',
                accept = c("csv")),
      fileInput('tch', 'Select TCH .csv file:',
                accept = c("csv")),
      fileInput('salary', 'Select Salary Allocation .csv file:',
                accept = c("csv")),
      fileInput('qsehra', 'Select QSEHRA Allocation .csv file:',
                accept = c("csv")),
      
      dateInput("MINDATE", "Select start date (yyyy-mm-dd):", value = "2021-01-01"),
      dateInput("MAXDATE", "Select end date (yyyy-mm-dd):",   value = "2021-01-31"),
      
      numericInput("rows", "Rows to preview:", 10, min = 1),
      
      textInput('archiveName',"Name of .zip archive:", value = "SI_payroll_JEs"),
      
      div(style="display:inline-block;width:99%;text-align: center;", downloadButton('download', "Download .zip of payroll JE Excel files"))
      
      
      
    ),
    
    mainPanel(
      # downloadButton("downloadData", "Download")),
      tabsetPanel(
        tabPanel("Gusto file preview", tableOutput("gustoHead")),
        tabPanel("TCH file preview", tableOutput("tchHead")),
        tabPanel("Salary Allocation file preview", tableOutput("salaryHead")),
        tabPanel("QSEHRA Allocation file preview", tableOutput("qsehraHead"))
      )
    )
  )
)
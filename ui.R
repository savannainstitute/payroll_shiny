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
      fileInput('salary', 'Select Allocations .csv file:',
                accept = c("csv")),

      dateInput("MINDATE", "Select start date (yyyy-mm-dd):", value = "2023-01-01"),
      dateInput("MAXDATE", "Select end date (yyyy-mm-dd):",   value = "2023-01-31"),

      numericInput("rows", "Rows to preview:", 10, min = 1),

      textInput('archiveName',"Name of .zip archive:", value = "SI_payroll_JEs"),

      div(style="display:inline-block;width:99%;text-align: center;", downloadButton('download', "Download .zip of JE Excel files"))

    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Gusto file preview",       tableOutput("gustoHead")),
        tabPanel("TCH file preview",         tableOutput("tchHead")),
        tabPanel("Allocations file preview", tableOutput("salaryHead"))
      )
    )
  )
)

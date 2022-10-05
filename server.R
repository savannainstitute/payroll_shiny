library(tidyverse)
library(lubridate)
library(xlsx)
library(rdrop2)
drop_auth(rdstoken = "token.rds")

# For testing
# gusto.raw <- read_csv("/Users/boat/Repositories/payroll_shiny/input_example/Gusto.csv",
#                       col_types = cols(),
#                       skip = 1,
#                       col_names = c("type", "pay.period.start", "pay.period.end", "check.date", "first", "last",
#                                     "gross.earnings", "employer.taxes", "name", "taxable.QSEHRA", "regular.earnings"))
# tch.raw <- read_csv("/Users/boat/Repositories/payroll_shiny/input_example/TCH.csv",
#                     col_types = cols(),
#                     skip = 1,
#                     col_names = c("check.date", "last", "first", "taxable.QSEHRA", "tax.free.QSEHRA", "name"))
# salary.raw <- read.csv("/Users/boat/Repositories/payroll_shiny/input_example/Salary\ Alloc.csv", check.names=FALSE)
# qsehra.raw <- read.csv("/Users/boat/Repositories/payroll_shiny/input_example/QSEHRA\ Alloc.csv", check.names=FALSE)
# 
# 
# 
# min.date = "2021-01-01"
# max.date =  "2021-01-31"


source("SI_Payroll_Allocations.R")


function(input, output){
  
  output$files <- renderTable(input$gusto)
  gustoData <- reactive({
    req(input$gusto)
    ext <- tools::file_ext(input$gusto$name)
    switch(ext,
           csv = read_csv(input$gusto$datapath,
                          col_types = cols(),
                          skip = 1,
                          col_names = c("type", "pay.period.start", "pay.period.end", "check.date", "first", "last",
                                        "gross.earnings", "employer.taxes", "name", "taxable.QSEHRA", "regular.earnings")),
           validate("Invalid file; Please upload a .csv file")
           
           
    )
  })
  output$gustoHead <- renderTable({
    head(gustoData(), input$rows)
  })

  tchData <- reactive({
    req(input$tch)
    ext <- tools::file_ext(input$tch$name)
    switch(ext,
           csv = read_csv(input$tch$datapath,
                          col_types = cols(),
                          skip = 1,
                          col_names = c("check.date", "last", "first", "taxable.QSEHRA", "tax.free.QSEHRA", "name")),
           validate("Invalid file; Please upload a .csv file")
    )
  })
  output$tchHead <- renderTable({
    head(tchData(), input$rows)
  })

  salaryData <- reactive({
    req(input$salary)
    ext <- tools::file_ext(input$salary$name)
    switch(ext,
           csv = read_csv(input$salary$datapath, col_types = cols()),
           validate("Invalid file; Please upload a .csv file")
    )
  })
  output$salaryHead <- renderTable({
    head(salaryData(), input$rows)
  })

  qsehraData <- reactive({
    req(input$qsehra)
    ext <- tools::file_ext(input$qsehra$name)
    switch(ext,
           csv = read_csv(input$qsehra$datapath, col_types = cols()),
           validate("Invalid file; Please upload a .csv file")
    )
  })
  output$qsehraHead <- renderTable({
    head(qsehraData(), input$rows)
  })

  outputFiles <- reactive({
    req(input$gusto, input$tch, input$salary, input$qsehra)

    gusto <- gustoData()
    tch <- tchData()
    salary <- salaryData()
    qsehra <- qsehraData()

    write.csv(gusto, 'gusto.csv')
    write.csv(qsehra, 'qsehra.csv')
    write.csv(salary, 'salary.csv')
    write.csv(tch, 'tch.csv')
    
    drop_upload('gusto.csv')
    drop_upload('qsehra.csv')
    drop_upload('salary.csv')
    drop_upload('tch.csv')
    
    withProgress(message = 'Generating payroll files', value = 0, {
    files <- allocate_payroll(gusto.raw  = gusto,
                              tch.raw    = tch,
                              salary.raw = salary,
                              qsehra.raw = qsehra,
                              min.date   = input$MINDATE,
                              max.date   = input$MAXDATE)
    })
    files

  })
  output$preview2 <- renderTable(head(outputFiles()$regular[[1]], 5))

  output$download = downloadHandler(
    filename = paste0(input$archiveName, ".zip"),
    content = function(file){

      # Set temporary working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))

      fs <- c()

      if(length(outputFiles()$regular) > 0){
        for(i in 1:length(outputFiles()$regular)) {
          df <- outputFiles()$regular[[i]] # error: Warning: Error in [[: subscript out of bounds

          path <- paste0(names(outputFiles()$regular)[i], "_PAYROLL_ALLOCATION.xlsx")
          fs <- c(fs, path)
          xlsx::write.xlsx2(df, path, row.names = FALSE, showNA = FALSE, sheetName = NULL)
        }
      }

      if(length(outputFiles()$bonus) > 0){
        for(i in 1:length(outputFiles()$bonus)) {
          df <- outputFiles()$bonus[[i]] # bug: doesn't name the columns w
          path <- paste0(names(outputFiles()$bonus)[i], "_BONUS_PAYROLL_ALLOCATION.xlsx")
          fs <- c(fs, path)
          xlsx::write.xlsx2(df, path, row.names = FALSE, showNA = FALSE)
        }

      }

      # Zip them up
      zip(file, fs)

    }
  )

}

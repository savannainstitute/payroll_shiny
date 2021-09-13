library(tidyverse)
library(lubridate)
library(xlsx)


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
                                        "gross.earnings", "employer.taxes", "taxable.QSEHRA", "regular.earnings")),
           validate("Invalid file; Please upload a .csv file")
    )
  })
  output$gustoHead <- renderTable({
    head(gustoData(), 10)
  })
  
  tchData <- reactive({
    req(input$tch)
    ext <- tools::file_ext(input$tch$name)
    switch(ext,
           csv = read_csv(input$tch$datapath,
                          col_types = cols(),
                          skip = 1,
                          col_names = c("check.date", "last", "first", "taxable.QSEHRA", "tax.free.QSEHRA")),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  output$tchHead <- renderTable({
    head(tchData(), 10)
  })
  
  salaryData <- reactive({
    req(input$salary)
    ext <- tools::file_ext(input$salary$name)
    switch(ext,
           csv = read_csv(input$salary$datapath, col_types = cols()),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  output$salaryHead <- renderTable({
    head(salaryData(), 10)
  })
  
  qsehraData <- reactive({
    req(input$qsehra)
    ext <- tools::file_ext(input$qsehra$name)
    switch(ext,
           csv = read_csv(input$qsehra$datapath, col_types = cols()),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  output$qsehraHead <- renderTable({
    head(qsehraData(), 10)
  })

  outputFiles <- reactive({
    req(input$gusto, input$tch, input$salary, input$qsehra)

    gusto <- gustoData()
    tch <- tchData()
    salary <- salaryData()
    qsehra <- qsehraData()
    
    withProgress(message = 'Generating payroll files', value = 0, {
    files <- allocate_payroll(gusto.raw = gusto,
                              tch.raw = tch,
                              salary.raw = salary,
                              qsehra.raw = qsehra,
                              min.date = input$MINDATE,
                              max.date = input$MAXDATE)
    })
    files
    
  })
  output$preview2 <- renderTable(head(outputFiles()$regular[[1]], 5))
  
  # output$download <- downloadHandler(
  #   filename = function() {
  #     paste0(names(outputFiles()), "_PAYROLL_ALLOCATION.xlsx")
  #   },
  #   content = function(file) {
  #     xlsx::write.xlsx2(outputFiles(), file, row.names = FALSE, showNA = FALSE)
  #   }
  # )
  
  # output$download <- downloadHandler(
  #   filename = function() {
  #     paste0("test.xlsx")
  #   },
  #   content = function(file) {
  #     xlsx::write.xlsx2(outputFiles(), file, row.names = FALSE, showNA = FALSE)
  #   }
  # )

  output$download = downloadHandler(
    filename = paste0(input$archiveName, ".zip"),
    content = function(file){

      # Set temporary working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))

      fs <- c()
      for(i in 1:length(outputFiles()$regular)) {
        df <- outputFiles()$regular[[i]]
        # df <- as.data.frame(df)
        # colnames(df) <- sapply(strsplit(colnames(df), "."), "[", 2)
        
        
        path <- paste0(names(outputFiles()$regular)[i], "_PAYROLL_ALLOCATION.xlsx")
        fs <- c(fs, path)
        xlsx::write.xlsx2(df, path, row.names = FALSE, showNA = FALSE, sheetName = NULL)
      }
      
      for(i in 1:length(outputFiles()$bonus)) {
        df <- outputFiles()$bonus[[i]]
        path <- paste0(names(outputFiles()$bonus)[i], "_BONUS_PAYROLL_ALLOCATION.xlsx")
        fs <- c(fs, path)
        xlsx::write.xlsx2(df, path, row.names = FALSE, showNA = FALSE)
      }

      # Zip them up
      zip(file, fs)
    }
  )
  
  
  
}


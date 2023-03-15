library(tidyverse)
library(lubridate)
library(xlsx)
in.path <- "/Users/kevinwolz/Documents/GitHub/SI_Payroll_Allocation_JE_Creator/input/"

gusto.path  <- paste0(in.path, "SI Payroll Allocations 2023+ - Gusto.csv")
tch.path    <- paste0(in.path, "SI Payroll Allocations 2023+ - TCH.csv")
salary.path <- paste0(in.path, "SI Payroll Allocations 2023+ - Allocations.csv")

##### INPUTS #####
gusto.raw <- read_csv(gusto.path,
                      col_types = cols(),
                      skip = 1,
                      col_names = c("type", "pay.period.start", "pay.period.end", "check.date", "first", "last",
                                    "gross.earnings", "employer.taxes", "name", "taxable.QSEHRA", "regular.earnings"))

tch.raw <- read_csv(tch.path,
                    col_types = cols(),
                    skip = 1,
                    col_names = c("check.date", "last", "first", "taxable.QSEHRA", "tax.free.QSEHRA", "name"))

allocations.raw <- read_csv(salary.path, col_types = cols())


min.date <- mdy("1/1/23")

max.date <- mdy("1/31/23")

##### DO IT #####
out <- allocate_payroll(gusto.raw, tch.raw, allocations.raw, min.date, max.date)

##### WRITE OUTPUTS #####
out.path <- "/Users/kevinwolz/Documents/GitHub/SI_Payroll_Allocation_JE_Creator/output/"
if(length(out$regular) > 0){
  for(i in 1:length(out$regular)) {
    df <- out$regular[[i]] # error: Warning: Error in [[: subscript out of bounds

    path <- paste0(out.path, names(out$regular)[i], "_PAYROLL_ALLOCATION.xlsx")
    xlsx::write.xlsx2(df, path, row.names = FALSE, showNA = FALSE, sheetName = NULL)
  }
}

if(length(out$bonus) > 0){
  for(i in 1:length(out$bonus)) {
    df <- out$bonus[[i]] # bug: doesn't name the columns w
    path <- paste0(out.path, names(out$bonus)[i], "_BONUS_PAYROLL_ALLOCATION.xlsx")
    xlsx::write.xlsx2(df, path, row.names = FALSE, showNA = FALSE)
  }

}

gusto.raw <- read_csv("/Users/boat/Repositories/payroll_shiny/input_example/Gusto.csv",
                  col_types = cols(),
                  skip = 1,
                  col_names = c("type", "pay.period.start", "pay.period.end", "check.date", "first", "last",
                                "gross.earnings", "employer.taxes", "name", "taxable.QSEHRA", "regular.earnings"))


tch.raw <- read_csv("/Users/boat/Repositories/payroll_shiny/input_example/TCH.csv",
               col_types = cols(),
               skip = 1,
               col_names = c("check.date", "last", "first", "taxable.QSEHRA", "tax.free.QSEHRA", "name"))

salary.raw <- read.csv("/Users/boat/Repositories/payroll_shiny/input_example/Salary\ Alloc.csv", check.names=FALSE)

qsehra.raw <- read.csv("/Users/boat/Repositories/payroll_shiny/input_example/QSEHRA\ Alloc.csv", check.names=FALSE)



min.date = "2021-01-01"
max.date =  "2021-01-31"

files <- allocate_payroll(gusto.raw  = gusto,
                          tch.raw    = tch,
                          salary.raw = salary,
                          qsehra.raw = qsehra,
                          min.date   = MINDATE,
                          max.date   = MAXDATE)


send.mail(
  from = "scottbrainard@gmail.com",
  to = "scott@savannainstitute.org",
  subject = "Testing send",
  body = input$gusto$datapath,
  authenticate = TRUE, 
  html = TRUE, 
  send = TRUE,
  smtp = list(host.name = "smtp.mailtrap.io",
              port = 587,
              user.name = "013286f17bb050",
              passwd = "ed409e7676123b",
              tls = TRUE))
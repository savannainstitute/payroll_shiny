allocate_payroll <- function(gusto.raw, tch.raw, allocations.raw, min.date, max.date) {

  OUT <- list(regular = list(),
              bonus   = list())

  gusto.raw <- gusto.raw %>%
    dplyr::select(type, check.date, name, regular.earnings, employer.taxes) %>%
    filter(!is.na(check.date)) %>%
    mutate(check.date = mdy(check.date))

  gusto.bonus <- gusto.raw %>%
    filter(type != "Regular") %>%
    filter(regular.earnings + employer.taxes > 0) %>%
    dplyr::select(-type)

  gusto <- gusto.raw %>%
    filter(type == "Regular") %>%
    dplyr::select(-type)

  TCH <- tch.raw %>%
    select(-last, -first) %>%
    filter(!is.na(check.date)) %>%
    mutate(taxable.QSEHRA  = replace_na(taxable.QSEHRA,  0),
           tax.free.QSEHRA = replace_na(tax.free.QSEHRA, 0)) %>%
    mutate(total.QSEHRA = taxable.QSEHRA + tax.free.QSEHRA) %>%
    mutate(check.date = mdy(check.date))

  alloc_clean <- function(x) {
    x[["Notes"]] <- NULL

    x %>%
      dplyr::select(-all_of(c("Staff Start", "Staff End", "Fund Start", "Fund End", "Match Start", "Match End"))) %>%
      rename(name = Name, fund = Fund, matches = Matches) %>%
      filter(!is.na(name) & !is.na(fund))
  }

  qsehra <- allocations <- alloc_clean(allocations.raw)

  DATES <- names(allocations) %>%
    subset(!(. %in% c("name", "fund", "matches", "Can be Match?"))) %>%
    mdy() %>%
    subset(. >= min.date & . <= max.date)

  ##### REGULAR PAYROLLS #####
  ## Create JEs

  BONUS.DATES <- gusto.bonus$check.date %>%
    unique() %>%
    subset(. >= min.date & . <= max.date)

  n <- length(DATES) + length(BONUS.DATES)
  for(i in 1:length(DATES)) {

    #incProgress(1/n, detail = paste("Creating file", i))

    d      <- DATES[i]
    d.nice <- gsub("-", "", as.character(d))
    d.col  <- which(mdy(names(allocations)) == d)

    # Filter data
    g <- gusto %>%
      filter(check.date == d) %>%
      dplyr::select(-check.date)

    tch <- TCH %>%
      filter(check.date == d) %>%
      dplyr::select(-check.date)

    q.exists <- sum(tch$total.QSEHRA) > 0

    # Combine data + allocations
    p <- allocations %>%
      dplyr::select(name, fund, matches, d.col) %>%
      rename(percs = names(.)[ncol(.)]) %>%
      filter(!is.na(percs)) %>%
      left_join(g, by = "name") %>%
      mutate(percs = as.numeric(gsub("%", "", percs)) / 100) %>%
      mutate(wages = round(percs * regular.earnings, 2),
             taxes = round(percs * employer.taxes,   2))

    q <- qsehra %>%
      dplyr::select(name, fund, matches, d.col) %>%
      rename(percs = names(.)[ncol(.)]) %>%
      filter(!is.na(percs)) %>%
      left_join(tch, by = "name") %>%
      mutate(percs = as.numeric(gsub("%", "", percs)) / 100) %>%
      mutate(taxable  = round(percs * taxable.QSEHRA,  2),
             tax.free = round(percs * tax.free.QSEHRA, 2),
             total    = round(percs * total.QSEHRA,    2))

    # Check that each employee is exactly 100% allocated
    p.PERC.CHECK <- p %>%
      group_by(name) %>%
      summarize(percs = sum(percs))
    if(any(p.PERC.CHECK$percs != 1)) stop(paste("Salary allocations for some staff not summing to 100% on", d))

    q.PERC.CHECK <- q %>%
      group_by(name) %>%
      summarize(percs = sum(percs))
    if(any(q.PERC.CHECK$percs != 1)) stop(paste("QSEHRA allocations for some staff not summing to 100% on", d))

    # Build salary portion of JE
    for.JE <- p %>%
      group_by(fund, matches) %>%
      summarize(wages = sum(wages, na.rm = TRUE),
                taxes = sum(taxes, na.rm = TRUE)) %>%
      filter(wages + taxes > 0)

    subtotals <- for.JE %>%
      ungroup() %>%
      summarize(wages = sum(wages, na.rm = TRUE),
                taxes = sum(taxes, na.rm = TRUE))

    fund.summary <- for.JE %>%
      group_by(fund) %>%
      summarize(total = sum(wages + taxes))

    JE1 <- tibble(Account = "5100 - Gross Wages",
                  Fund    = for.JE$fund,
                  Debit   = for.JE$wages,
                  `Tags: Department` = "Administration",
                  `Tags: Program`    = "Overhead")

    JE2 <- tibble(Account = "5110 - Employer Payroll Taxes",
                  Fund    = for.JE$fund,
                  Debit   = for.JE$taxes,
                  `Tags: Department` = "Administration",
                  `Tags: Program`    = "Overhead")

    JE3 <- tibble(Account = "999 - Wash Account",
                  Fund    = fund.summary$fund,
                  Credit  = fund.summary$total)

    JEtop <- tibble(Date    = c(d, NA, NA),
                    Payee   = NA, # If there are no QSEHRA payments this period, this initaites this column
                    Memo    = c(paste("Payroll Allocation", d.nice), NA, NA),
                    Account = c("5100 - Gross Wages", "5110 - Employer Payroll Taxes", "999 - Wash Account"),
                    Fund    = "3998 - Payroll Allocation Fund",
                    Debit   = c(NA, NA, subtotals$wages + subtotals$taxes),
                    Credit  = c(subtotals$wages, subtotals$taxes, NA),
                    `Tags: Department` = c("Administration", "Administration", NA),
                    `Tags: Program` = c("Overhead", "Overhead", NA))

    # Build salary portion of JE
    if(q.exists) {
      for.JEq <- q %>%
        group_by(fund, matches, name) %>%
        summarize(taxable  = sum(taxable,  na.rm = TRUE),
                  tax.free = sum(tax.free, na.rm = TRUE),
                  total    = sum(total,    na.rm = TRUE)) %>%
        filter(total > 0)

      subtotals.q <- for.JEq %>%
        ungroup() %>%
        summarize(taxable  = sum(taxable,  na.rm = TRUE),
                  tax.free = sum(tax.free, na.rm = TRUE),
                  total    = sum(total,    na.rm = TRUE))

      fund.summary.q <- for.JEq %>%
        group_by(fund) %>%
        summarize(total = sum(total, na.rm = TRUE))

      JEq1 <- tibble(Account = "5115 - QSEHRA",
                     Fund    = for.JEq$fund,
                     Debit   = for.JEq$total,
                     Payee   = for.JEq$name,
                     Note    = for.JEq$name,
                     `Tags: Department` = "Administration",
                     `Tags: Program` = "Overhead")

      JEq2 <- tibble(Account = "999 - Wash Account",
                     Fund    = fund.summary.q$fund,
                     Credit  = fund.summary.q$total)

      JEqtop <- tibble(Account = c("5120 - Reimbursements", "5100 - Gross Wages", "999 - Wash Account"),
                       Fund    = "3998 - Payroll Allocation Fund",
                       Debit   = c(NA, NA, subtotals.q$total),
                       Credit  = c(subtotals.q$tax.free, subtotals.q$taxable, NA),
                       `Tags: Department` = c("Administration", "Administration", NA),
                       `Tags: Program`    = c("Overhead", "Overhead", NA)) %>%
        filter(is.na(Credit) | Credit > 0) # Removes subtotal of 0 if there is either no taxable or no tax-free payments
    } else {
      JEqtop <- JEq1 <- JEq2 <- tibble() # In case there are no QSEHRA payments at all this period
    }
    # Merge salary + QSEHRA portions and write out
    JE.out <- bind_rows(JEtop, JE1, JE2, JE3, JEqtop, JEq1, JEq2) %>%
      mutate(Note = NA) %>%
      dplyr::select(Date, Memo, Account, Fund, Payee, Note, Debit, Credit,
             `Tags: Department`, `Tags: Program`) %>%
      as.data.frame()

    OUT$regular[d.nice] <- list(JE.out)
  }

  ##### BONUS PAYROLLS #####
  ## Create JEs

  if(length(BONUS.DATES) > 0) {
    for(i in 1:length(BONUS.DATES)) {
      #incProgress(1/n, detail = paste("Creating file", i))

      d      <- BONUS.DATES[i]
      d.nice <- gsub("-", "", as.character(d))

      p <- gusto.bonus %>%
        filter(check.date == d) %>%
        dplyr::select(-check.date) %>%
        mutate(fund = "3000 - General Fund", matches = NA_character_, percs = 1) %>%
        mutate(wages = round(percs * regular.earnings, 2),
               taxes = round(percs * employer.taxes,   2))

      for.JE <- p %>%
        group_by(fund, matches) %>%
        summarize(wages = sum(wages, na.rm = TRUE),
                  taxes = sum(taxes, na.rm = TRUE)) %>%
        filter(wages + taxes > 0)

      subtotals <- for.JE %>%
        ungroup() %>%
        summarize(wages = sum(wages, na.rm = TRUE),
                  taxes = sum(taxes, na.rm = TRUE))

      fund.summary <- for.JE %>%
        group_by(fund) %>%
        summarize(total = sum(wages + taxes))

      JE1 <- tibble(Account = "5100 - Gross Wages",
                    Fund    = for.JE$fund,
                    Debit   = for.JE$wages,
                    `Tags: Department` = "Administration",
                    `Tags: Program` = "Overhead")

      JE2 <- tibble(Account = "5110 - Employer Payroll Taxes",
                    Fund    = for.JE$fund,
                    Debit   = for.JE$taxes,
                    `Tags: Department` = "Administration",
                    `Tags: Program` = "Overhead")

      JE3 <- tibble(Account = "999 - Wash Account",
                    Fund    = fund.summary$fund,
                    Credit  = fund.summary$total)

      JEtop <- tibble(Date    = c(d, NA, NA),
                      Payee   = NA, # If there are no QSEHRA payments this period, this initaites this column
                      Memo    = c(paste("BONUS Payroll Allocation", d.nice), NA, NA),
                      Account = c("5100 - Gross Wages", "5110 - Employer Payroll Taxes", "999 - Wash Account"),
                      Fund    = "3998 - Payroll Allocation Fund",
                      Debit   = c(NA, NA, subtotals$wages + subtotals$taxes),
                      Credit  = c(subtotals$wages, subtotals$taxes, NA),
                      `Tags: Department` = c("Administration", "Administration", NA),
                      `Tags: Program` = c("Overhead", "Overhead", NA))

      JE.out <- bind_rows(JEtop, JE1, JE2, JE3) %>%
        mutate(Note = NA) %>%
        dplyr::select(Date, Memo, Account, Fund, Payee, Note, Debit, Credit,
               `Tags: Department`, `Tags: Program`) %>%
        as.data.frame()

      OUT$bonus[d.nice] <- list(JE.out)
    }
  }

  return(OUT)
}

calculate_monthly_payment <- function(principal, annual_rate, years) {
    r <- annual_rate / 12 / 100
    n <- years * 12
    M <- principal * r * (1 + r)^n / ((1 + r)^n - 1)

    return(round (M,2))
}

build_amortization_schedule <- function(principal, annual_rate, years) {
#create the empty vectors
months_vec <- c()
payment_vec <- c()
principal_vec <- c()
interest_vec <- c()
balance_vec <- c()

# calculate the values for the loop
r <- annual_rate / 12 / 100
n <- years * 12
M <- calculate_monthly_payment(principal, annual_rate, years)
balance <- principal   #updated each month

#loop 
for (i in 1:n) {
    interest_portion <- balance * r
    principal_portion <- M - interest_portion
    balance <- balance - principal_portion
    months_vec <- c(months_vec, i) 
    payment_vec <- c(payment_vec, M)
    principal_vec <- c(principal_vec, principal_portion)
    interest_vec <- c(interest_vec, round(interest_portion, 2))
    balance_vec <- c(balance_vec, round(balance, 2))
}

schedule <- data.frame(
  Month = months_vec,
  Payment = payment_vec,
  Principal = principal_vec,
  Interest = interest_vec,
  Balance = balance_vec
)
  
  return(schedule)
}

print_loan_summary <- function(principal, annual_rate, years) {
  schedule       <- build_amortization_schedule(principal, annual_rate, years)
  monthly        <- calculate_monthly_payment(principal, annual_rate, years)
  total_paid     <- round(sum(schedule$Payment), 2)
  total_interest <- round(sum(schedule$Interest), 2)

  cat("\n========================================\n")
  cat("           LOAN SUMMARY\n")
  cat("========================================\n")
  cat(sprintf("  Loan Amount:          $%s\n",   format(principal,      big.mark = "," , scientific = FALSE)))
  cat(sprintf("  Annual Rate:          %.2f%%\n", annual_rate))
  cat(sprintf("  Term:                 %d years\n", years))
  cat(sprintf("  Monthly Payment:      $%s\n",   format(monthly,        nsmall = 2, big.mark = ",")))
  cat(sprintf("  Total Paid:           $%s\n",   format(total_paid,     nsmall = 2, big.mark = ",")))
  cat(sprintf("  Total Interest Paid:  $%s\n",   format(total_interest, nsmall = 2, big.mark = ",")))
  cat("========================================\n\n")

  cat("--- Month-by-Month Schedule ---\n")
  print(schedule, row.names = FALSE)
  cat("\n")

  return(invisible(schedule))
}


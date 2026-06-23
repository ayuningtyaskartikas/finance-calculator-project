# ============================================================
# savings_calculator.R
# ============================================================


calculate_savings <- function(principal, annual_rate, years, monthly_contribution = 0) {
  r <- annual_rate / 100 / 12   # monthly rate
  n <- years * 12               # total months

  fv_principal     <- principal * (1 + r)^n
  fv_contributions <- if (r == 0) monthly_contribution * n
                      else monthly_contribution * ((1 + r)^n - 1) / r

  return(round(fv_principal + fv_contributions, 2))
}


build_savings_schedule <- function(principal, annual_rate, years, monthly_contribution = 0) {
  r <- annual_rate / 100 / 12
  t <- 1:years
  n <- t * 12                   # vector of total months per year

  # Vectorized — computes all years at once, no loop needed
  fv_principal     <- principal * (1 + r)^n
  fv_contributions <- if (r == 0) monthly_contribution * n
                      else monthly_contribution * ((1 + r)^n - 1) / r

  balance           <- round(fv_principal + fv_contributions, 2)
  total_contributed <- principal + (monthly_contribution * 12 * t)
  interest_earned   <- round(balance - total_contributed, 2)

  return(data.frame(
    Year              = t,
    Balance           = balance,
    Total_Contributed = total_contributed,
    Interest_Earned   = interest_earned
  ))
}


print_savings_summary <- function(principal, annual_rate, years, monthly_contribution = 0) {
  schedule          <- build_savings_schedule(principal, annual_rate, years, monthly_contribution)
  final_balance     <- tail(schedule$Balance, 1)
  total_contributed <- tail(schedule$Total_Contributed, 1)
  interest_earned   <- tail(schedule$Interest_Earned, 1)

  cat("\n========================================\n")
  cat("          SAVINGS SUMMARY\n")
  cat("========================================\n")
  cat(sprintf("  Initial Deposit:      $%s\n",   format(principal,            big.mark = ",")))
  cat(sprintf("  Monthly Contribution: $%s\n",   format(monthly_contribution, big.mark = ",")))
  cat(sprintf("  Annual Rate:          %.2f%%\n", annual_rate))
  cat(sprintf("  Term:                 %d years\n", years))
  cat(sprintf("  Final Balance:        $%s\n",   format(final_balance,        nsmall = 2, big.mark = ",")))
  cat(sprintf("  Total Contributed:    $%s\n",   format(total_contributed,    big.mark = ",")))
  cat(sprintf("  Interest Earned:      $%s\n",   format(interest_earned,      nsmall = 2, big.mark = ",")))
  cat("========================================\n\n")

  cat("--- Year-by-Year Growth ---\n")
  print(schedule, row.names = FALSE)
  cat("\n")

  return(invisible(schedule))
}
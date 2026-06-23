# ============================================================
# charts.R
# ============================================================

library(ggplot2)


#CHART 1: Savings balance growth over time
plot_savings <- function(schedule) {
  ggplot(schedule, aes(x = Year)) +
    geom_line(aes(y = Balance, color = "Balance"), linewidth = 1.2) +
    geom_line(aes(y = Total_Contributed, color = "Total Contributed"), linewidth = 1.2, linetype = "dashed") +
    scale_color_manual(values = c("Balance" = "#185FA5", "Total Contributed" = "#D85A30")) +
    scale_y_continuous(labels = scales::dollar) +
    labs(
      title    = "Savings Growth Over Time",
      x        = "Year",
      y        = "Amount ($)",
      color    = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}


#CHART 2: Loan balance over time
plot_loan_balance <- function(schedule) {
  ggplot(schedule, aes(x = Month, y = Balance)) +
    geom_line(color = "#185FA5", linewidth = 1.2) +
    scale_y_continuous(labels = scales::dollar) +
    labs(
      title = "Remaining Loan Balance Over Time",
      x     = "Month",
      y     = "Balance ($)"
    ) +
    theme_minimal()
}


#CHART 3: Principal vs interest paid per year (stacked bar)
plot_loan_breakdown <- function(schedule) {
  # Summarize monthly data into yearly totals
  schedule$Year <- ceiling(schedule$Month / 12)

  yearly <- aggregate(cbind(Principal, Interest) ~ Year, data = schedule, FUN = sum)

  # Reshape to long format for ggplot
  long <- reshape(yearly,
    varying   = c("Principal", "Interest"),
    v.names   = "Amount",
    timevar   = "Type",
    times     = c("Principal", "Interest"),
    direction = "long"
  )

  ggplot(long, aes(x = Year, y = Amount, fill = Type)) +
    geom_col() +
    scale_fill_manual(values = c("Principal" = "#0F6E56", "Interest" = "#D85A30")) +
    scale_y_continuous(labels = scales::dollar) +
    labs(
      title = "Principal vs. Interest Paid Per Year",
      x     = "Year",
      y     = "Amount ($)",
      fill  = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}


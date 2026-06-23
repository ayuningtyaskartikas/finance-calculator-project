calculate_monthly_payment <- function(principal, annual_rate, years) {
    #convert the annual % into a monthly decimal
    r <- annual_rate / 12 / 100

    #monthly payments
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

# define inputs
principal <- 200000
annual_rate <- 6.5
years <- 30

# calculate the values for the loop
r <- annual_rate / 12 / 100
n <- years * 12
M <- calculate_monthly_payment(principal, annual_rate, years)
balance <- principal   #updated each month

#loop 
for (i in 1:n) {

  # 1. calculate interest for this month
    interest_portion <- balance * r
  # 2. calculate principal for this month
    principal_portion <- M - interest_portion
  # 3. update balance (subtract principal)
    balance <- balance - principal_portion
  # 4. append i to months_vec
    months_vec <- c(months_vec, i) 
  # 5. append payment to payment_vec
    payment_vec <- c(payment_vec, M)
  # 6. append principal to principal_vec
    principal_vec <- c(principal_vec, principal_portion)
  # 7. append interest to interest_vec
    interest_vec <- c(interest_vec, interest_portion)
  # 8. append balance to balance_vec
    balance_vec <- c(balance_vec, balance)
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



library(urca)
# simulation


# Set seed for reproducibility
set.seed(123348576)
plot(u)
# Number of time points
n <- 50

# Generate white noise
u <- rnorm(n)

# 1. Pure Random Walk: Yt = Yt-1 + ut
Y1 <- numeric(n)
Y1
Y1[1] <- 0  # starting point
for (t in 2:n) {
  Y1[t] <- Y1[t-1] + u[t]
}

plot(Y1)
plot(Y1, type = "l", main = "1. Pure Random Walk", xlab = "Time", ylab = "Y1")



ur.df(Y1, type = "none", selectlags = "BIC" ) |> summary()  #non
ur.df(Y1, type = "drift", selectlags = "BIC" ) |> summary() 
ur.df(Y1, type = "trend", selectlags = "BIC" ) |> summary()

plot(diff(Y1), type = "l")
ur.df(diff(Y1), type = "none", selectlags = "BIC" ) |> summary()  #stationary

# 2. Random Walk with Drift: Yt = beta + Yt-1 + ut
beta <- 0.5
Y2 <- numeric(n)
Y2[1] <- 0
for (t in 2:n) {
  Y2[t] <- 0.5 + Y2[t-1] + u[t]
}

plot(Y2, type = "l", main = "2. Random Walk with Drift",
     xlab = "Time",
     ylab = "Y2")
plot(diff(Y2), type = "l", main = expression(Delta* "Random Walk with Drift"),
     xlab = "Time",
     ylab = "Y2")

ur.df(Y2, type = "none", lags = 2 ) |> summary()
ur.df(Y2, type = "drift", lags = 4) |> summary()
ur.df(Y2, type = "trend", selectlags = "BIC" ) |> summary()

ur.df(diff(Y2), type = "none", selectlags = "BIC" ) |> summary()
ur.df(diff(Y2), type = "drift", selectlags = "BIC" ) |> summary()
ur.df(diff(Y2), type = "trend", selectlags = "BIC" ) |> summary()



# 3. Random Walk with Drift + Trend: Yt = beta1 + beta2 * t + Yt-1 + ut
beta1 <- 0.5
beta2 <- 0.8
Y3 <- numeric(n)
Y3[1] <- 0.5
for (t in 2:n) {
  Y3[t] <-0.7 + 0.08 * t + 0.61*Y3[t-1] + u[t]
}
plot(Y3, type = "l", main = "3. Random Walk with Drift and Trend", 
     xlab = "Time", ylab = "Y")


ur.df(Y3, type = "none", lags = 2 ) |> summary()
ur.df(Y3, type = "drift", lags = 4) |> summary()
ur.df(Y3, type = "trend", selectlags = "BIC" ) |> summary()
plot(diff(Y3), type = "l")
ur.df(diff(Y3), type = "none", selectlags = "BIC" ) |> summary()
ur.df(diff(Y3), type = "drift", selectlags = "BIC" ) |> summary()
ur.df(diff(Y3), type = "trend", selectlags = "BIC" ) |> summary()



# Now plot them
par(mfrow = c(3,1))  # 3 plots in one column
dev.new()





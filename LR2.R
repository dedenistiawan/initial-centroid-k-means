#membangkitkan bilangan random X dan Y
library(tidyverse)
set.seed(123)
theta_0 <- 5
theta_1 <- 2
n_obs <- 500
x <- rnorm(n_obs)
y <- theta_1*x + theta_0 + rnorm(n_obs, 0, 3)

#menggabungkan data X dan Y
data <- tibble(x = x, y = y)

#membuat plot X dan Y
ggplot(data, aes(x = x, y = y)) + 
  geom_point(size = 2) + theme_bw() + 
  labs(title = 'Data Simulasi')

#mengahapus variabel theta_0 &theta_1
rm(theta_0, theta_1)

#Menghitung Persamaan Regresi dengan manual
x_bar <- mean(x) # Menghitung nilai rata-rata variabel X
y_bar <- mean(y) #Menghitung nilai rata-rata variabel Y

#menghitung Nilai Slope/Beta(b)
slope <- sum((x - x_bar)*(y - y_bar))/sum((x - x_bar)^2)
slope

#menghitung nilai intercep/Alpha (a)
intercept <- y_bar - (slope * x_bar) 
intercept

plot(x,y, col = "black", main='Regresi dengan Perhitungan Manual'); 
abline(a = intercept, b = slope, col='blue', lwd=2)


#menghitung persamaan regresi dengan OLS menggunkan fungsi lm
ols <- lm(y ~ x, data = data)
mod <- print(ols)

#menghitung persamaan regresi dengan gradient descent
cost_function <- function(theta_0, theta_1, x, y){
  pred <- theta_1*x + theta_0
  res_sq <- (y - pred)^2
  res_ss <- sum(res_sq)
  return(mean(res_ss))
}

cost_function(theta_0 = ols$coefficients[1][[1]], 
              theta_1 = ols$coefficients[2][[1]],
              x = data$x, y = data$y)


gradient_desc <- function(theta_0, theta_1, x, y){
  N = length(x)
  pred <- theta_1*x + theta_0
  res <- y - pred
  delta_theta_0 <- (2/N)*sum(res)
  delta_theta_1 <- (2/N)*sum(res*x)
  return(c(delta_theta_0, delta_theta_1))
}

alpha <- 0.1
iter <- 100

minimize_function <- function(theta_0, theta_1, x, y, alpha){
  gd <- gradient_desc(theta_0, theta_1, x, y)
  d_theta_0 <- gd[1] * alpha
  d_theta_1 <- gd[2] * alpha
  new_theta_0 <- theta_0 + d_theta_0
  new_theta_1 <- theta_1 + d_theta_1
  return(c(new_theta_0, new_theta_1))
}

res <- list()
res[[1]] <- c(0, 0)

for (i in 2:iter){
  res[[i]] <- minimize_function(
    res[[i-1]][1], res[[i-1]][2], data$x, data$y, alpha
  )
}

res <- lapply(res, function(x) as.data.frame(t(x))) %>% bind_rows()
colnames(res) <- c('theta0', 'theta1')

loss <- res %>% as_tibble() %>% rowwise() %>%
  summarise(mse = cost_function(theta0, theta1, data$x, data$y))

res <- res %>% bind_cols(loss) %>%
  mutate(iteration = seq(1, 100)) %>% as_tibble()

res

ggplot(res, aes(x = iteration, y = mse)) + 
  geom_point(size = 2) + 
  theme_classic() + geom_line(aes(group = 1)) +
  labs(title = 'Gradient descent over 100 iterations')

ggplot(data, aes(x = x, y = y)) + 
  geom_point(size = 2) + 
  geom_abline(aes(intercept = theta0, slope = theta1),
              data = res, linewidth = 0.5, color = 'red') + 
  theme_classic() + 
  geom_abline(aes(intercept = theta0, slope = theta1), 
              data = res %>% slice_head(), 
              linewidth = 0.5, color = 'blue') + 
  geom_abline(aes(intercept = theta0, slope = theta1), 
              data = res %>% slice_tail(), 
              linewidth = 0.5, color = 'green') +
  labs(title = 'Gradient descent over 100 iterations')

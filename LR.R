#membangkitkan Bilangan Random
set.seed(123)
x <- runif(1000, -5, 5)
y <- x + rnorm(1000) + 3

data <- tibble(x = x, y = y)


x_bar <- mean(x) # Menghitung nilai rata-rata variabel X
y_bar <- mean(y) #Menghitung nilai rata-rata variabel Y

#menghitung Nilai Slope/Beta(b)
slope <- sum((x - x_bar)*(y - y_bar))/sum((x - x_bar)^2)
slope

#menghitung nilai intercep/Alpha (a)
intercept <- y_bar - (slope * x_bar) 
intercept

plot(x,y, col = "grey80", main='Regresi dengan Perhitungan Manual', xlim = c(-2, 5), ylim = c(0,10)); 
abline(a = intercept, b = slope, col='blue', lwd=2)

#menghitung persamaan regresi dengan fungsi lm
lm <- lm( y ~ x )
mod <- print(lm)

#Gradient Descent:
cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y))
}

alpha <- 0.01
num_iters <- 1000

# keep history
cost_history <- double(num_iters)
theta_history <- list(num_iters)

# initialize coefficients
theta <- matrix(c(0,0), nrow=2)

# add a column of 1's for the intercept coefficient
X <- cbind(1, matrix(x))

# gradient descent
for (i in 1:num_iters) {
  error <- (X %*% theta - y)
  delta <- t(X) %*% error / length(y)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X, y, theta)
  theta_history[[i]] <- theta
}

print(theta)

iters <- c((1:31)^2, 1000)
cols <- rev(terrain.colors(num_iters))
library(gifski)
png("frame%03d.png")
par(ask = FALSE)

for (i in iters) {
  plot(x,y, col="grey80", main='Linear regression using Gradient Descent')
  text(x = -3, y = 10, paste("slope = ", round(theta_history[[i]][2], 3), sep = " "), adj = 0)
  text(x = -3, y = 8, paste("intercept = ", round(theta_history[[i]][1], 3), sep = " "), adj = 0)
  abline(coef=theta_history[[i]], col=cols[i], lwd = 2)
}

dev.off()



png_files <- sprintf("frame%03d.png", 1:32)
gif_file <- gifski(png_files, delay = 0.1)
unlink(png_files)
utils::browseURL(gif_file)

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


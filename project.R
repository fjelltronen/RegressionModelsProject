data(mtcars)
mtcars$am <- factor(mtcars$am)

table(mtcars$am)

## We will use a t-test to compare the `mpg` of automatic/manual transmissions.

auto <- mtcars[mtcars$am == "0",]$mpg
# test the normality of this variable
shapiro.test(auto)$p > 0.05
manual <- mtcars[mtcars$am == "1",]$mpg
# test the normality of this variable
shapiro.test(manual)$p > 0.05

# do they have the same variance?
var.test(x = auto, y = manual, alternative = "less")$p.value > 0.05
# t-test
t.test(x = auto,y = manual,var.equal = FALSE,paired = FALSE,alternative = "less")$p.value < 0.05

g1 <- ggplot(data = mtcars, aes(x = am, y = mpg)) + 
  geom_point(size = 3, alpha = 0.4) + labs(title = "mpg vs. am", x = "transmission (0 = automatic, 1 = manual)", y = "fuel consumption") + 
  geom_smooth(method = "lm", formula = mpg~am)




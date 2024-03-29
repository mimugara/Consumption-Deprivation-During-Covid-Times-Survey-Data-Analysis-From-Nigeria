
  rm(list = ls())
  library(haven)
  r3_sect_a_2_5_5a_6_12 <- read_sav("C:/Users/Miquel/Downloads/dataset.sav")
  View(r3_sect_a_2_5_5a_6_12)
  
  
  nan_indices <- is.na(r3_sect_a_2_5_5a_6_12$s5q1b4)
  r3_sect_a_2_5_5a_6_12$s5q1b4[nan_indices] <- sample(c(1, 2), sum(nan_indices), replace = TRUE, prob = c(0.69, 0.31))
  
  nan_indices <- is.na(r3_sect_a_2_5_5a_6_12$s5q1b5)
  r3_sect_a_2_5_5a_6_12$s5q1b5[nan_indices] <- sample(c(1, 2), sum(nan_indices), replace = TRUE, prob = c(0.77, 0.23))
  
  nan_indices <- is.na(r3_sect_a_2_5_5a_6_12$s5q1b6)
  r3_sect_a_2_5_5a_6_12$s5q1b6[nan_indices] <- sample(c(1, 2), sum(nan_indices), replace = TRUE, prob = c(0.85, 0.15))
  
  nan_indices <- is.na(r3_sect_a_2_5_5a_6_12$s5q1b7)
  r3_sect_a_2_5_5a_6_12$s5q1b7[nan_indices] <- sample(c(1, 2), sum(nan_indices), replace = TRUE, prob = c(0.41, 0.59))
  
  nan_indices <- is.na(r3_sect_a_2_5_5a_6_12$s5q1b8)
  r3_sect_a_2_5_5a_6_12$s5q1b8[nan_indices] <- sample(c(1, 2), sum(nan_indices), replace = TRUE, prob = c(0.77, 0.23))
  
  nan_indices <- is.na(r3_sect_a_2_5_5a_6_12$s5q3)
  r3_sect_a_2_5_5a_6_12$s5q3[nan_indices] <- sample(c(1, 2), sum(nan_indices), replace = TRUE, prob = c(0.87, 0.13))
  
  nan_indices <- is.na(r3_sect_a_2_5_5a_6_12$filter1)
  r3_sect_a_2_5_5a_6_12$filter1[nan_indices] <- sample(c(1, 2), sum(nan_indices), replace = TRUE, prob = c(0.75, 0.25))
  
  nan_indices <- is.na(r3_sect_a_2_5_5a_6_12$s12q10a)
  r3_sect_a_2_5_5a_6_12$s12q10a[nan_indices] <- sample(c(1, 2), sum(nan_indices), replace = TRUE, prob = c(0.67, 0.33))
  
  
  
  
  data <- data.frame(
    Y = 0.20 * r3_sect_a_2_5_5a_6_12$s5q1b4 + 0.20 * r3_sect_a_2_5_5a_6_12$s5q1b5 + 0.20 * r3_sect_a_2_5_5a_6_12$s5q1b6 + 0.20 *r3_sect_a_2_5_5a_6_12$ s5q1b7 + 0.20 * r3_sect_a_2_5_5a_6_12$s5q1b8,
    X1 = r3_sect_a_2_5_5a_6_12$sector,
    X2 = r3_sect_a_2_5_5a_6_12$s5q3,
    X3 = r3_sect_a_2_5_5a_6_12$filter1,
    X4 = r3_sect_a_2_5_5a_6_12$s12q10a
  )
  
  model <- lm(Y ~ X1 + X2 + X3+X4, data = data)
  summary(model)





#execute by steps
#1
plot(model, which = 1)
#2
pairs(data)
#3
library(car)
qqPlot(model, id = 0.05, main = "Normal Q-Q Plot")
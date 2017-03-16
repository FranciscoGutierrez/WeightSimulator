products <- read.csv("products_v1.csv")  # read csv file 


# Metric Units (kg, cm, years)
# 
# calculateBMR(110, 180, 27, TRUE)
# Mifflin - St Jeor equation. 
calculateBMR <- function(weight, height, age, male = TRUE){
  BMR <- 0
  if(male) {
    BMR <- 10 * weight + 6.25 * height - 5 * age + 5
  } else {
    BMR <- 10 * weight + 6.25 * height - 5 * age - 161
  }
  return(BMR)
}

calculateWeight <- function(calories, height, age, male = TRUE){
  weight <- 0
  if(male) {
    weight <-  ((((6.25 * height) - (5 * age) + 5) - calories) * -1)/10
  } else {
    weight <-  ((((6.25 * height) - (5 * age) - 161) - calories) * -1)/10
  }
  return(weight)
}

# 1 BMR (1)
# 2 Sedentary - Little or no exercise (1.2)
# 3 Lightly Active - Exercise/sports 1-3 times/week (1.375)
# 4 Moderately Active - Exercise/sports 3-5 times/week (1.55)
# 5 Very Active - Hard Exercise/sports 6-7 times/week (1.725)
# 6 Extra Active - Very hard exercise/sports or physical job (1.95)
calculateActivity <- function (BMR, option = 1){
  switch(option,
         BMR * 1,
         BMR * 1.2,
         BMR * 1.375,
         BMR * 1.55,
         BMR * 1.725,
         BMR * 1.95)
} 

activityBMR <- function(weight, height, age, male = TRUE, activity = 1){
  BMR <- 0
  if(male) {
    BMR <- 10 * weight + 6.25 * height - 5 * age + 5
  } else {
    BMR <- 10 * weight + 6.25 * height - 5 * age - 161
  }
  BMR <- switch(activity, BMR * 1,BMR * 1.2,BMR * 1.375,BMR * 1.55,BMR * 1.725,BMR * 1.95)
  return(BMR)
}

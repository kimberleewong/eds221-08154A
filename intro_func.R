# for loops in for loops
file_prefix <- c("temp", "ph", "salinity")
file_suffix <- c(1, 2, 3, 4)

for (i in 1:length(file_prefix)) {
  for(j in 1:length(file_suffix)) {
   print(paste0(file_prefix[i], " ", file_suffix[j])) 
  }
}

odds <- c(1, 3, 5)
evens <- c(2, 4, 6, 8)

for(i in seq_along(odds)) {
  for (j in seq_along(evens)) {
    print(odds[i] * evens[j])
  }
}

# making functions

birddog_sum <- function(bird, dog) {
  pets <- bird + dog
  return(pets)
}

x <- birddog_sum(bird = 2, dog = 4)

double_it <- function(x) {
  print(2*x)
}
double_it(x = 10)
double_it(1:4)

exclaim_age <- function(age) {
  print(paste("I am", age, "years old!"))
}
exclaim_age(29)

find_max <- function(val1, val2) {
  if (val1 > val2) {
    return(val1)
  } else if (val2 > val1) {
    return(val2)
  }
}

find_max(32,23)


quarter_splits <- c(1.0, 1.1, 1.2, 1.1, 1.4, 1.5, 1.6, 1.4)

for (i in 1:length(quarter_splits)) {
  print(quarter_splits[i] + quarter_splits[i + 1])
}


animal_age <- function(animal, age) {
  if (animal == "dog") {
    print(age * 7)
  } else if (animal == "goat"){
    print(age * 4.7)
  }
}


animal_age("dog", 3)

dog_choice <- data.frame(dog_name = c("khora",
                                      "teddy",
                                      "waffle",
                                      "banjo"),
                         food = c("everything",
                                  "salmon",
                                  "pancakes",
                                  "chicken"))

library(tidyverse)

dog_menu <- function(name) {
  my_sub <- dog_choice |>
  dplyr::filter(dog_name == name)
print(paste("My name is", my_sub$dog_name, "and I like to eat", my_sub$food))
}

dog_menu("banjo")

animal_age <- function(animal, age) {
  if (animal == "dog") {
    print(age * 7)
  } else if (animal == "goat") {
    print(age * 4.7)
  }
}

animal_age_stop <- function(animal, age) {
  
  if (!animal %in% c("dog", "goat")) {
    stop("Oops! Animal must be a dog or a goat.")
  }
  
  if (is.numeric(age) == FALSE) {
    stop("The age must be a number between 0 and 100")
  }
  
  if (age <= 0) {
    stop("Age must be a number greater than zero.")
  }
  
  if (animal == "dog") {
    print(age * 7)
  } else if (animal == "goat") {
    print(age * 4.7)
  }
}

animal_age_stop("goat", 2)

calc_windpower <- function(rho, radius, windspeed) {
  
  if(windspeed > 130) {
    warning("wow, that's fast! are you sure?")
  }
  
  if(rho > 1.255) {
    warning("that air density is sus, u sure?")
  }
  
  if(radius < 0) {
    stop("rotor radius must be a positive value(meters).")
  }
  
  print(0.3*rho*pi*(radius^2)*(windspeed^3))
}

calc_windpower(rho = 1.2, radius = -1, windspeed = 130)











# B4 

# Punctul a)

initial_users = 10000
target_users = 15000
n = 1000  
p = 0.25   
q = 0.01  

simulate_year = function(current_users) {
  new_users = rbinom(1, n, p)
  remaining_users = sum(rbinom(current_users, 1, 1 - q))
  return(remaining_users + new_users)
}

simulate_until_target = function() {
  current_users = initial_users
  years = 0
  while (current_users < target_users) {
    current_users = simulate_year(current_users)
    years = years + 1
  }
  return(years)
}

set.seed(123)
num_simulations = 1000
years_to_target = replicate(num_simulations, simulate_until_target())
mean_years = mean(years_to_target)
print(mean_years)
#Afiseaza 40.86 dar afiseaza cu intarziere  


# Punctul b) 
years = 40        
months = 10
n_trials = 1000

simulate_growth2 = function(n_initial, years, months, n, p, q) {
  n_current = n_initial
  total_months = years * 12 + months
  for (i in 1:total_months) {
    n_current = simulate_year2(n_current, n, p, q)
  }
  return(n_current)
  
}
simulate_year2 = function(n_current, n, p, q) {
  n_new = rbinom(1, n, p)           
  n_remaining = rbinom(1, n_current, 1 - q) 
  return(n_remaining + n_new)
}

results = replicate(n_trials, simulate_growth2(initial_users, years, months, initial_users, p, q))
probability = mean(results >= target_users)

print(probability)



# Punctul c)

required_simulations = function(prob_estimate, error_margin, confidence_level) {
  z = qnorm((1 + confidence_level) / 2)
  return(ceiling((z^2 * prob_estimate * (1 - prob_estimate)) / (error_margin^2)))
}


initial_simulations = 1000
results = replicate(initial_simulations, simulate_growth2(initial_users, years, months, n, p, q))
prob_estimate = mean(results >= target_users)


error_margin = 0.01
confidence_level = 0.99
num_simulations = required_simulations(prob_estimate, error_margin, confidence_level)


results = replicate(num_simulations, simulate_growth2(initial_users, years, months, n, p, q))
final_prob_estimate = mean(results >= target_users)


z = qnorm((1 + confidence_level) / 2)
margin_of_error = z * sqrt(final_prob_estimate * (1 - final_prob_estimate) / num_simulations)

print(paste("Estimated probability:", final_prob_estimate))
print(paste("Margin of error:", margin_of_error))
print(paste("Number of simulations:", num_simulations))





# B2

a = 0
b = 2
c = 0
d = 4

is_inside_triangle = function(x, y) {
  return(y >= 0 & y <= 2*x & y <= 6 - 3*x)
}

estimate_triangle_area = function(a, b, c, d, n_samples) {
  count_inside = 0
  
  for (i in 1:n_samples) {
    x = runif(1, a, b)
    y = runif(1, c, d)
    
    if (is_inside_triangle(x, y)) {
      count_inside = count_inside + 1
    }
  }
  
  area_box = (b - a) * (d - c)
  estimated_area = (count_inside / n_samples) * area_box
  return(estimated_area)
}

n_samples = 20000

estimated_area = estimate_triangle_area(a, b, c, d, n_samples)

print(paste("Estimated area of the triangle:", estimated_area))

exact_area = 0.5 * (2 * 4)
print(paste("Exact area of the triangle:", exact_area))

relative_error = abs(estimated_area - exact_area) / exact_area
print(paste("Relative error:", relative_error))



# B3

# Punctul a)
integral_a = function(x) {
  (2 * x - 1) / (x^2 - x - 6)
}

result_a = integrate(integral_a, lower = -1, upper = 1)
exact_value_a = log(3) - log(2)
cat("Integral (a) estimated value:", result_a$value, "\n")
cat("Integral (a) exact value:", exact_value_a, "\n")
cat("Integral (a) absolute error:", abs(result_a$value - exact_value_a), "\n\n")


# Punctul b)
integral_b = function(x) {
  (x + 4) / sqrt(3 * x - 3)
}

result_b = integrate(integral_b, lower = 3 + .Machine$double.eps, upper = 11)
exact_value_b = 61.2
cat("Integral (b) estimated value:", result_b$value, "\n")
cat("Integral (b) exact value:", exact_value_b, "\n")
cat("Integral (b) absolute error:", abs(result_b$value - exact_value_b), "\n\n")


# Punctul c)
integral_c = function(x) {
  x * exp(-x^2)
}

result_c = integrate(integral_c, lower = 0, upper = Inf)
exact_value_c = 0.5
cat("Integral (c) estimated value:", result_c$value, "\n")
cat("Integral (c) exact value:", exact_value_c, "\n")
cat("Integral (c) absolute error:", abs(result_c$value - exact_value_c), "\n")




# B1

set.seed(123) 

is_inside_torus = function(x, y, z, R, r) {
  return((z^2 + ((sqrt(x^2 + y^2) - R)^2)) < r^2)
}

estimate_torus_volume = function(R, r, sample_size) {
  x = runif(sample_size, min = -R-r, max = R+r)
  y = runif(sample_size, min = -R-r, max = R+r)
  z = runif(sample_size, min = -r, max = r)
  
  points_inside = sum(is_inside_torus(x, y, z, R, r))
  volume_bounding_box = (2*(R+r))^2 * 2*r
  estimated_volume = (points_inside / sample_size) * volume_bounding_box
  
  return(estimated_volume)
}

R = 10
r = 3
sample_sizes = c(10000, 20000, 50000)
exact_volume = 2 * pi^2 * R * r^2
results = data.frame(SampleSize = sample_sizes, EstimatedVolume = NA, RelativeError = NA)

for (i in 1:length(sample_sizes)) {
  estimated_volume = estimate_torus_volume(R, r, sample_sizes[i])
  relative_error = abs(estimated_volume - exact_volume) / exact_volume
  results$EstimatedVolume[i] = estimated_volume
  results$RelativeError[i] = relative_error
}

print(results)
print(paste("Exact Volume:", exact_volume))






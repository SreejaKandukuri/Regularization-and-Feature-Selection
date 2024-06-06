#GAME ATTENDANCE 
alpha1 <- 0.05
degrees_of_freedom1 <- 19

critical_value1 <- qt(alpha/2, df = degrees_of_freedom1)
critical_value1
data1 <- c(6210, 3150, 2700, 3012, 4875, 3540, 6127, 2581, 2642, 2573, 2792, 2800, 2500, 3700, 6030, 5437, 2758, 3490, 2851, 2720)

sample_median1 <- median(data1)

sample_median1
if (sample_median1 < -critical_value1 || sample_median1 > critical_value1) {
  decision1 <- "Reject"
} else {
  decision1 <- "Fail to reject"
}
decision1

#LOTTERY TICKET SALES 
alpha2 <- 0.05
degrees_of_freedom2 <- 39

critical_value2 <- qt(alpha2, df = degrees_of_freedom2)
critical_value2
test_value2 <- 15

if (test_value2 < critical_value2) {
  decision2 <- "Reject"
} else {
  decision2 <- "Fail to reject"
}
decision2

#LIFE OF PRISON SENTENCE : 
alpha3 <- 0.05
degrees_of_freedom3 <- 8

critical_value3 <- qt(alpha3/2, df = degrees_of_freedom3)
critical_value3
males <- c(8, 12, 6, 14, 22, 27, 32, 24, 26, 19, 15, 13)
females <- c(7, 5, 2, 3, 21, 26, 30, 9, 4, 17, 23, 12, 11, 16)

test_result3 <- wilcox.test(males, females, exact = FALSE)
test_value3 <- test_result3$statistic
p_value3 <- test_result3$p.value

critical_value3 <- qt(0.025, df = length(males) + length(females) - 2)  # Two-sided test

if (abs(test_value3) > critical_value3) {
  decision3 <- "Reject"
} else {
  decision3 <- "Fail to reject"
}

decision3
p_value3
test_value3
#WINNING BASEBALL GAMES 
# Data values for NL and AL Eastern Divisions
nl_wins <- c(89, 96, 88, 101, 90, 91, 92, 96, 108, 100, 95)
al_wins <- c(108, 86, 91, 97, 100, 102, 95, 104, 95, 89, 88, 101)

# Step 2: Find the critical value
alpha <- 0.05
degrees_of_freedom <- length(nl_wins) + length(al_wins) - 1

critical_value <- qnorm(alpha/2)
critical_value

# Step 3: Compute the test value
test_result <- wilcox.test(nl_wins, al_wins, exact = FALSE)
test_value <- test_result$statistic
test_value
p_value <- test_result$p.value

# Step 4: Make the decision
if (abs(test_value) > critical_value) {
  decision <- "Reject"
} else {
  decision <- "Fail to reject"
}

# Step 5: Summarize the results
decision
p_value
test_value
#TABLE K 
# Case 1
ws1 <- 13
n1 <- 15
alpha1 <- 0.01
critical_value1 <- qsignrank(alpha1/2, n1)
decision1 <- ifelse(abs(ws1) > critical_value1, "Reject", "Fail to reject")

# Case 2
ws2 <- 32
n2 <- 28
alpha2 <- 0.025
critical_value2 <- qsignrank(alpha2, n2)
decision2 <- ifelse(ws2 > critical_value2, "Reject", "Fail to reject")

# Case 3
ws3 <- 65
n3 <- 20
alpha3 <- 0.05
critical_value3 <- qsignrank(alpha3, n3)
decision3 <- ifelse(ws3 > critical_value3, "Reject", "Fail to reject")

# Case 4
ws4 <- 22
n4 <- 14
alpha4 <- 0.10
critical_value4 <- qsignrank(alpha4/2, n4)
decision4 <- ifelse(abs(ws4) > critical_value4, "Reject", "Fail to reject")

# Print decisions
print(decision1)
print(decision2)
print(decision3)
print(decision4)

#MATHEMATICS LITERACY SCORE 
# Data
western <- c(527, 406, 474, 381, 411)
europe <- c(520, 510, 513, 548, 496)
eastern <- c(523, 547, 547, 391, 549)

# Kruskal-Wallis test
test_result5 <- kruskal.test(list(western, europe, eastern))
test_result5
# Test value (chi-square statistic)
test_value5 <- test_result5$statistic
test_value5


#SPEARMAN RANK CORRELATION COEFFICIENT 
#SUBWAY AND COMMUTER RAIL PASSENGERS 
city <- c(1, 2, 3, 4, 5, 6)
subway <- c(845, 494, 425, 313, 108, 41)
rail <- c(39, 291, 142, 103, 33, 38)

correlation <- cor.test(subway, rail, method = "spearman")
correlation
correlation_coefficient <- correlation$estimate
correlation_coefficient

#RANDOM NUMBER GENERATOR
#PRIZES IN CARAMEL CORN BOXES 
# Set the number of simulations
num_simulations <- 40

# Initialize a vector to store the number of boxes needed in each simulation
boxes_needed <- numeric(num_simulations)

# Perform the simulations
for (i in 1:num_simulations) {
  prizes <- c("prize1", "prize2", "prize3", "prize4")
  boxes_bought <- 0
  prizes_found <- c()
  
  while (length(prizes_found) < length(prizes)) {
    boxes_bought <- boxes_bought + 1
    random_prize <- sample(prizes, 1)
    if (!(random_prize %in% prizes_found)) {
      prizes_found <- c(prizes_found, random_prize)
    }
  }
  
  boxes_needed[i] <- boxes_bought
}

# Calculate the average number of boxes needed
average_boxes_needed <- mean(boxes_needed)

# Print the result
print(average_boxes_needed)

#LOTTERY WINNERS 
# Given probabilities
p_b <- 0.60
p_i <- 0.30
p_g <- 0.10

# Initialize variables
expected_b <- 0
expected_i <- 0
expected_g <- 0

# Calculate expected value for each letter
for (trials in 1:30) {  # Assuming up to 30 trials
  expected_b <- expected_b + trials * p_b * (1 - p_b)^(trials - 1)
  expected_i <- expected_i + trials * p_i * (1 - p_i)^(trials - 1)
  expected_g <- expected_g + trials * p_g * (1 - p_g)^(trials - 1)
}

# Calculate the total expected value
average_tickets_needed <- expected_b + expected_i + expected_g

# Print the result
print(average_tickets_needed)



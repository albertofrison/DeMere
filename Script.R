# This simple project aims to explain the De Méré Paradox by simple the simple computation and "brute force" demonstration through R.
# The What is the most probable between the following two bets?
# 1) Obtaining at least one 6, by throwing one die four times, or
# 2) Obtaining at least one double 6, by throwing a couple of dice 24 times
# Differently from one could imagine, the two probabilities differ with 1) being more probable than 2).
# This project tries to explain the problem, mathematically (theoretical part) and by conducting a large number of experiments (calculated)
options (digits = 4) # .0001
library (ggplot2)

# Theoretically the two bets have the following probabilities:
bets_theoretical <- c(0,0)
# Bet 1)
bets_theoretical[1] <- 1 - (5/6)^4 #ONE (100%) less the probability of NOT getting a 6 each and every of the 4 times
# Bet 2)
bets_theoretical[2] <- 1 - (35/36)^24 #ONE (100%) less the probability of NOT getting a double 6 each and every of the 24 times

# In order to "prove" the above we can try to simulate a large number of bets
die <- c(1:6) # die variable, contaning 6 faces 1 to 6
trials <- 50000 #number of trials (the more the better but also slower)

# Simulating <trials> times bet 1)
bets_calculated <- c(0,0)
bets_calculated[1] <- sum(replicate(trials ,sum(replicate(4 ,sample (die, 1, replace = TRUE) == 6))>=1))/trials 
# Simulating <trials> times bet 2)
bets_calculated[2] <- sum(replicate(trials ,sum(replicate(24,(sample(die, 1, replace = TRUE) == 6) & (sample(die, 1, replace = TRUE)== 6)))>=1))/trials 

# Creating data frame for ggplot plotting
bets_chart <- data.frame (bet_number = c("One 6","Two double 6","One 6","Two double 6"),  experiment = c("50k Simulations","50k Simulations", "Theoretical", "Theoretical"), result = c(bets_calculated, bets_theoretical))

# Plot into GGplot
ggplot (bets_chart, aes (x= experiment, y=result, fill = experiment, label = round(result,4))) +
geom_bar(stat = "identity") +
facet_grid (bet_number ~ .) +
geom_text() +
ggtitle ("De Mere Paradox")

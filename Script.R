#This simple project aims to explain the De Méré Paradox by simple the simple computation and "brute force" demonstration through R.
#The What is the most probable between the following two bets?
#1) Obtaining at least one 6, by throwing one die four times
#2) Obtaining at least one double 6, by throwing a couple of dice 24 times
options (digits = 4) # .0001
library (ggplot2)

#theoretically the two bets have the following probabilities:
bets_theoretical <- c(0,0)
# Bet 1)
bets_theoretical[1] <- 1 - (5/6)^4 #ONE (100%) less the probability of NOT getting a 6 each and every of the 4 times
# Bet 2)
bets_theoretical[2] <- 1 - (35/36)^24 #ONE (100%) less the probability of NOT getting a double 6 each and every of the 24 times

#in order to "prove" the above we can try to simulate a large number of bets
die <- c(1:6) # die variable, contaning 6 faces 1 to 6
trials <- 50000 #number of trials (the more the better but also slower)

# Simulating <trials> times bet 1)
bets_calculated <- c(0,0)

bets_calculated[1] <- sum(replicate(trials ,sum(replicate(4 ,sample (die, 1, replace = TRUE) == 6))>=1))/trials 

# Simulating <trials> times bet 2)
bets_calculated[2] <- sum(replicate(trials ,sum(replicate(24,(sample(die, 1, replace = TRUE) == 6) & (sample(die, 1, replace = TRUE)== 6)))>=1))/trials 

bets_chart <- data.frame (bet_number = c(1,2,1,2),  experiment = c("calculated","calculated", "theoretical", "theoretical"), result = c(bets_calculated, bets_theoretical))

bets_chart



#barplot(bets_chart$result , main ="De Méré Paradox", xlab = "Probability", col = c("darkblue", "red"), legend #= bets_chart$bet_number, beside = TRUE)

#ggplot (bets_chart, aes (x= experiment, y=result, fill = experiment, label = result))+
#geom_bar(stat = "identity") +
#geom_point() +
#facet_grid (bet_number ~ .) +
#geom_text() +
#ggtitle ("De Mere Paradox")


# Poisson Process

k=4 # column number

plot(m.freqstation[,k], type='l')

lambda1 = mean(m.freqstation[,k]) # arrive's lambda
lambda2 = mean(m.end_freqstation[,k]) # departure's lambda

arrive <- dpois(0:25, lambda2*4)
departure <- dpois(0:25, lambda1*4)

plot(departure, type='l', ylim=c(0,max(arrive, departure)),
     main=paste('Poisson Process for each hour of', colnames(m.freqstation)[k], 'station'), 
     xlab='Qtd', ylab='Prob',
     sub='Red: arrive | Black: departure')+
  lines(arrive, col='red')


ppois(0, lambda2*4) * (1-ppois(0,lambda1)) # Probability that no one arrives in one hour and at least one leaves in 15 minutes 



  # Interarrival times



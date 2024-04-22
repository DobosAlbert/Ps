sample=read.csv("life_expect.csv",header=T)
male=sample[['male']]
female=sample[['female']]

max=max(male)
min=min(male)
b=seq(min,max,(max-min)/7)
hist(male,breaks = b,right=F)


max=max(female)
min=min(female)
b=seq(min,max,(max-min)/7)

hist(female,breaks = b,right=F)

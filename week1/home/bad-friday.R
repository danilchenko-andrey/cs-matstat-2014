setwd('~/workspace/cs-matstat/')

#download.file(url='http://beta.compscicenter.ru/media/homework_supplement/82/13_6', destfile='data/week1-home.tsv')

read.csv('data/week1-home.tsv', sep='\t', header=F) -> bad.friday
names(bad.friday) <- c('shopping.type', 'date', 'customers.6', 'customers.13', 'shop')
# shopping.type is unused

# we can build additional column for difference and work with it
bad.friday$diff <- bad.friday$customers.6 - bad.friday$customers.13

# getting summary
summary(bad.friday)

# customers.6 and customers.13 are about the same range
max(bad.friday$customers.6) - min(bad.friday$customers.6)
max(bad.friday$customers.13) - min(bad.friday$customers.13)

# customers.13 mean is greater than mean of customers.6
# but median of customers.6 is greater than median of customers.13
# so we can suppose that customers.* distributions are not normal

# and so we see:
h.13 <- hist(bad.friday$customers.13, col=rgb(1,0,0,0.3), main='Histogram of customers count', xlab='Count')
h.6  <- hist(bad.friday$customers.6, col=rgb(0,1,1,0.3), add=T)
lines(h.6$counts~h.6$mids, col='blue')
lines(h.13$counts~h.13$mids, col='red')
legend('topright', c('6','13'), col=c('blue','red'), pch=15)

# but histogram of difference looks like good normal distribution with a little negative skew
h <- hist(bad.friday$diff, breaks=10, main='Histogram of diffs 6-13', xlab='Diff')

# Let's check is there any difference between years?
boxplot(bad.friday$diff~bad.friday$date)
abline(h=median(bad.friday$diff), col=2, lwd=2)
# now we see that 1990, July looks different
# this year can cause skew of histogram

filtered <- bad.friday[bad.friday$date != '1990, July',]
hist(filtered$diff, breaks=10, main='Histogram of diffs 6-13 (filtered by date)', xlab='Diff')
# and now histogram looks much like normal distribution except some outliers on the left

# Now let's check if there is any difference between shops
boxplot(bad.friday$diff~bad.friday$shop)
# Oh, Lewisham looks really bad!

filtered2 <- bad.friday[bad.friday$shop != 'Lewisham',]
hist(filtered2$diff, main='Histogram of diffs 6-13 (filtered)', xlab='Diff')
# and now it's pretty good

# let's check what is mean difference
mean(filtered2$diff)
# not big in compare with standard deviation
sd(filtered2$diff)
mean(filtered2$diff)/sd(filtered2$diff)

# so I suppose that there's no difference in number of customers. 
# So 13 is not such a bad day =)
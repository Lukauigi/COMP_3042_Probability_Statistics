#make this example reproducible
set.seed(0)

#define number of samples
n = 10000

#create empty vector of length n
sample_means = rep(NA, n)

#fill empty vector with means
for(i in 1:n){
  sample_means[i] = mean(rnorm(20, mean=5.3, sd=9))
}

#view first six sample means
head(sample_means)

#create histogram to visualize the sampling distribution
hist(sample_means, main = "", xlab = "Sample Means", col = "steelblue")

#mean of sampling distribution
mean(sample_means)

#standard deviation of sampling distribution
sd(sample_means)

#calculate probability that sample mean is less than or equal to 6
sum(sample_means <= 6) / length(sample_means)

library(lattice)
library(ggplot2)

gg = ggplot(singer, aes(x = height))
gg = gg + stat_ecdf()
gg = gg + labs(title = "NY Choral Society heights")
gg = gg + xlab("Height (in)") + ylab("Empirical CDF")
gg

gg = ggplot(singer, aes(x = height))
gg = gg + geom_density()
gg = gg + labs(title = "NY Choral Society heights")
gg = gg + xlab("Height (in)") + ylab("Estimated PDF")
gg

gg = ggplot(singer, aes(x = height))
gg = gg + geom_histogram(binwidth = 1)
gg = gg + labs(title = "NY Choral Society heights")
gg = gg + xlab("Height (in)") + ylab("Number of singers")
gg

gg = ggplot(singer, aes(sample = height))
gg = gg + stat_qq()
gg = gg + labs(title = "Normal QQ plot of singer heights")
gg

####GOOD SECTION ON SUBSETTING AND MERGING DATA
library(NHANES)
library(dplyr)
dd = subset(NHANES, Age >= 20 & Age <= 79)
dd = dd[c("AgeDecade", "Smoke100")]
cc = dd %>% group_by(AgeDecade, Smoke100) %>%
  summarize(count=n())
ee = dd %>% group_by(AgeDecade) %>%
  summarize(total=n())
ff = merge(cc, ee)
ff$Proportion = ff$count / ff$total
ff = ff[c("AgeDecade", "Smoke100", "Proportion")]
gg = ggplot(ff, aes(x = Smoke100, y = Proportion))
gg = gg + geom_bar(stat = "identity")
gg = gg + facet_wrap(~AgeDecade)
gg = gg + xlab("Smoke100") + ylab("Proportion")
gg

library(MASS)
ggplot(geyser, aes(x = waiting, y = duration)) + geom_point() + stat_density_2d() + coord_flip()
ggplot(geyser, aes(x = duration, y = waiting)) + geom_point() + stat_density_2d()


# Type 1
# Null true but rejected:
#   prob of test stat falling in the rejection region when null is true
# 
# Type 2
# Null false, but retained

#A test's power is the probability of correctly rejecting the null hypothesis when it is false;

#The significance level ?? is the probability of making the wrong decision when the null hypothesis is true. 

#prob of getting 

library(reshape2)
x <-c(1,2,3)
y <-c(100,200,300)
x_name <- "cond"
y_name <- "rating"

df <- melt(data.frame(x,y))
colnames(df) <- c(x_name, y_name)
print(df)


espr = c(15.8, 3.3, 4.1, 3.0, 12.7, 3.2)
brew = c(12.0, 12.5, 13.0, 13.4, 13.4, 13.0)

x_name <- "group"
y_name <- "caff"

caff.df <- melt(data.frame(espr,brew))
colnames(caff.df) <- c(x_name, y_name)

diff.means = function(x, y) {
  return(mean(x) - mean(y))
}

perm.diff.means = function(x, y) {
  n1 = length(x)
  n2 = length(y)
  z = sample(c(x, y))
  perm.x = z[1:n1]
  perm.y = z[(n1 + 1):(n1 + n2)]
  return(diff.means(perm.x, perm.y))
}

perm.test.diff.means = function(x, y, nperm = 10000) {
  test.stat = diff.means(x, y)
  perm.dist = replicate(nperm, perm.diff.means(x, y))
  return(sum(abs(perm.dist) >= abs(test.stat))/nperm)
}

diff.means(espr, brew)
perm.test.diff.means(espr, brew, nperm = 100000)

oneway_test(caff ~ group, data = caff.df, dist = "exact")

library(gtools)
x = c(1, 2, 3, 5, 4, 6, 9)
vec1.strt = 1
vec1.end = 4
vec2.strt = 5
vec2.end = 7

perm = permutations(n=length(x), r=length(x), v=x)

perm.unique = perm[0,]

for(perm.row in 1:nrow(perm)) {
  if(perm.row == 1) {
    perm.unique = rbind(perm.unique, perm[perm.row,])
  } else {
    flag = FALSE
    #remove duplicate permutations
    for(perm.unique.row in 1:nrow(perm.unique)) {
      if ((all(sort(perm[perm.row, vec1.strt:vec1.end]) == sort(perm.unique[perm.unique.row, vec1.strt:vec1.end]))) && +
        (all(sort(perm[perm.row, vec2.strt:vec2.end]) == sort(perm.unique[perm.unique.row, vec2.strt:vec2.end])))) {
        break
      } else {
        flag = TRUE
      }
    }
    if(flag) {
      perm.unique = rbind(perm.unique, perm[perm.row,])
    }
  }
}

perm.unique

View(perm)

sort(perm[8, 5:7])
sort(perm.unique[2, 5:7])

if(all(sort(perm[4, 1:4]) == sort(perm.unique[2, 1:4]))) {
  TRUE
} else {FALSE}





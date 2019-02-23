library(gtools)
xy = c(1, 2, 3, 5, 4, 6, 9)
vec1.strt = 1
vec1.end = 4
vec2.strt = 5
vec2.end = 7

#Gets all possible permutations of order
perm = permutations(n=length(xy), r=length(xy), v=xy)

perm.unique = perm[0,]

#Captures only hhe unique permutations for the two distinct vectors
for(perm.row in 1:nrow(perm)) {
  if(perm.row == 1) {
    perm.unique = rbind(perm.unique, perm[perm.row,])
  } else {
    flag = FALSE
    #remove duplicate permutations
    for(perm.unique.row in 1:nrow(perm.unique)) {
      if ((all(sort(perm[perm.row, vec1.strt:vec1.end]) == sort(perm.unique[perm.unique.row, vec1.strt:vec1.end]))) && +
          (all(sort(perm[perm.row, vec2.strt:vec2.end]) == sort(perm.unique[perm.unique.row, vec2.strt:vec2.end])))) {
        flag = FALSE
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

for(perm.unique.row in 1:nrow(perm.unique)) {
  if(perm.unique.row == 1) {
    max_diff = c(max(perm.unique[perm.unique.row, vec1.strt:vec1.end]) - max(perm.unique[perm.unique.row, vec2.strt:vec2.end]))
  } else {
    max_diff = c(max_diff, max(perm.unique[perm.unique.row, vec1.strt:vec1.end]) - max(perm.unique[perm.unique.row, vec2.strt:vec2.end]))
  }
}

max_diff


caff = c(1,2,3,4,5,6,7)
group = c("group1","group1","group1","group1","group1","group2","group2")

caff.df <- data.frame(caff, group)


caff.df

oneway_test(caff ~ group, data = caff.df, dist = "exact")

espr = c(15.8, 3.3, 4.1, 3.0, 12.7, 3.2)
brew = c(12.0, 12.5, 13.0, 13.4, 13.4, 13.0)

x_name <- "group"
y_name <- "caff"

caff.df <- melt(data.frame(espr,brew))
colnames(caff.df) <- c(x_name, y_name)

caff.df

oneway_test(caff ~ group, data = caff.df, dist = "exact")


caff = c(21.3, 82.8, 72.7, 86.5, 88.6, 84.9, 88.4, 95.8, 92.8, 98.6)
group = c("espr", "espr", "espr", "espr", "brew", "brew", "brew", "brew", "brew", "brew")

caff.df <- data.frame(caff, group)

#order by caff amount
caff.df = caff.df[order(caff.df$caff),]

caff.groups = caff.df$group
ranks = 1:10
caff.groups = factor(caff.groups)

wilcox_test(ranks ~ caff.groups, dist = "exact")

install.packages('ggplot2')
install.packages('lattice')
install.packages('gapminder')
install.packages('GGally')
install.packages('NHANES')
install.packages('broom')
install.packages('BSDA')
install.packages('coin')
install.packages('reshape2')
install.packages('gtools')
install.packages('dplyr')
install.packages('data.table')
install.packages('alr4')

set.seed(681)
x1 = rnorm(10000)
x2 = 3 * x1 + rnorm(10000)
y = -8 * x1 + 6 * x2 + rnorm(10000)

sim.data = data.frame(y, x1, x2)
model1 = lm(y ~ x1, data = sim.data)

summary(model1)

WT91 = 50
WT21 = 25
WT181 = 34.26 - .97 * WT21 + 1.2 * WT91


WT92 = 51
WT22 = 25
WT182 = 34.26 - .97 * WT22 + 1.2 * WT92

WT182 - WT181

WT18 = 34.26 - .97 * WT2 + 1.2 * WT9










































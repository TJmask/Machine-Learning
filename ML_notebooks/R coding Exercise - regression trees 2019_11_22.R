# install.packages('tree')
rm(list=ls())
library(tree)
library(data.table)

calif     = fread('https://raw.githubusercontent.com/jbryer/CompStats/master/Data/cadata.dat')
treefit   = tree(log(MedianHouseValue) ~ Longitude+Latitude,data=calif)


plot(treefit)
text(treefit,cex=0.75)


price.deciles = quantile(calif$MedianHouseValue,0:10/10)
cut.prices    = cut(calif$MedianHouseValue,price.deciles,include.lowest=TRUE)
plot(x    = calif$Longitude,
     y    = calif$Latitude,
     col  = grey(10:2/11)[cut.prices],
     pch  = 20,
     xlab ="Longitude",
     ylab="Latitude")

partition.tree(tree    = treefit,
               ordvars = c("Longitude","Latitude"),
               add     = TRUE)

treefit3 = tree(log(MedianHouseValue) ~., data=calif)
plot(x    = calif$Longitude,
     y    = calif$Latitude,
     col  = grey(10:2/11)[cut.prices],
     pch  = 20,
     xlab = "Longitude",
     ylab = "Latitude")



plot(treefit3)
text(treefit3,cex=0.5,digits=3)

cut.predictions = cut(predict(treefit3),log(price.deciles),include.lowest=TRUE)
plot(x    = calif$Longitude,
     y    = calif$Latitude,
     col  = grey(10:2/11)[cut.predictions],
     pch  = 20,
     xlab = "Longitude",
     ylab = "Latitude")



my.tree = tree(y ~ x1 + x2, data=my.data) # Fits tree
prune.tree(my.tree,best=5) # Returns best pruned tree with 5 leaves, evaluating
# error on training data
prune.tree(my.tree,best=5,newdata=test.set) # Ditto, but evaluates on test.set
my.tree.seq = prune.tree(my.tree) # Sequence of pruned tree sizes/errors
plot(my.tree.seq) # Plots size vs. error
my.tree.seq$dev # Vector of error rates for prunings, in order
opt.trees = which(my.tree.seq$dev == min(my.tree.seq$dev)) # Positions of
# optimal (with respect to error) trees
min(my.tree.seq$size[opt.trees]) # Size of smallest optimal tree


my.tree.cv = cv.tree(my.tree)


cv.tree(my.tree,best=19)



cv.tree(my.tree)


treefit2.cv <- cv.tree(treefit2)
plot(treefit2.cv)

opt.trees = which(treefit2.cv$dev == min(treefit2.cv$dev))
best.leaves = min(treefit2.cv$size[opt.trees])
treefit2.pruned = prune.tree(treefit2,best=best.leaves)
plot(treefit2.pruned)



plot(calif$Longitude,calif$Latitude,col=grey(10:2/11)[cut.prices],pch=20,
     xlab="Longitude",ylab="Latitude")
partition.tree(treefit2.pruned,ordvars=c("Longitude","Latitude"),
               add=TRUE,cex=0.3)






data(shuttle, package="MASS")
shuttle.tr <- tree(use ~ ., shuttle, subset=1:253, mindev=1e-6, minsize=2)
shuttle.tr
shuttle1 <- shuttle[254:256, ] # 3 missing cases
predict(shuttle.tr, shuttle1)




data(fgl, package="MASS")
fgl.tr <- tree(type ~ ., fgl)
plot(print(fgl.tr))
fgl.cv <- cv.tree(fgl.tr,, prune.tree)
for(i in 2:5) fgl.cv$dev <- fgl.cv$dev +  cv.tree(fgl.tr,, prune.tree)$dev
fgl.cv$dev <- fgl.cv$dev/5
plot(fgl.cv)



data(cpus, package="MASS")
cpus.ltr <- tree(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, cpus)
cpus.ltr
summary(cpus.ltr)
plot(cpus.ltr); text(cpus.ltr)
ir.tr <- tree(Species ~., iris)
ir.tr
summary(ir.tr)




data(fgl, package="MASS")
fgl.tr <- tree(type ~ ., fgl)
summary(fgl.tr)
plot(fgl.tr); text(fgl.tr, all=TRUE, cex=0.5)
fgl.tr1 <- snip.tree(fgl.tr, node=c(108, 31, 26))
tree.screens()
plot(fgl.tr1)
tile.tree(fgl.tr1, fgl$type)
close.screen(all = TRUE)
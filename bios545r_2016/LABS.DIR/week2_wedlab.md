BIOS560R Spring 2014 - LAB Matrices, Factors, Lists
============================================================
Pittard wsp@emory.edu
***

## Working with matrices

Practice 1.0) Let's create some matrices

```r

myvec = c(1:12)

dim(myvec) = c(3, 4)

myvec

mymat = matrix(c(7, 4, 2, 4, 7, 2), nrow = 3, ncol = 2)

mymat
```


***

1.1) You can influence how the matrix is constructed


```r

mymat = matrix(c(7, 4, 2, 4, 7, 2), nrow = 3, ncol = 2, byrow = TRUE)
mymat

```


***

1.2) You can name the rows and columns of a matrix according to a pattern


```r
set.seed(123)

X = matrix(rpois(20, 1.5), nrow = 4)

X

rownames(X) = paste("Trial", 1:nrow(X), sep = ".")

X

colnames(X) = paste("P", 1:ncol(X), sep = ".")

X

```


***

1.2) You can also name columns something more specific


```r

drug.names = c("aspirin", "paracetamol", "nurofen", "hedex", "placebo")

colnames(X) = drug.names

X

X["Trial.1", ]  # Gets all columns for Trial #1

# Get's the nurofen column for Trial.1

X["Trial.1", "nurofen"]

X[, "nurofen"]  # Get all Trials for nurofen

X[, "nurofen", drop = FALSE]  # Preserves matrix structure if desired

```


***

1.3) Indexing into Matrices are usually done numerically


```r

set.seed(123)

X = matrix(rpois(9, 1.5), nrow = 3)

X

X[1, 1]  # First row, First Column

X[2, 2]  # Second row, Second Column

X[3, 3]  # Third row, Third column

diag(X)

X

X[1:2, 1]  # Gets First and second rows and the first column

X[1:2, 2]  # Gets First and second rows and the second column

X[1:2, ]  # Gets First and second rows and ALL columns

```


***

1.4) 


```r

X[1:4]

X >= 2

X[X >= 2]  # Returns which values are greater or equal to 2

which(X >= 2)  # Returns which elements are greater or equal to 2

X%%2 == 0  # Returns a logical matrix 

X[X%%2 == 0]  # Finds the actual element values

X[X%%2 == 0] = 99  # Set all even numbers to 99

```


***

1.5) We can use the row and column functions to find elements that satisfy specific conditions. This approach takes some getting used to it but its better than using a for and if loop combination.


```r
X

row(X)

col(X)

row(X) == col(X)

X[row(X) == col(X)]

X[row(X) == col(X)] = 0  # Put zeroes in the diagonal

```


***

1.6) We can use the cbind and rbind functions to add columns and rows to a matrix. This is necessary when we have several matrices we would like to combine.


```r
set.seed(123)

X = matrix(rpois(9, 1.5), nrow = 3)

colnames(X) = c("aspirin", "paracetamol", "nurofen")

rownames(X) = paste("Trial", 1:3, sep = ".")

rbind(X, Trial.4 = c(4, 7, 5))

X

rowSums(X)

cbind(X, rowsums = rowSums(X))

```


***

1.7) Let's use some functions on a matrix to show that we can extract some meaningful information


```r

set.seed(123)

X = matrix(rpois(9, 1.5), nrow = 3)

colnames(X) = c("aspirin", "paracetamol", "nurofen")

rownames(X) = paste("Trial", 1:3, sep = ".")

X

mean(X[, 3])  # Mean of the 3rd column

var(X[3, ])  # Variance of the 3rd row

X

rowSums(X)

colSums(X)

rowMeans(X)

colMeans(X)

colMeans(X)[3]

```


***

1.8) We can also do some cool linear algebra operations


```r

A = matrix(c(1, 3, 2, 2, 8, 9), 3, 2)

t(A)

A

B = matrix(c(5, 8, 4, 2), 2, 2)

A %*% B

A = matrix(1:4, 2, 2)

A

B = solve(A)

B

A %*% B  # We get the identity matrix

A

b = c(7, 10)

x = solve(A) %*% b

x

# Slide 25

B

diag(B)  # Fetches the diagonal


diag(c(1, 2, 3))  # Creates a matrix with 1,2,3 on the diagonal


diag(1, 4)  # Creates a 4 x 4 Indentity matrix

```


***

1.9) Get used to the apply function. It does some cool things


```r
set.seed(123)

X = matrix(rpois(9, 1.5), nrow = 3)

colnames(X) = c("aspirin", "paracetamol", "nurofen")

rownames(X) = paste("Trial", 1:3, sep = ".")

apply(X, 2, function(var) var/sum(var))

apply(X, 1, function(var) var/sum(var))

# Find the distance between each element and the mean of its respective
# column

apply(X, 2, function(x) x - mean(x))

```


## Working with Factors

Problem 2.0) Factors


```r

transvec = c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 
    0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1)

table(transvec)  # Count 'em up. Which are Auto and Manual ?

mytransfac = factor(transvec, levels = c(0, 1), labels = c("Auto", "Man"))

table(mytransfac)

levels(mytransfac)

mytransfac

library(lattice)

xyplot(mpg ~ wt | mytransfac, mtcars, main = "MPG vs Weight - Auto and Manual Transmissions")

bwplot(~mpg | mytransfac, mtcars, main = "MPG - Auto and Manual Transmissions", 
    layout = c(1, 2))

```


***

2.1) We can cut up continuous quantities into discrete categories and, ultimately, factors


```r
mympg = c(21, 21, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19.2, 17.8, 16.4, 
    17.3, 15.2, 10.4, 10.4, 14.7, 32.4, 30.4, 33.9, 21.5, 15.5, 15.2, 13.3, 
    19.2, 27.3, 26, 30.4, 15.8, 19.7, 15, 21.4)

tapply(mympg, mytransfac, mean)

# Slide 38

cut(0:10, breaks = 4)

table(cut(0:10, breaks = 4))

# Slide 39

my.cut = cut(0:10, breaks = 4, labels = c("Q1", "Q2", "Q3", "Q4"))

table(my.cut)

```


***

2.2) More on cutting continuous quantities into factors


```r

quantile(0:10)

table(cut(0:10, breaks = quantile(0:10), include.lowest = TRUE))

set.seed(123)

exam.score = runif(25, 50, 100)

cut(exam.score, breaks = c(50, 60, 70, 80, 90, 100))

cut(exam.score, breaks = c(50, 60, 70, 80, 90, 100), labels = c("F", "D", "C", 
    "B", "A"))

my.table = table(cut(exam.score, breaks = c(50, 60, 70, 80, 90, 100), labels = c("F", 
    "D", "C", "B", "A")))

barchart(my.table, main = "Grade BarChart", col = terrain.colors(5))

cut(exam.score, breaks = c(50, 60, 70, 80, 90, 100))

cut(exam.score, breaks = c(50, 60, 70, 80, 90, 100), right = F)

```


***

2.3) If you don't use cut then you have to write for and if loops to do the work


```r
exam.score = runif(25, 50, 100)

acount = 0
bcount = 0
ccount = 0
dcount = 0
fcount = 0
exam.score = runif(25, 50, 100)
for (ii in 1:length(exam.score)) {
    
    if (exam.score[ii] < 60) {
        fcount = fcount + 1
    } else if ((exam.score[ii] >= 60) & (exam.score[ii] < 70)) {
        dcount = dcount + 1
    } else if ((exam.score[ii] >= 70) & (exam.score[ii] < 80)) {
        ccount = ccount + 1
    } else if ((exam.score[ii] >= 80) & (exam.score[ii] < 90)) {
        bcount = bcount + 1
    } else if ((exam.score[ii] >= 90) & (exam.score[ii] <= 100)) {
        acount = acount + 1
    }
}

cat("acount bcount ccount dcount fcount \n")

cat(acount, bcount, ccount, dcount, fcount, "\n")

```


***

2.4) We can also impose an order on the factors we create:


```r

mons = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jan", "Feb", "May", "Jun", 
    "Apr", "Mar")

my.fact.mons = factor(mons)

my.fact.mons[1] < my.fact.mons[2]

levels(my.fact.mons)

my.fact.mons = factor(mons, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun"), 
    ordered = TRUE)

my.fact.mons

my.fact.mons[1] < my.fact.mons[2]

table(my.fact.mons)

levels(my.fact.mons)  # This is what we want !

```


***

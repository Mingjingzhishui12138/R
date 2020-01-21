letters
letters[5]
letters[1:5]
letters[c(1, 3, 5)]
letters[-(1:10)]
head(letters, 5)
tail(letters, 5)

mat
mat[-3, -4]
mat[2:4, 3:5]


colnames(mat) <- paste("col", 1:5, sep="")
rownames(mat) <- paste("row", 1:5, sep="")
mat 



head(mtcars)
mtcars[c('Hornet 4 Drive', 'Datsun 710'), c('mpg', 'cyl')]





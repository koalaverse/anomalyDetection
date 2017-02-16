#' @title Principal Component Analysis

pca <- function(data) {
  data = as.matrix(data)
  N = nrow(data)
  M = ncol(data)
  R = cor(data)
  tmp = eigen(R)
  tmp2 = sort(tmp$values,decreasing = TRUE,index.return = TRUE)
  eigval = tmp2$x
  eigvec = tmp$vectors[,order(tmp2$ix)]

  #loadings matrix
  pca_loadings = matrix(0,M,M)
  for (i in 1:M) {
    pca_loadings[,i] = sqrt(eigval[i]) %*% eigvec[,i]
    next
  }

  #component scores
  xbar = colMeans(data)
  Xd = data - matrix(1,N,1)%*%t(xbar)
  v = diag(M)*diag(1/sqrt(var(data)))
  Xs = Xd %*% v
  Y = Xs %*% eigvec
  pca_scores = Y

  output = list(eigval=eigval,eigvec=eigvec,pca_loadings=pca_loadings,pca_scores=pca_scores)

  return(output)

}

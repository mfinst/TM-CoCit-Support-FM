function(lda_dtm, seed = 62078515652) {
  alpha_array = vector(mode= "double", length = 18)
  gamma_array = vector(mode= "double", length = 18)
  for ( k in 2:20) {
    print(paste('k ', k , ' of 18'))
    tmp_lda = LDA(BI.DTM, k = k, method='Gibbs',
                  control = list(seed = seed))
    alpha_array[k] = slot(tmp_lda, "alpha")
    gamma_array[k] = mean(calculate_gamma_mean(tmp_lda))
  }
  print('now plotting')
  # debugging
  print(alpha_array)
  print(gamma_array)
  # plot
  # alpha
  plot(alpha_array,type="b", main="LDA Alpha Value for Various K Topics",
       ylab="Alpha Value",
       xlab="Value of K")
  # plot gamma mean
  plot(gamma_array,type="b", main="LDA Gamma mean for Various K Topics",
       ylab="Average Gamma",
       xlab="Value of K")
}

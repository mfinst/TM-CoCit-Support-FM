function(distance = NULL) {
  if (is.null(distance)) {
    return(NULL)
  }
  rng = 2:20 #K from 2 to 20
  tries = 100 #Run the K Means algorithm 100 times
  avg.totw.ss = integer(length(rng)) #Set up an empty vector to hold all of points
  for(v in rng) { # For each value of the range variable
    print(paste('test for k = ', v))
    v.totw.ss = integer(tries) #Set up an empty vector to hold the 100 tries
    for(i in 1:tries) {
      k.temp = kmeans(distance,centers=v) #Run kmeans
      v.totw.ss[i] = k.temp$tot.withinss#Store the total withinss
    }
    avg.totw.ss[v-1] = mean(v.totw.ss) #Average the 100 total withinss
    print( paste('test completed for k = ', v))
  }
  plot(rng,avg.totw.ss,type="b", main="Total Within SS by Various K",
       ylab="Average Total Within Sum of Squares",
       xlab="Value of K")
}

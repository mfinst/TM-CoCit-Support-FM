function(datavector = NULL, k = 3, lang = "english", make.plot = FALSE, furtherStops = NULL) {
  # Code Not Mine!
  corpus = corpus_gen(datavector, lang, furtherStops)
  dtm = DocumentTermMatrix(corpus)
  m = as.matrix(dtm)
  v = sort(colSums(m), decreasing=TRUE)
  d = data.frame(word = names(v), freq = v)
  dtm.tfidf = weightTfIdf(dtm)
  dtm.tfidf = removeSparseTerms(dtm.tfidf, 0.999)
  tfidf.matrix = as.matrix(dtm.tfidf)
  dist.matrix = dist(tfidf.matrix, method = "cosine")
  # clustering
  clustering.kmeans = kmeans(tfidf.matrix, k)
  clustering.hierarchical = hclust(dist.matrix, method = "ward.D2")
  clustering.dbscan = dbscan::hdbscan(dist.matrix, minPts = 8)

  master.cluster = clustering.kmeans$cluster
  slave.hierarchical = cutree(clustering.hierarchical, k = k)
  slave.dbscan = clustering.dbscan$cluster
  stacked.clustering = rep(NA, length(master.cluster))

  names(stacked.clustering) = 1:length(master.cluster)

  # Clustering Berechnungen
  for (cluster in unique(master.cluster)) {
    indexes = which(master.cluster == cluster, arr.ind = TRUE)
    slave1.votes = table(slave.hierarchical[indexes])
    slave1.maxcount = names(slave1.votes)[which.max(slave1.votes)]
    slave1.indexes = which(slave.hierarchical == slave1.maxcount, arr.ind = TRUE)
    slave2.votes = table(slave.dbscan[indexes])
    slave2.maxcount = names(slave2.votes)[which.max((slave2.votes))]
    stacked.clustering[indexes] = slave2.maxcount
  }
  if (make.plot) {
    # plots
    points = cmdscale(dist.matrix, k = 2)
    palette = colorspace::diverge_hcl(k) # color palette based on clusters
    previous.par.par = par(mfrow=c(2,2), mar = rep(1.5, 4))
    plot(points, main = 'K-Means Cluster', mai = c(0,0,0,0),
         mar = c(0,0,0,0), col = as.factor(master.cluster),
         xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    plot(points, main = 'Hierachical Cluster', mai = c(0,0,0,0),
         mar = c(0,0,0,0), col = as.factor(slave.hierarchical),
         xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    plot(points, main = 'Density-based Cluster', mai = c(0,0,0,0),
         mar = c(0,0,0,0), col = as.factor(slave.dbscan),
         xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    plot(points, main = 'Stacked Cluster', mai = c(0,0,0,0),
         mar = c(0,0,0,0), col = as.factor(stacked.clustering),
         xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    par(previous.par)
  }
}

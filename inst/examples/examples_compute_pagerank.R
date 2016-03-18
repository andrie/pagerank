MRAN <- "https://mran.revolutionanalytics.com/snapshot/2016-03-15/"
pr <- compute_pagerank(MRAN)
head(pr, 10)

plot_graph(MRAN, 10)
plot_graph(MRAN, 10, cex = 0.8)
plot_graph(MRAN, 25)
plot_graph(MRAN, 50)

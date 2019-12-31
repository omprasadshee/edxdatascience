#############################################Data Loading and Preparation #############

if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")

# SOURCE-from the wall street journal article
url <- "http://online.wsj.com/public/resources/documents/info-Degrees_that_Pay_you_Back-sort.html?mod=article_inline#top"
h <- read_html(url)
nodes <- h %>% html_nodes('table')

# locate table of interest from nodes "xml_nodeset"
tab <- nodes[[7]]
raw_data <- html_table(tab)
head(raw_data)

rm(h, nodes, tab, url)

##########################################Data Manipulation######################################


if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

#Massage the data 
colnames(raw_data) <- c("College.Major", "Starting.Median.Salaries", "Mid.Career.Median.Salaries", "Career.Percent.Growth", "Percentile.10", "Percentile.25", "Percentile.75", "Percentile.90" )
raw_data <- raw_data[-1,]
rownames(raw_data) <- 1:nrow(raw_data)

# Data Cleansing
degrees <- raw_data %>% 
  mutate_at(vars(Starting.Median.Salaries: Percentile.90), function(x) as.numeric(gsub('[\\$,]',"",x))) %>%
  mutate(Career.Percent.Growth = Career.Percent.Growth / 100)

rm(raw_data)

##########################################Exploratory Analysis###############  ############

# Load packages 
 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(cluster)) install.packages("cluster", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")

# Feature Selection for k_means_data
# Elbow method using Median and 10 -90 percentile 

k_means_data <- degrees %>%
  select(Starting.Median.Salaries, Mid.Career.Median.Salaries, Percentile.10, Percentile.90) %>% 
  scale()
#METHOD1
# Execute using WSS Method and fviz_nbclust--ELBOW METHOD 
elbow_method <- fviz_nbclust(k_means_data, FUNcluster = kmeans, method = "wss")
elbow_method + theme_fivethirtyeight() + 
  theme(axis.title = element_text()) + 
  xlab('\nNumber of Clusters k') + 
  ylab('Total Within Sum of Square\n')

#METHOD2
#Execute using the SILHOUETTE Method and the function fviz_nbclust- SILHOUTTE METHOD 
silhouette_method <- fviz_nbclust(k_means_data, FUNcluster = kmeans, method = "silhouette")
silhouette_method + theme_fivethirtyeight() + 
  theme(axis.title = element_text()) + 
  xlab('\nNumber of Clusters k') + 
  ylab('Average Width of Silhouette\n')

#METHOD3

# Execute using GAP STATISTICS Method and clusGap function
gap_stat <- clusGap(k_means_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
# Data Visualization using fviz_gap_stat function
gap_stat_method <- fviz_gap_stat(gap_stat)
gap_stat_method + theme_fivethirtyeight() + 
  theme(axis.title = element_text()) + 
  xlab('\nNumber of clusters k') + 
  ylab('Gap Statistics (k)\n')

# Usage of K-means Clustering  algorithm and a random seed setting 
suppressWarnings(set.seed(111, sample.kind = 'Rounding'))
# k=optimal number of clusters
num_clusters <- 3
# Executing k-means Clustering algorithm 
k_means <- kmeans(k_means_data, centers = num_clusters, iter.max = 15, nstart = 25)
# Labelling of degrees
degrees_labeled <- degrees %>%
  mutate(clusters = k_means$cluster)

# Data Visualisation of Clusters
# Starting and Mid Career Median Salaries Visualisation
career_growth <- ggplot(degrees_labeled, aes(x = Starting.Median.Salaries, y = Mid.Career.Median.Salaries, color=factor(clusters))) + 
  geom_point(alpha = 4/5, size = 7) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) + 
  scale_color_manual(name = "Clusters", values = c("#EC2C73", "#29AEC7", "#FFDD30"))
career_growth + theme_fivethirtyeight() + 
  theme(axis.title = element_text()) + 
  xlab('\nStarting- Median Salaries') + 
  ylab('Mid Career- Median Salaries\n')

#Further detailed Analysis of clusters
# to reshape degrees using gather function and reorder the new percentile column using mutate() function
degrees_perc <- degrees_labeled %>%
  select(College.Major, Percentile.10, Percentile.25, Mid.Career.Median.Salaries, Percentile.75, Percentile.90, clusters) %>%
  gather(key=percentile, value=salaries, -c(College.Major, clusters)) %>%
  mutate(percentile = factor(percentile, levels = c("Percentile.10", "Percentile.25", "Mid.Career.Median.Salaries", "Percentile.75", "Percentile.90")))

# Cluster1
# Cluster 1 Distribution by percentile and plotting of graph
cluster_1 <-  ggplot(degrees_perc %>% filter(clusters == 1), aes(x=percentile, y=salaries, group=College.Major, color=College.Major)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(size=7)) + 
  scale_y_continuous(labels = scales::dollar)
cluster_1 + theme_fivethirtyeight() + labs(color = "College Major") + 
  theme(axis.title = element_text()) + 
  xlab('\nPercentile') + 
  ylab('Salaries\n')

# Cluster2
# Cluster 2 Distribution by percentile and plotting of graph
cluster_2 <-  ggplot(degrees_perc %>% filter(clusters == 2), aes(x=percentile, y=salaries, group=College.Major, color=College.Major)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(size=7)) + 
  scale_y_continuous(labels = scales::dollar)
cluster_2 + theme_fivethirtyeight() + labs(color = "College Major") + 
  theme(axis.title = element_text()) + 
  xlab('\nPercentile') + 
  ylab('Salaries\n')

# Cluster3
# Cluster 3 Distribution by percentile and plotting of graph
cluster_3 <-  ggplot(degrees_perc %>% filter(clusters == 3), aes(x=percentile, y=salaries, group=College.Major, color=College.Major)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(size=7)) + 
  scale_y_continuous(labels = scales::dollar)
cluster_3 + theme_fivethirtyeight() + labs(color = "College Major") + 
  theme(axis.title = element_text()) + 
  xlab('\nPercentile') + 
  ylab('Salaries\n')

# Career.Percent.Growth -sorting them in this order
degrees_sorted <- degrees_labeled %>% arrange(desc(Career.Percent.Growth))
degrees_sorted %>% as_tibble()

library(tidyverse)
k_values <- data.frame(method = c('Elbow method', 'Silhouette method', 'Gap Statistic method'), k = c(3, 2, 3))
k_values %>% knitr::kable()
 
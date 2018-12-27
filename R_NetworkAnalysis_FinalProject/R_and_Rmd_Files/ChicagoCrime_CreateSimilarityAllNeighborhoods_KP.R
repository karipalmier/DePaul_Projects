######################################################################################################
# 
# ChicagoCrime_CreateSimilarityAllNeighborhoods.R
# 
# The file creates the similarity projection of the all crime graph data.  This is accomplished by 
# generating a cosine similarity adjacency matrix, where each entry of the matrix is the cosine
# similarity of the crime vectors of the two neighborhoods (rows and columns are neighborhoods).
#
# Created By      Date
# ----------      ----
# Kari Palmier    6/2/2018
#
######################################################################################################

library("igraph")
library("ggplot2")
library("GGally")
# Must load other packages first
library("sand")
library("intergraph")
library("Matrix")

base_path = "C:\\DePaulCoursework\\Spring CSC 495\\Project\\"
source(paste(base_path, "mycugtest.R", sep=""))
source(paste(base_path, "myqaptest.R", sep=""))

dir.create(file.path(base_path, "R_Output"), showWarnings = FALSE)
base_out_path = paste(base_path, "R_Output\\", sep = "")
dir.create(file.path(base_out_path, "SimilarityNeighborhoods"), showWarnings = FALSE)
output_path = paste(base_out_path, "SimilarityNeighborhoods\\", sep = "")

path = paste(base_path, "Graph_Data\\", sep = "")
setwd(path)

# Summarize data
print("Original All Graph Summary:")
all_gr = read.graph("feb_all.graphml", format = "graphml")
print(summary(all_gr))
print("", quote=FALSE)
print("", quote=FALSE)

# Projections
print("All Projection Graph Summaries:")
all_proj = bipartite.projection(all_gr, which = "TRUE")
print(summary(all_proj))
print("", quote=FALSE)
print("", quote=FALSE)

all_df_file = paste(base_path, "Dataframe_Data\\feb_edges.csv", sep = "")
all_edge_df = read.csv(all_df_file, header = T, sep = ",", stringsAsFactors = FALSE)
colnames(all_edge_df) = c("From", "To")
head(all_edge_df)

community_ids = as.numeric(V(all_gr)$name)
community_names = V(all_gr)$Label
bipartite_names = V(all_proj)$Label

all_edge_df$From_Label = rep("", length(all_edge_df$From))
all_edge_df$To_Label = rep("", length(all_edge_df$To))
for (i in community_ids){
  from_ndx = all_edge_df$From == i
  to_ndx = all_edge_df$To == i
  
  all_edge_df$From_Label[from_ndx] = community_names[i]
  all_edge_df$To_Label[to_ndx] = community_names[i]
}
head(all_edge_df)

V(all_proj)$crime_count = rep(0, length(bipartite_names))
for (i in 1:length(bipartite_names)){
  V(all_proj)$crime_count[i] = sum(all_edge_df$From_Label == bipartite_names[i])
}

init_descs = unique(V(all_gr)$Crime.Desc)
crime_descs = init_descs[init_descs != ""]

for (i in 1:length(bipartite_names)){
  if (i == 1){
    all_count_df = data.frame(counts = rep(0, length(crime_descs)))
    colnames(all_count_df) = c(bipartite_names[i])
  } else {
    all_count_df[bipartite_names[i]] = rep(0, length(crime_descs))
  }
  
  temp_counts = c()
  for (j in 1:length(crime_descs)){
    community_ndx = all_edge_df$From_Label == bipartite_names[i]
    temp_df = all_edge_df[community_ndx,]
    crime_ndx = temp_df$To_Label == crime_descs[j]
    temp_counts[j] = sum(crime_ndx)
  }
  
  all_count_df[bipartite_names[i]] = temp_counts
}
rownames(all_count_df) = crime_descs
all_count_df

temp_matrix = matrix(0, ncol = length(bipartite_names), nrow = length(bipartite_names))
for (i in 1:length(bipartite_names)){
  for (j in 1:length(bipartite_names)){
    if (bipartite_names[i] == bipartite_names[j]){
      temp_matrix[i, j] = 1
    } else {
      vector_i = as.vector(all_count_df[, bipartite_names[i]])
      vector_j = as.vector(all_count_df[, bipartite_names[j]])
      temp_matrix[i, j] = (vector_i %*% vector_j) / (norm(vector_i, type = "2") * norm(vector_j, type = "2"))
    }
  }
}

all_similarity_gr = graph_from_adjacency_matrix(temp_matrix,  mode = "undirected", weighted = TRUE)

all_attr = list.vertex.attributes(all_proj)
for (i in 1:length(all_attr)){
  temp_attr = all_attr[i]
  temp_var = paste("V(all_proj)$", temp_attr, sep = "")
  vertex_vals = eval(parse(text = temp_var))
  all_similarity_gr = set_vertex_attr(all_similarity_gr, name = temp_attr, value = vertex_vals)
}
summary(all_similarity_gr)

# Save updated files
out_file_all = paste(path, "feb_all_neighborhoods_similarity.graphml", sep = "")
write_graph(all_similarity_gr, out_file_all, format = "graphml")

# Save dataframe files
out_file_all_df = paste(df_path, "feb_all_neighborhoods_similarity_df.csv", sep = "")
write.csv(all_count_df, out_file_all_df, row.names = TRUE)



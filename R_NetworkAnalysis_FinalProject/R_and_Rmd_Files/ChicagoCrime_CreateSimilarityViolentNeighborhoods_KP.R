######################################################################################################
# 
# ChicagoCrime_CreateSimilarityViolentNeighborhoods.R
# 
# The file creates the similarity projection of the violent graph data.  This is accomplished by 
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
dir.create(file.path(base_out_path, "Neighborhoods"), showWarnings = FALSE)
output_path = paste(base_out_path, "Neighborhoods\\", sep = "")

path = paste(base_path, "Graph_Data\\", sep = "")
df_path = paste(base_path, "Dataframe_Data\\", sep = "")
setwd(path)

# Summarize data
print("Original Violent Graph Summary:")
violent_gr = read.graph("feb_violent.graphml", format = "graphml")
print(summary(violent_gr))
print("", quote=FALSE)
print("", quote=FALSE)

# Projections
print("Violent Projection Graph Summaries:")
violent_proj = bipartite.projection(violent_gr, which = "TRUE")
print(summary(violent_proj))
print("", quote=FALSE)
print("", quote=FALSE)

violent_df_file = paste(base_path, "Dataframe_Data\\feb_violent_edges.csv", sep = "")
violent_edge_df = read.csv(violent_df_file, header = T, sep = ",", stringsAsFactors = FALSE)
colnames(violent_edge_df) = c("From", "To")
head(violent_edge_df)

community_ids = as.numeric(V(violent_gr)$name)
community_names = V(violent_gr)$Label
bipartite_names = V(violent_proj)$Label

violent_edge_df$From_Label = rep("", length(violent_edge_df$From))
violent_edge_df$To_Label = rep("", length(violent_edge_df$To))
for (i in community_ids){
  from_ndx = violent_edge_df$From == i
  to_ndx = violent_edge_df$To == i
  
  violent_edge_df$From_Label[from_ndx] = community_names[i]
  violent_edge_df$To_Label[to_ndx] = community_names[i]
}
head(violent_edge_df)

V(violent_proj)$crime_count = rep(0, length(bipartite_names))
for (i in 1:length(bipartite_names)){
  V(violent_proj)$crime_count[i] = sum(violent_edge_df$From_Label == bipartite_names[i])
}

init_descs = unique(V(violent_gr)$Crime.Desc)
crime_descs = init_descs[init_descs != ""]

for (i in 1:length(bipartite_names)){
  if (i == 1){
    violent_count_df = data.frame(counts = rep(0, length(crime_descs)))
    colnames(violent_count_df) = c(bipartite_names[i])
  } else {
    violent_count_df[bipartite_names[i]] = rep(0, length(crime_descs))
  }
  
  temp_counts = c()
  for (j in 1:length(crime_descs)){
    community_ndx = violent_edge_df$From_Label == bipartite_names[i]
    temp_df = violent_edge_df[community_ndx,]
    crime_ndx = temp_df$To_Label == crime_descs[j]
    temp_counts[j] = sum(crime_ndx)
  }
  
  violent_count_df[bipartite_names[i]] = temp_counts
}
rownames(violent_count_df) = crime_descs
violent_count_df

temp_matrix = matrix(0, ncol = length(bipartite_names), nrow = length(bipartite_names))
for (i in 1:length(bipartite_names)){
  for (j in 1:length(bipartite_names)){
    if (bipartite_names[i] == bipartite_names[j]){
      temp_matrix[i, j] = 1
    } else {
      vector_i = as.vector(violent_count_df[, bipartite_names[i]])
      vector_j = as.vector(violent_count_df[, bipartite_names[j]])
      temp_matrix[i, j] = (vector_i %*% vector_j) / (norm(vector_i, type = "2") * norm(vector_j, type = "2"))
    }
  }
}

violent_similarity_gr = graph_from_adjacency_matrix(temp_matrix,  mode = "undirected", weighted = TRUE)

violent_attr = list.vertex.attributes(violent_proj)
for (i in 1:length(violent_attr)){
  temp_attr = violent_attr[i]
  temp_var = paste("V(violent_proj)$", temp_attr, sep = "")
  vertex_vals = eval(parse(text = temp_var))
  violent_similarity_gr = set_vertex_attr(violent_similarity_gr, name = temp_attr, value = vertex_vals)
}
summary(violent_similarity_gr)

# Save similarity files
out_file_violent = paste(path, "feb_violent_neighborhoods_similarity.graphml", sep = "")
write_graph(violent_similarity_gr, out_file_violent, format = "graphml")

# Save dataframe files
out_file_violent_df = paste(df_path, "feb_violent_neighborhoods_similarity_df.csv", sep = "")
write.csv(violent_count_df, out_file_violent_df, row.names = TRUE)




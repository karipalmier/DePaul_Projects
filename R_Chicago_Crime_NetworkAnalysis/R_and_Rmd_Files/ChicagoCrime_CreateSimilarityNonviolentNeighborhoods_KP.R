######################################################################################################
# 
# ChicagoCrime_CreateSimilarityNonviolentNeighborhoods.R
# 
# The file creates the similarity projection of the non-violent graph data.  This is accomplished by 
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
setwd(path)

# Summarize data
print("Original Nonviolent Graph Summary:")
nonviolent_gr = read.graph("feb_nonviolent.graphml", format = "graphml")
print(summary(nonviolent_gr))
print("", quote=FALSE)
print("", quote=FALSE)

# Projections
print("Nonviolent Projection Graph Summaries:")
nonviolent_proj = bipartite.projection(nonviolent_gr, which = "TRUE")
print(summary(nonviolent_proj))
print("", quote=FALSE)
print("", quote=FALSE)

nonviolent_df_file = paste(base_path, "Dataframe_Data\\feb_nonviolent_edges.csv", sep = "")
nonviolent_edge_df = read.csv(nonviolent_df_file, header = T, sep = ",", stringsAsFactors = FALSE)
colnames(nonviolent_edge_df) = c("From", "To")
head(nonviolent_edge_df)

community_ids = as.numeric(V(nonviolent_gr)$name)
community_names = V(nonviolent_gr)$Label
bipartite_names = V(nonviolent_proj)$Label

nonviolent_edge_df$From_Label = rep("", length(nonviolent_edge_df$From))
nonviolent_edge_df$To_Label = rep("", length(nonviolent_edge_df$To))
for (i in 1:length(community_ids)){
  curr_ndx = community_ids[i]
  from_ndx = nonviolent_edge_df$From == curr_ndx
  to_ndx = nonviolent_edge_df$To == curr_ndx
  
  nonviolent_edge_df$From_Label[from_ndx] = community_names[i]
  nonviolent_edge_df$To_Label[to_ndx] = community_names[i]
}
head(nonviolent_edge_df)

V(nonviolent_proj)$crime_count = rep(0, length(bipartite_names))
for (i in 1:length(bipartite_names)){
  V(nonviolent_proj)$crime_count[i] = sum(nonviolent_edge_df$From_Label == bipartite_names[i])
}

init_descs = unique(V(nonviolent_gr)$Crime.Desc)
crime_descs = init_descs[init_descs != ""]

for (i in 1:length(bipartite_names)){
  if (i == 1){
    nonviolent_count_df = data.frame(counts = rep(0, length(crime_descs)))
    colnames(nonviolent_count_df) = c(bipartite_names[i])
  } else {
    nonviolent_count_df[bipartite_names[i]] = rep(0, length(crime_descs))
  }
  
  temp_counts = c()
  for (j in 1:length(crime_descs)){
    community_ndx = nonviolent_edge_df$From_Label == bipartite_names[i]
    temp_df = nonviolent_edge_df[community_ndx,]
    crime_ndx = temp_df$To_Label == crime_descs[j]
    temp_counts[j] = sum(crime_ndx)
  }
  
  nonviolent_count_df[bipartite_names[i]] = temp_counts
}
rownames(nonviolent_count_df) = crime_descs
nonviolent_count_df

temp_matrix = matrix(0, ncol = length(bipartite_names), nrow = length(bipartite_names))
for (i in 1:length(bipartite_names)){
  for (j in 1:length(bipartite_names)){
    if (bipartite_names[i] == bipartite_names[j]){
      temp_matrix[i, j] = 1
    } else {
      vector_i = as.vector(nonviolent_count_df[, bipartite_names[i]])
      vector_j = as.vector(nonviolent_count_df[, bipartite_names[j]])
      temp_matrix[i, j] = (vector_i %*% vector_j) / (norm(vector_i, type = "2") * norm(vector_j, type = "2"))
    }
  }
}

nonviolent_similarity_gr = graph_from_adjacency_matrix(temp_matrix,  mode = "undirected", weighted = TRUE)

nonviolent_attr = list.vertex.attributes(nonviolent_proj)
for (i in 1:length(nonviolent_attr)){
  temp_attr = nonviolent_attr[i]
  temp_var = paste("V(nonviolent_proj)$", temp_attr, sep = "")
  vertex_vals = eval(parse(text = temp_var))
  nonviolent_similarity_gr = set_vertex_attr(nonviolent_similarity_gr, name = temp_attr, value = vertex_vals)
}
summary(nonviolent_similarity_gr)

# Save updated files
out_file_nonviolent = paste(path, "feb_nonviolent_neighborhoods_similarity.graphml", sep = "")
write_graph(nonviolent_similarity_gr, out_file_nonviolent, format = "graphml")

# Save dataframe files
out_file_nonviolent_df = paste(df_path, "feb_nonviolent_neighborhoods_similarity_df.csv", sep = "")
write.csv(nonviolent_count_df, out_file_nonviolent_df, row.names = TRUE)


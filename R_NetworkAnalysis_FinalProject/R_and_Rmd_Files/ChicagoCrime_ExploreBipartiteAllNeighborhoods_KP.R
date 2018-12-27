######################################################################################################
# 
# ChicagoCrime_ExploreBipartiteAllNeighborhoods.R
# 
# The file performs analysis on the bipartite neighborhood projections of all crimes 
# from the Feb 2017 Chicago Crime datasets.  The first step is to 
# create the bipartite projectionof the graph data.  
# Next the projection were filtered so that edge weights below given thresholds 
# were removed.  After that any singleton nodes were removed.  Finally the giant 
# component of the final networks were extracted.  Weighted degree, assortativity, 
# transitivity, and modularity analyses were then performed on the giant components 
# of the both networks.
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

base_path = "C:\\DePaulCoursework\\Spring CSC 495\\Project\\"
source(paste(base_path, "mycugtest.R", sep=""))
source(paste(base_path, "myqaptest.R", sep=""))

dir.create(file.path(base_path, "R_Output"), showWarnings = FALSE)
base_out_path = paste(base_path, "R_Output\\", sep = "")
dir.create(file.path(base_out_path, "Neighborhoods"), showWarnings = FALSE)
output_path = paste(base_out_path, "Neighborhoods\\", sep = "")

path = paste(base_path, "Graph_Data\\", sep = "")
setwd(path)

out_file_name = paste(output_path, "All_Graph_Summaries.txt", sep = '')
outFile = file(out_file_name, open="wt")
sink(file = outFile, append = TRUE)

# Summarize data
print("Original All Graph Summary:")
all_gr = read.graph("feb_all.graphml", format = "graphml")
print(summary(all_gr))
print("", quote=FALSE)
print("", quote=FALSE)

# Projections
print("All Projection Graph Summaries:")
all_comm = bipartite.projection(all_gr, which = "TRUE")
print(summary(all_comm))
print("", quote=FALSE)
print("", quote=FALSE)

# Remove non-matching attributes
print("All Projections After Vertex Removal:")
all_clean = delete_vertex_attr(all_comm, "Crime.Type")
all_clean = delete_vertex_attr(all_clean, "Crime.Desc")
print(summary(all_clean))
print("", quote=FALSE)
print("", quote=FALSE)

# Calculate graph densities
all_density = edge_density(all_clean)
print(paste("All Graph Density:", all_density))
print("", quote=FALSE)
print("", quote=FALSE)

# See if there are any components
print("All Graph Components:")
all_decomp = decompose(all_clean)
print(all_decomp)
print("", quote=FALSE)
print("", quote=FALSE)

print("All Graph Edge Weight Summary:")
print(summary(E(all_clean)$weight))
print("", quote=FALSE)
print("", quote=FALSE)

all_wdeg = graph.strength(all_clean)
print("All Graph Weighted Degree Summary:")
print(summary(all_wdeg))
print("", quote=FALSE)
print("", quote=FALSE)

# Plot graphs
temp_jpg = paste(output_path, "All_Graph.jpg", sep = "")
jpeg(file = temp_jpg)

plot(all_clean, layout = layout_with_kk)

dev.off()

###################### Edge Weights ##############################################################

# All Edge weight histogram
temp_jpg = paste(output_path, "All_Hist_EdgeWeight_Init.jpg", sep = "")
jpeg(file = temp_jpg)

g_allw = ggplot(data = data.frame(weights = E(all_clean)$weight), aes(x=weights)) +
  geom_histogram(bins = 50) +
  ggtitle("Edge Weights Histogram") + 
  labs(x = "Edge Weights", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(g_allw)
dev.off()

# Combined Edge Weight Log-Log Distribution
e_num_edge_bins = 30
e_min_edge = min(E(all_clean)$weight)
e_max_edge = max(E(all_clean)$weight)
e_step_edge = (e_max_edge - e_min_edge) / e_num_edge_bins

e_edge_breaks = seq(e_min_edge, e_max_edge, e_step_edge)
edge_bins = cut(E(all_clean)$weight, breaks = e_edge_breaks, labels=FALSE)
log_weight_df = data.frame(tabulate(edge_bins))

log_weight_df$edge <- e_edge_breaks[1:(length(e_edge_breaks)-1)]
log_weight_df = log_weight_df[log_weight_df$tabulate.edge_bins.>0,]

x_num_breaks = 5
x_min_edge = min(log_weight_df$edge)
x_max_edge = max(log_weight_df$edge)
x_step_edge = (x_max_edge - x_min_edge) / x_num_breaks

y_num_breaks = 5
y_min_edge = min(log_weight_df$tabulate.edge_bins.)
y_max_edge = max(log_weight_df$tabulate.edge_bins.)
y_step_edge = (y_max_edge - y_min_edge) / y_num_breaks

temp_jpg = paste(output_path, "All_Log_EdgeW_Init.jpg", sep = "")
jpeg(file = temp_jpg)

g_combined_log = ggplot(log_weight_df, aes(x=edge, y=tabulate.edge_bins.)) +
  geom_point() + geom_line() +
  scale_x_log10(name="Edge Weights", breaks = seq(x_min_edge, x_max_edge, x_step_edge)) + 
  scale_y_log10("Frequency", breaks = seq(y_min_edge, y_max_edge, y_step_edge)) +
  ggtitle("All Edge Weights") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(g_combined_log)
dev.off()

################################## Edge Filtering ##############################################################

# get all edge filtereing threshold from percent data left
e_perc_rem = 0.95
edge_w = E(all_clean)$weight
sorted_edge_w = sort(edge_w)
e_thresh = quantile(sorted_edge_w, e_perc_rem)
print(paste("All Edge Weight Filtering Threshold:", e_thresh))
print("", quote=FALSE)
e_perc_rem = sum(e_hist_data$data[[1]]$count[1:e_thresh_ndx]) / sum(e_hist_data$data[[1]]$count)
print(paste("All Edge Weight Filtering % Edges Removed:", e_perc_rem))
print("", quote=FALSE)
print("", quote=FALSE)

all_filter = delete.edges(all_clean, which(E(all_clean)$weight < e_thresh))
print("All Graph After Edge Filtering:")
print(summary(all_filter))
print("", quote=FALSE)

# All Edge weight histogram
temp_jpg = paste(output_path, "All_Hist_EdgeWeight_Filtered.jpg", sep = "")
jpeg(file = temp_jpg)

g_allw = ggplot(data = data.frame(weights = E(all_filter)$weight), aes(x=weights)) +
  geom_histogram(bins = 50) +
  ggtitle("Filtered Edge Weights Histogram") + 
  labs(x = "Edge Weights", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(g_allw)
dev.off()

# Combined Edge Weight Log-Log Distribution
e_num_edge_bins = 30
e_min_edge = min(E(all_filter)$weight)
e_max_edge = max(E(all_filter)$weight)
e_step_edge = (e_max_edge - e_min_edge) / e_num_edge_bins

e_edge_breaks = seq(e_min_edge, e_max_edge, e_step_edge)
edge_bins = cut(E(all_filter)$weight, breaks = e_edge_breaks, labels=FALSE)
log_weight_df = data.frame(tabulate(edge_bins))

log_weight_df$edge <- e_edge_breaks[1:(length(e_edge_breaks)-1)]
log_weight_df = log_weight_df[log_weight_df$tabulate.edge_bins.>0,]

x_num_breaks = 5
x_min_edge = min(log_weight_df$edge)
x_max_edge = max(log_weight_df$edge)
x_step_edge = (x_max_edge - x_min_edge) / x_num_breaks

y_num_breaks = 5
y_min_edge = min(log_weight_df$tabulate.edge_bins.)
y_max_edge = max(log_weight_df$tabulate.edge_bins.)
y_step_edge = (y_max_edge - y_min_edge) / y_num_breaks

temp_jpg = paste(output_path, "All_Log_EdgeW_Filtered.jpg", sep = "")
jpeg(file = temp_jpg)

g_combined_log = ggplot(log_weight_df, aes(x=edge, y=tabulate.edge_bins.)) +
  geom_point() + geom_line() +
  scale_x_log10(name="Edge Weights", breaks = seq(x_min_edge, x_max_edge, x_step_edge)) + 
  scale_y_log10("Frequency", breaks = seq(y_min_edge, y_max_edge, y_step_edge)) +
  ggtitle("All Filtered Edge Weight") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(g_combined_log)
dev.off()

###################### Weighted Degree #############################################################

all_wdeg = graph.strength(all_clean)

print("All Weighted Degree Summary:")
print(summary(all_wdeg))
print("", quote=FALSE)

# All Degree Distribution
temp_jpg = paste(output_path, "All_Hist_WDegree_Init.jpg", sep = "")
jpeg(file = temp_jpg)

g_all_wdeg = ggplot(data = data.frame(WDegree = all_wdeg), aes(x=WDegree)) +
  geom_histogram(bins = 30) +
  ggtitle("Weighted Degree Histogram") + 
  labs(x = "Weighted Degree", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(g_all_wdeg)
dev.off()

# Combined Weighted Degree Log-Log Distribution
e_num_wdeg_bins = 30
e_min_wdeg = min(all_wdeg)
e_max_wdeg = max(all_wdeg)
e_step_wdeg = (e_max_wdeg - e_min_wdeg) / e_num_wdeg_bins

e_wdeg_breaks = seq(e_min_wdeg, e_max_wdeg, e_step_wdeg)
wdeg_bins = cut(all_wdeg, breaks = e_wdeg_breaks, labels=FALSE)
log_weight_df = data.frame(tabulate(wdeg_bins))

log_weight_df$wdeg <- e_wdeg_breaks[1:(length(e_wdeg_breaks)-1)]
log_weight_df = log_weight_df[log_weight_df$tabulate.wdeg_bins.>0,]
log_weight_df$type = rep("VIOLENT", length(log_weight_df$wdeg))

x_num_breaks = 5
x_min_wdeg = min(log_weight_df$wdeg)
x_max_wdeg = max(log_weight_df$wdeg)
x_step_wdeg = (x_max_wdeg - x_min_wdeg) / x_num_breaks

y_num_breaks = 5
y_min_wdeg = min(log_weight_df$tabulate.wdeg_bins.)
y_max_wdeg = max(log_weight_df$tabulate.wdeg_bins.)
y_step_wdeg = (y_max_wdeg - y_min_wdeg) / y_num_breaks

temp_jpg = paste(output_path, "All_Log_WDeg_Init.jpg", sep = "")
jpeg(file = temp_jpg)

g_combined_log = ggplot(log_weight_df, aes(x=wdeg, y=tabulate.wdeg_bins., color = type)) +
  geom_point() + geom_line() +
  scale_x_log10(name="Weight Degree", breaks = seq(x_min_wdeg, x_max_wdeg, x_step_wdeg)) + 
  scale_y_log10("Frequency", breaks = seq(y_min_wdeg, y_max_wdeg, y_step_wdeg)) +
  ggtitle("All Weighted Degree") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(g_combined_log)
dev.off()

################################## Singleton Filtering #######################################################

all_filter = delete.vertices(all_filter, which(degree(all_filter) == 0))
print("All Graph After Singleton Filtering:")
print(summary(all_filter))
print("", quote=FALSE)

all_filter_wdeg = graph.strength(all_filter)

print("Filtered All Weighted Degree Summary:")
print(summary(all_filter_wdeg))
print("", quote=FALSE)

# All Degree Distribution
temp_jpg = paste(output_path, "All_Hist_WDegree_Filtered.jpg", sep = "")
jpeg(file = temp_jpg)

g_all_wdeg = ggplot(data = data.frame(WDegree = all_filter_wdeg), aes(x=WDegree)) +
  geom_histogram(bins = 30) +
  ggtitle("FIltered Weighted Degree Histogram") + 
  labs(x = "Weighted Degree", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(g_all_wdeg)
V(all_filter)$wdegree = all_filter_wdeg
dev.off()

# Combined Weighted Degree Log-Log Distribution
e_num_wdeg_bins = 30
e_min_wdeg = min(all_filter_wdeg)
e_max_wdeg = max(all_filter_wdeg)
e_step_wdeg = (e_max_wdeg - e_min_wdeg) / e_num_wdeg_bins

e_wdeg_breaks = seq(e_min_wdeg, e_max_wdeg, e_step_wdeg)
wdeg_bins = cut(all_filter_wdeg, breaks = e_wdeg_breaks, labels=FALSE)
log_weight_df = data.frame(tabulate(wdeg_bins))

log_weight_df$wdeg <- e_wdeg_breaks[1:(length(e_wdeg_breaks)-1)]
log_weight_df = log_weight_df[log_weight_df$tabulate.wdeg_bins.>0,]

x_num_breaks = 5
x_min_wdeg = min(log_weight_df$wdeg)
x_max_wdeg = max(log_weight_df$wdeg)
x_step_wdeg = (x_max_wdeg - x_min_wdeg) / x_num_breaks

y_num_breaks = 5
y_min_wdeg = min(log_weight_df$tabulate.wdeg_bins.)
y_max_wdeg = max(log_weight_df$tabulate.wdeg_bins.)
y_step_wdeg = (y_max_wdeg - y_min_wdeg) / y_num_breaks

temp_jpg = paste(output_path, "All_Log_WDeg_Filtered.jpg", sep = "")
jpeg(file = temp_jpg)

g_combined_log = ggplot(log_weight_df, aes(x=wdeg, y=tabulate.wdeg_bins., color = type)) +
  geom_point() + geom_line() +
  scale_x_log10(name="Weight Degree", breaks = seq(x_min_wdeg, x_max_wdeg, x_step_wdeg)) + 
  scale_y_log10("Frequency", breaks = seq(y_min_wdeg, y_max_wdeg, y_step_wdeg)) +
  ggtitle("All Filtered Weighted Degree") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(g_combined_log)
dev.off()


# Calculate graph densities
all_density = edge_density(all_filter)
print(paste("Filtered All Graph Density:", all_density))
print("", quote=FALSE)

# See if there are any components
print("Filtered All Graph Components:")
all_decomp = decompose(all_filter)
print(all_decomp)
print("", quote=FALSE)

print("Filtered All Edge Weight Summary:")
print(summary(E(all_filter)$weight))
print("", quote=FALSE)


sink()

close(outFile)
closeAllConnections()


###################### Assortativity #################################################################

# Calculate assortativity of vertex attributes
dir.create(file.path(output_path, "Assortativity"), showWarnings = FALSE)
assort_path = paste(output_path, "Assortativity\\", sep = "")

dir.create(file.path(output_path, "Attributes"), showWarnings = FALSE)
attr_path = paste(output_path, "Attributes\\", sep = "")

tmp_scatter = paste(attr_path, "All_WeightedDegree_Vs_Community.jpg", sep = '')
jpeg(file = tmp_scatter)

temp_df = data.frame(Community = V(all_filter)$Label, WDegree = V(all_filter)$wdegree)
temp_df = temp_df[order(temp_df$WDegree),]
tmp_g = ggplot(data = temp_df, aes(x = reorder(Community, WDegree), y = WDegree)) + 
  geom_point() + 
  labs(x = "Community Name", y = "Weighted Degree") +
  ggtitle("All Crime Weighted Degree By Community Name") +
  theme(text = element_text(size=7, face="bold"), axis.text.x = element_text(angle = 90, hjust = 1))
print(tmp_g)
dev.off()

out_file_name = paste(assort_path, "All_Assortativity.txt", sep = '')
outFile = file(out_file_name, open="wt")
sink(file = outFile, append = TRUE)

attrs_skip = c("name", "Label", "id", "wdegree", "Community.Area")

all_attr = list.vertex.attributes(all_filter)

for ( i in 1:length(all_attr)){
  temp_attr = all_attr[i]
  
  if (!(temp_attr %in% attrs_skip)){
    
    print("------------------------------------------------------------------------------")
    print(paste("Testing Assortativity for attribute:", temp_attr))
    print("", quote=FALSE)
    
    temp_var = paste("V(all_filter)$", temp_attr, sep = "")
    vertex_vals = eval(parse(text = temp_var))
    print("Vertex Values:")
    print(vertex_vals)
    print("", quote=FALSE)
    print("Vertex Value Summary:")
    print(summary(vertex_vals))
    print("", quote=FALSE)
    
    temp_attr_jpg =  sub("\\.", "", temp_attr)
    
    tmp_scatter = paste(attr_path, "All_", temp_attr_jpg, '_Vs_Community.jpg', sep = '')
    jpeg(file = tmp_scatter)
    
    temp_df = data.frame(Community = V(all_filter)$Label, temp_attr_jpg = vertex_vals)
    temp_df = temp_df[order(vertex_vals),]
    tmp_g = ggplot(data = temp_df, aes(x = reorder(Community, temp_attr_jpg), y = temp_attr_jpg)) + 
      geom_point() + 
      labs(x = "Community Name", y = temp_attr_jpg) +
      ggtitle(paste(temp_attr_jpg, "By Community Name")) +
      theme(text = element_text(size=7, face="bold"), axis.text.x = element_text(angle = 90, hjust = 1))
    print(tmp_g)
    dev.off()
    
    if (grepl("Bin", temp_attr, fixed = TRUE)){
      temp_assort = assortativity_nominal(all_filter, types = factor(vertex_vals), 
                                  directed = FALSE)
      temp_cug = mycugtest(all_filter, assortativity_nominal, cmode = "edges", 
                           types = factor(vertex_vals), directed = FALSE)
      temp_qap = myqaptest(all_filter, assortativity_nominal, types = factor(vertex_vals),
                           directed = FALSE)
    } else {
      temp_assort = assortativity(all_filter, types1 = vertex_vals, 
                                  directed = FALSE)
      temp_cug = mycugtest(all_filter, assortativity, cmode = "edges", types1 = vertex_vals,
                           directed = FALSE)
      temp_qap = myqaptest(all_filter, assortativity, types1 = vertex_vals,
                           directed = FALSE)
    }
    
    print(paste("Assortativity:", temp_assort))
    print("", quote=FALSE)
    
    print("Assortativity CUG Test Results:")
    print.cug.test(temp_cug)
    print("", quote=FALSE)
    
    tmp_cug = paste(assort_path, "All_", temp_attr_jpg, '_CUG.jpg', sep = '')
    jpeg(file = tmp_cug)
    plot.cug.test(temp_cug)
    dev.off()
    
    print("Assortativity QAP Test Results:")
    print(summary.qaptest(temp_qap))
    print("", quote=FALSE)
    
    tmp_qap = paste(assort_path, "All_", temp_attr_jpg, '_QAP.jpg', sep = '')
    jpeg(file = tmp_qap)
    plot.qaptest(temp_qap)
    dev.off()
    
  }

}

sink()

close(outFile)
closeAllConnections()

###################### Transitivity #################################################################

dir.create(file.path(output_path, "Transitivity"), showWarnings = FALSE)
trans_path = paste(output_path, "Transitivity\\", sep = "")

out_file_name = paste(trans_path, "All_Transitivity.txt", sep = '')
outFile = file(out_file_name, open="wt")
sink(file = outFile, append = TRUE)

v_local_trans = mean(transitivity(all_filter, type = "local"))
print(paste("Mean Local Transitivity:", v_local_trans))
print("", quote=FALSE)

temp_cug = mycugtest(all_filter, transitivity, cmode = "edges", type = "local")

print("Local Transitivity CUG Test Results:")
print.cug.test(temp_cug)
print("", quote=FALSE)

tmp_cug = paste(trans_path, "All_Local_Transitivity_CUG.jpg", sep = '')
jpeg(file = tmp_cug)
plot.cug.test(temp_cug)
dev.off()

v_global_trans = transitivity(all_filter, type = "global")
print(paste("Global Transitivity:", v_global_trans))
print("", quote=FALSE)

temp_cug = mycugtest(all_filter, transitivity, cmode = "edges", type = "global")

print("Global Transitivity CUG Test Results:")
print.cug.test(temp_cug)
print("", quote=FALSE)

tmp_cug = paste(trans_path, "All_Global_Transitivity_CUG.jpg", sep = '')
jpeg(file = tmp_cug)
plot.cug.test(temp_cug)
dev.off()

sink()

close(outFile)
closeAllConnections()

###################### Community Detection #####################################################

set.seed(20170423)

out_file_name = paste(output_path, "All_Graph_Modularities.txt", sep = '')
outFile = file(out_file_name, open="wt")
sink(file = outFile, append = TRUE)

# All community detection
comm.sg = cluster_spinglass(all_filter)
V(all_filter)$comm_sg = comm.sg$membership

comm.lv = cluster_louvain(all_filter)
V(all_filter)$comm_lv = comm.lv$membership

comm.le = cluster_leading_eigen(all_filter)
V(all_filter)$comm_le = comm.le$membership

comm.fg = cluster_fast_greedy(all_filter)
V(all_filter)$comm_fg = comm.fg$membership

comm.bt = cluster_edge_betweenness(all_filter, weights = NULL)
V(all_filter)$comm_bt = comm.bt$membership

comm.wt3 = cluster_walktrap(all_filter, steps = 3)
V(all_filter)$comm_wt5 = comm.wt3$membership

comm.wt5 = cluster_walktrap(all_filter, steps = 5)
V(all_filter)$comm_wt10 = comm.wt5$membership

comm.wt7 = cluster_walktrap(all_filter, steps = 7)
V(all_filter)$comm_wt10 = comm.wt7$membership

comm.mod = lapply(list(comm.sg, comm.lv, comm.le, comm.fg, comm.bt, comm.wt3, comm.wt5, comm.wt7), modularity)
comm.len = lapply(list(comm.sg, comm.lv, comm.le, comm.fg, comm.bt, comm.wt3, comm.wt5, comm.wt7), length)

print(comm.mod)
print(comm.len)

algorithms = c("SG", "LV", "LE", "FG", "BT", "WT3", "WT5", "WT7")
comm_df = data.frame(Modularity = as.vector(comm.mod, mode = "numeric"),
                     Length = as.vector(comm.len, mode = "numeric"), Algorithm = algorithms)

# Create bar chart of lengths
temp_jpg = paste(output_path, "All_Cluster_Sizes.jpg", sep = "")
jpeg(file = temp_jpg)

p = ggplot(data = comm_df, aes(x = Algorithm, y = Length, fill = Algorithm))
p = p + geom_bar(stat = "identity")
p = p + geom_text(aes(label=round(Length, 4)), vjust=0) 
p = p + ggtitle("Number of Community Clusters By Algorithm")
p = p + labs(x = "Community Detection Algorithm", y = "Number of Clusters (Length)")
print(p)
dev.off()

# Create bar chart of lengths
temp_jpg = paste(output_path, "All_Cluster_Modularities.jpg", sep = "")
jpeg(file = temp_jpg)

p = ggplot(data = comm_df, aes(x = Algorithm, y = Modularity, fill = Algorithm))
p = p + geom_bar(stat = "identity")
p = p + geom_text(aes(label=round(Modularity, 4)), vjust=0) 
p = p + ggtitle("Community Cluster Modularities By Algorithm")
p = p + labs(x = "Community Detection Algorithm", y = "Modularity")
print(p)
dev.off()

sink()

close(outFile)
closeAllConnections()


# Save updated files
out_file_all = paste(path, "feb_all_neighborhoods_filtered.graphml", sep = "")
write_graph(all_filter, out_file_all, format = "graphml")



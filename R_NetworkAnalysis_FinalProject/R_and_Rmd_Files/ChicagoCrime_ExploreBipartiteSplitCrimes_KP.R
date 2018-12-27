######################################################################################################
# 
# ChicagoCrime_ExploreBipartiteSplitCrimes.R
# 
# The file performs analysis on the bipartite crime projections of the 
# violent and non-violent Feb 2017 Chicago Crime datasets.  The first step is to 
# create the bipartite projections of both the violent and non-violent graph data.  
# Weighted degree, assortativity, 
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
dir.create(file.path(base_out_path, "Crimes"), showWarnings = FALSE)
output_path = paste(base_out_path, "Crimes\\", sep = "")

path = paste(base_path, "Graph_Data\\", sep = "")
setwd(path)

out_file_name = paste(output_path, "Graph_Summaries.txt", sep = '')
outFile = file(out_file_name, open="wt")
sink(file = outFile, append = TRUE)

# Summarize data
print("Original Violent Graph Summary:")
violent_gr = read.graph("feb_violent.graphml", format = "graphml")
print(summary(violent_gr))
print("", quote=FALSE)
print("", quote=FALSE)

print("Original NonViolent Graph Summary:")
nonviolent_gr = read.graph("feb_nonviolent.graphml", format = "graphml")
print(summary(nonviolent_gr))
print("", quote=FALSE)
print("", quote=FALSE)

# Projections
print("Violent Projection Graph Summaries:")
violent_comm = bipartite.projection(violent_gr, which = "FALSE")
print(summary(violent_comm))
print("", quote=FALSE)
print("", quote=FALSE)

print("NonViolent Projection Graph Summaries:")
nonviolent_comm = bipartite.projection(nonviolent_gr, which = "FALSE")
print(summary(nonviolent_comm))
print("", quote=FALSE)
print("", quote=FALSE)

# Remove non-matching attributes
print("Violent Projections After Vertex Removal:")
violent_filter = delete_vertex_attr(violent_comm, "Prop.Occupied")
violent_filter = delete_vertex_attr(violent_filter, "Prop.Rented")
violent_filter = delete_vertex_attr(violent_filter, "Prop.Vacant")
violent_filter = delete_vertex_attr(violent_filter, "Prop.Owned")
violent_filter = delete_vertex_attr(violent_filter, "Median.Age")
violent_filter = delete_vertex_attr(violent_filter, "Total.Population")
violent_filter = delete_vertex_attr(violent_filter, "Prop.African")
violent_filter = delete_vertex_attr(violent_filter, "Prop.White")
violent_filter = delete_vertex_attr(violent_filter, "Prop.Asian")
violent_filter = delete_vertex_attr(violent_filter, "Community.Area")
print(summary(violent_filter))
print("", quote=FALSE)
print("", quote=FALSE)

print("NonViolent Projections After Vertex Removal:")
nonviolent_filter = delete_vertex_attr(nonviolent_comm, "Prop.Occupied")
nonviolent_filter = delete_vertex_attr(nonviolent_filter, "Prop.Rented")
nonviolent_filter = delete_vertex_attr(nonviolent_filter, "Prop.Vacant")
nonviolent_filter = delete_vertex_attr(nonviolent_filter, "Prop.Owned")
nonviolent_filter = delete_vertex_attr(nonviolent_filter, "Median.Age")
nonviolent_filter = delete_vertex_attr(nonviolent_filter, "Total.Population")
nonviolent_filter = delete_vertex_attr(nonviolent_filter, "Prop.African")
nonviolent_filter = delete_vertex_attr(nonviolent_filter, "Prop.White")
nonviolent_filter = delete_vertex_attr(nonviolent_filter, "Prop.Asian")
nonviolent_filter = delete_vertex_attr(nonviolent_filter, "Community.Area")
print(summary(nonviolent_filter))
print("", quote=FALSE)
print("", quote=FALSE)

# Calculate graph densities
violent_density = edge_density(violent_filter)
print(paste("Violent Graph Density:", violent_density))
print("", quote=FALSE)

nonviolent_density = edge_density(nonviolent_filter)
print(paste("NonViolent Graph Density:", nonviolent_density))
print("", quote=FALSE)
print("", quote=FALSE)

# See if there are any components
print("Violent Graph Components:")
violent_decomp = decompose(violent_filter)
print(violent_decomp)
print("", quote=FALSE)

print("NonViolent Graph Components:")
nonviolent_decomp = decompose(nonviolent_filter)
print(nonviolent_decomp)
print("", quote=FALSE)
print("", quote=FALSE)

print("Violent Edge Weight Summary:")
print(summary(E(violent_filter)$weight))
print("", quote=FALSE)

print("NonViolent Edge Weight Summary:")
print(summary(E(nonviolent_filter)$weight))
print("", quote=FALSE)
print("", quote=FALSE)

violent_wdeg = graph.strength(violent_filter)
nonviolent_wdeg = graph.strength(nonviolent_filter)

print("Violent Weighted Degree Summary:")
print(summary(violent_wdeg))
print("", quote=FALSE)

print("NonViolent Weighted Degree Summary:")
print(summary(nonviolent_wdeg))
print("", quote=FALSE)
print("", quote=FALSE)

# Plot graphs
temp_jpg = paste(output_path, "Violent_Graph.jpg", sep = "")
jpeg(file = temp_jpg)

plot(violent_filter, layout = layout_with_kk)

dev.off()

temp_jpg = paste(output_path, "Nonviolent_Graph.jpg", sep = "")
jpeg(file = temp_jpg)

plot(nonviolent_filter, layout = layout_with_kk)

dev.off()

sink()

close(outFile)
closeAllConnections()

###################### Edge Weights ##############################################################

# Violent Edge weight histogram
temp_jpg = paste(output_path, "Violent_Hist_EdgeWeight.jpg", sep = "")
jpeg(file = temp_jpg)

g_violentw = ggplot(data = data.frame(weights = E(violent_filter)$weight), aes(x=weights)) +
  geom_histogram(bins = 50) +
  ggtitle("Violent Community Edge Weights Histogram") + 
  labs(x = "Edge Weights", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(g_violentw)
dev.off()

# Nonviolent Edge weight histogram
temp_jpg = paste(output_path, "Nonviolent_Hist_EdgeWeight.jpg", sep = "")
jpeg(file = temp_jpg)

g_nonviolentw = ggplot(data = data.frame(weights = E(nonviolent_filter)$weight), aes(x=weights)) +
  geom_histogram(bins = 50) +
  ggtitle("Non-Violent Community Edge Weights Histogram") + 
  labs(x = "Edge Weights", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(g_nonviolentw)
dev.off()

# Combined Edge Weight Distribution Histogram
weight_df = data.frame(type = rep("VIOLENT", length(E(violent_filter)$weight)), 
                       weight = E(violent_filter)$weight)
weight_df = rbind(weight_df, data.frame(type = rep("NONVIOLENT", length(E(nonviolent_filter)$weight)), 
                                        weight = E(nonviolent_filter)$weight))

temp_jpg = paste(output_path, "Combined_Hist_EdgeW.jpg", sep = "")
jpeg(file = temp_jpg)

g_combined_ehist = ggplot(data = weight_df, aes(x = weight, fill = type)) +
  geom_histogram(position = "dodge", bins = 30) +
  ggtitle("Edge Weight By Crime Type") +
  labs(x = "Edge Weight", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(g_combined_ehist)
dev.off()

# Combined Edge Weight Log-Log Distribution
v_num_edge_bins = 30
v_min_edge = min(E(violent_filter)$weight)
v_max_edge = max(E(violent_filter)$weight)
v_step_edge = (v_max_edge - v_min_edge) / v_num_edge_bins

v_edge_breaks = seq(v_min_edge, v_max_edge, v_step_edge)
edge_bins = cut(E(violent_filter)$weight, breaks = v_edge_breaks, labels=FALSE)
v_tab_edge = data.frame(tabulate(edge_bins))

v_tab_edge$edge <- v_edge_breaks[1:(length(v_edge_breaks)-1)]
v_tab_edge = v_tab_edge[v_tab_edge$tabulate.edge_bins.>0,]
v_tab_edge$type = rep("VIOLENT", length(v_tab_edge$edge))

nv_num_edge_bins = 30
nv_min_edge = min(E(nonviolent_filter)$weight)
nv_max_edge = max(E(nonviolent_filter)$weight)
nv_step_edge = (nv_max_edge - nv_min_edge) / nv_num_edge_bins

nv_edge_breaks = seq(nv_min_edge, nv_max_edge, nv_step_edge)
edge_bins = cut(E(nonviolent_filter)$weight, breaks = nv_edge_breaks, labels=FALSE)
nv_tab_edge = data.frame(tabulate(edge_bins))

nv_tab_edge$edge <- nv_edge_breaks[1:(length(nv_edge_breaks)-1)]
nv_tab_edge = nv_tab_edge[nv_tab_edge$tabulate.edge_bins.>0,]
nv_tab_edge$type = rep("NONVIOLENT", length(nv_tab_edge$edge))

log_weight_df = rbind(v_tab_edge,nv_tab_edge)

x_num_breaks = 5
x_min_edge = min(log_weight_df$edge)
x_max_edge = max(log_weight_df$edge)
x_step_edge = (x_max_edge - x_min_edge) / x_num_breaks

y_num_breaks = 5
y_min_edge = min(log_weight_df$tabulate.edge_bins.)
y_max_edge = max(log_weight_df$tabulate.edge_bins.)
y_step_edge = (y_max_edge - y_min_edge) / y_num_breaks

temp_jpg = paste(output_path, "Combined_Log_EdgeW_Filtered.jpg", sep = "")
jpeg(file = temp_jpg)

g_combined_log = ggplot(log_weight_df, aes(x=edge, y=tabulate.edge_bins., color = type)) +
  geom_point() + geom_line() +
  scale_x_log10(name="Edge Weights", breaks = seq(x_min_edge, x_max_edge, x_step_edge)) + 
  scale_y_log10("Frequency", breaks = seq(y_min_edge, y_max_edge, y_step_edge)) +
  ggtitle("Filtered Edge Weight By Crime Type") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(g_combined_log)
dev.off()

###################### Weigthed Degree #############################################################

# Violent Degree Distribution
temp_jpg = paste(output_path, "Violent_Hist_WDegree.jpg", sep = "")
jpeg(file = temp_jpg)

g_violent_wdeg = ggplot(data = data.frame(WDegree = violent_wdeg), aes(x=WDegree)) +
  geom_histogram(bins = 30) +
  ggtitle("Violent Community Weighted Degree Histogram") + 
  labs(x = "Weighted Degree", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(g_violent_wdeg)
V(violent_filter)$wdegree = violent_wdeg
dev.off()

# Non-Violent Degree Distribution
temp_jpg = paste(output_path, "Nonviolent_Hist_WDegree.jpg", sep = "")
jpeg(file = temp_jpg)

g_nonviolent_wdeg = ggplot(data = data.frame(WDegree = nonviolent_wdeg), aes(x=WDegree)) +
  geom_histogram(bins = 30) +
  ggtitle("Non-Violent Community Weighted Degree Histogram") + 
  labs(x = "Weighted Degree", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(g_nonviolent_wdeg)
V(nonviolent_filter)$wdegree = nonviolent_wdeg
dev.off()

# Combined Weighted Degree Distribution Histogram
degree_df = data.frame(type = rep("VIOLENT", length(violent_wdeg)), degree = violent_wdeg)
degree_df = rbind(degree_df, data.frame(type = rep("NONVIOLENT", length(nonviolent_wdeg)), 
                                        degree = nonviolent_wdeg))

temp_jpg = paste(output_path, "Combined_Hist_WDegree.jpg", sep = "")
jpeg(file = temp_jpg)

g_combined_hist = ggplot(data = degree_df, aes(x = degree, fill = type)) +
  geom_histogram(position = "dodge", bins = 30) +
  ggtitle("Weighted Degree By Crime Type") +
  labs(x = "Weighted Degree", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(g_combined_hist)
dev.off()

# Combined Weighted Degree Log-Log Distribution
v_num_wdeg_bins = 30
v_min_wdeg = min(violent_wdeg)
v_max_wdeg = max(violent_wdeg)
v_step_wdeg = (v_max_wdeg - v_min_wdeg) / v_num_wdeg_bins

v_wdeg_breaks = seq(v_min_wdeg, v_max_wdeg, v_step_wdeg)
wdeg_bins = cut(violent_wdeg, breaks = v_wdeg_breaks, labels=FALSE)
v_tab_wdeg = data.frame(tabulate(wdeg_bins))

v_tab_wdeg$wdeg <- v_wdeg_breaks[1:(length(v_wdeg_breaks)-1)]
v_tab_wdeg = v_tab_wdeg[v_tab_wdeg$tabulate.wdeg_bins.>0,]
v_tab_wdeg$type = rep("VIOLENT", length(v_tab_wdeg$wdeg))

nv_num_wdeg_bins = 30
nv_min_wdeg = min(nonviolent_wdeg)
nv_max_wdeg = max(nonviolent_wdeg)
nv_step_wdeg = (nv_max_wdeg - nv_min_wdeg) / nv_num_wdeg_bins

nv_wdeg_breaks = seq(nv_min_wdeg, nv_max_wdeg, nv_step_wdeg)
wdeg_bins = cut(nonviolent_wdeg, breaks = nv_wdeg_breaks, labels=FALSE)
nv_tab_wdeg = data.frame(tabulate(wdeg_bins))

nv_tab_wdeg$wdeg <- nv_wdeg_breaks[1:(length(nv_wdeg_breaks)-1)]
nv_tab_wdeg = nv_tab_wdeg[nv_tab_wdeg$tabulate.wdeg_bins.>0,]
nv_tab_wdeg$type = rep("NONVIOLENT", length(nv_tab_wdeg$wdeg))

log_weight_df = rbind(v_tab_wdeg,nv_tab_wdeg)

x_num_breaks = 5
x_min_wdeg = min(log_weight_df$wdeg)
x_max_wdeg = max(log_weight_df$wdeg)
x_step_wdeg = (x_max_wdeg - x_min_wdeg) / x_num_breaks

y_num_breaks = 5
y_min_wdeg = min(log_weight_df$tabulate.wdeg_bins.)
y_max_wdeg = max(log_weight_df$tabulate.wdeg_bins.)
y_step_wdeg = (y_max_wdeg - y_min_wdeg) / y_num_breaks

temp_jpg = paste(output_path, "Combined_Log_WDeg_Init.jpg", sep = "")
jpeg(file = temp_jpg)

g_combined_log = ggplot(log_weight_df, aes(x=wdeg, y=tabulate.wdeg_bins., color = type)) +
  geom_point() + geom_line() +
  scale_x_log10(name="Weight Degree", breaks = seq(x_min_wdeg, x_max_wdeg, x_step_wdeg)) + 
  scale_y_log10("Frequency", breaks = seq(y_min_wdeg, y_max_wdeg, y_step_wdeg)) +
  ggtitle("Weighted Degree By Crime Type") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(g_combined_log)
dev.off()

###################### Transitivity #################################################################

dir.create(file.path(output_path, "Transitivity"), showWarnings = FALSE)
trans_path = paste(output_path, "Transitivity\\", sep = "")

out_file_name = paste(trans_path, "Violent_Transitivity.txt", sep = '')
outFile = file(out_file_name, open="wt")
sink(file = outFile, append = TRUE)

v_local_trans = mean(transitivity(violent_filter, type = "local"))
print(paste("Mean Local Transitivity:", v_local_trans))

temp_cug = mycugtest(violent_filter, transitivity, cmode = "edges", directed = FALSE, type = "local")

print("Local Transitivity CUG Test Results:")
print.cug.test(temp_cug)
print("", quote=FALSE)

tmp_cug = paste(trans_path, "Violent_Local_Transitivity_CUG.jpg", sep = '')
jpeg(file = tmp_cug)
plot.cug.test(temp_cug)
dev.off()

temp_qap = myqaptest(violent_filter,transitivity, directed = FALSE, type = "local")

print("Local Transitivity QAP Test Results:")
print(summary.qaptest(temp_qap))
print("", quote=FALSE)

tmp_qap = paste(trans_path, "Violent_Local_Transitivity_QAP.jpg", sep = '')
jpeg(file = tmp_qap)
plot.qaptest(temp_qap)
dev.off()

v_global_trans = transitivity(violent_filter, type = "global")
print(paste("Global Transitivity:", v_global_trans))

temp_cug = mycugtest(violent_filter, transitivity, cmode = "edges", directed = FALSE, type = "global")

print("Global Transitivity CUG Test Results:")
print.cug.test(temp_cug)
print("", quote=FALSE)

tmp_cug = paste(trans_path, "Violent_Global_Transitivity_CUG.jpg", sep = '')
jpeg(file = tmp_cug)
plot.cug.test(temp_cug)
dev.off()

temp_qap = myqaptest(violent_filter, transitivity, directed = FALSE, type = "global")

print("Global Transitivity QAP Test Results:")
print(summary.qaptest(temp_qap))
print("", quote=FALSE)

tmp_qap = paste(trans_path, "Violent_Global_Transitivity_QAP.jpg", sep = '')
jpeg(file = tmp_qap)
plot.qaptest(temp_qap)
dev.off()

sink()

close(outFile)
closeAllConnections()


out_file_name = paste(trans_path, "NonViolent_Transitivity.txt", sep = '')
outFile = file(out_file_name, open="wt")
sink(file = outFile, append = TRUE)

v_local_trans = mean(transitivity(nonviolent_filter, type = "local"))
print(paste("Mean Local Transitivity:", v_local_trans))

temp_cug = mycugtest(nonviolent_filter, transitivity, cmode = "edges", directed = FALSE, type = "local")

print("Local Transitivity CUG Test Results:")
print.cug.test(temp_cug)
print("", quote=FALSE)

tmp_cug = paste(trans_path, "NonViolent_Local_Transitivity_CUG.jpg", sep = '')
jpeg(file = tmp_cug)
plot.cug.test(temp_cug)
dev.off()

temp_qap = myqaptest(nonviolent_filter,transitivity, directed = FALSE, type = "local")

print("Local Transitivity QAP Test Results:")
print(summary.qaptest(temp_qap))
print("", quote=FALSE)

tmp_qap = paste(trans_path, "NonViolent_Local_Transitivity_QAP.jpg", sep = '')
jpeg(file = tmp_qap)
plot.qaptest(temp_qap)
dev.off()

v_global_trans = transitivity(nonviolent_filter, type = "global")
print(paste("Global Transitivity:", v_global_trans))

temp_cug = mycugtest(nonviolent_filter, transitivity, cmode = "edges", directed = FALSE, type = "global")

print("Global Transitivity CUG Test Results:")
print.cug.test(temp_cug)
print("", quote=FALSE)

tmp_cug = paste(trans_path, "NonViolent_Global_Transitivity_CUG.jpg", sep = '')
jpeg(file = tmp_cug)
plot.cug.test(temp_cug)
dev.off()

temp_qap = myqaptest(nonviolent_filter, transitivity, directed = FALSE, type = "global")

print("Global Transitivity QAP Test Results:")
print(summary.qaptest(temp_qap))
print("", quote=FALSE)

tmp_qap = paste(trans_path, "NonViolent_Global_Transitivity_QAP.jpg", sep = '')
jpeg(file = tmp_qap)
plot.qaptest(temp_qap)
dev.off()

sink()

close(outFile)
closeAllConnections()

###################### Community Detection #####################################################

set.seed(20170423)

out_file_name = paste(output_path, "Graph_Modularities.txt", sep = '')
outFile = file(out_file_name, open="wt")
sink(file = outFile, append = TRUE)

# Violent community detection
comm.sg = cluster_spinglass(violent_filter)
V(violent_filter)$comm_sg = comm.sg$membership

comm.lv = cluster_louvain(violent_filter)
V(violent_filter)$comm_lv = comm.lv$membership

comm.le = cluster_leading_eigen(violent_filter)
V(violent_filter)$comm_le = comm.le$membership

comm.fg = cluster_fast_greedy(violent_filter)
V(violent_filter)$comm_fg = comm.fg$membership

comm.bt = cluster_edge_betweenness(violent_filter, weights = NULL)
V(violent_filter)$comm_bt = comm.bt$membership

comm.wt5 = cluster_walktrap(violent_filter, steps = 2)
V(violent_filter)$comm_wt5 = comm.wt5$membership

comm.wt10 = cluster_walktrap(violent_filter, steps = 3)
V(violent_filter)$comm_wt10 = comm.wt10$membership

comm.mod = lapply(list(comm.sg, comm.lv, comm.le, comm.fg, comm.bt, comm.wt5, comm.wt10), modularity)
comm.len = lapply(list(comm.sg, comm.lv, comm.le, comm.fg, comm.bt, comm.wt5, comm.wt10), length)

print(comm.mod)
print(comm.len)

algorithms = c("SG", "LV", "LE", "FG", "BT", "WT2", "WT3")
comm_df = data.frame(Modularity = as.vector(comm.mod, mode = "numeric"),
                     Length = as.vector(comm.len, mode = "numeric"), Algorithm = algorithms)

# Create bar chart of lengths
temp_jpg = paste(output_path, "Violent_Cluster_Sizes.jpg", sep = "")
jpeg(file = temp_jpg)

p = ggplot(data = comm_df, aes(x = Algorithm, y = Length, fill = Algorithm))
p = p + geom_bar(stat = "identity")
p = p + geom_text(aes(label=round(Length, 4)), vjust=0) 
p = p + ggtitle("Number of Community Clusters By Algorithm")
p = p + labs(x = "Community Detection Algorithm", y = "Number of Clusters (Length)")
print(p)
dev.off()


# NonViolent community detection
commNV.sg = cluster_spinglass(nonviolent_filter)
V(nonviolent_filter)$comm_sg = commNV.sg$membership

commNV.lv = cluster_louvain(nonviolent_filter)
V(nonviolent_filter)$comm_lv = commNV.lv$membership

commNV.le = cluster_leading_eigen(nonviolent_filter)
V(nonviolent_filter)$comm_le = commNV.le$membership

commNV.fg = cluster_fast_greedy(nonviolent_filter)
V(nonviolent_filter)$comm_fg = commNV.fg$membership

commNV.bt = cluster_edge_betweenness(nonviolent_filter, weights = NULL)
V(nonviolent_filter)$comm_bt = commNV.bt$membership

commNV.wt5 = cluster_walktrap(nonviolent_filter, steps = 2)
V(nonviolent_filter)$comm_wt5 = commNV.wt5$membership

commNV.wt10 = cluster_walktrap(nonviolent_filter, steps = 3)
V(nonviolent_filter)$comm_wt10 = commNV.wt10$membership

commNV.mod = lapply(list(commNV.sg, commNV.lv, commNV.le, commNV.fg, commNV.bt, commNV.wt5, commNV.wt10), modularity)
commNV.len = lapply(list(commNV.sg, commNV.lv, commNV.le, commNV.fg, commNV.bt, commNV.wt5, commNV.wt10), length)

print(commNV.mod)
print(commNV.len)

algorithms = c("SG", "LV", "LE", "FG", "BT", "WT2", "WT3")
comm_df = data.frame(Modularity = as.vector(commNV.mod, mode = "numeric"),
                     Length = as.vector(commNV.len, mode = "numeric"), Algorithm = algorithms)

# Create bar chart of lengths
temp_jpg = paste(output_path, "NonViolent_Cluster_Sizes.jpg", sep = "")
jpeg(file = temp_jpg)

p = ggplot(data = comm_df, aes(x = Algorithm, y = Length, fill = Algorithm))
p = p + geom_bar(stat = "identity")
p = p + geom_text(aes(label=round(Length, 4)), vjust=0) 
p = p + ggtitle("Number of Community Clusters By Algorithm")
p = p + labs(x = "Community Detection Algorithm", y = "Number of Clusters (Length)")
print(p)
dev.off()

sink()

close(outFile)
closeAllConnections()


# Save updated files
out_file_violent = paste(path, "feb_violent_crimes.graphml", sep = "")
write_graph(violent_filter, out_file_violent, format = "graphml")

out_file_nonviolent = paste(path, "feb_nonviolent_crimes.graphml", sep = "")
write_graph(nonviolent_filter, out_file_nonviolent, format = "graphml")



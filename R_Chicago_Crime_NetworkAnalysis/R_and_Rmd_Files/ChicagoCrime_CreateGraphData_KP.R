######################################################################################################
# 
# ChicagoCrime_CreateSGraphData.R
# 
# The file creates the graphml data from the initial chicago 2017 crime dataset and the Chicago 2017
# census data.  The census entries for each neighborhood chosen are assigned to be the graph data 
# node attributes.  Three different types of graph files are generated, one with only violent crime,
# one with only non-violent crime, and one with all crime.  Graph data for several different months
# was generated. 
#
# Created By      Date
# ----------      ----
# Kari Palmier    6/2/2018
#
######################################################################################################

library("sand")
library("intergraph")

crime_file_path = "C:\\DePaulCoursework\\Spring CSC 495\\Project\\Crimes_to_2017.csv"
crime_df = read.csv(crime_file_path, header = T, sep = ",", row.names = 1, stringsAsFactors = FALSE)
head(crime_df)

info_file_path = "C:\\DePaulCoursework\\Spring CSC 495\\Project\\CCASF12010CMAP.csv"
info_df = read.csv(info_file_path, header = T, sep = ",", stringsAsFactors = FALSE)
head(info_df)

ID = info_df$GeogKey
Label = info_df$ï..Geog
type = rep(TRUE, nrow(info_df))
Prop.Occupied = info_df$Occupied.Housing.Units / info_df$Total.Housing.Units
Prop.Rented = info_df$Renter.occupied / info_df$Total.Housing.Units
Prop.Vacant = info_df$Vacant.Housing.Units / info_df$Total.Housing.Units
Prop.Owned = (info_df$Owned.with.a.mortgage.or.a.loan + info_df$Owned.free.and.clear) / info_df$Total.Housing.Units
Median.Age = info_df$Median.Age
Total.Population = info_df$Total.Population
Average.Household.Size = info_df$Average.Household.Size
Prop.African = info_df$Not.Hispanic.or.Latino..Black.or.African.American.alone / info_df$Total.Population
Prop.White = info_df$Not.Hispanic.or.Latino..White.alone / info_df$Total.Population
Prop.Hispanic = info_df$Hispanic.or.Latino / info_df$Total.Population
Prop.Asian = info_df$Not.Hispanic.or.Latino..Asian.alone / info_df$Total.Population
Community.Area = info_df$GeogKey
Crime.Type = rep("", nrow(info_df))
Crime.Desc = rep("", nrow(info_df))


node_df = data.frame(ID = ID, Label = Label, type = type, Prop.Occupied = Prop.Occupied, Prop.Rented = Prop.Rented, 
                     Prop.Vacant = Prop.Vacant, Prop.Owned = Prop.Owned, Median.Age = Median.Age, 
                     Total.Population = Total.Population, Average.Household.Size = Average.Household.Size,
                     Prop.African = Prop.African, Prop.White = Prop.White, 
                     Prop.Asian = Prop.Asian, Prop.Hispanic = Prop.Hispanic, 
                     Community.Area = Community.Area, Crime.Type = Crime.Type,
                     Crime.Desc = Crime.Desc, stringsAsFactors = FALSE)

violent_node_df = node_df
nonviolent_node_df = node_df

crime_types = unique(crime_df$Primary.Type)
crime_types

violent_crimes = c("CRIM SEXUAL ASSAULT", "BATTERY", "STALKING", "HUMAN TRAFFICKING", "WEAPONS VIOLATION",
                   "ASSAULT", "SEX OFFENSE", "HOMICIDE", "KIDNAPPING", "ROBBERY", "ARSON", "OFFENSE INVOLVING CHILDREN",
                   "INTIMIDATION", "INTERFERENCE WITH PUBLIC OFFICER")
nonviolent_crimes = c("DECEPTIVE PRACTICE", "THEFT", "MOTOR VEHICLE THEFT", "PUBLIC PEACE VIOLATION",
                      "LIQUOR LAW VIOLATION", "OBSCENITY", "CRIMINAL DAMAGE", "NARCOTICS", 
                      "CONCEALED CARRY LICENSE VIOLATION", "PUBLIC INDECENCY", "BURGLARY", "PROSTITUTION", 
                      "GAMBLING", "CRIMINAL TRESPASS")

num_violent = length(violent_crimes)
num_nonviolent = length(nonviolent_crimes)
violent_id_num = max(info_df$GeogKey) + 1
nonviolent_id_num = max(info_df$GeogKey) + 1
crime_id_num = max(info_df$GeogKey) + 1

for (i in 1:num_violent){
  Temp.ID = crime_id_num
  Temp.Label = violent_crimes[i]
  Temp.type = FALSE
  Temp.Prop.Occupied = NA
  Temp.Prop.Rented = NA
  Temp.Prop.Vacant = NA
  Temp.Prop.Owned = NA
  Temp.Median.Age = NA
  Temp.Total.Population = NA
  Temp.Average.Household.Size = NA
  Temp.Prop.African = NA
  Temp.Prop.White = NA
  Temp.Prop.Hispanic = NA
  Temp.Prop.Asian = NA
  Temp.Community.Area = NA
  Temp.Crime.Type = "VIOLENT"
  Temp.Crime.Desc = violent_crimes[i]
  
  temp_df = data.frame(ID = Temp.ID, Label = Temp.Label, type = Temp.type, Prop.Occupied = Temp.Prop.Occupied, 
                       Prop.Rented = Temp.Prop.Rented, 
                       Prop.Vacant = Temp.Prop.Vacant, Prop.Owned = Temp.Prop.Owned, Median.Age = Temp.Median.Age, 
                       Total.Population = Temp.Total.Population, 
                       Average.Household.Size = Temp.Average.Household.Size,
                       Prop.African = Temp.Prop.African, 
                       Prop.White = Temp.Prop.White, Prop.Asian = Temp.Prop.Asian, 
                       Prop.Hispanic = Temp.Prop.Hispanic,
                       Community.Area = Temp.Community.Area, Crime.Type = Temp.Crime.Type,
                       Crime.Desc = Temp.Crime.Desc, stringsAsFactors = FALSE)
  
  node_df = rbind(node_df, temp_df)
  violent_node_df = rbind(violent_node_df, temp_df)
  
  crime_id_num = crime_id_num + 1
}

for (i in 1:num_nonviolent){
  Temp.ID = crime_id_num
  Temp.Label = nonviolent_crimes[i]
  Temp.type = FALSE
  Temp.Prop.Occupied = NA
  Temp.Prop.Rented = NA
  Temp.Prop.Vacant = NA
  Temp.Prop.Owned = NA
  Temp.Median.Age = NA
  Temp.Total.Population = NA
  Temp.Average.Household.Size = NA
  Temp.Prop.African = NA
  Temp.Prop.White = NA
  Temp.Prop.Hispanic = NA
  Temp.Prop.Asian = NA
  Temp.Community.Area = NA
  Temp.Crime.Type = "NONVIOLENT"
  Temp.Crime.Desc = nonviolent_crimes[i]
  
  temp_df = data.frame(ID = Temp.ID, Label = Temp.Label, type = Temp.type, Prop.Occupied = Temp.Prop.Occupied, 
                       Prop.Rented = Temp.Prop.Rented, 
                       Prop.Vacant = Temp.Prop.Vacant, Prop.Owned = Temp.Prop.Owned, Median.Age = Temp.Median.Age, 
                       Total.Population = Temp.Total.Population, 
                       Average.Household.Size = Temp.Average.Household.Size,
                       Prop.African = Temp.Prop.African, 
                       Prop.White = Temp.Prop.White, Prop.Asian = Temp.Prop.Asian, 
                       Prop.Hispanic = Temp.Prop.Hispanic,
                       Community.Area = Temp.Community.Area, Crime.Type = Temp.Crime.Type,
                       Crime.Desc = Temp.Crime.Desc, stringsAsFactors = FALSE)
  
  node_df = rbind(node_df, temp_df)
  nonviolent_node_df = rbind(nonviolent_node_df, temp_df)
  
  crime_id_num = crime_id_num + 1
}

start_month = 6
stop_month = 8

summer_edges_df = data.frame(V1 = numeric(), V2 = numeric())
summer_violent_edges_df = data.frame(V1 = numeric(), V2 = numeric())
summer_nonviolent_edges_df = data.frame(V1 = numeric(), V2 = numeric())

june_edges_df = data.frame(V1 = numeric(), V2 = numeric())
june_violent_edges_df = data.frame(V1 = numeric(), V2 = numeric())
june_nonviolent_edges_df = data.frame(V1 = numeric(), V2 = numeric())

aug_edges_df = data.frame(V1 = numeric(), V2 = numeric())
aug_violent_edges_df = data.frame(V1 = numeric(), V2 = numeric())
aug_nonviolent_edges_df = data.frame(V1 = numeric(), V2 = numeric())

oct_edges_df = data.frame(V1 = numeric(), V2 = numeric())
oct_violent_edges_df = data.frame(V1 = numeric(), V2 = numeric())
oct_nonviolent_edges_df = data.frame(V1 = numeric(), V2 = numeric())

mar_edges_df = data.frame(V1 = numeric(), V2 = numeric())
mar_violent_edges_df = data.frame(V1 = numeric(), V2 = numeric())
mar_nonviolent_edges_df = data.frame(V1 = numeric(), V2 = numeric())

dec_edges_df = data.frame(V1 = numeric(), V2 = numeric())
dec_violent_edges_df = data.frame(V1 = numeric(), V2 = numeric())
dec_nonviolent_edges_df = data.frame(V1 = numeric(), V2 = numeric())

feb_edges_df = data.frame(V1 = numeric(), V2 = numeric())
feb_violent_edges_df = data.frame(V1 = numeric(), V2 = numeric())
feb_nonviolent_edges_df = data.frame(V1 = numeric(), V2 = numeric())

num_crimes = nrow(crime_df)
for (j in 1:num_crimes){
  
  temp_date = crime_df$Date[j]
  temp_month = as.numeric(substr(temp_date, 1, 2))
  
  community = crime_df$Community.Area[j]
  crime = crime_df$Primary.Type[j]
  geo_ndx = which(node_df$ID == community)
  
  if ((temp_month >= start_month) && (temp_month <= stop_month)){
    
    if (any(violent_crimes == crime)){
      
      crime_ndx = which(node_df$Crime.Desc == crime)
      temp_df = data.frame(V1 = node_df$ID[geo_ndx], V2 = node_df$ID[crime_ndx])
      summer_edges_df = rbind(summer_edges_df, temp_df)
      summer_violent_edges_df = rbind(summer_violent_edges_df, temp_df)
      
      if (temp_month == start_month){
        
        june_edges_df = rbind(june_edges_df, temp_df)
        june_violent_edges_df = rbind(june_violent_edges_df, temp_df)
        
      } else if (temp_month == stop_month){
        
        aug_edges_df = rbind(aug_edges_df, temp_df)
        aug_violent_edges_df = rbind(aug_violent_edges_df, temp_df)
        
      }
      
    } else if (any(nonviolent_crimes == crime)){
      
      crime_ndx = which(node_df$Crime.Desc == crime)
      temp_df = data.frame(V1 = node_df$ID[geo_ndx], V2 = node_df$ID[crime_ndx])
      summer_edges_df = rbind(summer_edges_df, temp_df)
      summer_nonviolent_edges_df = rbind(summer_nonviolent_edges_df, temp_df)
      
      if (temp_month == start_month){
        
        june_edges_df = rbind(june_edges_df, temp_df)
        june_nonviolent_edges_df = rbind(june_nonviolent_edges_df, temp_df)
        
      } else if (temp_month == stop_month){
        
        aug_edges_df = rbind(aug_edges_df, temp_df)
        aug_nonviolent_edges_df = rbind(aug_nonviolent_edges_df, temp_df)
        
      }
      
    }
    
  } else if (temp_month == 10){
    
    if (any(violent_crimes == crime)){
      
      crime_ndx = which(node_df$Crime.Desc == crime)
      temp_df = data.frame(V1 = node_df$ID[geo_ndx], V2 = node_df$ID[crime_ndx])
      oct_edges_df = rbind(oct_edges_df, temp_df)
      oct_violent_edges_df = rbind(oct_violent_edges_df, temp_df)
    
    } else if (any(nonviolent_crimes == crime)){
      
      crime_ndx = which(node_df$Crime.Desc == crime)
      temp_df = data.frame(V1 = node_df$ID[geo_ndx], V2 = node_df$ID[crime_ndx])
      oct_edges_df = rbind(oct_edges_df, temp_df)
      oct_nonviolent_edges_df = rbind(oct_nonviolent_edges_df, temp_df)
      
    } 
    
  } else if (temp_month == 3){
      
    if (any(violent_crimes == crime)){
      
      crime_ndx = which(node_df$Crime.Desc == crime)
      temp_df = data.frame(V1 = node_df$ID[geo_ndx], V2 = node_df$ID[crime_ndx])
      mar_edges_df = rbind(mar_edges_df, temp_df)
      mar_violent_edges_df = rbind(mar_violent_edges_df, temp_df)
      
    } else if (any(nonviolent_crimes == crime)){
      
      crime_ndx = which(node_df$Crime.Desc == crime)
      temp_df = data.frame(V1 = node_df$ID[geo_ndx], V2 = node_df$ID[crime_ndx])
      mar_edges_df = rbind(mar_edges_df, temp_df)
      mar_nonviolent_edges_df = rbind(mar_nonviolent_edges_df, temp_df)
      
    }
    
  } else if (temp_month == 12){
    
    if (any(violent_crimes == crime)){
      
      crime_ndx = which(node_df$Crime.Desc == crime)
      temp_df = data.frame(V1 = node_df$ID[geo_ndx], V2 = node_df$ID[crime_ndx])
      dec_edges_df = rbind(dec_edges_df, temp_df)
      dec_violent_edges_df = rbind(dec_violent_edges_df, temp_df)
      
    } else if (any(nonviolent_crimes == crime)){
      
      crime_ndx = which(node_df$Crime.Desc == crime)
      temp_df = data.frame(V1 = node_df$ID[geo_ndx], V2 = node_df$ID[crime_ndx])
      dec_edges_df = rbind(dec_edges_df, temp_df)
      dec_nonviolent_edges_df = rbind(dec_nonviolent_edges_df, temp_df)
      
    } 
    
  } else if (temp_month == 2){
      
    if (any(violent_crimes == crime)){
      
      crime_ndx = which(node_df$Crime.Desc == crime)
      temp_df = data.frame(V1 = node_df$ID[geo_ndx], V2 = node_df$ID[crime_ndx])
      feb_edges_df = rbind(feb_edges_df, temp_df)
      feb_violent_edges_df = rbind(feb_violent_edges_df, temp_df)
      
    } else if (any(nonviolent_crimes == crime)){
      
      crime_ndx = which(node_df$Crime.Desc == crime)
      temp_df = data.frame(V1 = node_df$ID[geo_ndx], V2 = node_df$ID[crime_ndx])
      feb_edges_df = rbind(feb_edges_df, temp_df)
      feb_nonviolent_edges_df = rbind(feb_nonviolent_edges_df, temp_df)
      
    }
    
  }
  
}


dataframe_path = "C:\\DePaulCoursework\\Spring CSC 495\\Project\\Dataframe_Data\\"

write.csv(summer_edges_df, paste(dataframe_path, "summer_edges.csv", sep=""), row.names = FALSE)
write.csv(summer_violent_edges_df, paste(dataframe_path, "summer_violent_edges.csv", sep=""), row.names = FALSE)
write.csv(summer_nonviolent_edges_df, paste(dataframe_path, "summer_nonviolent_edges.csv", sep=""), row.names = FALSE)

write.csv(june_edges_df, paste(dataframe_path, "june_edges.csv", sep=""), row.names = FALSE)
write.csv(june_violent_edges_df, paste(dataframe_path, "june_violent_edges.csv", sep=""), row.names = FALSE)
write.csv(june_nonviolent_edges_df, paste(dataframe_path, "june_nonviolent_edges.csv", sep=""), row.names = FALSE)

write.csv(aug_edges_df, paste(dataframe_path, "aug_edges.csv", sep=""), row.names = FALSE)
write.csv(aug_violent_edges_df, paste(dataframe_path, "aug_violent_edges.csv", sep=""), row.names = FALSE)
write.csv(aug_nonviolent_edges_df, paste(dataframe_path, "aug_nonviolent_edges.csv", sep=""), row.names = FALSE)

write.csv(oct_edges_df, paste(dataframe_path, "oct_edges.csv", sep=""), row.names = FALSE)
write.csv(oct_violent_edges_df, paste(dataframe_path, "oct_violent_edges.csv", sep=""), row.names = FALSE)
write.csv(oct_nonviolent_edges_df, paste(dataframe_path, "oct_nonviolent_edges.csv", sep=""), row.names = FALSE)

write.csv(mar_edges_df, paste(dataframe_path, "mar_edges.csv", sep=""), row.names = FALSE)
write.csv(mar_violent_edges_df, paste(dataframe_path, "mar_violent_edges.csv", sep=""), row.names = FALSE)
write.csv(mar_nonviolent_edges_df, paste(dataframe_path, "mar_nonviolent_edges.csv", sep=""), row.names = FALSE)

write.csv(dec_edges_df, paste(dataframe_path, "dec_edges.csv", sep=""), row.names = FALSE)
write.csv(dec_violent_edges_df, paste(dataframe_path, "dec_violent_edges.csv", sep=""), row.names = FALSE)
write.csv(dec_nonviolent_edges_df, paste(dataframe_path, "dec_nonviolent_edges.csv", sep=""), row.names = FALSE)

write.csv(feb_edges_df, paste(dataframe_path, "feb_edges.csv", sep=""), row.names = FALSE)
write.csv(feb_violent_edges_df, paste(dataframe_path, "feb_violent_edges.csv", sep=""), row.names = FALSE)
write.csv(feb_nonviolent_edges_df, paste(dataframe_path, "feb_nonviolent_edges.csv", sep=""), row.names = FALSE)

write.csv(node_df, paste(dataframe_path, "all_nodes.csv", sep=""), row.names = FALSE)
write.csv(violent_node_df, paste(dataframe_path, "violent_nodes.csv", sep=""), row.names = FALSE)
write.csv(nonviolent_node_df, paste(dataframe_path, "nonviolent_nodes.csv", sep=""), row.names = FALSE)


summer_gr = graph_from_data_frame(summer_edges_df, vertices=node_df, directed=FALSE)
summer_violent_gr = graph_from_data_frame(summer_violent_edges_df, vertices=violent_node_df, directed=FALSE)
summer_nonviolent_gr = graph_from_data_frame(summer_nonviolent_edges_df, vertices=nonviolent_node_df, directed=FALSE)

june_gr = graph_from_data_frame(june_edges_df, vertices=node_df, directed=FALSE)
june_violent_gr = graph_from_data_frame(june_violent_edges_df, vertices=violent_node_df, directed=FALSE)
june_nonviolent_gr = graph_from_data_frame(june_nonviolent_edges_df, vertices=nonviolent_node_df, directed=FALSE)

aug_gr = graph_from_data_frame(aug_edges_df, vertices=node_df, directed=FALSE)
aug_violent_gr = graph_from_data_frame(aug_violent_edges_df, vertices=violent_node_df, directed=FALSE)
aug_nonviolent_gr = graph_from_data_frame(aug_nonviolent_edges_df, vertices=nonviolent_node_df, directed=FALSE)

oct_gr = graph_from_data_frame(oct_edges_df, vertices=node_df, directed=FALSE)
oct_violent_gr = graph_from_data_frame(oct_violent_edges_df, vertices=violent_node_df, directed=FALSE)
oct_nonviolent_gr = graph_from_data_frame(oct_nonviolent_edges_df, vertices=nonviolent_node_df, directed=FALSE)

mar_gr = graph_from_data_frame(mar_edges_df, vertices=node_df, directed=FALSE)
mar_violent_gr = graph_from_data_frame(mar_violent_edges_df, vertices=violent_node_df, directed=FALSE)
mar_nonviolent_gr = graph_from_data_frame(mar_nonviolent_edges_df, vertices=nonviolent_node_df, directed=FALSE)

dec_gr = graph_from_data_frame(dec_edges_df, vertices=node_df, directed=FALSE)
dec_violent_gr = graph_from_data_frame(dec_violent_edges_df, vertices=violent_node_df, directed=FALSE)
dec_nonviolent_gr = graph_from_data_frame(dec_nonviolent_edges_df, vertices=nonviolent_node_df, directed=FALSE)

feb_gr = graph_from_data_frame(feb_edges_df, vertices=node_df, directed=FALSE)
feb_violent_gr = graph_from_data_frame(feb_violent_edges_df, vertices=violent_node_df, directed=FALSE)
feb_nonviolent_gr = graph_from_data_frame(feb_nonviolent_edges_df, vertices=nonviolent_node_df, directed=FALSE)


graph_path = "C:\\DePaulCoursework\\Spring CSC 495\\Project\\Graph_Data\\"

write_graph(summer_gr, paste(graph_path, "summer_all.graphml", sep = ""), format = "graphml")
write_graph(summer_violent_gr, paste(graph_path, "summer_violent.graphml", sep = ""), format = "graphml")
write_graph(summer_nonviolent_gr, paste(graph_path, "summer_nonviolent.graphml", sep = ""), format = "graphml")

write_graph(june_gr, paste(graph_path, "june_all.graphml", sep = ""), format = "graphml")
write_graph(june_violent_gr, paste(graph_path, "june_violent.graphml", sep = ""), format = "graphml")
write_graph(june_nonviolent_gr, paste(graph_path, "june_nonviolent.graphml", sep = ""), format = "graphml")

write_graph(aug_gr, paste(graph_path, "aug_all.graphml", sep = ""), format = "graphml")
write_graph(aug_violent_gr, paste(graph_path, "aug_violent.graphml", sep = ""), format = "graphml")
write_graph(aug_nonviolent_gr, paste(graph_path, "aug_nonviolent.graphml", sep = ""), format = "graphml")

write_graph(oct_gr, paste(graph_path, "oct_all.graphml", sep = ""), format = "graphml")
write_graph(oct_violent_gr, paste(graph_path, "oct_violent.graphml", sep = ""), format = "graphml")
write_graph(oct_nonviolent_gr, paste(graph_path, "oct_nonviolent.graphml", sep = ""), format = "graphml")

write_graph(mar_gr, paste(graph_path, "mar_all.graphml", sep = ""), format = "graphml")
write_graph(mar_violent_gr, paste(graph_path, "mar_violent.graphml", sep = ""), format = "graphml")
write_graph(mar_nonviolent_gr, paste(graph_path, "mar_nonviolent.graphml", sep = ""), format = "graphml")

write_graph(dec_gr, paste(graph_path, "dec_all.graphml", sep = ""), format = "graphml")
write_graph(dec_violent_gr, paste(graph_path, "dec_violent.graphml", sep = ""), format = "graphml")
write_graph(dec_nonviolent_gr, paste(graph_path, "dec_nonviolent.graphml", sep = ""), format = "graphml")

write_graph(feb_gr, paste(graph_path, "feb_all.graphml", sep = ""), format = "graphml")
write_graph(feb_violent_gr, paste(graph_path, "feb_violent.graphml", sep = ""), format = "graphml")
write_graph(feb_nonviolent_gr, paste(graph_path, "feb_nonviolent.graphml", sep = ""), format = "graphml")


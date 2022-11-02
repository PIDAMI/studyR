load("data/lab1_e2.Rdata") 

merge <- function(data) {
  list_tables <- lapply(data, function(week_table) Reduce(c, list(week_table)))
  do.call(rbind, list_tables)
}

## right solution, readable and w/t tons of unnecessary code 
# library(dplyr)

# get_id <- function(all_data) {

#     merged_table <- merge(all_data)  
#     merged_table %>% group_by(id)  %>%
#     summarise(mean_temp = mean(temp),
#               count = n(),
#               .groups = 'drop') %>% filter(count == 7) %>% select(id, mean_temp)
# }
# get_id(all_data)



get_id <- function(all_data) {
  
  merged_table <- merge(all_data)
  
  tmp<-data.frame(id=integer(), total_temp=double(), count=integer())
  for (row in 1:nrow(merged_table)){
    entry <- merged_table[row,]
    if (nrow(tmp[which(tmp$id==entry$id),]) == 0){
      tmp[nrow(tmp)+1,] <- c(entry$id, entry$temp, 1)
    } else {
      prev_val <- tmp[which(tmp$id==entry$id),]
      tmp[which(tmp$id==entry$id),] <-c(prev_val$id, prev_val$total_temp + entry$temp, prev_val$count + 1) 
    }
  }
  
  full_week <- tmp[tmp$count==7,]
  data.frame(id = full_week$id, mean_temp = full_week$total_temp / 7)
}
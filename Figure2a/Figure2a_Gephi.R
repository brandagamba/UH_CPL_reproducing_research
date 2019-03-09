library(tidyverse)
library(splitstackshape)
setwd("C:/Users/Plata/Documents/Spring 2019/Project/UH_CPL_reproducing_research")
path <- "C:/Users/Plata/Documents/Spring 2019/Project/Materials"
nodes <- read.csv(paste(path,"/Faculty_GoogleScholar_Funding_Data_N4190.csv", sep=''), header = TRUE)
links <- read.csv(paste(path,"/GoogleScholar_paper_stats.csv", sep=''), header = FALSE)

## Transforming Data and building F-network (without pollinators)
colnames(links)<-c("google_id", "year", "citations", "coathor_codes")
colnames(nodes)[1]<-c("id")
colnames(links)[1]<-c("source")
colnames(links)[4]<-c("target")

# Remove non-Google IDs from link's target column
links = links %>% 
  mutate(target, target = strsplit(as.character(target), ",")) %>% 
  unnest(target) %>% 
  filter(!target %in% c(0, 1, 2))
# Remove self-referencing links (source <=> target)
links = links %>% filter(target != source)

# Add a new nodes column that indicates the first year that the faculty member became cross disciplinary (XD)
departments = nodes %>% select(id, dept)
colnames(departments) = c("source", "source_department")
links = links %>% inner_join(departments, by = "source")
colnames(departments) = c("target", "target_department")
links = links %>% inner_join(departments, by = "target")
links = links %>% mutate(XD = ifelse(source_department != target_department, "XD", as.character(source_department)))
first_xd_years = links %>% 
  arrange(year) %>% 
  group_by(source) %>% 
  filter(XD == "XD") %>% 
  slice(1) %>% 
  ungroup() %>%
  select(source, year)
colnames(first_xd_years) = c("id", "first_xd_year")
nodes = nodes %>% left_join(first_xd_years, by = "id")
#TODO: still missing some nodes where XDIndicator = 'XD' (first_xd_year = NA)
# although there are no non-XDIndicator nodes!

get_xd_department = function(first_xd_year, department, limit_year)
{
  return(ifelse(!is.na(first_xd_year) & first_xd_year <= limit_year, "XD", as.character(department)))
}

write.csv(links, file = "Fig2a_Links.csv", row.names=F)
nodes1 = nodes %>% mutate(xd_dept = get_xd_department(first_xd_year, dept, 1990))
write.csv(nodes1, file = "Fig2a_Nodes_1990.csv", row.names=F)
nodes1 = nodes %>% mutate(xd_dept = get_xd_department(first_xd_year, dept, 1995))
write.csv(nodes1, file = "Fig2a_Nodes_1995.csv", row.names=F)
nodes2 = nodes %>% mutate(xd_dept = get_xd_department(first_xd_year, dept, 2000))
write.csv(nodes2, file = "Fig2a_Nodes_2000.csv", row.names=F)
nodes3 = nodes %>% mutate(xd_dept = get_xd_department(first_xd_year, dept, 2005))
write.csv(nodes3, file = "Fig2a_Nodes_2005.csv", row.names=F)
nodes4 = nodes %>% mutate(xd_dept = get_xd_department(first_xd_year, dept, 2010))
write.csv(nodes4, file = "Fig2a_Nodes_2010.csv", row.names=F)
nodes5 = nodes %>% mutate(xd_dept = get_xd_department(first_xd_year, dept, 2015))
write.csv(nodes5, file = "Fig2a_Nodes_2015.csv", row.names=F)

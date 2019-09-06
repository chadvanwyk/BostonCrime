#Do nearest neighbour from latlong and create graph from that 
#Can use text to do whatever we need too 
#PLaces have things in common... Maybe infer?? 

library(dplyr)
library(tidyr)
library(ggplot2)
library(rlist)

data <- read.csv('./BostonCrime/crime.csv', sep = ",", na.strings =c('','NA','na','N/A','n/a','NaN','nan'), strip.white = TRUE, stringsAsFactors = FALSE)

############### CLEANING ###############

#The shooting column
count <- 0
for (x in data$SHOOTING) {
  if (is.na(x) == TRUE) {
    count = count + 1
  }
}
count
#So there are 326765 NA's where they should be N's

#Create new list assigning to N to empty obs
new_shooting <- c()

for (x in data$SHOOTING) {
  if (is.na(x) == TRUE) {
    new_shooting <- c(new_shooting, 'N')
  } else {
    new_shooting <- c(new_shooting, x)
  }
}

data$Shooting <- new_shooting
data$SHOOTING <- NULL
data <- data %>% rename(SHOOTING = Shooting)
data <- data %>% rename(LAT = Lat, LONG = Long, LOCATION = Location)

#How may NA's in the dataset
sum(is.na(data))

#Per column if you so wish
#These are the columns that contain NA's, unsure of what to do
sum(is.na(data$REPORTING_AREA))
sum(is.na(data$LAT))
sum(is.na(data$LONG))

#Summary view of missing data
#Problem area's are LAT, LONG (thus LOCATION), and REPORTING AREA
missing_data <- sort(sapply(data, function(x) sum(is.na(x))), decreasing = TRUE)

#Convert district code to district name
#Codes are uninformative
dist_names <- c()
for (x in data$DISTRICT) {
  if (is.na(x)) {
    dist_names <- c(dist_names, NA)
  } else if (x == "A1") {
    dist_names <- c(dist_names, "Downtown")
  } else if (x == "A15") {
    dist_names <- c(dist_names, "Charlestown")
  } else if (x == "A7") {
    dist_names <- c(dist_names, "East_Boston")
  } else if (x == "B2") {
    dist_names <- c(dist_names, "Roxbury")
  } else if (x == "B3") {
    dist_names <- c(dist_names, "Mattapan")
  } else if (x == "C6") {
    dist_names <- c(dist_names, "South_Boston")
  } else if (x == "C11") {
    dist_names <- c(dist_names, "Dorchester")
  } else if (x == "D4") {
    dist_names <- c(dist_names, "South_End")
  } else if (x == "D14") {
    dist_names <- c(dist_names, "Brightdown")
  } else if (x == "E5") {
    dist_names <- c(dist_names, "West_Roxbury")
  } else if (x == "E13") {
    dist_names <- c(dist_names, "Jamaica_Plain")
  } else if (x == "E18") {
    dist_names <- c(dist_names, "Hyde_Park")
  }
}

data$DISTRICT <- NULL
data$DISTRICT <- dist_names

############### EXPLORATORY DATA ANALYSIS ##################

#Number of Crimes for each year
no_crimes <- ggplot(data, aes(YEAR)) +
  geom_bar() 
no_crimes

#Number of Distinct Crimes
length(unique(data$OFFENSE_CODE_GROUP))

#The different crimes
unique(data$OFFENSE_CODE_GROUP)

#Crimes per offense group 
cpy <- data %>%
  group_by(OFFENSE_CODE_GROUP) %>%
  summarize(n())
cpy

#Crimes per group per district
cpgpd <- data %>%
  group_by(DISTRICT, OFFENSE_CODE_GROUP) %>%
  summarize(n())
cpgpd

#Graph the above 
test <- data %>% 
  group_by(DISTRICT, OFFENSE_CODE_GROUP) %>%
  filter(DISTRICT %in% c("West Roxbury", "Jamaica Plain", "Roxbury") & OFFENSE_CODE_GROUP %in% c("Manslaughter", "Robbery", "Towed")) %>%
  summarize(count = n())


ggplot(test) + 
  geom_point(aes(x = DISTRICT, y = OFFENSE_CODE_GROUP, size = count))


#Crimes per District per year
data %>%
  group_by(YEAR, DISTRICT) %>%
  summarize(count = n()) %>% 
  ggplot(aes(x = YEAR, y = count)) +
  geom_bar(stat = "identity") +
  facet_wrap(~DISTRICT)

#Shootings per District per year
data %>% 
  filter(SHOOTING == "Y") %>%
  group_by(YEAR, DISTRICT) %>%
  summarise(count = n()) %>%
    ggplot(aes(x = (DISTRICT), y = count, color = DISTRICT)) + 
    geom_bar(stat = "identity") +
    facet_wrap(~YEAR)

#Robberies per year in Boston
data %>%
  filter(OFFENSE_CODE_GROUP == "Robbery") %>%
  group_by(YEAR) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = YEAR, y = count)) +
  geom_line()

#Robberies per district per year
data %>%
  filter(OFFENSE_CODE_GROUP == "Robbery") %>%
  group_by(YEAR, DISTRICT) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = YEAR, y = count)) +
  geom_line() +
  facet_wrap(~DISTRICT)

#Some stuff Malcolm and I were doing
data %>%
  drop_na() %>%
  group_by(DISTRICT) %>%
  filter(OFFENSE_CODE_GROUP %in% c("Robbery", "Firearm Violations")) %>%
  ggplot(aes(x = LAT, y = LONG, color = OFFENSE_CODE_GROUP)) +
  geom_point() +
  facet_wrap(~YEAR)
  
data %>%
  drop_na() %>%
  group_by(DISTRICT) %>%
  filter(YEAR == "2017" & OFFENSE_CODE_GROUP == "Fraud") %>%
  summarise(count = n())

#Looking for a relationship between Firearm Violations and Robbery
data %>%
  drop_na() %>% 
  group_by(DISTRICT, OFFENSE_CODE_GROUP, YEAR) %>% 
  filter(OFFENSE_CODE_GROUP %in% c("Robbery", "Firearm Violations")) %>%
  summarize(count = n()) %>% 
  ggplot(aes(x = YEAR, y = count, color = OFFENSE_CODE_GROUP)) +
  geom_line() +
  facet_wrap(~DISTRICT)


################### FINDING CLUSTERS ###################


#Data Prep
Rob_Hyde <- data %>% 
            na.omit() %>% 
            filter(DISTRICT == "Hyde Park" & YEAR == "2015" & OFFENSE_CODE_GROUP == "Robbery")

#Trying this day of the week thing
dow <- c()

for (x in Rob_Hyde$DAY_OF_WEEK) {
  if (x == "Monday") {
    dow <- c(dow, 1)
  } else if (x == "Tuesday") {
    dow <- c(dow, 2)
  } else if (x == "Wednesday") {
    dow <- c(dow, 3)
  } else if (x == "Thursday") {
    dow <- c(dow, 4)
  } else if (x == "Friday") {
    dow <- c(dow, 5)
  } else if (x == "Saturday") {
    dow <- c(dow, 6)
  } else if (x == "Sunday") {
    dow <- c(dow, 7)
  }
}

Rob_Hyde$DOW <- dow

#K-means
krd <- Rob_Hyde %>% 
       select("LAT", "LONG") %>% 
       kmeans(centers = 2)

#Plot to see clusters 
plot(Rob_Hyde$LAT, Rob_Hyde$LONG, col = krd$cluster)

#Make a dataframe out of output
testicle <- data.frame(krd$cluster)
testicle <- tibble::rowid_to_column(testicle, "ID")
testicle <- testicle %>% rename(CLUSTER = krd.cluster)
testicle$VALUE <- 1

library(reshape2)
net <- dcast(testicle, ID ~ CLUSTER)
net[is.na(net)] <- 0

#Aldu
adlu <- net %>% gather(key = "cl2", value = "val", 2:3) %>% filter(val != 0) %>% select(-val)
adlu %>% as_tbl_graph() %>% plot()

#Create graph object
library(igraph)
net <- net %>% select("ID", "1")

obj <- graph_from_incidence_matrix(net, directed = FALSE, 
             mode = c("all", "out", "in", "total"),
             weighted = NULL, add.names = NULL)

plot(obj, vertex.color = "grey75")


library(tidygraph)
net %>% as_tbl_graph() %>% plot()

#Trying tibble graphs 

rht <- Rob_Hyde %>% select("DAY_OF_WEEK", "OFFENSE_CODE_GROUP")
gobj <- as_tbl_graph(rht)
plot(gobj)


##################### TRYING NEW CLUSTER SHIT ######################


#This is lowkey stupid
day_larc <- data %>% 
  na.omit() %>%
  filter(LONG != "-1" & YEAR == "2016" & DISTRICT == "Downtown" & OFFENSE_CODE_GROUP == "Larceny") %>% 
  #ggplot(aes(x = LAT, y = LONG)) + 
  #geom_point()
  select("DOW","OFFENSE_CODE_GROUP")

dow <- c()

for (x in day_larc$DAY_OF_WEEK) {
  if (x == "Monday") {
    dow <- c(dow, 1)
  } else if (x == "Tuesday") {
    dow <- c(dow, 2)
  } else if (x == "Wednesday") {
    dow <- c(dow, 3)
  } else if (x == "Thursday") {
    dow <- c(dow, 4)
  } else if (x == "Friday") {
    dow <- c(dow, 5)
  } else if (x == "Saturday") {
    dow <- c(dow, 6)
  } else if (x == "Sunday") {
    dow <- c(dow, 7)
  }
}

day_larc$DOW <- dow
day_larc <- tibble::rowid_to_column(day_larc, "ID")
day_larc <- day_larc %>% select("ID", "DOW")

day_larc %>% as_tbl_graph(directed = FALSE) %>% plot()

plot(day_larc)


##################### TRYING EVEN NEWER STUFF #########################

#Data prep
severe <- data %>% 
          na.omit() %>% 
          filter (DISTRICT %in% c("Downtown", "South End", "Roxbury") & 
                  OFFENSE_CODE_GROUP %in% c("Aggravated Assault", "Robbery") & 
                  YEAR == "2017" & 
                  LAT != "-1" ) 
          #ggplot(aes(x = LAT, y = LONG)) +
          #geom_point()

#Kmeans Clustering
clust_severe <- severe %>% 
                 select("LAT", "LONG") %>% 
                 kmeans(centers = 5)

clust_severe6 <- severe %>% 
                  select("LAT", "LONG") %>% 
                  kmeans(centers = 6)

clust_severe7 <- severe %>% 
                  select("LAT", "LONG") %>% 
                  kmeans(centers = 7)

#Plot to see clusters 
plot(severe$LAT, severe$LONG, col = clust_severe$cluster)

#Make dataframe
sev_dat <- data.frame(Crime = severe$OFFENSE_CODE_GROUP, 
                      Cluster_5 = clust_severe$cluster, 
                      Cluster_6 = clust_severe6$cluster,
                      Cluster_7 = clust_severe7$cluster)
sev_dat <- tibble::rowid_to_column(sev_dat, "ID")

#Create matrix
count <- 1
yes <- c()
for (x in sev_dat$Cluster_5) {
  if (x == sev_dat$Cluster_6[count]) {
    yes <- c(yes, 1)
  } else {
    yes <- c(yes, 0)
  }
  count <- count + 1
}

sev_dat$Link <- yes

test <- dcast(test, ID ~ Cluster_5)
test[is.na(test)] <- 0  

#Graph Object 
obj <- graph_from_incidence_matrix(test, directed = FALSE,
                                   weighted = T, add.names = NULL)
plot(obj)

write_graph(obj, "./object.graphml", format = c("graphml"))


######################### SOME NEWER SHIT ########################

#Take stuff out that you want to graph
#Cast Sparse to spread
#Graph from incidence matrix
#Bipartite (fuck with WHICH)

library(tidytext)
library(ggraph)

#Data prep
severest <- data %>% 
            na.omit() %>%
            filter(UCR_PART == "Part One" &
            YEAR == "2017" & 
            LAT != "-1" ) %>% 
            select("DISTRICT", "UCR_PART") %>% 
            as_tbl_graph()  

#Bipartite
bpgraph <- severest %>% 
           cast_sparse(DISTRICT, UCR_PART) %>%
           graph_from_incidence_matrix(directed = FALSE) %>% 
           bipartite_projection(multiplicity = TRUE, which = FALSE) %>% 
           ggraph("stress") +
           geom_edge_link(aes(width = weight)) + 
           geom_node_point(aes(size = deg))
           

plot(bpgraph)

#Write to dtop
write_graph(bpgraph, "./bpgraph.graphml", format = c("graphml"))
  

############################### TOP CRIME PER DISTRICT ##############################

crimes_p_dist <- data %>% 
                  na.omit() %>% 
                  filter(LAT != "-1") %>% 
                  group_by(DISTRICT, YEAR) %>% 
                  summarise(count = n()) %>% 
                  ggplot(aes(x = DISTRICT, y = count, fill = DISTRICT)) +
                  geom_bar(color = "black", stat = "identity") +
                  geom_text(aes(label = count), vjust = 1.6, color = "white") +
                  ggtitle("Total Crimes per District") +
                  xlab("Districts") + 
                  ylab("Count") +
                  theme_minimal(base_family = "sans") +
                  theme(axis.text.x=element_blank(),
                        axis.ticks.x=element_blank(),
                        axis.line = element_line(colour = "black")) +
                  facet_wrap(~YEAR)


############################### WHAT IS THE MOST DANGEROUS HOUR ##############################

data %>% 
  na.omit() %>% 
  filter(LAT != "-1") %>% 
  group_by(DAY_OF_WEEK, HOUR) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = HOUR, y = count)) +
  geom_segment(aes(x = HOUR, xend = HOUR, y = 0, yend = count), color = "skyblue") +
  geom_point(color = "blue", size = 4) +
  ggtitle("Crimes per Hour") +
  xlab("Hour") + 
  ylab("Count") +
  theme_light(base_family = "sans") +
  theme(panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank()) +
  facet_wrap(~DAY_OF_WEEK)


######################## TOTAL CRIMES PER WEEK #######################

crime_per_day <- data %>%
  group_by(DAY_OF_WEEK, OFFENSE_CODE_GROUP) %>% 
  add_count(OFFENSE_CODE_GROUP,DAY_OF_WEEK) %>%
  summarize(n = sum(n))

crime_per_day %>% 
  filter(n > 2500)%>%
  ggplot()+
  aes(x = DAY_OF_WEEK, y = n) +
  geom_col(position = "dodge2")+ggtitle("Total Number of Crimes per Week Day")+
  labs(fill = "DAY_OF_WEEK") +
  scale_fill_futurama()+
  theme_tufte(base_family = "sans") +
  xlab("Week Day") + 
  ylab("Number of Crimes")+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")

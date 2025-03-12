# Data analysis for "Timing and behavioral situation of social interactions affect the relationship between social integration and fitness"
# code and data prepared by Emily C. Laub (eclaub@g.ucla.edu; eclaub@umich.edu)



# libraries

library(igraph)
library(dplyr)
library(tidyr)
library(lubridate)
library(lme4)
library(car)
library(performance)
library(ggplot2)



# Data files
Clean_graph <- read.csv("Day_interactions_2021_cleaned_20250311.csv") # Interactions during day associations
roosting_frame <- read.csv("Roosting_interactions_2021_cleaned_20250311.csv") # Interactions during day associations

date_summaries <- read.csv("Number_observations_wasps_2021_20250311.csv") # Number of observations of wasps across days, alone or together

attributes_2021 <- read.csv("Wasp_attributes_2021_20250311.csv") # individual wasp attributes including ID, Weight, nest choice. 




##### First get data network data for wasps in both roosting and day association situations
# This runs a for loop to get node attributes for both behavioral situations, for each period.
# get a dataframe that has date, round, period, initiator, recipient, weight.

unqper <- unique(Clean_graph$Period) # yes says 1, 2, 3
results <- vector(mode = "list", length = length(unqper))
results <- list()
# list of dataframes measures output <- list()
for (i in seq_along(unqper)){
  data_ix <- Clean_graph[Clean_graph$Period == unqper[i],]
  data_ix <- data_ix[, 2:3] # only nodes
  data_ix_roost <- roosting_frame[roosting_frame$Period == unqper[i],]
  data_ix_roost <- data_ix_roost[, 4:5] # only nodes
  
  # Graph association
  Layer_Asso <- graph_from_data_frame(data_ix,directed = F)
  E(Layer_Asso)$weight = 1
  Layer_Ass = igraph::simplify(Layer_Asso, edge.attr.comb = "sum", remove.loops = TRUE)# now we have association network for selected Period
  Edgelist_Ass<- cbind(as_edgelist(Layer_Ass), E(Layer_Ass)$weight) # This is to get the output we need for MuxViz
  colnames(Edgelist_Ass) <- c('Initiator','Recipient','weight')
  # Graph roosting
  Layer_roost <-graph_from_data_frame(data_ix_roost,directed = F)
  E(Layer_roost)$weight = 1
  Layer_roosting = igraph::simplify(Layer_roost, edge.attr.comb = "sum", remove.loops = TRUE) # now we have association network for selected Period
  Edgelist_roosting<- cbind(as_edgelist(Layer_roosting), E(Layer_roosting)$weight) # This is to get the output we need for MuxViz
  colnames(Edgelist_roosting) <- c('Initiator','Recipient','weight') # For Mux
  
  # make aggregate
  aggregate <- rbind(Edgelist_Ass, Edgelist_roosting)
  aggregate_net <-graph_from_data_frame(aggregate[,1:2], directed = F)
  E(aggregate_net)$weight <- as.numeric(aggregate[,3])
  aggregate_network = igraph::simplify(aggregate_net, edge.attr.comb = "sum") 
  
  
  # get degree/strength association
  
  V(Layer_Ass)$Association_degree <- degree(Layer_Ass, v = V(Layer_Ass), normalized = FALSE)
  V(Layer_Ass)$Association_degree_norm <- degree(Layer_Ass, v = V(Layer_Ass), normalized = TRUE)
  V(Layer_Ass)$Association_strength <- strength(Layer_Ass, v = V(Layer_Ass))
  V(Layer_Ass)$Association_strength_norm <- (strength(Layer_Ass, v = V(Layer_Ass))/sum(strength(Layer_Ass)))
  
  # get degree/strength roosting
  V(Layer_roosting)$Roosting_degree <- degree(Layer_roosting, v = V(Layer_roosting), normalized = FALSE)
  V(Layer_roosting)$Roosting_degree_norm <- degree(Layer_roosting, v = V(Layer_roosting), normalized = TRUE)
  V(Layer_roosting)$Roosting_strength <- strength(Layer_roosting, v = V(Layer_roosting))
  V(Layer_roosting)$Roosting_strength_norm <- (strength(Layer_roosting, v = V(Layer_roosting))/sum(strength(Layer_roosting)))
  
  # get degree/strength aggregate
  V(aggregate_network)$Aggregate_degree <- degree(aggregate_network, v = V(aggregate_network), normalized = FALSE)
  V(aggregate_network)$Aggregate_degree_norm <- degree(aggregate_network, v = V(aggregate_network), normalized = TRUE)
  V(aggregate_network)$Aggregate_strength <- strength(aggregate_network, v = V(aggregate_network))
  V(aggregate_network)$Aggregate_strength_norm <- (strength(aggregate_network, v = V(aggregate_network))/sum(strength(aggregate_network)))
  
  # Aggregate dataframe
  Aggregate_frame <- data.frame(matrix(ncol = 0, nrow = length(V(aggregate_network)))) # this is clumsy, but only way i can force vector attributes into a dataframe nicely
  Aggregate_frame$node <- V(aggregate_network)$name ### Node name. Need to left_join association and roosting to aggregate in case there are wasps missing in some net but not the other
  Aggregate_frame$Aggregate_degree_norm <- V(aggregate_network)$Aggregate_degree_norm 
  Aggregate_frame$Aggregate_degree <- V(aggregate_network)$Aggregate_degree
  Aggregate_frame$Aggregate_strength_norm <- V(aggregate_network)$Aggregate_strength_norm 
  Aggregate_frame$Aggregate_strength <- V(aggregate_network)$Aggregate_strength
  
  # Roosting dataframe
  Roosting_frame <- data.frame(matrix(ncol = 0, nrow = length(V(Layer_roosting)))) # this is clumsy, but only way i can force vector attributes into a dataframe nicely
  Roosting_frame$node <- V(Layer_roosting)$name ### Node name. Need to left_join association and roosting to aggregate in case there are wasps missing in some net but not the other
  Roosting_frame$Roosting_degree_norm <- V(Layer_roosting)$Roosting_degree_norm
  Roosting_frame$Roosting_degree <- V(Layer_roosting)$Roosting_degree 
  Roosting_frame$Roosting_strength_norm <- V(Layer_roosting)$Roosting_strength_norm 
  Roosting_frame$Roosting_strength <- V(Layer_roosting)$Roosting_strength
  
  # Association dataframe
  Association_frame <- data.frame(matrix(ncol = 0, nrow = length(V(Layer_Ass)))) # this is clumsy, but only way i can force vector attributes into a dataframe nicely
  Association_frame$node <- V(Layer_Ass)$name ### Node name. Need to left_join association and roosting to aggregate in case there are wasps missing in some net but not the other
  Association_frame$Association_degree_norm <- V(Layer_Ass)$Association_degree_norm
  Association_frame$Association_degree <- V(Layer_Ass)$Association_degree 
  Association_frame$Association_strength_norm <- V(Layer_Ass)$Association_strength_norm 
  Association_frame$Association_strength <- V(Layer_Ass)$Association_strength
  
  
  
  # Bind all of these data frames together, make sure to bind into aggregate bc it should include all the wasps. 
  All_data_period_i = 
    Aggregate_frame %>% 
    left_join(Roosting_frame) %>% 
    left_join(Association_frame) %>% 
    mutate(Period = unqper[i]) %>% 
    as.data.frame()
  
  
  
  results[[i]] <- All_data_period_i
}



outs <- do.call(rbind, results) # get the listed results in one BIG data frame. 
outs <- as.data.frame(outs)

All_multi =
  outs %>% 
  pivot_wider(names_prefix = "Period", names_from = Period, names_sep = "_", values_from = c(Aggregate_degree_norm, Aggregate_degree, Aggregate_strength_norm, Aggregate_strength, Roosting_degree_norm, Roosting_degree,
                                                                                             Roosting_strength_norm, Roosting_strength, Association_degree_norm, Association_degree, Association_strength_norm, Association_strength) ) %>% 
  as.data.frame()


########## This is actually ok, bc has all wasps in network. This is fine. 
head(All_multi)
head(attributes_2021)

All_in =
  attributes_2021 %>% 
  left_join(All_multi, by = c("ID" = "node")) %>% 
  as.data.frame()




##### THEN GET STUFF TOGETHER FOR NEST CHOICE
head(All_in)
nest_ny = 
  All_in %>% 
  mutate(nest_num = case_when(
    nest_choice == "y" ~ 1, # This is to create a binary Y/N for the model. 
    TRUE ~ 0
  ))%>% 
  as.data.frame()



# ok now get the 0 things going. This is for wasps that were not observed with partners, should have degree of 0

nest_ny =
  nest_ny %>% 
  mutate(Roosting_degree_norm_Period1_clean = case_when(
    is.na(Roosting_degree_norm_Period1) == TRUE ~ 0,
    TRUE ~ Roosting_degree_norm_Period1 )) %>% 
  mutate(Roosting_strength_norm_Period1_clean = case_when(
    is.na(Roosting_strength_norm_Period1) == TRUE ~ 0,
    TRUE ~ Roosting_strength_norm_Period1 )) %>% 
  mutate(Association_strength_norm_Period1_clean = case_when(
    is.na(Association_strength_norm_Period1) == TRUE ~ 0,
    TRUE ~ Association_strength_norm_Period1 )) %>% 
  mutate(Association_degree_norm_Period1_clean = case_when(
    is.na(Association_degree_norm_Period1) == TRUE ~ 0,
    TRUE ~ Association_degree_norm_Period1 )) %>% 
  select(-(Aggregate_degree_norm_Period1:Aggregate_strength_Period3)) %>% 
  as.data.frame()

# Find wasps observed in the vespiary for fewer a day length of 5 days. 

removes = 
  date_summaries %>% 
  filter(length_obs < 5) %>% 
  as.data.frame()

View(removes)

# Remove wasps never observed at all and wasps that were observed in vespiary fewer than 5 days
nest_ny =
  nest_ny %>% 
  filter(!(ID %in% c("MN24", "MN52", "MN72", "MN74"))) %>% # wasps never observed at all
  filter(!(ID %in% c("MN25", "MN34", "MN38", "MN47", "MN49", "MN50", "MN53", "MN68", "MN75") )) %>% # wasps observed fewer than a day length of 5 days
  filter(!(Weight == "dead")) %>% 
  as.data.frame()


str(nest_ny)

 

### Analysis. 

################## nest ny

# degree association
nest_ny$Weight <- as.numeric(nest_ny$Weight)

mod2_nestny <- glm(nest_num ~ scale(Association_degree_norm_Period1_clean) 
                   +   Weight, data = nest_ny, family = binomial())
summary(mod2_nestny)
Anova(mod2_nestny)

# degree roosting

mod3_nestny <- glm(nest_num ~ scale(Roosting_degree_norm_Period1_clean) 
                   +  Weight, data = nest_ny, family = binomial())
summary(mod3_nestny)
Anova(mod3_nestny)

#strength association

mod2_nestny <- glm(nest_num ~ scale(Association_strength_norm_Period1_clean) 
                   +  Weight, data = nest_ny, family = binomial())
summary(mod2_nestny)
Anova(mod2_nestny)

# strength roosting
mod3_nestny <- glm(nest_num ~ scale(Roosting_strength_norm_Period1_clean) 
                   +  Weight, data = nest_ny, family = binomial())
summary(mod3_nestny)
Anova(mod3_nestny)


##################################################################################################
# OK. Now for dominant and alone wasps

# Start with All_in
# get dominant and alone
# left join the information of how many days wasps were observed. 


dom_alone = 
  All_in %>% 
  filter(dominance == "d" | dominance == "a") %>% 
  filter(!(ID == "MN15")) %>% # This wasp is removed because her nest was usurped by another, losing her nest. 
  left_join(date_summaries, by = c('ID' = "ID.y")) %>% 
  as.data.frame()



dom_alone_clean = ##### This is to figure out if wasps should recieve an NA (gone or missing and couldn't participate, or a 0 (observed in vespiary, but didn't interact))
  dom_alone %>% 
  mutate(Roosting_degree_norm_Period1_clean = case_when(
    (ymd(last_day) > ymd('2021-06-08')) & is.na(Roosting_degree_norm_Period1) == TRUE ~ 0,
    TRUE ~ Roosting_degree_norm_Period1
  )) %>% 
  mutate(Association_degree_norm_Period1_clean = case_when(
    (ymd(last_day) > ymd('2021-06-08')) & is.na(Association_degree_norm_Period1) == TRUE ~ 0,
    TRUE ~ Association_degree_norm_Period1
  )) %>% 
  mutate(Roosting_strength_norm_Period1_clean = case_when(
    (ymd(last_day) > ymd('2021-06-08')) & is.na(Roosting_strength_norm_Period1) == TRUE ~ 0,
    TRUE ~ Roosting_strength_norm_Period1
  )) %>% 
  mutate(Association_strength_norm_Period1_clean = case_when(
    (ymd(last_day) > ymd('2021-06-08')) & is.na(Association_strength_norm_Period1) == TRUE ~ 0,
    TRUE ~ Association_strength_norm_Period1
  )) %>% 
  mutate(Roosting_degree_norm_Period2_clean = case_when(
    (ymd(last_day) > ymd('2021-06-17')) & is.na(Roosting_degree_norm_Period2) == TRUE ~ 0,
    TRUE ~ Roosting_degree_norm_Period2
  )) %>% 
  mutate(Association_degree_norm_Period2_clean = case_when(
    (ymd(last_day) > ymd('2021-06-17')) & is.na(Association_degree_norm_Period2) == TRUE ~ 0,
    TRUE ~ Association_degree_norm_Period2
  )) %>% 
  mutate(Roosting_strength_norm_Period2_clean = case_when(
    (ymd(last_day) > ymd('2021-06-17')) & is.na(Roosting_strength_norm_Period2) == TRUE ~ 0,
    TRUE ~ Roosting_strength_norm_Period2
  )) %>% 
  mutate(Association_strength_norm_Period2_clean = case_when(
    (ymd(last_day) > ymd('2021-06-17')) & is.na(Association_strength_norm_Period2) == TRUE ~ 0,
    TRUE ~ Association_strength_norm_Period2
  )) %>% 
  mutate(Roosting_degree_norm_Period3_clean = case_when(
    (ymd(last_day) > ymd('2021-07-02')) & is.na(Roosting_degree_norm_Period3) == TRUE ~ 0,
    TRUE ~ Roosting_degree_norm_Period3
  )) %>% 
  mutate(Association_degree_norm_Period3_clean = case_when(
    (ymd(last_day) > ymd('2021-07-02')) & is.na(Association_degree_norm_Period3) == TRUE ~ 0,
    TRUE ~ Association_degree_norm_Period3
  )) %>% 
  mutate(Roosting_strength_norm_Period3_clean = case_when(
    (ymd(last_day) > ymd('2021-07-02')) & is.na(Roosting_strength_norm_Period3) == TRUE ~ 0,
    TRUE ~ Roosting_strength_norm_Period3
  )) %>% 
  mutate(Association_strength_norm_Period3_clean = case_when(
    (ymd(last_day) > ymd('2021-07-02')) & is.na(Association_strength_norm_Period3) == TRUE ~ 0,
    TRUE ~ Association_strength_norm_Period3
  )) %>% 
  as.data.frame()

# remove the wasp with 0 cells
dom_alone_clean =
  dom_alone_clean %>% 
  filter(Cell_Count > 0) %>% 
  as.data.frame()



############ Run models
str(dom_alone_clean)
dom_alone_clean$Weight <- as.numeric(dom_alone_clean$Weight)
# Degree Roosting Period 1

r1_nest <- glm(Cell_Count ~ max_stable_group + Weight + Roosting_degree_norm_Period1_clean 
               , data = dom_alone_clean, family = poisson())


summary(r1_nest)
Anova(r1_nest)
performance::check_model(r1_nest)
r1_nest %>% check_model()
check_collinearity(r1_nest)
#check_collinearity



# Degree Association Period 1

a1_nest <- glm(Cell_Count ~ max_stable_group + Weight +  
                 Association_degree_norm_Period1_clean
               , data = dom_alone_clean, family = poisson())


summary(a1_nest)
Anova(a1_nest)
check_collinearity(a1_nest)


################################ Strength P1

# strength Roosting Period 1

rs1_nest <- glm(Cell_Count ~ max_stable_group + Weight + Roosting_strength_norm_Period1_clean 
                , data = dom_alone_clean, family = poisson())


summary(rs1_nest)
Anova(rs1_nest)
check_collinearity(r1_nest)

# Strength Association Period 1

as1_nest <- glm(Cell_Count ~  max_stable_group + Weight +  
                  Association_strength_norm_Period1_clean
                , data = dom_alone_clean, family = poisson())


summary(as1_nest)
Anova(as1_nest)
check_collinearity(as1_nest)

###############################################################################
# Period 2

# Degree Roosting Period 2

r2_nest <- glm(Cell_Count ~ max_stable_group + Weight + Roosting_degree_norm_Period2_clean 
               , data = dom_alone_clean, family = poisson())


summary(r2_nest)
Anova(r2_nest)
check_collinearity(r2_nest)



# Degree Association Period 2

a2_nest <- glm(Cell_Count ~ max_stable_group + Weight +  
                 Association_degree_norm_Period2_clean
               , data = dom_alone_clean, family = poisson())


summary(a2_nest)
Anova(a2_nest)
check_collinearity(a2_nest)


################################ Strength P2

# strength Roosting Period 2

rs2_nest <- glm(Cell_Count ~ max_stable_group + Weight + Roosting_strength_norm_Period2_clean 
                , data = dom_alone_clean, family = poisson())


summary(rs2_nest)
Anova(rs2_nest)
check_collinearity(rs2_nest)



# Strength Association Period 2

as2_nest <- glm(Cell_Count ~ max_stable_group + Weight +  
                  Association_strength_norm_Period2_clean
                , data = dom_alone_clean, family = poisson())


summary(as2_nest)
Anova(as2_nest)
check_collinearity(as2_nest)

################################################################################################################
################################################################################################################
# Period 3

# Degree Roosting Period 3

r3_nest <- glm(Cell_Count ~ max_stable_group + Weight + Roosting_degree_norm_Period3_clean 
               , data = dom_alone_clean, family = poisson())


summary(r3_nest)
Anova(r3_nest)
check_collinearity(r3_nest)



# Degree Association Period 3

a3_nest <- glm(Cell_Count ~ max_stable_group + Weight +  
                 Association_degree_norm_Period3_clean
               , data = dom_alone_clean, family = poisson())


summary(a3_nest)
Anova(a3_nest)
check_model(a3_nest)


################################ Strength P3

# strength Roosting Period 3

rs3_nest <- glm(Cell_Count ~ max_stable_group + Weight + Roosting_strength_norm_Period3_clean 
                , data = dom_alone_clean, family = poisson())


summary(rs3_nest)
Anova(rs3_nest)
check_collinearity(rs3_nest)
performance(rs3_nest)
check_model(rs3_nest)

# Strength Association Period 3

as3_nest <- glm(Cell_Count ~  max_stable_group + Weight +  
                  Association_strength_norm_Period3_clean
                , data = dom_alone_clean, family = poisson())


summary(as3_nest)
Anova(as3_nest)
check_collinearity(as3_nest)
check_model(as3_nest)


############################### GRAPHING

library(ggplot2)


##### Degree



ggplot(dom_alone_clean, aes(Roosting_degree_norm_Period1_clean, Cell_Count)) +
  xlab("Proportion of individuals") + ylab("Nest size (# cells)") +
  theme_mine2() +
  theme(axis.title.x = element_text(margin=margin(t= 6)), #add margin to x-axis title
        axis.title.y = element_text(margin=margin(r= 6))) +       #
  stat_smooth(method="glm", fill = "lightsteelblue1", color = "#6E7B8B", size = 2, alpha = .3, linetype = 1 ) +
  geom_point(color = 'lightsteelblue', alpha = .6, size = 4) 

ggplot(dom_alone_clean, aes(Roosting_degree_norm_Period2_clean, Cell_Count)) +
  xlab("Proportion of individuals") + ylab("Nest size (# cells)") +
  theme_mine2() +
  theme(axis.title.x = element_text(margin=margin(t= 6)), #add margin to x-axis title
        axis.title.y = element_text(margin=margin(r= 6))) +       #
  stat_smooth(method="glm", fill = "lightsteelblue1", color = "#6E7B8B", size = 2, alpha = .5, linetype = 1 ) +
  geom_point(color = 'lightsteelblue', alpha = .8, size = 4) 

ggplot(dom_alone_clean, aes(Roosting_degree_norm_Period3_clean, Cell_Count)) +
  xlab("Proportion of individuals") + ylab("Nest size (# cells)") +
  theme_mine2() +
  theme(axis.title.x = element_text(margin=margin(t= 6)), #add margin to x-axis title
        axis.title.y = element_text(margin=margin(r= 6))) +       #
  stat_smooth(method="glm", fill = "lightsteelblue1", color = "#6E7B8B", size = 2, alpha = .7, linetype = 2 ) +
  geom_point(color = 'lightsteelblue', alpha = 1, size = 4) 

# Association
stat_smooth(method="glm", fill = "lightgoldenrod1", color = "#8B814C", size = 2, alpha = .3, linetype = 2 ) +
  geom_point(color = 'lightgoldenrod3', alpha = .6, size = 2) 



ggplot(dom_alone_clean, aes(Association_degree_norm_Period1_clean, Cell_Count)) +
  xlab("Proportion of individuals") + ylab("Nest size (# cells)") +
  theme_mine2() +
  theme(axis.title.x = element_text(margin=margin(t= 6)), #add margin to x-axis title
        axis.title.y = element_text(margin=margin(r= 6))) +       #
  stat_smooth(method="glm", fill = "lightgoldenrod1", color = "#8B814C", size = 2, alpha = .3, linetype = 2 ) +
  geom_point(color = 'lightgoldenrod3', alpha = .6, size = 4)  

ggplot(dom_alone_clean, aes(Association_degree_norm_Period2_clean, Cell_Count)) +
  xlab("Proportion of individuals") + ylab("Nest size (# cells)") +
  theme_mine2() +
  theme(axis.title.x = element_text(margin=margin(t= 6)), #add margin to x-axis title
        axis.title.y = element_text(margin=margin(r= 6))) +       #
  stat_smooth(method="glm", fill = "lightgoldenrod1", color = "#8B814C", size = 2, alpha = .5, linetype = 1 ) +
  geom_point(color = 'lightgoldenrod3', alpha = .8, size = 4) 

ggplot(dom_alone_clean, aes(Association_degree_norm_Period3_clean, Cell_Count)) +
  xlab("Proportion of individuals") + ylab("Nest size (# cells)") +
  theme_mine2() +
  theme(axis.title.x = element_text(margin=margin(t= 6)), #add margin to x-axis title
        axis.title.y = element_text(margin=margin(r= 6))) +       #
  stat_smooth(method="glm", fill = "lightgoldenrod1", color = "#8B814C", size = 2, alpha = .7, linetype = 1 ) +
  geom_point(color = 'lightgoldenrod3', alpha = 1, size = 4)  

######################## Strength 

ggplot(dom_alone_clean, aes(Roosting_strength_norm_Period1_clean, Cell_Count)) +
  xlab("Proportion of interactions") + ylab("Nest size (# cells)") +
  theme_mine2() +
  theme(axis.title.x = element_text(margin=margin(t= 6)), #add margin to x-axis title
        axis.title.y = element_text(margin=margin(r= 6))) +       #
  stat_smooth(method="glm", fill = "steelblue2", color = "#36648B", size = 2, alpha = .3, linetype = 2 ) +
  geom_point(color = 'steelblue3', alpha = .6, size = 4)

ggplot(dom_alone_clean, aes(Roosting_strength_norm_Period2_clean, Cell_Count)) +
  xlab("Proportion of interactions") + ylab("Nest size (# cells)") +
  theme_mine2() +
  theme(axis.title.x = element_text(margin=margin(t= 6)), #add margin to x-axis title
        axis.title.y = element_text(margin=margin(r= 6))) +       #
  stat_smooth(method="glm", fill = "steelblue2", color = "#36648B", size = 2, alpha = .5, linetype = 1 ) +
  geom_point(color = 'steelblue3', alpha = .8, size = 4) 

ggplot(dom_alone_clean, aes(Roosting_strength_norm_Period3_clean, Cell_Count)) +
  xlab("Proportion of interactions") + ylab("Nest size (# cells)") +
  theme_mine2() +
  theme(axis.title.x = element_text(margin=margin(t= 6)), #add margin to x-axis title
        axis.title.y = element_text(margin=margin(r= 6))) +       #
  stat_smooth(method="glm", fill = "steelblue2", color = "#36648B", size = 2, alpha = .7, linetype = 1 ) +
  geom_point(color = 'steelblue3', alpha = 1, size = 4) 

# Association

ggplot(dom_alone_clean, aes(Association_strength_norm_Period1_clean, Cell_Count)) +
  xlab("Proportion of interactions") + ylab("Nest size (# cells)") +
  theme_mine2() +
  theme(axis.title.x = element_text(margin=margin(t= 6)), #add margin to x-axis title
        axis.title.y = element_text(margin=margin(r= 6))) +       #
  stat_smooth(method="glm", fill = "goldenrod1", color = "#8B6914", size = 2, alpha = .3, linetype = 2 ) +
  geom_point(color = 'goldenrod3', alpha = .6, size = 4)   

ggplot(dom_alone_clean, aes(Association_strength_norm_Period2_clean, Cell_Count)) +
  xlab("Proportion of interactions") + ylab("Nest size (# cells)") +
  theme_mine2() +
  theme(axis.title.x = element_text(margin=margin(t= 6)), #add margin to x-axis title
        axis.title.y = element_text(margin=margin(r= 6))) +       #
  stat_smooth(method="glm", fill = "goldenrod1", color = "#8B6914", size = 2, alpha = .5, linetype = 2 ) +
  geom_point(color = 'goldenrod3', alpha = .8, size = 4) 

ggplot(dom_alone_clean, aes(Association_strength_norm_Period3_clean, Cell_Count)) +
  xlab("Proportion of interactions") + ylab("Nest size (# cells)") +
  theme_mine2() +
  theme(axis.title.x = element_text(margin=margin(t= 6)), #add margin to x-axis title
        axis.title.y = element_text(margin=margin(r= 6))) +       #
  stat_smooth(method="glm", fill = "goldenrod1", color = "#8B6914", size = 2, alpha = .7, linetype = 1 ) +
  geom_point(color = 'goldenrod3', alpha = 1, size = 4) 


################################################################################################
# Binomial nest ny

r6_direct <- ggplot(nest_ny, aes(x =  Association_degree_norm_Period1_clean, y = as.numeric(nest_num))) +
  labs(x = "Number indivdiduals", y = "Nest choice") +
  geom_point(fill = "lightgoldenrod3", size = 10, alpha = 5/9, shape = 21, color = "black", stroke = 2) +
  scale_y_continuous(limits = c(min(nest_ny$nest_num), 
                                max(nest_ny$nest_num)),
                     breaks = c(0, 1),  labels = c("No", "Yes")) +
  stat_smooth(method = "glm", color = "#8B814C", se = T, fill = "lightgoldenrod1", alpha = 3/9,
              method.args = list(family = binomial), size = 1.5, linetype = 2) +
  theme(axis.title.x = element_text(color = "black", size = 35), 
        axis.title.y = element_text(color = "black", size = 35), 
        axis.text = element_text(color = "black", size = 28), 
        legend.position = "none",
        plot.background = element_rect(fill = 'white', color = NA),
        plot.margin = margin(.5,1.2,.1,.5, "cm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        panel.background = element_rect(fill = 'white', colour = "black"))


plot(r6_direct)


r5_direct <- ggplot(nest_ny, aes(x =  Roosting_degree_norm_Period1_clean, y = as.numeric(nest_num))) +
  labs(x = "Number indivdiduals", y = "Nest choice") +
  geom_point(fill = "lightsteelblue", size = 10, alpha = 5/9, shape = 21, color = "black", stroke = 2) +
  scale_y_continuous(limits = c(min(nest_ny$nest_num), 
                                max(nest_ny$nest_num)),
                     breaks = c(0, 1),  labels = c("No", "Yes")) +
  stat_smooth(method = "glm", color = "#6E7B8B", se = T, fill = "lightsteelblue1", alpha = 3/9,
              method.args = list(family = binomial), size = 1.5, linetype = 2) +
  theme(axis.title.x = element_text(color = "black", size = 35), 
        axis.title.y = element_text(color = "black", size = 35), 
        axis.text = element_text(color = "black", size = 28), 
        legend.position = "none",
        plot.background = element_rect(fill = 'white', color = NA),
        plot.margin = margin(.5,1.2,.1,.5, "cm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        panel.background = element_rect(fill = 'white', colour = "black"))


plot(r5_direct)







r8_direct <- ggplot(nest_ny, aes(x =  Association_strength_norm_Period1_clean, y = as.numeric(nest_num))) +
  labs(x = "Number interactions", y = "Nest choice") +
  geom_point(fill = 'goldenrod3', size = 10, alpha = 5/9, shape = 21, color = "black", stroke = 2) +
  scale_y_continuous(limits = c(min(nest_ny$nest_num), 
                                max(nest_ny$nest_num)),
                     breaks = c(0, 1),  labels = c("No", "Yes")) +
  stat_smooth(method = "glm", color = "#8B6914", se = T, fill = "goldenrod1", alpha = 3/9,
              method.args = list(family = binomial), linewidth = 1.5, linetype = 1) +
  theme(axis.title.x = element_text(color = "black", size = 35), 
        axis.title.y = element_text(color = "black", size = 35), 
        axis.text = element_text(color = "black", size = 28), 
        legend.position = "none",
        plot.background = element_rect(fill = 'white', color = NA),
        plot.margin = margin(.5,1.2,.1,.5, "cm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        panel.background = element_rect(fill = 'white', colour = "black"))


plot(r8_direct)

r7_direct <- ggplot(nest_ny, aes(x =  Roosting_strength_norm_Period1_clean, y = as.numeric(nest_num))) +
  labs(x = "Number interactions", y = "Nest choice") +
  geom_point(fill = "steelblue3", size = 10, alpha = 5/9, shape = 21, color = "black", stroke = 2) +
  scale_y_continuous(limits = c(min(nest_ny$nest_num), 
                                max(nest_ny$nest_num)),
                     breaks = c(0, 1),  labels = c("No", "Yes")) +
  stat_smooth(method = "glm", color = "#36648B", se = T, fill = "steelblue2", alpha = 3/9,
              method.args = list(family = binomial), size = 1.5, linetype = 2) +
  theme(axis.title.x = element_text(color = "black", size = 35), 
        axis.title.y = element_text(color = "black", size = 35), 
        axis.text = element_text(color = "black", size = 28), 
        legend.position = "none",
        plot.background = element_rect(fill = 'white', color = NA),
        plot.margin = margin(.5,1.2,.1,.5, "cm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        panel.background = element_rect(fill = 'white', colour = "black"))



plot(r7_direct)

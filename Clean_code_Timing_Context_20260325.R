# Code for analyzing: "Timing and behavioural situation of social interactions affect the relationship between sociality and fitness"
# Prepared by Emily C. Laub
# 20251215

library(dplyr)
library(lme4)
library(ggplot2)
library(readxl)
library(stringr)
library(lubridate)
library(igraph)
library(tidyr)
library(car)
library(performance)
library(ggeffects)
library(smplot2)

# data files
roosting_frame <- read.csv("Roosting_interactions_2021_cleaned_20250311.csv") # Interactions during Roosting 

date_summaries <- read.csv("Number_observations_wasps_2021_20250311.csv")# Number of observations of wasps across days, alone or together
attributes_2021 <- read.csv("attributes_2021_20251215.csv") # individual wasp attributes including ID, Weight, nest choice. 
Clean_graph <- read.csv("Day_association_20251212.csv") # Interactions during Day associations


# get a dataframe that has date, round, period, initiator, recipient, weight.

unqper <- unique(Clean_graph$Period) # yes says 1, 2, 3
results <- data.frame(matrix(ncol = 0, nrow = nrow(attributes_2021)) )
results$node <- attributes_2021$ID
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
  aggregate_network = igraph::simplify(aggregate_net, edge.attr.comb = "sum", remove.loops = TRUE) 
  
  
  
  
  # Listed_wasps <- unique(c(late_dataframe_ID_only$IntID_1, late_dataframe_ID_only$IntID_2))
  
  #  All_labels = 
  #    attributes_2021 %>%
  #   select(ID, ID_num_for_network) %>%
  #   as.data.frame()
  
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
  Aggregate_frame$AggregateDegreeNorm <- V(aggregate_network)$Aggregate_degree_norm 
  Aggregate_frame$AggregateDegree <- V(aggregate_network)$Aggregate_degree
  Aggregate_frame$AggregateStrengthNorm <- V(aggregate_network)$Aggregate_strength_norm 
  Aggregate_frame$AggregateStrength <- V(aggregate_network)$Aggregate_strength
  Aggregate_frame$AggregateNumberNodes <- length(V(aggregate_network))
  
  
  # Roosting dataframe
  Roosting_frame <- data.frame(matrix(ncol = 0, nrow = length(V(Layer_roosting)))) # this is clumsy, but only way i can force vector attributes into a dataframe nicely
  Roosting_frame$node <- V(Layer_roosting)$name ### Node name. Need to left_join association and roosting to aggregate in case there are wasps missing in some net but not the other
  Roosting_frame$RoostingDegreeNorm <- V(Layer_roosting)$Roosting_degree_norm
  Roosting_frame$RoostingDegree <- V(Layer_roosting)$Roosting_degree 
  Roosting_frame$RoostingStrengthNorm <- V(Layer_roosting)$Roosting_strength_norm 
  Roosting_frame$RoostingStrength <- V(Layer_roosting)$Roosting_strength
  Roosting_frame$RoostingNumberNodes <- length(V(Layer_roosting))
  
  # Association dataframe
  Association_frame <- data.frame(matrix(ncol = 0, nrow = length(V(Layer_Ass)))) # this is clumsy, but only way i can force vector attributes into a dataframe nicely
  Association_frame$node <- V(Layer_Ass)$name ### Node name. Need to left_join association and roosting to aggregate in case there are wasps missing in some net but not the other
  Association_frame$AssociationDegreeNorm <- V(Layer_Ass)$Association_degree_norm
  Association_frame$AssociationDegree <- V(Layer_Ass)$Association_degree 
  Association_frame$AssociationStrengthNorm <- V(Layer_Ass)$Association_strength_norm 
  Association_frame$AssociationStrength <- V(Layer_Ass)$Association_strength
  Association_frame$AssociationNumberNodes <- length(V(Layer_Ass))
  
  
  # Bind all of these data frames together, make sure to bind into aggregate bc it should include all the wasps. 
  All_data_period_i = 
    Aggregate_frame %>% 
    left_join(Roosting_frame) %>% 
    left_join(Association_frame) %>% 
    rename_at(vars(-node), ~ paste0(., "_", unqper[i])) %>%
    as.data.frame()
  
  results =
    results %>% 
    left_join(All_data_period_i, by = c('node')) %>%
    as.data.frame()
  
  
}
View(results)

# Ok so now let's deal with the NA/0s conundrum. Can have an NA if never interact bc disappeared, but should be 0 if wasp is observed within that time period. 
# Add in  
head(date_summaries)
head(results)
head
results_clean =
  results %>% 
  left_join(date_summaries, by = c('node' = 'ID.y')) %>% 
  mutate(AggregateDegreeNorm_1 = case_when(
    ( (ymd(last_day) > ymd('2021-06-01')) & is.na(AggregateDegreeNorm_1) == TRUE) ~ 0,
    TRUE ~ AggregateDegreeNorm_1
  )) %>% 
  mutate(AggregateDegree_1 = case_when(
    ( (ymd(last_day) > ymd('2021-06-01')) & is.na(AggregateDegree_1) == TRUE) ~ 0,
    TRUE ~ AggregateDegree_1
  )) %>% 
  mutate(AggregateStrengthNorm_1 = case_when(
    ( (ymd(last_day) > ymd('2021-06-01')) & is.na(AggregateStrengthNorm_1) == TRUE) ~ 0,
    TRUE ~ AggregateStrengthNorm_1
  )) %>% 
  mutate(RoostingDegreeNorm_1  = case_when(
    ( (ymd(last_day) > ymd('2021-06-01')) & is.na(RoostingDegreeNorm_1 ) == TRUE) ~ 0,
    TRUE ~ RoostingDegreeNorm_1 
  )) %>% 
  mutate(RoostingDegree_1  = case_when(
    ( (ymd(last_day) > ymd('2021-06-01')) & is.na(RoostingDegree_1 ) == TRUE) ~ 0,
    TRUE ~ RoostingDegree_1 
  )) %>% 
  mutate(RoostingStrengthNorm_1  = case_when(
    ( (ymd(last_day) > ymd('2021-06-01')) & is.na(RoostingStrengthNorm_1 ) == TRUE) ~ 0,
    TRUE ~ RoostingStrengthNorm_1 
  )) %>% 
  mutate(RoostingStrength_1  = case_when(
    ( (ymd(last_day) > ymd('2021-06-01')) & is.na(RoostingStrength_1 ) == TRUE) ~ 0,
    TRUE ~ RoostingStrength_1 
  )) %>% 
  mutate(AssociationDegreeNorm_1  = case_when(
    ( (ymd(last_day) > ymd('2021-06-01')) & is.na(AssociationDegreeNorm_1) == TRUE) ~ 0,
    TRUE ~ AssociationDegreeNorm_1 
  )) %>% 
  mutate(AssociationDegree_1  = case_when(
    ( (ymd(last_day) > ymd('2021-06-01')) & is.na(AssociationDegree_1) == TRUE) ~ 0,
    TRUE ~ AssociationDegree_1 
  )) %>% 
  mutate(Association.StrengthNorm_1  = case_when(
    ( (ymd(last_day) > ymd('2021-06-01')) & is.na(AssociationStrengthNorm_1) == TRUE) ~ 0,
    TRUE ~ AssociationStrengthNorm_1 
  )) %>% 
  mutate(AssociationStrength_1  = case_when(
    ( (ymd(last_day) > ymd('2021-06-01')) & is.na(AssociationStrength_1) == TRUE) ~ 0,
    TRUE ~ AssociationStrength_1 
  )) %>% 
  mutate(AggregateDegreeNorm_2 = case_when(
    ( (ymd(last_day) > ymd('2021-06-17')) & is.na(AggregateDegreeNorm_2) == TRUE) ~ 0,
    TRUE ~ AggregateDegreeNorm_2
  )) %>% 
  mutate(AggregateStrengthNorm_2 = case_when(
    ( (ymd(last_day) > ymd('2021-06-17')) & is.na(AggregateStrengthNorm_2) == TRUE) ~ 0,
    TRUE ~ AggregateStrengthNorm_2
  )) %>% 
  mutate(RoostingDegreeNorm_2 = case_when(
    ( (ymd(last_day) > ymd('2021-06-17')) & is.na(RoostingDegreeNorm_2) == TRUE) ~ 0,
    TRUE ~ RoostingDegreeNorm_2
  )) %>% 
  mutate(RoostingDegree_2 = case_when(
    ( (ymd(last_day) > ymd('2021-06-17')) & is.na(RoostingDegree_2) == TRUE) ~ 0,
    TRUE ~ RoostingDegree_2
  )) %>% 
  mutate(RoostingStrengthNorm_2 = case_when(
    ( (ymd(last_day) > ymd('2021-06-17')) & is.na(RoostingStrengthNorm_2) == TRUE) ~ 0,
    TRUE ~ RoostingStrengthNorm_2
  )) %>% 
  mutate(AssociationDegreeNorm_2 = case_when(
    ( (ymd(last_day) > ymd('2021-06-17')) & is.na(AssociationDegreeNorm_2) == TRUE) ~ 0,
    TRUE ~ AssociationDegreeNorm_2
  )) %>% 
  mutate(AssociationDegree_2 = case_when(
    ( (ymd(last_day) > ymd('2021-06-17')) & is.na(AssociationDegree_2) == TRUE) ~ 0,
    TRUE ~ AssociationDegree_2)) %>%
  mutate(AssociationStrengthNorm_2 = case_when(
    ( (ymd(last_day) > ymd('2021-06-17')) & is.na(AssociationStrengthNorm_2) == TRUE) ~ 0,
    TRUE ~ AssociationStrengthNorm_2
  )) %>% 
  mutate(AggregateDegreeNorm_3 = case_when(
    ( (ymd(last_day) > ymd('2021-07-02')) & is.na(AggregateDegreeNorm_3) == TRUE) ~ 0,
    TRUE ~ AggregateDegreeNorm_3
  )) %>% 
  mutate(AggregateStrengthNorm_3 = case_when(
    ( (ymd(last_day) > ymd('2021-07-02')) & is.na(AggregateStrengthNorm_3) == TRUE) ~ 0,
    TRUE ~ AggregateStrengthNorm_3
  )) %>% 
  mutate(RoostingDegreeNorm_3 = case_when(
    ( (ymd(last_day) > ymd('2021-07-02')) & is.na(RoostingDegreeNorm_3) == TRUE) ~ 0,
    TRUE ~ RoostingDegreeNorm_3
  )) %>% 
  mutate(RoostingDegree_3 = case_when(
    ( (ymd(last_day) > ymd('2021-07-02')) & is.na(RoostingDegree_3) == TRUE) ~ 0,
    TRUE ~ RoostingDegree_3
  )) %>% 
  mutate(RoostingStrengthNorm_3 = case_when(
    ( (ymd(last_day) > ymd('2021-07-02')) & is.na(RoostingStrengthNorm_3) == TRUE) ~ 0,
    TRUE ~ RoostingStrengthNorm_3
  )) %>% 
  mutate(AssociationDegreeNorm_3 = case_when(
    ( (ymd(last_day) > ymd('2021-07-02')) & is.na(AssociationDegreeNorm_3) == TRUE) ~ 0,
    TRUE ~ AssociationDegreeNorm_3
  )) %>% 
  mutate(AssociationDegree_3 = case_when(
    ( (ymd(last_day) > ymd('2021-07-02')) & is.na(AssociationDegree_3) == TRUE) ~ 0,
    TRUE ~ AssociationDegree_3
  )) %>% 
  mutate(AssociationStrengthNorm_3 = case_when(
    ( (ymd(last_day) > ymd('2021-07-02')) & is.na(AssociationStrengthNorm_3) == TRUE) ~ 0,
    TRUE ~ AssociationStrengthNorm_3
  )) %>%
  mutate(RoostingNumberNodes_1 = 56) %>% # Make sure that this gets checked when choosing behavioral parameters
  mutate(RoostingNumberNodes_2 = 44) %>% # Make sure that this gets checked when choosing behavioral parameters
  mutate(RoostingNumberNodes_3 = 36) %>% # Make sure that this gets checked when choosing behavioral parameters
  mutate(AssociationNumberNodes_1 = 62) %>% # Make sure that this gets checked when choosing behavioral parameters
  mutate(AssociationNumberNodes_2 = 42) %>% # Make sure that this gets checked when choosing behavioral parameters
  mutate(AssociationNumberNodes_3 = 34) %>% # Make sure that this gets checked when choosing behavioral parameters
  mutate(Versatility_1 = AssociationDegree_1 + RoostingDegree_1) %>%
  mutate(Versatility_2 = AssociationDegree_2 + RoostingDegree_2) %>%
  mutate(Versatility_3 = AssociationDegree_3 + RoostingDegree_3) %>%
  mutate(VersatilityNorm_1 = Versatility_1/((AssociationNumberNodes_1-1) + (RoostingNumberNodes_1 - 1)) ) %>%
  mutate(VersatilityNorm_2 = Versatility_2/((AssociationNumberNodes_2-1) + (RoostingNumberNodes_2 - 1)) ) %>%
  mutate(VersatilityNorm_3 = Versatility_3/((AssociationNumberNodes_3-1) + (RoostingNumberNodes_3 - 1)) ) %>%
  left_join(select(attributes_2021, ID, Weight,NestID,max_stable_group,Cell_Count, nest_choice, Individual.site.choice.day..3.day.of.same.consequetive.location.,dominance ), by = c('node' = 'ID')) %>% 
  as.data.frame()


############################################


####################### Nest NY
nest_ny =
  results_clean %>% 
  mutate(nest_num = case_when(
    nest_choice == "y" ~ 1,
    TRUE ~ 0
  )) %>%
  filter(!(is.na(last_day) == TRUE))%>% 
  filter(length_obs > 4) %>% 
  as.data.frame()

View(nest_ny)

mean(nest_ny$Roosting.DegreeNorm)



nest_ny$Weight <-as.numeric(nest_ny$Weight)

mod_nestny <- glm(nest_num ~ (VersatilityNorm_1)  + (Weight), data = nest_ny, family = binomial())
summary(mod_nestny)
Anova(mod_nestny)
check_collinearity(mod_nestny)

mod_nestny <- glm(nest_num ~ (AggregateDegreeNorm_1)  + (Weight), data = nest_ny, family = binomial())
summary(mod_nestny)
Anova(mod_nestny)
check_collinearity(mod_nestny)

mod_nestny <- glm(nest_num ~ (RoostingDegreeNorm_1) + (AssociationDegreeNorm_1) + (Weight), data = nest_ny, family = binomial())
summary(mod_nestny)
Anova(mod_nestny)
check_collinearity(mod_nestny)

mod_nestny <- glm(nest_num ~ (AggregateStrengthNorm_1)  + (Weight), data = nest_ny, family = binomial())
summary(mod_nestny)
Anova(mod_nestny)
check_collinearity(mod_nestny)

mod_nestny <- glm(nest_num ~ (RoostingStrengthNorm_1) + (AssociationStrengthNorm_1) + (Weight), data = nest_ny, family = binomial())
summary(mod_nestny)
Anova(mod_nestny)
check_collinearity(mod_nestny)




# ok now try again

dom_alone = 
  results_clean %>%
  filter(dominance == "d" | dominance == "a") %>% 
  filter(!(node == "MN15")) %>% #Usurped wasp
  filter(Cell_Count > 0) %>%  # 1 wasp chose nest site, but never made nest cell
  as.data.frame()

dom_alone$Weight <- as.numeric(dom_alone$Weight)


dom_alone_sf = 
  dom_alone %>%
  mutate(sf = case_when(
    max_stable_group == 1 ~ 'sf',
    max_stable_group > 1 ~ 'mf'
  )) %>% 
  as.data.frame()


######### ok now we need to graph

theme_mine4 <- function(base_size = 20, base_family = "Helvetica") {
  # Starts with theme_grey and then modify some parts
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 22),
      strip.text.y = element_text(size = 22),
      axis.text.x = element_text(size=30),
      axis.text.y = element_text(size=30,hjust=1),
      axis.ticks =  element_line(colour = "black"), 
      axis.title.x= element_text(size=35),
      axis.title.y= element_text(size=35,angle=90),
      panel.background = element_blank(), 
      panel.border =element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.margin = unit(1.0, "lines"), 
      plot.background = element_blank(), 
      plot.margin = unit(c(1,  1, 1, 1), "lines"),
      axis.line.x = element_line(color="black", size = 1),
      axis.line.y = element_line(color="black", size = 1)
    )
}


# try with functions
graph_model <- function(model_var, terms_var, linetype, linecolor, ribbon_fill, point_color, x_axis_label) {
  pred_data <- ggpredict(model_var, terms = terms_var)
  p_custom <- ggplot(pred_data, aes_string(x = "x", y = "predicted")) +
    geom_line(linetype = linetype, color = linecolor, size = 2) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = ribbon_fill, alpha = .3) +
    geom_point(data = model.frame(model_var),
               aes_string(x = terms_var, y = all.vars(formula(model_var))[1]),
               alpha = 0.5, color = point_color, size = 4) +
    theme_mine4() +
    labs(
      x = x_axis_label,
      y = "Nest size (# cells)"
    )
  return (p_custom)
}

##########


mod_nest_long_v1 <- glm(Cell_Count ~ (max_stable_group) + (VersatilityNorm_1) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight), data = dom_alone, family = poisson() )
summary(mod_nest_long_v1)
Anova(mod_nest_long_v1)
check_collinearity(mod_nest_long_v1)
check_model(mod_nest_long_v1)

mod_nest_long_v2 <- glm(Cell_Count ~ (max_stable_group) + (VersatilityNorm_2) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight), data = dom_alone, family = poisson() )
summary(mod_nest_long_v2)
Anova(mod_nest_long_v2)
check_collinearity(mod_nest_long_v2)
check_model(mod_nest_long_v2)

mod_nest_long_v3 <- glm(Cell_Count ~ (max_stable_group) + (VersatilityNorm_3) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight), data = dom_alone, family = poisson() )
summary(mod_nest_long_v3)
Anova(mod_nest_long_v3)
check_collinearity(mod_nest_long_v3)
check_model(mod_nest_long_v3)

graph_model(mod_nest_long_v1, 'VersatilityNorm_1', "dashed", "mediumorchid4", "mediumorchid2", "mediumorchid3", "Normalized versatility" )
graph_model(mod_nest_long_v2, 'VersatilityNorm_2', "solid", "mediumorchid4", "mediumorchid2", "mediumorchid3", "Normalized versatility" )
graph_model(mod_nest_long_v3, 'VersatilityNorm_3', "dashed", "mediumorchid4", "mediumorchid2", "mediumorchid3", "Normalized versatility" )


# Aggregate degree norm

mod_nest_long_aggd1 <- glm(Cell_Count ~ (max_stable_group) + (AggregateDegreeNorm_1) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight), data = dom_alone, family = poisson() )
summary(mod_nest_long_aggd1)
Anova(mod_nest_long_aggd1)
check_collinearity(mod_nest_long_aggd1)
check_model(mod_nest_long_aggd1)

mod_nest_long_aggd2 <- glm(Cell_Count ~ (max_stable_group) + (AggregateDegreeNorm_2) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight), data = dom_alone, family = poisson() )
summary(mod_nest_long_aggd2)
Anova(mod_nest_long_aggd2)
check_collinearity(mod_nest_long_aggd2)
check_model(mod_nest_long_aggd2)

mod_nest_long_aggd3 <- glm(Cell_Count ~ (max_stable_group) + (AggregateDegreeNorm_3) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight), data = dom_alone, family = poisson() )
summary(mod_nest_long_aggd3)
Anova(mod_nest_long_aggd3)
check_collinearity(mod_nest_long_aggd3)
check_model(mod_nest_long_aggd3)

graph_model(mod_nest_long_aggd1, 'Aggregate.DegreeNorm', "dashed", "#2E8B57", "seagreen2", 'seagreen3', "Proportion of individuals" )
graph_model(mod_nest_long_aggd2, 'Aggregate.DegreeNorm', "solid", "#2E8B57", "seagreen2", 'seagreen3', "Proportion of individuals" )
graph_model(mod_nest_long_aggd3, 'Aggregate.DegreeNorm', "dashed", "#2E8B57", "seagreen2", 'seagreen3', "Proportion of individuals" )


# Aggregate Strength

mod_nest_long1 <- glm(Cell_Count ~ (max_stable_group) + (AggregateStrengthNorm_1) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight), data = dom_alone, family = poisson() )
summary(mod_nest_long1)
Anova(mod_nest_long1)
check_collinearity(mod_nest_long1)
check_model(mod_nest_long1)

mod_nest_long2 <- glm(Cell_Count ~ (max_stable_group) + (AggregateStrengthNorm_2) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight), data = dom_alone, family = poisson() )
summary(mod_nest_long2)
Anova(mod_nest_long2)
check_collinearity(mod_nest_long2)
check_model(mod_nest_long2)

mod_nest_long3 <- glm(Cell_Count ~ (max_stable_group) + (AggregateStrengthNorm_3) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight), data = dom_alone, family = poisson() )
summary(mod_nest_long3)
Anova(mod_nest_long3)
check_collinearity(mod_nest_long3)
check_model(mod_nest_long3)

graph_model(mod_nest_long1, "Aggregate.StrengthNorm", "dashed", "darkseagreen4", "darkseagreen2", 'darkseagreen3', "Proportion of interactions" )
graph_model(mod_nest_long2, "Aggregate.StrengthNorm", "solid", "darkseagreen4", "darkseagreen2", 'darkseagreen3', "Proportion of interactions" )
graph_model(mod_nest_long3, "Aggregate.StrengthNorm", "solid", "darkseagreen4", "darkseagreen2", 'darkseagreen3', "Proportion of interactions" )

# Degree 
mod_nest_long_d1 <- glm(Cell_Count ~ (max_stable_group) + (RoostingDegreeNorm_1) + (AssociationDegreeNorm_1) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight), data = dom_alone, family = poisson() )
summary(mod_nest_long_d1)
Anova(mod_nest_long_d1)
check_collinearity(mod_nest_long_d1)
check_model(mod_nest_long_d1)

mod_nest_long_d2 <- glm(Cell_Count ~ (max_stable_group) + (RoostingDegreeNorm_2) + (AssociationDegreeNorm_2) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight), data = dom_alone, family = poisson() )
summary(mod_nest_long_d2)
Anova(mod_nest_long_d2)
check_collinearity(mod_nest_long_d2)
check_model(mod_nest_long_d2)

mod_nest_long_d3 <- glm(Cell_Count ~ (max_stable_group) + (RoostingDegreeNorm_3) + (AssociationDegreeNorm_3) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight), data = dom_alone, family = poisson() )
summary(mod_nest_long_d3)
Anova(mod_nest_long_d3)
check_collinearity(mod_nest_long_d3)
check_model(mod_nest_long_d3)

graph_model(mod_nest_long_d1, "Roosting.DegreeNorm", "solid", "lightsteelblue4", "lightsteelblue1", 'lightsteelblue3', "Proportion of individuals" )
graph_model(mod_nest_long_d2, "Roosting.DegreeNorm", "solid", "lightsteelblue4", "lightsteelblue1", 'lightsteelblue3', "Proportion of individuals" )
graph_model(mod_nest_long_d3, "Roosting.DegreeNorm", "dashed", "lightsteelblue4", "lightsteelblue1", 'lightsteelblue3', "Proportion of individuals" )

graph_model(mod_nest_long_d1, "Association.DegreeNorm", "dashed", "lightgoldenrod4", "lightgoldenrod1", 'lightgoldenrod3', "Proportion of individuals" )
graph_model(mod_nest_long_d2, "Association.DegreeNorm", "solid", "lightgoldenrod4", "lightgoldenrod1", 'lightgoldenrod3', "Proportion of individuals" )
graph_model(mod_nest_long_d3, "Association.DegreeNorm", "dashed", "lightgoldenrod4", "lightgoldenrod1", 'lightgoldenrod3', "Proportion of individuals" )


# Strength

# functions
mod_nest_long_s1 <- glm(Cell_Count ~ (max_stable_group) + (RoostingStrengthNorm_1) + (AssociationStrengthNorm_1) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight), data = dom_alone, family = poisson() )
summary(mod_nest_long_s1)
Anova(mod_nest_long_s1)
check_collinearity(mod_nest_long_s1)
check_model(mod_nest_long_s1)

mod_nest_long_s2 <- glm(Cell_Count ~ (max_stable_group) + (RoostingStrengthNorm_2) + (AssociationStrengthNorm_2) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight), data = dom_alone, family = poisson() )
summary(mod_nest_long_s2)
Anova(mod_nest_long_s2)
check_collinearity(mod_nest_long_s2)
check_model(mod_nest_long_s2)

mod_nest_long_s3 <- glm(Cell_Count ~ (max_stable_group) + (RoostingStrengthNorm_3) + (AssociationStrengthNorm_3) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight), data = dom_alone, family = poisson() )
summary(mod_nest_long_s3)
Anova(mod_nest_long_s3)
check_collinearity(mod_nest_long_s3)
check_model(mod_nest_long_s3)

graph_model(mod_nest_long_s1, "Roosting.StrengthNorm", "solid", "steelblue4", "steelblue1", 'steelblue', "Proportion of interactions" )
graph_model(mod_nest_long_s2, "Roosting.StrengthNorm", "dashed", "steelblue4", "steelblue1", 'steelblue', "Proportion of interactions" )
graph_model(mod_nest_long_s3, "Roosting.StrengthNorm", "dashed", "steelblue4", "steelblue1", 'steelblue', "Proportion of interactions" )

graph_model(mod_nest_long_s1, "Association.StrengthNorm", "dashed", "goldenrod4", "goldenrod1", 'goldenrod3', "Proportion of interactions" )
graph_model(mod_nest_long_s2, "Association.StrengthNorm", "dashed", "goldenrod4", "goldenrod1", 'goldenrod3', "Proportion of interactions" )
graph_model(mod_nest_long_s3, "Association.StrengthNorm", "dashed", "goldenrod4", "goldenrod1", 'goldenrod3', "Proportion of interactions" )






########## Graphing supplementary correlations between degree strength 

ggplot(dom_alone, aes(x = RoostingDegreeNorm_1, y = RoostingStrengthNorm_1)) +
  geom_point(alpha = 0.5, color = 'steelblue',  size = 2) +
  sm_statCorr( color = 'steelblue1',  text_size = 12)+
  theme_mine4() +
  labs(
    x = "Normalized degree",
    y = "Normalized strength"
  )

ggplot(dom_alone, aes(x = AssociationDegreeNorm_1, y = AssociationStrengthNorm_1)) +
  geom_point(alpha = 0.5, color = 'lightgoldenrod',  size = 2) +
  sm_statCorr( color = 'lightgoldenrod1', text_size = 12)+
  theme_mine4() +
  labs(
    x = "Normalized degree",
    y = "Normalized strength"
  )


ggplot(dom_alone, aes(x = RoostingDegreeNorm_2, y = RoostingStrengthNorm_2)) +
  geom_point(alpha = 0.5, color = 'steelblue',  size = 2) +
  sm_statCorr( color = 'steelblue1', text_size = 12)+
  theme_mine4() +
  labs(
    x = "Normalized degree",
    y = "Normalized strength"
  )

ggplot(dom_alone, aes(x = AssociationDegreeNorm_2, y = AssociationStrengthNorm_2)) +
  geom_point(alpha = 0.5, color = 'lightgoldenrod',  size = 2) +
  sm_statCorr( color = 'lightgoldenrod1', text_size = 12)+
  theme_mine4() +
  labs(
    x = "Normalized degree",
    y = "Normalized strength"
  )

ggplot(dom_alone, aes(x = RoostingDegreeNorm_3, y = RoostingStrengthNorm_3)) +
  geom_point(alpha = 0.5, color = 'steelblue',  size = 2) +
  sm_statCorr( color = 'steelblue1', text_size = 12)+
  theme_mine4() +
  labs(
    x = "Normalized degree",
    y = "Normalized strength"
  )

ggplot(dom_alone, aes(x = AssociationDegreeNorm_3, y = AssociationStrengthNorm_3)) +
  geom_point(alpha = 0.5, color = 'lightgoldenrod',  size = 2) +
  sm_statCorr( color = 'lightgoldenrod1', text_size = 12)+
  theme_mine4() +
  labs(
    x = "Normalized degree",
    y = "Normalized strength"
  )

########################################################################################
# Supplemental Analysis - single vs multiple foundress

########## check single vs. multiple foundress

dom_alone_sf = 
  results_clean %>%
  filter(dominance == "d" | dominance == "a") %>% 
  filter(!(node == "MN15")) %>%
  filter(Cell_Count > 0) %>% 
  mutate(sf = case_when(
    max_stable_group == 1 ~ 'sf',
    max_stable_group > 1 ~ 'mf'
  )) %>% 
  as.data.frame()

dom_alone_sf$Weight <- as.numeric(dom_alone_sf$Weight)

dom_alone_sf$sf<- as.factor(dom_alone_sf$sf)

# Versatility
str(dom_alone_sf)

mod_nest_long_v1 <- glm(Cell_Count ~ (max_stable_group) + (VersatilityNorm_1) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight) + sf, data = dom_alone_sf, family = poisson() )
summary(mod_nest_long_v1)
Anova(mod_nest_long_v1)
check_collinearity(mod_nest_long_v1)

mod_nest_long_v2 <- glm(Cell_Count ~ (max_stable_group) + (VersatilityNorm_2) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight) + sf, data = dom_alone_sf, family = poisson() )
summary(mod_nest_long_v2)
Anova(mod_nest_long_v2)
check_collinearity(mod_nest_long_v2)

mod_nest_long_v3 <- glm(Cell_Count ~ (max_stable_group) + (VersatilityNorm_3) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight) + sf, data = dom_alone_sf, family = poisson() )
summary(mod_nest_long_v3)
Anova(mod_nest_long_v3)
check_collinearity(mod_nest_long_v3)


# Degree
mod_nest_long_d1 <- glm(Cell_Count ~ (max_stable_group) + (RoostingDegreeNorm_1) + (AssociationDegreeNorm_1) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight) + sf, data = dom_alone_sf, family = poisson() )
summary(mod_nest_long_d1)
Anova(mod_nest_long_d1)
check_collinearity(mod_nest_long_d1)

mod_nest_long_d2 <- glm(Cell_Count ~ (max_stable_group) + (RoostingDegreeNorm_2) + (AssociationDegreeNorm_2) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight) + sf, data = dom_alone_sf, family = poisson() )
summary(mod_nest_long_d2)
Anova(mod_nest_long_d2)
check_collinearity(mod_nest_long_d2)

mod_nest_long_d3 <- glm(Cell_Count ~ (max_stable_group) + (RoostingDegreeNorm_3) + (AssociationDegreeNorm_3) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight) + sf, data = dom_alone_sf, family = poisson() )
summary(mod_nest_long_d3)
Anova(mod_nest_long_d3)
check_collinearity(mod_nest_long_d3)

# Strength

mod_nest_long_s1 <- glm(Cell_Count ~ (max_stable_group) + (RoostingStrengthNorm_1) + (AssociationStrengthNorm_1) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight) + sf, data = dom_alone_sf, family = poisson() )
summary(mod_nest_long_s1)
Anova(mod_nest_long_s1)
check_collinearity(mod_nest_long_s1)

mod_nest_long_s2 <- glm(Cell_Count ~ (max_stable_group) + (RoostingStrengthNorm_2) + (AssociationStrengthNorm_2) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight) + sf, data = dom_alone_sf, family = poisson() )
summary(mod_nest_long_s2)
Anova(mod_nest_long_s2)
check_collinearity(mod_nest_long_s2)

mod_nest_long_s3 <- glm(Cell_Count ~ (max_stable_group) + (RoostingStrengthNorm_3) + (AssociationStrengthNorm_3) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight)+ sf, data = dom_alone_sf, family = poisson() )
summary(mod_nest_long_s3)
Anova(mod_nest_long_s3)
check_collinearity(mod_nest_long_s3)

# Aggregate
mod_nest_long_aggd1 <- glm(Cell_Count ~ (max_stable_group) + (AggregateDegreeNorm_1) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight) + sf, data = dom_alone_sf, family = poisson() )
summary(mod_nest_long_aggd1)
Anova(mod_nest_long_aggd1)
check_collinearity(mod_nest_long_aggd1)

mod_nest_long_aggd2 <- glm(Cell_Count ~ (max_stable_group) + (AggregateDegreeNorm_2) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight) + sf, data = dom_alone_sf, family = poisson() )
summary(mod_nest_long_aggd2)
Anova(mod_nest_long_aggd2)
check_collinearity(mod_nest_long_aggd2)

mod_nest_long_aggd3 <- glm(Cell_Count ~ (max_stable_group) + (AggregateDegreeNorm_3) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight) + sf, data = dom_alone_sf, family = poisson() )
summary(mod_nest_long_aggd3)
Anova(mod_nest_long_aggd3)
check_collinearity(mod_nest_long_aggd3)


# Aggregate Strength

mod_nest_long1 <- glm(Cell_Count ~ (max_stable_group) + (AggregateStrengthNorm_1) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight) + sf, data = dom_alone_sf, family = poisson() )
summary(mod_nest_long1)
Anova(mod_nest_long1)
check_collinearity(mod_nest_long1)

mod_nest_long2 <- glm(Cell_Count ~ (max_stable_group) + (AggregateStrengthNorm_2) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight) + sf, data = dom_alone_sf, family = poisson() )
summary(mod_nest_long2)
Anova(mod_nest_long2)
check_collinearity(mod_nest_long2)

mod_nest_long3 <- glm(Cell_Count ~ (max_stable_group) + (AggregateStrengthNorm_3) + (Individual.site.choice.day..3.day.of.same.consequetive.location.)+ (Weight) + sf, data = dom_alone_sf, family = poisson() )
summary(mod_nest_long3)
Anova(mod_nest_long3)
check_collinearity(mod_nest_long3)





####################################################################################################################
# Supplemental 

# Supplemental for graphing of trajectory of wasps over time. 

results_long = 
  results_clean %>% 
  pivot_longer(!c(node,Weight, max_stable_group, X, RoostingNumberNodes_1,AggregateNumberNodes_1, AggregateNumberNodes_2, AggregateNumberNodes_3,RoostingNumberNodes_2,RoostingNumberNodes_3,AssociationNumberNodes_1,AssociationNumberNodes_2,AssociationNumberNodes_3 , first_day, last_day,n_obs, length_obs, NestID, Cell_Count, nest_choice,Individual.site.choice.day..3.day.of.same.consequetive.location.
                  , dominance,), names_to = "Network_Measure", values_to = "value") %>%
  as.data.frame()

# Seperate by period
View(results_long)
View(results_clean)

results_long_period =
  results_long %>% 
  separate_wider_delim(Network_Measure, "_", names = c("Network_Measure", "Period")) %>%
  as.data.frame()

head(results_long_period)



results_wider =
  results_long_period %>% 
  pivot_wider(names_from = Network_Measure, values_from = value) %>%
  filter(dominance == "d" | dominance == "a") %>% 
  filter(!(node == "MN15")) %>% #Usurped wasp
  filter(Cell_Count > 0) %>% # 1 wasp chose nest site, but never made nest cell
  mutate(Period_name = case_when(
    Period == 1 ~ "Shopping",
    Period == 2 ~ "Establishment",
    Period == 3 ~ "Growth"
  )) %>% 
  as.data.frame()

results_wider$Period_name <- factor(results_wider$Period_name,
                                    ordered = TRUE,
                                    levels = c("Shopping", "Establishment", "Growth"))
head(results_wider)

ggplot(results_wider, aes(x = Period_name, y = RoostingDegreeNorm, group= node)) +
  geom_point(alpha = 0.5,  size = 2, aes(colour = node)) +
  geom_line(aes(colour = node))+
  theme_mine4() +
  labs(
    x = "Time period",
    y = "Normalized degree, roosting"
  )


ggplot(results_wider, aes(x = Period_name, y = RoostingStrengthNorm, group= node)) +
  geom_point(alpha = 0.5,  size = 2, aes(colour = node)) +
  geom_line(aes(colour = node))+
  theme_mine4() +
  labs(
    x = "Time period",
    y = "Normalized strength, roosting"
  )

ggplot(results_wider, aes(x = Period_name, y = AssociationDegreeNorm, group= node)) +
  geom_point(alpha = 0.5,  size = 2, aes(colour = node)) +
  geom_line(aes(colour = node))+
  theme_mine4() +
  labs(
    x = "Time period",
    y = "Normalized degree, day association"
  )

ggplot(results_wider, aes(x = Period_name, y = AssociationStrengthNorm, group= node)) +
  geom_point(alpha = 0.5,  size = 2, aes(colour = node)) +
  geom_line(aes(colour = node))+
  theme_mine4() +
  labs(
    x = "Time period",
    y = "Normalized strength, day association"
  )




###############################################################################################################################################

######### Supplementary graphing
head(Clean_graph)
head(roosting_frame)

unqper <- unique(Clean_graph$Period) # yes says 1, 2, 3
graph_agg <- data.frame(matrix(ncol = 4, nrow = 0) )
colnames(graph_agg) <- c('Initator', 'Recipient', 'weight', 'Layer')

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
  aggregate_network = igraph::simplify(aggregate_net, edge.attr.comb = "sum", remove.loops = TRUE) 
  Edgelist_aggregate<- cbind(as_edgelist(aggregate_network), E(aggregate_network)$weight)
  Aggregate <- as.data.frame(Edgelist_aggregate)
  Aggregate$Layer <- unqper[i]
  colnames(Aggregate) <- c("Initiator", "Recipient", "weight", 'Layer')
  
  
  
  graph_agg = rbind(graph_agg, Aggregate)
  
  
}


##### ok now should have giant edgelist with each layer in an aggregate, need to do it this way because of maintaining the coordinates. 

head(graph_agg)

# now make as graph, then get names, then get the ones who don't appear by the set_diff, and add them as vertices, then add the last day 

# better to make stacked graph from three different graphs, keep location of individuals the same. 



aggregate_net <-graph_from_data_frame(graph_agg[,1:2], directed = F)
E(aggregate_net)$weight <- as.numeric(graph_agg[,3])
E(aggregate_net)$Layer <- as.numeric(graph_agg[,4])


aggregate_net <- aggregate_net %>%
  add_vertices(5, name = c("MN25", "MN47", "MN50"), type = "isolated") # add wasps that don't interact

layout_1 <- file.choose("coordinates_for_net.csv")

layout_1 <- read.csv(layout_1)
layout_1 =
  layout_1 %>% 
  filter(!(ID == "MN75"))%>% # remove wasps that weren't observed
  filter(!(ID == "MN34")) %>% # remove wasps that weren't observed
  as.data.frame()

head(layout_1)
head(aggregate_frame)


aggregate_frame <- data.frame(matrix(ncol = 0, nrow = length(V(aggregate_net)))) # this is clumsy, but only way i can force vector attributes into a dataframe nicely
aggregate_frame$node <- V(aggregate_net)$name


aggregate_frame = 
  aggregate_frame %>% 
  left_join(select(layout_1, ID, x, y), by = c('node' = 'ID')) %>%
  as.data.frame()

head(aggregate_frame)
V(aggregate_net)$name

layout_for_net <- as.matrix(aggregate_frame[,2:3])

net.m <- aggregate_net - (E(aggregate_net)[E(aggregate_net)$Layer=="2"])  
net_shop <- net.m - (E(net.m)[E(net.m)$Layer=="3"])


# another way to delete edges:

net.k <- aggregate_net - (E(aggregate_net)[E(aggregate_net)$Layer=="1"])  
net_est <- net.k - (E(net.k)[E(net.k)$Layer=="3"])

net.j <- aggregate_net  - (E(aggregate_net)[E(aggregate_net)$Layer=="1"])  
net_grow <- net.j - (E(net.j)[E(net.j)$Layer=="2"])
V(net_grow)
E(net_grow)$Layer
##### change vertex colors
shop_cols <- data.frame(matrix(ncol = 0, nrow = length(V(net_shop))))
shop_cols$node <- V(net_shop)$name
shop_cols =
  shop_cols %>% 
  left_join(date_summaries, by = c('node' = 'ID.y'))%>%
  mutate(vertex_color = case_when(
    last_day >= ymd('2021-06-01') ~ 'green',
    TRUE ~ '#FFA500'
  )) %>% 
  mutate(label_color = case_when(
    last_day >= ymd('2021-06-01') ~ 'black',
    TRUE ~ '#FFA500'
  )) %>%
  as.data.frame()

V(net_shop)$label <- shop_cols$label_color
V(net_shop)$vertex_col <- shop_cols$vertex_color

head(date_summaries)
# establishment
est_cols <- data.frame(matrix(ncol = 0, nrow = length(V(net_est))))
est_cols$node <- V(net_est)$name
est_cols =
  est_cols %>% 
  left_join(date_summaries, by = c('node' = 'ID.y'))%>%
  mutate(vertex_color = case_when(
    last_day >= ymd('2021-06-17') ~ 'green',
    TRUE ~ '#FFA500'
  )) %>% 
  mutate(label_color = case_when(
    last_day >= ymd('2021-06-17') ~ 'black',
    TRUE ~ '#FFA500'
  )) %>%
  as.data.frame()

V(net_est)$label <- est_cols$label_color
V(net_est)$vertex_col <- est_cols$vertex_color

# growth
grow_cols <- data.frame(matrix(ncol = 0, nrow = length(V(net_grow))))
grow_cols$node <- V(net_grow)$name
grow_cols =
  grow_cols %>% 
  left_join(date_summaries, by = c('node' = 'ID.y'))%>%
  mutate(vertex_color = case_when(
    last_day >= ymd('2021-07-02') ~ 'green',
    TRUE ~ '#FFA500'
  )) %>% 
  mutate(label_color = case_when(
    last_day >= ymd('2021-07-02') ~ 'black',
    TRUE ~ '#FFA500'
  )) %>%
  as.data.frame()

V(net_grow)$label <- grow_cols$label_color
V(net_grow)$vertex_col <- grow_cols$vertex_color






vertex.label.color

plot(net_shop, vertex.color= V(net_shop)$vertex_col, vertex.label = V(net_shop)$name, vertex.label.color = V(net_shop)$label, layout=layout_for_net, main="Period: Shopping", vertex.size = .1, edge.color='darkolivegreen2', edge.width = E(net_shop)$weight)
ggsave("Shopping_aggregate_20260107.png")
plot(net_est,  vertex.color= V(net_est)$vertex_col, vertex.label = V(net_est)$name, vertex.label.color = V(net_est)$label,layout=layout_for_net, main="Period: Establishment", vertex.size = .1, edge.width = E(net_est)$weight, edge.color = 'darkolivegreen3')

plot(net_grow,  vertex.color = V(net_grow)$vertex_col, vertex.label = V(net_grow)$name, vertex.label.color = V(net_grow)$label, layout=layout_for_net, main="Period: Growth", vertex.size = .1, edge.width = E(net_grow)$weight, edge.color = 'darkolivegreen4')







head(dom_alone)


# for supplementary table
dom_alone_summary =
  dom_alone %>% 
  pivot_longer(!c(node,Weight, max_stable_group, RoostingNumberNodes_1,AggregateNumberNodes_1, AggregateNumberNodes_2, AggregateNumberNodes_3,RoostingNumberNodes_2,RoostingNumberNodes_3,AssociationNumberNodes_1,AssociationNumberNodes_2,AssociationNumberNodes_3 , first_day, last_day,n_obs, length_obs, NestID, Cell_Count, nest_choice,Individual.site.choice.day..3.day.of.same.consequetive.location.
                  , dominance), names_to = "Network_Measure", values_to = "value") %>% 
  group_by(Network_Measure) %>%
  summarise(Mean = mean(value, na.rm = TRUE), Minimum = min(value,na.rm = TRUE ), Max = max(value, na.rm = TRUE), Standard = sd(value, na.rm = TRUE))%>%
  as.data.frame()
View(dom_alone_summary)


########################################################################
# Supplementary data figures

head(results_wider)

results_wider$Weight <- as.numeric(results_wider$Weight)
results_wider$max_stable_group <- as.numeric(results_wider$max_stable_group)
results_wider$Individual.site.choice.day..3.day.of.same.consequetive.location. <- as.numeric(results_wider$Individual.site.choice.day..3.day.of.same.consequetive.location.)

dom_alone_1 =
  results_wider %>%
  filter(Period == 1)%>%
  as.data.frame()

dom_alone_2 =
  results_wider %>%
  filter(Period == 2)%>%
  as.data.frame()

dom_alone_3 =
  results_wider %>%
  filter(Period == 3)%>%
  as.data.frame()

# correlation tables
dom_alone_simple_1 =
  dom_alone_1 %>% 
  select(max_stable_group, Weight, Individual.site.choice.day..3.day.of.same.consequetive.location., RoostingDegreeNorm, RoostingStrengthNorm, AssociationDegreeNorm,
         AssociationStrengthNorm, AggregateDegreeNorm, AggregateStrengthNorm, VersatilityNorm) %>%
  as.data.frame()
colnames(dom_alone_simple_1) <- c("Maximum group size","Body mass","Nest choice day", "Roosting degree", "Roosting strength", "Diurnal degree", "Diurnal strength", "Aggregate degree", "Aggregate strength", "Versatility")
dom_alone_1_cor <- round(cor(dom_alone_simple_1), digits = 3)

dom_alone_simple_2 =
  dom_alone_2 %>% 
  select(max_stable_group, Weight, Individual.site.choice.day..3.day.of.same.consequetive.location., RoostingDegreeNorm, RoostingStrengthNorm, AssociationDegreeNorm,
         AssociationStrengthNorm, AggregateDegreeNorm, AggregateStrengthNorm, VersatilityNorm) %>%
  as.data.frame()
colnames(dom_alone_simple_2) <- c("Maximum group size","Body mass","Nest choice day", "Roosting degree", "Roosting strength", "Diurnal degree", "Diurnal strength", "Aggregate degree", "Aggregate strength", "Versatility")
dom_alone_2_cor <- round(cor(dom_alone_simple_2), digits = 3)

dom_alone_simple_3 =
  dom_alone_3 %>% 
  select(max_stable_group, Weight, Individual.site.choice.day..3.day.of.same.consequetive.location., RoostingDegreeNorm, RoostingStrengthNorm, AssociationDegreeNorm,
         AssociationStrengthNorm, AggregateDegreeNorm, AggregateStrengthNorm, VersatilityNorm) %>%
  as.data.frame()
colnames(dom_alone_simple_3) <- c("Maximum group size","Body mass","Nest choice day", "Roosting degree", "Roosting strength", "Diurnal degree", "Diurnal strength", "Aggregate degree", "Aggregate strength", "Versatility")
dom_alone_3_cor <- round(cor(dom_alone_simple_3, use = "pairwise.complete.obs"), digits = 3)


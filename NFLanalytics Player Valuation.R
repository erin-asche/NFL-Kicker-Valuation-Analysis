

# LOADING AND CLEANING THE DATA
#---------------------------------------------------------------------------------
#package loading---
library(nflverse)#NFl data loaded
library(dplyr) #data wrangling tool
library(tidyr)#reshaping data tools

#checks whether packages are installed and if not, installs them
if(!require(cluster)) install.packages("cluster") 
if(!require(amap)) install.packages("amap") 
if(!require(proxy)) install.packages("proxy") 

#more package loading
library(cluster)#AGNES and DIANA clustering tool
library(amap)#used for distance calculations
library(proxy)#used for distance calculations
library(ggplot2)#used for visualizations

#----Loading player information

# Convert otc_id to character in both data frames to ensure a match
ids <- load_players() %>% #downloads player information
  select(gsis_id, otc_id, position) %>% #keeps only selected items
  mutate(otc_id = as.character(otc_id)) # converts oct_id to text (prevents future errors when stored as char)

contracts <- load_contracts() %>%  #downloads contract data
  filter(position %in% c( "K")) %>% #keeps only selected items
  mutate(otc_id = as.character(otc_id)) # converts oct_id to text (prevents future errors when stored as char)

#inspects columns and double checking merges
names(contracts)

# joining contracts to players
contracts_joined <- contracts %>% #merges player info into contract dataset
  left_join(ids, by = "otc_id") #use otc_id as matching key

# ###diagnostic check: Identify kickers with missing birthdays
# #---birth date investigation
# contracts_joined %>%
#   filter(position.x == "K") %>% #keeps only kickers
#   summarise( #counts: 
#   total_kickers = n(), #total records
#   missing_birthdate = sum(is.na(date_of_birth))# missing birt dates (NAs)
#   )
# 
# #finds kickers with missing birthdays
# contracts_joined %>%
#   filter(
#   position.x == "K",
#   is.na(date_of_birth)
#   ) %>%
# 
# select(player)#prints the missing Birthdays names

#create kicker contract dataset using each players most recent contract
kicker_contracts <- contracts_joined %>%
  filter(position.x == "K") %>%
  group_by(gsis_id.x) %>%
  slice_max(year_signed, n = 1) %>%
  ungroup()



#assess missing birth-date information for age analysis
kicker_contracts%>%
  summarise(
  total_kickers = n(),
  missing_birthdate = sum(is.na(date_of_birth)),
  pct_missing = round(
    100 * sum(is.na(date_of_birth)) / n(),
    1
  )
)


#change date format of birthday to accomadate for later 
kicker_contracts <- kicker_contracts %>%
  mutate(
  date_of_birth = as.Date(
    date_of_birth,
    format = "%B %d, %Y"
  )
)

# Create age variable
kicker_contracts <- kicker_contracts %>%
  mutate(
    age = as.numeric(
      difftime(
        Sys.Date(),
        date_of_birth,
        units = "days"
      )
    ) / 365.25
  )

#double check dates are in the correct format
#head(kicker_contracts$date_of_birth)

# 2. Load Play-by-Play for Performance
pbp <- load_pbp(2021:2023) %>% #loads everything from 2021-2023 season
  mutate(is_indoor = if_else(roof %in% c("outdoors", "open"), 0, 1))#creates new variable of whether indoor or outdoor
####is_indoor = 1 === TRUE, is_indoor = 0 === False (outdoor)
#####later on will be useful on answering do kickers perform better indoor or outdoor


#------Kicker Performance summary (2021-2023 combined)
###measuring kicker combined seasons to find pattern 
#keep only field goal attempts from play by play data
kicker_perf <- pbp %>%
  filter(field_goal_attempt == 1) %>%
  select(#retains variables needed for performance analysis
    gsis_id = kicker_player_id,
    is_indoor,
    kick_distance,
    field_goal_result
  ) %>%
  mutate(
    fg_made = if_else(field_goal_result == "made", 1, 0) # create binary variable- made = 1, missed = 0
  )

#calculate performance metrics seperatly for each environment kicking situations
kicker_summary <- kicker_perf %>%
  group_by(gsis_id, is_indoor) %>%
  summarise(
    FG_Pct = mean(fg_made, na.rm = TRUE), #fg %
    attempts = n(), #total fg attempts
    avg_distance = mean(kick_distance, na.rm = TRUE), #avg kick distance
    .groups = "drop" #removes grouping after summary
  )


#reshapre data from long to wide format- seperate indoor and outdoor stat columns
kicker_wide <- kicker_summary %>%
  mutate(env = if_else(is_indoor == 1, "indoor", "outdoor")) %>%
  select(-is_indoor) %>%   # remove this BEFORE pivot
  pivot_wider(
    names_from = env,
    values_from = c(FG_Pct, attempts, avg_distance),
    names_glue = "{.value}_{env}"
  )

#measure environmental sensitivity
kicker_wide <- kicker_wide %>%
  mutate(
    fg_diff = FG_Pct_indoor - FG_Pct_outdoor
  )

#identify kickers with largest indoor/outdoor differences
kicker_wide %>%
  arrange(fg_diff)
#identify kickers with smallest indoor/outdoor differences
kicker_wide %>%
  arrange(abs(fg_diff))

#------visualization of environmental sensitivity

#compare indoor and outdoor fg %-------------
###distribution of environmental sensitivity
##positive values indicate stronger indoor performance
##negative values indicate stronger outdoor performance
plot(
  kicker_wide$FG_Pct_outdoor,
  kicker_wide$FG_Pct_indoor,
  xlab = "Outdoor FG%",
  ylab = "Indoor FG%",
  main = "Kicker Environment Sensitivity"
)
#reference line where indoor performance = outdoor performance
abline(0, 1, col = "red")

#----Prepare data for clustering---

#keep kickers with sufficient sample sizes
k_cluster_df <- kicker_wide %>%
  filter(
    attempts_indoor >= 10,
    attempts_outdoor >= 10
  ) %>%
  select(
    gsis_id,
    FG_Pct_indoor,
    FG_Pct_outdoor,
    avg_distance_indoor,
    avg_distance_outdoor,
    fg_diff
  )
#select variables used
k_input <- k_cluster_df %>%
  select(-gsis_id)

# Remove variables with zero variance- variables that contain the same value for every kicker
k_input <- k_input[, apply(k_input, 2, var) != 0]

# Standardize variables so larger scale metrics do not dominate clustering results
k_input <- scale(k_input)

#-------hierarchical clustering-------------------------------------------------------
#measures similarity between kickers based on: 
# - indoor fg%
# - outdoor fg%
# - indoor avg kick dist
# - outdoor avg kick dist
# - environment sensitivity (fg_diff)


#----------- DIANA - divisive clustering-----------
###one large cluster then splits repeatedly- large into smaller
dist_k <- dist(k_input) #Compute Euclidean distance matrix
diana_k <- diana(dist_k) # apply diana clustering technique

 # ---- AGNES - agglomerative clustering using complete linkage ----
###begins with each kicker as own cluster then finds like matches repeatedly

agnes_k <- agnes(dist_k, method = "complete") # apply agnes clustering techniqu
#assign each kicker to one of the 3 agnes clusters


#assign each kicker to one of the 3 DIANA clusters
k_cluster_df$cluster_diana <- cutree(as.hclust(diana_k), k = 3)
#assign each kicker to one of the 3 AGNES clusters
k_cluster_df$cluster_agnes <- cutree(as.hclust(agnes_k), k = 3)


# -----------Merge contract and demographic information---------
#Merge ange and contract information into clustered kicker profiles for salary value analysis
k_cluster_df <- k_cluster_df %>%
  left_join(
   kicker_contracts %>%
   select(
      gsis_id.x,
     age,
      apy,
      value,
      guaranteed
    ),
  by = c("gsis_id" = "gsis_id.x")
)

#-------Create Value Metrics------------------------
#create overall performance measure
k_cluster_df <- k_cluster_df %>%
  mutate(
    avg_fg_pct =
    (FG_Pct_indoor + FG_Pct_outdoor) / 2
  )


#create value metric-performance generated per dollar of apy
###higher score = more performance per dollar
k_cluster_df <- k_cluster_df %>%
  mutate(
      value_score = if_else(
          !is.na(apy) & apy > 0,
          avg_fg_pct /apy,
          NA_real_
      )
        
  )
#another value metric
#lower = better
k_cluster_df <- k_cluster_df %>%
  mutate(
    cost_per_fg_pct = apy / avg_fg_pct,
    guaranteed_per_fg_pct = guaranteed / avg_fg_pct,
    value_per_fg_pct = value / avg_fg_pct
  )

# Compare cluster assignments between methods
table(
  k_cluster_df$cluster_diana,
  k_cluster_df$cluster_agnes
)

# DIANA cluster profiles
k_cluster_df %>%
  group_by(cluster_diana) %>%
  summarise(
    avg_age = mean(age, na.rm = TRUE),
    avg_apy = mean(apy, na.rm = TRUE),
    avg_contract_value = mean(value, na.rm = TRUE),
    avg_guaranteed = mean(guaranteed, na.rm = TRUE),
    
    avg_indoor = mean(FG_Pct_indoor, na.rm = TRUE),
    avg_outdoor = mean(FG_Pct_outdoor, na.rm = TRUE),
    avg_diff = mean(fg_diff, na.rm = TRUE),
    
    avg_value_score = mean(value_score, na.rm = TRUE),
    
    avg_fg_pct = mean(avg_fg_pct, na.rm = TRUE),
    avg_cost_per_fg = mean(cost_per_fg_pct, na.rm = TRUE),
    avg_guaranteed_per_fg = mean(guaranteed_per_fg_pct, na.rm = TRUE),
    avg_value_per_fg = mean(value_per_fg_pct, na.rm = TRUE),
    
    n = n()
)

# AGNES cluster profiles
k_cluster_df %>%
  group_by(cluster_agnes) %>%
  summarise(
    avg_age = mean(age, na.rm = TRUE),
    avg_apy = mean(apy, na.rm = TRUE),
    avg_contract_value = mean(value, na.rm = TRUE),
    avg_guaranteed = mean(guaranteed, na.rm = TRUE),
    
    avg_indoor = mean(FG_Pct_indoor, na.rm = TRUE),
    avg_outdoor = mean(FG_Pct_outdoor, na.rm = TRUE),
    avg_diff = mean(fg_diff, na.rm = TRUE),
    
    avg_value_score = mean(value_score, na.rm = TRUE),
    
    avg_fg_pct = mean(avg_fg_pct, na.rm = TRUE),
    avg_cost_per_fg = mean(cost_per_fg_pct, na.rm = TRUE),
    avg_guaranteed_per_fg = mean(guaranteed_per_fg_pct, na.rm = TRUE),
    avg_value_per_fg = mean(value_per_fg_pct, na.rm = TRUE),
    
    n = n()
)



# --- export to csv---
write.csv(k_cluster_df, "kicker_clusters.csv", row.names = FALSE)


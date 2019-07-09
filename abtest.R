rm(list = ls())

library(rlang)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(brew)
library(pwr)


df <- read.csv("C:/Users/J/Documents/R Projects/Data/ab-testing/ab_data.csv", header = TRUE)
head(df)


unique.id <- unique(df$user_id)
length(unique.id)


notaligned.user <- df %>% filter( group == "treatment" & landing_page == "old_page")
notaligned.user2 <- df %>% filter( group == "control" & landing_page == "new_page")
notaligned.user.all <- rbind(notaligned.user, notaligned.user2)
dim(notaligned.user.all)


# create 2*2 table
num_aligned_ctrl = nrow(df %>% 
                          filter( group == "control" & landing_page == "old_page"))
num_aligned_treat = nrow(df %>% 
                           filter( group == "treatment" & landing_page == "new_page")) 

# data frame for differ the control and treatment group with aligned and notaligned count
align_plot_data = data.frame("Group" = c("control","treatment"), "Aligned" = c(num_aligned_ctrl,num_aligned_treat), "Not_aligned" = c(nrow(notaligned.user2), nrow(notaligned.user)), stringsAsFactors = FALSE)

align_plot_data2 = align_plot_data %>% 
  gather(`Aligned`, `Not_aligned`, key = "AlignOrNot", value = "Number")


# plot aligned and not aligned in ctrl and treat groups
# plot bar chart
# http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization

# Stacked barplot with multiple groups
ggplot(data=align_plot_data2, aes(x=align_plot_data2$Group, y=align_plot_data2$Number, fill=AlignOrNot)) +
  geom_bar(stat="identity", position=position_dodge2(reverse = TRUE), width = 0.6) +
  scale_fill_manual(values=c('#aecfe4','#425b84')) +  # scale_fill_brewer(palette="Dark2")  # color https://www.color-hex.com/color-palette/71239
  geom_text(aes(label=align_plot_data2$Number), vjust=-0.1, color="dark grey",
            position=position_dodge2(0.6, reverse = TRUE), size=3.5) +
  labs(title="Not Aligned vs. Aligned Count", 
       x="Group", y = "Count") +
  theme(legend.position="top") +   # change legend position
  theme_grey() 

# change y axis unit to 15K  https://stackoverflow.com/questions/13973644/si-prefixes-in-ggplot2-axis-labels
# + scale_y_continuous(labels=format_si()) +


# delete the not aligned data from the original dataset
df.clean <- df %>% anti_join(notaligned.user.all)
dim(df)
dim(df.clean)

#Step2: if a user clicked several times, only keep the first result for analysis.
# arrange date from earlist to latest
length(unique(df.clean$user_id))
length(df.clean2$user_id)
df.clean2 <- df.clean %>% group_by(user_id) %>% arrange(timestamp)
# keep only the earlist record for each user
df.clean3 <- df.clean2[!duplicated(df.clean2$user_id), ] # used the time ascending dataset
length(df.clean3$user_id)
### Step 3: check missing value
# check missing value
df.clean3[!complete.cases(df.clean3),]
# no NA

# check unique id 
unique.id <- unique(df.clean3)
dim(unique.id)

df.final <- df.clean3


#### Testing

#control
landing.total <- df.final %>% group_by(landing_page) %>% count(landign_page)
landing.total

converted.total <- df.final %>% group_by(landing_page) %>% count(converted)
converted.total

summarise(group_by(df.final,landing_page,converted), count = n())


prop.test(c(17264,17489),c(145310,145274), correct = FALSE)
# 

pwr.2p2n.test(h=0.2, n1= 145310, sig.level = 0.05, power = 0.9)

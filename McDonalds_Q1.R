#Q1 - McDonalds

library(stringr)
library(ggplot2)
library(data.table)
library(dplyr)
install.packages("repr") #for resizing the figures
library(repr)


menu<-read.csv(file.choose())


# check if any column contains na
sum(is.na(menu)) 

# drop "Calories_From_Fat" as it's already included in "Calories"
menu = menu %>%
  select(-Calories_from_Fat)

menu = menu %>%
  mutate(Category = as.factor(Category),
         Item = as.factor(Item))

glimpse(menu)

options(repr.plot.width = 14, repr.plot.height = 14)

menu %>%
  count(Category) %>%
  ggplot(aes(x =(Category), y = n)) +
  geom_col(fill = "deepskyblue4") +
  ggtitle("Number of Items in Each Category") +
  labs(y= "Count", x = "Category") +
  geom_text(aes(label=n), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 90, size = 15),
        axis.text.y = element_text(size = 15))

menu$Calories

mean_cal = round(mean(menu$Calories),2) # average calories in the whole menu 

options(repr.plot.width = 14, repr.plot.height = 14)
menu %>%
  group_by(Category) %>%
  summarize(mean_cat_cal = round(mean(Calories), 2)) %>%
  ggplot((aes(Category, mean_cat_cal)), mean_cat_cal) +
  geom_col(fill = "deepskyblue4") +
  geom_hline(yintercept = mean_cal, linetype = "dashed")+
  ggtitle("Average Calories in Each Category") +
  labs(y= "Avg. Cal", x = "Category") +
  geom_text(aes(label=mean_cat_cal), vjust = -0.5) +
  geom_text(aes(8, mean_cal, label=str_c("Menu Average = ", as.character(mean_cal)), vjust = -0.5),color = "darkred") +
  theme(axis.text.x = element_text(angle = 90, size = 15),
        axis.text.y = element_text(size = 15))

#Average meal = 368.27


#Giving highest abd lowest calories of each category
options(repr.plot.width = 14, repr.plot.height = 20)
Items_Calories_Draw <- function(Cat_name){
  menu %>%
    filter(Category == Cat_name) %>%
    ggplot(aes(x = (Item),y = Calories)) +
    geom_col(fill = "deepskyblue4") +
    coord_flip() +
    ggtitle(str_c("Calories of Each Item in Categoty ", Cat_name))+
    xlab("Item") +
    theme(axis.text.x = element_text(angle = 90, size = 15),
          axis.text.y = element_text(size = 15))
  
}
lapply(levels(menu$Category), Items_Calories_Draw)

menu_tot_cal = sum(menu$Calories)

# now let's see by how much "Soda" contribute to the overall caloric intake 
Soda_total_cal = menu %>%
  filter(Category == "Beverages") %>%
  select(Calories) %>%
  #slice(1:20) %>%
  sum()
Soda_percent = Soda_total_cal / menu_tot_cal * 100

# now let's see by how much "Coffee" contribute to the overall caloric intake 
Coffee_total_cal = menu %>%
  filter(Category == "Coffee & Tea") %>%
  select(Calories) %>%
  #slice(9:n()) %>%
  sum()
Coffee_percent = Coffee_total_cal / menu_tot_cal * 100

Soda_percent
Coffee_percent

print("Soda contributes by 3.2 percent and Coffee contributes by 28.2 percent")



Chicken_menu = menu %>%
  filter(Category == "Chicken & Fish", Item != "Filet-O-Fish") 

Chicken_menu = Chicken_menu %>%
  mutate(type = case_when(str_detect(Item, "Grilled") == 1 ~ "Grilled",
                          TRUE ~ "Crispy"),
         type = as.factor(type))

options(repr.plot.width = 14, repr.plot.height = 14)
ggplot(Chicken_menu, aes(x = type, y = Calories, fill = type)) +
  geom_bplot()+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))+
  ggtitle("Comparison between Grilled vs Crispy Chicken Sandwich w.r.t Calories")
print("Avegare calories of grilled chicken sandwich is less than crispy ")

options(repr.plot.width = 14, repr.plot.height = 14)
ggplot(Chicken_menu, aes(x = type, y = Protein, fill = type)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))+
  ggtitle("Comparison between Grilled vs Crispy Chicken Sandwich w.r.t Protein")

print("Protein is high in Crispy chicken sandwich than grilled sandwich")


Eggs_menu = menu %>% 
  filter(str_detect(str_to_lower(Item), "egg")) %>%
  mutate(Type = case_when(str_detect(str_to_lower(Item), "white") ~ "Eggs Whites",
                          TRUE ~ "Whole Eggs"),
         Type = as.factor(Type))
# now let's compare Eggs Whites vs Whole Eggs w.r.t Calories
options(repr.plot.width = 14, repr.plot.height = 14)
ggplot(Eggs_menu, aes(x = Type, y = Calories, fill = Type)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))+
  ggtitle("Comparison between Whole Eggs vs Eggs Whites w.r.t Calories")

print("Egg whites have more calorie than whole egg")

# now let's compare Eggs Whites vs Whole Eggs w.r.t Protein
options(repr.plot.width = 14, repr.plot.height = 14)
ggplot(Eggs_menu, aes(x = Type, y = Protein, fill = Type)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))+
  ggtitle("Comparison between Whole Eggs vs Eggs Whites w.r.t Protein")

print("Egg whites has more protein than whole egg")

# now let's compare Eggs Whites vs Whole Eggs w.r.t Cholesterol
options(repr.plot.width = 14, repr.plot.height = 14)
ggplot(Eggs_menu, aes(x = Type, y = Cholesterol, fill = Type)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))+
  ggtitle("Comparison between Whole Eggs vs Eggs Whites w.r.t Cholesterol")

print("The average cholestrol for eggs whites way less than the whole eggs, looks like that eggs yolk contains very much cholesterol")

print("Hence it is better to order egg white than ordering whole egg as egg white is more nutricious")

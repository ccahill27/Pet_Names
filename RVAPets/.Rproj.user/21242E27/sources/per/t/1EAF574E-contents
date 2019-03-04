library(tidyverse)

Pet_Names <- read_csv('Pet_Licenses.csv')
colnames(Pet_Names)<-c("Type","Name","Location","Date")

Pet_Names$Type<-gsub('Kitten','Cat',Pet_Names$Type)
Pet_Names$Type<-gsub('Puppy','Dog',Pet_Names$Type)
Pet_Names$Name<-gsub('Mr. ','Mr.',Pet_Names$Name)
Pet_Names$Name<-gsub('Mr ','Mr.',Pet_Names$Name)

##Take First Word from Name
Pet_Names$Name <-word(Pet_Names$Name,1)


#Find Most Common Cross Species
common_names <- as.data.frame(table(Pet_Names[,2]))

common_names <- common_names[order(common_names[,2],decreasing=TRUE),]



common_names[1:30,] 

library(RColorBrewer)

grad<-colorRampPalette(c("skyblue", "navy"))(80)

ggplot(common_names[1:80,], aes(x = reorder(Var1, Freq),y=Freq))+
  geom_bar(stat='identity',fill=grad) +
  scale_y_continuous(breaks=seq(0,50,5)) +
  labs(x="Name",y="Total Cats and Dogs Named",title="Most Common Cat and Dog Names in Richmond, VA") +
  coord_flip()

ggplot(aes(reorder(common_names,Freq),Freq))



## FIND DOG NAMES

Pet_Names %>% filter(Type=='Dog') -> Dog_Names
Dog_Names <- as.data.frame(table(Dog_Names[,2]))
Dog_Names <- Dog_Names[order(Dog_Names[,2],decreasing=TRUE),]
colnames(Dog_Names)<-c("Name","Count")
Dog_Names$Animal <- rep('Dog',nrow(Dog_Names))



ggplot(Dog_Names[1:50,], aes(x = reorder(Name, Count),y=Count))+
  geom_bar(stat='identity') +
  coord_flip()

#Name Map
library('wordcloud2')
wordcloud(words=Dog_Names$Name,freq=Dog_Names$Count,min.freq = 3,
          random.order = FALSE,rot.per = 0.35,colors = brewer.pal(8,"Dark2"))

wordcloud2(Dog_Names,figPath = 'dog_head.png')

## FIND CAT NAMES

Pet_Names %>% filter(Type=='Cat') -> Cat_Names
Cat_Names <- as.data.frame(table(Cat_Names[,2]))
Cat_Names <- Cat_Names[order(Cat_Names[,2],decreasing=TRUE),]
colnames(Cat_Names)<-c("Name","Count")
Cat_Names$Animal<-rep('Cat',nrow(Cat_Names))



ggplot(Cat_Names[1:50,], aes(x = reorder(Name, Count),y=Count))+
  geom_bar(stat='identity') +
  coord_flip()

#Name Map
wordcloud(words=Cat_Names$Name,freq=Cat_Names$Count,min.freq = 2,
          random.order = FALSE,rot.per = 0.35,colors = brewer.pal(8,"Dark2"))

wordcloud2(Cat_Names,figPath = 'cat_head.jpg')
wordcloud2(Cat_Names,figPath='cat.png')


#Merge

All_Names<-Pet_Names[,1:2]
colnames(All_Names)<-c('Animal','Name')



#Plot Most Common Dog vs. Cat

Most_Common <- Dog_Names[1:34,1]


Com_Names<-All_Names[is.element(All_Names$Name,Most_Common),]







plotting_df <-
  Com_Names %>% 
  group_by(Name, Animal) %>% 
  summarise(Freq = n()) %>% 
  # a trick!
  mutate(Freq = if_else(Animal == "Dog", -Freq, Freq))


library(grDevices)
cb_palette <- c("orchid3", "dodgerblue3")



temp_df <-
  plotting_df %>% 
  filter(Animal == "Dog") %>% 
  arrange(-Freq)
the_order <- temp_df$Name


p <- plotting_df %>%
  ggplot(aes(x = Name, y = Freq, group = Animal, fill= Animal)) +
  geom_bar(stat='identity',width=0.75) +
  coord_flip() +
  scale_x_discrete(limits = the_order) +
  scale_y_continuous(breaks = seq(-50, 12, 2), 
                     labels = abs(seq(-50, 12, 2))) +
  labs(x = "Name", y = "Count", title = "Most Common Dog Names vs. Cat Names") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill =  "grey90")) +
  # reverse the order of items in legend
  # guides(fill = guide_legend(reverse = TRUE)) +
  # change the default colors of bars
  scale_fill_manual(values=cb_palette,
                    name="",
                    breaks=c("Dog", "Cat"),
                    labels=c("Dog", "Cat")) 


p






#Plot Most Common Cat vs. Dog

Most_Common <- Cat_Names[1:35,1]


Com_Names<-All_Names[is.element(All_Names$Name,Most_Common),]







plotting_df <-
  Com_Names %>% 
  group_by(Name, Animal) %>% 
  summarise(Freq = n()) %>% 
  # a trick!
  mutate(Freq = if_else(Animal == "Dog", -Freq, Freq))

temp_df <-
  plotting_df %>% 
  filter(Animal == "Cat") %>% 
  arrange(Freq)
the_order <- temp_df$Name



p <- plotting_df %>%
  ggplot(aes(x = Name, y = Freq, group = Animal, fill= Animal)) +
  geom_bar(stat='identity',width=0.75) +
  coord_flip() +
  scale_x_discrete(limits = the_order) +
  scale_y_continuous(breaks = seq(-50, 10, 2), 
                     labels = abs(seq(-50, 10, 2))) +
  labs(x = "Name", y = "Count", title = "Dog Names vs. Most Common Cat Names") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill =  "grey90")) +
  # reverse the order of items in legend
  # guides(fill = guide_legend(reverse = TRUE)) +
  # change the default colors of bars
  scale_fill_manual(values=cb_palette,
    name="",
    breaks=c("Dog", "Cat"),
    labels=c("Dog", "Cat")) 

p


Pet_Names$Group <- rep('Animal',nrow(Pet_Names))

dogs<-nrow(Pet_Names[Pet_Names$Type=='Dog',])/nrow(Pet_Names)
cats<-nrow(Pet_Names[Pet_Names$Type=='Cat',])/nrow(Pet_Names)


dog_mid<- nrow(Pet_Names[Pet_Names$Type=='Dog',]) / 2
cat_mid<- nrow(Pet_Names[Pet_Names$Type=='Dog',])  +  nrow(Pet_Names[Pet_Names$Type=='Cat',])/2

n_dog<-nrow(Pet_Names[Pet_Names$Type=='Dog',])


g <- ggplot(Pet_Names,aes(Group))
g + 
  geom_bar(aes(fill=Type)) +
  labs(x="",y="",title="Percentage of Dogs and Cats to Total Dog/Cat Pet Population in Richmond, VA") +
  coord_flip() +
  scale_fill_manual(values=cb_palette,
                    name="",
                    breaks=c("Dog", "Cat"),
                    labels=c("Dog", "Cat")) +
  annotate("text", label = "78.4%", x = 1, y = dog_mid, size = 25, colour = "white") +
  annotate("text", label = "21.6%", x = 1, y = cat_mid, size = 25, colour = "white") +
  scale_y_continuous(breaks=c(0,n_dog,nrow(Pet_Names)),labels=c(0,n_dog,nrow(Pet_Names)))+
  theme_bw() + theme(panel.border = element_blank(),panel.grid = element_blank())


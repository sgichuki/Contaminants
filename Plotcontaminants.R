library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(reshape2)
library(gridExtra)

#Load data
fishconc_df<-read.csv("~/GitHub/Contaminants/fishconc.csv")

#Change data types, lapply function applies the conversion to all columns selected
#To convert the dates, using ymd(year, month, date) from lubridate
fishconc_df[,3:5] <- lapply(fishconc_df[,3:5],as.factor)
fishconc_df[,1:2] <- lapply(fishconc_df[,1:2],ymd)

#Subset data by year of sampling, this splits for all sampling years.
df_list<-split(fishconc_df, format(as.Date(fishconc_df$sample_date), "%Y"))

#To subset for one year only
#year1<-subset(fishconc_df, format(as.Date(sample_date),"%Y")==2000)

#Combine the list into a new dataframe. This code appends an id column with the year when the 
#list is bound into a new data frame
yearly_allrows<-bind_rows(df_list,.id = "id")

#Scatter plot - length of fish versus amount of Hg 
ggplot(yearly_allrows,aes(x=length_mm,y=amount_ppm))+
  geom_point(aes(colour=species_code_description),size=2)

#Violin plot of Hg concentrations in fish for each year
hgconc_violinplot=ggplot(yearly_allrows,aes(x = id,y=amount_ppm))+geom_violin(draw_quantiles = c(0.25, 0.5, 0.75),na.rm = TRUE)+
  xlab("Hg conc in fish")+ylab("Hg conc(ppm)")+
  ggtitle("Hg conc's in fish for each year 2000-2005")

ggsave("hgconc_violinplot.png",width = 297,height = 210,units = c("mm"),dpi = 300)

#Boxplot of Hg concentrations in fish for each year
ggplot(yearly_allrows,aes(x = id,y=amount_ppm))+
  geom_boxplot(outlier.color = "red",na.rm = TRUE)+
  xlab("Hg conc in fish")+ylab("Hg conc(ppm)")+
  ggtitle("Conc's in fish for each year 2000-2005")

boxplot_hginfish=ggplot(data = fishconc_df, aes(x = species_code_description,y=amount_ppm))+
  geom_boxplot(outlier.colour ="red",na.rm = TRUE)+theme_bw()+xlab("")+
  ylab("conc(ppm)")+ggtitle("Hg conc. in fish per species, outliers in red")+
  coord_flip()

ggsave("boxplot_hginfish.png",width = 297,height = 210,units = c("mm"),dpi = 300)

#Group by multiple colums using dplyr 
yearlyconc_df <-yearly_allrows %>%
  group_by(as.factor(id), species_code_description) %>%
  summarize(amount_ppm = mean(amount_ppm,na.rm = TRUE),.groups='drop')

#Rename the first column 
names(yearlyconc_df)[1]<-"id"

#Plot average concentration per fish species for each year
avgfishconc_plot=ggplot(yearlyconc_df, aes(id, amount_ppm, fill = species_code_description)) + 
  geom_bar(stat="identity",position="dodge")+xlab("Sampling years")+labs(fill = "species")+
  ylab("Hg conc in fish(ppm)")+scale_fill_brewer(palette = "Paired")+
  facet_grid(~id,scales = "free")+
  ggtitle("Average Hg in each species of fish")

ggsave("avgfishconc_plot.png",width = 297,height = 210,units = c("mm"),dpi = 300)

#Plot overall Hg conc per fish species
fishconcbyspecies <- fishconc_df %>%
  group_by(species_code_description) %>%
  summarise(amount_ppm = mean(amount_ppm, na.rm = TRUE))

fishconc_perspecies=ggplot(data = fishconcbyspecies, mapping = aes(x = reorder(species_code_description, amount_ppm), amount_ppm)) + 
  geom_bar(stat = "Identity",fill="goldenrod",color="blanchedalmond") + 
  ggtitle("Overall average Hg concentration by species")+
  coord_flip()+xlab("")+theme_bw()

ggsave("fishconc-perspecies.png",width = 297,height = 210,units = c("mm"),dpi = 300)


#Plot overall median fish length for each species
fishlengthbyspecies <- fishconc_df %>%
  group_by(species_code_description) %>%
  summarise(length_mm = median(length_mm, na.rm = TRUE))

fishlength_plot=ggplot(data = fishlengthbyspecies, mapping = aes(x = reorder(species_code_description, length_mm), length_mm)) + 
  geom_bar(stat = "Identity",fill="goldenrod",color="blanchedalmond") + 
  ggtitle("Median length by species")+ylab("length(mm)")+
  coord_flip()+xlab("")+theme_bw()

ggsave("fishlength_plot.png",width = 297,height = 210,units = c("mm"),dpi = 300)





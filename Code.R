library(tidyverse)
barriers<-c("Bias or prejudice at the recruitment stage","Prejudice of employers",
            "Lack of diversity in leadership","Workplaces not being diverse",
            "Lack of opportunities or connections","Inability to undertake an unpaid internship",
            "Lack of qualifications","Other")
value<-c(54,54,52,50,47,25,25,1)

cbind(barriers,value)->df1
data.frame(df1)->df1
df1$value<-as.numeric(df1$value)

df1%>%
  mutate(
    lilt=ifelse(row_number()==1:4,"black","gray95"),
    position=ifelse(row_number()==1:7,1.5,-0.5),
    textcolour=ifelse(row_number()==1:4,"white","black")
  )->df1


ggplot(df1,aes(x=value,y=reorder(barriers,value,decreasing=TRUE),fill=lilt,label=paste0(value,"%")))+
  geom_col(width = 0.7,colour="black")+
  geom_text(fontface="bold",aes(hjust=position,colour=textcolour),show.legend = FALSE)+
  scale_colour_manual(values=c("black","white"))+
  scale_fill_manual(values=c("#fe4a49","#dddddd"))+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(face="bold",size=12,colour="black",margin=margin(r=-21)),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill="gray95"),
    panel.background = element_rect(fill="gray95"),
    legend.position = "none",
    plot.margin = unit(c(0.5,1,0.5,1),"cm"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(size=18,face="bold",colour = "black",margin = margin(b=12)),
    plot.subtitle = element_text(size=14,colour = "black",margin = margin(b=24)),
    plot.caption = element_text(hjust=0,size=10,colour = "black",margin = margin(t=12))
  )+
  labs(title="KEY BARRIERS FOR YOUNG BLACK PEOPLE GOING INTO EMPLOYMENT IN THE UK",
       subtitle=str_wrap("Data show that bias at the recruitment levels, prejudice of employers, and workplaces not being diverse were the main hindrances for the young black population venturing into employment in the United Kingdom, in 2020",120),
       caption="Source: Statista| Design: @annapurani93"
      )->plot1


ggsave("keybarriers.png",plot1,width = 12,height = 7)
ggsave("keybarriers.pdf",plot1,width = 12,height = 7)

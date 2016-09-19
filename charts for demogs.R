library(dplyr)
library(ggplot2)

setwd("W:/WES Data & Frame/2015/Data/CORE/Deeper analysis/Demographics/R check")

dmeans_c <- read.csv("dmeans_for_charts.csv",header=TRUE)

# Create comparison chart used in PPT presentations
# Demographic is Supervisor Status comparing Staffing Practices driver scores to BCPS
comp_BCPS <- dmeans_c %>%
  mutate(Group_label=paste(Group," ","(",round(Average,0),")",sep="")) %>%  # concatenate driver score to demog group for label
  filter(Demographic=="Supervisor Status" & Driver=="Staffing Practices")

# Match driver colours to driver scores
driver_colours <- c("#FFAA71", "#FFE36D", "#98E09A", "#ABCBEF", "#C5A7BF")
names(driver_colours) <- c("Understand your challenges", "Focus on improvements", "Leverage your strengths", "Celebrate your successes", "Model your achievements")

# Plot
ggplot(comp_BCPS, aes(x=Group, y=Compare, fill=Legend)) + 
  geom_bar(stat="identity", width=0.5) +
  geom_text(aes(label=round(comp_BCPS$Compare,0))) +  # Add driver diff as label
  scale_fill_manual(values=driver_colours) +  # fill bars based on driver score
  scale_x_discrete(labels=comp_BCPS$Group_label) +  # y-axis labels = demog group concatenated with driver score
  scale_y_continuous(limits=c(-20,100), breaks=seq(-20,100,by=10)) +
  ylab("Difference in scores from BC Public Service") +
  geom_hline(yintercept = 0) +
  coord_flip() +
  theme_classic() +
  theme( 
    axis.text.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks=element_blank(),
    legend.position="right",
    legend.title=element_blank())

# Create simple bar charts for comparing drivers used frequently in reports
# Demographic = Appointment Status, Drivers = Engagement and Professional Development
comp_drivers <- dmeans_c %>%
  filter(Demographic=="Appointment Status", Driver %in% c("Engagement","Professional Development"))

ggplot(comp_drivers,aes(x=Driver, y=Average, fill=Group)) +
  geom_bar(stat = "identity", position="dodge") +
  geom_text(aes(label=round(Average,0)),position=position_dodge(width=0.9),vjust=-1,fontface="bold") +
  scale_fill_manual(values=c("#234075","#17A1E3")) +
  scale_y_continuous(limits=c(0,100), breaks=seq(0,100,10), expand=c(0,0)) +
  annotate(geom="text",x=-Inf, y=Inf, vjust="inward",hjust="inward", label="Average Score\n(out of 100 points)") +
  theme_classic() +
  theme(
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.title.y=element_blank(),
    axis.title.x=element_blank(),
    legend.position="bottom",
    legend.title=element_blank(),
    legend.text=element_text(face = "bold"),
    axis.line.x=element_line(color="black"))


# Create a line chart for comparing a lot of drivers between demographic groups
# Demographic = Appointment Status
# This chart is not very polished, need to improve x-axis labels
line_cht <- dmeans_c %>%
  filter(Demographic=="Appointment Status")

ggplot(line_cht,aes(x=Driver, y=Average, colour=Group, shape=Group, group=Group)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(limits=line_cht$Driver) +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10)) +
  # rename the legend to the type of demog chosen
  scale_colour_manual(name=as.character(first(line_cht$Demographic)),values=c("#234075","#17A1E3")) + 
  scale_shape_discrete(name=as.character(first(line_cht$Demographic))) +
  ylab("Average Score (out of 100 points)") +
  theme_classic() +
  theme(
    axis.text.x=element_text(angle=90),
    axis.line.x=element_line(color="black"),
    axis.line.y=element_line(color="black"))









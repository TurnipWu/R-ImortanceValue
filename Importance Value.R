library(tidyverse)
# Example data
exampledata <- tribble(
  ~plot,~name,~DBH,
  "0101","S1",1,
  "0101","S1",5,
  "0101","S2",2,
  "0201","S1",3,
  "0201","S1",1
)
# Custom function to calculate dominance
Dom <- function(x){
  sum((x/2)^2*pi)
}
# Count the total number of plots
plot_count <- length(unique(exampledata$plot))
# Calculate dominance, abundance, and frequency
# Then calculate the relative values and Importance Value (Iv)
save_data <-exampledata|>
  group_by(name)|>
  reframe(
    Dom=Dom(DBH),# Dominance
    Abu=n(),# Abundance
    Fre=length(unique(plot))/plot_count)|># Frequency
  mutate(
    RelDom=Dom/sum(Dom), # Relative Dominance
    RelAbu=Abu/sum(Abu), # Relative Abundance
    RelFre=Fre/sum(Fre), # Relative Frequency
    Iv=(RelDom+RelAbu+RelFre)/3 # Importance Value
  )
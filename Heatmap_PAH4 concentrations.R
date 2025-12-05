library(tidyverse)
library(reshape2)
library(ggtext)

# Data
df <- read.table(header=TRUE, text='
TeaType BaA CHR BbF BaP
G01 ND ND ND ND
G02 ND ND ND ND
G03 ND ND ND ND
G04 ND ND ND ND
G05 NQ ND ND ND
G06 0.271 NQ ND 0.346
G07 ND ND ND ND
G08 ND ND ND ND
G09 ND ND ND ND
G10 ND ND ND ND
G11 ND ND ND ND
G12 ND ND ND ND
G13 ND ND ND ND
G14 0.459 0.481 0.379 0.379
G15 ND ND ND ND
G16 ND ND ND ND
O01 ND ND ND ND
O02 ND ND ND ND
O03 ND ND ND ND
O04 ND ND ND ND
O05 ND ND ND NQ
O06 ND ND ND ND
O07 ND ND ND ND
O08 ND ND ND ND
O09 ND ND ND ND
O10 ND ND ND ND
O11 NQ ND NQ 0.379
O12 ND ND ND ND
O13 ND ND ND ND
O14 ND ND ND ND
O15 ND ND ND ND
O16 ND ND ND ND
O17 ND ND ND ND
O18 ND ND ND ND
O19 ND ND ND ND
O20 ND ND ND ND
B01 ND ND ND ND
B02 ND ND ND ND
B03 ND ND ND ND
B04 ND ND ND ND
B05 ND ND ND ND
B06 ND ND ND ND
B07 ND ND ND ND
B08 ND ND ND ND
B09 ND ND ND ND
B10 ND ND ND ND
B11 ND ND ND ND
B12 ND ND ND NQ
B13 ND ND ND NQ
B14 ND ND ND ND
B15 ND ND ND ND
', stringsAsFactors=FALSE)

# Convert ND/NQ
convert_numeric <- function(x, compound){
  if(compound == "BaA"){
    if(x == "ND") return(NA)
    if(x == "NQ") return(0.20)
  } else {
    if(x == "ND") return(NA)
    if(x == "NQ") return(0.30)
  }
  return(as.numeric(x))
}

df_num <- df
for(col in colnames(df)[-1]){
  df_num[[col]] <- sapply(df[[col]], convert_numeric, compound=col)
}

# Reshape
df_long <- melt(df_num, id.vars="TeaType", variable.name="Compound", value.name="Value")
df_long$Label <- melt(df, id.vars="TeaType")$value

# Add group info
df_long$Group <- substr(df_long$TeaType, 1, 1)
df_long$Group <- recode(df_long$Group, "G"="Green", "O"="Oolong", "B"="Black")

# Axis text colors
axis_colors <- c(Green="darkgreen", Oolong="darkorange", Black="black")

# Plot
ggplot(df_long, aes(x=TeaType, y=Compound, fill=Value)) +
  geom_tile(color="grey70") +
  geom_text(aes(label=ifelse(Label=="ND", "", Label)),
            size=2, colour=ifelse(df_long$Label=="NQ", "blue", "black")) +
  scale_fill_gradient(low="white", high="red", na.value="white") +
  facet_wrap(~Group, scales="free_x", nrow=1) +
  theme_minimal() +
  theme(axis.text.x = ggtext::element_markdown(angle=90, size=6),
        axis.text.y = element_text(size=10),
        strip.text = element_text(size=12, face="bold"),
        panel.grid = element_blank()) +
  scale_x_discrete(
    guide = guide_axis(check.overlap = TRUE),
    labels = function(x) {
      mapply(function(txt, grp){
        color <- axis_colors[grp]
        paste0("<span style='color:", color, "'>", txt, "</span>")
      }, x, substr(x, 1, 1) %>% recode(G="Green", O="Oolong", B="Black"))
    }
  ) +
  labs(title="PAH4 Concentrations in Tea Samples", fill="Value") +
  theme(plot.title=element_text(hjust=0.5))

# fonts in windows -------------
# see https://github.com/wch/extrafont
install.packages('extrafont')
library(extrafont)

font_import()
# This tries to autodetect the directory containing the TrueType fonts.
# If it fails on your system, please let me know.

# Vector of font family names
fonts()

# Show entire table
fonttable()

# Register fonts for Windows bitmap output
loadfonts(device="win")

library(ggplot2)
library(ggthemes)

fontTable = fonttable()

fontTable$Face = with(fontTable, ifelse(Bold & Italic, "bold.italic", 
                            ifelse(Bold, "bold",
                                   ifelse(Italic, "italic", "plain"))))
fontTable$Face = factor(fontTable$Face, levels = c("plain","bold","italic","bold.italic"), ordered = TRUE)
fontTable$FamilyName = factor(fontTable$FamilyName, levels = rev(sort(unique(fontTable$FamilyName))), ordered = TRUE)

p = ggplot(fontTable) +
  geom_text(aes(x=Face, y=FamilyName, label=FullName, family=FamilyName, fontface=Face)) +
  labs(title="Windows Fonts in R", x=NULL, y=NULL) +
  theme_tufte() +
  theme(axis.ticks = element_blank(),
        axis.text=element_text(size=12, colour="gray40", family='Arial'),
        axis.text.x=element_text(face=c("plain","bold","italic","bold.italic")),
        plot.title=element_text(size=16, family='Arial'))

ggsave("Figures/font_ggplot_map.png", width = 10, height = 49)


"Bembo" %in% fonts()

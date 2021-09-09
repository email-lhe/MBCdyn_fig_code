rm(list=ls())
library(ggplot2)
library(basicTrendline)
library(graphics)
library(grid)
library(gridExtra)
library(lemon)

add_label_legend <- function(pos = "topleft", label, ...) {
  legend(pos, label, bty = "n", ...)
}

mbc_obs_tot <- read.table("data_vali.csv", sep = ",", header = T)
mbc_obs_tot$data_obs = log10(mbc_obs_tot$data_obs)

mbc_sim_tot <- read.table("data_sim.csv", sep = ",", header = T)
mbc_sim_tot$data_sim = log10(mbc_sim_tot$data_sim)

validation_mbc_tot <- data.frame(mbc_obs_tot, mbc_sim_tot)
colnames(validation_mbc_tot) = c("month", "data_sim", "data_obs")
validation_mbc_tot[validation_mbc_tot == -9999] <- NA


#color <- c("aquamarine4", "chartreuse4", "azure4", "transparent", "black", "cornflowerblue", "white", "limegreen", "springgreen4", "olivedrab", "seagreen4", "gold4")
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

#color <- c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff', '#000000')
color <- c('#e6194b', '#ffe119', '#f58231', '#46f0f0', '#bcf60c', '#008080', '#9a6324', '#800000', '#808000', '#000075', '#808080', '#000000')


png("Fig_S6.png", width=12, height=10, units="cm",res=600)
par(family = "Times New Roman", mar=c(4,5,2.5,1))

scaleFUN <- function(x) sprintf("%.1f", x)

fig = ggplot() + theme_void()

gt <- ggplot_gtable(ggplot_build(fig))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)


view1 <- viewport(width=0.88,height=0.99,x=0.41,y=0.51)


fit_point <- lm(data_sim ~ data_obs + 0, data = na.omit(data_tot))
fig_point <- ggplot(data_tot, aes(x=data_obs, y=data_sim)) +
   theme_classic() + 
   geom_abline(slope=1, intercept=0, col = "red") +
   theme(axis.text.y   = element_text(size=14, family = "Times New Roman"),
        axis.text.x   = element_text(size=14, family = "Times New Roman"),
        axis.title.y  = element_text(size=14, family = "Times New Roman", angle = 90, vjust = -1),
        axis.title.x  = element_text(size=14, family = "Times New Roman", angle = 0, vjust = -1),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(0.2, 0.2, 0, 0.2, "cm"),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_rect(colour=NA, fill=NA), strip.text.x = element_blank()) +
     geom_point(col = color[as.numeric(data_tot$month)], alpha = 0.6) +
     labs(x= expression(paste("Log(observed MBC [g C ", m^-2, "])")), y = expression(paste("Log(simulated MBC [g C ", m^-2, "])"))) +
     geom_smooth(formula=y~0+x, method = "lm", alpha = .4, col = "black", aes(fill = data_sim)) + 
     annotate(label = sprintf("y = %.3f x\nR² = %.2f", coef(fit_point), summary(fit_point)$r.squared), geom = "text", family = "Times New Roman", x = 3, y = 0.5, size = 6) +
     theme(legend.position = "none") +
     scale_y_continuous(labels=scaleFUN, limits=c(-1, 4.0)) +
     scale_x_continuous(labels=scaleFUN, limits=c(-1, 4.0))

print(fig_point, vp=view1)


view2 <- viewport(width=0.2,height=0.99,x=0.98,y=0.55)

Legend <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")
shots <- rep('Label',12)
xc <-c(1:1)
yc <-c(1:12)
df <-data.frame(shots,Legend,xc,yc)
fig_legend <- ggplot(df, aes(x = xc, y = yc, color = color)) +
  geom_point(alpha = 0) +
  scale_color_manual(values = color,
    labels = month,
    name = '', drop=FALSE) +
  guides(color = guide_legend(override.aes = list(alpha = 1), # make line visible in legend 
                              direction = "vertical",
                               )) + 
  #Theme
  theme(
    panel.background = element_rect(fill = "transparent",colour = "transparent"),
    plot.margin = unit(c(0.1,2,0.1,1), "cm"),
    plot.title = element_text(family = "Times New Roman", size = 1, hjust = 0.5, vjust = 1),
    plot.background = element_rect(fill = "transparent", colour = "transparent"),
    axis.title=element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'left',
    legend.title=element_text(family = "Times New Roman", face="bold", size=3),
    legend.text=element_text(family = "Times New Roman", face="bold", size=12),
    legend.background = element_rect(fill = "transparent")
  )

print(fig_legend, vp=view2)


dev.off()


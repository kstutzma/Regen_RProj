ss.p1 <- ggpredict(ss.m4, terms = c("avgLD_l", "Treat_Type"))


ggplot(ss.p1, aes(x, predicted, color = group))+
  geom_line(linewidth = 1.25,  show.legend = FALSE) +
  scale_color_manual(values = c("#D8B70A",  "#02401B", "#A2A475", "#81A88D", "#972D15"))+
  theme_classic()+
  theme(panel.background = element_blank()) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 20))+
  labs(x = "Average leaf litter depth (log transformed)", 
       y = NULL)

# PIRI SS plot 2 -------------------
ss.p2 <- ggpredict(ss.m4, terms = c("l.Veg_Total", "Treat_Type"))


ggplot(ss.p2, aes(x, predicted, color = group))+
  geom_line(linewidth = 1.25,  show.legend = FALSE) +
  scale_color_manual(values = c("#D8B70A",  "#02401B", "#A2A475", "#81A88D", "#972D15"))+
  theme_classic()+
  theme(panel.background = element_blank()) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 20))+
  labs(x = "Average understory vegetation cover \n (log transformed)",
       y = "Pitch pine stems \n (corrected for time from treatment)")


# PIRI SS plot 3 -------------------
ss.p3 <- ggpredict(ss.m4, terms = c("l.SO", "Treat_Type"))


ggplot(ss.p3, aes(x, predicted, color = group))+
  geom_line(linewidth = 1.25, show.legend = FALSE) +
  scale_color_manual(values = c("#D8B70A",  "#02401B", "#A2A475", "#81A88D", "#972D15"))+
  theme_classic()+
  theme(panel.background = element_blank()) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 20),
        legend.position = "right", legend.text = element_text(size = 18), legend.title = element_text(size = 22),)+
  labs(x = "Shrub oak seedling counts (log transformed)",
       y = NULL,
       color = "Treatment Type")



# PIRI LS -------------------
ls.p1 <- ggpredict(ls.m19, terms = c("avgLD_l", "Treat_Type"))


ggplot(ls.p1, aes(x, predicted, color = group))+
  geom_line(linewidth = 1.25) +
  scale_color_manual(values = c("#D8B70A",  "#02401B", "#A2A475", "#81A88D", "#972D15"))+
  theme_classic()+
  theme(panel.background = element_blank()) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 20),
        legend.position = "right", legend.text = element_text(size = 14), legend.title = element_text(size = 18),
        plot.title=element_text(hjust=0.5, size=26))+
  labs(x = "Average leaf litter depth (log transformed)",
       y = "Pitch pine stems \n (corrected for time from treatment)",
       color = "Treatment Type")




# piri sapling model 1 -------------------

sa.piri1 <- ggpredict(sa.m6b, terms = c("avgLD_l", "Treat_Type"))
#control and fallrx are almost completely identical

ggplot(sa.piri1, aes(x, predicted, color = group))+
  geom_line(linewidth = 1.5, show.legend = FALSE) +
  scale_color_manual(values = c("#D8B70A",  "#02401B", "#A2A475", "#81A88D", "#972D15"))+
  theme_classic()+
  theme(panel.background = element_blank()) +
  labs(colour = "Treatment Type")+
  theme(legend.position = "right", legend.text = element_text(size = 14), legend.title = element_text(size = 18), 
        axis.text = element_text(size = 14), axis.title = element_text(size = 20))+
  labs(x = "Average leaf litter depth (log transformed)",
       y = "Pitch pine stems \n (corrected for time from treatment)")



sa.piri4 <- ggpredict(sa.m6b, terms = c("l.BA_HA", "Treat_Type"))
#again control and fall rx overlap


ggplot(sa.piri4, aes(x, predicted, color = group))+
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c("#D8B70A",  "#02401B", "#A2A475", "#81A88D", "#972D15"))+
  theme_classic()+
  theme(panel.background = element_blank()) +
  labs(colour = "Treatment Type")+
  theme(legend.position = "right", legend.text = element_text(size = 14), legend.title = element_text(size = 18), 
        axis.text = element_text(size = 14), axis.title = element_text(size = 20))+
  labs(x = "Average basal area per hectare \n (log transformed)",
       y = "Pitch pine stems \n (corrected for time from treatment)")



apb.nmds.plot +
  geom_segment(data = sig.apb_spp.scrs, aes(x=0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd = 0.4)+
  geom_path(data = ellipse.apb, aes(x=x, y=y, color=Group),  linejoin = "round", lwd = 1.25, show.legend = FALSE) +
  ggrepel::geom_text_repel(data = sig.apb_spp.scrs, aes(x=NMDS1, y=NMDS2, label = common), cex = 4, direction = "both", segment.size = 0.25)+
  geom_segment(data = apb.sig_envr.scrs, aes(x = 0, xend = NMDS1, y = 0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), lwd = 1)+
  ggrepel::geom_text_repel(data = apb.sig_envr.scrs, aes(x=NMDS1, y=NMDS2, label = common), cex = 5, direction = "both", segment.size = 0.25, force = 1) 





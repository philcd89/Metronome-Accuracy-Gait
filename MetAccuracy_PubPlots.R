library(patchwork)
library(ggsignif)
library(tidyverse)

load("C:/Users/philc/Google Drive/MDL/Projects/Metronome_Accuracy/MetAccuracy_Output_7-3-20.RData")

StepInt_pubplot = StepInterval_Raw_agg_plot+
  geom_signif(comparison = list(c("75", "125")), annotations = "*", y_position = 0.018, size = 1.2, textsize = 8, vjust = 0.5, tip_length = 0.035)+
  ylim(c(NA, 0.02))+
  scale_x_discrete(labels = c("75" = "75 bpm", "100" = "100 bpm", "125" = "125 bpm"))+
  ylab("ISI Deviation (s)")+
  xlab("Condition")+
  theme(legend.position = "none")+
  ggtitle("Inter-step Interval Deviation")

Cadence_pubplot = CadenceDev_agg_plot+
  geom_signif(comparison = list(c("100", "125")), annotations = "***", y_position = 4.3, size = 1.2, textsize = 8, vjust = 0.5, tip_length = 0.035)+
  geom_signif(comparison = list(c("75", "125")), annotations = "***", y_position = 4.7, size = 1.2, textsize = 8, vjust = 0.5, tip_length = 0.035)+
  scale_x_discrete(labels = c("75" = "75 bpm", "100" = "100 bpm", "125" = "125 bpm"))+
  ylim(c(NA, 5))+
  xlab("Condition")+
  ylab("Cadence Deviation (steps/min)")+
  theme(legend.title = element_blank())+
  ggtitle("Cadence Deviation")

StepInt_pubplot + Cadence_pubplot + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size = 20))


ISI_BLcomp = ggplot(data = AccDat_BL_ttestdf, aes(x = cond, y = IntStepInterval_mean))+
  geom_bar(stat = "summary")+
  geom_errorbar(aes(ymin = IntStepInterval_mean - IntStepInterval_std.error, ymax = IntStepInterval_mean - IntStepInterval_std.error), stat = "summary", width = 0.4, size = 1)+
  geom_signif(comparisons = list(c("b1", "mpb75")), annotations = "*", y_position = 0.65, size  = 1.2, textsize = 8, vjust = 0.5, tip_length = 0.075)+
  geom_signif(comparisons = list(c("b1", "mpb12")), annotations = "*", y_position = 0.7, size  = 1.2, textsize = 8, vjust = 0.5, tip_length = 0.075)+
  coord_cartesian(ylim = c(0, 0.75))+
  scale_x_discrete(labels=c("b1" = "Initial\nBaseline", "mpb75" = "75 bpm\nInt. BL", "mpb10" = "100 bpm\nInt. BL", "mpb12" = "125 bpm\nInt. BL"))+
  xlab("Condition")+
  ylab("ISI (s)")+
  theme_bw()+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14),
        panel.border = element_rect(color = 'black', fill = NA, size = 2),
        panel.grid = element_line(color = "gray"),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))+
  ggtitle('Initial Baseline v. Intermediate Baselines:\nInter-step Interval')

Cadence_BLcomp = ggplot(data = AccDat_BL_ttestdf, aes(x = cond, y = cadence_mean))+
  geom_bar(stat = "summary")+
  geom_errorbar(aes(ymin = cadence_mean - cadence_std.error, ymax = cadence_mean - cadence_std.error), stat = "summary", width = 0.4, size = 1)+
  geom_signif(comparisons = list(c("b1", "mpb75")), annotations = "***", y_position = 120, size  = 1.2, textsize = 8, vjust = 0.5, tip_length = 0.08)+
  geom_signif(comparisons = list(c("b1", "mpb10")), annotations = "+", y_position = 130, size  = 1.2, textsize = 5, vjust = 0.0, tip_length = 0.08)+
  geom_signif(comparisons = list(c("b1", "mpb12")), annotations = "*", y_position = 140, size  = 1.2, textsize = 8, vjust = 0.5, tip_length = 0.08)+
  coord_cartesian(ylim = c(0, 150))+
  scale_x_discrete(labels=c("b1" = "Initial\nBaseline", "mpb75" = "75 bpm\nInt. BL", "mpb10" = "100 bpm\nInt. BL", "mpb12" = "125 bpm\nInt. BL"))+
  xlab("Conditon")+
  ylab("Cadence (steps/min)")+
  theme_bw()+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14),
        panel.border = element_rect(color = 'black', fill = NA, size = 2),
        panel.grid = element_line(color = "gray"),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))+
  ggtitle('Initial Baseline v. Intermediate Baselines:\nCadence')

ISI_BLcomp + Cadence_BLcomp + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size = 20))

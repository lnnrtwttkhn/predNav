library(lme4)
library(lmerTest)
library(sjPlot)
library(patchwork)
library(tidyverse)
library(ggplot2)
library(readxl)
library(plotrix)
library(effects)
library(broom)
library(rstatix)
theme_set(theme_light(base_size = 40))
theme_update(text = element_text(size=20),
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank(),
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank())

vis_sim_flat<-read_csv("~/Desktop/serverbroken_2/vis_sim_flat.csv")
vis_sim_noCue<-read_csv("~/Desktop/serverbroken_2/vis_sim_nocue.csv")

vis_sim_flat<-read_csv("~/Desktop/serverbroken_2/vis_sim_flat_4.csv")
r2<-read_csv("~/Desktop/serverbroken_2/vis_sim_flat_r2.csv")
r2<-r2 %>% 
  dplyr::filter(X1 < 95)
vis_sim_flat$r2<-r2$r2
vis_sim_noCue<-read_csv("/Volumes/sophon.psych.columbia.edu/data/prednav/data/preprocessed/vis_sim_nocue_4.csv")

r2<-read_csv("/Volumes/sophon.psych.columbia.edu/data/prednav/data/preprocessed/vis_sim_flat  ")

vis_sim_flat<-rbind(vis_sim_flat,vis_sim_flat_2)
vis_sim_noCue<-rbind(vis_sim_noCue,vis_sim_noCue_2)

vis_sim_flat$Noise<-rep(seq(0.1,1.9,0.1), each =5)
vis_sim_flatavg<-vis_sim_flat %>% 
  dplyr::mutate(sigma = (sigmaf+sigmab)/2) %>% 
  dplyr::select(-c(X1, asymptote,sigmaf,sigmab)) %>%
  dplyr::group_by(Noise) %>% 
  dplyr::summarise(avgAmpl = mean(amplitude),
                   semAmpl = std.error(amplitude),
                   avgSigma = mean(sigma),
                   semSigma = std.error(sigma),
                   avgr2 = mean(r2),
                   semr2 = std.error(r2))
vis_sim_flatavg
amp<-ggplot(data = vis_sim_flatavg, aes(x = Noise, y = avgAmpl))+
  geom_line(size = 0.8)+
  geom_ribbon(aes(x = Noise,ymax = avgAmpl+semAmpl, ymin = avgAmpl-semAmpl),alpha = 0.5) +
  #theme_classic()+
  ylab('Amplitude')
amp

vis_sim_flat$sigmaAvg<-(vis_sim_flat$sigmaf+vis_sim_flat$sigmab)/2

sig<-ggplot(data = vis_sim_flatavg, aes(x = Noise, y = avgSigma))+
  geom_ribbon(aes(x = Noise,ymax = avgSigma+semSigma, ymin = avgSigma-semSigma),fill = "grey",alpha = 0.5) +
  geom_line(size = 0.8, color = "black")+
  ylim(0.5,1)+
  #theme_classic()+
  ylab('Width')
sig

r2<-ggplot(data = vis_sim_flatavg, aes(x = Noise, y = avgr2))+
  geom_ribbon(aes(x = Noise,ymax = avgr2+semr2, ymin = avgr2-semr2),fill = "grey",alpha = 0.5) +
  geom_line(size = 0.8, color = "black")+
  ##ylim(0.5,1)+
  #theme_classic()+
  ylab(bquote(R^2))
r2

amp+r2+sig
ggsave("~/Desktop/visSimAmp.png", w = 10, h = 4) ##was 8.5 widtht

ggplot(data = vis_sim_flat, aes(x = amplitude, y = sigmaAvg))+
  geom_line(size = 1)

ggsave("~/Desktop/visSim2.png")

noise_overall<-vis_sim_flat %>% 
  dplyr::select(-c(X1, sigmaAvg)) %>% 
  dplyr::group_by(Noise) %>% 
  dplyr::summarise(amplitude = mean(amplitude),
                   asymptote = mean(asymptote),
                   sigma_f = mean(sigmaf),
                   sigma_b = mean(sigmab))

Fun_0.5 <- function(x) {
  0.07369695* ((x>=0)*exp(- (x**2)/(2*0.6283209)) + 
                           (x<0)*exp(- (x**2)/(2* 0.7253668)))+ -0.011747021
}


Fun_1.0 <- function(x) {
  0.04877424 * ((x>=0)*exp(- (x**2)/(2*0.5928366)) + 
                           (x<0)*exp(- (x**2)/(2* 0.7233243)))+ -0.007513103
}

Fun_1.5 <- function(x) {
  0.03043649* ((x>=0)*exp(- (x**2)/(2*0.7115448)) + 
                           (x<0)*exp(- (x**2)/(2* 0.6588260)))+ -0.004675489

}

ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = `Fun_1.5`, col = "#FADA5E", size = 1.5)+
  stat_function(fun = `Fun_1.0`, col = "#F28C28", size = 1.5)+
  stat_function(fun = `Fun_0.5`, col = "red", size = 1.5)+
  xlab("Steps into the Past and Future")+
  ylab("Pattern similarity to\n environment templates (r)")+
  geom_hline(yintercept =0, linetype = "dashed")
  #ylim(-0.012,0.05)+
  ##theme_classic(base_size = 24)
ggsave("~/Desktop/antPPA.png", w = 6, h = 4.5)

vis_sim_$sigmaAvg<-(vis_sim_noCue$sigmaf+vis_sim_noCue$sigmab)/2

ggplot(data = vis_sim_noCue, aes(x = amplitude, y = sigmaAvg))+
  geom_line(size = 1)

  

library(ggplot2)

#big graphs for paper
postFun <- function(x) {
  0.057682072996515815 * ((x>=0)*exp(- (x**2)/(2*1.80460081)) + 
                            (x<0)*exp(- (x**2)/(2*1.80460081)))+ -0.011548361810923408
  
}

midFun <- function(x) {
  0.036333415744022615 * ((x>=0)*exp(- (x**2)/(2*2.264193215404046)) + 
                            (x<0)*exp(- (x**2)/(2*2.264193215404046)))+ -0.007653557071071609
  
}

antFun <- function(x) {
  0.009673950577353476 * ((x>=0)*exp(- (x**2)/(2*2.8056989071395684)) + 
                            (x<0)*exp(- (x**2)/(2*2.8056989071395684)))+ -0.0011190012674691231
  
}

ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = antFun, col = "#FADA5E", size = 1.5)+
  stat_function(fun = midFun, col = "#F28C28", size = 1.5)+
  stat_function(fun = postFun, col = "red", size = 1.5)+
  xlab("Steps from the Cue")+
  ylab("Pattern \nSimilarity (r)")+
  geom_hline(yintercept =0, linetype = "dashed")+
  ylim(-0.012,0.05)+
  theme_classic(base_size = 20)
ggsave("~/Desktop/antPPA.png", w = 5, h = 3.5)

df<-data.frame(Amplitude =c(0.057682072996515815,0.036333415744022615,0.009673950577353476),
               Sigma = c(1.80460081,2.264193215404046,2.8056989071395684),
               Subregion = c("posterior "," middle "," anterior"))
df$Subregion<-factor(df$Subregion, levels = c("posterior "," middle "," anterior"))
sigma<-ggplot(df,aes(x = Subregion, y = Sigma, color = Subregion))+guides(color = FALSE)+ 
  geom_point(size = 5)+
  scale_color_manual(values = c("red","#F28C28","#FADA5E")) +
  ylim(1.8,2.85)+
  theme_classic(base_size = 18)
amplitude<-ggplot(df,aes(x = Subregion, y = Amplitude, color = Subregion))+
  geom_point(size = 5)+
  scale_color_manual(values = c("red","#F28C28","#FADA5E")) +guides(color = FALSE)+ 
  theme_classic(base_size = 18)+
  ylim(0,0.06)
sigma+amplitude
ggsave("~/Desktop/slGraphsPPA.png", w =6.5 , h = 3)

##rsc
postFun <- function(x) {
  0.03635884437670883 * ((x>=0)*exp(- (x**2)/(2*2.2206469955997052)) + 
                            (x<0)*exp(- (x**2)/(2*2.2206469955997052)))+ -0.0113453085972165
  
}

midFun <- function(x) {
  0.030284686703386812 * ((x>=0)*exp(- (x**2)/(2*2.4040462787774577)) + 
                            (x<0)*exp(- (x**2)/(2*2.4040462787774577)))+ -0.00904222285034656
  
}

antFun <- function(x) {
  0.02237501367711089 * ((x>=0)*exp(- (x**2)/(2*2.736648177953855)) + 
                            (x<0)*exp(- (x**2)/(2*2.736648177953855)))+ -0.0072704511899037445
  
}

ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = antFun, col = "#FADA5E", size = 1.5)+
  stat_function(fun = midFun, col = "#F28C28", size = 1.5)+
  stat_function(fun = postFun, col = "red", size = 1.5)+
  xlab("Steps from the Cue")+
  ylab("Pattern \nSimilarity (r)")+
  geom_hline(yintercept =0, linetype = "dashed")+
  ylim(-0.012,0.035)+
  theme_classic(base_size = 20)
ggsave("~/Desktop/postRSC.png", h = 3.5, w = 5)

df<-data.frame(Amplitude =c(0.03635884437670883,0.030284686703386812,0.02237501367711089),
               Sigma = c(2.2206469955997052,2.4040462787774577,2.736648177953855),
               Subregion = c("posterior "," middle "," anterior"))
df$Subregion<-factor(df$Subregion, levels = c("posterior "," middle "," anterior"))
sigma<-ggplot(df,aes(x = Subregion, y = Sigma, color = Subregion))+guides(color = FALSE)+ 
  geom_point(size = 5)+
  scale_color_manual(values = c("red","#F28C28","#FADA5E")) +
  ylim(1.8,2.8)+
  theme_classic(base_size = 18)
amplitude<-ggplot(df,aes(x = Subregion, y = Amplitude, color = Subregion))+
  geom_point(size = 5)+
  ylim(0,0.06)+
  scale_color_manual(values = c("red","#F28C28","#FADA5E")) +guides(color = FALSE)+ 
  theme_classic(base_size = 18)
sigma+amplitude
ggsave("~/Desktop/slGraphs.png", w =6.5 , h = 3)


##sub specific graphs for SUPPLEMENT
#first do ppa
ppa<-read_csv("~/Desktop/serverbroken_2/ppa_df_sl_graph.csv")


#posterior PPA
postFun <- function(x) {
  0.057682072996515815 * ((x>=0)*exp(- (x**2)/(2*1.80460081)) + 
                            (x<0)*exp(- (x**2)/(2*1.80460081)))+ -0.011548361810923408
  
}

GaussDat <- data.frame(x = c(-4,-3,-2,-1, 1,2,3,4), y = NA)
GaussDat <- data.frame(x = seq(from = -4,to = 4, by = 0.05), y = NA)
GaussDat$y<-postFun(GaussDat$x)

Gauss_plot<-ppa %>% 
  dplyr::select(c(AmpPost, SigmaPost, AsymptotePost))
gaussP<-function(x, AmpPost, SigmaPost, AsymptotePost) {
  AmpPost * ((x>=0)*exp(- (x**2)/(2*SigmaPost)) + 
                 (x<0)*exp(- (x**2)/(2* SigmaPost)))+ -AsymptotePost
}
x <- seq(from = -4, to = 4, by = 0.05)

#points<-data.frame(y = c(  -0.00473887,  0.00298936, -0.00022473,  0.00393559,  0.00722259,  0.00296359,
#3                           -0.0040871,  -0.00711217, -0.00473887), x = seq(-4,4), 
##                   sem = c(0.004847386,0.004674957,0.004672453, 0.005307833,0.005574596,0.005164583,0.003982394,0.005237885,0.004847386))
my_func <- plyr::mdply(Gauss_plot, function(AmpPost, SigmaPost, AsymptotePost){
  data.frame(x=x, y=gaussP(x,AmpPost, SigmaPost, AsymptotePost))}) %>% 
  bind_rows
my_func$sub<-rep(1:32, each=161)

#library('RColorBrewer')
#myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
postPPA <-ggplot(data = my_func, aes(x=x, y=y, group = sub)) + 
  geom_line(color = "grey") +
  #geom_point(data = points, mapping = aes(y = y, x = x, group = 1),colour = "black", fill = "white", size = 2, stroke = 1, shape = 21)+ #ba8a90
  #geom_errorbar(data = points, mapping = aes(x = x, ymin=y-sem, ymax=y+sem,group = 1), color = "black",width=0.1, size = 1)+
  #stat_function(fun = hippFun,size = 2, aes(color=y))+ #ba8a90
  geom_line(data = GaussDat, mapping = aes(x = x, y = y, group = 1),color = "red", size = 2)+
  xlab("Steps from the Cue")+ylab("Pattern Similarity (r)")+
  ylim(-0.1, 0.13)+
  #scale_color_continuous(type = "viridis", guide = 'none')+
  #scale_color_gradientn(colours = myPalette(100), limits=c(min(hippGaussDat$y),max(hippGaussDat$y)),na.value = "black", guide = "none")+
  theme_classic(base_size = 20)
postPPA

#mid PPA
midFun <- function(x) {
  0.036333415744022615 * ((x>=0)*exp(- (x**2)/(2*2.264193215404046)) + 
                            (x<0)*exp(- (x**2)/(2*2.264193215404046)))+ -0.007653557071071609
  
}

GaussDat <- data.frame(x = c(-4,-3,-2,-1, 1,2,3,4), y = NA)
GaussDat <- data.frame(x = seq(from = -4,to = 4, by = 0.05), y = NA)
GaussDat$y<-midFun(GaussDat$x)

Gauss_plot<-ppa %>% 
  dplyr::select(c(AmpMid, SigmaMid, AsymptoteMid))
gaussP<-function(x, AmpMid, SigmaMid, AsymptoteMid) {
  AmpMid * ((x>=0)*exp(- (x**2)/(2*SigmaMid)) + 
               (x<0)*exp(- (x**2)/(2* SigmaMid)))+ -AsymptoteMid
}
x <- seq(from = -4, to = 4, by = 0.05)

my_func <- plyr::mdply(Gauss_plot, function(AmpMid, SigmaMid, AsymptoteMid){
  data.frame(x=x, y=gaussP(x,AmpMid, SigmaMid, AsymptoteMid))}) %>% 
  bind_rows
my_func$sub<-rep(1:32, each=161)

midPPA <-ggplot(data = my_func, aes(x=x, y=y, group = sub)) + 
  geom_line(color = "grey") +
  #geom_point(data = points, mapping = aes(y = y, x = x, group = 1),colour = "black", fill = "white", size = 2, stroke = 1, shape = 21)+ #ba8a90
  #geom_errorbar(data = points, mapping = aes(x = x, ymin=y-sem, ymax=y+sem,group = 1), color = "black",width=0.1, size = 1)+
  #stat_function(fun = hippFun,size = 2, aes(color=y))+ #ba8a90
  geom_line(data = GaussDat, mapping = aes(x = x, y = y, group = 1),color = "#F28C28", size = 2)+
  xlab("Steps from the Cue")+ylab("Pattern Similarity (r)")+
  ylim(-0.1, 0.13)+
  #scale_color_continuous(type = "viridis", guide = 'none')+
  #scale_color_gradientn(colours = myPalette(100), limits=c(min(hippGaussDat$y),max(hippGaussDat$y)),na.value = "black", guide = "none")+
  theme_classic(base_size = 20)
midPPA

#anterior PPA
antFun <- function(x) {
  0.009673950577353476 * ((x>=0)*exp(- (x**2)/(2*2.8056989071395684)) + 
                            (x<0)*exp(- (x**2)/(2*2.8056989071395684)))+ -0.0011190012674691231
  
}

GaussDat <- data.frame(x = c(-4,-3,-2,-1, 1,2,3,4), y = NA)
GaussDat <- data.frame(x = seq(from = -4,to = 4, by = 0.05), y = NA)
GaussDat$y<-antFun(GaussDat$x)

Gauss_plot<-ppa %>% 
  dplyr::select(c(AmpAnt, SigmaAnt, AsymptoteAnt))
gaussP<-function(x, AmpAnt, SigmaAnt, AsymptoteAnt) {
  AmpAnt * ((x>=0)*exp(- (x**2)/(2*SigmaAnt)) + 
              (x<0)*exp(- (x**2)/(2* SigmaAnt)))+ -AsymptoteAnt
}
x <- seq(from = -4, to = 4, by = 0.05)

my_func <- plyr::mdply(Gauss_plot, function(AmpAnt, SigmaAnt, AsymptoteAnt){
  data.frame(x=x, y=gaussP(x,AmpAnt, SigmaAnt, AsymptoteAnt))}) %>% 
  bind_rows
my_func$sub<-rep(1:32, each=161)

antPPA <-ggplot(data = my_func, aes(x=x, y=y, group = sub)) + 
  geom_line(color = "grey") +
  geom_line(data = GaussDat, mapping = aes(x = x, y = y, group = 1),color = "#FADA5E", size = 2)+
  xlab("Steps from the Cue")+ylab("Pattern Similarity (r)")+
  ylim(-0.11, 0.13)+
  theme_classic(base_size = 20)
antPPA

postPPA+midPPA+antPPA
ggsave("~/Desktop/PPAsupplement.png", w = 11, h = 4) ##was 8.5 width

##rsc sub specific graphs
ppa<-read_csv("~/Desktop/serverbroken_2/rsc_df_sl_graph.csv")


#posterior PPA
postFun <- function(x) {
  0.03635884437670883 * ((x>=0)*exp(- (x**2)/(2*2.2206469955997052)) + 
                           (x<0)*exp(- (x**2)/(2*2.2206469955997052)))+ -0.0113453085972165
  
}

GaussDat <- data.frame(x = c(-4,-3,-2,-1, 1,2,3,4), y = NA)
GaussDat <- data.frame(x = seq(from = -4,to = 4, by = 0.05), y = NA)
GaussDat$y<-postFun(GaussDat$x)

Gauss_plot<-ppa %>% 
  dplyr::select(c(AmpPost, SigmaPost, AsymptotePost))
gaussP<-function(x, AmpPost, SigmaPost, AsymptotePost) {
  AmpPost * ((x>=0)*exp(- (x**2)/(2*SigmaPost)) + 
               (x<0)*exp(- (x**2)/(2* SigmaPost)))+ -AsymptotePost
}
x <- seq(from = -4, to = 4, by = 0.05)

#points<-data.frame(y = c(  -0.00473887,  0.00298936, -0.00022473,  0.00393559,  0.00722259,  0.00296359,
#3                           -0.0040871,  -0.00711217, -0.00473887), x = seq(-4,4), 
##                   sem = c(0.004847386,0.004674957,0.004672453, 0.005307833,0.005574596,0.005164583,0.003982394,0.005237885,0.004847386))
my_func <- plyr::mdply(Gauss_plot, function(AmpPost, SigmaPost, AsymptotePost){
  data.frame(x=x, y=gaussP(x,AmpPost, SigmaPost, AsymptotePost))}) %>% 
  bind_rows
my_func$sub<-rep(1:32, each=161)

#library('RColorBrewer')
#myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
postPPA <-ggplot(data = my_func, aes(x=x, y=y, group = sub)) + 
  geom_line(color = "grey") +
  #geom_point(data = points, mapping = aes(y = y, x = x, group = 1),colour = "black", fill = "white", size = 2, stroke = 1, shape = 21)+ #ba8a90
  #geom_errorbar(data = points, mapping = aes(x = x, ymin=y-sem, ymax=y+sem,group = 1), color = "black",width=0.1, size = 1)+
  #stat_function(fun = hippFun,size = 2, aes(color=y))+ #ba8a90
  geom_line(data = GaussDat, mapping = aes(x = x, y = y, group = 1),color = "red", size = 2)+
  xlab("Steps from the Cue")+ylab("Pattern Similarity (r)")+
  ylim(-0.1, 0.1538)+
  #scale_color_continuous(type = "viridis", guide = 'none')+
  #scale_color_gradientn(colours = myPalette(100), limits=c(min(hippGaussDat$y),max(hippGaussDat$y)),na.value = "black", guide = "none")+
  theme_classic(base_size = 20)
postPPA
#ggsave("~/Desktop/hippGaussplot_flat_color.png", h = 4, w = 5)

#mid PPA
midFun <- function(x) {
  0.030284686703386812 * ((x>=0)*exp(- (x**2)/(2*2.4040462787774577)) + 
                            (x<0)*exp(- (x**2)/(2*2.4040462787774577)))+ -0.00904222285034656
  
}

GaussDat <- data.frame(x = c(-4,-3,-2,-1, 1,2,3,4), y = NA)
GaussDat <- data.frame(x = seq(from = -4,to = 4, by = 0.05), y = NA)
GaussDat$y<-midFun(GaussDat$x)

Gauss_plot<-ppa %>% 
  dplyr::select(c(AmpMid, SigmaMid, AsymptoteMid))
gaussP<-function(x, AmpMid, SigmaMid, AsymptoteMid) {
  AmpMid * ((x>=0)*exp(- (x**2)/(2*SigmaMid)) + 
              (x<0)*exp(- (x**2)/(2* SigmaMid)))+ -AsymptoteMid
}
x <- seq(from = -4, to = 4, by = 0.05)

my_func <- plyr::mdply(Gauss_plot, function(AmpMid, SigmaMid, AsymptoteMid){
  data.frame(x=x, y=gaussP(x,AmpMid, SigmaMid, AsymptoteMid))}) %>% 
  bind_rows
my_func$sub<-rep(1:32, each=161)

midPPA <-ggplot(data = my_func, aes(x=x, y=y, group = sub)) + 
  geom_line(color = "grey") +
  #geom_point(data = points, mapping = aes(y = y, x = x, group = 1),colour = "black", fill = "white", size = 2, stroke = 1, shape = 21)+ #ba8a90
  #geom_errorbar(data = points, mapping = aes(x = x, ymin=y-sem, ymax=y+sem,group = 1), color = "black",width=0.1, size = 1)+
  #stat_function(fun = hippFun,size = 2, aes(color=y))+ #ba8a90
  geom_line(data = GaussDat, mapping = aes(x = x, y = y, group = 1),color = "#F28C28", size = 2)+
  xlab("Steps from the Cue")+ylab("Pattern Similarity (r)")+
  ylim(-0.125, 0.153)+
  #scale_color_continuous(type = "viridis", guide = 'none')+
  #scale_color_gradientn(colours = myPalette(100), limits=c(min(hippGaussDat$y),max(hippGaussDat$y)),na.value = "black", guide = "none")+
  theme_classic(base_size = 20)
midPPA

#anterior PPA
antFun <- function(x) {
  0.02237501367711089 * ((x>=0)*exp(- (x**2)/(2*2.736648177953855)) + 
                           (x<0)*exp(- (x**2)/(2*2.736648177953855)))+ -0.0072704511899037445
  
}


GaussDat <- data.frame(x = c(-4,-3,-2,-1, 1,2,3,4), y = NA)
GaussDat <- data.frame(x = seq(from = -4,to = 4, by = 0.05), y = NA)
GaussDat$y<-antFun(GaussDat$x)

Gauss_plot<-ppa %>% 
  dplyr::select(c(AmpAnt, SigmaAnt, AsymptoteAnt))
gaussP<-function(x, AmpAnt, SigmaAnt, AsymptoteAnt) {
  AmpAnt * ((x>=0)*exp(- (x**2)/(2*SigmaAnt)) + 
              (x<0)*exp(- (x**2)/(2* SigmaAnt)))+ -AsymptoteAnt
}
x <- seq(from = -4, to = 4, by = 0.05)

my_func <- plyr::mdply(Gauss_plot, function(AmpAnt, SigmaAnt, AsymptoteAnt){
  data.frame(x=x, y=gaussP(x,AmpAnt, SigmaAnt, AsymptoteAnt))}) %>% 
  bind_rows
my_func$sub<-rep(1:32, each=161)

antPPA <-ggplot(data = my_func, aes(x=x, y=y, group = sub)) + 
  geom_line(color = "grey") +
  geom_line(data = GaussDat, mapping = aes(x = x, y = y, group = 1),color = "#FADA5E", size = 2)+
  xlab("Steps from the Cue")+ylab("Pattern Similarity (r)")+
  ylim(-0.10, 0.15)+
  theme_classic(base_size = 20)
antPPA

postPPA+midPPA+antPPA
ggsave("~/Desktop/PPAsupplement.png", w = 11, h = 4) ##was 8.5 width


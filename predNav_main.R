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
library(RColorBrewer)

# #behavioural data----------
# baseDir<-"~/Desktop/serverbroken_2/behav_data/"
# setwd(baseDir)
# subs<-list.files()
# 
# task_list<-c("Prediction")
# 
# task_run_list<-c(2)
# 
# subs<-subs[-c(1,14,20,29)]
# 
# for (sub in subs){
#   
#   #change wd to include the sub number
#   setwd(paste0(baseDir,'/',sub))
#   
#   
#   for (task in task_list) {
#     
#     #create an index of task lists to pick the proper run
#     #I don't think I need this because there's only one task list
#     i<-ifelse(task == "Prediction", 1, 2)
#     
#     #is this going though all numbers?
#     for (run in (1:task_run_list)) {
#       
#       print(task_run_list)
#       print(1:task_run_list)
#       print(run)
#       print("######")
#       
#       #the subject num (will likely have to revisit this when sub nums get larger than 10)
#       fname<- paste(str_sub(sub,-1), "_", as.character(run), "_", task, sep = "") 
#       
#       #get the files that match the pattern from fname, and then the first index of that list (that will be the csv)
#       temp_file<-list.files(pattern = fname)
#       temp_file<-temp_file[1]
#       temp_file<-read.csv(paste0(baseDir, sub, "/", temp_file))
#       
#       
#       #gets rid of every other row with nothing in it, and removes unneccessary columns
#       temp_clean<-temp_file[!is.na(temp_file$iti_StartTime),]
#       temp_clean$frameRate<-NULL
#       temp_clean$psychopyVersion<-NULL
#       temp_clean$X56<-NULL
#       
#       temp_clean<-temp_clean %>% 
#         mutate(cue = paste0(str_sub(scene, end = -5), "_", path)) %>% 
#         mutate(acc = Resp.corr) %>%
#         mutate(rt = Resp.rt)
#       
#       #rename columns for rt and acc
#       #colnames(temp_clean)[40:42]<- c("resp","acc","rt")
#       
#       #now get the order of distance into future - this code is taken straight from behav_analysis.r
#       order_files<-list.files(pattern=c("WalkThrough"))
#       order_only<-read.csv(order_files[1],header=TRUE)
#       
#       order_only$scene<-as.character(order_only$scene)
#       order_only<-subset(order_only, (scene != "18.png") & (scene != "19.png"))
#       
#       temp_clean$path<-as.character(temp_clean$path)
#       temp_clean$map<-as.character(temp_clean$map)
#       temp_clean$scene<-as.character(temp_clean$scene)
#       temp_clean$scene_a<-as.character(temp_clean$scene_a)
#       temp_clean$scene_b<-as.character(temp_clean$scene_b)
#       temp_clean$cor_distance<-NA
#       temp_clean$incor_distance<-NA
#       
#       for (row in 1:nrow(temp_clean)){
#         if (temp_clean$scene_a[row] != ""){
#           pth<-temp_clean$path[row]
#           mp<-temp_clean$map[row]
#           p<-temp_clean$participant[row]
#           temp<-subset(order_only, path == pth & map == mp)
#           if (temp_clean$cor_resp[row] == 1){
#             left<-temp_clean$scene_a[row]
#             incorr<-temp_clean$scene_b[row] #added this
#             pred<-temp_clean$scene[row]
#             ind_1<-which(temp$scene == pred)
#             ind_2<-which(temp$scene == left)
#             ind_3<-which(temp$scene == incorr) #added this
#             temp_clean$cor_distance[row]<-ifelse(ind_2>ind_1, abs(ind_2-ind_1), abs(ind_2+8-ind_1))
#             temp_clean$incor_distance[row]<-ifelse(ind_3>ind_1, abs(ind_3-ind_1), abs(ind_3+8-ind_1))
#           }
#           else if (temp_clean$cor_resp[row] == 2){
#             right<-temp_clean$scene_b[row]
#             incorr<-temp_clean$scene_a[row] #added this
#             pred<-temp_clean$scene[row]
#             ind_1<-which(temp$scene == pred)
#             ind_2<-which(temp$scene == right)
#             ind_3<-which(temp$scene == incorr) #added this
#             temp_clean$cor_distance[row]<-ifelse(ind_2>ind_1, abs(ind_2-ind_1), abs(ind_2+8-ind_1))
#             temp_clean$incor_distance[row]<-ifelse(ind_3>ind_1, abs(ind_3-ind_1), abs(ind_3+8-ind_1))
#           }
#         }
#       }
#       
#       write_out_file<-temp_clean %>% 
#         dplyr::select(cue, scene, session, participant, acc, rt, path, map, cor_distance, incor_distance) %>% 
#         dplyr::filter(!is.na(cor_distance))
#       print('#########')
#       print(run)
#       print(unique(write_out_file$participant))
#       print(mean(write_out_file$acc))
#       print(table(write_out_file$cor_distance))
#       
#       col_order<-c("1_green", "2_green", "3_green", "4_green", "5_green", "6_green", 
#                    "7_green", "8_green", "9_green", "10_green", "11_green", "12_green", 
#                    "13_green", "14_green", "15_green", "16_green",
#                    "1_blue", "2_blue", "3_blue", "4_blue", "5_blue", "6_blue", "7_blue", 
#                    "8_blue", "9_blue", "10_blue", "11_blue", "12_blue", 
#                    "13_blue", "14_blue", "15_blue", "16_blue")
#       write_out_file<- write_out_file %>%
#         slice(match(col_order, cue))
#       
#       outname<-paste0(sub,"_task-",task,"_run-0",run,"_pred_behav_regressors.txt")
#       
#       if ((sub == 'sub-01') & (run == 1)) {
#         behav_data<-write_out_file
#       }
#       else {
#         behav_data<-rbind(behav_data, write_out_file)
#       }
#       
#       #Uncomment this to write the regressors by room number
#       #write.table(write_out_file, outname, col.names= colnames(write_out_file), 
#       #            row.names= FALSE, sep = "\t"  )
#     }
#   }
# }

figure2_data <- read.csv("data/figure2.csv")
predAcc<-figure2_data %>% 
  #dplyr::group_by(participant) %>% 
  dplyr::summarise(avg = mean(avgAcc, na.rm = T)) 
predAcc

predAccDist<-figure2_data  %>% 
  dplyr::group_by(participant, cor_distance) %>% 
  dplyr::summarise(avg = mean(avgAcc, na.rm = T)) %>% 
  dplyr::filter(!is.na(cor_distance))
predAccDist

predAccDistOverall<-figure2_data %>% 
  dplyr::group_by(cor_distance) %>% 
  dplyr::summarise(avg = mean(avgAcc, na.rm = T)) %>% 
  dplyr::filter(!is.na(cor_distance))
predAccDistOverall

# calculate average difference in accuracy between one-step and four-step trials.
predAccDistOverall$avg[predAccDistOverall$cor_distance == 1] - predAccDistOverall$avg[predAccDistOverall$cor_distance == 4]

#accuracy distance model
summary(glmer(avgAcc~cor_distance+(1+cor_distance|participant), family = "binomial", data = figure2_data))
#rt distance model
summary(lmer(avgRT~cor_distance+(1+cor_distance|participant),  data = figure2_data))

#accuracy graph
pd <- position_jitter(w=0.2, h=0)
predAcc_plot <- ggplot() +
  #geom_bar(data=distance_mean, aes(y=mean_acc,x=cor_distance),fill  = "white", color = "black", stat = "identity", width = 0.5) + 
  geom_line(data = predAccDist, aes(y=avg, x=cor_distance, group=factor(participant)),position = pd, size =  1, alpha = 0.8, color = "darkgrey") +
  geom_point(data=predAccDistOverall, aes(y=avg, x=cor_distance, group= 1), size = 5, colour = "black") +
  geom_line(data = predAccDistOverall, aes(y = avg, x = cor_distance, group = 1), size = 3, color = "black") +
  geom_hline(yintercept=0.5, linetype="dashed", color = "black", size = 2)+
  #geom_ribbon(data = distance_mean, aes(x = cor_distance, ymin = (mean_acc - (value)), 
  #                                      ymax = (mean_acc + (value))), alpha = 0.4)+
  ylim(c(0,1.0)) + 
  scale_x_continuous()+
  theme_classic(base_size = 30)+
  theme(legend.position = "none") +
  labs(y= "Accuracy", x = "Steps into the Future")
predAcc_plot

#cor dist RT graph
predRTDist<-figure2_data %>% 
  dplyr::group_by(participant, cor_distance) %>%
  dplyr::filter(avgAcc == 1) %>% 
  dplyr::summarise(avg = mean(avgRT, na.rm = T)) %>% 
  dplyr::filter(!is.na(cor_distance))
predRTDist

predRTDistOverall<-figure2_data %>% 
  dplyr::group_by(cor_distance) %>%
  dplyr::filter(avgAcc == 1) %>% 
  dplyr::summarise(avg = mean(avgRT, na.rm = T)) %>% 
  dplyr::filter(!is.na(cor_distance))
predRTDistOverall

pd <- position_jitter(w=0.2, h=0)
predRT_plot <- ggplot() +
  #geom_bar(data=distance_mean, aes(y=mean_acc,x=cor_distance),fill  = "white", color = "black", stat = "identity", width = 0.5) + 
  geom_line(data = predRTDist, aes(y=avg, x=cor_distance, group=factor(participant)),position = pd, size =  1, alpha = 0.8, color = "darkgrey") +
  geom_point(data=predRTDistOverall, aes(y=avg, x=cor_distance, group= 1), size = 5, colour = "black") +
  geom_line(data = predRTDistOverall, aes(y = avg, x = cor_distance, group = 1), size = 3, color = "black") +
  #geom_hline(yintercept=0.5, linetype="dashed", color = "black", size = 2)+
  #geom_ribbon(data = distance_mean, aes(x = cor_distance, ymin = (mean_acc - (value)), 
  #                                      ymax = (mean_acc + (value))), alpha = 0.4)+
  #ylim(c(0,1.0)) + 
  scale_x_continuous()+
  theme_classic(base_size = 30)+
  theme(legend.position = "none") +
  labs(y= "Response Time (s)", x = "Steps into the Future")
predRT_plot

#gaussians-----------------
#hippocampus--cued path
hipp<-read_csv("~/Desktop/serverbroken_2/hipp_gauss.csv")
hipp$region<-"hippocampus"

hippFun <- function(x) {
  0.017959403645080096 * ((x>=0)*exp(- (x**2)/(2*1.3796765099198625)) + 
                            (x<0)*exp(- (x**2)/(2*3.8054908228634234)))+ -0.006510771135735058
}
hippGaussDat <- data.frame(x = c(-4,-3,-2,-1, 1,2,3,4), y = NA)
hippGaussDat <- data.frame(x = seq(from = -4,to = 4, by = 0.05), y = NA)
hippGaussDat$y<-hippFun(hippGaussDat$x)

hippGauss_plot<-hipp %>% 
  dplyr::select(c(amplitude, sigma_f, sigma_b, asymptote))
gaussP<-function(x, amplitude, sigma_f, sigma_b, asymptote) {
  amplitude * ((x>=0)*exp(- (x**2)/(2*sigma_f)) + 
                 (x<0)*exp(- (x**2)/(2* sigma_b)))+ -asymptote
}
x <- seq(from = -4, to = 4, by = 0.05)

points<-data.frame(y = c(  -0.00473887,  0.00298936, -0.00022473,  0.00393559,  0.00722259,  0.00296359,
                           -0.0040871,  -0.00711217, -0.00473887), x = seq(-4,4), 
                   sem = c(0.004847386,0.004674957,0.004672453, 0.005307833,0.005574596,0.005164583,0.003982394,0.005237885,0.004847386))
my_func <- plyr::mdply(hippGauss_plot, function(amplitude, sigma_f, sigma_b, asymptote){
  data.frame(x=x, y=gaussP(x,amplitude, sigma_f, sigma_b, asymptote))}) %>% 
  bind_rows
my_func$sub<-rep(1:32, each=161)

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
ggplot(data = my_func, aes(x=x, y=y, group = sub)) + 
  geom_line(color = "grey") +
  geom_point(data = points, mapping = aes(y = y, x = x, group = 1),colour = "black", fill = "white", size = 2, stroke = 1, shape = 21)+ #ba8a90
  geom_errorbar(data = points, mapping = aes(x = x, ymin=y-sem, ymax=y+sem,group = 1), color = "black",width=0.1, size = 1)+
  #stat_function(fun = hippFun,size = 2, aes(color=y))+ #ba8a90
  geom_line(data = hippGaussDat, mapping = aes(x = x, y = y, color = y, group = 1), size = 2)+
  xlab("Steps from the Cue")+ylab("Pattern Similarity (r)")+
  #ylim(-0.05, 0.06)+
  #scale_color_continuous(type = "viridis", guide = 'none')+
  scale_color_gradientn(colours = myPalette(100), limits=c(min(hippGaussDat$y),max(hippGaussDat$y)),na.value = "black", guide = "none")+
  theme_classic(base_size = 20)


#hippocampus uncued
#uncued values
hipp_unCued_Fun <- function(x) {
  0.0053599347879061884 * ((x>=0)*exp(- (x**2)/(2*3.045250209235981)) + 
                             (x<0)*exp(- (x**2)/(2*2.638560145803498)))+ -0.0031348211745625557
}
hippGaussDat_uncued <- data.frame(x = c(-4,-3,-2,-1, 1,2,3,4), y = NA)
hippGaussDat_uncued <- data.frame(x = seq(from = -4,to = 4, by = 0.05), y = NA)
hippGaussDat_uncued$y<-hipp_unCued_Fun(hippGaussDat_uncued$x)

hipp_uncued<-read_excel("~/Desktop/serverbroken_2/uncued_asymGaussReg_hipp_final.xlsx")
hipp_uncued_Gauss_plot<-hipp_uncued %>% 
  dplyr::select(c(amplitude_uncued, sigma_f_uncued, sigma_b_uncued, asymptote_uncued))
gaussP<-function(x, amplitude_uncued, sigma_f_uncued, sigma_b_uncued, asymptote_uncued) {
  amplitude_uncued * ((x>=0)*exp(- (x**2)/(2*sigma_f_uncued)) + 
                        (x<0)*exp(- (x**2)/(2* sigma_b_uncued)))+ -asymptote_uncued
}
x <- seq(from = -4,to = 4, by = 0.05)
points<-data.frame(y = c(  -0.00623806,  0.00303763, -0.00555283,  0.00054192,  0.00620201, -0.00772322,
                           -0.00206067,  0.00511685, -0.00623806), x = seq(-4,4),sem = c(0.004847386,0.004674957,0.004672453, 0.005307833,0.005574596,0.005164583,0.003982394,0.005237885,0.004847386))

my_func <- plyr::mdply(hipp_uncued_Gauss_plot, function(amplitude_uncued, sigma_f_uncued, sigma_b_uncued, asymptote_uncued){
  data.frame(x=x, y=gaussP(x,amplitude_uncued, sigma_f_uncued, sigma_b_uncued, asymptote_uncued))}) %>% 
  bind_rows
my_func$sub<-rep(1:32, each=161)
hippGaussDat_uncued$yCued<-hippGaussDat$y

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

ggplot(data = my_func, aes(x=x, y=y, color = y,group = sub)) + 
  geom_line(color = "grey") +
  geom_point(data = points, mapping = aes(y = y, x = x, group = 1),colour = "black", fill = "white", size = 2, stroke = 1, shape = 21)+
  geom_errorbar(data = points, mapping = aes(x = x, ymin=y-sem, ymax=y+sem, group = 1), color = "black",width=0.1, size = 1)+
  geom_line(data = hippGaussDat_uncued, aes(x=x,y=y, group = 1, color = y), size =2)+
  #stat_function(fun = hipp_unCued_Fun,size = 2, color="#ba8a90")+ ###color="#ba8a90"
  xlab("Steps from the Cue")+ylab("Pattern Similarity (r)")+
  #ylim(-0.05, 0.06)+
  #scale_colour_manual(values=colours) +
  theme_classic(base_size = 20)+
  ##scale_color_continuous(type = "viridis", limits=c(0.01, -0.005))+
  scale_color_gradientn(colours = myPalette(100), limits=c(min(hippGaussDat$y),max(hippGaussDat$y)),na.value = "black", guide = "none")

#visual cortex cued
visFun <- function(x) {
  0.09949153086715995 * ((x>=0)*exp(- (x**2)/(2*0.09348457538317367)) + 
                           (x<0)*exp(- (x**2)/(2* 0.09468982294619233)))+ -0.008739923711419201
}
vis<-read_excel("~/Desktop/serverbroken_2/asymGaussReg_vis_cued_graphMarch1.xlsx")

visGaussDat <- data.frame(x = c(-4,-3,-2,-1, 1,2,3,4), y = NA)
visGaussDat <- data.frame(x = seq(from = -4,to = 4, by = 0.05), y = NA)
visGaussDat$y<-visFun(visGaussDat$x)
visGauss_plot<-vis %>% 
  dplyr::select(c(amplitude, sigma_f, sigma_b, asymptote))
gaussP<-function(x, amplitude, sigma_f, sigma_b, asymptote) {
  amplitude * ((x>=0)*exp(- (x**2)/(2*sigma_f)) + 
                 (x<0)*exp(- (x**2)/(2* sigma_b)))+ asymptote
}
x <- seq(-4,4, by = 0.05)
points<-data.frame(y = c(-0.01107491, -0.0109334,  -0.00768439, -0.00486498,  0.09075183, -0.00988303,
                         -0.00231027, -0.01209375, -0.01107491), x = seq(-4,4),
                   sem = c(0.005297683,0.004900626,0.004850905,0.004705684,0.004691917,0.005049748,0.004512379,0.004215895,0.00529768))

my_func <- plyr::mdply(visGauss_plot, function(amplitude, sigma_f, sigma_b, asymptote){
  data.frame(x=x, y=gaussP(x,amplitude, sigma_f, sigma_b, asymptote))}) %>% 
  bind_rows
my_func$sub<-rep(1:32, each=161)
ggplot(data = my_func, aes(x=x, y=y, group = sub)) + 
  geom_line(color = "grey") +
  #stat_function(fun = visFun,size = 2, color="#8288c9")+ #color="#8288c9"
  geom_point(data = points, mapping = aes(y = y, x = x, group = 1),colour = "black", fill = "white", size = 2, stroke = 1, shape = 21)+
  geom_errorbar(data = points, mapping = aes(x = x, ymin=y-sem, ymax=y+sem,group = 1), color = "black",width=0.1, size = 1)+
  geom_line(data = visGaussDat, mapping = aes(y = y, x = x, group = 1, color = y), size = 2)+
  xlab("Steps from the Cue")+ylab("Pattern Similarity (r)")+
  #scale_colour_manual(values=colours) +
  #scale_colour_continuous(type='viridis', guide = "none") +
  scale_color_gradientn(colours = myPalette(100), limits=c(min(visGaussDat$y),max(visGaussDat$y)),na.value = "black", guide = "none")+
  theme_classic(base_size = 20)

#visual cortex uncued
vis_unCued_Fun <- function(x) {
  0.0902738391521609 * ((x>=0)*exp(- (x**2)/(2*0.03317736963766558)) + 
                          (x<0)*exp(- (x**2)/(2*0.03226921828800432)))+ -0.007639198560543784
}
visGaussDat_uncued <- data.frame(x = c(-4,-3,-2,-1, 1,2,3,4), y = NA)
visGaussDat_uncued <- data.frame(x = seq(from = -4,to = 4, by = 0.05), y = NA)
visGaussDat_uncued$y<-vis_unCued_Fun(visGaussDat_uncued$x)

vis_uncued<-read_excel("~/Desktop/serverbroken_2/uncued_asymGaussReg_vis_graphMarch1.xlsx")
vis_uncued_Gauss_plot<-vis_uncued %>% 
  dplyr::select(c(amplitude_uncued, sigma_f_uncued, sigma_b_uncued, asymptote_uncued))
gaussP<-function(x, amplitude_uncued, sigma_f_uncued, sigma_b_uncued, asymptote_uncued) {
  amplitude_uncued * ((x>=0)*exp(- (x**2)/(2*sigma_f_uncued)) + 
                        (x<0)*exp(- (x**2)/(2* sigma_b_uncued)))+ -asymptote_uncued
}
x <- seq(from = -4,to = 4, by = 0.05)
points<-data.frame(y = c(    -0.01107491,-0.00486498, -0.00231027, -0.0109334, 0.09075183, 
                             -0.01209375, -0.00768439, -0.00988303, -0.01107491 ), x = seq(-4,4),
                   sem = c(0.005297683,0.004900626,0.004850905,0.004705684,0.004691917,0.005049748,0.004512379,0.004215895,0.00529768))

my_func <- plyr::mdply(vis_uncued_Gauss_plot, function(amplitude_uncued, sigma_f_uncued, sigma_b_uncued, asymptote_uncued){
  data.frame(x=x, y=gaussP(x,amplitude_uncued, sigma_f_uncued, sigma_b_uncued, asymptote_uncued))}) %>% 
  bind_rows
my_func$sub<-rep(1:32, each=161)
ggplot(data = my_func, aes(x=x, y=y, group = sub)) + 
  geom_line(color = "grey") +
  geom_point(data = points, mapping = aes(y = y, x = x, group = 1),colour = "black", fill = "white", size = 2, stroke = 1, shape = 21)+
  geom_errorbar(data = points, mapping = aes(x = x, ymin=y-sem, ymax=y+sem,group = 1), color = "black",width=0.1, size = 1)+
  geom_line(data = visGaussDat_uncued, aes(x =x,y=y, group = 1, color = y), size = 2)+
  #stat_function(fun = vis_unCued_Fun,size = 2, color="#8288c9")+
  xlab("Steps from the Cue")+ylab("Pattern Similarity (r)")+
  #ylim(-0.05, 0.06)+
  #scale_colour_continuous(type="viridis", guide = 'none') +
  scale_color_gradientn(colours = myPalette(100), limits=c(min(visGaussDat$y)-0.0025,max(visGaussDat$y)),na.value = "black", guide = "none")+
  theme_classic(base_size = 20)

#forward vs backward sigma in hipp vs vis cortex
hipp_prep<-hipp %>%
  dplyr::select(amplitude, sigma_f, sigma_b, asymptote, sub, region)
cue_avgP<-rbind(hipp_prep,vis)
cue_avgP$region_c<-ifelse(cue_avgP$region == "vis", -0.5, 0.5)

forwardBackwardSigma<-cue_avgP %>% 
  dplyr::select(c(region_c, sub, sigma_f, sigma_b)) %>% 
  pivot_longer(cols = c(sigma_f, sigma_b),names_to = "direction", values_to = "sigma") %>% 
  mutate(direction_c = ifelse(direction == "sigma_f", 0.5, -0.5))
forwardBackwardSigma

summary(fb_mod<-lmer(sigma~region_c*direction_c+(1+region_c+direction_c|sub), data = forwardBackwardSigma))
confint(fb_mod,method = "Wald")

#error to shuffled null INCLUDING CUE
hippErr<-read_csv("~/Desktop/serverbroken_2/hipp_gauss_err.csv")
hippErr<-hippErr %>%  rename(hipp = `0`)
visErr<-read_csv("~/Desktop/serverbroken_2/vis_gauss_err.csv")
visErr<-visErr %>%  rename(vis = `0`)
err<-cbind(visErr, hippErr) #, insulaErr
err_real<-err %>% 
  rownames_to_column() %>% 
  dplyr::filter(rowname == 1) %>% 
  pivot_longer(cols = c("hipp","vis"),values_to = "err", names_to = "region")
err_perm<-err %>% 
  rownames_to_column() %>% 
  dplyr::filter(rowname != 1) %>% 
  pivot_longer(cols = c("hipp","vis"),values_to = "err", names_to = "region")
err_perm$region<-factor(err_perm$region, levels = c("vis","hipp"))
err_real$region<-factor(err_real$region, levels = c("vis","hipp"))
number_ticks <- function(n) {function(limits) pretty(limits, n)}
err_realVis<-err_real %>% 
  dplyr::filter(region == "vis")
err_permVis<-err_perm %>% 
  dplyr::filter(region == "vis")


err_realHipp<-err_real %>% 
  dplyr::filter(region == "hipp")
err_permHipp<-err_perm %>% 
  dplyr::filter(region == "hipp")

#shuffled null EXCLUDING cue
hippErr_noCue<-read_csv("~/Desktop/serverbroken_2/hipp_gauss_err_noCue.csv")
hippErr_noCue<-hippErr_noCue %>%  rename(hipp = `0`)

hippErr_noCue_real<-hippErr_noCue %>% 
  rownames_to_column() %>% 
  dplyr::filter(rowname == 1) %>% 
  pivot_longer(cols = c("hipp"),values_to = "err", names_to = "region")
hippErr_noCue_perm<-hippErr_noCue %>% 
  rownames_to_column() %>% 
  dplyr::filter(rowname != 1) %>% 
  pivot_longer(cols = c("hipp"),values_to = "err", names_to = "region")
hippErr_noCue_real$region<-factor(hippErr_noCue_real$region, levels = c("hipp"))
hippErr_noCue_real$mod<-'No Cue Null'

hippErr_noCue_perm$region<-factor(hippErr_noCue_perm$region, levels = c("hipp"))
hippErr_noCue_perm$mod<-'No Cue Null'

err_permHipp$mod<-"Flat Null"
err_realHipp$mod<-"Flat Null"
violinprepReal<-rbind(err_realHipp, hippErr_noCue_real)
violinprepPerm<-rbind(err_permHipp, hippErr_noCue_perm)
ggplot()+
  geom_violin(violinprepPerm, mapping = aes(x = mod, y = err))+
  geom_point(violinprepReal, mapping = aes(x = mod, y = err), size = 3, color = "dodgerblue")+
  facet_wrap(~mod, scales="free")+
  ylab("Squared Errors")+xlab("Null Model")+  
  scale_color_manual(name = "Null Model", labels = c("vis"),values=c("dodgerblue")) + #"#F4B942" "#4059AD",#ba8a90
  scale_x_discrete(labels = c("hipp" = "Hippocampus"))+ #, "hipp" = "Hippocampus"
  scale_y_continuous(breaks = number_ticks(3))+
  theme_classic(base_size = 20)


#no cue violin - visual cortex
visErr_noCue<-read_csv("~/Desktop/serverbroken_2/vis_gauss_err_noCue.csv")
visErr_noCue<-visErr_noCue %>%  rename(hipp = `0`)

visErr_noCue_real<-visErr_noCue %>% 
  rownames_to_column() %>% 
  dplyr::filter(rowname == 1) %>% 
  pivot_longer(cols = c("hipp"),values_to = "err", names_to = "region")
visErr_noCue_perm<-visErr_noCue %>% 
  rownames_to_column() %>% 
  dplyr::filter(rowname != 1) %>% 
  pivot_longer(cols = c("hipp"),values_to = "err", names_to = "region")
visErr_noCue_real$region<-factor(visErr_noCue_real$region, levels = c("hipp"))
visErr_noCue_real$mod<-'No Cue Null'

visErr_noCue_perm$region<-factor(visErr_noCue_perm$region, levels = c("hipp"))
visErr_noCue_perm$mod<-'No Cue Null'

err_realVis$mod<-"Flat Null"
err_permVis$mod<-"Flat Null"
violinprepReal<-rbind(err_realVis, visErr_noCue_real)
violinprepPerm<-rbind(err_permVis, visErr_noCue_perm)
ggplot()+
  geom_violin(violinprepPerm, mapping = aes(x = mod, y = err))+
  geom_point(violinprepReal, mapping = aes(x = mod, y = err), size = 3, color = "dodgerblue")+
  facet_wrap(~mod, scales="free")+
  ylab("Squared Errors")+xlab("Null Model")+  
  scale_color_manual(name = "Null Model", labels = c("vis"),values=c("dodgerblue")) + #"#F4B942" "#4059AD",#ba8a90
  scale_x_discrete(labels = c("hipp" = "Hippocampus"))+ #, "hipp" = "Hippocampus"
  scale_y_continuous(breaks = number_ticks(3))+
  #ylim(0,0.002)+
  theme_classic(base_size = 20)


#insula
insula<-read_excel("~/Desktop/rsa/patternSim/cue_avgP_asymGaussReg_ins_final.xlsx")

insFun <- function(x) {
  0.018237757267019133 * ((x>=0)*exp(- (x**2)/(2*1.946614366336002)) + 
                            (x<0)*exp(- (x**2)/(2* 0.903691604184795)))+ -0.0037133943146860082
}

insGaussDat <- data.frame(x = c(-4,-3,-2,-1, 1,2,3,4), y = NA)
insGaussDat <- data.frame(x = seq(from = -4,to = 4, by = 0.05), y = NA)
insGaussDat$y<-insFun(insGaussDat$x)

insGauss_plot<-insula %>% 
  dplyr::select(c(amplitude, sigma_f, sigma_b, asymptote))
gaussP<-function(x, amplitude, sigma_f, sigma_b, asymptote) {
  amplitude * ((x>=0)*exp(- (x**2)/(2*sigma_f)) + 
                 (x<0)*exp(- (x**2)/(2* sigma_b)))+ -asymptote
}
x <- seq(from = -4, to = 4, by = 0.05)


points<-data.frame(y = c(-0.00603234,  0.01129097, -0.01372406,  0.01431355,  0.01395011,  0.00431204,
                         0.01084677, -0.00875643, -0.00603234), x = seq(-4,4),
                   sem = c(0.008295854,0.008934472,0.007604349, 0.008153174,0.00756854,0.007561997,0.005820077,0.007734461,0.008295854))

my_func <- plyr::mdply(insGauss_plot, function(amplitude, sigma_f, sigma_b, asymptote){
  data.frame(x=x, y=gaussP(x,amplitude, sigma_f, sigma_b, asymptote))}) %>% 
  bind_rows
my_func$sub<-rep(1:32, each=161)


myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
ggplot(data = my_func, aes(x=x, y=y, group = sub)) + 
  geom_line(color = "grey") +
  geom_point(data = points, mapping = aes(y = y, x = x, group = 1),colour = "black", fill = "white", size = 2, stroke = 1, shape = 21)+ #ba8a90
  geom_errorbar(data = points, mapping = aes(x = x, ymin=y-sem, ymax=y+sem,group = 1), color = "black",width=0.1, size = 1)+
  #stat_function(fun = hippFun,size = 2, aes(color=y))+ #ba8a90
  geom_line(data = insGaussDat, mapping = aes(x = x, y = y, color = y, group = 1), size = 2)+
  xlab("Steps from the Cue")+ylab("Pattern Similarity (r)")+
  #ylim(-0.05, 0.06)+
  #scale_color_continuous(type = "viridis", guide = 'none')+
  scale_color_gradientn(colours = myPalette(100), limits=c(min(insGaussDat$y),max(insGaussDat$y)),na.value = "black", guide = "none")+
  theme_classic(base_size = 20)

#behavXgaussian------------
sub_betas<-data.frame(participant = numeric(0), beta = numeric(0))
for (sub in unique(behav_data$participant)) {
  temp<-behav_data %>% 
    dplyr::filter(participant == sub) 
  #dplyr::filter(delay == -0.5) %>% 
  mod<-lm(rt~cor_distance, data = temp)
  
  #mod<-glm(Correct~corrAnsDistance_e, family = "binomial", data = temp)
  bind<-c(sub, mod$coefficients[2])
  sub_betas<-rbind(sub_betas, bind)
}
sub_betas$asymptote<-hipp$asymptote

#ASYMPTOTE correlation
cor.test(sub_betas$asymptote, sub_betas$X0.233548777269599, method = "spearman")
ggplot(sub_betas, aes(x=asymptote, y=X0.233548777269599))+
  geom_point(aes(x = asymptote, y= X0.233548777269599), color = "#ba8a90", size = 2 , alpha = 0.4)+
  geom_smooth(aes(x = asymptote, y= X0.233548777269599), color="#ba8a90", se=TRUE, show.legend = F, method="lm", size=2)+
  xlab("Hippocampus Asymptote") + ylab("RT Cost for \nFurther Rooms (slope)") +
  theme_classic(base_size = 20)

cor.test(vis$asymptote, sub_betas$X0.233548777269599, method = "spearman") ##significant--the lower your asymptote the higher your beta
sub_betas$asymptoteVis<-vis$asymptote

ggplot(sub_betas, aes(x=asymptoteVis, y=X0.233548777269599))+
  geom_point(aes(x = asymptoteVis, y= X0.233548777269599), color = "#8288c9", size = 2 , alpha = 0.4)+
  geom_smooth(aes(x = asymptoteVis, y= X0.233548777269599), color="#8288c9", se=TRUE, show.legend = F, method="lm", size=2)+
  xlab("Visual Cortex Asymptote") + ylab("RT Cost for \nFurther Rooms (slope)") +
  theme_classic(base_size = 20)

#forward and backward sigmas and RT beta
cor.test(hipp$sigma_f, sub_betas$X0.233548777269599, method = "spearman")
cor.test(hipp$sigma_b, sub_betas$X0.233548777269599, method = "spearman")

cor.test(vis$sigma_f, sub_betas$X0.233548777269599, method = "spearman")
cor.test(vis$sigma_b, sub_betas$X0.233548777269599, method = "spearman")

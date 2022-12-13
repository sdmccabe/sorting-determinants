####Regressions (updated)####
#Created by Ethan Busby
#June 13, 2022

library(stargazer)
library(estimatr)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(sjstats)
library(ggplot2)

Democrat=anes20[anes20$dem==1,]
Republican=anes20[anes20$rep==1,]
Independent=anes20[anes20$ind==1,]

Democrat16=anes16[anes16$dem==1,]
Republican16=anes16[anes16$rep==1,]
Independent16=anes16[anes16$ind==1,]

####H1 - predicting sorting####
reg_h1.1 <- lm(sorting_r ~ female+age+rep+ind+urban+smalltown+rural+income+educ+ideo+
                 union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+
                 facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other+
                 news_fox+news_msnbc+news_cnn+know_scale+panel, data = anes20,
                 weights=anes20$V200010b)

reg_h1.1rob <- lm_robust(sorting_r ~ female+age+rep+ind+urban+smalltown+rural+income+educ+ideo+
                           union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+
                           facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other+
                           news_fox+news_msnbc+news_cnn+know_scale+panel, data = anes20,
                         weights=anes20$V200010b)

reg_h1.2 <- lm(sorting_r ~ female+age+urban+smalltown+rural+income+educ+ideo+
                 union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+
                 facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other+
                 news_fox+news_msnbc+news_cnn+know_scale+panel, data = Democrat,
               weights=Democrat$V200010b)

reg_h1.2rob <- lm_robust(sorting_r ~ female+age+urban+smalltown+rural+income+educ+ideo+
                           union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+
                           facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other+
                           news_fox+news_msnbc+news_cnn+know_scale+panel, data = Democrat,
                         weights=Democrat$V200010b)

reg_h1.3 <- lm(sorting_r ~ female+age+urban+smalltown+rural+income+educ+ideo+
                 union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+
                 facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other+
                 news_fox+news_msnbc+news_cnn+know_scale+panel, data = Republican,
               weights=Republican$V200010b)

reg_h1.3rob <- lm_robust(sorting_r ~ female+age+urban+smalltown+rural+income+educ+ideo+
                           union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+
                           facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other+
                           news_fox+news_msnbc+news_cnn+know_scale+panel, data = Republican,
                         weights=Republican$V200010b)

reg_h1.4 <- lm(sorting_r ~ female+age+urban+smalltown+rural+income+educ+ideo+
                 union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+
                 facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other+
                 news_fox+news_msnbc+news_cnn+know_scale+panel, data = Independent,
               weights=Independent$V200010b)

reg_h1.4rob <- lm_robust(sorting_r ~ female+age+urban+smalltown+rural+income+educ+ideo+
                           union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+
                           facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other+
                           news_fox+news_msnbc+news_cnn+know_scale+panel, data = Independent,
                         weights=Independent$V200010b)


setwd("C:/Users/busby89/Google Drive/Sorting Project/R Reg Robust/Updated ANES R Reg Robust/Busby updates")
stargazer(reg_h1.1, dep.var.labels = c("Sorting"),
          covariate.labels = c("Female", "Age","Republican","Independent","Urban","Small Town","Rural","Income",
                               "Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay",
                               "Bisexual","Has Children", "Racial Resentment","Authoritarianism", "Religiosity Scale", 
                               "Born Again", "White", "Facebook", "Twitter", "Instagram", "Reddit", "Youtube",
                               "Snapchat", "Tiktok", "Other SM", "TV-Fox", "TV-MSNBC", "TV-CNN", "Political Knowledge",
                               "Panel respondent"),
          se=list(NULL,reg_h1.1rob$std.error), type='html', digits=2, out="sorting_all.html")

stargazer(reg_h1.2,reg_h1.3, reg_h1.4, dep.var.labels = c("Democrat","Republican","Independent"),
          column.labels = c("Democrats", "Republicans", "Independents"),
          dep.var.labels.include=F,
          covariate.labels = c("Female", "Age","Urban","Small Town","Rural","Income",
                               "Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay",
                               "Bisexual","Has Children", "Racial Resentment","Authoritarianism", "Religiosity Scale", 
                               "Born Again", "White", "Facebook", "Twitter", "Instagram", "Reddit", "Youtube",
                               "Snapchat", "Tiktok", "Other SM", "TV-Fox", "TV-MSNBC", "TV-CNN", "Political Knowledge",
                               "Panel respondent"),
          se=list(NULL,reg_h1.2rob$std.error,reg_h1.3rob$std.error,reg_h1.4rob$std.error), type='html', digits=2, out="sorting_party.html")

#Plot of the results:
p1=plot_model(reg_h1.1)
#Reorder coefficients
p1=plot_model(reg_h1.1, colors="system", robust=T, 
              group.terms=c(1,1,1,2,1,1,1,1,2,1,1,1,2,1,1,1,
                            2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1))
p1+ylim(-0.12, 0.03)

###Plot more than one model at once
p2=plot_models(reg_h1.3, reg_h1.4, reg_h1.2, reg_h1.1, grid=T, robust=T,
               m.labels=c("Republicans", "Independents", "Democrats", "All"),
               show.legend = F, dot.size = 1.5, colors=c("black", "blue", "purple", "red"),
               show.values=T, digits=3, value.size=2.75, spacing=0.5)

jpeg("Predicting sorting.jpeg", width=9, height=9, units="in", res=600)
p2+ylim(-.15, 0.15)+labs(title="Predicting party-ideology sorting, 2020 ANES", 
                         caption="Plot shows estimated relationship with sorting, where 0 indicates perfectly unsorted and 1 perfectly sorted. \nEstimates created with OLS regression and are weighted with ANES weights. \n*p<0.05, **p<0.01, ***p<0.001")
dev.off()

##Do this for the 2016 data##

reg16_h1.1 <- lm(sorting_r ~ female+age+rep+ind+income+educ+ideo+
                 union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+
                 facebook_tw+
                 news_fox+news_msnbc+news_cnn+know_scale, data = anes16,
               weights=anes16$V160102)

reg16_h1.1rob <- lm_robust(sorting_r ~ female+age+rep+ind+income+educ+ideo+
                           union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+
                           facebook_tw+
                           news_fox+news_msnbc+news_cnn+know_scale, data = anes16,
                         weights=anes16$V160102)

reg16_h1.2 <- lm(sorting_r ~ female+age+income+educ+ideo+
                 union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+
                 facebook_tw+
                 news_fox+news_msnbc+news_cnn+know_scale, data = Democrat16,
               weights=Democrat16$V160102)

reg16_h1.2rob <- lm_robust(sorting_r ~ female+age+income+educ+ideo+
                           union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+
                           facebook_tw+
                           news_fox+news_msnbc+news_cnn+know_scale, data = Democrat16,
                         weights=Democrat16$V160102)

reg16_h1.3 <- lm(sorting_r ~ female+age+income+educ+ideo+
                 union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+
                 facebook_tw+
                 news_fox+news_msnbc+news_cnn+know_scale, data = Republican16,
               weights=Republican16$V160102)

reg16_h1.3rob <- lm_robust(sorting_r ~ female+age+income+educ+ideo+
                           union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+
                           facebook_tw+
                           news_fox+news_msnbc+news_cnn+know_scale, data = Republican16,
                         weights=Republican16$V160102)

reg16_h1.4 <- lm(sorting_r ~ female+age+income+educ+ideo+
                 union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+
                 facebook_tw+
                 news_fox+news_msnbc+news_cnn+know_scale, data = Independent16,
               weights=Independent16$V160102)

reg16_h1.4rob <- lm_robust(sorting_r ~ female+age+income+educ+ideo+
                           union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+
                           facebook_tw+
                           news_fox+news_msnbc+news_cnn+know_scale, data = Independent16,
                         weights=Independent16$V160102)


setwd("C:/Users/busby89/Google Drive/Sorting Project/R Reg Robust/Updated ANES R Reg Robust/Busby updates")
stargazer(reg16_h1.1, dep.var.labels = c("Sorting"),
          covariate.labels = c("Female", "Age","Republican","Independent","Income",
                               "Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay",
                               "Bisexual","Has Children", "Racial Resentment","Authoritarianism", "Religiosity Scale", 
                               "Born Again", "White", "Facebook/Twitter", "TV-Fox", "TV-MSNBC", "TV-CNN", "Political Knowledge"),
          se=list(NULL,reg16_h1.1rob$std.error), type='html', digits=2, out="sorting_all_16.html")

stargazer(reg16_h1.2,reg16_h1.3, reg16_h1.4, dep.var.labels = c("Democrat","Republican","Independent"),
          column.labels = c("Democrats", "Republicans", "Independents"),
          dep.var.labels.include=F,
          covariate.labels = c("Female", "Age","Income",
                               "Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay",
                               "Bisexual","Has Children", "Racial Resentment","Authoritarianism", "Religiosity Scale", 
                               "Born Again", "White", "Facebook/Twitter","TV-Fox", "TV-MSNBC", "TV-CNN", "Political Knowledge"),
          se=list(NULL,reg16_h1.2rob$std.error,reg16_h1.3rob$std.error,reg16_h1.4rob$std.error), type='html', digits=2, out="sorting_party_16.html")

#Plot of the results:
p3=plot_model(reg16_h1.1, colors="system", robust=T, 
              group.terms=c(1,2,1,1,2,1,1,2,1,2,
                            1,1,2,1,1,1,2,2,2,2,
                            2,1,1))
p3+ylim(-0.14, 0.05)

###Plot more than one model at once
p4=plot_models(reg16_h1.3, reg16_h1.4, reg16_h1.2, reg16_h1.1, grid=T, robust=T,
               m.labels=c("Republicans", "Independents", "Democrats", "All"),
               show.legend = F, dot.size = 1.5, colors=c("black", "blue", "purple", "red"),
               show.values=T, digits=3, value.size=3,
               spacing=0.5)

jpeg("Predicting sorting_16.jpeg", width=9, height=7.5, units="in", res=600)
p4+ylim(-.15, 0.16)+labs(title="Predicting party-ideology sorting, 2016 ANES", 
                         caption="Plot shows estimated relationship with sorting, where 0 indicates perfectly unsorted and 1 perfectly sorted. \nEstimates created with OLS regression and are weighted with ANES weights. \n*p<0.05, **p<0.01, ***p<0.001")
dev.off()

#Plot both years together
p_20=p2+ylim(-.15, 0.15)+labs(title="Predicting party-ideology sorting, 2020 ANES", 
                              caption="Plot shows estimated relationship with sorting, where 0 indicates perfectly unsorted and 1 perfectly sorted. \nEstimates created with OLS regression and are weighted with ANES weights. \n*p<0.05, **p<0.01, ***p<0.001")

p_16=p4+ylim(-.15, 0.16)+labs(title="Predicting party-ideology sorting, 2016 ANES", 
                         caption="Plot shows estimated relationship with sorting, where 0 indicates perfectly unsorted and 1 perfectly sorted. \nEstimates created with OLS regression and are weighted with ANES weights. \n*p<0.05, **p<0.01, ***p<0.001")

jpeg("Predicting sorting_combined.jpeg", width=7, height=12, units="in", res=600)
plot_grid(p_16+labs(caption="", title= "2016 ANES")+ylab(""), p_20+ylab("")+labs(title="2020 ANES",
            caption="Plot shows estimated relationship with sorting, where 0 indicates perfectly unsorted and 1 perfectly sorted. \nEstimates created with OLS regression and are weighted with ANES weights. *p<0.05, **p<0.01, ***p<0.001"), 
          nrow=2, rel_heights = c(0.85,1.0), rel_widths = c(1,1))
dev.off()


####2016 data with sorting as the IV####
setwd("C:/Users/busby89/Google Drive/Sorting Project/R Reg Robust/Updated ANES R Reg Robust/Busby updates/2016 results")

## hypothesis 4 (FTDiff DV regression)

reg16_h4.1<- lm(ftdifference~ sorting_r+age+rep+ind+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =anes16, weights=V160102)
reg16_h4.1rob<- lm_robust(ftdifference~ sorting_r+age+rep+ind+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =anes16, weights=anes16$V160102)

stargazer(reg16_h4.1, dep.var.labels = c("FT Differences"),
          covariate.labels = c("Sorting", "Age", "Republican","Independent","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook/Twitter","TV-Fox", "TV-MSNBC","TV-CNN","Political Knowledge"),
          se=list(NULL,reg16_h4.1rob$std.error), type="html", digits=2, out="FTDifrob.html")

reg16_h4.1.1<- lm(ftdifference~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Democrat16, weights=V160102)
reg16_h4.1.1rob<- lm_robust(ftdifference~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Democrat16, weights=V160102)

reg16_h4.1.2<- lm(ftdifference~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Republican16, weights=V160102)
reg16_h4.1.2rob<- lm_robust(ftdifference~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Republican16, weights=V160102)

reg16_h4.1.3<- lm(ftdifference~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Independent16, weights=V160102)
reg16_h4.1.3rob<- lm_robust(ftdifference~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Independent16, weights=V160102)


## hypothesis 5 (Political involvement indexes)

reg16_h5.1 <-lm(OVERALLINDEX1 ~ sorting_r+age+rep +ind+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data = anes16, weights=V160102)
reg16_h5.1rob <-lm_robust(OVERALLINDEX1 ~ sorting_r+age+rep +ind+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data = anes16, weights=V160102)

reg16_h5.2 <-lm(SYSTEMICINDEX1 ~ sorting_r+age+rep +ind+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data = anes16, weights=V160102)
reg16_h5.2rob <-lm_robust(SYSTEMICINDEX1 ~ sorting_r+age+rep +ind+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data = anes16, weights=V160102)

reg16_h5.3<-lm(NONSYSTEMICINDEX1 ~ sorting_r+age+rep +ind+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data = anes16, weights=V160102)
reg16_h5.3rob<-lm_robust(NONSYSTEMICINDEX1 ~ sorting_r+age+rep +ind+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data = anes16, weights=V160102)

reg16_h5.4 <-lm(MASONINDEX1 ~ sorting_r+age+rep +ind+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data = anes16, weights=V160102)
reg16_h5.4rob <-lm_robust(MASONINDEX1 ~ sorting_r+age+rep +ind+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data = anes16, weights=V160102)


stargazer(reg16_h5.1,reg16_h5.2,reg16_h5.3,reg16_h5.4, dep.var.labels=c("Overall Index","Systemic Index","Non-systemic Index","Mason Index"),
          covariate.labels = c("Sorting","Age","Republican","Independent","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook/Twitter","TV-Fox", "TV-MSNBC","TV-CNN","Political Knowledge"),
          se=list(NULL,reg16_h5.1rob$std.error,reg16_h5.2rob$std.error, reg16_h5.3rob$std.error, reg16_h5.4rob$std.error), type="html", digits=2, out="PoliInvrob.html")

reg16_h5.1.1 <-lm(OVERALLINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Democrat16, weights=V160102)
reg16_h5.1.1rob <-lm_robust(OVERALLINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Democrat16, weights=V160102)

reg16_h5.2.1 <-lm(SYSTEMICINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Democrat16, weights=V160102)
reg16_h5.2.1rob <-lm_robust(SYSTEMICINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Democrat16, weights=V160102)

reg16_h5.3.1<-lm(NONSYSTEMICINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Democrat16, weights=V160102)
reg16_h5.3.1rob<-lm_robust(NONSYSTEMICINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Democrat16, weights=V160102)

reg16_h5.4.1 <-lm(MASONINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Democrat16, weights=V160102)
reg16_h5.4.1rob <-lm_robust(MASONINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Democrat16, weights=V160102)


stargazer(reg16_h5.1.1,reg16_h5.2.1,reg16_h5.3.1,reg16_h5.4.1, dep.var.labels=c("Overall Index","Systemic Index","Non-systemic Index","Mason Index"),
          covariate.labels = c("Sorting","Age","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook/Twitter","TV-Fox", "TV-MSNBC","TV-CNN","Political Knowledge"),
          se=list(NULL,reg16_h5.1.1rob$std.error,reg16_h5.2rob$std..1error, reg16_h5.3.1rob$std.error, reg16_h5.4.1rob$std.error), type="html", digits=2, out="PoliInvrobDEM.html")


reg16_h5.1.3 <-lm(OVERALLINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Independent16, weights=V160102)
reg16_h5.1.3rob <-lm_robust(OVERALLINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Independent16, weights=V160102)

reg16_h5.2.3 <-lm(SYSTEMICINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Independent16, weights=V160102)
reg16_h5.2.3rob <-lm_robust(SYSTEMICINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Independent16, weights=V160102)

reg16_h5.3.3<-lm(NONSYSTEMICINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Independent16, weights=V160102)
reg16_h5.3.3rob<-lm_robust(NONSYSTEMICINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Independent16, weights=V160102)

reg16_h5.4.3 <-lm(MASONINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Independent16, weights=V160102)
reg16_h5.4.3rob <-lm_robust(MASONINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Independent16, weights=V160102)


stargazer(reg16_h5.1.3,reg16_h5.2.3,reg16_h5.3.3,reg16_h5.4.3, dep.var.labels=c("Overall Index","Systemic Index","Non-systemic Index","Mason Index"),
          covariate.labels = c("Sorting","Age","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook/Twitter","TV-Fox", "TV-MSNBC","TV-CNN","Political Knowledge"),
          se=list(NULL,reg16_h5.1.3rob$std.error,reg16_h5.2.3rob$std.error, reg16_h5.3.3rob$std.error, reg16_h5.4.3rob$std.error), type="html", digits=2, out="PoliInvrobIND.html")

reg16_h5.1.2 <-lm(OVERALLINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Republican16, weights=V160102)
reg16_h5.1.2rob <-lm_robust(OVERALLINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Republican16, weights=V160102)

reg16_h5.2.2 <-lm(SYSTEMICINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Republican16, weights=V160102)
reg16_h5.2.2rob <-lm_robust(SYSTEMICINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Republican16, weights=V160102)

reg16_h5.3.2<-lm(NONSYSTEMICINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Republican16, weights=V160102)
reg16_h5.3.2rob<-lm_robust(NONSYSTEMICINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Republican16, weights=V160102)

reg16_h5.4.2 <-lm(MASONINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Republican16, weights=V160102)
reg16_h5.4.2rob <-lm_robust(MASONINDEX1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Republican16, weights=V160102)


stargazer(reg16_h5.1.2,reg16_h5.2.2,reg16_h5.3.2,reg16_h5.4.2, dep.var.labels=c("Overall Index","Systemic Index","Non-systemic Index","Mason Index"),
          covariate.labels = c("Sorting","Age","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook/Twitter","TV-Fox", "TV-MSNBC","TV-CNN","Political Knowledge"),
          se=list(NULL,reg16_h5.1.2rob$std.error,reg16_h5.2.2rob$std.error, reg16_h5.3.2rob$std.error, reg16_h5.4.2rob$std.error), type="html", digits=2, out="PoliInvrobREP.html")

## hypothesis 7 (compromise)

reg16_h7.1<- lm(comp1.1 ~ sorting_r+age+rep+ind+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data = anes16, weights=V160102)
reg16_h7.1rob<- lm_robust(comp1.1 ~ sorting_r+age+rep+ind+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data = anes16, weights=V160102)

reg16_h7.2 <- lm(comp2.1 ~ sorting_r+age+rep+ind+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data = anes16, weights=V160102)
reg16_h7.2rob <- lm_robust(comp2.1 ~ sorting_r+age+rep+ind+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data = anes16, weights=V160102)

stargazer(reg16_h7.1, reg16_h7.2, dep.var.labels = c("Compromise 1","Compromise 2"),
          covariate.labels = c("Sorting","Age","Republican","Independent","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook/Twitter","TV-Fox", "TV-MSNBC","TV-CNN","Political Knowledge"),
          se=list(NULL,reg16_h7.1rob$std.error,reg16_h7.2rob$std.error), type = "html",digits=2, out="Compromise.html")

##party breakdown

##comp for Democrats
reg16_h7.1.1<- lm(comp1.1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Democrat16, weights=V160102)
reg16_h7.1.1rob<- lm_robust(comp1.1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Democrat16, weights=V160102)

reg16_h7.2.1 <- lm(comp2.1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Democrat16, weights=V160102)
reg16_h7.2.1rob <- lm_robust(comp2.1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Democrat16, weights=V160102)

stargazer(reg16_h7.1.1, reg16_h7.2.1, dep.var.labels = c("Compromise 1 Democrat","Compromise 2 Democrat"),
          covariate.labels = c("Sorting","Age","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook/Twitter","TV-Fox", "TV-MSNBC","TV-CNN","Political Knowledge"),
          se=list(NULL,reg16_h7.1.1rob$std.error,reg16_h7.2.1rob$std.error), type = "html",digits=2, out="CompromiseDem.html")

#comp for republicans
reg16_h7.1.2<- lm(comp1.1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Republican16, weights=V160102)
reg16_h7.1.2rob<- lm_robust(comp1.1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Republican16, weights=V160102)

reg16_h7.2.2 <- lm(comp2.1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Republican16, weights=V160102)
reg16_h7.2.2rob <- lm_robust(comp2.1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Republican16, weights=V160102)

stargazer(reg16_h7.1.2, reg16_h7.2.2, dep.var.labels = c("Compromise 1 Republican","Compromise 2 Republican"),
          covariate.labels = c("Sorting","Age","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook/Twitter","TV-Fox", "TV-MSNBC","TV-CNN","Political Knowledge"),
          se=list(NULL,reg16_h7.1.2rob$std.error,reg16_h7.2.2rob$std.error), type = "html",digits=2, out="CompromiseRep.html")

#comp for independents
reg16_h7.1.3<- lm(comp1.1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Independent16, weights=V160102)
reg16_h7.1.3rob<- lm_robust(comp1.1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Independent16, weights=V160102)

reg16_h7.2.3 <- lm(comp2.1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Independent16, weights=V160102)
reg16_h7.2.3rob <- lm_robust(comp2.1 ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Independent16, weights=V160102)

stargazer(reg16_h7.1.3, reg16_h7.2.3, dep.var.labels = c("Compromise 1 Republican","Compromise 2 Republican"),
          covariate.labels = c("Sorting","Age","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook/Twitter","TV-Fox", "TV-MSNBC","TV-CNN","Political Knowledge"),
          se=list(NULL,reg16_h7.1.3rob$std.error,reg16_h7.2.3rob$std.error), type = "html",digits=2, out="CompromiseInd.html")

##hypothesis 8 (Fair Election)

reg16_h8 <- lm(fairelec ~ sorting_r+age+rep+ind+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data = anes16, weights=V160102)

reg16_h8rob <- lm_robust(fairelec ~ sorting_r+age+rep+ind+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data = anes16, weights=V160102)


stargazer(reg16_h8, dep.var.labels = c("Fair Election"),
          covariate.labels = c("Sorting","Age","Republican","Independent","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook/Twitter","TV-Fox", "TV-MSNBC","TV-CNN","Political Knowledge"),
          se=list(NULL,reg16_h8rob$std.error), type = "html",digits=2, out="FairElection.html")

#broken up by party

reg16_h8.1 <- lm(fairelec ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Democrat16, weights=V160102)
reg16_h8.1rob <- lm_robust(fairelec ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Democrat16, weights=V160102)

reg16_h8.2 <- lm(fairelec ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Republican16, weights=V160102)
reg16_h8.2rob <- lm_robust(fairelec ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Republican16, weights=V160102)

reg16_h8.3 <- lm(fairelec ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Independent16, weights=V160102)
reg16_h8.3rob <- lm_robust(fairelec ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Independent16, weights=V160102)

stargazer(reg16_h8.1, reg16_h8.2, reg16_h8.3, 
          column.labels = c("Democrats", "Republicans", "Independents"),
          dep.var.labels.include=F,
          covariate.labels = c("Sorting","Age","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook/Twitter","TV-Fox", "TV-MSNBC","TV-CNN","Political Knowledge"),
          se=list(NULL,reg16_h8.1rob$std.error,reg16_h8.2rob$std.error,reg16_h8.3rob$std.error), type = "html",digits=2, out="FairElectionParty3.html")

#hypothesis 9 violence and unrest

##use new violence variable (violence justified for political goals?)

reg16_h9.3.3 <- lm(vio_justy ~ sorting_r+age+rep+ind+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data = anes16, weights=V160102)
reg16_h9.3.3rob <- lm_robust(vio_justy ~ sorting_r+age+rep+ind+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data = anes16, weights=V160102)


stargazer (reg16_h9.3.3, dep.var.labels = c("Violence Justified"),
           covariate.labels = c("Sorting","Age","Republican","Independent","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook/Twitter","TV-Fox", "TV-MSNBC","TV-CNN","Political Knowledge"),
           se=list(NULL,reg16_h9.3.3rob$std.error),type = "html",digits=2, out="UnrestVioJust.html")

##break these up by party

reg16_h9.4.3 <- lm(vio_justy ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Democrat16, weights=V160102)
reg16_h9.4.3rob <- lm_robust(vio_justy ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Democrat16, weights=V160102)

stargazer(reg16_h9.4.3, dep.var.labels = c("Violence Justified"),
          covariate.labels = c("Sorting","Age","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook/Twitter","TV-Fox", "TV-MSNBC","TV-CNN","Political Knowledge"),
          se=list(NULL,reg16_h9.4.3rob$std.error),type = "html",digits=2, out="UnrestVioJustDEM.html")

reg16_h9.5.3 <- lm(vio_justy ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Republican16, weights=V160102)
reg16_h9.5.3rob <- lm_robust(vio_justy ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Republican16, weights=V160102)

stargazer(reg16_h9.5.3, dep.var.labels = c("Violence Justified"),
          covariate.labels = c("Sorting","Age","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook/Twitter","TV-Fox", "TV-MSNBC","TV-CNN","Political Knowledge"),
          se=list(NULL,reg16_h9.5.3rob$std.error),type = "html",digits=2, out="UnrestVioJustREP.html")


reg16_h9.6.3 <- lm(vio_justy ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Independent16, weights=V160102)
reg16_h9.6.3rob <- lm_robust(vio_justy ~ sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Independent16, weights=V160102)

stargazer(reg16_h9.6.3, dep.var.labels = c("Violence Justified"),
          covariate.labels = c("Sorting","Age","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook/Twitter","TV-Fox", "TV-MSNBC","TV-CNN","Political Knowledge"),
          se=list(NULL,reg16_h9.5.3rob$std.error),type = "html",digits=2, out="UnrestVioJustIND.html")


### support for democracy H10


#dem1 

reg16_h10.1.1 <- lm(dem1 ~sorting_r+age+rep+ind+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data = anes16, weights=V160102)
reg16_h10.1.1rob <- lm_robust(dem1 ~sorting_r+age+rep+ind+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data = anes16, weights=V160102)


stargazer(reg16_h10.1.1, dep.var.labels = c("Democracy"),
          covariate.labels = c("Sorting","Age","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook/Twitter","TV-Fox", "TV-MSNBC","TV-CNN","Political Knowledge"),
          se=list(NULL,reg16_h10.1.1rob$std.error),type = "html",digits=2, out="DemSupportAll.html")


reg16_h10.2.1 <- lm(dem1 ~sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Democrat16, weights=V160102)
reg16_h10.2.1rob <- lm_robust(dem1 ~sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Democrat16, weights=V160102)


stargazer(reg16_h10.2.1,dep.var.labels = c("Democracy"),
          covariate.labels = c("Sorting","Age","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook/Twitter","TV-Fox", "TV-MSNBC","TV-CNN","Political Knowledge"),
          se=list(NULL,reg16_h10.2.1rob$std.error),type = "html",digits=2, out="DemSupportDem.html")



reg16_h10.3.1 <- lm(dem1 ~sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Republican16, weights=V160102)
reg16_h10.3.1rob <- lm_robust(dem1 ~sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Republican16, weights=V160102)



stargazer(reg16_h10.3.1, dep.var.labels = c("Democracy"),
          covariate.labels = c("Sorting","Age","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook/Twitter","TV-Fox", "TV-MSNBC","TV-CNN","Political Knowledge"),
          se=list(NULL,reg16_h10.3.1rob$std.error),type = "html",digits=2, out="DemSupportRep.html")

####2016 DV visualizations####
#Trying some things out with the sjPlot options
dvs_2016=plot_models(reg16_h10.1.1, reg16_h9.3.3, reg16_h8, reg16_h7.1, reg16_h7.2, reg16_h5.4, reg16_h4.1, robust=T,
                         m.labels = c("Disatisfaction with democracy", "Support for political violence", "Skeptical of election integrity",
                                      "Compromise is selling out", "Prefer leader sticks to principles", "Participation", "Affective polarization"),
                         rm.terms = c("age", "male", "income", "educ", "ideo", "union", "gun", "married", "gay", "bi", "children", "RR", "AUTH", "religscale", 
                                      "bornagain", "white", "facebook_tw", "news_fox", "news_msnbc", "news_cnn", "know_scale", "rep", "ind"),
                         dot.size = 2, show.values=T, digits=3, value.size=3,
                     colors=c("black", "#999999", "#E69F00", "#56B4E9", "#009E73", 
                              "#D55E00", "#CC79A7"),
                     axis.labels="")

dvs_2016_std=plot_models(reg16_h10.1.1, reg16_h9.3.3, reg16_h8, reg16_h7.1, reg16_h7.2, reg16_h5.4, reg16_h4.1, robust=T,
            m.labels = c("Disatisfaction with democracy", "Support for political violence", "Skeptical of election integrity",
                         "Compromise is selling out", "Prefer leader sticks to principles", "Participation", "Affective polarization"),
            rm.terms = c("age", "male", "income", "educ", "ideo", "union", "gun", "married", "gay", "bi", "children", "RR", "AUTH", "religscale", 
                         "bornagain", "white", "facebook_tw", "news_fox", "news_msnbc", "news_cnn", "know_scale", "rep", "ind"),
            dot.size = 2, show.values=T, digits=3, value.size=3, std.est = "std",
            colors=c("black", "#999999", "#E69F00", "#56B4E9", "#009E73", 
                     "#D55E00", "#CC79A7"),
            axis.labels = "")


jpeg("DVs and sorting_16.jpeg", width=7, height=5, units="in", res=600)
dvs_2016_std+ylim(-.2, 0.2)+theme_bw()+
  geom_hline(yintercept = 0, linetype="dashed", color="red")+
  labs(title="Effects of party-ideology sorting, 2016 ANES", 
                         caption="Plot shows estimated relationship with sorting for each DV. Estimates created with \nOLS regression, are standardized, and are weighted with ANES weights. \n*p<0.05, **p<0.01, ***p<0.001")
dev.off()

####2020 DV visualizations####
setwd("C:/Users/busby89/Google Drive/Sorting Project/R Reg Robust/Updated ANES R Reg Robust/Busby updates")

####Models####
## hypothesis 4 (FTDiff DV regression)

reg_h4.1<- lm(ftdifference~ sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)
reg_h4.1rob<- lm_robust(ftdifference~ sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)

reg_h4.1.1<- lm(ftdifference~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)
reg_h4.1.1rob<- lm_robust(ftdifference~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)

reg_h4.1.2<- lm(ftdifference~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)
reg_h4.1.2rob<- lm_robust(ftdifference~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)

reg_h4.1.3<- lm(ftdifference~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)
reg_h4.1.3rob<- lm_robust(ftdifference~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)

## hypothesis 5 (Political involvement indexes)

reg_h5.1 <-lm(OVERALLINDEX1 ~ sorting_r+age+rep +ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)
reg_h5.1rob <-lm_robust(OVERALLINDEX1 ~ sorting_r+age+rep +ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)

reg_h5.2 <-lm(SYSTEMICINDEX1 ~ sorting_r+age+rep +ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)
reg_h5.2rob <-lm_robust(SYSTEMICINDEX1 ~ sorting_r+age+rep +ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)

reg_h5.3<-lm(NONSYSTEMICINDEX1 ~ sorting_r+age+rep +ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)
reg_h5.3rob<-lm_robust(NONSYSTEMICINDEX1 ~ sorting_r+age+rep +ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)

reg_h5.4 <-lm(MASONINDEX1 ~ sorting_r+age+rep +ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)
reg_h5.4rob <-lm_robust(MASONINDEX1 ~ sorting_r+age+rep +ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)

reg_h5.1.1 <-lm(OVERALLINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)
reg_h5.1.1rob <-lm_robust(OVERALLINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)

reg_h5.2.1 <-lm(SYSTEMICINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)
reg_h5.2.1rob <-lm_robust(SYSTEMICINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)

reg_h5.3.1<-lm(NONSYSTEMICINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)
reg_h5.3.1rob<-lm_robust(NONSYSTEMICINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)

reg_h5.4.1 <-lm(MASONINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)
reg_h5.4.1rob <-lm_robust(MASONINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)

reg_h5.1.3 <-lm(OVERALLINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)
reg_h5.1.3rob <-lm_robust(OVERALLINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)

reg_h5.2.3 <-lm(SYSTEMICINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)
reg_h5.2.3rob <-lm_robust(SYSTEMICINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)

reg_h5.3.3<-lm(NONSYSTEMICINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)
reg_h5.3.3rob<-lm_robust(NONSYSTEMICINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)

reg_h5.4.3 <-lm(MASONINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)
reg_h5.4.3rob <-lm_robust(MASONINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)

reg_h5.1.2 <-lm(OVERALLINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)
reg_h5.1.2rob <-lm_robust(OVERALLINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)

reg_h5.2.2 <-lm(SYSTEMICINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)
reg_h5.2.2rob <-lm_robust(SYSTEMICINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)

reg_h5.3.2<-lm(NONSYSTEMICINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)
reg_h5.3.2rob<-lm_robust(NONSYSTEMICINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)

reg_h5.4.2 <-lm(MASONINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)
reg_h5.4.2rob <-lm_robust(MASONINDEX1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)

## hypothesis 7 (compromise)

reg_h7.1<- lm(comp1.1 ~ sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)
reg_h7.1rob<- lm_robust(comp1.1 ~ sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)

reg_h7.2 <- lm(comp2.1 ~ sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)
reg_h7.2rob <- lm_robust(comp2.1 ~ sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)

##party breakdown

##comp for Democrats
reg_h7.1.1<- lm(comp1.1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)
reg_h7.1.1rob<- lm_robust(comp1.1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)

reg_h7.2.1 <- lm(comp2.1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)
reg_h7.2.1rob <- lm_robust(comp2.1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)

#comp for republicans
reg_h7.1.2<- lm(comp1.1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)
reg_h7.1.2rob<- lm_robust(comp1.1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)

reg_h7.2.2 <- lm(comp2.1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)
reg_h7.2.2rob <- lm_robust(comp2.1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)

#comp for independents
reg_h7.1.3<- lm(comp1.1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)
reg_h7.1.3rob<- lm_robust(comp1.1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)

reg_h7.2.3 <- lm(comp2.1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)
reg_h7.2.3rob <- lm_robust(comp2.1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)

##hypothesis 8 (Fair Election)

reg_h8 <- lm(fairelec ~ sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)

reg_h8rob <- lm_robust(fairelec ~ sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)


#broken up by party

reg_h8.1 <- lm(fairelec ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)
reg_h8.1rob <- lm_robust(fairelec ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)

reg_h8.2 <- lm(fairelec ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)
reg_h8.2rob <- lm_robust(fairelec ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)

reg_h8.3 <- lm(fairelec ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)
reg_h8.3rob <- lm_robust(fairelec ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)

#hypothesis 9 violence and unrest

reg_h9.1 <- lm(unrestbi ~ sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)
reg_h9.1rob <- lm_robust(unrestbi ~ sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)

reg_h9.2 <- lm(violence1 ~ sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)
reg_h9.2rob <- lm_robust(violence1 ~ sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)


#split unrest and violence by dem and rep

reg_h9.1.1 <- lm(unrest1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)
reg_h9.1.1rob <- lm_robust(unrest1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)

reg_h9.2.1 <- lm(violence1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)
reg_h9.2.1rob <- lm_robust(violence1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)

reg_h9.1.2 <- lm(unrest1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)
reg_h9.1.2rob <- lm_robust(unrest1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)

reg_h9.2.2 <- lm(violence1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)
reg_h9.2.2rob <- lm_robust(violence1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)

reg_h9.1.3 <- lm(unrest1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)
reg_h9.1.3rob <- lm_robust(unrest1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)

reg_h9.2.3 <- lm(violence1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)
reg_h9.2.3rob <- lm_robust(violence1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)

##use new violence variable (violence justified for political goals?)

reg_h9.3.1 <- lm(unrest ~ sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)
reg_h9.3.1rob <- lm_robust(unrest ~ sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)
reg_h9.3.2 <- lm(violence1 ~ sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)
reg_h9.3.2rob <- lm_robust(violence1 ~ sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)
reg_h9.3.3 <- lm(vio_justy ~ sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)
reg_h9.3.3rob <- lm_robust(vio_justy ~ sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)


##break these up by party

reg_h9.4.1 <- lm(unrest ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)
reg_h9.4.1rob <- lm_robust(unrest ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)
reg_h9.4.2 <- lm(violence1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)
reg_h9.4.2rob <- lm_robust(violence1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)
reg_h9.4.3 <- lm(vio_justy ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)
reg_h9.4.3rob <- lm_robust(vio_justy ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)


reg_h9.5.1 <- lm(unrest ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)
reg_h9.5.1rob <- lm_robust(unrest ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)
reg_h9.5.2 <- lm(violence1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)
reg_h9.5.2rob <- lm_robust(violence1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)
reg_h9.5.3 <- lm(vio_justy ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)
reg_h9.5.3rob <- lm_robust(vio_justy ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)



reg_h9.6.1 <- lm(unrest ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)
reg_h9.6.1rob <- lm_robust(unrest ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)
reg_h9.6.2 <- lm(violence1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)
reg_h9.6.2rob <- lm_robust(violence1 ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)
reg_h9.6.3 <- lm(vio_justy ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)
reg_h9.6.3rob <- lm_robust(vio_justy ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)


# Hypothesis 10
reg_h10 <- lm(violence1 ~ sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)
reg_h10rob <- lm_robust(violence1 ~ sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)


### support for democracy as a scale 

reg_h10.1 <- lm(demsupport ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)
reg_h10.1rob <- lm_robust(demsupport ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)
reg_h10.2 <- lm(demsupport ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)
reg_h10.2rob <- lm_robust(demsupport ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)
reg_h10.3 <- lm(demsupport ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)
reg_h10.3rob <- lm_robust(demsupport ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)
reg_h10.4 <- lm(demsupport ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)
reg_h10.4rob <- lm_robust(demsupport ~ sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)

## democracy variables independently 

reg_h10.1.1 <- lm(dem1 ~sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)
reg_h10.1.1rob <- lm_robust(dem1 ~sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)

stargazer(reg_h10.1.1, dep.var.labels = c("Democracy"),
          covariate.labels = c("Sorting","Age","Republican","Independent","Male","Urban", "Small town", "Rural", "Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Political Knowledge", "Panel respondent","TV-Fox", "TV-MSNBC","TV-CNN", "Facebook", "Twitter", "Instagram", "Reddit", "YouTube", "Snapchat", "TikTok", "Other SM"),
          se=list(NULL,reg_h10.1.1rob$std.error),type = "html",digits=2, out="DemSupportAll.html")

reg_h10.1.2 <- lm(dem2 ~sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)
reg_h10.1.2rob <- lm_robust(dem2 ~sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)

reg_h10.1.3 <- lm(dem3 ~sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)
reg_h10.1.3rob <- lm_robust(dem3 ~sorting_r+age+rep+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=anes20, weight=anes20$V2000106)

reg_h10.2.1 <- lm(dem1 ~sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)
reg_h10.2.1rob <- lm_robust(dem1 ~sorting_r+ind+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)

reg_h10.2.2 <- lm(dem2 ~sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)
reg_h10.2.2rob <- lm_robust(dem2 ~sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)

reg_h10.2.3 <- lm(dem3 ~sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)
reg_h10.2.3rob <- lm_robust(dem3 ~sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Democrat, weight=Democrat$V2000106)


reg_h10.3.1 <- lm(dem1 ~sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)
reg_h10.3.1rob <- lm_robust(dem1 ~sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)

reg_h10.3.2 <- lm(dem2 ~sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)
reg_h10.3.2rob <- lm_robust(dem2 ~sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)

reg_h10.3.3 <- lm(dem3 ~sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)
reg_h10.3.3rob <- lm_robust(dem3 ~sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Republican, weight=Republican$V2000106)

####Graphic####
dvs_2020=plot_models(reg_h10.1.1, reg_h10.1.2, reg_h10.1.3, reg_h9.3.3, reg_h8, reg_h7.1, reg_h7.2, reg_h5.4, reg_h4.1, robust=T,
                     m.labels = c("Disatisfaction with democracy", "Checks and balances aren't important", "Helpful if the president could act alone",
                                  "Support for political violence", "Skeptical of election integrity",
                                  "Compromise is selling out", "Prefer leader sticks to principles", "Participation", "Affective polarization"),
                     rm.terms = c("age", "male", "income", "educ", "ideo", "union", "gun", "married", "gay", "bi", "children", "RR", "AUTH", "religscale", 
                                  "bornagain", "white", "facebook", "twitter", "instagram", "reddit", "youtube", "snapchat",
                                  "tiktok", "sm_other", "news_fox", "news_msnbc", "news_cnn", "know_scale", "rep", "ind", "panel", "urban", "smalltown", "rural"),
                     dot.size = 2, show.values=T, digits=3, value.size=3,
                     colors=c("black", "#999999", "#E69F00", "#56B4E9", "#009E73", 
                              "#D55E00", "red", "purple", "#CC79A7"),
                     axis.labels="")

dvs_2020_std=plot_models(reg_h10.1.1, reg_h10.1.2, reg_h10.1.3, reg_h9.3.3, reg_h8, reg_h7.1, reg_h7.2, reg_h5.4, reg_h4.1, robust=T,
                         m.labels = c("Disatisfaction with democracy", "Checks and balances aren't important", "Helpful if the president could act alone",
                                      "Support for political violence", "Skeptical of election integrity",
                                      "Compromise is selling out", "Prefer leader sticks to principles", "Participation", "Affective polarization"),
                         rm.terms = c("age", "male", "income", "educ", "ideo", "union", "gun", "married", "gay", "bi", "children", "RR", "AUTH", "religscale", 
                                      "bornagain", "white", "facebook", "twitter", "instagram", "reddit", "youtube", "snapchat",
                                      "tiktok", "sm_other", "news_fox", "news_msnbc", "news_cnn", "know_scale", "rep", "ind", "panel", "urban", "smalltown", "rural"),
                         dot.size = 2, show.values=T, digits=3, value.size=3, std.est = "std",
                         colors=c("black", "#999999", "#E69F00", "#56B4E9", "#009E73", 
                                  "#D55E00","red", "purple", "#CC79A7"),
                         axis.labels = "",
                         spacing = 0.6)


jpeg("DVs and sorting_20.jpeg", width=7, height=5, units="in", res=600)
dvs_2020_std+ylim(-.2, 0.2)+theme_bw()+
  geom_hline(yintercept = 0, linetype="dashed", color="red")+
  labs(title="Effects of party-ideology sorting, 2020 ANES", 
       caption="Plot shows estimated relationship with sorting for each DV. Estimates created with \nOLS regression, are standardized, and are weighted with ANES weights. \n*p<0.05, **p<0.01, ***p<0.001")
dev.off()

##Combined 2020 results for the table##
#Running into an error here - let's see if I can fix it:
m1=reg_h10.1.1
m2=reg_h10.1.2
m3=reg_h10.1.3
m4=reg_h9.3.3
m5=reg_h8
m6=reg_h7.1
m7=reg_h7.2
m8=reg_h5.4
m9=reg_h4.1

stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9,
          dep.var.labels = c("Dissatisfaction with Democracy", "Checks and balances aren't important", 
                             "Helpful if president could act alone", "Support for political violence",
                             "Skeptical of election integrity", "Compromise is selling out",
                             "Prefer leader sticks to principles", "Participation", 
                             "Affective polarization"),
          covariate.labels = c("Sorting","Age","Republican","Independent","Male","Urban", "Small town", "Rural", "Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Political Knowledge", "Panel respondent","TV-Fox", "TV-MSNBC","TV-CNN", "Facebook", "Twitter", "Instagram", "Reddit", "YouTube", "Snapchat", "TikTok", "Other SM"),
          se=list(NULL,reg_h10.1.1rob$std.error, reg_h10.1.2rob$std.error, reg_h10.1.3rob$std.error, 
                  reg_h9.3.3rob$std.error, reg_h8rob$std.error, reg_h7.1rob$std.error, 
                  reg_h7.2rob$std.error, reg_h5.4rob$std.error, reg_h4.1rob$std.error),
          type = "html",digits=2, out="All DVs combined.html")

####Tables broken out by partisanship####
##Democratic satisfaction
reg_h10.4.1 <- lm(dem1 ~sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)
reg_h10.4.1rob <- lm_robust(dem1 ~sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)

stargazer(reg_h10.2.1, reg_h10.3.1, reg_h10.4.1,
          dep.var.caption= "",
          dep.var.labels = c("Dissatisfaction with Democracy"),
          column.labels = c("Democrats (2020)", "Republicans (2020)", "Independents (2020)"),
          model.numbers = F,
          covariate.labels = c("Sorting","Age","Male","Urban", "Small town", "Rural", "Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Political Knowledge", "Panel respondent","TV-Fox", "TV-MSNBC","TV-CNN", "Facebook", "Twitter", "Instagram", "Reddit", "YouTube", "Snapchat", "TikTok", "Other SM"),
          se=list(NULL,reg_h10.2.1rob$std.error, reg_h10.3.1rob$std.error, reg_h10.4.1rob$std.error),
          type = "html",digits=2, out="Democracy combined_20.html")

reg16_h10.4.1 <- lm(dem1 ~sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Independent16, weights=V160102)
reg16_h10.4.1rob <- lm_robust(dem1 ~sorting_r+age+male+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+facebook_tw+news_fox+news_msnbc+news_cnn+know_scale, data =Independent16, weights=V160102)

stargazer(reg16_h10.2.1, reg16_h10.3.1, reg16_h10.4.1,
          dep.var.caption= "",
          dep.var.labels = c("Dissatisfaction with Democracy"),
          column.labels = c("Democrats (2016)", "Republicans (2016)", "Independents (2016)"),
          model.numbers = F,
          covariate.labels = c("Sorting","Age","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook","TV-Fox", "TV-MSNBC","TV-CNN", "Political Knowledge"),
          se=list(NULL,reg16_h10.2.1rob$std.error, reg16_h10.3.1rob$std.error, reg16_h10.4.1rob$std.error),
          type = "html",digits=2, out="Democracy combined_16.html")

##Checks and Balances (only from 2020)
reg_h10.4.2 <- lm(dem2 ~sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)
reg_h10.4.2rob <- lm_robust(dem2 ~sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)

stargazer(reg_h10.2.2, reg_h10.3.2, reg_h10.4.2,
          dep.var.caption= "",
          dep.var.labels = c("Checks and balances aren't important"),
          column.labels = c("Democrats (2020)", "Republicans (2020)", "Independents (2020)"),
          model.numbers = F,
          covariate.labels = c("Sorting","Age","Male","Urban", "Small town", "Rural", "Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Political Knowledge", "Panel respondent","TV-Fox", "TV-MSNBC","TV-CNN", "Facebook", "Twitter", "Instagram", "Reddit", "YouTube", "Snapchat", "TikTok", "Other SM"),
          se=list(NULL,reg_h10.2.2rob$std.error, reg_h10.3.2rob$std.error, reg_h10.4.2rob$std.error),
          type = "html",digits=2, out="Checks and balances combined_20.html")

##President act alone (only from 2020)
reg_h10.4.3 <- lm(dem3 ~sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)
reg_h10.4.3rob <- lm_robust(dem3 ~sorting_r+age+male+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white+know_scale+panel+news_fox+news_cnn+news_msnbc+facebook+twitter+instagram+reddit+youtube+snapchat+tiktok+sm_other, data=Independent, weight=Independent$V2000106)

stargazer(reg_h10.2.3, reg_h10.3.3, reg_h10.4.3,
          dep.var.caption= "",
          dep.var.labels = c("Helpful if president could act alone"),
          column.labels = c("Democrats (2020)", "Republicans (2020)", "Independents (2020)"),
          model.numbers = F,
          covariate.labels = c("Sorting","Age","Male","Urban", "Small town", "Rural", "Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Political Knowledge", "Panel respondent","TV-Fox", "TV-MSNBC","TV-CNN", "Facebook", "Twitter", "Instagram", "Reddit", "YouTube", "Snapchat", "TikTok", "Other SM"),
          se=list(NULL,reg_h10.2.3rob$std.error, reg_h10.3.3rob$std.error, reg_h10.4.3rob$std.error),
          type = "html",digits=2, out="President act alone combined_20.html")

##Political violence
stargazer(reg_h9.4.3, reg_h9.5.3, reg_h9.6.3,
          dep.var.caption= "",
          dep.var.labels = c("Support for political violence"),
          column.labels = c("Democrats (2020)", "Republicans (2020)", "Independents (2020)"),
          model.numbers = F,
          covariate.labels = c("Sorting","Age","Male","Urban", "Small town", "Rural", "Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Political Knowledge", "Panel respondent","TV-Fox", "TV-MSNBC","TV-CNN", "Facebook", "Twitter", "Instagram", "Reddit", "YouTube", "Snapchat", "TikTok", "Other SM"),
          se=list(NULL,reg_h9.4.3rob$std.error, reg_h9.5.3rob$std.error, reg_h9.6.3rob$std.error),
          type = "html",digits=2, out="Violence combined_20.html")

stargazer(reg16_h9.4.3, reg16_h9.5.3, reg16_h9.6.3,
          dep.var.caption= "",
          dep.var.labels = c("Support for political violence"),
          column.labels = c("Democrats (2016)", "Republicans (2016)", "Independents (2016)"),
          model.numbers = F,
          covariate.labels = c("Sorting","Age","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook","TV-Fox", "TV-MSNBC","TV-CNN", "Political Knowledge"),
          se=list(NULL,reg16_h9.4.3rob$std.error, reg16_h9.5.3rob$std.error, reg16_h9.6.3rob$std.error),
          type = "html",digits=2, out="Violence combined_16.html")

##Fair election
stargazer(reg_h8.1, reg_h8.2, reg_h8.3,
          dep.var.caption= "",
          dep.var.labels = c("Skeptical of election integrity"),
          column.labels = c("Democrats (2020)", "Republicans (2020)", "Independents (2020)"),
          model.numbers = F,
          covariate.labels = c("Sorting","Age","Male","Urban", "Small town", "Rural", "Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Political Knowledge", "Panel respondent","TV-Fox", "TV-MSNBC","TV-CNN", "Facebook", "Twitter", "Instagram", "Reddit", "YouTube", "Snapchat", "TikTok", "Other SM"),
          se=list(NULL,reg_h8.1rob$std.error, reg_h8.2rob$std.error, reg_h8.3rob$std.error),
          type = "html",digits=2, out="Election combined_20.html")

stargazer(reg16_h8.1, reg16_h8.2, reg16_h8.3,
          dep.var.caption= "",
          dep.var.labels = c("Skeptical of election integrity"),
          column.labels = c("Democrats (2016)", "Republicans (2016)", "Independents (2016)"),
          model.numbers = F,
          covariate.labels = c("Sorting","Age","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook","TV-Fox", "TV-MSNBC","TV-CNN", "Political Knowledge"),
          se=list(NULL,reg16_h8.1rob$std.error, reg16_h8.2rob$std.error, reg16_h8.3rob$std.error),
          type = "html",digits=2, out="Election combined_16.html")

##Compromise is selling out

stargazer(reg_h7.1.1, reg_h7.1.2, reg_h7.1.3,
          dep.var.caption= "",
          dep.var.labels = c("Compromise is selling out"),
          column.labels = c("Democrats (2020)", "Republicans (2020)", "Independents (2020)"),
          model.numbers = F,
          covariate.labels = c("Sorting","Age","Male","Urban", "Small town", "Rural", "Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Political Knowledge", "Panel respondent","TV-Fox", "TV-MSNBC","TV-CNN", "Facebook", "Twitter", "Instagram", "Reddit", "YouTube", "Snapchat", "TikTok", "Other SM"),
          se=list(NULL,reg_h7.1.1rob$std.error, reg_h7.1.2rob$std.error, reg_h7.1.3rob$std.error),
          type = "html",digits=2, out="Selling out combined_20.html")

stargazer(reg16_h7.1.1, reg16_h7.1.2, reg16_h7.1.3,
          dep.var.caption= "",
          dep.var.labels = c("Compromise is selling out"),
          column.labels = c("Democrats (2016)", "Republicans (2016)", "Independents (2016)"),
          model.numbers = F,
          covariate.labels = c("Sorting","Age","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook","TV-Fox", "TV-MSNBC","TV-CNN", "Political Knowledge"),
          se=list(NULL,reg16_h7.1.1rob$std.error, reg16_h7.1.2rob$std.error, reg16_h7.1.3rob$std.error),
          type = "html",digits=2, out="Selling out_16.html")

##Leader who sticks to principles

stargazer(reg_h7.2.1, reg_h7.2.2, reg_h7.2.3,
          dep.var.caption= "",
          dep.var.labels = c("Prefer leader who sticks to principles"),
          column.labels = c("Democrats (2020)", "Republicans (2020)", "Independents (2020)"),
          model.numbers = F,
          covariate.labels = c("Sorting","Age","Male","Urban", "Small town", "Rural", "Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Political Knowledge", "Panel respondent","TV-Fox", "TV-MSNBC","TV-CNN", "Facebook", "Twitter", "Instagram", "Reddit", "YouTube", "Snapchat", "TikTok", "Other SM"),
          se=list(NULL,reg_h7.2.1rob$std.error, reg_h7.2.2rob$std.error, reg_h7.2.3rob$std.error),
          type = "html",digits=2, out="Principles combined_20.html")

stargazer(reg16_h7.2.1, reg16_h7.2.2, reg16_h7.2.3,
          dep.var.caption= "",
          dep.var.labels = c("Prefer leader who sticks to principles"),
          column.labels = c("Democrats (2016)", "Republicans (2016)", "Independents (2016)"),
          model.numbers = F,
          covariate.labels = c("Sorting","Age","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook","TV-Fox", "TV-MSNBC","TV-CNN", "Political Knowledge"),
          se=list(NULL,reg16_h7.2.1rob$std.error, reg16_h7.2.2rob$std.error, reg16_h7.2.3rob$std.error),
          type = "html",digits=2, out="Principles combined_16.html")

##Participation

stargazer(reg_h5.4.1, reg_h5.4.2, reg_h5.4.3,
          dep.var.caption= "",
          dep.var.labels = c("Participation"),
          column.labels = c("Democrats (2020)", "Republicans (2020)", "Independents (2020)"),
          model.numbers = F,
          covariate.labels = c("Sorting","Age","Male","Urban", "Small town", "Rural", "Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Political Knowledge", "Panel respondent","TV-Fox", "TV-MSNBC","TV-CNN", "Facebook", "Twitter", "Instagram", "Reddit", "YouTube", "Snapchat", "TikTok", "Other SM"),
          se=list(NULL,reg_h5.4.1rob$std.error, reg_h5.4.2rob$std.error, reg_h5.4.3rob$std.error),
          type = "html",digits=2, out="Participation combined_20.html")

stargazer(reg16_h5.4.1, reg16_h5.4.2, reg16_h5.4.3,
          dep.var.caption= "",
          dep.var.labels = c("Participation"),
          column.labels = c("Democrats (2016)", "Republicans (2016)", "Independents (2016)"),
          model.numbers = F,
          covariate.labels = c("Sorting","Age","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook","TV-Fox", "TV-MSNBC","TV-CNN", "Political Knowledge"),
          se=list(NULL,reg16_h5.4.1rob$std.error, reg16_h5.4.2rob$std.error, reg16_h5.4.3rob$std.error),
          type = "html",digits=2, out="Participation combined_16.html")

##Affective polarization
stargazer(reg_h4.1.1, reg_h4.1.2,
          dep.var.caption= "",
          dep.var.labels = c("Affective polarization"),
          column.labels = c("Democrats (2020)", "Republicans (2020)"),
          model.numbers = F,
          covariate.labels = c("Sorting","Age","Male","Urban", "Small town", "Rural", "Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Political Knowledge", "Panel respondent","TV-Fox", "TV-MSNBC","TV-CNN", "Facebook", "Twitter", "Instagram", "Reddit", "YouTube", "Snapchat", "TikTok", "Other SM"),
          se=list(NULL,reg_h4.1.1rob$std.error, reg_h4.1.2rob$std.error),
          type = "html",digits=2, out="Affective polarization combined_20.html")

stargazer(reg16_h4.1.1, reg16_h4.1.2,
          dep.var.caption= "",
          dep.var.labels = c("Affective polarization"),
          column.labels = c("Democrats (2016)", "Republicans (2016)"),
          model.numbers = F,
          covariate.labels = c("Sorting","Age","Male","Income","Education","Ideology","Union Membership","Gun Ownership","Marital Status","Gay","Bisexual","Has Children","Racial Resentment","Authoritarianism", "Religiosity Scale", "Born Again", "White", "Facebook","TV-Fox", "TV-MSNBC","TV-CNN", "Political Knowledge"),
          se=list(NULL,reg16_h4.1.1rob$std.error, reg16_h4.1.2rob$std.error),
          type = "html",digits=2, out="Affective polarization combined_16.html")
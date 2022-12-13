# Paired t-tests on sorting DVs to compare 2020 and 2016 
female+age+rep+ind+urban+smalltown+rural+income+educ+ideo+union+gun+married+gay+bi+children+RR+AUTH+religscale+bornagain+white

## Need to clean some of the variables because they were not coded for 2016

# Independents
panel_combined$ind.y=ifelse(panel_combined$V161158x==4, 1, 0)

#Urban/rural id
panel_combined$urban.y=ifelse(panel_combined$V167534==4, 1,0)
panel_combined$rural.y=ifelse(panel_combined$V167534==1, 1,0)

#Education: requires recoding because ANES disaggregated by grade in 2016 (weird)
panel_combined$educ.x= ifelse(panel_combined$V201510>0&panel_combined$V201510<9,panel_combined$V201510,NA)

panel_combined$educ.y=0
panel_combined$educ.y[panel_combined$V161270>0&panel_combined$V161270<9]=1

panel_combined$educ.y[panel_combined$V161270==9]=2
panel_combined$educ.y[panel_combined$V161270==10]=3
panel_combined$educ.y[panel_combined$V161270==11]=4
panel_combined$educ.y[panel_combined$V161270==12]=5
panel_combined$educ.y[panel_combined$V161270==13]=6
panel_combined$educ.y[panel_combined$V161270==14]=7
panel_combined$educ.y[panel_combined$V161270==15]=8
panel_combined$educ.y[panel_combined$V161270==16]=8

#Religiosity: need to code for 2020 as well
panel_combined$religscale.x=ifelse(panel_combined$V201452==2,0,NA )

panel_combined$religscale.x=ifelse(panel_combined$V201453==5,0,panel_combined$religscale.x)
panel_combined$religscale.x=ifelse(panel_combined$V201453==1,0.80, panel_combined$religscale.x)
panel_combined$religscale.x=ifelse(panel_combined$V201453==2,0.60, panel_combined$religscale.x)
panel_combined$religscale.x=ifelse(panel_combined$V201453==3,0.40, panel_combined$religscale.x)
panel_combined$religscale.x=ifelse(panel_combined$V201453==4,0.20, panel_combined$religscale.x)
panel_combined$religscale.x=ifelse(panel_combined$V201454==1,0.80, panel_combined$religscale.x)
panel_combined$religscale.x=ifelse(panel_combined$V201454==2,1.00, panel_combined$religscale.x)

panel_combined$religscale.y=ifelse(panel_combined$V161244==2,0,NA )

panel_combined$religscale.y=ifelse(panel_combined$V161245==5,0,panel_combined$religscale)
table(panel_combined$religscale.y)
panel_combined$religscale.y=ifelse(panel_combined$V161245==1,0.80, panel_combined$religscale)
panel_combined$religscale.y=ifelse(panel_combined$V161245==2,0.60, panel_combined$religscale)
panel_combined$religscale.y=ifelse(panel_combined$V161245==3,0.40, panel_combined$religscale)
panel_combined$religscale.y=ifelse(panel_combined$V161245==4,0.20, panel_combined$religscale)
panel_combined$religscale.y=ifelse(panel_combined$V161245a==1,0.80, panel_combined$religscale)
panel_combined$religscale.y=ifelse(panel_combined$V161245a==2,1.00, panel_combined$religscale)

#Born again: need to code for 2020 as well
panel_combined$bornagain.x=ifelse(panel_combined$V201456==1,1,0)
panel_combined$bornagain.y=ifelse(panel_combined$V161263==1,1,0)


## Now can start t tests ##

#White
t.test(panel_combined$white.x,panel_combined$white.y, paired=T)

#Born Again
t.test(panel_combined$bornagain.x, panel_combined$bornagain.y, paired=T)

#Religiosity
t.test(panel_combined$religscale.x, panel_combined$religscale.y, paired=T)

#Authoritarian
t.test(panel_combined$AUTH.x, panel_combined$AUTH.y, paired=T)

#Racial resentment
t.test(panel_combined$RR.x, panel_combined$RR.y, paired=T)

#Children
t.test(panel_combined$children.x, panel_combined$children.y, paired=T)

#bi
t.test(panel_combined$bi.x, panel_combined$bi.y, paired=T)

#gay
t.test(panel_combined$gay.x, panel_combined$gay.y, paired=T)

#married
t.test(panel_combined$married.x, panel_combined$married.y, paired=T)

#gun ownership
t.test(panel_combined$gun.x, panel_combined$gun.y, paired=T)

#Union membership
t.test(panel_combined$union.x, panel_combined$union.y, paired=T)

#ideology
t.test(panel_combined$ideo.x, panel_combined$ideo.y, paired=T)

#education
t.test(panel_combined$educ.x, panel_combined$educ.y, paired=T)

#income
t.test(panel_combined$income.x, panel_combined$income.y, paired=T)

#rural
t.test(panel_combined$rural, panel_combined$rural.y, paired=T)

#small town
t.test(panel_combined$smalltown.x, panel_combined$smalltown.y, paired=T)

#urban
t.test(panel_combined$urban, panel_combined$urban.y, paired=T)

#Independent
t.test(panel_combined$ind, panel_combined$ind.y,paired=T)

#Republican
t.test(panel_combined$rep.x,panel_combined$rep.y, paired=T)

#Age
t.test(panel_combined$age.x, panel_combined$age.y, paired=T)

#Female
t.test(panel_combined$female.x, panel_combined$female.y, paired=T)

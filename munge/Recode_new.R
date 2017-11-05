#These first two are based on "satis" vars, so must be recoded before them.
df$HEALTHBENEFITS=df$SATIS02; levels(df$HEALTHBENEFITS)=c("Health Ins", "No Health Ins",rep("Health Ins",3)) #Not Applicable means "No insureance"
df$RETIREBENEFITS=df$SATIS03; levels(df$RETIREBENEFITS)=c("Retirement","No Retirement",rep("Retirement",3)) 

df<-df %>%
  na_if("Not applicable") %>%
  droplevels()

satisvars<- df %>% select(starts_with("SATIS"),-SATIS_WORKPLACE,-SATIS_COMPENSATION) %>% names()
for (i in satisvars){df[,i]<-as.numeric(unlist(factor(df[,i], levels = c("Not satisfied","Marginally satisfied","Satisfied","Very satisfied"))))
} # Make them all numeric

df<-df %>% mutate(GAPPANTT=factor(if_else(FULLSTAT=="Yes", "FT NTT", 
                                          if_else(FULLSTAT=="No" & PTCHOICE=="No", "Aspiring Academic",
                                                  if_else(FULLSTAT=="No" & PTCHOICE=="Yes" & PTCAREER=="Yes", "Expert",
                                                          if_else(FULLSTAT=="No" & PTCHOICE=="Yes" & PTCAREER=="No" & GENACT03=="Yes", "Career-Ender", "Freelancer"))))))
df$GAPPANTT<-relevel(df$GAPPANTT,"FT NTT")

df<-df %>% mutate(TURNINTENT=factor(if_else(PASTACT01=="Yes" |  PASTACT02=="Yes", "Yes", "No")))
# This defines Turnover and dropping out of academia entirely, not changing academic employers. (see Pastact04)
df$YEAR<-NULL # 2010 for all R's
df$RESTYPE<-NULL # empty vector

df$AGE<-2010-df$BIRTHYR # other age varaible is cut in intervals for some reason

df<- df %>% 
  mutate(MARITAL2=MARITAL) %>% 
  mutate(MARITAL2= car::recode(MARITAL2, "c('Single','Divorced','Widowed','Separated')='Unmarried'; c('Married','Unmarried, living with partner')='Married'"))
df$MARITAL2<-relevel(df$MARITAL2,"Unmarried")

df<- df %>% 
  mutate(RACE=RACEGROUP)  %>% 
  mutate(RACE= car::recode(RACE, "c('American Indian','Other','Two or more race/ethnicity')='Other'"))
df$RACE<-relevel(df$RACE,"White")
df$RACEGROUP2<-df$RACEGROUP; levels(df$RACEGROUP2)<-c(rep("Minority",6),"White") # cohab=married


df<- df %>%
  mutate(DEGEARN2=recode(DEGEARN, `Bachelors (B.A., B.S., etc.)`="BA or Less",                                   
                         `Ed.D.`="Professional",
                         `LL.B., J.D.`="Professional",
                         `M.D., D.D.S. (or equivalent)`="Professional",
                         `Masters (M.A., M.S., M.F.A., M.B.A., etc.)`="Masters",
                         `None`="BA or Less", 
                         `Other degree`="BA or Less", # Assuming this means less, like an assoc. degree
                         `Other first professional degree beyond B.A. (e.g., D.D., D.V.M.)`="Professional",
                         `Ph.D.`="Ph.D."))

#df<- df %>% 
#  mutate(DEGEARN2 = car::recode(DEGEARN, "c('Bachelors (B.A., B.S., etc.)','None','Other degree')='BA or Less';
#                               'Masters (M.A., M.S., M.F.A., M.B.A., etc.)'='Masters';
#                               c('Ed.D.','LL.B., J.D.','M.D., D.D.S. (or equivalent)','Other first professional degree beyond B.A. (e.g., D.D., D.V.M.)')='Professional'"))
df$DEGEARN2<-factor(df$DEGEARN2,levels=c("Ph.D.","Professional","Masters","BA or Less"))

df$SEX<-relevel(df$SEX,"Male")

df$INSTTYPE<-relevel(df$INSTTYPE,"4-year")

df<- df %>% 
  mutate(CARNEGIE2 = recode(CARNEGIE, `RU/VH: Research Universities (very high research activity)`="R1",  
                           `RU/H: Research Universities (high research activity)`="R2",                                                              
                           `DRU: Doctoral/Research Universities`="R3/Doctoral",                                                                                 
                           `Assoc/Pub-R-L: Associates--Public Rural-serving Large` = "Associates/Other", 
                           `Assoc/Pub-R-M: Associates--Public Rural-serving Medium`="Associates/Other", 
                           `Assoc/Pub-S-MC: ASSOCIATE--Public Suburban-serving Multicampus`="Associates/Other",
                           `Assoc/Pub-S-SC: ASSOCIATE--Public Suburban-serving Single Campus`="Associates/Other",                                                    
                           `Assoc/Pub-U-MC: ASSOCIATE--Public Urban-serving Multicampus`="Associates/Other",                                                         
                           `Assoc/Pub-U-SC: ASSOCIATE--Public Urban-serving Single Campus`="Associates/Other",                                                       
                           `Assoc/Pub2in4: ASSOCIATE--Public 2-year colleges under 4-year universities`="Associates/Other",                                          
                           `Assoc/Pub4: ASSOCIATE--Public 4-year Primarily ASSOCIATE`="Associates/Other",
                           `Bac/A&S: Baccalaureate Colleges--Arts & Sciences`="Bachelors/Masters",                                                                  
                           `Bac/Assoc: Baccalaureate/ASSOCIATE Colleges`="Bachelors/Masters",                                               
                           `Bac/Diverse: Baccalaureate Colleges--Diverse Fields`="Bachelors/Masters",                                                                 
                           `Masters L: Masters Colleges and Universities (larger programs)`="Bachelors/Masters",                                                      
                           `Masters M: Masters Colleges and Universities (medium programs)`="Bachelors/Masters",                                                      
                           `Masters S: Masters Colleges and Universities (smaller programs)`="Bachelors/Masters",
                           `Spec/Arts: Special Focus Institutions--Schools of art, music, and design`="Associates/Other",                                           
                           `Spec/Bus: Special Focus Institutions--Schools of business and management`="Associates/Other",                                            
                           `Spec/Faith: Special Focus Institutions--Theological seminaries, Bible colleges, and other faith-related institutions`="Associates/Other",
                           `Spec/Health: Special Focus Institutions--Other health professions schools`="Associates/Other",                                           
                           `Spec/Other: Special Focus Institutions--Other special-focus institutions`="Associates/Other", `-3` = NA_character_))
df$CARNEGIE2<-factor(df$CARNEGIE2, levels = c("R1","R2","R3/Doctoral","Bachelors/Masters","Associates/Other"))


df<- df %>% 
  mutate(BIGLAN = recode(DEPT, `Agriculture/natural resources/related`="Hard/Applied",
                         `Architecture and related services`="Hard/Applied",
                         `Area/ethnic/cultural/gender studies`="Soft/Pure",
                         `Arts (visual and performing)`="Soft/Pure",
                         `Biological and biomedical sciences`="Hard/Applied",
                         `Business/management/marketing/related`="Hard/Applied",
                         `Communication/journalism/ comm. tech`="Soft/Applied",
                         `Computer/info sciences/support tech`="Hard/Applied",
                         `Construction trades`="Hard/Applied",
                         `Education`="Soft/Applied",
                         `Engineering technologies/technicians`="Hard/Applied",
                         `English language and literature/letters`="Soft/Pure",
                         `Family/consumer sciences, human sciences`="Soft/Applied",
                         `Foreign languages/literature/linguistics`="Soft/Pure",
                         `Health professions/clinical sciences`="Hard/Applied",
                         `Legal professions and studies`="Soft/Applied",
                         `Library science`="Soft/Applied",
                         `Mathematics and statistics`="Hard/Applied",
                         `Mechanical/repair technologies/techs`="Hard/Applied",
                         `Multi/interdisciplinary studies`="Soft/Pure",
                         `Other`="Other",
                         `Parks/recreation/leisure/fitness studies`="Soft/Applied",
                         `Personal and culinary services`="Soft/Applied",
                         `Philosophy, religion & theology`="Soft/Pure",
                         `Physical sciences`="Hard/Pure",
                         `Psychology`="Soft/Pure",
                         `Public administration/social services`="Soft/Applied",
                         `Science technologies/technicians`="Hard/Applied",
                         `Security & protective services`="Soft/Applied",
                         `Social sciences (except psych) and history`="Soft/Pure",
                         `Transportation & materials moving`="Other"))
df$BIGLAN<-factor(df$BIGLAN, levels = c("Hard/Applied","Hard/Pure","Soft/Applied","Soft/Pure","Other"))
df$BIGLAN2<-df$DEPT; levels(df$BIGLAN2)<-c("Hard.Applied.Life","Soft.Applied.NonLife","Soft.Pure.Life","Soft.Pure.NonLife","Hard.Applied.Life","Soft.Applied.NonLife","Soft.Applied.NonLife","Hard.Applied.NonLife","Hard.Applied.NonLife","Soft.Applied.Life","Hard.Applied.NonLife","Soft.Pure.NonLife","Soft.Applied.Life","Soft.Pure.NonLife","Hard.Applied.Life","Soft.Applied.NonLife","Soft.Applied.NonLife","Hard.Pure.NonLife","Hard.Applied.NonLife","Soft.Pure.Life","Soft.Applied.Life","Hard.Applied.NonLife","Soft.Pure.NonLife","Soft.Pure.NonLife","Hard.Pure.NonLife","Soft.Pure.Life","Soft.Applied.NonLife","Hard.Applied.NonLife","Soft.Applied.NonLife","Soft.Pure.Life","Soft.Applied.NonLife","Other")
df$BIGLAN3<-df$BIGLAN2; levels(df$BIGLAN3)<-c("Hard.Applied","Soft.Applied","Soft.Pure.Life","Soft.Pure.NonLife","Hard.Applied","Soft.Applied","Hard.Pure","Other")

df<- df %>%
  mutate(OBEREG=recode(OBEREG, `Far West - AK CA HI NV OR WA`="West_Other",
                       `Great Lakes - IL IN MI OH WI`="Midwest",
                       `Mid East - DE DC MD NJ NY PA`="East",
                       `New England - CT ME MA NH RI VT`="East",
                       `Other`="West_Other",
                       `Plains - IA KS MN MO NE ND SD`="Midwest",
                       `Rocky Mountains - CO ID MT UT WY`="West_Other",
                       `Southeast - AL AR FL GA KY LA MS NC SC TN VA WV`="South",
                       `Southwest - AZ NM OK TX`="West_Other"))
df$OBEREG<-relevel(df$OBEREG,"East")

df$TIMEEMPLOYED<-2010-df$APPTYR

df$SUBJID<-factor(df$SUBJID)

df<- df %>%
  mutate(PRINACT2=recode(PRINACT, `Services to clients and patients`="Admin/Other",`Administration`="Admin/Other",`Other`="Admin/Other"))
df$PRINACT2<-factor(df$PRINACT2, levels = c("Teaching","Research","Admin/Other"))

df$ACADRANK<-factor(df$ACADRANK, levels = c("Instructor","Lecturer","Assistant Professor","Associate Professor","Professor"))

df<- df %>%
  mutate(INSTOPN10N=as.numeric(INSTOPN10)) %>%
  group_by(ACE) %>%
  mutate(AVGINSTOPN10N = mean(INSTOPN10N,na.rm=T)) %>%
  as.data.frame()

df<- df %>%
  mutate(INSTOPN11N=as.numeric(INSTOPN11)) %>%
  group_by(ACE) %>%
  mutate(AVGINSTOPN11N = mean(INSTOPN11N,na.rm=T)) %>%
  as.data.frame()

df<- df %>%
  mutate(INSTDESCR03N=as.numeric(INSTDESCR03)) %>%
  group_by(ACE) %>%
  mutate(FACRESP = mean(INSTDESCR03N,na.rm=T)) %>%
  as.data.frame()

df<- df %>%
  mutate(INSTDESCR08N=as.numeric(INSTDESCR08)) %>%
  group_by(ACE) %>%
  mutate(ADMINRESP = mean(INSTDESCR08N,na.rm=T)) %>%
  as.data.frame()

df$ADJUNCT1<- factor(rep(NA, nrow(df)), levels=c("Professional Adjuncts","Itinerant Academic","Single Inst Adjunct","Full-time") )   
df$ADJUNCT1[df$FULLSTAT=="Yes"] <- "Full-time"
df$ADJUNCT1[df$PTCAREER=="Yes"] <- "Professional Adjuncts"
df$ADJUNCT1[df$PTCAREER=="No" & df$PTTEACH>1 ] <- "Itinerant Academic"
df$ADJUNCT1[df$PTCAREER=="No" & df$PTTEACH==1 ] <- "Single Inst Adjunct"

df$NCHILD3<-as.numeric(df$NCHILD1)-1+as.numeric(df$NCHILD2)-1 # 4+ changes to four in this op
df$PARENT<-factor(df$NCHILD3); levels(df$PARENT) <- c("No",rep("Yes",8))

HPWvars<- df %>% select(starts_with("HPW")) %>% names()
for (i in HPWvars){
  df[,i]<-factor(df[,i], levels = c("None","1-4","5-8","9-12","13-16","17-20","21-34","35-44","45+"))
} 
hpws<-c(0,2.5,6.5,10.5,14.5,18.5,27.5, 39.5,50) # These are ~ the average value within each interval
for (i in HPWvars){
  tempvar<-as.numeric(df[,i])
  name <- paste(i, "Q", sep = "")
  for(ii in 1:length(hpws)){
    tempvar[tempvar==ii]<-hpws[ii]
  } # Assumes midpoint of age interval
  df[name]<-assign(name, tempvar)
}

# Salary and Income
df$SALARYALL=pmax(df$SALARY, df$PTSALARY, na.rm = TRUE) # important to include the combined salary variable so that salary insn't exclude from the clustering.
# 68 people added salary sources very poorly.
df$INCOME<-df$SALARYALL/(df$SALARYSOURCE01/100)
df$INCOME[df$INCOME=="Inf"]<-0
df$LINCOME=log(abs(df$INCOME));df$LINCOME[df$LINCOME=="-Inf"]=0

# Professional Development factor
profdf<-df %>% select(starts_with("PROFDEV")) %>% as.data.frame
PROFDEVvars<-df %>% select(starts_with("PROFDEV")) %>% names() 
for(i in PROFDEVvars){
  levels(profdf[,i])=c(rep("0",3),"1")
  profdf[,i]<-as.numeric(profdf[,i])-1
}
pca<-princomp(~ ., data = profdf, na.action=na.exclude)
df$PROFDEVFAC<--(pca$scores[,1]) # I think this is the scores (for comp 1). sign is negative so inverse it.

df$SELECTIVITY2=cut(df$SELECTIVITY, breaks=quantile(df$SELECTIVITY, probs = c(0,.9,1),na.rm=T))  # defined as median SAT math and verbal (or ACT composite) of 1st time freshmen
levels(df$SELECTIVITY2)<-c("Not","Selective")

levels(df$GENACT01) <- c("Non-Union", "Union") #Act: Are you a member of a faculty union?
levels(df$GENACT02) <- c("Non-Citizen", "Citizen") #Act: Are you a member of a faculty union?

df<- df %>%
  mutate(INSTOPN10N=as.numeric(INSTOPN10)) %>%
  group_by(ACE) %>%
  mutate(AVGINSTOPN10N = mean(INSTOPN10N,na.rm=T)) %>%
  as.data.frame()

df<- df %>%
  mutate(INSTOPN11N=as.numeric(INSTOPN11)) %>%
  group_by(ACE) %>%
  mutate(AVGINSTOPN11N = mean(INSTOPN11N,na.rm=T)) %>%
  as.data.frame()

df<- df %>%
  mutate(INSTDESCR03N=as.numeric(INSTDESCR03)) %>%
  group_by(ACE) %>%
  mutate(FACRESP = mean(INSTDESCR03N,na.rm=T)) %>%
  as.data.frame()

df<- df %>%
  mutate(INSTDESCR08N=as.numeric(INSTDESCR08)) %>%
  group_by(ACE) %>%
  mutate(ADMINRESP = mean(INSTDESCR08N,na.rm=T)) %>%
  as.data.frame()


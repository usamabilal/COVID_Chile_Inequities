rm(list=ls())
library(data.table)
library(tidyverse)
library(readxl)
library(zoo)
library(lubridate)
library(broom)
library(gridExtra)
# data sources: 
# std pop from SEER https://seer.cancer.gov/stdpopulations/
# DEIS mortality: https://deis.minsal.cl/#datosabiertos
# population: https://www.ine.cl/estadisticas/sociales/demografia-y-vitales/proyecciones-de-poblacion
# schooling: https://redatam-ine.ine.cl/redbin/RpWebEngine.exe/Portal?BASE=CENSO_2017&lang=esp
# overcrowding: http://siedu.ine.cl/descargar/descarga.html

gran_santiago<-c(13114,13115,13132,13107,13124,13125,13102,13116,13119,13105,13112,13109,13131,13110,13111,13113,13120,13118,13129,13108,13127,13104,13128,13103,13126,13101,13106,13117,13121,13130,13123,13605,13122,13201,13401,13604)
std.pop<-read.fwf(file="Data/stdpop.19ages.txt", widths=c(3, 3, 8),
                  col.names = c("std", "age", "pop")) %>% 
  filter(std==10) %>% 
  mutate(age=ifelse(age<=1, age, (age-1)*5),
         age=ifelse(age>=80, 80, age),
         std_pop=pop/sum(pop)) %>% 
  group_by(age) %>% 
  summarise(w=sum(std_pop))
edu<-read_excel("Data/yrs_schooling.xlsx", skip=9) %>% 
  slice(-1) %>% 
  rename(AREA=2,
         edu=4) %>% 
  mutate(id_comuna=ifelse(grepl("AREA", AREA), AREA, NA),
         id_comuna=na.locf(id_comuna),
         id_comuna=as.numeric(substr(id_comuna, 7, 12)),
         years=as.numeric(edu)) %>% 
  filter(!is.na(years)) %>% 
  select(id_comuna, years)
hacin<-read_excel("Data/INDICADORES/LB2018_MATRIZ_SIEDU.xlsx", sheet="IS_33_I", skip=1) %>% 
  rename(id_comuna=CUT, hacin=`IS_33 Porcentaje de Hacinamiento`) %>% 
  select(id_comuna, hacin)
pop<-fread("Data/ine_estimaciones-y-proyecciones-2002-2035_base-2017_comunas0381d25bc2224f51b9770a705a434b74.csv") %>% 
  select(matches("^Comuna|Sexo|Edad|Poblacion")) %>% 
  rename(sex=2) %>% 
  gather(year, pop, -sex, -Comuna, -Edad) %>% 
  mutate(year=as.numeric(sub("Poblacion ", "", year)),
         age=case_when(
           Edad==0 ~ 0,
           Edad%in%(1:4) ~ 1,
           Edad>=80 ~ 80,
           T~floor(Edad/5)*5
         ), id_comuna=Comuna) %>% 
  group_by(id_comuna, age, year) %>% 
  summarise(pop=sum(pop)) %>% 
  mutate(stage=case_when(
    year==2020 ~ "pandemic",
    year%in%(2016:2020) ~ "pre",
    T ~ ""
  )) %>% 
  filter(stage!="") %>% 
  group_by(id_comuna, age, stage) %>% 
  summarise(pop=sum(pop))
deaths<-fread("Data/DEFUNCIONES_FUENTE_DEIS_2016_2020_24092020/DEFUNCIONES_FUENTE_DEIS_2016_2020_24092020.csv") %>% 
  mutate(date=dmy(FECHA_DEF),
         month=month(date),
         age=ifelse(EDAD_TIPO==1, EDAD_CANT, 0),
         age=case_when(
                    age==0 ~ 0,
                    age%in%(1:4) ~ 1,
                    age>=80 ~ 80,
                    T~floor(age/5)*5
         ),
         year=year(date),
         stage=ifelse(year==2020, "pandemic", "pre"),
         id_comuna=COMUNA) %>% 
  filter(month<9, EDAD_TIPO!=9) %>% 
  group_by(stage, age, id_comuna) %>% 
  summarise(deaths=n())

template<-expand.grid(stage=unique(deaths$stage),
                      age=unique(deaths$age),
                      id_comuna=unique(deaths$id_comuna))
deaths<-full_join(deaths, template) %>% 
  mutate(deaths=replace_na(deaths, 0))

dta<-full_join(deaths, pop) %>% 
  full_join(std.pop) %>% 
  mutate(rate=deaths/pop,
         rate=rate*w) %>% 
  group_by(stage, id_comuna) %>% 
  summarise(rate=sum(rate)*100000) %>% 
  full_join(edu) %>% 
  full_join(hacin) %>% 
  mutate(comuna_str=sprintf("%05d", id_comuna),
         region=substr(comuna_str, 1, 2),
         stage=factor(stage, levels=c("pre", "pandemic"))) %>% 
  filter(id_comuna%in%gran_santiago)
  #filter(region=="13")

labels<-c("2016-2019", "2020")
names(labels)<-c("pre", "pandemic")


dta %>% group_by(stage) %>% 
  group_modify(~{
    lm(log(rate)~I(years), data=.x) %>% tidy %>% 
      filter(term!="(Intercept)") %>% 
      mutate(rr=exp(estimate),
             lci=exp(estimate-1.96*std.error),
             uci=exp(estimate+1.96*std.error)) %>% 
      select(rr, lci, uci)
  })

dta %>% group_by(stage) %>% 
  group_modify(~{
    lm(log(rate)~I(hacin/5), data=.x) %>% tidy %>% 
      filter(term!="(Intercept)") %>% 
      mutate(rr=exp(estimate),
             lci=exp(estimate-1.96*std.error),
             uci=exp(estimate+1.96*std.error)) %>% 
      select(rr, lci, uci)
  })


p1<-ggplot(dta, aes(x=years, y=rate)) +
  stat_smooth(method="lm", se=F)+
  geom_point()+
  scale_y_log10()+
  annotation_logticks(sides="l")+
  facet_wrap(~stage, labeller = labeller(stage=labels)) +
  labs(y="Age-adjusted mortality rate per 100,000",
       x="Average years of schooling",
       tag="A")+
  theme_bw() +
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=16, face="bold"),
        plot.title = element_text(color="black", size=16, face="bold"),
        strip.text = element_text(color="black", size=16, face="bold"),
        strip.background = element_blank())
p2<-ggplot(dta, aes(x=hacin, y=rate)) +
  stat_smooth(method="lm", se=F)+
  geom_point()+
  scale_y_log10()+
  annotation_logticks(sides="l")+
  facet_wrap(~stage, labeller = labeller(stage=labels)) +
  labs(y="Age-adjusted mortality rate per 100,000",
       x="% households living in overcrowded situation",
       tag="B")+
  theme_bw() +
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=16, face="bold"),
        plot.title = element_text(color="black", size=16, face="bold"),
        strip.text = element_text(color="black", size=16, face="bold"),
        strip.background = element_blank())
pall<-arrangeGrob(grobs=list(p1, p2), ncol=1)
ggsave("figure.pdf",pall, width=10, height=10)


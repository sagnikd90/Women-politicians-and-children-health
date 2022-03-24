library(dplyr)
library(foreign)
library(ggplot2)
library(Hmisc)
library(plm)
library(broom)
library(jtools)
library(tidyverse)
library(lmtest)

memory.limit(size = 40000)


setwd("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets")


#################################### Andhra Pradesh ###################################

df_AP<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Andhra_Pradesh_2021-11-23.csv")

df_AP<- df_AP%>%
  subset(Year==1989|
           Year==1994|
           Year==1999|
           Year==2004|
           Year==2009|
           Year==2014)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

################################ Assigning district to the constituencies according to 2001 Population Census ###################################


df_AP$District_Name[df_AP$Constituency_Name=="adilabad"
                    |df_AP$Constituency_Name=="armoor"
                    |df_AP$Constituency_Name=="asifabad"
                    |df_AP$Constituency_Name=="balkonda"
                    |df_AP$Constituency_Name=="boath"
                    |df_AP$Constituency_Name=="bodhan"
                    |df_AP$Constituency_Name=="buggaram"
                    |df_AP$Constituency_Name=="chennur"
                    |df_AP$Constituency_Name=="jagtial"
                    |df_AP$Constituency_Name=="khanapur"
                    |df_AP$Constituency_Name=="luxettipet"
                    |df_AP$Constituency_Name=="manthani"
                    |df_AP$Constituency_Name=="metpalli"
                    |df_AP$Constituency_Name=="mudhole"
                    |df_AP$Constituency_Name=="myadaram"
                    |df_AP$Constituency_Name=="nirmal"
                    |df_AP$Constituency_Name=="sirpur" & df_AP$before_delim==1]<- "adilabad"


df_AP$Constituency_Name[df_AP$Constituency_Name=="anatapur"]<- "anantapur"
df_AP$Constituency_Name[df_AP$Constituency_Name=="dharamavaram"|df_AP$Constituency_Name=="dhamavaram"]<- "dharmavaram"
df_AP$Constituency_Name[df_AP$Constituency_Name=="kalyandurg"]<- "kalyandrug"
df_AP$Constituency_Name[df_AP$Constituency_Name=="nallamda"]<- "nallamada"
df_AP$Constituency_Name[df_AP$Constituency_Name=="rayadurg"]<- "rayadrug"
df_AP$Constituency_Name[df_AP$Constituency_Name=="singanmala" & df_AP$before_delim==1]<- "singanamala"

df_AP$District_Name[df_AP$Constituency_Name=="anantapur"
                    |df_AP$Constituency_Name=="dharmavaram"
                    |df_AP$Constituency_Name=="gooty"
                    |df_AP$Constituency_Name=="gorantla"
                    |df_AP$Constituency_Name=="hindpur"
                    |df_AP$Constituency_Name=="kadiri"
                    |df_AP$Constituency_Name=="kalyandrug"
                    |df_AP$Constituency_Name=="madakasira"
                    |df_AP$Constituency_Name=="nallamada"
                    |df_AP$Constituency_Name=="penukonda"
                    |df_AP$Constituency_Name=="singanamala"
                    |df_AP$Constituency_Name=="tadpatri"
                    |df_AP$Constituency_Name=="uravakonda"
                    |df_AP$Constituency_Name=="hindupur"
                    |df_AP$Constituency_Name=="rayadrug" & df_AP$before_delim==1]<- "anantapur"

df_AP$Constituency_Name[df_AP$Constituency_Name=="madanapalle"]<- "madanpalle"
df_AP$Constituency_Name[df_AP$Constituency_Name=="sullurpet"]<- "sulurpet"

df_AP$District_Name[df_AP$Constituency_Name=="chandragiri"
                    |df_AP$Constituency_Name=="chittoor"
                    |df_AP$Constituency_Name=="kuppam"
                    |df_AP$Constituency_Name=="madanpalle"
                    |df_AP$Constituency_Name=="nagari"
                    |df_AP$Constituency_Name=="palmaner"
                    |df_AP$Constituency_Name=="pileru"
                    |df_AP$Constituency_Name=="punganur"
                    |df_AP$Constituency_Name=="puttur"
                    |df_AP$Constituency_Name=="satyavedu"
                    |df_AP$Constituency_Name=="sri kalahasti"
                    |df_AP$Constituency_Name=="sulurpet"
                    |df_AP$Constituency_Name=="thamballapalle"
                    |df_AP$Constituency_Name=="tirupathi"
                    |df_AP$Constituency_Name=="vayalpad"
                    |df_AP$Constituency_Name=="venkatagiri"
                    |df_AP$Constituency_Name=="vepanjeri" & df_AP$before_delim==1]<- "chittoor"

df_AP$Constituency_Name[df_AP$Constituency_Name=="allagahda"]<- "allagada"
df_AP$Constituency_Name[df_AP$Constituency_Name=="jammalahadugu"|df_AP$Constituency_Name=="jammalamdugu"]<- "jammalamadugu"


df_AP$District_Name[df_AP$Constituency_Name=="allagadda"
                    |df_AP$Constituency_Name=="badvel"
                    |df_AP$Constituency_Name=="cuddapah"
                    |df_AP$Constituency_Name=="jammalamadugu"
                    |df_AP$Constituency_Name=="kamalapuram"
                    |df_AP$Constituency_Name=="kodur"
                    |df_AP$Constituency_Name=="lakkireddipalli"
                    |df_AP$Constituency_Name=="mydukur"
                    |df_AP$Constituency_Name=="proddatur"
                    |df_AP$Constituency_Name=="pulivendula"
                    |df_AP$Constituency_Name=="rajampet"
                    |df_AP$Constituency_Name=="rayachoty"
                    |df_AP$Constituency_Name=="cuddapam" & df_AP$before_delim==1]<- "cuddapah"

df_AP$Constituency_Name[df_AP$Constituency_Name=="ramachandramuram"]<- "ramachandrapuram"

df_AP$District_Name[df_AP$Constituency_Name=="achanta"
                    |df_AP$Constituency_Name=="alamuru"
                    |df_AP$Constituency_Name=="allavaram"
                    |df_AP$Constituency_Name=="amalapuram"
                    |df_AP$Constituency_Name=="anaparthy"
                    |df_AP$Constituency_Name=="burugupudi"
                    |df_AP$Constituency_Name=="jaggampeta"
                    |df_AP$Constituency_Name=="kadiam"
                    |df_AP$Constituency_Name=="kothapeta"
                    |df_AP$Constituency_Name=="mummidivaram"
                    |df_AP$Constituency_Name=="nagaram"
                    |df_AP$Constituency_Name=="peddapuram"
                    |df_AP$Constituency_Name=="pithapuram"
                    |df_AP$Constituency_Name=="polavaram"
                    |df_AP$Constituency_Name=="prathipadu"
                    |df_AP$Constituency_Name=="ramachandrapuram"
                    |df_AP$Constituency_Name=="razole"
                    |df_AP$Constituency_Name=="sampara"
                    |df_AP$Constituency_Name=="tallarevu"
                    |df_AP$Constituency_Name=="tuni"
                    |df_AP$Constituency_Name=="yellavaram"
                    |df_AP$Constituency_Name=="kakinada" & df_AP$before_delim==1]<- "east godavari"



df_AP$Constituency_Name[df_AP$Constituency_Name=="chilakuluripet"]<- "chilakaluripet"
df_AP$Constituency_Name[df_AP$Constituency_Name=="guntur i"]<- "guntur-i"
df_AP$Constituency_Name[df_AP$Constituency_Name=="guntur ii"]<- "guntur-ii"
df_AP$Constituency_Name[df_AP$Constituency_Name=="vemuru"]<- "vemur"

df_AP$District_Name[df_AP$Constituency_Name=="bapatla"
                    |df_AP$Constituency_Name=="chilakaluripet"
                    |df_AP$Constituency_Name=="duggirala"
                    |df_AP$Constituency_Name=="guntur-i"
                    |df_AP$Constituency_Name=="guntur-ii"
                    |df_AP$Constituency_Name=="gurzala"
                    |df_AP$Constituency_Name=="kuchinapudi"
                    |df_AP$Constituency_Name=="macherla"
                    |df_AP$Constituency_Name=="mangalagiri"
                    |df_AP$Constituency_Name=="martur"
                    |df_AP$Constituency_Name=="narasaraopet"
                    |df_AP$Constituency_Name=="parchur"
                    |df_AP$Constituency_Name=="peddakurapadu"
                    |df_AP$Constituency_Name=="ponnur"
                    |df_AP$Constituency_Name=="prathipad"
                    |df_AP$Constituency_Name=="repalle"
                    |df_AP$Constituency_Name=="sattenapalli"
                    |df_AP$Constituency_Name=="tadikonda"
                    |df_AP$Constituency_Name=="tenali"
                    |df_AP$Constituency_Name=="vemur"
                    |df_AP$Constituency_Name=="vinukonda" & df_AP$before_delim==1]<- "guntur"


df_AP$Constituency_Name[df_AP$Constituency_Name=="jammalahadugu"]<- "jammalamdugu"
df_AP$Constituency_Name[df_AP$Constituency_Name=="mydaram"]<- "myadaram"
df_AP$Constituency_Name[df_AP$Constituency_Name=="sirchilla"]<- "sircilla"

df_AP$District_Name[df_AP$Constituency_Name=="buggaram"
                    |df_AP$Constituency_Name=="choppadandi"
                    |df_AP$Constituency_Name=="huzurabad"
                    |df_AP$Constituency_Name=="indurthi"
                    |df_AP$Constituency_Name=="jagtial"
                    |df_AP$Constituency_Name=="kamalapur"
                    |df_AP$Constituency_Name=="karimnagar"
                    |df_AP$Constituency_Name=="manthani"
                    |df_AP$Constituency_Name=="metpalli"
                    |df_AP$Constituency_Name=="myadaram"
                    |df_AP$Constituency_Name=="narella"
                    |df_AP$Constituency_Name=="parkal"
                    |df_AP$Constituency_Name=="peddapalli"
                    |df_AP$Constituency_Name=="sircilla" & df_AP$before_delim==1]<- "karimnagar"


df_AP$Constituency_Name[df_AP$Constituency_Name=="bhadrahcalam"]<- "bhadrachalam"

df_AP$District_Name[df_AP$Constituency_Name=="bhadrachalam"
                    |df_AP$Constituency_Name=="burgampahad"
                    |df_AP$Constituency_Name=="khammam"
                    |df_AP$Constituency_Name=="kothagudem"
                    |df_AP$Constituency_Name=="madhira"
                    |df_AP$Constituency_Name=="palair"
                    |df_AP$Constituency_Name=="sathupalli"
                    |df_AP$Constituency_Name=="shujatnagar"
                    |df_AP$Constituency_Name=="yellandu" & df_AP$before_delim==1]<- "khammam"

df_AP$Constituency_Name[df_AP$Constituency_Name=="malleshwaram"]<- "malleswaram"

df_AP$District_Name[df_AP$Constituency_Name=="avanigadda"
                    |df_AP$Constituency_Name=="gannavaram"
                    |df_AP$Constituency_Name=="gudivada"
                    |df_AP$Constituency_Name=="jaggayyapet"
                    |df_AP$Constituency_Name=="kaikalur"
                    |df_AP$Constituency_Name=="kankipadu"
                    |df_AP$Constituency_Name=="malleswaram"
                    |df_AP$Constituency_Name=="mudinepalli"
                    |df_AP$Constituency_Name=="mylavaram"
                    |df_AP$Constituency_Name=="nandigama"
                    |df_AP$Constituency_Name=="nidumolu"
                    |df_AP$Constituency_Name=="nuzvid"
                    |df_AP$Constituency_Name=="tiruvuru"
                    |df_AP$Constituency_Name=="vuyyur"
                    |df_AP$Constituency_Name=="bandar" & df_AP$before_delim==1]<- "krishna"


df_AP$Constituency_Name[df_AP$Constituency_Name=="allagahda"]<- "allagadda"
df_AP$Constituency_Name[df_AP$Constituency_Name=="allagada"]<- "allagadda"

df_AP$District_Name[df_AP$Constituency_Name=="adoni"
                    |df_AP$Constituency_Name=="allagadda"
                    |df_AP$Constituency_Name=="alur"
                    |df_AP$Constituency_Name=="atmakur"
                    |df_AP$Constituency_Name=="dhone"
                    |df_AP$Constituency_Name=="kodumur"
                    |df_AP$Constituency_Name=="koilkuntla"
                    |df_AP$Constituency_Name=="kurnool"
                    |df_AP$Constituency_Name=="nandikotkur"
                    |df_AP$Constituency_Name=="nandyal"
                    |df_AP$Constituency_Name=="panyam"
                    |df_AP$Constituency_Name=="pattikonda"
                    |df_AP$Constituency_Name=="yemmiganur" & df_AP$before_delim==1]<- "kurnool"


df_AP$Constituency_Name[df_AP$Constituency_Name=="wanakarthi"]<- "wanaparthy"

df_AP$District_Name[df_AP$Constituency_Name=="achampet"
                    |df_AP$Constituency_Name=="alampur"
                    |df_AP$Constituency_Name=="amarchinta"
                    |df_AP$Constituency_Name=="gadwal"
                    |df_AP$Constituency_Name=="jadcherla"
                    |df_AP$Constituency_Name=="kalwakurthi"
                    |df_AP$Constituency_Name=="kodangal"
                    |df_AP$Constituency_Name=="kollapur"
                    |df_AP$Constituency_Name=="mahbubnagar"
                    |df_AP$Constituency_Name=="makthal"
                    |df_AP$Constituency_Name=="nagarkurnool"
                    |df_AP$Constituency_Name=="shadnagar"
                    |df_AP$Constituency_Name=="wanaparthy" & df_AP$before_delim==1]<- "mahbubnagar"


df_AP$Constituency_Name[df_AP$Constituency_Name=="ramayanpet"]<- "ramayampet"
df_AP$Constituency_Name[df_AP$Constituency_Name=="siddepet"]<- "siddipet"

df_AP$District_Name[df_AP$Constituency_Name=="andole"
                    |df_AP$Constituency_Name=="bhongir"
                    |df_AP$Constituency_Name=="cheriyal"
                    |df_AP$Constituency_Name=="chevella"
                    |df_AP$Constituency_Name=="dommat"
                    |df_AP$Constituency_Name=="gajwel"
                    |df_AP$Constituency_Name=="kamareddy"
                    |df_AP$Constituency_Name=="medak"
                    |df_AP$Constituency_Name=="narayankhed"
                    |df_AP$Constituency_Name=="narella"
                    |df_AP$Constituency_Name=="narsapur"
                    |df_AP$Constituency_Name=="ramayampet"
                    |df_AP$Constituency_Name=="sangareddy"
                    |df_AP$Constituency_Name=="siddipet"
                    |df_AP$Constituency_Name=="zahirabad" & df_AP$before_delim==1]<- "medak"


df_AP$Constituency_Name[df_AP$Constituency_Name=="ramanapet"]<- "ramannapet"

df_AP$District_Name[df_AP$Constituency_Name=="alair"
                    |df_AP$Constituency_Name=="bhongir"
                    |df_AP$Constituency_Name=="chalakurthi"
                    |df_AP$Constituency_Name=="deverkonda"
                    |df_AP$Constituency_Name=="kodad"
                    |df_AP$Constituency_Name=="miryalguda"
                    |df_AP$Constituency_Name=="mungode"
                    |df_AP$Constituency_Name=="nakrekal"
                    |df_AP$Constituency_Name=="nalgonda"
                    |df_AP$Constituency_Name=="ramannapet"
                    |df_AP$Constituency_Name=="suryapet"
                    |df_AP$Constituency_Name=="tungaturthi" & df_AP$before_delim==1]<- "nalgonda"


df_AP$Constituency_Name[df_AP$Constituency_Name=="sullurpet"]<- "sulurpet"

df_AP$District_Name[df_AP$Constituency_Name=="allur"
                    |df_AP$Constituency_Name=="atmakur"
                    |df_AP$Constituency_Name=="gudur"
                    |df_AP$Constituency_Name=="kavali"
                    |df_AP$Constituency_Name=="kovur"
                    |df_AP$Constituency_Name=="nellore"
                    |df_AP$Constituency_Name=="rapur"
                    |df_AP$Constituency_Name=="sarvepalli"
                    |df_AP$Constituency_Name=="satyavedu"
                    |df_AP$Constituency_Name=="sulurpet"
                    |df_AP$Constituency_Name=="udayagiri"
                    |df_AP$Constituency_Name=="venkatagiri" & df_AP$before_delim==1]<- "nellore"


df_AP$Constituency_Name[df_AP$Constituency_Name=="dichpally"]<- "dichpalli"

df_AP$District_Name[df_AP$Constituency_Name=="armoor"
                    |df_AP$Constituency_Name=="balkonda"
                    |df_AP$Constituency_Name=="banswada"
                    |df_AP$Constituency_Name=="bodhan"
                    |df_AP$Constituency_Name=="dichpalli"
                    |df_AP$Constituency_Name=="jukkal"
                    |df_AP$Constituency_Name=="kamareddy"
                    |df_AP$Constituency_Name=="nizamabad"
                    |df_AP$Constituency_Name=="yellareddy" & df_AP$before_delim==1]<- "nizamabad"


df_AP$Constituency_Name[df_AP$Constituency_Name=="santhanuthalcapadu"]<- "santhanuthalapadu"
df_AP$Constituency_Name[df_AP$Constituency_Name=="kondapi"]<- "kondepi"

df_AP$District_Name[df_AP$Constituency_Name=="addanki"
                    |df_AP$Constituency_Name=="cumbum"
                    |df_AP$Constituency_Name=="darsi"
                    |df_AP$Constituency_Name=="giddalur"
                    |df_AP$Constituency_Name=="kandukur"
                    |df_AP$Constituency_Name=="martur"
                    |df_AP$Constituency_Name=="ongole"
                    |df_AP$Constituency_Name=="parchur"
                    |df_AP$Constituency_Name=="santhanuthalapadu"
                    |df_AP$Constituency_Name=="chirala"
                    |df_AP$Constituency_Name=="kanigiri"
                    |df_AP$Constituency_Name=="kondepi"
                    |df_AP$Constituency_Name=="markapur" & df_AP$before_delim==1]<- "prakasam"


df_AP$District_Name[df_AP$Constituency_Name=="chevella"
                    |df_AP$Constituency_Name=="ibrahimpatnam"
                    |df_AP$Constituency_Name=="malakpet"
                    |df_AP$Constituency_Name=="medchal"
                    |df_AP$Constituency_Name=="pargi"
                    |df_AP$Constituency_Name=="tandur"
                    |df_AP$Constituency_Name=="vicarabad"
                    |df_AP$Constituency_Name=="asafnagar"
                    |df_AP$Constituency_Name=="chandrayangutta"
                    |df_AP$Constituency_Name=="charminar"
                    |df_AP$Constituency_Name=="himayatnagar"
                    |df_AP$Constituency_Name=="yakutpura" & df_AP$before_delim==1]<- "rangareddi"


df_AP$Constituency_Name[df_AP$Constituency_Name=="harischandrapuram"|df_AP$Constituency_Name=="harishchandra"]<- "harishchandra puram"
df_AP$Constituency_Name[df_AP$Constituency_Name=="pallikonda"]<- "palakonda"
df_AP$Constituency_Name[df_AP$Constituency_Name=="pathapatnam"]<- "patapatnam"

df_AP$District_Name[df_AP$Constituency_Name=="amadalavalasa"
                    |df_AP$Constituency_Name=="bhogapuram"
                    |df_AP$Constituency_Name=="cheepurupalli"
                    |df_AP$Constituency_Name=="etcherla"
                    |df_AP$Constituency_Name=="harishchandra puram"
                    |df_AP$Constituency_Name=="ichapuram"
                    |df_AP$Constituency_Name=="kothuru"
                    |df_AP$Constituency_Name=="narasannapeta"
                    |df_AP$Constituency_Name=="palakonda"
                    |df_AP$Constituency_Name=="patapatnam"
                    |df_AP$Constituency_Name=="sompeta"
                    |df_AP$Constituency_Name=="srikakulam"
                    |df_AP$Constituency_Name=="tekkali"
                    |df_AP$Constituency_Name=="vunukuru" & df_AP$before_delim==1]<- "srikakulam"


df_AP$Constituency_Name[df_AP$Constituency_Name=="bhemunipatnam"]<- "bheemunipatnam"
df_AP$Constituency_Name[df_AP$Constituency_Name=="narasipatnam"]<- "narsipatnam"
df_AP$Constituency_Name[df_AP$Constituency_Name=="visakhapatnam i"]<- "visakhapatnam-i"

df_AP$District_Name[df_AP$Constituency_Name=="anakapalli"
                    |df_AP$Constituency_Name=="bheemunipatnam"
                    |df_AP$Constituency_Name=="chintapalli"
                    |df_AP$Constituency_Name=="chodavaram"
                    |df_AP$Constituency_Name=="elamanchili"
                    |df_AP$Constituency_Name=="gajapathinagaram"
                    |df_AP$Constituency_Name=="madugula"
                    |df_AP$Constituency_Name=="narsipatnam"
                    |df_AP$Constituency_Name=="paderu"
                    |df_AP$Constituency_Name=="paravada"
                    |df_AP$Constituency_Name=="payakaraopeta"
                    |df_AP$Constituency_Name=="pendurthi"
                    |df_AP$Constituency_Name=="srungavarapukota"
                    |df_AP$Constituency_Name=="uttarapalli"
                    |df_AP$Constituency_Name=="visakhapatnam-i" & df_AP$before_delim==1]<- "visakhapatnam"


df_AP$Constituency_Name[df_AP$Constituency_Name=="kotturu"]<- "kothuru"
df_AP$Constituency_Name[df_AP$Constituency_Name=="vtzianagaram"]<- "vizianagaram"

df_AP$District_Name[df_AP$Constituency_Name=="bheemunipatnam"
                    |df_AP$Constituency_Name=="bhogapuram"
                    |df_AP$Constituency_Name=="bobbili"
                    |df_AP$Constituency_Name=="cheepurupalli"
                    |df_AP$Constituency_Name=="gajapathinagaram"
                    |df_AP$Constituency_Name=="kothuru"
                    |df_AP$Constituency_Name=="naguru"
                    |df_AP$Constituency_Name=="parvathipuram"
                    |df_AP$Constituency_Name=="salur"
                    |df_AP$Constituency_Name=="sathivada"
                    |df_AP$Constituency_Name=="srungavarapukota"
                    |df_AP$Constituency_Name=="therlam"
                    |df_AP$Constituency_Name=="uttarapalli"
                    |df_AP$Constituency_Name=="vizianagaram"
                    |df_AP$Constituency_Name=="vunukuru" & df_AP$before_delim==1]<- "vizianagaram"


df_AP$Constituency_Name[df_AP$Constituency_Name=="mahabubabad"]<- "mahbubabad"
df_AP$Constituency_Name[df_AP$Constituency_Name=="chinnur"]<- "chennur"

df_AP$District_Name[df_AP$Constituency_Name=="chennur"
                    |df_AP$Constituency_Name=="cheriyal"
                    |df_AP$Constituency_Name=="dornakal"
                    |df_AP$Constituency_Name=="ghanpur"
                    |df_AP$Constituency_Name=="hanamkonda"
                    |df_AP$Constituency_Name=="jangaon"
                    |df_AP$Constituency_Name=="mahbubabad"
                    |df_AP$Constituency_Name=="mulug"
                    |df_AP$Constituency_Name=="narsampet"
                    |df_AP$Constituency_Name=="parkal"
                    |df_AP$Constituency_Name=="shyampet"
                    |df_AP$Constituency_Name=="warangal"
                    |df_AP$Constituency_Name=="wardhannapet" & df_AP$before_delim==1]<- "warangal"


df_AP$Constituency_Name[df_AP$Constituency_Name=="gopalapuram"]<- "gopalpuram"

df_AP$District_Name[df_AP$Constituency_Name=="achanta"
                    |df_AP$Constituency_Name=="attili"
                    |df_AP$Constituency_Name=="bhimavaram"
                    |df_AP$Constituency_Name=="chintalapudi"
                    |df_AP$Constituency_Name=="dendulur"
                    |df_AP$Constituency_Name=="gopalpuram"
                    |df_AP$Constituency_Name=="kovvur"
                    |df_AP$Constituency_Name=="narasapur"
                    |df_AP$Constituency_Name=="palacole"
                    |df_AP$Constituency_Name=="penugonda"
                    |df_AP$Constituency_Name=="polavaram"
                    |df_AP$Constituency_Name=="tadepalligudem"
                    |df_AP$Constituency_Name=="tanuku"
                    |df_AP$Constituency_Name=="undi"
                    |df_AP$Constituency_Name=="ungutur"
                    |df_AP$Constituency_Name=="eluru" & df_AP$before_delim==1]<- "west godavari"

df_AP<- df_AP%>%
  subset(District_Name!="")

################################ Arunachal Pradesh ##############################################

df_AR<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Arunachal_Pradesh_2021-11-23.csv")

df_AR<- df_AR%>%
  subset(Year==1990|
           Year==1995|
           Year==1999|
           Year==2004|
           Year==2009|
           Year==2014)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_AR$Constituency_Name[df_AR$Constituency_Name=="bordumsa- diyum"]<- "bordumsa-diyum"

df_AR$District_Name[df_AR$Constituency_Name=="bordumsa-diyum"
                    |df_AR$Constituency_Name=="changlang north"
                    |df_AR$Constituency_Name=="changlang south"
                    |df_AR$Constituency_Name=="khonsa west"
                    |df_AR$Constituency_Name=="lekang"
                    |df_AR$Constituency_Name=="miao"
                    |df_AR$Constituency_Name=="nampong" & df_AR$before_delim==0]<- "changlang"

df_AR$District_Name[df_AR$Constituency_Name=="anini"
                    |df_AR$Constituency_Name=="dambuk"
                    |df_AR$Constituency_Name=="roing" & df_AR$before_delim==0]<- "dibang valley"

df_AR$Constituency_Name[df_AR$Constituency_Name=="chayang tajo"]<- "chayangtajo"

df_AR$District_Name[df_AR$Constituency_Name=="bameng"
                    |df_AR$Constituency_Name=="chayangtajo"
                    |df_AR$Constituency_Name=="pakke-kasang"
                    |df_AR$Constituency_Name=="seppa east"
                    |df_AR$Constituency_Name=="seppa west" & df_AR$before_delim==0]<- "east kameng"

df_AR$Constituency_Name[df_AR$Constituency_Name==""]<- ""

df_AR$District_Name[df_AR$Constituency_Name=="mebo"
                    |df_AR$Constituency_Name=="nari-koyu"
                    |df_AR$Constituency_Name=="pangin"
                    |df_AR$Constituency_Name=="pasighat east"
                    |df_AR$Constituency_Name=="pasighat west" & df_AR$before_delim==0]<- "east siang"

df_AR$Constituency_Name[df_AR$Constituency_Name=="hauyuliang"]<- "hayuliang"

df_AR$District_Name[df_AR$Constituency_Name=="chowkham"
                    |df_AR$Constituency_Name=="hayuliang"
                    |df_AR$Constituency_Name=="lekang"
                    |df_AR$Constituency_Name=="namsai"
                    |df_AR$Constituency_Name=="tezu" & df_AR$before_delim==0]<- "lohit"

df_AR$District_Name[df_AR$Constituency_Name=="kaloriang"
                    |df_AR$Constituency_Name=="nyapin"
                    |df_AR$Constituency_Name=="palin"
                    |df_AR$Constituency_Name=="raga"
                    |df_AR$Constituency_Name=="sagalee"
                    |df_AR$Constituency_Name=="tali"
                    |df_AR$Constituency_Name=="yachuli"
                    |df_AR$Constituency_Name=="ziro-hapoli" & df_AR$before_delim==0]<- "lower subansiri"

df_AR$Constituency_Name[df_AR$Constituency_Name==""]<- ""

df_AR$District_Name[df_AR$Constituency_Name=="doimukh"
                    |df_AR$Constituency_Name=="itanagar"
                    |df_AR$Constituency_Name=="sagalee" & df_AR$before_delim==0]<- "papum pare"

df_AR$District_Name[df_AR$Constituency_Name=="lumla"
                    |df_AR$Constituency_Name=="mukto"
                    |df_AR$Constituency_Name=="tawang" & df_AR$before_delim==0]<- "tawang"

df_AR$Constituency_Name[df_AR$Constituency_Name=="pongchau wakka"
                        |df_AR$Constituency_Name=="pongchou-wakka"
                        |df_AR$Constituency_Name=="pongchou wakka"]<- "pongchao-wakka"

df_AR$District_Name[df_AR$Constituency_Name=="borduria- bagapani"
                    |df_AR$Constituency_Name=="kanubari"
                    |df_AR$Constituency_Name=="khonsa east"
                    |df_AR$Constituency_Name=="khonsa west"
                    |df_AR$Constituency_Name=="longding-pumao"
                    |df_AR$Constituency_Name=="namsang"
                    |df_AR$Constituency_Name=="pongchao-wakka" & df_AR$before_delim==0]<- "tirap"


df_AR$Constituency_Name[df_AR$Constituency_Name=="tuting-yinkgkiong"]<- "tuting-yingkiong"

df_AR$District_Name[df_AR$Constituency_Name=="mariyang-geku"
                    |df_AR$Constituency_Name=="tuting-yingkiong" & df_AR$before_delim==0]<- "upper siang"

df_AR$District_Name[df_AR$Constituency_Name=="damporijo"
                    |df_AR$Constituency_Name=="daporijo"
                    |df_AR$Constituency_Name=="nacho"
                    |df_AR$Constituency_Name=="raga"
                    |df_AR$Constituency_Name=="taliha" & df_AR$before_delim==0]<- "upper subansiri"

df_AR$District_Name[df_AR$Constituency_Name=="bomdila"
                    |df_AR$Constituency_Name=="dirang"
                    |df_AR$Constituency_Name=="kalaktang"
                    |df_AR$Constituency_Name=="thrizino-buragaon" & df_AR$before_delim==0]<- "west kameng"

df_AR$District_Name[df_AR$Constituency_Name=="along east"
                    |df_AR$Constituency_Name=="along west"
                    |df_AR$Constituency_Name=="likabali"
                    |df_AR$Constituency_Name=="liromoba"
                    |df_AR$Constituency_Name=="mechuka"
                    |df_AR$Constituency_Name=="rumgong" & df_AR$before_delim==0]<- "west siang"

df_AR<- df_AR%>%
  subset(District_Name!="")

######################## Assam ############################

df_AS<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Assam_2021-11-23.csv")

df_AS<- df_AS%>%
  subset(Year==1991|
           Year==1996|
           Year==2001|
           Year==2006|
           Year==2011|
           Year==2016)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_AS$District_Name[df_AS$Constituency_Name=="baghbar"
                    |df_AS$Constituency_Name=="barpeta"
                    |df_AS$Constituency_Name=="bhabanipur"
                    |df_AS$Constituency_Name=="chapaguri"
                    |df_AS$Constituency_Name=="chenga"
                    |df_AS$Constituency_Name=="jania"
                    |df_AS$Constituency_Name=="patacharkuchi"
                    |df_AS$Constituency_Name=="sarukhetri"
                    |df_AS$Constituency_Name=="sorbhog" & df_AS$before_delim==1]<- "barpeta"

df_AS$District_Name[df_AS$Constituency_Name=="abhayapuri north"
                    |df_AS$Constituency_Name=="abhayapuri south"
                    |df_AS$Constituency_Name=="bijni"
                    |df_AS$Constituency_Name=="bongaigaon"
                    |df_AS$Constituency_Name=="sidli"
                    |df_AS$Constituency_Name=="sorbhog" & df_AS$before_delim==1]<- "bongaigaon"

df_AS$District_Name[df_AS$Constituency_Name=="algapur"
                    |df_AS$Constituency_Name=="barkhola"
                    |df_AS$Constituency_Name=="dholai"
                    |df_AS$Constituency_Name=="katigora"
                    |df_AS$Constituency_Name=="lakhipur"
                    |df_AS$Constituency_Name=="sonai"
                    |df_AS$Constituency_Name=="udharbond"
                    |df_AS$Constituency_Name=="silchar" & df_AS$before_delim==1]<- "cachar"

df_AS$Constituency_Name[df_AS$Constituency_Name=="udalaguri"]<- "udalguri"

df_AS$District_Name[df_AS$Constituency_Name=="dalgaon"
                    |df_AS$Constituency_Name=="kalaigaon"
                    |df_AS$Constituency_Name=="majbat"
                    |df_AS$Constituency_Name=="mangaldoi"
                    |df_AS$Constituency_Name=="panery"
                    |df_AS$Constituency_Name=="sipajhar"
                    |df_AS$Constituency_Name=="udalguri" & df_AS$before_delim==1]<- "darrang"

df_AS$Constituency_Name[df_AS$Constituency_Name=="dibrugarh(adjournedpoll)"]<- "dibrugarh"

df_AS$District_Name[df_AS$Constituency_Name=="dhemaji"
                    |df_AS$Constituency_Name=="jonai"
                    |df_AS$Constituency_Name=="moran"
                    |df_AS$Constituency_Name=="dibrugarh" & df_AS$before_delim==1]<- "dhemaji"

df_AS$District_Name[df_AS$Constituency_Name=="bilasipara east"
                    |df_AS$Constituency_Name=="bilasipara west"
                    |df_AS$Constituency_Name=="dhubri"
                    |df_AS$Constituency_Name=="gauripur"
                    |df_AS$Constituency_Name=="golakganj"
                    |df_AS$Constituency_Name=="gossaigaon"
                    |df_AS$Constituency_Name=="kokrajhar east"
                    |df_AS$Constituency_Name=="kokrajhar west"
                    |df_AS$Constituency_Name=="manakachar"
                    |df_AS$Constituency_Name=="salmara south"
                    |df_AS$Constituency_Name=="mankachar" & df_AS$before_delim==1]<- "dhubri"

df_AS$District_Name[df_AS$Constituency_Name=="chabua"
                    |df_AS$Constituency_Name=="duliajan"
                    |df_AS$Constituency_Name=="lahowal"
                    |df_AS$Constituency_Name=="mahmara"
                    |df_AS$Constituency_Name=="moran"
                    |df_AS$Constituency_Name=="naharkatia"
                    |df_AS$Constituency_Name=="tingkhong" & df_AS$before_delim==1]<- "dibrugarh"

df_AS$Constituency_Name[df_AS$Constituency_Name=="dudnai"]<- "dudhnai"

df_AS$District_Name[df_AS$Constituency_Name=="dudhnai"
                    |df_AS$Constituency_Name=="goalpara east"
                    |df_AS$Constituency_Name=="goalpara west"
                    |df_AS$Constituency_Name=="jaleswar" & df_AS$before_delim==1]<- "goalpara"

df_AS$Constituency_Name[df_AS$Constituency_Name=="bokahar"|df_AS$Constituency_Name=="bokahat"]<- "bokakhat"

df_AS$District_Name[df_AS$Constituency_Name=="bokajan"
                    |df_AS$Constituency_Name=="bokakhat"
                    |df_AS$Constituency_Name=="golaghat"
                    |df_AS$Constituency_Name=="khumtai"
                    |df_AS$Constituency_Name=="sarupathar" & df_AS$before_delim==1]<- "golaghat"

df_AS$District_Name[df_AS$Constituency_Name=="algapur"
                    |df_AS$Constituency_Name=="hailakandi"
                    |df_AS$Constituency_Name=="katlicherra" & df_AS$before_delim==1]<- "hailakandi"

df_AS$Constituency_Name[df_AS$Constituency_Name=="titabar(sc)"]<- "titabar"

df_AS$District_Name[df_AS$Constituency_Name=="dergaon"
                    |df_AS$Constituency_Name=="jorhat"
                    |df_AS$Constituency_Name=="majuli"
                    |df_AS$Constituency_Name=="mariani"
                    |df_AS$Constituency_Name=="teok"
                    |df_AS$Constituency_Name=="titabar" & df_AS$before_delim==1]<- "jorhat"

df_AS$District_Name[df_AS$Constituency_Name=="boko"
                    |df_AS$Constituency_Name=="chaygaon"
                    |df_AS$Constituency_Name=="dispur"
                    |df_AS$Constituency_Name=="hajo"
                    |df_AS$Constituency_Name=="kamalpur"
                    |df_AS$Constituency_Name=="palasbari"
                    |df_AS$Constituency_Name=="rangiya"
                    |df_AS$Constituency_Name=="sipajhar"
                    |df_AS$Constituency_Name=="gauhati east"
                    |df_AS$Constituency_Name=="gauhati west"
                    |df_AS$Constituency_Name=="jalukbari" & df_AS$before_delim==1]<- "kamrup"

df_AS$District_Name[df_AS$Constituency_Name=="baithalangso"
                    |df_AS$Constituency_Name=="bokajan"
                    |df_AS$Constituency_Name=="diphu"
                    |df_AS$Constituency_Name=="howraghat"
                    |df_AS$Constituency_Name=="lumding" & df_AS$before_delim==1]<- "karbi anglong"

df_AS$Constituency_Name[df_AS$Constituency_Name=="patharakandi"]<- "patharkandi"

df_AS$District_Name[df_AS$Constituency_Name=="badarpur"
                    |df_AS$Constituency_Name=="karimganj north"
                    |df_AS$Constituency_Name=="karimganj south"
                    |df_AS$Constituency_Name=="patharkandi"
                    |df_AS$Constituency_Name=="ratabari" & df_AS$before_delim==1]<- "karimganj"

df_AS$District_Name[df_AS$Constituency_Name=="gossaigaon"
                    |df_AS$Constituency_Name=="kokrajhar east"
                    |df_AS$Constituency_Name=="kokrajhar west"
                    |df_AS$Constituency_Name=="sidli" & df_AS$before_delim==1]<- "kokrajhar"

df_AS$Constituency_Name[df_AS$Constituency_Name=="dhakukhana"]<- "dhakuakhana"

df_AS$District_Name[df_AS$Constituency_Name=="bihpuria"
                    |df_AS$Constituency_Name=="dhakuakhana"
                    |df_AS$Constituency_Name=="lakhimpur"
                    |df_AS$Constituency_Name=="naoboicha" & df_AS$before_delim==1]<- "lakhimpur"

df_AS$District_Name[df_AS$Constituency_Name=="jagiroad"
                    |df_AS$Constituency_Name=="laharighat"
                    |df_AS$Constituency_Name=="marigaon" & df_AS$before_delim==1]<- "marigaon"

df_AS$District_Name[df_AS$Constituency_Name=="barhampur"
                    |df_AS$Constituency_Name=="dhing"
                    |df_AS$Constituency_Name=="hojai"
                    |df_AS$Constituency_Name=="jamunamukh"
                    |df_AS$Constituency_Name=="kaliabor"
                    |df_AS$Constituency_Name=="lumding"
                    |df_AS$Constituency_Name=="nowgong"
                    |df_AS$Constituency_Name=="raha"
                    |df_AS$Constituency_Name=="rupohihat"
                    |df_AS$Constituency_Name=="samaguri"
                    |df_AS$Constituency_Name=="batadroba" & df_AS$before_delim==1]<- "nagaon"

df_AS$District_Name[df_AS$Constituency_Name=="barama"
                    |df_AS$Constituency_Name=="barkhetry"
                    |df_AS$Constituency_Name=="chapaguri"
                    |df_AS$Constituency_Name=="dharmapur"
                    |df_AS$Constituency_Name=="nalbari"
                    |df_AS$Constituency_Name=="tamulpur" & df_AS$before_delim==1]<- "nalbari"

df_AS$District_Name[df_AS$Constituency_Name=="haflong" & df_AS$before_delim==1]<- "north cachar hills"

df_AS$District_Name[df_AS$Constituency_Name=="amguri"
                    |df_AS$Constituency_Name=="mahmara"
                    |df_AS$Constituency_Name=="mariani"
                    |df_AS$Constituency_Name=="nazira"
                    |df_AS$Constituency_Name=="sibsagar"
                    |df_AS$Constituency_Name=="sonari"
                    |df_AS$Constituency_Name=="thowra" & df_AS$before_delim==1]<- "sibsagar"

df_AS$District_Name[df_AS$Constituency_Name=="barchalla"
                    |df_AS$Constituency_Name=="behali"
                    |df_AS$Constituency_Name=="biswanath"
                    |df_AS$Constituency_Name=="dhekiajuli"
                    |df_AS$Constituency_Name=="gohpur"
                    |df_AS$Constituency_Name=="rangapara"
                    |df_AS$Constituency_Name=="sootea"
                    |df_AS$Constituency_Name=="tezpur" & df_AS$before_delim==1]<- "sonitpur"

df_AS$District_Name[df_AS$Constituency_Name=="chabua"
                    |df_AS$Constituency_Name=="doom dooma"
                    |df_AS$Constituency_Name=="margherita"
                    |df_AS$Constituency_Name=="tinsukia"
                    |df_AS$Constituency_Name=="digboi" & df_AS$before_delim==1]<- "tinsukia"


df_AS<- df_AS%>%
  subset(District_Name!="")


########################### Bihar ###################################

df_BR<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Bihar_2021-11-23.csv")

df_BR<- df_BR%>%
  subset(Year==1990|
           Year==1995|
           Year==2000|
           Year==2005|
           Year==2010|
           Year==2015)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_BR$District_Name[df_BR$Constituency_Name=="araria"
                    |df_BR$Constituency_Name=="forbesganj"
                    |df_BR$Constituency_Name=="jokihat"
                    |df_BR$Constituency_Name=="narpatganj"
                    |df_BR$Constituency_Name=="raniganj"
                    |df_BR$Constituency_Name=="sikti" & df_BR$before_delim==1]<- "araria"

df_BR$District_Name[df_BR$Constituency_Name=="aurangabad"
                    |df_BR$Constituency_Name=="deo"
                    |df_BR$Constituency_Name=="goh"
                    |df_BR$Constituency_Name=="nabinagar"
                    |df_BR$Constituency_Name=="obra"
                    |df_BR$Constituency_Name=="rafiganj" & df_BR$before_delim==1]<- "aurangabad"

df_BR$Constituency_Name[df_BR$Constituency_Name=="poraiyahat"]<- "poreyahat"

df_BR$District_Name[df_BR$Constituency_Name=="amarpur"
                    |df_BR$Constituency_Name=="banka"
                    |df_BR$Constituency_Name=="belhar"
                    |df_BR$Constituency_Name=="dhuraiya"
                    |df_BR$Constituency_Name=="katoria"
                    |df_BR$Constituency_Name=="poreyahat" & df_BR$before_delim==1]<- "banka"

df_BR$Constituency_Name[df_BR$Constituency_Name=="bachhwara"]<- "bachwara"
df_BR$Constituency_Name[df_BR$Constituency_Name=="maithani"]<- "matihani"

df_BR$District_Name[df_BR$Constituency_Name=="bachwara"
                    |df_BR$Constituency_Name=="bakhri"
                    |df_BR$Constituency_Name=="balia"
                    |df_BR$Constituency_Name=="barauni"
                    |df_BR$Constituency_Name=="begusarai"
                    |df_BR$Constituency_Name=="cheria bariarpur"
                    |df_BR$Constituency_Name=="matihani" & df_BR$before_delim==1]<- "begusarai"

df_BR$District_Name[df_BR$Constituency_Name=="bhagalpur"
                    |df_BR$Constituency_Name=="bihpur"
                    |df_BR$Constituency_Name=="colgong"
                    |df_BR$Constituency_Name=="gopalpur"
                    |df_BR$Constituency_Name=="nathnagar"
                    |df_BR$Constituency_Name=="pirpainti"
                    |df_BR$Constituency_Name=="sultanganj"
                    |df_BR$Constituency_Name=="mahagama" & df_BR$before_delim==1]<- "bhagalpur"

df_BR$Constituency_Name[df_BR$Constituency_Name=="barharia"]<- "barhara"

df_BR$District_Name[df_BR$Constituency_Name=="arrah"
                    |df_BR$Constituency_Name=="barhara"
                    |df_BR$Constituency_Name=="jagdishpur"
                    |df_BR$Constituency_Name=="piro"
                    |df_BR$Constituency_Name=="sahar"
                    |df_BR$Constituency_Name=="sandesh"
                    |df_BR$Constituency_Name=="shahpur" & df_BR$before_delim==1]<- "bhojpur"

df_BR$Constituency_Name[df_BR$Constituency_Name=="brahampur"]<- "brahmpur"

df_BR$District_Name[df_BR$Constituency_Name=="brahmpur"
                    |df_BR$Constituency_Name=="buxar"
                    |df_BR$Constituency_Name=="dumraon"
                    |df_BR$Constituency_Name=="jagdishpur"
                    |df_BR$Constituency_Name=="jagdismpur"
                    |df_BR$Constituency_Name=="rajpur" & df_BR$before_delim==1]<- "buxar"

df_BR$Constituency_Name[df_BR$Constituency_Name=="hayagaht"]<- "hayaghat"

df_BR$District_Name[df_BR$Constituency_Name=="bahera"
                    |df_BR$Constituency_Name=="baheri"
                    |df_BR$Constituency_Name=="darbhanga"
                    |df_BR$Constituency_Name=="darbhanga rural"
                    |df_BR$Constituency_Name=="ghanshyampur"
                    |df_BR$Constituency_Name=="hayaghat"
                    |df_BR$Constituency_Name=="jale"
                    |df_BR$Constituency_Name=="keoti"
                    |df_BR$Constituency_Name=="manigachhi"
                    |df_BR$Constituency_Name=="singhia" & df_BR$before_delim==1]<- "darbhanga"

df_BR$Constituency_Name[df_BR$Constituency_Name=="gaya muffasil"]<- "gaya mufassil"
df_BR$Constituency_Name[df_BR$Constituency_Name=="ghoshi"]<- "ghosi"

df_BR$District_Name[df_BR$Constituency_Name=="atri"
                    |df_BR$Constituency_Name=="barachatti"
                    |df_BR$Constituency_Name=="belaganj"
                    |df_BR$Constituency_Name=="bodh gaya"
                    |df_BR$Constituency_Name=="fatehpur"
                    |df_BR$Constituency_Name=="gaya mufassil"
                    |df_BR$Constituency_Name=="gaya town"
                    |df_BR$Constituency_Name=="ghosi"
                    |df_BR$Constituency_Name=="gurua"
                    |df_BR$Constituency_Name=="imamganj"
                    |df_BR$Constituency_Name=="konch"
                    |df_BR$Constituency_Name=="hussainabad"
                    |df_BR$Constituency_Name=="panki" & df_BR$before_delim==1]<- "gaya"

df_BR$Constituency_Name[df_BR$Constituency_Name=="gopal ganj"]<- "gopalganj"

df_BR$District_Name[df_BR$Constituency_Name=="baikunthpur"
                    |df_BR$Constituency_Name=="barauli"
                    |df_BR$Constituency_Name=="bhore"
                    |df_BR$Constituency_Name=="gopalganj"
                    |df_BR$Constituency_Name=="kateya" & df_BR$before_delim==1]<- "gopalganj"

df_BR$District_Name[df_BR$Constituency_Name=="chakai"
                    |df_BR$Constituency_Name=="jamui"
                    |df_BR$Constituency_Name=="jhajha"
                    |df_BR$Constituency_Name=="kharagpur"
                    |df_BR$Constituency_Name=="sikandra" & df_BR$before_delim==1]<- "jamui"

df_BR$Constituency_Name[df_BR$Constituency_Name=="ghoshi"]<- "ghosi"

df_BR$District_Name[df_BR$Constituency_Name=="arwal"
                    |df_BR$Constituency_Name=="ghosi"
                    |df_BR$Constituency_Name=="jahanabad"
                    |df_BR$Constituency_Name=="kurtha"
                    |df_BR$Constituency_Name=="makhdumpur" & df_BR$before_delim==1]<- "jehanabad"

df_BR$District_Name[df_BR$Constituency_Name=="bhabhua"
                    |df_BR$Constituency_Name=="chainpur"
                    |df_BR$Constituency_Name=="chenari"
                    |df_BR$Constituency_Name=="mohania"
                    |df_BR$Constituency_Name=="ramgarh" & df_BR$before_delim==1]<- "kaimur (bhabua)"

df_BR$Constituency_Name[df_BR$Constituency_Name=="barso"]<- "barsoi"

df_BR$District_Name[df_BR$Constituency_Name=="barari"
                    |df_BR$Constituency_Name=="barsoi"
                    |df_BR$Constituency_Name=="kadwa"
                    |df_BR$Constituency_Name=="katihar"
                    |df_BR$Constituency_Name=="korha"
                    |df_BR$Constituency_Name=="manihari"
                    |df_BR$Constituency_Name=="pranpur" & df_BR$before_delim==1]<- "katihar"

df_BR$Constituency_Name[df_BR$Constituency_Name=="alouli"]<- "alauli"

df_BR$District_Name[df_BR$Constituency_Name=="alauli"
                    |df_BR$Constituency_Name=="chautham"
                    |df_BR$Constituency_Name=="khagaria"
                    |df_BR$Constituency_Name=="parbatta" & df_BR$before_delim==1]<- "khagaria"

df_BR$District_Name[df_BR$Constituency_Name=="bahadurganj"
                    |df_BR$Constituency_Name=="kishanganj"
                    |df_BR$Constituency_Name=="sikti"
                    |df_BR$Constituency_Name=="thakurganj" & df_BR$before_delim==1]<- "kishanganj"

df_BR$District_Name[df_BR$Constituency_Name=="lakhisarai"
                    |df_BR$Constituency_Name=="sheikhpura"
                    |df_BR$Constituency_Name=="surajgarha" & df_BR$before_delim==1]<- "lakhisarai"

df_BR$District_Name[df_BR$Constituency_Name=="alamnagar"
                    |df_BR$Constituency_Name=="kumarkhand"
                    |df_BR$Constituency_Name=="madhepura"
                    |df_BR$Constituency_Name=="singheshwar" & df_BR$before_delim==1]<- "madhepura"

df_BR$Constituency_Name[df_BR$Constituency_Name=="harlakmi"]<- "harlakhi"

df_BR$District_Name[df_BR$Constituency_Name=="babubarhi"
                    |df_BR$Constituency_Name=="benipatti"
                    |df_BR$Constituency_Name=="bisfi"
                    |df_BR$Constituency_Name=="harlakhi"
                    |df_BR$Constituency_Name=="jhanjharpur"
                    |df_BR$Constituency_Name=="khajauli"
                    |df_BR$Constituency_Name=="laukaha"
                    |df_BR$Constituency_Name=="madhepur"
                    |df_BR$Constituency_Name=="madhubani"
                    |df_BR$Constituency_Name=="pandaul"
                    |df_BR$Constituency_Name=="phulparas" & df_BR$before_delim==1]<- "madhubani"

df_BR$District_Name[df_BR$Constituency_Name=="jamalpur"
                    |df_BR$Constituency_Name=="kharagpur"
                    |df_BR$Constituency_Name=="monghyr"
                    |df_BR$Constituency_Name=="tarapur" & df_BR$before_delim==1]<- "munger"

df_BR$District_Name[df_BR$Constituency_Name=="aurai"
                    |df_BR$Constituency_Name=="baruraj"
                    |df_BR$Constituency_Name=="belsand"
                    |df_BR$Constituency_Name=="bochaha"
                    |df_BR$Constituency_Name=="gaighatti"
                    |df_BR$Constituency_Name=="kanti"
                    |df_BR$Constituency_Name=="kurhani"
                    |df_BR$Constituency_Name=="minapur"
                    |df_BR$Constituency_Name=="muzaffarpur"
                    |df_BR$Constituency_Name=="paru"
                    |df_BR$Constituency_Name=="pipra"
                    |df_BR$Constituency_Name=="runisaidpur"
                    |df_BR$Constituency_Name=="sahebganj"
                    |df_BR$Constituency_Name=="sakra" & df_BR$before_delim==1]<- "muzaffarpur"

df_BR$District_Name[df_BR$Constituency_Name=="asthawan"
                    |df_BR$Constituency_Name=="chandi"
                    |df_BR$Constituency_Name=="harnaut"
                    |df_BR$Constituency_Name=="hilsa"
                    |df_BR$Constituency_Name=="islampur"
                    |df_BR$Constituency_Name=="nalanda"
                    |df_BR$Constituency_Name=="rajgir" & df_BR$before_delim==1]<- "nalanda"

df_BR$District_Name[df_BR$Constituency_Name=="gobindpur"
                    |df_BR$Constituency_Name=="hisua"
                    |df_BR$Constituency_Name=="nawada"
                    |df_BR$Constituency_Name=="rajauli"
                    |df_BR$Constituency_Name=="warsaliganj" & df_BR$before_delim==1]<- "nawada"

df_BR$Constituency_Name[df_BR$Constituency_Name=="bagaha"]<- "bagha"
df_BR$Constituency_Name[df_BR$Constituency_Name=="ram nagar"]<- "ramnagar"

df_BR$District_Name[df_BR$Constituency_Name=="bagha"
                    |df_BR$Constituency_Name=="bettiah"
                    |df_BR$Constituency_Name=="chanpatia"
                    |df_BR$Constituency_Name=="dhanaha"
                    |df_BR$Constituency_Name=="lauria"
                    |df_BR$Constituency_Name=="nautan"
                    |df_BR$Constituency_Name=="ramnagar"
                    |df_BR$Constituency_Name=="shikarpur"
                    |df_BR$Constituency_Name=="sikta" & df_BR$before_delim==1]<- "pashchim champaran"

df_BR$District_Name[df_BR$Constituency_Name=="bakhtiarpur"
                    |df_BR$Constituency_Name=="barh"
                    |df_BR$Constituency_Name=="bikram"
                    |df_BR$Constituency_Name=="dinapur"
                    |df_BR$Constituency_Name=="fatwa"
                    |df_BR$Constituency_Name=="maner"
                    |df_BR$Constituency_Name=="masaurhi"
                    |df_BR$Constituency_Name=="mokameh"
                    |df_BR$Constituency_Name=="paliganj"
                    |df_BR$Constituency_Name=="patna central"
                    |df_BR$Constituency_Name=="patna east"
                    |df_BR$Constituency_Name=="patna south"
                    |df_BR$Constituency_Name=="patna west"
                    |df_BR$Constituency_Name=="phulwari"
                    |df_BR$Constituency_Name=="massaurhi" & df_BR$before_delim==1]<- "patna"

df_BR$District_Name[df_BR$Constituency_Name=="adapur"
                    |df_BR$Constituency_Name=="dhaka"
                    |df_BR$Constituency_Name=="ghorasahan"
                    |df_BR$Constituency_Name=="gobindganj"
                    |df_BR$Constituency_Name=="harsidhi"
                    |df_BR$Constituency_Name=="kesariya"
                    |df_BR$Constituency_Name=="madhuban"
                    |df_BR$Constituency_Name=="motihari"
                    |df_BR$Constituency_Name=="pipra"
                    |df_BR$Constituency_Name=="raxaul"
                    |df_BR$Constituency_Name=="sahebganj"
                    |df_BR$Constituency_Name=="sugauli" & df_BR$before_delim==1]<- "purba champaran"

df_BR$District_Name[df_BR$Constituency_Name=="amour"
                    |df_BR$Constituency_Name=="baisi"
                    |df_BR$Constituency_Name=="banmankhi"
                    |df_BR$Constituency_Name=="dhamdaha"
                    |df_BR$Constituency_Name=="kasba"
                    |df_BR$Constituency_Name=="purnea"
                    |df_BR$Constituency_Name=="rupauli" & df_BR$before_delim==1]<- "purnia"

df_BR$District_Name[df_BR$Constituency_Name=="bikramganj"
                    |df_BR$Constituency_Name=="chenari"
                    |df_BR$Constituency_Name=="dehri"
                    |df_BR$Constituency_Name=="dinara"
                    |df_BR$Constituency_Name=="karakat"
                    |df_BR$Constituency_Name=="nokha"
                    |df_BR$Constituency_Name=="sasaram" & df_BR$before_delim==1]<- "rohtas"

df_BR$Constituency_Name[df_BR$Constituency_Name=="simiri-bakhtiarpur"|df_BR$Constituency_Name=="simri bakhtiarpur"]<- "simri-bakhtiarpur"
df_BR$Constituency_Name[df_BR$Constituency_Name=="sonabarsa"]<- "sonbarsa"

df_BR$District_Name[df_BR$Constituency_Name=="mahishi"
                    |df_BR$Constituency_Name=="saharsa"
                    |df_BR$Constituency_Name=="simri-bakhtiarpur"
                    |df_BR$Constituency_Name=="sonbarsa" & df_BR$before_delim==1]<- "saharsa"

df_BR$District_Name[df_BR$Constituency_Name=="bibhutpur"
                    |df_BR$Constituency_Name=="dalsinghsarai"
                    |df_BR$Constituency_Name=="hasanpur"
                    |df_BR$Constituency_Name=="kalyanpur"
                    |df_BR$Constituency_Name=="mohiuddin nagar"
                    |df_BR$Constituency_Name=="rosera"
                    |df_BR$Constituency_Name=="samastipur"
                    |df_BR$Constituency_Name=="sarairanjan"
                    |df_BR$Constituency_Name=="singhia"
                    |df_BR$Constituency_Name=="warisnagar" & df_BR$before_delim==1]<- "samastipur"

df_BR$District_Name[df_BR$Constituency_Name=="baniapur"
                    |df_BR$Constituency_Name=="chapra"
                    |df_BR$Constituency_Name=="garkha"
                    |df_BR$Constituency_Name=="jalalpur"
                    |df_BR$Constituency_Name=="manjhi"
                    |df_BR$Constituency_Name=="marhaura"
                    |df_BR$Constituency_Name=="masrakh"
                    |df_BR$Constituency_Name=="parsa"
                    |df_BR$Constituency_Name=="sonepur"
                    |df_BR$Constituency_Name=="taraiya" & df_BR$before_delim==1]<- "saran"

df_BR$District_Name[df_BR$Constituency_Name=="barbigha"
                    |df_BR$Constituency_Name=="sheikhpura" & df_BR$before_delim==1]<- "sheikhpura"

df_BR$District_Name[df_BR$Constituency_Name=="belsand"
                    |df_BR$Constituency_Name=="sheohar" & df_BR$before_delim==1]<- "sheohar"

df_BR$Constituency_Name[df_BR$Constituency_Name=="sitamurhi"]<- "sitamarhi"

df_BR$District_Name[df_BR$Constituency_Name=="bathnaha"
                    |df_BR$Constituency_Name=="majorganj"
                    |df_BR$Constituency_Name=="pupri"
                    |df_BR$Constituency_Name=="runisaidpur"
                    |df_BR$Constituency_Name=="sitamarhi"
                    |df_BR$Constituency_Name=="sonbarsa"
                    |df_BR$Constituency_Name=="sursand" & df_BR$before_delim==1]<- "sitamarhi"

df_BR$District_Name[df_BR$Constituency_Name=="basantpur"
                    |df_BR$Constituency_Name=="darauli"
                    |df_BR$Constituency_Name=="goreakothi"
                    |df_BR$Constituency_Name=="maharajganj"
                    |df_BR$Constituency_Name=="mairwa"
                    |df_BR$Constituency_Name=="raghunathpur"
                    |df_BR$Constituency_Name=="siwan"
                    |df_BR$Constituency_Name=="ziradei" & df_BR$before_delim==1]<- "siwan"

df_BR$District_Name[df_BR$Constituency_Name=="chhatapur"
                    |df_BR$Constituency_Name=="kishunpur"
                    |df_BR$Constituency_Name=="raghopur"
                    |df_BR$Constituency_Name=="supaul"
                    |df_BR$Constituency_Name=="tribeniganj" & df_BR$before_delim==1]<- "supaul"

df_BR$District_Name[df_BR$Constituency_Name=="hajipur"
                    |df_BR$Constituency_Name=="jandaha"
                    |df_BR$Constituency_Name=="lalganj"
                    |df_BR$Constituency_Name=="mahnar"
                    |df_BR$Constituency_Name=="mahua"
                    |df_BR$Constituency_Name=="patepur"
                    |df_BR$Constituency_Name=="raghopur"
                    |df_BR$Constituency_Name=="vaishali" & df_BR$before_delim==1]<- "vaishali"

df_BR<- df_BR%>%
  subset(District_Name!="")

############################### Chhattisgarh ##################################

df_CH<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Chhattisgarh_2021-12-29.csv")

df_CH<- df_CH%>%
  subset(Year==2003|
           Year==2008|
           Year==2013)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_CH$Constituency_Name[df_CH$Constituency_Name=="chitrakot"]<- "chitrakote"
df_CH$Constituency_Name[df_CH$Constituency_Name=="dantewara"]<- "dantewada"
df_CH$Constituency_Name[df_CH$Constituency_Name=="keshkal"]<- "keskala"
df_CH$Constituency_Name[df_CH$Constituency_Name=="narainpur"]<- "narayanpur"

df_CH$District_Name[df_CH$Constituency_Name=="bhanpuri"
                    |df_CH$Constituency_Name=="bijapur"
                    |df_CH$Constituency_Name=="chitrakote"
                    |df_CH$Constituency_Name=="dantewada"
                    |df_CH$Constituency_Name=="jagdalpur"
                    |df_CH$Constituency_Name=="keskala"
                    |df_CH$Constituency_Name=="keslur"
                    |df_CH$Constituency_Name=="kondagaon"
                    |df_CH$Constituency_Name=="konta"
                    |df_CH$Constituency_Name=="narayanpur" & df_CH$before_delim==1]<- "bastar"

df_CH$District_Name[df_CH$Constituency_Name=="bilaspur"
                    |df_CH$Constituency_Name=="bilha"
                    |df_CH$Constituency_Name=="jarhagaon"
                    |df_CH$Constituency_Name=="kota"
                    |df_CH$Constituency_Name=="lormi"
                    |df_CH$Constituency_Name=="maro"
                    |df_CH$Constituency_Name=="marwahi"
                    |df_CH$Constituency_Name=="masturi"
                    |df_CH$Constituency_Name=="mungeli"
                    |df_CH$Constituency_Name=="sipat"
                    |df_CH$Constituency_Name=="takhatpur" & df_CH$before_delim==1]<- "bilaspur"


df_CH$District_Name[df_CH$Constituency_Name=="bijapur"
                    |df_CH$Constituency_Name=="dantewada"
                    |df_CH$Constituency_Name=="konta"& df_CH$before_delim==1]<- "dantewada"


df_CH$District_Name[df_CH$Constituency_Name=="dhamtari"
                    |df_CH$Constituency_Name=="kurud"
                    |df_CH$Constituency_Name=="sihawa"& df_CH$before_delim==1]<- "dhamtari"

df_CH$Constituency_Name[df_CH$Constituency_Name=="dondi lahara"]<- "dondi lohara"

df_CH$District_Name[df_CH$Constituency_Name=="balod"
                    |df_CH$Constituency_Name=="bemetara"
                    |df_CH$Constituency_Name=="dhamdha"
                    |df_CH$Constituency_Name=="dondi lohara"
                    |df_CH$Constituency_Name=="durg"
                    |df_CH$Constituency_Name=="gunderdehi"
                    |df_CH$Constituency_Name=="khertha"
                    |df_CH$Constituency_Name=="maro"
                    |df_CH$Constituency_Name=="patan"
                    |df_CH$Constituency_Name=="saja" & df_CH$before_delim==1]<- "durg"


df_CH$District_Name[df_CH$Constituency_Name=="akaltara"
                    |df_CH$Constituency_Name=="champa"
                    |df_CH$Constituency_Name=="chandrapur"
                    |df_CH$Constituency_Name=="malkharoda"
                    |df_CH$Constituency_Name=="pamgarh"
                    |df_CH$Constituency_Name=="sakti" & df_CH$before_delim==1]<- "janjgir-champa"


df_CH$District_Name[df_CH$Constituency_Name=="bagicha"
                    |df_CH$Constituency_Name=="jashpur"
                    |df_CH$Constituency_Name=="pathalgaon"
                    |df_CH$Constituency_Name=="tapkara" & df_CH$before_delim==1]<- "jashpur"


df_CH$District_Name[df_CH$Constituency_Name=="bhanupratappur"
                    |df_CH$Constituency_Name=="kanker"
                    |df_CH$Constituency_Name=="narayanpur" & df_CH$before_delim==1]<- "kanker"


df_CH$District_Name[df_CH$Constituency_Name=="birendranagar"
                    |df_CH$Constituency_Name=="kawardha"
                    |df_CH$Constituency_Name=="lormi"
                    |df_CH$Constituency_Name=="mungeli" & df_CH$before_delim==1]<- "kawardha"


df_CH$District_Name[df_CH$Constituency_Name=="katghora"
                    |df_CH$Constituency_Name=="rampur"
                    |df_CH$Constituency_Name=="tanakhar" & df_CH$before_delim==1]<- "Korba"

df_CH$District_Name[df_CH$Constituency_Name=="baikunthpur"
                    |df_CH$Constituency_Name=="manendragarh" & df_CH$before_delim==1]<- "koriya"

df_CH$District_Name[df_CH$Constituency_Name=="basna"
                    |df_CH$Constituency_Name=="bhatgaon"
                    |df_CH$Constituency_Name=="khallari"
                    |df_CH$Constituency_Name=="mahasamund"
                    |df_CH$Constituency_Name=="saraipali" & df_CH$before_delim==1]<- "mahasamund"

df_CH$District_Name[df_CH$Constituency_Name=="dharamjaigarh"
                    |df_CH$Constituency_Name=="kharsia"
                    |df_CH$Constituency_Name=="lailunga"
                    |df_CH$Constituency_Name=="pathalgaon"
                    |df_CH$Constituency_Name=="raigarh"
                    |df_CH$Constituency_Name=="sarangarh"
                    |df_CH$Constituency_Name=="saria" & df_CH$before_delim==1]<- "raigarh"

df_CH$District_Name[df_CH$Constituency_Name=="abhanpur"
                    |df_CH$Constituency_Name=="arang"
                    |df_CH$Constituency_Name=="baloda bazar"
                    |df_CH$Constituency_Name=="bhatapara"
                    |df_CH$Constituency_Name=="bhatgaon"
                    |df_CH$Constituency_Name=="bindranawagarh"
                    |df_CH$Constituency_Name=="dharsiwa"
                    |df_CH$Constituency_Name=="kasdol"
                    |df_CH$Constituency_Name=="mandirhasod"
                    |df_CH$Constituency_Name=="pallari"
                    |df_CH$Constituency_Name=="raipur rural"
                    |df_CH$Constituency_Name=="raipur town"
                    |df_CH$Constituency_Name=="rajim" & df_CH$before_delim==1]<- "raipur"


df_CH$District_Name[df_CH$Constituency_Name=="chowki"
                    |df_CH$Constituency_Name=="dongargaon"
                    |df_CH$Constituency_Name=="dongargarh"
                    |df_CH$Constituency_Name=="khairagarh"
                    |df_CH$Constituency_Name=="khujji"
                    |df_CH$Constituency_Name=="rajnandgaon" & df_CH$before_delim==1]<- "rajnandgaon"


df_CH$District_Name[df_CH$Constituency_Name=="ambikapur"
                    |df_CH$Constituency_Name=="lundra"
                    |df_CH$Constituency_Name=="pal"
                    |df_CH$Constituency_Name=="pilkha"
                    |df_CH$Constituency_Name=="premnagar"
                    |df_CH$Constituency_Name=="samri"
                    |df_CH$Constituency_Name=="sitapur"
                    |df_CH$Constituency_Name=="surajpur" & df_CH$before_delim==1]<- "surguja"

df_CH<- df_CH%>%
  subset(District_Name!="")

############################# Delhi #################################

df_DL<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Delhi_2021-11-23.csv")

df_DL<- df_DL%>%
  subset(Year==1993|
           Year==1998|
           Year==2003|
           Year==2008|
           Year==2015)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_DL$Constituency_Name[df_DL$Constituency_Name=="trilok puri"]<- "trilokpuri"

df_DL$District_Name[df_DL$Constituency_Name=="ghonda"
                    |df_DL$Constituency_Name=="mandawali"
                    |df_DL$Constituency_Name=="trilokpuri" & df_DL$before_delim==1]<- "east"

df_DL$District_Name[df_DL$Constituency_Name=="babarpur"
                    |df_DL$Constituency_Name=="nand nagari"
                    |df_DL$Constituency_Name=="qarawal nagar"
                    |df_DL$Constituency_Name=="seemapuri"
                    |df_DL$Constituency_Name=="yamuna vihar" & df_DL$before_delim==1]<- "north east"

df_DL$District_Name[df_DL$Constituency_Name=="bawana"
                    |df_DL$Constituency_Name=="bhalswa jahangirpur"
                    |df_DL$Constituency_Name=="narela"
                    |df_DL$Constituency_Name=="sahibabad daulatpur" & df_DL$before_delim==1]<- "north west"

df_DL$District_Name[df_DL$Constituency_Name=="badarpur"
                    |df_DL$Constituency_Name=="mehrauli"
                    |df_DL$Constituency_Name=="okhla"
                    |df_DL$Constituency_Name=="saket" & df_DL$before_delim==1]<- "south"

df_DL$District_Name[df_DL$Constituency_Name=="mahipalpur"
                    |df_DL$Constituency_Name=="najafgarh"
                    |df_DL$Constituency_Name=="palam" & df_DL$before_delim==1]<- "south west"

df_DL$District_Name[df_DL$Constituency_Name=="hastsal"
                    |df_DL$Constituency_Name=="nangloi jat" & df_DL$before_delim==1]<- "west"

df_DL<- df_DL%>%
  subset(District_Name!="")

############################## Goa ################################

df_GO<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Goa_2021-11-23.csv")

df_GO<- df_GO%>%
  subset(Year==1989|
           Year==1994|
           Year==1999|
           Year==2002|
           Year==2007|
           Year==2012)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_GO$District_Name[df_GO$Constituency_Name=="aldona"
                    |df_GO$Constituency_Name=="bicholim"
                    |df_GO$Constituency_Name=="calangute"
                    |df_GO$Constituency_Name=="cumbarjua"
                    |df_GO$Constituency_Name=="dargalim"
                    |df_GO$Constituency_Name=="loutolim"
                    |df_GO$Constituency_Name=="maem"
                    |df_GO$Constituency_Name=="mandrem"
                    |df_GO$Constituency_Name=="marcaim"
                    |df_GO$Constituency_Name=="pale"
                    |df_GO$Constituency_Name=="pernem"
                    |df_GO$Constituency_Name=="ponda"
                    |df_GO$Constituency_Name=="poriem"
                    |df_GO$Constituency_Name=="priol"
                    |df_GO$Constituency_Name=="saligao"
                    |df_GO$Constituency_Name=="siroda"
                    |df_GO$Constituency_Name=="siolim"
                    |df_GO$Constituency_Name=="santo. andre"
                    |df_GO$Constituency_Name=="santa cruz"
                    |df_GO$Constituency_Name=="tivim"
                    |df_GO$Constituency_Name=="valpoi" & df_GO$before_delim==1]<- "north goa"

df_GO$District_Name[District_Name!=""]<- "south goa"

################################################ Gujarat ######################################################################

df_GJ<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Gujarat_2021-11-23.csv")

df_GJ<- df_GJ%>%
  subset(Year==1990|
           Year==1995|
           Year==1998|
           Year==2002|
           Year==2007|
           Year==2012|
           Year==2014)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_GJ$District_Name[df_GJ$Constituency_Name=="daskroi"
                    |df_GJ$Constituency_Name=="dhandhuka"
                    |df_GJ$Constituency_Name=="dholka"
                    |df_GJ$Constituency_Name=="mandal"
                    |df_GJ$Constituency_Name=="viramgam"
                    |df_GJ$Constituency_Name=="bavla" & df_GJ$before_delim==1]<- "ahmadabad"

df_GJ$District_Name[df_GJ$Constituency_Name=="amreli"
                    |df_GJ$Constituency_Name=="babra"
                    |df_GJ$Constituency_Name=="dhari"
                    |df_GJ$Constituency_Name=="jasdan"
                    |df_GJ$Constituency_Name=="kodinar"
                    |df_GJ$Constituency_Name=="kundla"
                    |df_GJ$Constituency_Name=="lathi"
                    |df_GJ$Constituency_Name=="rajula" & df_GJ$before_delim==1]<- "amreli"

df_GJ$District_Name[df_GJ$Constituency_Name=="anand"
                    |df_GJ$Constituency_Name=="bhadran"
                    |df_GJ$Constituency_Name=="borsad"
                    |df_GJ$Constituency_Name=="chakalasi"
                    |df_GJ$Constituency_Name=="matar"
                    |df_GJ$Constituency_Name=="petlad"
                    |df_GJ$Constituency_Name=="sarsa"
                    |df_GJ$Constituency_Name=="sojitra"
                    |df_GJ$Constituency_Name=="umreth"
                    |df_GJ$Constituency_Name=="cambay" & df_GJ$before_delim==1]<- "anand"

df_GJ$District_Name[df_GJ$Constituency_Name=="danta"
                    |df_GJ$Constituency_Name=="deesa"
                    |df_GJ$Constituency_Name=="deodar"
                    |df_GJ$Constituency_Name=="dhanera"
                    |df_GJ$Constituency_Name=="kankrej"
                    |df_GJ$Constituency_Name=="palanpur"
                    |df_GJ$Constituency_Name=="radhanpur"
                    |df_GJ$Constituency_Name=="vadgam"
                    |df_GJ$Constituency_Name=="vav" & df_GJ$before_delim==1]<- "banas kantha"

df_GJ$District_Name[df_GJ$Constituency_Name=="ankleshwar"
                    |df_GJ$Constituency_Name=="broach"
                    |df_GJ$Constituency_Name=="dediapada"
                    |df_GJ$Constituency_Name=="jhagadia"
                    |df_GJ$Constituency_Name=="vagra"
                    |df_GJ$Constituency_Name=="rajpipla"
                    |df_GJ$Constituency_Name=="jambusar" & df_GJ$before_delim==1]<- "bharuch"

df_GJ$District_Name[df_GJ$Constituency_Name=="bhavnagar"
                    |df_GJ$Constituency_Name=="bhavnagar north"
                    |df_GJ$Constituency_Name=="bhavnagar south"
                    |df_GJ$Constituency_Name=="botad"
                    |df_GJ$Constituency_Name=="gadhada"
                    |df_GJ$Constituency_Name=="ghogho"
                    |df_GJ$Constituency_Name=="mahuva"
                    |df_GJ$Constituency_Name=="palitana"
                    |df_GJ$Constituency_Name=="sihor"
                    |df_GJ$Constituency_Name=="talaja" & df_GJ$before_delim==1]<- "bhavnagar"

df_GJ$Constituency_Name[df_GJ$Constituency_Name=="davgadh baria"]<- "devgadh baria"
df_GJ$Constituency_Name[df_GJ$Constituency_Name=="limkhada"]<- "limkheda"

df_GJ$District_Name[df_GJ$Constituency_Name=="devgadh baria"
                    |df_GJ$Constituency_Name=="dohad"
                    |df_GJ$Constituency_Name=="jhalod"
                    |df_GJ$Constituency_Name=="limbdi"
                    |df_GJ$Constituency_Name=="limkheda"
                    |df_GJ$Constituency_Name=="rajgadh"
                    |df_GJ$Constituency_Name=="randhikpur"
                    |df_GJ$Constituency_Name=="limdi" & df_GJ$before_delim==1]<- "dohad"

df_GJ$District_Name[df_GJ$Constituency_Name=="dehgam"
                    |df_GJ$Constituency_Name=="gandhinagar"
                    |df_GJ$Constituency_Name=="kadi"
                    |df_GJ$Constituency_Name=="kalol"
                    |df_GJ$Constituency_Name=="mansa"
                    |df_GJ$Constituency_Name=="sarkhej"
                    |df_GJ$Constituency_Name=="naroda" & df_GJ$before_delim==1]<- "gandhinagar"

df_GJ$Constituency_Name[df_GJ$Constituency_Name=="jodia"]<- "jodiya"

df_GJ$District_Name[df_GJ$Constituency_Name=="bhanvad"
                    |df_GJ$Constituency_Name=="dwarka"
                    |df_GJ$Constituency_Name=="jamjodhpur"
                    |df_GJ$Constituency_Name=="jamnagar rural"
                    |df_GJ$Constituency_Name=="jodiya"
                    |df_GJ$Constituency_Name=="kalawad"
                    |df_GJ$Constituency_Name=="khambhalia"
                    |df_GJ$Constituency_Name=="tankara"
                    |df_GJ$Constituency_Name=="jamnagar" & df_GJ$before_delim==1]<- "jamnagar"

df_GJ$Constituency_Name[df_GJ$Constituency_Name=="malia"]<- "maliya"

df_GJ$District_Name[df_GJ$Constituency_Name=="junagadh"
                    |df_GJ$Constituency_Name=="keshod"
                    |df_GJ$Constituency_Name=="kodinar"
                    |df_GJ$Constituency_Name=="kutiyana"
                    |df_GJ$Constituency_Name=="maliya"
                    |df_GJ$Constituency_Name=="manavadar"
                    |df_GJ$Constituency_Name=="mangrol"
                    |df_GJ$Constituency_Name=="somnath"
                    |df_GJ$Constituency_Name=="talala"
                    |df_GJ$Constituency_Name=="una"
                    |df_GJ$Constituency_Name=="visavadar" & df_GJ$before_delim==1]<- "junagarh"

df_GJ$Constituency_Name[df_GJ$Constituency_Name=="jodia"]<- "jodiya"

df_GJ$District_Name[df_GJ$Constituency_Name=="abdasa"
                    |df_GJ$Constituency_Name=="anjar"
                    |df_GJ$Constituency_Name=="bhuj"
                    |df_GJ$Constituency_Name=="jodiya"
                    |df_GJ$Constituency_Name=="mandvi"
                    |df_GJ$Constituency_Name=="mundra"
                    |df_GJ$Constituency_Name=="rapar" & df_GJ$before_delim==1]<- "kachchh"

df_GJ$Constituency_Name[df_GJ$Constituency_Name=="kapadwanj"]<- "kapadvanj"
df_GJ$Constituency_Name[df_GJ$Constituency_Name=="madiad"]<- "nadiad"

df_GJ$District_Name[df_GJ$Constituency_Name=="balasinor"
                    |df_GJ$Constituency_Name=="chakalasi"
                    |df_GJ$Constituency_Name=="kapadvanj"
                    |df_GJ$Constituency_Name=="kathlal"
                    |df_GJ$Constituency_Name=="mahudha"
                    |df_GJ$Constituency_Name=="matar"
                    |df_GJ$Constituency_Name=="mehmedabad"
                    |df_GJ$Constituency_Name=="thasra"
                    |df_GJ$Constituency_Name=="nadiad" & df_GJ$before_delim==1]<- "kheda"

df_GJ$District_Name[df_GJ$Constituency_Name=="chanasma"
                    |df_GJ$Constituency_Name=="jotana"
                    |df_GJ$Constituency_Name=="kadi"
                    |df_GJ$Constituency_Name=="kheralu"
                    |df_GJ$Constituency_Name=="mehsana"
                    |df_GJ$Constituency_Name=="unjha"
                    |df_GJ$Constituency_Name=="vijapur"
                    |df_GJ$Constituency_Name=="visnagar" & df_GJ$before_delim==1]<- "mahesana"

df_GJ$District_Name[df_GJ$Constituency_Name=="dediapada"
                    |df_GJ$Constituency_Name=="nasvadi"
                    |df_GJ$Constituency_Name=="rajpipla" & df_GJ$before_delim==1]<- "narmada"

df_GJ$District_Name[df_GJ$Constituency_Name=="dharampur"
                    |df_GJ$Constituency_Name=="gandevi"
                    |df_GJ$Constituency_Name=="jalalpore"
                    |df_GJ$Constituency_Name=="navsari"
                    |df_GJ$Constituency_Name=="chikhli" & df_GJ$before_delim==1]<- "navsari"

df_GJ$District_Name[df_GJ$Constituency_Name=="godhra"
                    |df_GJ$Constituency_Name=="halol"
                    |df_GJ$Constituency_Name=="kalol"
                    |df_GJ$Constituency_Name=="lunavada"
                    |df_GJ$Constituency_Name=="rajgadh"
                    |df_GJ$Constituency_Name=="randhikpur"
                    |df_GJ$Constituency_Name=="santrampur"
                    |df_GJ$Constituency_Name=="shehra" & df_GJ$before_delim==1]<- "panch mahals"

df_GJ$District_Name[df_GJ$Constituency_Name=="patan"
                    |df_GJ$Constituency_Name=="radhanpur"
                    |df_GJ$Constituency_Name=="sami"
                    |df_GJ$Constituency_Name=="sidhpur"
                    |df_GJ$Constituency_Name=="vagdod" & df_GJ$before_delim==1]<- "patan"

df_GJ$District_Name[df_GJ$Constituency_Name=="kutiyana"
                    |df_GJ$Constituency_Name=="porbandar" & df_GJ$before_delim==1]<- "porbandar"

df_GJ$Constituency_Name[df_GJ$Constituency_Name=="rajkot i"]<- "rajkot-i"
df_GJ$Constituency_Name[df_GJ$Constituency_Name=="rajkot i i"]<- "rajkot-ii"

df_GJ$District_Name[df_GJ$Constituency_Name=="dhoraji"
                    |df_GJ$Constituency_Name=="gondal"
                    |df_GJ$Constituency_Name=="jasdan"
                    |df_GJ$Constituency_Name=="jetpur"
                    |df_GJ$Constituency_Name=="morvi"
                    |df_GJ$Constituency_Name=="rajkot-i"
                    |df_GJ$Constituency_Name=="rajkot-ii"
                    |df_GJ$Constituency_Name=="rajkot rural"
                    |df_GJ$Constituency_Name=="tankara"
                    |df_GJ$Constituency_Name=="upleta"
                    |df_GJ$Constituency_Name=="wankaner" & df_GJ$before_delim==1]<- "rajkot"

df_GJ$District_Name[df_GJ$Constituency_Name=="bhiloda"
                    |df_GJ$Constituency_Name=="himatnagar"
                    |df_GJ$Constituency_Name=="idar"
                    |df_GJ$Constituency_Name=="khedbrahma"
                    |df_GJ$Constituency_Name=="meghraj"
                    |df_GJ$Constituency_Name=="modasa"
                    |df_GJ$Constituency_Name=="prantij"
                    |df_GJ$Constituency_Name=="bayad" & df_GJ$before_delim==1]<- "sabar kantha"

df_GJ$District_Name[df_GJ$Constituency_Name=="bardoli"
                    |df_GJ$Constituency_Name=="kamrej"
                    |df_GJ$Constituency_Name=="mahuva"
                    |df_GJ$Constituency_Name=="mangrol"
                    |df_GJ$Constituency_Name=="nijhar"
                    |df_GJ$Constituency_Name=="olpad"
                    |df_GJ$Constituency_Name=="songadh"
                    |df_GJ$Constituency_Name=="surat city east"
                    |df_GJ$Constituency_Name=="surat city north"
                    |df_GJ$Constituency_Name=="surat city west"
                    |df_GJ$Constituency_Name=="vyara"
                    |df_GJ$Constituency_Name=="chorasi" & df_GJ$before_delim==1]<- "surat"

df_GJ$District_Name[df_GJ$Constituency_Name=="chotila"
                    |df_GJ$Constituency_Name=="dasada"
                    |df_GJ$Constituency_Name=="dhrangadhra"
                    |df_GJ$Constituency_Name=="halvad"
                    |df_GJ$Constituency_Name=="limbdi"
                    |df_GJ$Constituency_Name=="wadhwan" & df_GJ$before_delim==1]<- "surendranagar"

df_GJ$Constituency_Name[df_GJ$Constituency_Name=="dangs bansada"]<- "dangs-bansda"

df_GJ$District_Name[df_GJ$Constituency_Name=="dangs-bansda"]<- "the dangs"

df_GJ$Constituency_Name[df_GJ$Constituency_Name=="waghodia"]<- "vaghodia"

df_GJ$District_Name[df_GJ$Constituency_Name=="baroda city"
                    |df_GJ$Constituency_Name=="baroda rural"
                    |df_GJ$Constituency_Name=="chhota udaipur"
                    |df_GJ$Constituency_Name=="dabhoi"
                    |df_GJ$Constituency_Name=="jetpur"
                    |df_GJ$Constituency_Name=="nasvadi"
                    |df_GJ$Constituency_Name=="padra"
                    |df_GJ$Constituency_Name=="sankheda"
                    |df_GJ$Constituency_Name=="savli"
                    |df_GJ$Constituency_Name=="vaghodia"
                    |df_GJ$Constituency_Name=="karjan" & df_GJ$before_delim==1]<- "vadodara"

df_GJ$District_Name[df_GJ$Constituency_Name=="bulsar"
                    |df_GJ$Constituency_Name=="dharampur"
                    |df_GJ$Constituency_Name=="mota pondha"
                    |df_GJ$Constituency_Name=="pardi"
                    |df_GJ$Constituency_Name=="umbergaon" & df_GJ$before_delim==1]<- "valsad"


df_GJ$District_Name[df_GJ$District_Name=="banaskantha"]<- "banas kantha"
df_GJ$District_Name[df_GJ$District_Name=="dangs"]<- "the dangs"
df_GJ$District_Name[df_GJ$District_Name=="junagarh"]<- "junagadh"
df_GJ$District_Name[df_GJ$District_Name=="panch mahals"]<- "panchmahal"
df_GJ$District_Name[df_GJ$District_Name=="vadodra"]<- "vadodara"

df_GJ<- df_GJ%>%
  subset(District_Name!="")

########################## Haryana ##################################

df_HR<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Haryana_2021-11-23.csv")

df_HR<- df_HR%>%
  subset(Year==1991|
           Year==1996|
           Year==2000|
           Year==2005|
           Year==2009|
           Year==2014)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_HR$District_Name[df_HR$Constituency_Name=="ambala cantonment"
                    |df_HR$Constituency_Name=="ambala city"
                    |df_HR$Constituency_Name=="mullana"
                    |df_HR$Constituency_Name=="naggal"
                    |df_HR$Constituency_Name=="naraingarh"
                    |df_HR$Constituency_Name=="sadhaura" & df_HR$before_delim==1]<- "ambala"

df_HR$Constituency_Name[df_HR$Constituency_Name=="bawanikhera"]<- "bawani khera"

df_HR$District_Name[df_HR$Constituency_Name=="badhra"
                    |df_HR$Constituency_Name=="bawani khera"
                    |df_HR$Constituency_Name=="bhiwani"
                    |df_HR$Constituency_Name=="dadri"
                    |df_HR$Constituency_Name=="loharu"
                    |df_HR$Constituency_Name=="mundhal khurd"
                    |df_HR$Constituency_Name=="tosham" & df_HR$before_delim==1]<- "bhiwani"

df_HR$District_Name[df_HR$Constituency_Name=="ballabgarh"
                    |df_HR$Constituency_Name=="faridabad"
                    |df_HR$Constituency_Name=="ferozepur jhirka"
                    |df_HR$Constituency_Name=="hassanpur"
                    |df_HR$Constituency_Name=="hathin"
                    |df_HR$Constituency_Name=="mewla maharajpur"
                    |df_HR$Constituency_Name=="palwal"
                    |df_HR$Constituency_Name=="taoru" & df_HR$before_delim==1]<- "faridabad"

df_HR$District_Name[df_HR$Constituency_Name=="bhattu kalan"
                    |df_HR$Constituency_Name=="fatehabad"
                    |df_HR$Constituency_Name=="ratia"
                    |df_HR$Constituency_Name=="tohana" & df_HR$before_delim==1]<- "fatehabad"

df_HR$District_Name[df_HR$Constituency_Name=="ferozepur jhirka"
                    |df_HR$Constituency_Name=="gurgaon"
                    |df_HR$Constituency_Name=="nuh"
                    |df_HR$Constituency_Name=="pataudi"
                    |df_HR$Constituency_Name=="sohna"
                    |df_HR$Constituency_Name=="taoru" & df_HR$before_delim==1]<- "gurgaon"

df_HR$District_Name[df_HR$Constituency_Name=="adampur"
                    |df_HR$Constituency_Name=="barwala"
                    |df_HR$Constituency_Name=="ghirai"
                    |df_HR$Constituency_Name=="bhattu kalan"
                    |df_HR$Constituency_Name=="ghirai"
                    |df_HR$Constituency_Name=="hissar"
                    |df_HR$Constituency_Name=="narnaund"
                    |df_HR$Constituency_Name=="hansi" & df_HR$before_delim==1]<- "hisar"

df_HR$Constituency_Name[df_HR$Constituency_Name=="jhajjar(sc)"]<- "jhajjar"

df_HR$District_Name[df_HR$Constituency_Name=="badli"
                    |df_HR$Constituency_Name=="bahadurgarh"
                    |df_HR$Constituency_Name=="beri"
                    |df_HR$Constituency_Name=="jhajjar"
                    |df_HR$Constituency_Name=="salhawas" & df_HR$before_delim==1]<- "jhajjar"

df_HR$Constituency_Name[df_HR$Constituency_Name=="rajaund"]<- "rajond"

df_HR$District_Name[df_HR$Constituency_Name=="jind"
                    |df_HR$Constituency_Name=="julana"
                    |df_HR$Constituency_Name=="kalayat"
                    |df_HR$Constituency_Name=="narwana"
                    |df_HR$Constituency_Name=="rajond"
                    |df_HR$Constituency_Name=="safidon"
                    |df_HR$Constituency_Name=="uchana kalan" & df_HR$before_delim==1]<- "jind"

df_HR$District_Name[df_HR$Constituency_Name=="guhla"
                    |df_HR$Constituency_Name=="kaithal"
                    |df_HR$Constituency_Name=="kalayat"
                    |df_HR$Constituency_Name=="pai"
                    |df_HR$Constituency_Name=="pundri" & df_HR$before_delim==1]<- "kaithal"

df_HR$District_Name[df_HR$Constituency_Name=="assandh"
                    |df_HR$Constituency_Name=="gharaunda"
                    |df_HR$Constituency_Name=="indri"
                    |df_HR$Constituency_Name=="jundla"
                    |df_HR$Constituency_Name=="karnal"
                    |df_HR$Constituency_Name=="nilokheri" & df_HR$before_delim==1]<- "karnal"

df_HR$District_Name[df_HR$Constituency_Name=="pehowa"
                    |df_HR$Constituency_Name=="radaur"
                    |df_HR$Constituency_Name=="shahabad"
                    |df_HR$Constituency_Name=="thanesar" & df_HR$before_delim==1]<- "kurukshetra"

df_HR$District_Name[df_HR$Constituency_Name=="ateli"
                    |df_HR$Constituency_Name=="jatusana"
                    |df_HR$Constituency_Name=="mahendragarh"
                    |df_HR$Constituency_Name=="narnaul" & df_HR$before_delim==1]<- "mahendragarh"

df_HR$District_Name[df_HR$Constituency_Name=="kalka"
                    |df_HR$Constituency_Name=="naraingarh" & df_HR$before_delim==1]<- "panchkula"

df_HR$District_Name[df_HR$Constituency_Name=="naultha"
                    |df_HR$Constituency_Name=="sambhalka"
                    |df_HR$Constituency_Name=="panipat" & df_HR$before_delim==1]<- "panipat"

df_HR$District_Name[df_HR$Constituency_Name=="bawal"
                    |df_HR$Constituency_Name=="jatusana"
                    |df_HR$Constituency_Name=="rewari"
                    |df_HR$Constituency_Name=="salhawas" & df_HR$before_delim==1]<- "rewari"

df_HR$District_Name[df_HR$Constituency_Name=="hassangarh"
                    |df_HR$Constituency_Name=="kalanaur"
                    |df_HR$Constituency_Name=="kiloi"
                    |df_HR$Constituency_Name=="meham"
                    |df_HR$Constituency_Name=="rohtak" & df_HR$before_delim==1]<- "rohtak"

df_HR$District_Name[df_HR$Constituency_Name=="dabwali"
                    |df_HR$Constituency_Name=="darba kalan"
                    |df_HR$Constituency_Name=="ellenabad"
                    |df_HR$Constituency_Name=="rori"
                    |df_HR$Constituency_Name=="sirsa" & df_HR$before_delim==1]<- "sirsa"

df_HR$District_Name[df_HR$Constituency_Name=="baroda"
                    |df_HR$Constituency_Name=="gohana"
                    |df_HR$Constituency_Name=="kailana"
                    |df_HR$Constituency_Name=="rai"
                    |df_HR$Constituency_Name=="rohat"
                    |df_HR$Constituency_Name=="sonepat" & df_HR$before_delim==1]<- "sonipat"

df_HR$Constituency_Name[df_HR$Constituency_Name=="jagadhari"]<- "jagadhri"
df_HR$Constituency_Name[df_HR$Constituency_Name=="yamuna nagar"]<- "yamunanagar"

df_HR$District_Name[df_HR$Constituency_Name=="chhachhrauli"
                    |df_HR$Constituency_Name=="jagadhri"
                    |df_HR$Constituency_Name=="radaur"
                    |df_HR$Constituency_Name=="sadhaura"
                    |df_HR$Constituency_Name=="yamunanagar" & df_HR$before_delim==1]<- "yamunanagar"

df_HR<- df_HR%>%
  subset(District_Name!="")

##################### Himachal Pradesh #########################

df_HP<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Himachal_Pradesh_2021-11-23.csv")

df_HP<- df_HP%>%
  subset(Year==1990|
           Year==1993|
           Year==1998|
           Year==2003|
           Year==2007|
           Year==2012)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_HP$District_Name[df_HP$Constituency_Name=="bilaspur"
                    |df_HP$Constituency_Name=="ghumarwin"
                    |df_HP$Constituency_Name=="geharwin"
                    |df_HP$Constituency_Name=="kotkehloor" & df_HP$before_delim==1]<- "bilaspur"

df_HP$District_Name[df_HP$Constituency_Name=="banikhet"
                    |df_HP$Constituency_Name=="bharmour"
                    |df_HP$Constituency_Name=="bhattiyat"
                    |df_HP$Constituency_Name=="chamba"
                    |df_HP$Constituency_Name=="rajnagar" & df_HP$before_delim==1]<- "chamba"

df_HP$District_Name[df_HP$Constituency_Name=="bamsan"
                    |df_HP$Constituency_Name=="hamirpur"
                    |df_HP$Constituency_Name=="mewa"
                    |df_HP$Constituency_Name=="nadaun"
                    |df_HP$Constituency_Name=="nadaunta"
                    |df_HP$Constituency_Name=="thural" & df_HP$before_delim==1]<- "hamirpur"

df_HP$District_Name[df_HP$Constituency_Name=="baijnath"
                    |df_HP$Constituency_Name=="dharamsala"
                    |df_HP$Constituency_Name=="gangath"
                    |df_HP$Constituency_Name=="guler"
                    |df_HP$Constituency_Name=="jaswan"
                    |df_HP$Constituency_Name=="jawalamukhi"
                    |df_HP$Constituency_Name=="jawali"
                    |df_HP$Constituency_Name=="kangra"
                    |df_HP$Constituency_Name=="nagrota"
                    |df_HP$Constituency_Name=="nurpur"
                    |df_HP$Constituency_Name=="palampur"
                    |df_HP$Constituency_Name=="pragpur"
                    |df_HP$Constituency_Name=="rajgir"
                    |df_HP$Constituency_Name=="shahpur"
                    |df_HP$Constituency_Name=="sulah"
                    |df_HP$Constituency_Name=="thural" & df_HP$before_delim==1]<- "kangra"

df_HP$District_Name[df_HP$Constituency_Name=="kinnaur" & df_HP$before_delim==1]<- "kinnaur"

df_HP$Constituency_Name[df_HP$Constituency_Name=="kullu"]<- "kulu"

df_HP$District_Name[df_HP$Constituency_Name=="ani"
                    |df_HP$Constituency_Name=="banjar"
                    |df_HP$Constituency_Name=="kulu" & df_HP$before_delim==1]<- "kullu"

df_HP$Constituency_Name[df_HP$Constituency_Name=="lahaul spiti"]<- "lahaul and spiti"

df_HP$District_Name[df_HP$Constituency_Name=="lahaul and spiti" & df_HP$before_delim==1]<- "lahul & spiti"

df_HP$Constituency_Name[df_HP$Constituency_Name=="sundarnagar"]<- "sundernagar"

df_HP$District_Name[df_HP$Constituency_Name=="balh"
                    |df_HP$Constituency_Name=="chachiot"
                    |df_HP$Constituency_Name=="darang"
                    |df_HP$Constituency_Name=="dharampur"
                    |df_HP$Constituency_Name=="gopalpur"
                    |df_HP$Constituency_Name=="joginder nagar"
                    |df_HP$Constituency_Name=="karsog"
                    |df_HP$Constituency_Name=="kumarsain"
                    |df_HP$Constituency_Name=="mandi"
                    |df_HP$Constituency_Name=="nachan"
                    |df_HP$Constituency_Name=="sundernagar" & df_HP$before_delim==1]<- "mandi"

df_HP$Constituency_Name[df_HP$Constituency_Name=="jubbal kotkhai"]<- "jubbal-kotkhai"

df_HP$District_Name[df_HP$Constituency_Name=="chopal"
                    |df_HP$Constituency_Name=="jubbal-kotkhai"
                    |df_HP$Constituency_Name=="kasumpti"
                    |df_HP$Constituency_Name=="kumarsain"
                    |df_HP$Constituency_Name=="rampur"
                    |df_HP$Constituency_Name=="rohru"
                    |df_HP$Constituency_Name=="simla"
                    |df_HP$Constituency_Name=="theog" & df_HP$before_delim==1]<- "shimla"

df_HP$District_Name[df_HP$Constituency_Name=="nahan"
                    |df_HP$Constituency_Name=="pachhad"
                    |df_HP$Constituency_Name=="paonta doon"
                    |df_HP$Constituency_Name=="rainka"
                    |df_HP$Constituency_Name=="shillai" & df_HP$before_delim==1]<- "sirmaur"

df_HP$District_Name[df_HP$Constituency_Name=="arki"
                    |df_HP$Constituency_Name=="doon"
                    |df_HP$Constituency_Name=="kasauli"
                    |df_HP$Constituency_Name=="nalagarh"
                    |df_HP$Constituency_Name=="solan" & df_HP$before_delim==1]<- "solan"

df_HP$Constituency_Name[df_HP$Constituency_Name=="santokhgarh"]<- "santokgarh"

df_HP$District_Name[df_HP$Constituency_Name=="chintpurni"
                    |df_HP$Constituency_Name=="gagret"
                    |df_HP$Constituency_Name=="kutlehar"
                    |df_HP$Constituency_Name=="santokgarh"
                    |df_HP$Constituency_Name=="una" & df_HP$before_delim==1]<- "una"

df_HP$District_Name[df_HP$District_Name=="lahaul & spiti"]<- "lahul & spiti"

df_HP<- df_HP%>%
  subset(District_Name!="")

############################### Jharkhand ##################################

df_JH<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Jharkhand_2021-12-29.csv")

df_JH<- df_JH%>%
  subset(Year==2005|
           Year==2009|
           Year==2014)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_JH$Constituency_Name[df_JH$Constituency_Name=="chandankyari"]<- "chandankiyari"
df_JH$Constituency_Name[df_JH$Constituency_Name=="gomiya"]<- "gomia"

df_JH$District_Name[df_JH$Constituency_Name=="bermo"
                    |df_JH$Constituency_Name=="bokaro"
                    |df_JH$Constituency_Name=="chandankiyari"
                    |df_JH$Constituency_Name=="dumri"
                    |df_JH$Constituency_Name=="gomia" & df_JH$before_delim==1]<- "bokaro"

df_JH$District_Name[df_JH$Constituency_Name=="chatra"
                    |df_JH$Constituency_Name=="simaria" & df_JH$before_delim==1]<- "chatra"

df_JH$District_Name[df_JH$Constituency_Name=="deoghar"
                    |df_JH$Constituency_Name=="jarmundi"
                    |df_JH$Constituency_Name=="madhupur"
                    |df_JH$Constituency_Name=="sarath" & df_JH$before_delim==1]<- "deoghar"

df_JH$District_Name[df_JH$Constituency_Name=="baghmara"
                    |df_JH$Constituency_Name=="dhanbad"
                    |df_JH$Constituency_Name=="nirsa"
                    |df_JH$Constituency_Name=="sindri"
                    |df_JH$Constituency_Name=="tundi" & df_JH$before_delim==1]<- "dhanbad"

df_JH$Constituency_Name[df_JH$Constituency_Name=="shikaripara"]<- "sikaripara"

df_JH$District_Name[df_JH$Constituency_Name=="dumka"
                    |df_JH$Constituency_Name=="jama"
                    |df_JH$Constituency_Name=="jamtara"
                    |df_JH$Constituency_Name=="jarmundi"
                    |df_JH$Constituency_Name=="litipara"
                    |df_JH$Constituency_Name=="nala"
                    |df_JH$Constituency_Name=="poreyahat"
                    |df_JH$Constituency_Name=="sikaripara" & df_JH$before_delim==1]<- "dumka"

df_JH$District_Name[df_JH$Constituency_Name=="bhawanathpur"
                    |df_JH$Constituency_Name=="bishrampur"
                    |df_JH$Constituency_Name=="daltonganj"
                    |df_JH$Constituency_Name=="garhwa" & df_JH$before_delim==1]<- "garhwa"

df_JH$District_Name[df_JH$Constituency_Name=="bagodar"
                    |df_JH$Constituency_Name=="dhanwar"
                    |df_JH$Constituency_Name=="dumri"
                    |df_JH$Constituency_Name=="gandey"
                    |df_JH$Constituency_Name=="giridih"
                    |df_JH$Constituency_Name=="jamua" & df_JH$before_delim==1]<- "giridih"

df_JH$District_Name[df_JH$Constituency_Name=="barhait"
                    |df_JH$Constituency_Name=="borio"
                    |df_JH$Constituency_Name=="godda"
                    |df_JH$Constituency_Name=="mahagama" & df_JH$before_delim==1]<- "godda"

df_JH$District_Name[df_JH$Constituency_Name=="bishnupur"
                    |df_JH$Constituency_Name=="gumla"
                    |df_JH$Constituency_Name=="kolebira"
                    |df_JH$Constituency_Name=="simdega"
                    |df_JH$Constituency_Name=="sisai"
                    |df_JH$Constituency_Name=="torpa" & df_JH$before_delim==1]<- "gumla"

df_JH$District_Name[df_JH$Constituency_Name=="barhi"
                    |df_JH$Constituency_Name=="barkagaon"
                    |df_JH$Constituency_Name=="barkatha"
                    |df_JH$Constituency_Name=="hazaribagh"
                    |df_JH$Constituency_Name=="mandu"
                    |df_JH$Constituency_Name=="ramgarh" & df_JH$before_delim==1]<- "hazaribagh"

df_JH$District_Name[df_JH$Constituency_Name=="barkatha"
                    |df_JH$Constituency_Name=="kodarma" & df_JH$before_delim==1]<- "kodarma"

df_JH$District_Name[df_JH$Constituency_Name=="lohardaga" & df_JH$before_delim==1]<- "lohardaga"

df_JH$District_Name[df_JH$Constituency_Name=="litipara"
                    |df_JH$Constituency_Name=="mahespur"
                    |df_JH$Constituency_Name=="pakaur" & df_JH$before_delim==1]<- "pakaur"

df_JH$Constituency_Name[df_JH$Constituency_Name=="chattarpur"]<- "chhatarpur"

df_JH$District_Name[df_JH$Constituency_Name=="chhatarpur"
                    |df_JH$Constituency_Name=="daltonganj"
                    |df_JH$Constituency_Name=="hussainabad"
                    |df_JH$Constituency_Name=="latehar"
                    |df_JH$Constituency_Name=="manika"
                    |df_JH$Constituency_Name=="panki" & df_JH$before_delim==1]<- "palamu"

df_JH$Constituency_Name[df_JH$Constituency_Name=="kharasawan"]<- "kharsawan"
df_JH$Constituency_Name[df_JH$Constituency_Name=="majhganon"]<- "majhgaon"

df_JH$District_Name[df_JH$Constituency_Name=="chaibasa"
                    |df_JH$Constituency_Name=="chakradharpur"
                    |df_JH$Constituency_Name=="ichagarh"
                    |df_JH$Constituency_Name=="jaganathpur"
                    |df_JH$Constituency_Name=="kharsawan"
                    |df_JH$Constituency_Name=="majhgaon"
                    |df_JH$Constituency_Name=="manoharpur"
                    |df_JH$Constituency_Name=="seraikella" & df_JH$before_delim==1]<- "pashchimi singhbhum"

df_JH$Constituency_Name[df_JH$Constituency_Name=="ghatshila"]<- "ghatsila"
df_JH$Constituency_Name[df_JH$Constituency_Name=="jugashlai"]<- "jugsalai"

df_JH$District_Name[df_JH$Constituency_Name=="baharagora"
                    |df_JH$Constituency_Name=="ghatsila"
                    |df_JH$Constituency_Name=="jugsalai"
                    |df_JH$Constituency_Name=="potka" & df_JH$before_delim==1]<- "purbi singbhum"

df_JH$District_Name[df_JH$Constituency_Name=="hatia"
                    |df_JH$Constituency_Name=="kanke"
                    |df_JH$Constituency_Name=="khijri"
                    |df_JH$Constituency_Name=="khunti"
                    |df_JH$Constituency_Name=="mandar"
                    |df_JH$Constituency_Name=="ranchi"
                    |df_JH$Constituency_Name=="silli"
                    |df_JH$Constituency_Name=="tamar"
                    |df_JH$Constituency_Name=="torpa" & df_JH$before_delim==1]<- "Ranchi"

df_JH$District_Name[df_JH$Constituency_Name=="barhait"
                    |df_JH$Constituency_Name=="borio"
                    |df_JH$Constituency_Name=="rajmahal" & df_JH$before_delim==1]<- "sahibganj"

df_JH<- df_JH%>%
  subset(District_Name!="")

################################ Karnataka###########################################

df_KR<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Karnataka_2021-11-23.csv")

df_KR<- df_KR%>%
  subset(Year==1989|
           Year==1994|
           Year==1999|
           Year==2004|
           Year==2008|
           Year==2013)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_KR$District_Name[df_KR$Constituency_Name=="bagalkot"
                    |df_KR$Constituency_Name=="bilgi"
                    |df_KR$Constituency_Name=="guledgud"
                    |df_KR$Constituency_Name=="hungund"
                    |df_KR$Constituency_Name=="jamkhandi"
                    |df_KR$Constituency_Name=="mudhol"
                    |df_KR$Constituency_Name=="badami" & df_KR$before_delim==1]<- "bagalkot"

df_KR$District_Name[df_KR$Constituency_Name=="anekal"
                    |df_KR$Constituency_Name=="devanahalli"
                    |df_KR$Constituency_Name=="hosakote"
                    |df_KR$Constituency_Name=="magadi"
                    |df_KR$Constituency_Name=="nelamangala"
                    |df_KR$Constituency_Name=="uttarahalli"
                    |df_KR$Constituency_Name=="varthur"
                    |df_KR$Constituency_Name=="basavanagudi"
                    |df_KR$Constituency_Name=="binnypet"
                    |df_KR$Constituency_Name=="chamarajapet"
                    |df_KR$Constituency_Name=="chickpet"
                    |df_KR$Constituency_Name=="gandhinagar"
                    |df_KR$Constituency_Name=="jayanagar"
                    |df_KR$Constituency_Name=="malleswaram"
                    |df_KR$Constituency_Name=="rajajinagar" & df_KR$before_delim==1]<- "bangalore"

df_KR$District_Name[df_KR$Constituency_Name=="channapatna"
                    |df_KR$Constituency_Name=="devanahalli"
                    |df_KR$Constituency_Name=="doddaballapur"
                    |df_KR$Constituency_Name=="hosakote"
                    |df_KR$Constituency_Name=="kanakapura"
                    |df_KR$Constituency_Name=="koratagere"
                    |df_KR$Constituency_Name=="magadi"
                    |df_KR$Constituency_Name=="nagamangala"
                    |df_KR$Constituency_Name=="ramanagaram"
                    |df_KR$Constituency_Name=="sathanur" & df_KR$before_delim==1]<- "bangalore rural"

df_KR$District_Name[df_KR$Constituency_Name=="arabhavi"
                    |df_KR$Constituency_Name=="athani"
                    |df_KR$Constituency_Name=="bagewadi"
                    |df_KR$Constituency_Name=="chikkodi"
                    |df_KR$Constituency_Name=="gokak"
                    |df_KR$Constituency_Name=="hukkeri"
                    |df_KR$Constituency_Name=="kagwad"
                    |df_KR$Constituency_Name=="khanapur"
                    |df_KR$Constituency_Name=="kittur"
                    |df_KR$Constituency_Name=="parasgad"
                    |df_KR$Constituency_Name=="raibag"
                    |df_KR$Constituency_Name=="ramdurg"
                    |df_KR$Constituency_Name=="sadalga"
                    |df_KR$Constituency_Name=="sankeshwar"
                    |df_KR$Constituency_Name=="bailhongal"
                    |df_KR$Constituency_Name=="belgaum"
                    |df_KR$Constituency_Name=="nippani"
                    |df_KR$Constituency_Name=="uchagaon" & df_KR$before_delim==1]<- "belgaum"

df_KR$District_Name[df_KR$Constituency_Name=="bellary"
                    |df_KR$Constituency_Name=="hadagalli"
                    |df_KR$Constituency_Name=="hospet"
                    |df_KR$Constituency_Name=="kudligi"
                    |df_KR$Constituency_Name=="sandur"
                    |df_KR$Constituency_Name=="siruguppa"
                    |df_KR$Constituency_Name=="kottur"
                    |df_KR$Constituency_Name=="kurugodu" & df_KR$before_delim==1]<- "bellary"

df_KR$District_Name[df_KR$Constituency_Name=="aurad"
                    |df_KR$Constituency_Name=="basavakalyan"
                    |df_KR$Constituency_Name=="bhalki"
                    |df_KR$Constituency_Name=="bidar"
                    |df_KR$Constituency_Name=="hulsoor"
                    |df_KR$Constituency_Name=="humnabad" & df_KR$before_delim==1]<- "bidar"

df_KR$District_Name[df_KR$Constituency_Name=="ballolli"
                    |df_KR$Constituency_Name=="basavana-bagewadi"
                    |df_KR$Constituency_Name=="bijapur"
                    |df_KR$Constituency_Name=="huvin-hippargi"
                    |df_KR$Constituency_Name=="indi"
                    |df_KR$Constituency_Name=="muddebihal"
                    |df_KR$Constituency_Name=="sindgi"
                    |df_KR$Constituency_Name=="tikota" & df_KR$before_delim==1]<- "bijapur"

df_KR$District_Name[df_KR$Constituency_Name=="chamarajanagar"
                    |df_KR$Constituency_Name=="gundlupet"
                    |df_KR$Constituency_Name=="hanur"
                    |df_KR$Constituency_Name=="kollegal"
                    |df_KR$Constituency_Name=="santhemarahalli"
                    |df_KR$Constituency_Name=="chamaraja" & df_KR$before_delim==1]<- "chamarajanagar"

df_KR$District_Name[df_KR$Constituency_Name=="belur"
                    |df_KR$Constituency_Name=="birur"
                    |df_KR$Constituency_Name=="chikmagalur"
                    |df_KR$Constituency_Name=="kadur"
                    |df_KR$Constituency_Name=="mudigere"
                    |df_KR$Constituency_Name=="tarikere"
                    |df_KR$Constituency_Name=="sringeri" & df_KR$before_delim==1]<- "chikmagalur"

df_KR$District_Name[df_KR$Constituency_Name=="bharamasagara"
                    |df_KR$Constituency_Name=="challakere"
                    |df_KR$Constituency_Name=="chitradurga"
                    |df_KR$Constituency_Name=="hiriyur"
                    |df_KR$Constituency_Name=="holalkere"
                    |df_KR$Constituency_Name=="hosadurga"
                    |df_KR$Constituency_Name=="molakalmuru" & df_KR$before_delim==1]<- "chitradurga"

df_KR$District_Name[df_KR$Constituency_Name=="bantwal"
                    |df_KR$Constituency_Name=="belthangady"
                    |df_KR$Constituency_Name=="moodabidri"
                    |df_KR$Constituency_Name=="puttur"
                    |df_KR$Constituency_Name=="sullia"
                    |df_KR$Constituency_Name=="vittal"
                    |df_KR$Constituency_Name=="surathkal"
                    |df_KR$Constituency_Name=="ullal" & df_KR$before_delim==1]<- "dakshina kannada"

df_KR$District_Name[df_KR$Constituency_Name=="bharamasagara"
                    |df_KR$Constituency_Name=="channagiri"
                    |df_KR$Constituency_Name=="davangere"
                    |df_KR$Constituency_Name=="harapanahalli"
                    |df_KR$Constituency_Name=="harihar"
                    |df_KR$Constituency_Name=="holehonnur"
                    |df_KR$Constituency_Name=="honnali"
                    |df_KR$Constituency_Name=="jagalur"
                    |df_KR$Constituency_Name=="mayakonda" & df_KR$before_delim==1]<- "davanagere"

df_KR$District_Name[df_KR$Constituency_Name=="dharwad"
                    |df_KR$Constituency_Name=="dharwad rural"
                    |df_KR$Constituency_Name=="hubli"
                    |df_KR$Constituency_Name=="hubli rural"
                    |df_KR$Constituency_Name=="kalghatgi"
                    |df_KR$Constituency_Name=="kundgol"
                    |df_KR$Constituency_Name=="navalgund" & df_KR$before_delim==1]<- "dharwad"

df_KR$District_Name[df_KR$Constituency_Name=="gadag"
                    |df_KR$Constituency_Name=="mundargi"
                    |df_KR$Constituency_Name=="naragund"
                    |df_KR$Constituency_Name=="ron"
                    |df_KR$Constituency_Name=="shirhatti"
                    |df_KR$Constituency_Name=="yelburga" & df_KR$before_delim==1]<- "gadag"

df_KR$District_Name[df_KR$Constituency_Name=="afzalpur"
                    |df_KR$Constituency_Name=="alland"
                    |df_KR$Constituency_Name=="chincholi"
                    |df_KR$Constituency_Name=="chitapur"
                    |df_KR$Constituency_Name=="gulbarga"
                    |df_KR$Constituency_Name=="gurmitkal"
                    |df_KR$Constituency_Name=="jewargi"
                    |df_KR$Constituency_Name=="kamalapur"
                    |df_KR$Constituency_Name=="sedam"
                    |df_KR$Constituency_Name=="shahapur"
                    |df_KR$Constituency_Name=="shorapur"
                    |df_KR$Constituency_Name=="yadgir"
                    |df_KR$Constituency_Name=="shahabad" & df_KR$before_delim==1]<- "gulbarga"

df_KR$District_Name[df_KR$Constituency_Name=="arsikere"
                    |df_KR$Constituency_Name=="arkalgud"
                    |df_KR$Constituency_Name=="belur"
                    |df_KR$Constituency_Name=="gandsi"
                    |df_KR$Constituency_Name=="hassan"
                    |df_KR$Constituency_Name=="holenarasipur"
                    |df_KR$Constituency_Name=="sakleshpur"
                    |df_KR$Constituency_Name=="somwarpet"
                    |df_KR$Constituency_Name=="sravanabelagola" & df_KR$before_delim==1]<- "hassan"

df_KR$District_Name[df_KR$Constituency_Name=="byadgi"
                    |df_KR$Constituency_Name=="hanagal"
                    |df_KR$Constituency_Name=="haveri"
                    |df_KR$Constituency_Name=="hirekerur"
                    |df_KR$Constituency_Name=="ranibennur"
                    |df_KR$Constituency_Name=="shiggaon"
                    |df_KR$Constituency_Name=="sorab" & df_KR$before_delim==1]<- "haveri"

df_KR$District_Name[df_KR$Constituency_Name=="madikere"
                    |df_KR$Constituency_Name=="somwarpet"
                    |df_KR$Constituency_Name=="virajpet" & df_KR$before_delim==1]<- "kodagu"

df_KR$District_Name[df_KR$Constituency_Name=="bagepalli"
                    |df_KR$Constituency_Name=="bethamangala"
                    |df_KR$Constituency_Name=="chikballapur"
                    |df_KR$Constituency_Name=="chintamani"
                    |df_KR$Constituency_Name=="gowribidanur"
                    |df_KR$Constituency_Name=="kolar"
                    |df_KR$Constituency_Name=="kolar gold fields"
                    |df_KR$Constituency_Name=="malur"
                    |df_KR$Constituency_Name=="mulbagal"
                    |df_KR$Constituency_Name=="sidlaghatta"
                    |df_KR$Constituency_Name=="srinivasapur"
                    |df_KR$Constituency_Name=="vemgal" & df_KR$before_delim==1]<- "kolar"

df_KR$District_Name[df_KR$Constituency_Name=="gangavathi"
                    |df_KR$Constituency_Name=="kanakagiri"
                    |df_KR$Constituency_Name=="koppal"
                    |df_KR$Constituency_Name=="kushtagi"
                    |df_KR$Constituency_Name=="yelburga" & df_KR$before_delim==1]<- "koppal"

df_KR$District_Name[df_KR$Constituency_Name=="keragodu"
                    |df_KR$Constituency_Name=="kiragaval"
                    |df_KR$Constituency_Name=="krishnarajpet"
                    |df_KR$Constituency_Name=="maddur"
                    |df_KR$Constituency_Name=="malavalli"
                    |df_KR$Constituency_Name=="mandya"
                    |df_KR$Constituency_Name=="nagamangala"
                    |df_KR$Constituency_Name=="pandavapura"
                    |df_KR$Constituency_Name=="srirangapatna" & df_KR$before_delim==1]<- "mandya"

df_KR$Constituency_Name[df_KR$Constituency_Name=="krishnaraj"]<- "krishnarajanagar"

df_KR$District_Name[df_KR$Constituency_Name=="bannur"
                    |df_KR$Constituency_Name=="chamundeswari"
                    |df_KR$Constituency_Name=="gundlupet"
                    |df_KR$Constituency_Name=="heggadadevanakote"
                    |df_KR$Constituency_Name=="hunsur"
                    |df_KR$Constituency_Name=="krishnarajanagar"
                    |df_KR$Constituency_Name=="nanjangud"
                    |df_KR$Constituency_Name=="periyapatna"
                    |df_KR$Constituency_Name=="t. narasipur" & df_KR$before_delim==1]<- "mysore"

df_KR$District_Name[df_KR$Constituency_Name=="deodurg"
                    |df_KR$Constituency_Name=="kalmala"
                    |df_KR$Constituency_Name=="kushtagi"
                    |df_KR$Constituency_Name=="lingsugur"
                    |df_KR$Constituency_Name=="manvi"
                    |df_KR$Constituency_Name=="raichur"
                    |df_KR$Constituency_Name=="sindhnoor" & df_KR$before_delim==1]<- "raichur"

df_KR$District_Name[df_KR$Constituency_Name=="bhadravathi"
                    |df_KR$Constituency_Name=="holehonnur"
                    |df_KR$Constituency_Name=="hosanagar"
                    |df_KR$Constituency_Name=="sagar"
                    |df_KR$Constituency_Name=="shikaripur"
                    |df_KR$Constituency_Name=="shimoga"
                    |df_KR$Constituency_Name=="sorab"
                    |df_KR$Constituency_Name=="tirthahalli" & df_KR$before_delim==1]<- "shimoga"

df_KR$District_Name[df_KR$Constituency_Name=="bellavi"
                    |df_KR$Constituency_Name=="chiknaikanahalli"
                    |df_KR$Constituency_Name=="gubbi"
                    |df_KR$Constituency_Name=="huliyurdurga"
                    |df_KR$Constituency_Name=="kallambella"
                    |df_KR$Constituency_Name=="koratagere"
                    |df_KR$Constituency_Name=="kunigal"
                    |df_KR$Constituency_Name=="madhugiri"
                    |df_KR$Constituency_Name=="pavagada"
                    |df_KR$Constituency_Name=="sira"
                    |df_KR$Constituency_Name=="tiptur"
                    |df_KR$Constituency_Name=="tumkur"
                    |df_KR$Constituency_Name=="turuvekere" & df_KR$before_delim==1]<- "tumkur"

df_KR$District_Name[df_KR$Constituency_Name=="baindur"
                    |df_KR$Constituency_Name=="brahmavar"
                    |df_KR$Constituency_Name=="coondapur"
                    |df_KR$Constituency_Name=="karkal"
                    |df_KR$Constituency_Name=="kaup"
                    |df_KR$Constituency_Name=="udupi" & df_KR$before_delim==1]<- "udupi"

df_KR$District_Name[df_KR$Constituency_Name=="ankola"
                    |df_KR$Constituency_Name=="bhatkal"
                    |df_KR$Constituency_Name=="haliyal"
                    |df_KR$Constituency_Name=="karwar"
                    |df_KR$Constituency_Name=="kumta"
                    |df_KR$Constituency_Name=="sirsi" & df_KR$before_delim==1]<- "uttara kannada"

df_KR$District_Name[df_KR$District_Name=="chickmagalur"]<- "chikmagalur"

df_KR<- df_KR%>%
  subset(District_Name!="")

######################### Kerala #####################################

df_KL<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Kerala_2021-11-23.csv")

df_KL<- df_KL%>%
  subset(Year==1991|
           Year==1996|
           Year==2001|
           Year==2006|
           Year==2011|
           Year==2016)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))


df_KL$Constituency_Name[df_KL$Constituency_Name=="karungapally"]<- "karunagapally"
df_KL$Constituency_Name[df_KL$Constituency_Name=="shertalai"]<- "sherthalai"

df_KL$District_Name[df_KL$Constituency_Name=="ambalapuzha"
                    |df_KL$Constituency_Name=="aranmula"
                    |df_KL$Constituency_Name=="aroor"
                    |df_KL$Constituency_Name=="chengannur"
                    |df_KL$Constituency_Name=="haripad"
                    |df_KL$Constituency_Name=="karunagapally"
                    |df_KL$Constituency_Name=="kayamkulam"
                    |df_KL$Constituency_Name=="kunnathur"
                    |df_KL$Constituency_Name=="kuttanad"
                    |df_KL$Constituency_Name=="mararikulam"
                    |df_KL$Constituency_Name=="mavelikara"
                    |df_KL$Constituency_Name=="pandalam"
                    |df_KL$Constituency_Name=="sherthalai"
                    |df_KL$Constituency_Name=="thiruvalla" & df_KL$before_delim==1]<- "alappuzha"

df_KL$Constituency_Name[df_KL$Constituency_Name=="thrippunithura"]<- "trippunithura"

df_KL$District_Name[df_KL$Constituency_Name=="alwaye"
                    |df_KL$Constituency_Name=="ankamali"
                    |df_KL$Constituency_Name=="ernakulam"
                    |df_KL$Constituency_Name=="kothamangalam"
                    |df_KL$Constituency_Name=="kunnathunad"
                    |df_KL$Constituency_Name=="mattancherry"
                    |df_KL$Constituency_Name=="muvattupuzha"
                    |df_KL$Constituency_Name=="narakal"
                    |df_KL$Constituency_Name=="palai"
                    |df_KL$Constituency_Name=="palluruthy"
                    |df_KL$Constituency_Name=="parur"
                    |df_KL$Constituency_Name=="perumbavoor"
                    |df_KL$Constituency_Name=="piravom"
                    |df_KL$Constituency_Name=="vadakkekara"
                    |df_KL$Constituency_Name=="mattanchery"
                    |df_KL$Constituency_Name=="trippunithura" & df_KL$before_delim==1]<- "ernakulam"

df_KL$District_Name[df_KL$Constituency_Name=="devicolam"
                    |df_KL$Constituency_Name=="idukki"
                    |df_KL$Constituency_Name=="peermade"
                    |df_KL$Constituency_Name=="thodupuzha"
                    |df_KL$Constituency_Name=="udumbanchola" & df_KL$before_delim==1]<- "idukki"

df_KL$District_Name[df_KL$Constituency_Name=="azhicode"
                    |df_KL$Constituency_Name=="irikkur"
                    |df_KL$Constituency_Name=="kuthuparamba"
                    |df_KL$Constituency_Name=="payyannur"
                    |df_KL$Constituency_Name=="peravoor"
                    |df_KL$Constituency_Name=="peringalam"
                    |df_KL$Constituency_Name=="taliparamba"
                    |df_KL$Constituency_Name=="trikkaripur"
                    |df_KL$Constituency_Name=="cannanore"
                    |df_KL$Constituency_Name=="edakkad" & df_KL$before_delim==1]<- "kannur"

df_KL$Constituency_Name[df_KL$Constituency_Name=="trikkaripur"]<- "trikkarpur"

df_KL$District_Name[df_KL$Constituency_Name=="hosdrug"
                    |df_KL$Constituency_Name=="kasaragod"
                    |df_KL$Constituency_Name=="manjeswar"
                    |df_KL$Constituency_Name=="trikkarpur"
                    |df_KL$Constituency_Name=="udma" & df_KL$before_delim==1]<- "kasaragod"

df_KL$Constituency_Name[df_KL$Constituency_Name=="punalur"]<- "punaloor"

df_KL$District_Name[df_KL$Constituency_Name=="chadayamangalam"
                    |df_KL$Constituency_Name=="chathanoor"
                    |df_KL$Constituency_Name=="kundara"
                    |df_KL$Constituency_Name=="neduvathur"
                    |df_KL$Constituency_Name=="punaloor"
                    |df_KL$Constituency_Name=="varkala"
                    |df_KL$Constituency_Name=="chavara"
                    |df_KL$Constituency_Name=="eravipuram"
                    |df_KL$Constituency_Name=="kottarakara"
                    |df_KL$Constituency_Name=="pathanapuram"
                    |df_KL$Constituency_Name=="quilon" & df_KL$before_delim==1]<- "kollam"

df_KL$District_Name[df_KL$Constituency_Name=="kaduthuruthy"
                    |df_KL$Constituency_Name=="kallooppara"
                    |df_KL$Constituency_Name=="kanjirappally "
                    |df_KL$Constituency_Name=="kottayam"
                    |df_KL$Constituency_Name=="palai"
                    |df_KL$Constituency_Name=="poonjar"
                    |df_KL$Constituency_Name=="puthuppally"
                    |df_KL$Constituency_Name=="thiruvalla"
                    |df_KL$Constituency_Name=="vazhoor"
                    |df_KL$Constituency_Name=="changanacherry"
                    |df_KL$Constituency_Name=="ettumanoor"
                    |df_KL$Constituency_Name=="kanjirappally"
                    |df_KL$Constituency_Name=="vaikom" & df_KL$before_delim==1]<- "kottayam"

df_KL$District_Name[df_KL$Constituency_Name=="balusseri"
                    |df_KL$Constituency_Name=="koduvally"
                    |df_KL$Constituency_Name=="kunnamangalam"
                    |df_KL$Constituency_Name=="meppayur"
                    |df_KL$Constituency_Name=="nadapuram"
                    |df_KL$Constituency_Name=="perambra"
                    |df_KL$Constituency_Name=="quilandy"
                    |df_KL$Constituency_Name=="thiruvambadi"
                    |df_KL$Constituency_Name=="badagara"
                    |df_KL$Constituency_Name=="beypore"
                    |df_KL$Constituency_Name=="calicut- i"
                    |df_KL$Constituency_Name=="calicut- ii" & df_KL$before_delim==1]<- "kozhikode"

df_KL$District_Name[df_KL$Constituency_Name=="tanur"
                    |df_KL$Constituency_Name=="kondotty"
                    |df_KL$Constituency_Name=="kuttippuram"
                    |df_KL$Constituency_Name=="malappuram"
                    |df_KL$Constituency_Name=="manjeri"
                    |df_KL$Constituency_Name=="mankada"
                    |df_KL$Constituency_Name=="nilambur"
                    |df_KL$Constituency_Name=="perinthalmanna"
                    |df_KL$Constituency_Name=="ponnani"
                    |df_KL$Constituency_Name=="tirur"
                    |df_KL$Constituency_Name=="wandoor"
                    |df_KL$Constituency_Name=="nilambuk" & df_KL$before_delim==1]<- "malappuram"

df_KL$District_Name[df_KL$Constituency_Name=="alathur"
                    |df_KL$Constituency_Name=="chittur"
                    |df_KL$Constituency_Name=="coyalmannam"
                    |df_KL$Constituency_Name=="kollengode"
                    |df_KL$Constituency_Name=="malampuzha"
                    |df_KL$Constituency_Name=="mannarkkad"
                    |df_KL$Constituency_Name=="ottapalam"
                    |df_KL$Constituency_Name=="palghat"
                    |df_KL$Constituency_Name=="pattambi"
                    |df_KL$Constituency_Name=="sreekrishnapuram"
                    |df_KL$Constituency_Name=="thrithala" & df_KL$before_delim==1]<- "palakkad"

df_KL$District_Name[df_KL$Constituency_Name=="aranmula"
                    |df_KL$Constituency_Name=="konni"
                    |df_KL$Constituency_Name=="pathanamthitta"
                    |df_KL$Constituency_Name=="ranni"
                    |df_KL$Constituency_Name=="adoor" & df_KL$before_delim==1]<- "pathanamthitta"

df_KL$District_Name[df_KL$Constituency_Name=="ariyanad"
                    |df_KL$Constituency_Name=="attingal"
                    |df_KL$Constituency_Name=="kazhakuttam"
                    |df_KL$Constituency_Name=="nedumangad"
                    |df_KL$Constituency_Name=="nemom"
                    |df_KL$Constituency_Name=="neyyattinkara"
                    |df_KL$Constituency_Name=="parassala"
                    |df_KL$Constituency_Name=="trivandrum east"
                    |df_KL$Constituency_Name=="trivandrum west"
                    |df_KL$Constituency_Name=="trivandrum north"
                    |df_KL$Constituency_Name=="vamanapuram"
                    |df_KL$Constituency_Name=="kilimanoor"
                    |df_KL$Constituency_Name=="kovalam" & df_KL$before_delim==1]<- "thiruvananthapuram"

df_KL$District_Name[df_KL$Constituency_Name=="ankamali"
                    |df_KL$Constituency_Name=="chalakudi"
                    |df_KL$Constituency_Name=="chelakara"
                    |df_KL$Constituency_Name=="irinjalakuda"
                    |df_KL$Constituency_Name=="kodakara"
                    |df_KL$Constituency_Name=="kodungallur"
                    |df_KL$Constituency_Name=="kunnamkulam"
                    |df_KL$Constituency_Name=="mala"
                    |df_KL$Constituency_Name=="manalur"
                    |df_KL$Constituency_Name=="nattika"
                    |df_KL$Constituency_Name=="ollur"
                    |df_KL$Constituency_Name=="parur"
                    |df_KL$Constituency_Name=="trichur"
                    |df_KL$Constituency_Name=="wadakkancherry"
                    |df_KL$Constituency_Name=="cherpu"
                    |df_KL$Constituency_Name=="guruvayoor" & df_KL$before_delim==1]<- "thrissur"

df_KL$District_Name[df_KL$Constituency_Name=="kalpetta"
                    |df_KL$Constituency_Name=="north wynad"
                    |df_KL$Constituency_Name=="sultan's battery" & df_KL$before_delim==1]<- "wayanad"

df_KL<- df_KL%>%
  subset(District_Name!="")

##################################### Jammu and Kashmir ######################################

df_JK<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Jammu_&_Kashmir_2021-11-23.csv")

df_JK<- df_JK%>%
  subset(Year==1996|
           Year==2002|
           Year==2008|
           Year==2014)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_JK$Constituency_Name[df_JK$Constituency_Name=="bijbehera"]<- "bijbehara"
df_JK$Constituency_Name[df_JK$Constituency_Name=="homshalibug"]<- "homeshalibugh"
df_JK$Constituency_Name[df_JK$Constituency_Name=="shawngas"]<- "shangus"

df_JK$District_Name[df_JK$Constituency_Name=="anantnag"
                    |df_JK$Constituency_Name=="bijbehara"
                    |df_JK$Constituency_Name=="devsar"
                    |df_JK$Constituency_Name=="doru"
                    |df_JK$Constituency_Name=="homeshalibugh"
                    |df_JK$Constituency_Name=="kokernag"
                    |df_JK$Constituency_Name=="kulgam"
                    |df_JK$Constituency_Name=="pahalgam"
                    |df_JK$Constituency_Name=="shangus" & df_JK$before_delim==1]<- "anantnag"

df_JK$Constituency_Name[df_JK$Constituency_Name=="chadura"]<- "chadoora"
df_JK$Constituency_Name[df_JK$Constituency_Name=="khansahib"]<- "khan sahib"
df_JK$Constituency_Name[df_JK$Constituency_Name=="chrari sharief"]<- "chari sharif"

df_JK$District_Name[df_JK$Constituency_Name=="badgam"
                    |df_JK$Constituency_Name=="batamaloo"
                    |df_JK$Constituency_Name=="beerwah"
                    |df_JK$Constituency_Name=="chadoora"
                    |df_JK$Constituency_Name=="chari sharif"
                    |df_JK$Constituency_Name=="khan sahib"
                    |df_JK$Constituency_Name=="khanyar"
                    |df_JK$Constituency_Name=="rajpora"
                    |df_JK$Constituency_Name=="sonawar" & df_JK$before_delim==1]<- "badgam"

df_JK$Constituency_Name[df_JK$Constituency_Name=="rafibad"]<- "rafiabad"
df_JK$Constituency_Name[df_JK$Constituency_Name=="refiabad"]<- "rafiabad"
df_JK$Constituency_Name[df_JK$Constituency_Name=="refibad"]<- "rafiabad"

df_JK$District_Name[df_JK$Constituency_Name=="baramulla"
                    |df_JK$Constituency_Name=="bandipora"
                    |df_JK$Constituency_Name=="gulmarg"
                    |df_JK$Constituency_Name=="gurez"
                    |df_JK$Constituency_Name=="lolab"
                    |df_JK$Constituency_Name=="pattan"
                    |df_JK$Constituency_Name=="rafiabad"
                    |df_JK$Constituency_Name=="sangrama"
                    |df_JK$Constituency_Name=="sonawari"
                    |df_JK$Constituency_Name=="sopore"
                    |df_JK$Constituency_Name=="uri" & df_JK$before_delim==1]<- "baramula"

df_JK$Constituency_Name[df_JK$Constituency_Name=="kistwar"]<- "kishtwar"
df_JK$Constituency_Name[df_JK$Constituency_Name=="bhaderwah"]<- "bhadarwah"

df_JK$District_Name[df_JK$Constituency_Name=="banihal"
                    |df_JK$Constituency_Name=="bhadarwah"
                    |df_JK$Constituency_Name=="doda"
                    |df_JK$Constituency_Name=="inderwal"
                    |df_JK$Constituency_Name=="kishtwar"
                    |df_JK$Constituency_Name=="ramban" & df_JK$before_delim==1]<- "doda"

df_JK$Constituency_Name[df_JK$Constituency_Name=="bishna"]<- "bishnah"
df_JK$Constituency_Name[df_JK$Constituency_Name=="chhamba"]<- "chhamb"
df_JK$Constituency_Name[df_JK$Constituency_Name=="jammu contonment"]<- "jammu cantonment"
df_JK$Constituency_Name[df_JK$Constituency_Name=="ranbir singh pura"|df_JK$Constituency_Name=="ranbirsingh pura"]<- "r.s. pura"

df_JK$District_Name[df_JK$Constituency_Name=="akhnoor"
                    |df_JK$Constituency_Name=="bishnah"
                    |df_JK$Constituency_Name=="chhamb"
                    |df_JK$Constituency_Name=="gandhinagar"
                    |df_JK$Constituency_Name=="jammu cantonment"
                    |df_JK$Constituency_Name=="jammu east"
                    |df_JK$Constituency_Name=="jammu west"
                    |df_JK$Constituency_Name=="marh"
                    |df_JK$Constituency_Name=="nagrota"
                    |df_JK$Constituency_Name=="r.s. pura"
                    |df_JK$Constituency_Name=="raipur domana"
                    |df_JK$Constituency_Name=="samba"
                    |df_JK$Constituency_Name=="suchetgarh"
                    |df_JK$Constituency_Name=="vijaypur" & df_JK$before_delim==1]<- "jammu"

df_JK$District_Name[df_JK$Constituency_Name=="kargil"
                    |df_JK$Constituency_Name=="nobra"
                    |df_JK$Constituency_Name=="zanskar" & df_JK$before_delim==1]<- "kargil"

df_JK$Constituency_Name[df_JK$Constituency_Name=="bhillawar"]<- "billawar"

df_JK$District_Name[df_JK$Constituency_Name=="bani"
                    |df_JK$Constituency_Name=="basohli"
                    |df_JK$Constituency_Name=="billawar"
                    |df_JK$Constituency_Name=="hiranagar"
                    |df_JK$Constituency_Name=="kathua" & df_JK$before_delim==1]<- "kathua"

df_JK$Constituency_Name[df_JK$Constituency_Name=="langet"]<- "langate"

df_JK$District_Name[df_JK$Constituency_Name=="handwara"
                    |df_JK$Constituency_Name=="karnah"
                    |df_JK$Constituency_Name=="kupwara"
                    |df_JK$Constituency_Name=="langate" & df_JK$before_delim==1]<- "kupwara"

df_JK$District_Name[df_JK$Constituency_Name=="leh"]<- "leh"

df_JK$District_Name[df_JK$Constituency_Name=="pampore"
                    |df_JK$Constituency_Name=="pulwama"
                    |df_JK$Constituency_Name=="tral"
                    |df_JK$Constituency_Name=="wachi" & df_JK$before_delim==1]<- "pulwama"

df_JK$District_Name[df_JK$Constituency_Name=="mendhar"
                    |df_JK$Constituency_Name=="poonch haveli"
                    |df_JK$Constituency_Name=="surankote" & df_JK$before_delim==1]<- "punch"

df_JK$District_Name[df_JK$Constituency_Name=="darhal"
                    |df_JK$Constituency_Name=="kalakote"
                    |df_JK$Constituency_Name=="nowshera"
                    |df_JK$Constituency_Name=="rajouri" & df_JK$before_delim==1]<- "rajauri"

df_JK$Constituency_Name[df_JK$Constituency_Name=="habakadal"]<- "habbakadal"
df_JK$Constituency_Name[df_JK$Constituency_Name=="hazaratbal"]<- "hazratbal"

df_JK$District_Name[df_JK$Constituency_Name=="amirakadal"
                    |df_JK$Constituency_Name=="ganderbal"
                    |df_JK$Constituency_Name=="habbakadal"
                    |df_JK$Constituency_Name=="hazratbal"
                    |df_JK$Constituency_Name=="kangan" & df_JK$before_delim==1]<- "srinagar"

df_JK$Constituency_Name[df_JK$Constituency_Name=="gulab garh"]<- "gulabgarh"

df_JK$District_Name[df_JK$Constituency_Name=="chanani"
                    |df_JK$Constituency_Name=="noorabad"
                    |df_JK$Constituency_Name=="ramnagar"
                    |df_JK$Constituency_Name=="reasi"
                    |df_JK$Constituency_Name=="udhampur"
                    |df_JK$Constituency_Name=="gool arnas"
                    |df_JK$Constituency_Name=="gulabgarh" & df_JK$before_delim==1]<- "udhampur"

df_JK<- df_JK%>%
  subset(District_Name!="")

######################################## Madhya Pradesh ######################################################

df_MP<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Madhya_Pradesh_2021-11-23.csv")

df_MP<- df_MP%>%
  subset(Year==1990|
           Year==1993|
           Year==1998|
           Year==2003|
           Year==2008|
           Year==2013)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_MP$Constituency_Name[df_MP$Constituency_Name=="khairlanjee"|df_MP$Constituency_Name=="khairlanji"]<- "khairalanjee"

df_MP$District_Name[df_MP$Constituency_Name=="baihar"
                    |df_MP$Constituency_Name=="balaghat"
                    |df_MP$Constituency_Name=="katangi"
                    |df_MP$Constituency_Name=="khairalanjee"
                    |df_MP$Constituency_Name=="kirnapur"
                    |df_MP$Constituency_Name=="lanji"
                    |df_MP$Constituency_Name=="paraswada"
                    |df_MP$Constituency_Name=="waraseoni"
                    |df_MP$Constituency_Name=="birendranagar" & df_MP$before_delim==1]<- "balaghat"

df_MP$District_Name[df_MP$Constituency_Name=="anjad"
                    |df_MP$Constituency_Name=="barwani"
                    |df_MP$Constituency_Name=="rajpur"
                    |df_MP$Constituency_Name=="sendhwa" & df_MP$before_delim==1]<- "barwani"

df_MP$District_Name[df_MP$Constituency_Name=="amla"
                    |df_MP$Constituency_Name=="betul"
                    |df_MP$Constituency_Name=="bhainsdehi"
                    |df_MP$Constituency_Name=="ghora dongri"
                    |df_MP$Constituency_Name=="masod"
                    |df_MP$Constituency_Name=="multai" & df_MP$before_delim==1]<- "betul"

df_MP$District_Name[df_MP$Constituency_Name=="attair"
                    |df_MP$Constituency_Name=="bhind"
                    |df_MP$Constituency_Name=="gohad"
                    |df_MP$Constituency_Name=="lahar"
                    |df_MP$Constituency_Name=="mehgaon"
                    |df_MP$Constituency_Name=="ron"
                    |df_MP$Constituency_Name=="ater" & df_MP$before_delim==1]<- "bhind"

df_MP$District_Name[df_MP$Constituency_Name=="berasia"
                    |df_MP$Constituency_Name=="bhopal south"
                    |df_MP$Constituency_Name=="bhopal north"
                    |df_MP$Constituency_Name=="govindpura" & df_MP$before_delim==1]<- "bhopal"

df_MP$District_Name[df_MP$Constituency_Name=="bijawar"
                    |df_MP$Constituency_Name=="chandla"
                    |df_MP$Constituency_Name=="chhatarpur"
                    |df_MP$Constituency_Name=="maharajpur"
                    |df_MP$Constituency_Name=="malehra" & df_MP$before_delim==1]<- "chhatarpur"

df_MP$District_Name[df_MP$Constituency_Name=="amarwara"
                    |df_MP$Constituency_Name=="chaurai"
                    |df_MP$Constituency_Name=="chhindwara"
                    |df_MP$Constituency_Name=="damua"
                    |df_MP$Constituency_Name=="jamai"
                    |df_MP$Constituency_Name=="pandhurna"
                    |df_MP$Constituency_Name=="parasia"
                    |df_MP$Constituency_Name=="sausar" & df_MP$before_delim==1]<- "chhindwara"

df_MP$District_Name[df_MP$Constituency_Name=="damoh"
                    |df_MP$Constituency_Name=="hatta"
                    |df_MP$Constituency_Name=="nohata"
                    |df_MP$Constituency_Name=="patharia" & df_MP$before_delim==1]<- "damoh"

df_MP$District_Name[df_MP$Constituency_Name=="datia"
                    |df_MP$Constituency_Name=="karera"
                    |df_MP$Constituency_Name=="lahar"
                    |df_MP$Constituency_Name=="pichhore"
                    |df_MP$Constituency_Name=="seondha" & df_MP$before_delim==1]<- "datia"

df_MP$Constituency_Name[df_MP$Constituency_Name=="hatpiplya"]<- "hatpipalya"

df_MP$District_Name[df_MP$Constituency_Name=="bagli"
                    |df_MP$Constituency_Name=="dewas"
                    |df_MP$Constituency_Name=="hatpipalya"
                    |df_MP$Constituency_Name=="khategaon"
                    |df_MP$Constituency_Name=="sonkatch" & df_MP$before_delim==1]<- "dewas"

df_MP$District_Name[df_MP$Constituency_Name=="badnawar"
                    |df_MP$Constituency_Name=="dhar"
                    |df_MP$Constituency_Name=="dharampuri"
                    |df_MP$Constituency_Name=="kukshi"
                    |df_MP$Constituency_Name=="manawar"
                    |df_MP$Constituency_Name=="sardarpur" & df_MP$before_delim==1]<- "dhar"

df_MP$District_Name[df_MP$Constituency_Name=="bajag"
                    |df_MP$Constituency_Name=="bichhia"
                    |df_MP$Constituency_Name=="dindori"
                    |df_MP$Constituency_Name=="niwas"
                    |df_MP$Constituency_Name=="shahpura" & df_MP$before_delim==1]<- "dindori"

df_MP$District_Name[df_MP$Constituency_Name=="burhanpur"
                    |df_MP$Constituency_Name=="harsud"
                    |df_MP$Constituency_Name=="khandwa"
                    |df_MP$Constituency_Name=="nepanagar"
                    |df_MP$Constituency_Name=="nimarkhedi"
                    |df_MP$Constituency_Name=="pandhana"
                    |df_MP$Constituency_Name=="shahpur" & df_MP$before_delim==1]<- "east nimar"

df_MP$Constituency_Name[df_MP$Constituency_Name=="ashok nagar"]<- "ashoknagar"

df_MP$District_Name[df_MP$Constituency_Name=="ashoknagar"
                    |df_MP$Constituency_Name=="chachaura"
                    |df_MP$Constituency_Name=="guna"
                    |df_MP$Constituency_Name=="mungaoli"
                    |df_MP$Constituency_Name=="raghogarh"
                    |df_MP$Constituency_Name=="shadora" & df_MP$before_delim==1]<- "guna"

df_MP$District_Name[df_MP$Constituency_Name=="bhander"
                    |df_MP$Constituency_Name=="dabra"
                    |df_MP$Constituency_Name=="gird"
                    |df_MP$Constituency_Name=="gwalior"
                    |df_MP$Constituency_Name=="lashkar east"
                    |df_MP$Constituency_Name=="lashkar west"
                    |df_MP$Constituency_Name=="morar" & df_MP$before_delim==1]<- "gwalior"

df_MP$District_Name[df_MP$Constituency_Name=="harda"
                    |df_MP$Constituency_Name=="timarni" & df_MP$before_delim==1]<- "harda"

df_MP$District_Name[df_MP$Constituency_Name=="hoshangabad"
                    |df_MP$Constituency_Name=="itarsi"
                    |df_MP$Constituency_Name=="piparia"
                    |df_MP$Constituency_Name=="seoni-malwa" & df_MP$before_delim==1]<- "hoshangabad"

df_MP$District_Name[df_MP$Constituency_Name=="depalpur"
                    |df_MP$Constituency_Name=="indore-i"
                    |df_MP$Constituency_Name=="indore-ii"
                    |df_MP$Constituency_Name=="indore-iii"
                    |df_MP$Constituency_Name=="indore-iv"
                    |df_MP$Constituency_Name=="indore-v"
                    |df_MP$Constituency_Name=="mhow"
                    |df_MP$Constituency_Name=="sawer" & df_MP$before_delim==1]<- "indore"

df_MP$District_Name[df_MP$Constituency_Name=="bargi"
                    |df_MP$Constituency_Name=="jabalpur cantonment"
                    |df_MP$Constituency_Name=="jabalpur central"
                    |df_MP$Constituency_Name=="jabalpur east"
                    |df_MP$Constituency_Name=="jabalpur west"
                    |df_MP$Constituency_Name=="majholi"
                    |df_MP$Constituency_Name=="panagar"
                    |df_MP$Constituency_Name=="patan"
                    |df_MP$Constituency_Name=="sihora" & df_MP$before_delim==1]<- "jabalpur"

df_MP$District_Name[df_MP$Constituency_Name=="alirajpur"
                    |df_MP$Constituency_Name=="jhabua"
                    |df_MP$Constituency_Name=="jobat"
                    |df_MP$Constituency_Name=="petlawad"
                    |df_MP$Constituency_Name=="thandla" & df_MP$before_delim==1]<- "jhabua"

df_MP$District_Name[df_MP$Constituency_Name=="badwara"
                    |df_MP$Constituency_Name=="bahoriband"
                    |df_MP$Constituency_Name=="murwara"
                    |df_MP$Constituency_Name=="sihora"
                    |df_MP$Constituency_Name=="vijairaghogarh"
                    |df_MP$Constituency_Name=="bahotiband" & df_MP$before_delim==1]<- "katni"

df_MP$District_Name[df_MP$Constituency_Name=="bichhia"
                    |df_MP$Constituency_Name=="dindori"
                    |df_MP$Constituency_Name=="mandla"
                    |df_MP$Constituency_Name=="nainpur"
                    |df_MP$Constituency_Name=="niwas" & df_MP$before_delim==1]<- "mandla"

df_MP$District_Name[df_MP$Constituency_Name=="garoth"
                    |df_MP$Constituency_Name=="mandsaur"
                    |df_MP$Constituency_Name=="neemuch"
                    |df_MP$Constituency_Name=="sitamau"
                    |df_MP$Constituency_Name=="suwasara" & df_MP$before_delim==1]<- "mandsaur"

df_MP$District_Name[df_MP$Constituency_Name=="ambah"
                    |df_MP$Constituency_Name=="dimni"
                    |df_MP$Constituency_Name=="joura"
                    |df_MP$Constituency_Name=="morena"
                    |df_MP$Constituency_Name=="sabalgarh"
                    |df_MP$Constituency_Name=="sumawali" & df_MP$before_delim==1]<- "morena"

df_MP$District_Name[df_MP$Constituency_Name=="bohani"
                    |df_MP$Constituency_Name=="gadarwara"
                    |df_MP$Constituency_Name=="gotegaon"
                    |df_MP$Constituency_Name=="narsimhapur" & df_MP$before_delim==1]<- "narsimhapur"

df_MP$District_Name[df_MP$Constituency_Name=="jawad"
                    |df_MP$Constituency_Name=="manasa"
                    |df_MP$Constituency_Name=="neemuch" & df_MP$before_delim==1]<- "neemuch"

df_MP$District_Name[df_MP$Constituency_Name=="amanganj"
                    |df_MP$Constituency_Name=="panna"
                    |df_MP$Constituency_Name=="pawai" & df_MP$before_delim==1]<- "panna"

df_MP$District_Name[df_MP$Constituency_Name=="bareli"
                    |df_MP$Constituency_Name=="bhojpur"
                    |df_MP$Constituency_Name=="sanchi"
                    |df_MP$Constituency_Name=="udaipura" & df_MP$before_delim==1]<- "raisen"

df_MP$District_Name[df_MP$Constituency_Name=="biaora"
                    |df_MP$Constituency_Name=="khilchipur"
                    |df_MP$Constituency_Name=="narsingarh"
                    |df_MP$Constituency_Name=="rajgarh"
                    |df_MP$Constituency_Name=="sarangpur" & df_MP$before_delim==1]<- "rajgarh"

df_MP$District_Name[df_MP$Constituency_Name=="alot"
                    |df_MP$Constituency_Name=="jaora"
                    |df_MP$Constituency_Name=="ratlam rural"
                    |df_MP$Constituency_Name=="ratlam town"
                    |df_MP$Constituency_Name=="sailana" & df_MP$before_delim==1]<- "ratlam"

df_MP$District_Name[df_MP$Constituency_Name=="deotalab"
                    |df_MP$Constituency_Name=="gurh"
                    |df_MP$Constituency_Name=="mangawan"
                    |df_MP$Constituency_Name=="mauganj"
                    |df_MP$Constituency_Name=="rewa"
                    |df_MP$Constituency_Name=="sirmaur"
                    |df_MP$Constituency_Name=="teonthar"
                    |df_MP$Constituency_Name=="sirmour" & df_MP$before_delim==1]<- "rewa"

df_MP$District_Name[df_MP$Constituency_Name=="banda"
                    |df_MP$Constituency_Name=="bina"
                    |df_MP$Constituency_Name=="deori"
                    |df_MP$Constituency_Name=="khurai"
                    |df_MP$Constituency_Name=="naryaoli"
                    |df_MP$Constituency_Name=="rehli"
                    |df_MP$Constituency_Name=="sagar"
                    |df_MP$Constituency_Name=="surkhi" & df_MP$before_delim==1]<- "sagar"

df_MP$District_Name[df_MP$Constituency_Name=="amarpatan"
                    |df_MP$Constituency_Name=="chitrakoot"
                    |df_MP$Constituency_Name=="maihar"
                    |df_MP$Constituency_Name=="nagod"
                    |df_MP$Constituency_Name=="raigaon"
                    |df_MP$Constituency_Name=="satna"
                    |df_MP$Constituency_Name=="rampur baghelan"
                    |df_MP$Constituency_Name=="chitrakote" & df_MP$before_delim==1]<- "satna"

df_MP$District_Name[df_MP$Constituency_Name=="ashta"
                    |df_MP$Constituency_Name=="budhni"
                    |df_MP$Constituency_Name=="ichhawar"
                    |df_MP$Constituency_Name=="sehore" & df_MP$before_delim==1]<- "sehore"

df_MP$District_Name[df_MP$Constituency_Name=="barghat"
                    |df_MP$Constituency_Name=="ghansor"
                    |df_MP$Constituency_Name=="keolari"
                    |df_MP$Constituency_Name=="lakhanadon"
                    |df_MP$Constituency_Name=="seoni" & df_MP$before_delim==1]<- "seoni"

df_MP$Constituency_Name[df_MP$Constituency_Name=="pushprajgarh"]<- "pushparajgarh"

df_MP$District_Name[df_MP$Constituency_Name=="anuppur"
                    |df_MP$Constituency_Name=="beohari"
                    |df_MP$Constituency_Name=="jaisinghnagar"
                    |df_MP$Constituency_Name=="kotma"
                    |df_MP$Constituency_Name=="pushparajgarh"
                    |df_MP$Constituency_Name=="sohagpur" & df_MP$before_delim==1]<- "shahdol"

df_MP$District_Name[df_MP$Constituency_Name=="agar"
                    |df_MP$Constituency_Name=="gulana"
                    |df_MP$Constituency_Name=="shajapur"
                    |df_MP$Constituency_Name=="susner" & df_MP$before_delim==1]<- "shajapur"

df_MP$District_Name[df_MP$Constituency_Name=="bijeypur"
                    |df_MP$Constituency_Name=="sheopur" & df_MP$before_delim==1]<- "sheopur"

df_MP$District_Name[df_MP$Constituency_Name=="karera"
                    |df_MP$Constituency_Name=="kolaras"
                    |df_MP$Constituency_Name=="pichhore"
                    |df_MP$Constituency_Name=="pohri"
                    |df_MP$Constituency_Name=="shivpuri" & df_MP$before_delim==1]<- "shivpuri"

df_MP$Constituency_Name[df_MP$Constituency_Name=="gopad banas"]<- "gopadbanas"
df_MP$Constituency_Name[df_MP$Constituency_Name=="dhahani"]<- "dhauhani"

df_MP$District_Name[df_MP$Constituency_Name=="churahat"
                    |df_MP$Constituency_Name=="deosar"
                    |df_MP$Constituency_Name=="dhauhani"
                    |df_MP$Constituency_Name=="gopadbanas"
                    |df_MP$Constituency_Name=="sidhi"
                    |df_MP$Constituency_Name=="singrauli" & df_MP$before_delim==1]<- "sidhi"

df_MP$District_Name[df_MP$Constituency_Name=="jatara"
                    |df_MP$Constituency_Name=="khargapur"
                    |df_MP$Constituency_Name=="niwari"
                    |df_MP$Constituency_Name=="tikamgarh" & df_MP$before_delim==1]<- "tikamgarh"

df_MP$District_Name[df_MP$Constituency_Name=="badnagar"
                    |df_MP$Constituency_Name=="ghatiya"
                    |df_MP$Constituency_Name=="khachrod"
                    |df_MP$Constituency_Name=="mahidpur"
                    |df_MP$Constituency_Name=="tarana"
                    |df_MP$Constituency_Name=="ujjain north"
                    |df_MP$Constituency_Name=="ujjain south" & df_MP$before_delim==1]<- "ujjain"

df_MP$Constituency_Name[df_MP$Constituency_Name=="norozabad"]<- "nowrozabad"

df_MP$District_Name[df_MP$Constituency_Name=="jaisinghnagar"
                    |df_MP$Constituency_Name=="nowrozabad"
                    |df_MP$Constituency_Name=="sohagpur"
                    |df_MP$Constituency_Name=="umaria" & df_MP$before_delim==1]<- "umaria"

df_MP$District_Name[df_MP$Constituency_Name=="basoda"
                    |df_MP$Constituency_Name=="kurwai"
                    |df_MP$Constituency_Name=="sanchi"
                    |df_MP$Constituency_Name=="shamshabad"
                    |df_MP$Constituency_Name=="sironj"
                    |df_MP$Constituency_Name=="vidisha" & df_MP$before_delim==1]<- "vidisha"

df_MP$District_Name[df_MP$Constituency_Name=="barwaha"
                    |df_MP$Constituency_Name=="bhikangaon"
                    |df_MP$Constituency_Name=="dhulkot"
                    |df_MP$Constituency_Name=="kasrawad"
                    |df_MP$Constituency_Name=="khargone" & df_MP$before_delim==1]<- "west nimar"

df_MP<- df_MP%>%
  subset(District_Name!="")

######################################## Maharashtra ################################

df_MR<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Maharashtra_2021-11-23.csv")


df_MR<- df_MR%>%
  subset(Year==1990|
           Year==1995|
           Year==1999|
           Year==2004|
           Year==2009|
           Year==2014)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_MR$Constituency_Name[df_MR$Constituency_Name=="p.aithan"]<- "paithan"
df_MR$Constituency_Name[df_MR$Constituency_Name=="p.andharpur"]<- "pandharpur"
df_MR$Constituency_Name[df_MR$Constituency_Name=="p.aranda"]<- "paranda"
df_MR$Constituency_Name[df_MR$Constituency_Name=="p.arbhani"]<- "parbhani"
df_MR$Constituency_Name[df_MR$Constituency_Name=="p.arner"]<- "parner"
df_MR$Constituency_Name[df_MR$Constituency_Name=="p.artur"]<- "partur"
df_MR$Constituency_Name[df_MR$Constituency_Name=="p.athri"]<- "pathri"
df_MR$Constituency_Name[df_MR$Constituency_Name=="p.ulgaon"]<- "pulgaon"
df_MR$Constituency_Name[df_MR$Constituency_Name=="p.usad"]<- "pusad"

df_MR$District_Name[df_MR$Constituency_Name=="ahmednagar north"
                    |df_MR$Constituency_Name=="ahmednagar south"
                    |df_MR$Constituency_Name=="ashti"
                    |df_MR$Constituency_Name=="beed"
                    |df_MR$Constituency_Name=="georai"
                    |df_MR$Constituency_Name=="karjat"
                    |df_MR$Constituency_Name=="karmala"
                    |df_MR$Constituency_Name=="kopargaon"
                    |df_MR$Constituency_Name=="nagar-akola"
                    |df_MR$Constituency_Name=="parner"
                    |df_MR$Constituency_Name=="pathardi"
                    |df_MR$Constituency_Name=="rahuri"
                    |df_MR$Constituency_Name=="sangamner"
                    |df_MR$Constituency_Name=="sheogaon"
                    |df_MR$Constituency_Name=="shirdi"
                    |df_MR$Constituency_Name=="shrigonda"
                    |df_MR$Constituency_Name=="shrirampur"
                    |df_MR$Constituency_Name=="vaijapur" & df_MR$before_delim==1]<- "ahmadnagar"

df_MR$Constituency_Name[df_MR$Constituency_Name=="boroeon manju"]<- "borgaon manju"
df_MR$Constituency_Name[df_MR$Constituency_Name=="murtijapur"]<- "murtajapur"
df_MR$Constituency_Name[df_MR$Constituency_Name=="akoga"]<- "akola"

df_MR$District_Name[df_MR$Constituency_Name=="akola"
                    |df_MR$Constituency_Name=="akot"
                    |df_MR$Constituency_Name=="balapur"
                    |df_MR$Constituency_Name=="borgaon manju"
                    |df_MR$Constituency_Name=="karanja"
                    |df_MR$Constituency_Name=="murtajapur" & df_MR$before_delim==1]<- "akola"

df_MR$Constituency_Name[df_MR$Constituency_Name=="morhsi"]<- "morshi"

df_MR$District_Name[df_MR$Constituency_Name=="achalpur"
                    |df_MR$Constituency_Name=="amravati"
                    |df_MR$Constituency_Name=="badnera"
                    |df_MR$Constituency_Name=="chandur"
                    |df_MR$Constituency_Name=="daryapur"
                    |df_MR$Constituency_Name=="melghat"
                    |df_MR$Constituency_Name=="morshi"
                    |df_MR$Constituency_Name=="teosa"
                    |df_MR$Constituency_Name=="walgaon" & df_MR$before_delim==1]<- "amravati"

df_MR$District_Name[df_MR$Constituency_Name=="bhokardan"
                    |df_MR$Constituency_Name=="badnapur"
                    |df_MR$Constituency_Name=="gangapur"
                    |df_MR$Constituency_Name=="jamner"
                    |df_MR$Constituency_Name=="kannad"
                    |df_MR$Constituency_Name=="pachora"
                    |df_MR$Constituency_Name=="paithan"
                    |df_MR$Constituency_Name=="sillod"
                    |df_MR$Constituency_Name=="vaijapur"
                    |df_MR$Constituency_Name=="aurangabad east"
                    |df_MR$Constituency_Name=="aurangabad west" & df_MR$before_delim==1]<- "aurangabad"

df_MR$District_Name[df_MR$Constituency_Name=="adyar"
                    |df_MR$Constituency_Name=="bhandara"
                    |df_MR$Constituency_Name=="lakhandur"
                    |df_MR$Constituency_Name=="ramtek"
                    |df_MR$Constituency_Name=="sakoli"
                    |df_MR$Constituency_Name=="tumsar"
                    |df_MR$Constituency_Name=="umred" & df_MR$before_delim==1]<- "bhandara"

df_MR$District_Name[df_MR$Constituency_Name=="ambad"
                    |df_MR$Constituency_Name=="ashti"
                    |df_MR$Constituency_Name=="beed"
                    |df_MR$Constituency_Name=="chausala"
                    |df_MR$Constituency_Name=="georai"
                    |df_MR$Constituency_Name=="kaij"
                    |df_MR$Constituency_Name=="kalamb"
                    |df_MR$Constituency_Name=="manjlegaon"
                    |df_MR$Constituency_Name=="paranda"
                    |df_MR$Constituency_Name=="renapur" & df_MR$before_delim==1]<- "bid"

df_MR$Constituency_Name[df_MR$Constituency_Name=="buadhana"]<- "buldhana"
df_MR$Constituency_Name[df_MR$Constituency_Name=="sinndkhedraja"]<- "sindkhedraja"

df_MR$District_Name[df_MR$Constituency_Name=="buldhana"
                    |df_MR$Constituency_Name=="chikhli"
                    |df_MR$Constituency_Name=="jalamb"
                    |df_MR$Constituency_Name=="khamgaon"
                    |df_MR$Constituency_Name=="malkapur"
                    |df_MR$Constituency_Name=="mehkar"
                    |df_MR$Constituency_Name=="sindkhedraja" & df_MR$before_delim==1]<- "buldana"

df_MR$District_Name[df_MR$Constituency_Name=="bhadrawati"
                    |df_MR$Constituency_Name=="brahmapuri"
                    |df_MR$Constituency_Name=="chandrapur"
                    |df_MR$Constituency_Name=="chimur"
                    |df_MR$Constituency_Name=="rajura"
                    |df_MR$Constituency_Name=="saoli"
                    |df_MR$Constituency_Name=="wani" & df_MR$before_delim==1]<- "chandrapur"

df_MR$District_Name[df_MR$Constituency_Name=="kusumba"
                    |df_MR$Constituency_Name=="nawapur"
                    |df_MR$Constituency_Name=="sakri"
                    |df_MR$Constituency_Name=="shahade"
                    |df_MR$Constituency_Name=="shirpur"
                    |df_MR$Constituency_Name=="sindkheda"
                    |df_MR$Constituency_Name=="dhule" & df_MR$before_delim==1]<- "dhule"

df_MR$Constituency_Name[df_MR$Constituency_Name=="armori(st)"]<- "armori"

df_MR$District_Name[df_MR$Constituency_Name=="armori"
                    |df_MR$Constituency_Name=="gadchiroli"
                    |df_MR$Constituency_Name=="sironcha" & df_MR$before_delim==1]<- "gadchiroli"

df_MR$Constituency_Name[df_MR$Constituency_Name=="gondiya"]<- "gondia"

df_MR$District_Name[df_MR$Constituency_Name=="amgaon"
                    |df_MR$Constituency_Name=="gondia"
                    |df_MR$Constituency_Name=="goregaon"
                    |df_MR$Constituency_Name=="lakhandur"
                    |df_MR$Constituency_Name=="sakoli"
                    |df_MR$Constituency_Name=="tirora" & df_MR$before_delim==1]<- "gondiya"

df_MR$District_Name[df_MR$Constituency_Name=="basmath"
                    |df_MR$Constituency_Name=="hingoli"
                    |df_MR$Constituency_Name=="jintur"
                    |df_MR$Constituency_Name=="kalamnuri" & df_MR$before_delim==1]<- "hingoli"

df_MR$Constituency_Name[df_MR$Constituency_Name=="yawal"]<- "yaval"
df_MR$Constituency_Name[df_MR$Constituency_Name=="ealabad"]<- "edlabad"

df_MR$District_Name[df_MR$Constituency_Name=="amalner"
                    |df_MR$Constituency_Name=="bhusawal"
                    |df_MR$Constituency_Name=="chalisgaon"
                    |df_MR$Constituency_Name=="chopda"
                    |df_MR$Constituency_Name=="edlabad"
                    |df_MR$Constituency_Name=="erandol"
                    |df_MR$Constituency_Name=="jalgaon"
                    |df_MR$Constituency_Name=="jamner"
                    |df_MR$Constituency_Name=="pachora"
                    |df_MR$Constituency_Name=="parola"
                    |df_MR$Constituency_Name=="raver"
                    |df_MR$Constituency_Name=="yaval" & df_MR$before_delim==1]<- "jalgaon"

df_MR$District_Name[df_MR$Constituency_Name=="ambad"
                    |df_MR$Constituency_Name=="badnapur"
                    |df_MR$Constituency_Name=="bhokardan"
                    |df_MR$Constituency_Name=="jalna"
                    |df_MR$Constituency_Name=="partur" & df_MR$before_delim==1]<- "jalna"

df_MR$District_Name[df_MR$Constituency_Name=="chandgad"
                    |df_MR$Constituency_Name=="gadhinglaj"
                    |df_MR$Constituency_Name=="kagal"
                    |df_MR$Constituency_Name=="kolhapur"
                    |df_MR$Constituency_Name=="panhala"
                    |df_MR$Constituency_Name=="radhanagari"
                    |df_MR$Constituency_Name=="sangrul"
                    |df_MR$Constituency_Name=="shahuwadi"
                    |df_MR$Constituency_Name=="shirol"
                    |df_MR$Constituency_Name=="vadgaon"
                    |df_MR$Constituency_Name=="ichalkaranji"
                    |df_MR$Constituency_Name=="karvir" & df_MR$before_delim==1]<- "kolhapur"

df_MR$District_Name[df_MR$Constituency_Name=="ahmedpur"
                    |df_MR$Constituency_Name=="ausa"
                    |df_MR$Constituency_Name=="her"
                    |df_MR$Constituency_Name=="latur"
                    |df_MR$Constituency_Name=="nilanga"
                    |df_MR$Constituency_Name=="renapur"
                    |df_MR$Constituency_Name=="udgir" & df_MR$before_delim==1]<- "latur"

df_MR$Constituency_Name[df_MR$Constituency_Name=="kamthi"]<- "kamptee"

df_MR$District_Name[df_MR$Constituency_Name=="arvi"
                    |df_MR$Constituency_Name=="chimur"
                    |df_MR$Constituency_Name=="hinganghat"
                    |df_MR$Constituency_Name=="kalmeshwar"
                    |df_MR$Constituency_Name=="kamptee"
                    |df_MR$Constituency_Name=="katol"
                    |df_MR$Constituency_Name=="ramtek"
                    |df_MR$Constituency_Name=="savner"
                    |df_MR$Constituency_Name=="umred"
                    |df_MR$Constituency_Name=="nagpur central"
                    |df_MR$Constituency_Name=="nagpur east"
                    |df_MR$Constituency_Name=="nagpur west"
                    |df_MR$Constituency_Name=="nagpur north"
                    |df_MR$Constituency_Name=="nagpur south" & df_MR$before_delim==1]<- "nagpur"

df_MR$District_Name[df_MR$Constituency_Name=="bhokar"
                    |df_MR$Constituency_Name=="biloli"
                    |df_MR$Constituency_Name=="hadgaon"
                    |df_MR$Constituency_Name=="kandhar"
                    |df_MR$Constituency_Name=="kinwat"
                    |df_MR$Constituency_Name=="mudkhed"
                    |df_MR$Constituency_Name=="mukhed"
                    |df_MR$Constituency_Name=="nanded"
                    |df_MR$Constituency_Name=="umarkhed" & df_MR$before_delim==1]<- "nanded"

df_MR$Constituency_Name[df_MR$Constituency_Name=="taloda"]<- "talode"

df_MR$District_Name[df_MR$Constituency_Name=="akrani"
                    |df_MR$Constituency_Name=="nandurbar"
                    |df_MR$Constituency_Name=="nawapur"
                    |df_MR$Constituency_Name=="shahade"
                    |df_MR$Constituency_Name=="talode" & df_MR$before_delim==1]<- "nandurbar"

df_MR$Constituency_Name[df_MR$Constituency_Name=="kalwan"]<- "kalvan"
df_MR$Constituency_Name[df_MR$Constituency_Name=="yeola"]<- "yevla"
df_MR$Constituency_Name[df_MR$Constituency_Name=="nasik"]<- "nashik"

df_MR$District_Name[df_MR$Constituency_Name=="baglan"
                    |df_MR$Constituency_Name=="chandwad"
                    |df_MR$Constituency_Name=="dabhadi"
                    |df_MR$Constituency_Name=="deolali"
                    |df_MR$Constituency_Name=="dindori"
                    |df_MR$Constituency_Name=="igatpuri"
                    |df_MR$Constituency_Name=="kalvan"
                    |df_MR$Constituency_Name=="nandgaon"
                    |df_MR$Constituency_Name=="niphad"
                    |df_MR$Constituency_Name=="sinnar"
                    |df_MR$Constituency_Name=="surgana"
                    |df_MR$Constituency_Name=="yevla"
                    |df_MR$Constituency_Name=="malegaon"
                    |df_MR$Constituency_Name=="nashik" & df_MR$before_delim==1]<- "nashik"

df_MR$District_Name[df_MR$Constituency_Name=="barshi"
                    |df_MR$Constituency_Name=="kalamb"
                    |df_MR$Constituency_Name=="omerga"
                    |df_MR$Constituency_Name=="osmanabad"
                    |df_MR$Constituency_Name=="paranda"
                    |df_MR$Constituency_Name=="tuljapur" & df_MR$before_delim==1]<- "osmanabad"

df_MR$District_Name[df_MR$Constituency_Name=="basmath"
                    |df_MR$Constituency_Name=="gangakhed"
                    |df_MR$Constituency_Name=="jintur"
                    |df_MR$Constituency_Name=="parbhani"
                    |df_MR$Constituency_Name=="pathri"
                    |df_MR$Constituency_Name=="singnapur" & df_MR$before_delim==1]<- "parbhani"

df_MR$District_Name[df_MR$Constituency_Name=="ambegaon"
                    |df_MR$Constituency_Name=="baramati"
                    |df_MR$Constituency_Name=="bhor"
                    |df_MR$Constituency_Name=="daund"
                    |df_MR$Constituency_Name=="haveli"
                    |df_MR$Constituency_Name=="indapur"
                    |df_MR$Constituency_Name=="junnar"
                    |df_MR$Constituency_Name=="khed-alandi"
                    |df_MR$Constituency_Name=="maval"
                    |df_MR$Constituency_Name=="mulshi"
                    |df_MR$Constituency_Name=="purandhar"
                    |df_MR$Constituency_Name=="shirur"
                    |df_MR$Constituency_Name=="bhavani peth"
                    |df_MR$Constituency_Name=="kasba peth"
                    |df_MR$Constituency_Name=="parvati"
                    |df_MR$Constituency_Name=="pune cantonment" & df_MR$before_delim==1]<- "pune"

df_MR$Constituency_Name[df_MR$Constituency_Name=="shrivardhanr"]<- "shriwardhan"

df_MR$District_Name[df_MR$Constituency_Name=="alibag"
                    |df_MR$Constituency_Name=="khalapur"
                    |df_MR$Constituency_Name=="mahad"
                    |df_MR$Constituency_Name=="mangaon"
                    |df_MR$Constituency_Name=="panvel"
                    |df_MR$Constituency_Name=="pen"
                    |df_MR$Constituency_Name=="shriwardhan" & df_MR$before_delim==1]<- "raigarh"

df_MR$Constituency_Name[df_MR$Constituency_Name=="sangaumeshwar"]<- "sangameshwar"

df_MR$District_Name[df_MR$Constituency_Name=="chiplun"
                    |df_MR$Constituency_Name=="dapoli"
                    |df_MR$Constituency_Name=="guhagar"
                    |df_MR$Constituency_Name=="khed"
                    |df_MR$Constituency_Name=="rajapur"
                    |df_MR$Constituency_Name=="ratnagiri"
                    |df_MR$Constituency_Name=="sangameshwar" & df_MR$before_delim==1]<- "ratnagiri"

df_MR$District_Name[df_MR$Constituency_Name=="bhilwadi wangi"
                    |df_MR$Constituency_Name=="jat"
                    |df_MR$Constituency_Name=="kavathe mahankal"
                    |df_MR$Constituency_Name=="khanapur atpadi"
                    |df_MR$Constituency_Name=="sangli"
                    |df_MR$Constituency_Name=="shirala"
                    |df_MR$Constituency_Name=="tasgaon"
                    |df_MR$Constituency_Name=="vadgaon"
                    |df_MR$Constituency_Name=="walva"
                    |df_MR$Constituency_Name=="miraj" & df_MR$before_delim==1]<- "sangli"

df_MR$District_Name[df_MR$Constituency_Name=="jaoli"
                    |df_MR$Constituency_Name=="karad north"
                    |df_MR$Constituency_Name=="karad south"
                    |df_MR$Constituency_Name=="khatav"
                    |df_MR$Constituency_Name=="koregaon"
                    |df_MR$Constituency_Name=="man"
                    |df_MR$Constituency_Name=="patan"
                    |df_MR$Constituency_Name=="phaltan"
                    |df_MR$Constituency_Name=="satara"
                    |df_MR$Constituency_Name=="wai" & df_MR$before_delim==1]<- "satara"

df_MR$Constituency_Name[df_MR$Constituency_Name=="malwan"]<- "malvan"

df_MR$District_Name[df_MR$Constituency_Name=="devgad"
                    |df_MR$Constituency_Name=="malvan"
                    |df_MR$Constituency_Name=="panhala"
                    |df_MR$Constituency_Name=="sawantwadi"
                    |df_MR$Constituency_Name=="vengurla" & df_MR$before_delim==1]<- "sindhudurg"

df_MR$District_Name[df_MR$Constituency_Name=="akkalkot"
                    |df_MR$Constituency_Name=="barshi"
                    |df_MR$Constituency_Name=="karmala"
                    |df_MR$Constituency_Name=="madha"
                    |df_MR$Constituency_Name=="malshiras"
                    |df_MR$Constituency_Name=="mangalvedhe"
                    |df_MR$Constituency_Name=="mohol"
                    |df_MR$Constituency_Name=="pandharpur"
                    |df_MR$Constituency_Name=="paranda"
                    |df_MR$Constituency_Name=="sangole"
                    |df_MR$Constituency_Name=="solapur city north"
                    |df_MR$Constituency_Name=="solapur city south"
                    |df_MR$Constituency_Name=="south solapur"
                    |df_MR$Constituency_Name=="north solapur" & df_MR$before_delim==1]<- "solapur"


df_MR$Constituency_Name[df_MR$Constituency_Name=="kalwan"]<- "kalyan"

df_MR$District_Name[df_MR$Constituency_Name=="ambernath"
                    |df_MR$Constituency_Name=="bhiwandi"
                    |df_MR$Constituency_Name=="dahanu"
                    |df_MR$Constituency_Name=="igatpuri"
                    |df_MR$Constituency_Name=="jawhar"
                    |df_MR$Constituency_Name=="kalyan"
                    |df_MR$Constituency_Name=="murbad"
                    |df_MR$Constituency_Name=="palghar"
                    |df_MR$Constituency_Name=="shahapur"
                    |df_MR$Constituency_Name=="vasai"
                    |df_MR$Constituency_Name=="wada"
                    |df_MR$Constituency_Name=="thane"
                    |df_MR$Constituency_Name=="ulhasnagar" & df_MR$before_delim==1]<- "thane"

df_MR$District_Name[df_MR$Constituency_Name=="arvi"
                    |df_MR$Constituency_Name=="bhadrawati"
                    |df_MR$Constituency_Name=="hinganghat"
                    |df_MR$Constituency_Name=="pulgaon"
                    |df_MR$Constituency_Name=="ralegaon"
                    |df_MR$Constituency_Name=="wardha"
                    |df_MR$Constituency_Name=="teosa" & df_MR$before_delim==1]<- "wardha"

df_MR$District_Name[df_MR$Constituency_Name=="karanja"
                    |df_MR$Constituency_Name=="mangrulpir"
                    |df_MR$Constituency_Name=="medshi"
                    |df_MR$Constituency_Name=="washim" & df_MR$before_delim==1]<- "washim"

df_MR$District_Name[df_MR$Constituency_Name=="darwha"
                    |df_MR$Constituency_Name=="digras"
                    |df_MR$Constituency_Name=="kelapur"
                    |df_MR$Constituency_Name=="kinwat"
                    |df_MR$Constituency_Name=="pusad"
                    |df_MR$Constituency_Name=="rajura"
                    |df_MR$Constituency_Name=="ralegaon"
                    |df_MR$Constituency_Name=="umarkhed"
                    |df_MR$Constituency_Name=="wani"
                    |df_MR$Constituency_Name=="yavatmal" & df_MR$before_delim==1]<- "yavatmal"

df_MR<- df_MR%>%
  subset(District_Name!="")


################################ Manipur ########################################################

df_MN<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Manipur_2021-11-23.csv")


df_MN<- df_MN%>%
  subset(Year==1990|
           Year==1995|
           Year==2000|
           Year==2002|
           Year==2007|
           Year==2012)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_MN$District_Name[df_MN$Constituency_Name=="bishenpur"
                    |df_MN$Constituency_Name=="kumbi"
                    |df_MN$Constituency_Name=="moirang"
                    |df_MN$Constituency_Name=="oinam" & df_MN$before_delim==1]<- "bishnupur"

df_MN$District_Name[df_MN$Constituency_Name=="chandel"
                    |df_MN$Constituency_Name=="tengnoupal"]<- "chandel"

df_MN$District_Name[df_MN$Constituency_Name=="churachandpur"
                    |df_MN$Constituency_Name=="henglep"
                    |df_MN$Constituency_Name=="saikot"
                    |df_MN$Constituency_Name=="singhat"
                    |df_MN$Constituency_Name=="thanlon"
                    |df_MN$Constituency_Name=="tipaimukh" & df_MN$before_delim==1]<- "churachandpur"

df_MN$District_Name[df_MN$Constituency_Name=="andro"
                    |df_MN$Constituency_Name=="heingang"
                    |df_MN$Constituency_Name=="jiribam"
                    |df_MN$Constituency_Name=="keirao"
                    |df_MN$Constituency_Name=="keisamthong"
                    |df_MN$Constituency_Name=="khundrakpam"
                    |df_MN$Constituency_Name=="khurai"
                    |df_MN$Constituency_Name=="khetrigao"
                    |df_MN$Constituency_Name=="lamlai"
                    |df_MN$Constituency_Name=="langthabal"
                    |df_MN$Constituency_Name=="lilong"
                    |df_MN$Constituency_Name=="patsoi"
                    |df_MN$Constituency_Name=="sagolband"
                    |df_MN$Constituency_Name=="saikul"
                    |df_MN$Constituency_Name=="singjamei"
                    |df_MN$Constituency_Name=="thangmeiband"
                    |df_MN$Constituency_Name=="thongju"
                    |df_MN$Constituency_Name=="uripok"
                    |df_MN$Constituency_Name=="wangkhei"
                    |df_MN$Constituency_Name=="yaiskul" & df_MN$before_delim==1]<- "imphal east"

df_MN$District_Name[df_MN$Constituency_Name=="konthoujam"
                    |df_MN$Constituency_Name=="lamsang"
                    |df_MN$Constituency_Name=="mayang imphal"
                    |df_MN$Constituency_Name=="nambol"
                    |df_MN$Constituency_Name=="naoriya pakhanglakpa"
                    |df_MN$Constituency_Name=="saitu"
                    |df_MN$Constituency_Name=="sekmai"
                    |df_MN$Constituency_Name=="wangoi" & df_MN$before_delim==1]<- "imphal west"

df_MN$District_Name[df_MN$Constituency_Name=="kangpokpi"
                    |df_MN$Constituency_Name=="karong"
                    |df_MN$Constituency_Name=="keirao"
                    |df_MN$Constituency_Name=="mao"
                    |df_MN$Constituency_Name=="saikul"
                    |df_MN$Constituency_Name=="saitu"
                    |df_MN$Constituency_Name=="tadubi" & df_MN$before_delim==1]<- "senapati"

df_MN$District_Name[df_MN$Constituency_Name=="tamei"
                    |df_MN$Constituency_Name=="tamenglong"
                    |df_MN$Constituency_Name=="nungba" & df_MN$before_delim==1]<- "tamenglong"

df_MN$District_Name[df_MN$Constituency_Name=="heirok"
                    |df_MN$Constituency_Name=="hiyanglam"
                    |df_MN$Constituency_Name=="kakching"
                    |df_MN$Constituency_Name=="kangpokpi"
                    |df_MN$Constituency_Name=="sugnoo"
                    |df_MN$Constituency_Name=="thoubal"
                    |df_MN$Constituency_Name=="wabgai"
                    |df_MN$Constituency_Name=="wangjing tentha"
                    |df_MN$Constituency_Name=="wangkhem" & df_MN$before_delim==1]<- "thoubal"

df_MN$District_Name[df_MN$Constituency_Name=="chingai"
                    |df_MN$Constituency_Name=="phungyar"
                    |df_MN$Constituency_Name=="ukhrul" & df_MN$before_delim==1]<- "ukhrul"


df_MN<- df_MN%>%
  subset(District_Name!="")

################################ Meghalaya ########################################

df_MG<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Meghalaya_2021-11-23.csv")

df_MG<- df_MG%>%
  subset(Year==1993|
           Year==1998|
           Year==2003|
           Year==2008|
           Year==2013)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_MG$District_Name[df_MG$Constituency_Name=="bajengdoba"
                    |df_MG$Constituency_Name=="kharkutta"
                    |df_MG$Constituency_Name=="mendipathar"
                    |df_MG$Constituency_Name=="resubelpara"
                    |df_MG$Constituency_Name=="rongjeng"
                    |df_MG$Constituency_Name=="rongrenggiri"
                    |df_MG$Constituency_Name=="songsak" & df_MG$before_delim==1]<- "east garo hills"

df_MG$District_Name[df_MG$Constituency_Name=="dienglieng"
                    |df_MG$Constituency_Name=="lyngkyrdem"
                    |df_MG$Constituency_Name=="mawkhar"
                    |df_MG$Constituency_Name=="mawlai"
                    |df_MG$Constituency_Name=="mawprem"
                    |df_MG$Constituency_Name=="mawsynram"
                    |df_MG$Constituency_Name=="mylliem"
                    |df_MG$Constituency_Name=="nongkrem"
                    |df_MG$Constituency_Name=="nongshken"
                    |df_MG$Constituency_Name=="nongspung"
                    |df_MG$Constituency_Name=="pynthorumkhrah"
                    |df_MG$Constituency_Name=="shella"
                    |df_MG$Constituency_Name=="sohiong"
                    |df_MG$Constituency_Name=="sohryngkham"
                    |df_MG$Constituency_Name=="umroi"
                    |df_MG$Constituency_Name=="sohra" & df_MG$before_delim==1]<- "east khasi hills"

df_MG$District_Name[df_MG$Constituency_Name=="jowai"
                    |df_MG$Constituency_Name=="nartiang"
                    |df_MG$Constituency_Name=="nongbah-wahiajer"
                    |df_MG$Constituency_Name=="raliang"
                    |df_MG$Constituency_Name=="rymbai"
                    |df_MG$Constituency_Name=="sutnga-shangpung"
                    |df_MG$Constituency_Name=="war-jaintia" & df_MG$before_delim==1]<- "jaintia hills"

df_MG$District_Name[df_MG$Constituency_Name=="jirang"
                    |df_MG$Constituency_Name=="mawhati"
                    |df_MG$Constituency_Name=="nongpoh"
                    |df_MG$Constituency_Name=="umroi" & df_MG$before_delim==1]<- "ri bhoi"

df_MG$District_Name[df_MG$Constituency_Name=="baghmara"
                    |df_MG$Constituency_Name=="chokpot"
                    |df_MG$Constituency_Name=="rongrenggiri" & df_MG$before_delim==1]<- "south garo hills"

df_MG$District_Name[df_MG$Constituency_Name=="ampatigiri"
                    |df_MG$Constituency_Name=="dadenggiri"
                    |df_MG$Constituency_Name=="dalamgiri"
                    |df_MG$Constituency_Name=="dalu"
                    |df_MG$Constituency_Name=="kherapara"
                    |df_MG$Constituency_Name=="mahendraganj"
                    |df_MG$Constituency_Name=="phulbari"
                    |df_MG$Constituency_Name=="rajabala"
                    |df_MG$Constituency_Name=="rongchugiri"
                    |df_MG$Constituency_Name=="rongram"
                    |df_MG$Constituency_Name=="salmanpura"
                    |df_MG$Constituency_Name=="selsella"
                    |df_MG$Constituency_Name=="tikrikilla"
                    |df_MG$Constituency_Name=="tura"
                    |df_MG$Constituency_Name=="rangsakona" & df_MG$before_delim==1]<- "west garo hills"

df_MG$District_Name[df_MG$Constituency_Name=="langrin"
                    |df_MG$Constituency_Name=="mairang"
                    |df_MG$Constituency_Name=="mawkyrwat"
                    |df_MG$Constituency_Name=="mawthengkut"
                    |df_MG$Constituency_Name=="nongstoin"
                    |df_MG$Constituency_Name=="pariong" & df_MG$before_delim==1]<- "west khasi hills"

df_MG<- df_MG%>%
  subset(District_Name!="")

############################### Mizoram ####################################################

df_MZ<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Mizoram_2021-11-24.csv")


df_MZ<- df_MZ%>%
  subset(Year==1989|
           Year==1993|
           Year==1998|
           Year==2003|
           Year==2008|
           Year==2013)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_MZ$Constituency_Name[df_MZ$Constituency_Name=="aizawal east"]<- "aizawl east"
df_MZ$Constituency_Name[df_MZ$Constituency_Name=="aizawal north"]<- "aizawl north"
df_MZ$Constituency_Name[df_MZ$Constituency_Name=="aizawal south"]<- "aizawl south"
df_MZ$Constituency_Name[df_MZ$Constituency_Name=="aizawal west"]<- "aizawl west"
df_MZ$Constituency_Name[df_MZ$Constituency_Name=="sateek(st)"]<- "sateek"

df_MZ$District_Name[df_MZ$Constituency_Name=="aizawl east"
                    |df_MZ$Constituency_Name=="aizawl east-i"
                    |df_MZ$Constituency_Name=="aizawl east-ii"
                    |df_MZ$Constituency_Name=="aizawl north"
                    |df_MZ$Constituency_Name=="aizawl north-i"
                    |df_MZ$Constituency_Name=="aizawl north ii"
                    |df_MZ$Constituency_Name=="aizawl south"
                    |df_MZ$Constituency_Name=="aizawl south - i"
                    |df_MZ$Constituency_Name=="aizawl south - ii"
                    |df_MZ$Constituency_Name=="aizawl west"
                    |df_MZ$Constituency_Name=="aizawl west-ii"
                    |df_MZ$Constituency_Name=="aizawl west i"
                    |df_MZ$Constituency_Name=="ratu"
                    |df_MZ$Constituency_Name=="saitual"
                    |df_MZ$Constituency_Name=="sateek"
                    |df_MZ$Constituency_Name=="suangpuilawn"
                    |df_MZ$Constituency_Name=="tlungvel" & df_MZ$before_delim==1]<- "aizawl"

df_MZ$District_Name[df_MZ$Constituency_Name=="champhai"
                    |df_MZ$Constituency_Name=="khawbung"
                    |df_MZ$Constituency_Name=="khawhai"
                    |df_MZ$Constituency_Name=="khawzawl"
                    |df_MZ$Constituency_Name=="ngopa" & df_MZ$before_delim==1]<- "champhai"

df_MZ$District_Name[df_MZ$Constituency_Name=="bilkhawthlir"
                    |df_MZ$Constituency_Name=="kawnpui"
                    |df_MZ$Constituency_Name=="kolasib" & df_MZ$before_delim==1]<- "kolasib"

df_MZ$Constituency_Name[df_MZ$Constituency_Name=="chawngtea"]<- "chawngte"

df_MZ$District_Name[df_MZ$Constituency_Name=="chawngte"
                    |df_MZ$Constituency_Name=="lawngtlai" & df_MZ$before_delim==1]<- "lawangtlai"

df_MZ$Constituency_Name[df_MZ$Constituency_Name=="hnahthiaal"]<- "hnahthial"

df_MZ$District_Name[df_MZ$Constituency_Name=="buarpui"
                    |df_MZ$Constituency_Name=="hnahthial"
                    |df_MZ$Constituency_Name=="lunglei"
                    |df_MZ$Constituency_Name=="lunglei north"
                    |df_MZ$Constituency_Name=="lunglei south"
                    |df_MZ$Constituency_Name=="tawipui"
                    |df_MZ$Constituency_Name=="tlabung"
                    |df_MZ$Constituency_Name=="vanva" & df_MZ$before_delim==1]<- "lunglei"

df_MZ$District_Name[df_MZ$Constituency_Name=="kawrthah"
                    |df_MZ$Constituency_Name=="lokicherra"
                    |df_MZ$Constituency_Name=="mamit"
                    |df_MZ$Constituency_Name=="phuldungsei" & df_MZ$before_delim==1]<- "mamit"

df_MZ$District_Name[df_MZ$Constituency_Name=="saiha"
                    |df_MZ$Constituency_Name=="sangau"
                    |df_MZ$Constituency_Name=="tuipang" & df_MZ$before_delim==1]<- "saiha"

df_MZ$District_Name[df_MZ$Constituency_Name=="lungpho"
                    |df_MZ$Constituency_Name=="north vanlaiphai"
                    |df_MZ$Constituency_Name=="serchhip" & df_MZ$before_delim==1]<- "serchhip"

df_MZ<- df_MZ%>%
  subset(District_Name!="")

############################ Nagaland ########################################

df_NG<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Nagaland_2021-11-23.csv")

df_NG<- df_NG%>%
  subset(Year==1989|
           Year==1993|
           Year==1998|
           Year==2003|
           Year==2008|
           Year==2013)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_NG$District_Name[df_NG$Constituency_Name=="dimapur-i"
                    |df_NG$Constituency_Name=="dimapur-ii"
                    |df_NG$Constituency_Name=="dimapur-iii"
                    |df_NG$Constituency_Name=="ghaspani-i"
                    |df_NG$Constituency_Name=="ghaspani-ii" & df_NG$before_delim==1]<- "dimapur"

df_NG$District_Name[df_NG$Constituency_Name=="kohima town"
                    |df_NG$Constituency_Name=="northern angami-i"
                    |df_NG$Constituency_Name=="northern angami-ii"
                    |df_NG$Constituency_Name=="peren"
                    |df_NG$Constituency_Name=="southern angami-i"
                    |df_NG$Constituency_Name=="southern angami-ii"
                    |df_NG$Constituency_Name=="tenning"
                    |df_NG$Constituency_Name=="tseminyu"
                    |df_NG$Constituency_Name=="western angami" & df_NG$before_delim==1]<- "kohima"

df_NG$District_Name[df_NG$Constituency_Name=="alongtaki"
                    |df_NG$Constituency_Name=="angetyongpang"
                    |df_NG$Constituency_Name=="aonglenden"
                    |df_NG$Constituency_Name=="arkakong"
                    |df_NG$Constituency_Name=="impur"
                    |df_NG$Constituency_Name=="jangpetkong"
                    |df_NG$Constituency_Name=="koridang"
                    |df_NG$Constituency_Name=="mokokchung town"
                    |df_NG$Constituency_Name=="mongoya"
                    |df_NG$Constituency_Name=="tuli" & df_NG$before_delim==1]<- "mokokchung"

df_NG$District_Name[df_NG$Constituency_Name=="aboi"
                    |df_NG$Constituency_Name=="moka"
                    |df_NG$Constituency_Name=="mon town"
                    |df_NG$Constituency_Name=="phomching"
                    |df_NG$Constituency_Name=="tapi"
                    |df_NG$Constituency_Name=="tehok"
                    |df_NG$Constituency_Name=="tizit"
                    |df_NG$Constituency_Name=="tobu"
                    |df_NG$Constituency_Name=="wakching" & df_NG$before_delim==1]<- "mon"

df_NG$District_Name[df_NG$Constituency_Name=="chazouba"
                    |df_NG$Constituency_Name=="chizami"
                    |df_NG$Constituency_Name=="meluri"
                    |df_NG$Constituency_Name=="pfutsero"
                    |df_NG$Constituency_Name=="phek" & df_NG$before_delim==1]<- "phek"

df_NG$District_Name[df_NG$Constituency_Name=="longkhim chare"
                    |df_NG$Constituency_Name=="longleng"
                    |df_NG$Constituency_Name=="noklak"
                    |df_NG$Constituency_Name=="noksen"
                    |df_NG$Constituency_Name=="pungro kiphire"
                    |df_NG$Constituency_Name=="shamator chessore"
                    |df_NG$Constituency_Name=="seyochung sitimi"
                    |df_NG$Constituency_Name=="tamlu"
                    |df_NG$Constituency_Name=="thonoknyu"
                    |df_NG$Constituency_Name=="tuensang sadar-i"
                    |df_NG$Constituency_Name=="tuensang sadar-ii" & df_NG$before_delim==1]<- "tuensang"

df_NG$District_Name[df_NG$Constituency_Name=="bhandari"
                    |df_NG$Constituency_Name=="sanis"
                    |df_NG$Constituency_Name=="tyui"
                    |df_NG$Constituency_Name=="wokha" & df_NG$before_delim==1]<- "wokha"

df_NG$District_Name[df_NG$Constituency_Name=="aghunato"
                    |df_NG$Constituency_Name=="akuluto"
                    |df_NG$Constituency_Name=="atoizu"
                    |df_NG$Constituency_Name=="pughoboto"
                    |df_NG$Constituency_Name=="satakha"
                    |df_NG$Constituency_Name=="zunheboto" & df_NG$before_delim==1]<- "zunheboto"

df_NG<- df_NG%>%
  subset(District_Name!="")

################################ Orissa ########################################################

df_OR<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Odisha_2021-11-23.csv")


df_OR<- df_OR%>%
  subset(Year==1990|
           Year==1995|
           Year==2000|
           Year==2004|
           Year==2009|
           Year==2014)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_OR$District_Name[df_OR$Constituency_Name=="angul"
                    |df_OR$Constituency_Name=="athmallik"
                    |df_OR$Constituency_Name=="birmaharajpur"
                    |df_OR$Constituency_Name=="hindol"
                    |df_OR$Constituency_Name=="pallahara"
                    |df_OR$Constituency_Name=="talcher" & df_OR$before_delim==1]<- "anugul"

df_OR$District_Name[df_OR$Constituency_Name=="bolangir"
                    |df_OR$Constituency_Name=="kantabanji"
                    |df_OR$Constituency_Name=="kesinga"
                    |df_OR$Constituency_Name=="loisingha"
                    |df_OR$Constituency_Name=="melchhamunda"
                    |df_OR$Constituency_Name=="patnagarh"
                    |df_OR$Constituency_Name=="saintala"
                    |df_OR$Constituency_Name=="sonepur"
                    |df_OR$Constituency_Name=="titilagarh" & df_OR$before_delim==1]<- "balangir"

df_OR$District_Name[df_OR$Constituency_Name=="anandapur"
                    |df_OR$Constituency_Name=="baisinga"
                    |df_OR$Constituency_Name=="balasore"
                    |df_OR$Constituency_Name=="basta"
                    |df_OR$Constituency_Name=="bhadrak"
                    |df_OR$Constituency_Name=="bhandaripokhari"
                    |df_OR$Constituency_Name=="bhograi"
                    |df_OR$Constituency_Name=="jaleswar"
                    |df_OR$Constituency_Name=="nilgiri"
                    |df_OR$Constituency_Name=="simulia"
                    |df_OR$Constituency_Name=="soro" & df_OR$before_delim==1]<- "baleshwar"

df_OR$District_Name[df_OR$Constituency_Name=="bargarh"
                    |df_OR$Constituency_Name=="bhatli"
                    |df_OR$Constituency_Name=="bijepur"
                    |df_OR$Constituency_Name=="binka"
                    |df_OR$Constituency_Name=="loisingha"
                    |df_OR$Constituency_Name=="melchhamunda"
                    |df_OR$Constituency_Name=="padampur"
                    |df_OR$Constituency_Name=="patnagarh"
                    |df_OR$Constituency_Name=="sambalpur" & df_OR$before_delim==1]<- "bargarh"

df_OR$District_Name[df_OR$Constituency_Name=="balikuda"
                    |df_OR$Constituency_Name=="boudh"
                    |df_OR$Constituency_Name=="phulbani" & df_OR$before_delim==1]<- "baudh"

df_OR$District_Name[df_OR$Constituency_Name=="anandapur"
                    |df_OR$Constituency_Name=="basudevpur"
                    |df_OR$Constituency_Name=="bhadrak"
                    |df_OR$Constituency_Name=="bhandaripokhari"
                    |df_OR$Constituency_Name=="chandbali"
                    |df_OR$Constituency_Name=="dhamnagar"
                    |df_OR$Constituency_Name=="jajpur"
                    |df_OR$Constituency_Name=="korai" & df_OR$before_delim==1]<- "bhadrak"

df_OR$District_Name[df_OR$Constituency_Name=="athgarh"
                    |df_OR$Constituency_Name=="balikuda"
                    |df_OR$Constituency_Name=="banki"
                    |df_OR$Constituency_Name=="baramba"
                    |df_OR$Constituency_Name=="barchana"
                    |df_OR$Constituency_Name=="bari-derabisi"
                    |df_OR$Constituency_Name=="choudwar"
                    |df_OR$Constituency_Name=="cuttack city"
                    |df_OR$Constituency_Name=="cuttack sadar"
                    |df_OR$Constituency_Name=="daspalla"
                    |df_OR$Constituency_Name=="gobindpur"
                    |df_OR$Constituency_Name=="kissannagar"
                    |df_OR$Constituency_Name=="mahanga"
                    |df_OR$Constituency_Name=="salepur" & df_OR$before_delim==1]<- "cuttack"

df_OR$District_Name[df_OR$Constituency_Name=="deogarh"
                    |df_OR$Constituency_Name=="rairakhol" & df_OR$before_delim==1]<- "debagarh"

df_OR$District_Name[df_OR$Constituency_Name=="dhenkanal"
                    |df_OR$Constituency_Name=="gondia"
                    |df_OR$Constituency_Name=="hindol"
                    |df_OR$Constituency_Name=="kamakhyanagar"
                    |df_OR$Constituency_Name=="pallahara" & df_OR$before_delim==1]<- "dhenkanal"

df_OR$District_Name[df_OR$Constituency_Name=="mohana"
                    |df_OR$Constituency_Name=="gunupur"
                    |df_OR$Constituency_Name=="parlakhemundi"
                    |df_OR$Constituency_Name=="ramagiri" & df_OR$before_delim==1]<- "gajapati"

df_OR$District_Name[df_OR$Constituency_Name=="aska"
                    |df_OR$Constituency_Name=="bhanjanagar"
                    |df_OR$Constituency_Name=="chatrapur"
                    |df_OR$Constituency_Name=="chikati"
                    |df_OR$Constituency_Name=="gopalpur"
                    |df_OR$Constituency_Name=="hinjili"
                    |df_OR$Constituency_Name=="jaganathprasad"
                    |df_OR$Constituency_Name=="kavisuryanagar"
                    |df_OR$Constituency_Name=="khallikote"
                    |df_OR$Constituency_Name=="kodala"
                    |df_OR$Constituency_Name=="ramagiri"
                    |df_OR$Constituency_Name=="suruda"
                    |df_OR$Constituency_Name=="berhampur" & df_OR$before_delim==1]<- "ganjam"

df_OR$District_Name[df_OR$Constituency_Name=="balikuda"
                    |df_OR$Constituency_Name=="ersama"
                    |df_OR$Constituency_Name=="jagatsinghpur"
                    |df_OR$Constituency_Name=="kissannagar"
                    |df_OR$Constituency_Name=="tirtol" & df_OR$before_delim==1]<- "jagatsinghapur"

df_OR$Constituency_Name[df_OR$Constituency_Name=="dharmasala"]<- "dharamsala"

df_OR$District_Name[df_OR$Constituency_Name=="barchana"
                    |df_OR$Constituency_Name=="bari-derabisi"
                    |df_OR$Constituency_Name=="binjharpur"
                    |df_OR$Constituency_Name=="dharamsala"
                    |df_OR$Constituency_Name=="jajpur"
                    |df_OR$Constituency_Name=="korai"
                    |df_OR$Constituency_Name=="sukinda" & df_OR$before_delim==1]<- "jajapur"

df_OR$District_Name[df_OR$Constituency_Name=="brajarajnagar"
                    |df_OR$Constituency_Name=="jharsuguda"
                    |df_OR$Constituency_Name=="laikera" & df_OR$before_delim==1]<- "jharsuguda"

df_OR$District_Name[df_OR$Constituency_Name=="bhawanipatna"
                    |df_OR$Constituency_Name=="dharamgarh"
                    |df_OR$Constituency_Name=="junagarh"
                    |df_OR$Constituency_Name=="kesinga"
                    |df_OR$Constituency_Name=="koksara"
                    |df_OR$Constituency_Name=="narla" & df_OR$before_delim==1]<- "kalahandi"

df_OR$District_Name[df_OR$Constituency_Name=="balliguda"
                    |df_OR$Constituency_Name=="phulbani"
                    |df_OR$Constituency_Name=="udayagiri" & df_OR$before_delim==1]<- "kandhmal"

df_OR$District_Name[df_OR$Constituency_Name=="aul"
                    |df_OR$Constituency_Name=="bari-derabisi"
                    |df_OR$Constituency_Name=="kendrapara"
                    |df_OR$Constituency_Name=="patamundai"
                    |df_OR$Constituency_Name=="patkura"
                    |df_OR$Constituency_Name=="rajnagar" & df_OR$before_delim==1]<- "kendrapara"

df_OR$District_Name[df_OR$Constituency_Name=="anandapur"
                    |df_OR$Constituency_Name=="champua"
                    |df_OR$Constituency_Name=="keonjhar"
                    |df_OR$Constituency_Name=="patna"
                    |df_OR$Constituency_Name=="ramchandrapur"
                    |df_OR$Constituency_Name=="telkoi" & df_OR$before_delim==1]<- "kendujhar"

df_OR$District_Name[df_OR$Constituency_Name=="balipatna"
                    |df_OR$Constituency_Name=="begunia"
                    |df_OR$Constituency_Name=="chilka"
                    |df_OR$Constituency_Name=="jatni"
                    |df_OR$Constituency_Name=="khurda"
                    |df_OR$Constituency_Name=="bhubaneswar" & df_OR$before_delim==1]<- "khordha"

df_OR$District_Name[df_OR$Constituency_Name=="chitrakonda"
                    |df_OR$Constituency_Name=="jeypore"
                    |df_OR$Constituency_Name=="koraput"
                    |df_OR$Constituency_Name=="kotpad"
                    |df_OR$Constituency_Name=="lakshmipur"
                    |df_OR$Constituency_Name=="pottangi" & df_OR$before_delim==1]<- "koraput"

df_OR$District_Name[df_OR$Constituency_Name=="chitrakonda"
                    |df_OR$Constituency_Name=="malkangiri" & df_OR$before_delim==1]<- "malkangiri"

df_OR$District_Name[df_OR$Constituency_Name=="bahalda"
                    |df_OR$Constituency_Name=="baisinga"
                    |df_OR$Constituency_Name=="bangriposi"
                    |df_OR$Constituency_Name=="baripada"
                    |df_OR$Constituency_Name=="jashipur"
                    |df_OR$Constituency_Name=="karanjia"
                    |df_OR$Constituency_Name=="khunta"
                    |df_OR$Constituency_Name=="kuliana"
                    |df_OR$Constituency_Name=="rairangpur"
                    |df_OR$Constituency_Name=="udala" & df_OR$before_delim==1]<- "mayurbhanj"

df_OR$District_Name[df_OR$Constituency_Name=="dabugam"
                    |df_OR$Constituency_Name=="kodinga"
                    |df_OR$Constituency_Name=="nowrangpur"
                    |df_OR$Constituency_Name=="umarkote" & df_OR$before_delim==1]<- "nabarangapur"

df_OR$District_Name[df_OR$Constituency_Name=="daspalla"
                    |df_OR$Constituency_Name=="khandapara"
                    |df_OR$Constituency_Name=="nayagarh"
                    |df_OR$Constituency_Name=="ranpur" & df_OR$before_delim==1]<- "nayagarh"

df_OR$District_Name[df_OR$Constituency_Name=="dharamgarh"
                    |df_OR$Constituency_Name=="khariar"
                    |df_OR$Constituency_Name=="nawapara" & df_OR$before_delim==1]<- "nuapada"

df_OR$District_Name[df_OR$Constituency_Name=="brahmagiri"
                    |df_OR$Constituency_Name=="kakatpur"
                    |df_OR$Constituency_Name=="nimapara"
                    |df_OR$Constituency_Name=="pipli"
                    |df_OR$Constituency_Name=="puri"
                    |df_OR$Constituency_Name=="satyabadi" & df_OR$before_delim==1]<- "puri"

df_OR$District_Name[df_OR$Constituency_Name=="bissam-cuttack"
                    |df_OR$Constituency_Name=="gunupur"
                    |df_OR$Constituency_Name=="lakshmipur"
                    |df_OR$Constituency_Name=="rayagada" & df_OR$before_delim==1]<- "rayagada"

df_OR$District_Name[df_OR$Constituency_Name=="kuchinda"
                    |df_OR$Constituency_Name=="laikera"
                    |df_OR$Constituency_Name=="rairakhol" & df_OR$before_delim==1]<- "sambalpur"

df_OR$District_Name[df_OR$Constituency_Name=="binka"
                    |df_OR$Constituency_Name=="sonepur" & df_OR$before_delim==1]<- "sonapur"

df_OR$Constituency_Name[df_OR$Constituency_Name=="raghunathapali"]<- "raghunathpali"

df_OR$District_Name[df_OR$Constituency_Name=="biramitrapur"
                    |df_OR$Constituency_Name=="bonai"
                    |df_OR$Constituency_Name=="raghunathpali"
                    |df_OR$Constituency_Name=="rajgangpur"
                    |df_OR$Constituency_Name=="sundargarh"
                    |df_OR$Constituency_Name=="talsara"
                    |df_OR$Constituency_Name=="rourkela" & df_OR$before_delim==1]<- "sundargarh"

df_OR<- df_OR%>%
  subset(District_Name!="")

############################### Punjab #############################################

df_PU<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Punjab_2021-11-23.csv")

df_PU<- df_PU%>%
  subset(Year==1992|
           Year==1997|
           Year==2002|
           Year==2007|
           Year==2012)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_PU$District_Name[df_PU$Constituency_Name=="ajnala"
                    |df_PU$Constituency_Name=="amritsar central"
                    |df_PU$Constituency_Name=="amritsar north"
                    |df_PU$Constituency_Name=="amritsar south"
                    |df_PU$Constituency_Name=="amritsar west"
                    |df_PU$Constituency_Name=="attari"
                    |df_PU$Constituency_Name=="beas"
                    |df_PU$Constituency_Name=="jandiala"
                    |df_PU$Constituency_Name=="khadoor sahib"
                    |df_PU$Constituency_Name=="majitha"
                    |df_PU$Constituency_Name=="naushahra panwan"
                    |df_PU$Constituency_Name=="patti"
                    |df_PU$Constituency_Name=="raja sansi"
                    |df_PU$Constituency_Name=="tarn taran"
                    |df_PU$Constituency_Name=="valtoha"
                    |df_PU$Constituency_Name=="verka" & df_PU$before_delim==1]<- "amritsar"

df_PU$District_Name[df_PU$Constituency_Name=="bhatinda"
                    |df_PU$Constituency_Name=="joga"
                    |df_PU$Constituency_Name=="nathana"
                    |df_PU$Constituency_Name=="pakka kalan"
                    |df_PU$Constituency_Name=="rampura phul"
                    |df_PU$Constituency_Name=="talwandi sabo" & df_PU$before_delim==1]<- "bathinda"

df_PU$District_Name[df_PU$Constituency_Name=="faridkot"
                    |df_PU$Constituency_Name=="kot kapura"
                    |df_PU$Constituency_Name=="panjgrain" & df_PU$before_delim==1]<- "faridkot"

df_PU$District_Name[df_PU$Constituency_Name=="amloh"
                    |df_PU$Constituency_Name=="sirhind" & df_PU$before_delim==1]<- "fatehgarh sahib"

df_PU$Constituency_Name[df_PU$Constituency_Name=="ferozepur cantt."]<- "firozepur cantonment"

df_PU$District_Name[df_PU$Constituency_Name=="abohar"
                    |df_PU$Constituency_Name=="balluana"
                    |df_PU$Constituency_Name=="fazilka"
                    |df_PU$Constituency_Name=="firozepur"
                    |df_PU$Constituency_Name=="firozepur cantonment"
                    |df_PU$Constituency_Name=="guru har sahai"
                    |df_PU$Constituency_Name=="jalalabad"
                    |df_PU$Constituency_Name=="zira" & df_PU$before_delim==1]<- "firozpur"

df_PU$District_Name[df_PU$Constituency_Name=="gurdaspur"
                    |df_PU$Constituency_Name=="dhariwal"
                    |df_PU$Constituency_Name=="dina nagar"
                    |df_PU$Constituency_Name=="fatehgarh"
                    |df_PU$Constituency_Name=="gurdaspur"
                    |df_PU$Constituency_Name=="kahnuwan"
                    |df_PU$Constituency_Name=="narot mehra"
                    |df_PU$Constituency_Name=="pathankot"
                    |df_PU$Constituency_Name=="qadian"
                    |df_PU$Constituency_Name=="srihargobindpur"
                    |df_PU$Constituency_Name=="sujanpur"
                    |df_PU$Constituency_Name=="batala" & df_PU$before_delim==1]<- "gurdaspur"

df_PU$District_Name[df_PU$Constituency_Name=="dasuya"
                    |df_PU$Constituency_Name=="garhdiwala"
                    |df_PU$Constituency_Name=="garhshankar"
                    |df_PU$Constituency_Name=="hoshiarpur"
                    |df_PU$Constituency_Name=="mahilpur"
                    |df_PU$Constituency_Name=="mukerian"
                    |df_PU$Constituency_Name=="sham chaurasi"
                    |df_PU$Constituency_Name=="tanda" & df_PU$before_delim==1]<- "hoshiarpur"

df_PU$District_Name[df_PU$Constituency_Name=="adampur"
                    |df_PU$Constituency_Name=="banga"
                    |df_PU$Constituency_Name=="jullundur cantonment"
                    |df_PU$Constituency_Name=="jullundur central"
                    |df_PU$Constituency_Name=="jullundur north"
                    |df_PU$Constituency_Name=="jullundur south"
                    |df_PU$Constituency_Name=="kartarpur"
                    |df_PU$Constituency_Name=="lohian"
                    |df_PU$Constituency_Name=="nakodar"
                    |df_PU$Constituency_Name=="nur mahal"
                    |df_PU$Constituency_Name=="phillaur" & df_PU$before_delim==1]<- "jalandhar"

df_PU$District_Name[df_PU$Constituency_Name=="bholath"
                    |df_PU$Constituency_Name=="kapurthala"
                    |df_PU$Constituency_Name=="lohian"
                    |df_PU$Constituency_Name=="phagwara"
                    |df_PU$Constituency_Name=="sultanpur" & df_PU$before_delim==1]<- "kapurthala"

df_PU$District_Name[df_PU$Constituency_Name=="dakha"
                    |df_PU$Constituency_Name=="jagraon"
                    |df_PU$Constituency_Name=="khanna"
                    |df_PU$Constituency_Name=="kum kalan"
                    |df_PU$Constituency_Name=="ludhiana east"
                    |df_PU$Constituency_Name=="ludhiana north"
                    |df_PU$Constituency_Name=="ludhiana rural"
                    |df_PU$Constituency_Name=="ludhiana west"
                    |df_PU$Constituency_Name=="payal"
                    |df_PU$Constituency_Name=="qila raipur"
                    |df_PU$Constituency_Name=="raikot"
                    |df_PU$Constituency_Name=="samrala" & df_PU$before_delim==1]<- "ludhiana"

df_PU$District_Name[df_PU$Constituency_Name=="budhlada"
                    |df_PU$Constituency_Name=="joga"
                    |df_PU$Constituency_Name=="mansa"
                    |df_PU$Constituency_Name=="sardulgarh" & df_PU$before_delim==1]<- "mansa"

df_PU$District_Name[df_PU$Constituency_Name=="bagha purana"
                    |df_PU$Constituency_Name=="dharamkot"
                    |df_PU$Constituency_Name=="moga"
                    |df_PU$Constituency_Name=="nihal singh wala"
                    |df_PU$Constituency_Name=="panjgrain" & df_PU$before_delim==1]<- "moga"

df_PU$District_Name[df_PU$Constituency_Name=="giddar baha"
                    |df_PU$Constituency_Name=="lambi"
                    |df_PU$Constituency_Name=="malout"
                    |df_PU$Constituency_Name=="muktsar" & df_PU$before_delim==1]<- "muktsar"

df_PU$District_Name[df_PU$Constituency_Name=="balachaur"
                    |df_PU$Constituency_Name=="banga"
                    |df_PU$Constituency_Name=="nawan shahr"
                    |df_PU$Constituency_Name=="phillaur" & df_PU$before_delim==1]<- "nawanshahr"

df_PU$District_Name[df_PU$Constituency_Name=="banur"
                    |df_PU$Constituency_Name=="dakala"
                    |df_PU$Constituency_Name=="ghanaur"
                    |df_PU$Constituency_Name=="nabha"
                    |df_PU$Constituency_Name=="rajpura"
                    |df_PU$Constituency_Name=="samana"
                    |df_PU$Constituency_Name=="shutrana"
                    |df_PU$Constituency_Name=="patiala town" & df_PU$before_delim==1]<- "patiala"

df_PU$Constituency_Name[df_PU$Constituency_Name=="anandpur sahib-ropar"]<- "anandpur sahib - ropar"

df_PU$District_Name[df_PU$Constituency_Name=="anandpur sahib - ropar"
                    |df_PU$Constituency_Name=="chamkaur sahib"
                    |df_PU$Constituency_Name=="kharar"
                    |df_PU$Constituency_Name=="morinda"
                    |df_PU$Constituency_Name=="nangal" & df_PU$before_delim==1]<- "rupnagar"

df_PU$District_Name[df_PU$Constituency_Name=="bhadaur"
                    |df_PU$Constituency_Name=="dhanaula"
                    |df_PU$Constituency_Name=="dhuri"
                    |df_PU$Constituency_Name=="dirbha"
                    |df_PU$Constituency_Name=="lehra"
                    |df_PU$Constituency_Name=="malerkotla"
                    |df_PU$Constituency_Name=="sangrur"
                    |df_PU$Constituency_Name=="sherpur"
                    |df_PU$Constituency_Name=="sunam"
                    |df_PU$Constituency_Name=="barnala" & df_PU$before_delim==1]<- "sangrur"

df_PU<- df_PU%>%
  subset(District_Name!="")

############################################ Rajasthan #############################################################

df_RJ<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Rajasthan_2021-11-23.csv")

df_RJ<- df_RJ%>%
  subset(Year==1990|
           Year==1993|
           Year==1998|
           Year==2003|
           Year==2008|
           Year==2013)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_RJ$District_Name[df_RJ$Constituency_Name=="ajmer east"
                    |df_RJ$Constituency_Name=="ajmer west"
                    |df_RJ$Constituency_Name=="beawar"
                    |df_RJ$Constituency_Name=="bhinai"
                    |df_RJ$Constituency_Name=="kekri"
                    |df_RJ$Constituency_Name=="kishangarh"
                    |df_RJ$Constituency_Name=="masuda"
                    |df_RJ$Constituency_Name=="nasirabad"
                    |df_RJ$Constituency_Name=="pushkar" & df_RJ$before_delim==1]<- "ajmer"

df_RJ$District_Name[df_RJ$Constituency_Name=="alwar"
                    |df_RJ$Constituency_Name=="bansur"
                    |df_RJ$Constituency_Name=="behror"
                    |df_RJ$Constituency_Name=="kathumar"
                    |df_RJ$Constituency_Name=="khairthal"
                    |df_RJ$Constituency_Name=="lachhmangarh"
                    |df_RJ$Constituency_Name=="mandawar"
                    |df_RJ$Constituency_Name=="rajgarh"
                    |df_RJ$Constituency_Name=="ramgarh"
                    |df_RJ$Constituency_Name=="thanagazi"
                    |df_RJ$Constituency_Name=="tizara" & df_RJ$before_delim==1]<- "alwar"

df_RJ$District_Name[df_RJ$Constituency_Name=="bagidora"
                    |df_RJ$Constituency_Name=="banswara"
                    |df_RJ$Constituency_Name=="danpur"
                    |df_RJ$Constituency_Name=="ghatol"
                    |df_RJ$Constituency_Name=="kushalgarh" & df_RJ$before_delim==1]<- "banswara"

df_RJ$District_Name[df_RJ$Constituency_Name=="atru"
                    |df_RJ$Constituency_Name=="baran"
                    |df_RJ$Constituency_Name=="chhabra"
                    |df_RJ$Constituency_Name=="digod"
                    |df_RJ$Constituency_Name=="kishanganj" & df_RJ$before_delim==1]<- "baran"

df_RJ$District_Name[df_RJ$Constituency_Name=="barmer"
                    |df_RJ$Constituency_Name=="chohtan"
                    |df_RJ$Constituency_Name=="gudamalani"
                    |df_RJ$Constituency_Name=="pachpadra"
                    |df_RJ$Constituency_Name=="sheo"
                    |df_RJ$Constituency_Name=="siwana" & df_RJ$before_delim==1]<- "barmer"

df_RJ$Constituency_Name[df_RJ$Constituency_Name=="weir(sc)"]<- "weir"

df_RJ$District_Name[df_RJ$Constituency_Name=="bharatpur"
                    |df_RJ$Constituency_Name=="deeg"
                    |df_RJ$Constituency_Name=="kaman"
                    |df_RJ$Constituency_Name=="kumher"
                    |df_RJ$Constituency_Name=="nadbai"
                    |df_RJ$Constituency_Name=="nagar"
                    |df_RJ$Constituency_Name=="rupbas"
                    |df_RJ$Constituency_Name=="weir" & df_RJ$before_delim==1]<- "bharatpur"

df_RJ$District_Name[df_RJ$Constituency_Name=="asind"
                    |df_RJ$Constituency_Name=="banera"
                    |df_RJ$Constituency_Name=="bhilwara"
                    |df_RJ$Constituency_Name=="jahazpur"
                    |df_RJ$Constituency_Name=="mandal"
                    |df_RJ$Constituency_Name=="mandalgarh"
                    |df_RJ$Constituency_Name=="sahada"
                    |df_RJ$Constituency_Name=="shahpura" & df_RJ$before_delim==1]<- "bhilwara"

df_RJ$District_Name[df_RJ$Constituency_Name=="bikaner"
                    |df_RJ$Constituency_Name=="kolayat"
                    |df_RJ$Constituency_Name=="lunkaransar"
                    |df_RJ$Constituency_Name=="nokha" & df_RJ$before_delim==1]<- "bikaner"

df_RJ$District_Name[df_RJ$Constituency_Name=="bundi"
                    |df_RJ$Constituency_Name=="hindoli"
                    |df_RJ$Constituency_Name=="nainwa"
                    |df_RJ$Constituency_Name=="patan"
                    |df_RJ$Constituency_Name=="pipalda" & df_RJ$before_delim==1]<- "bundi"

df_RJ$District_Name[df_RJ$Constituency_Name=="badi sadri"
                    |df_RJ$Constituency_Name=="begun"
                    |df_RJ$Constituency_Name=="chittorgarh"
                    |df_RJ$Constituency_Name=="gangrar"
                    |df_RJ$Constituency_Name=="kapasin"
                    |df_RJ$Constituency_Name=="nimbahera"
                    |df_RJ$Constituency_Name=="pratapgarh" & df_RJ$before_delim==1]<- "chittaurgarh"

df_RJ$District_Name[df_RJ$Constituency_Name=="churu"
                    |df_RJ$Constituency_Name=="dungargarh"
                    |df_RJ$Constituency_Name=="ratangarh"
                    |df_RJ$Constituency_Name=="sadulpur"
                    |df_RJ$Constituency_Name=="sardarshahar"
                    |df_RJ$Constituency_Name=="sujangarh"
                    |df_RJ$Constituency_Name=="taranagar" & df_RJ$before_delim==1]<- "churu"

df_RJ$District_Name[df_RJ$Constituency_Name=="bandikui"
                    |df_RJ$Constituency_Name=="dausa"
                    |df_RJ$Constituency_Name=="lalsot"
                    |df_RJ$Constituency_Name=="mahuwa"
                    |df_RJ$Constituency_Name=="sikrai" & df_RJ$before_delim==1]<- "dausa"

df_RJ$District_Name[df_RJ$Constituency_Name=="bari"
                    |df_RJ$Constituency_Name=="bayana"
                    |df_RJ$Constituency_Name=="dholpur"
                    |df_RJ$Constituency_Name=="rajakhera" & df_RJ$before_delim==1]<- "dhaulpur"

df_RJ$District_Name[df_RJ$Constituency_Name=="aspur"
                    |df_RJ$Constituency_Name=="chorasi"
                    |df_RJ$Constituency_Name=="dungarpur"
                    |df_RJ$Constituency_Name=="sagwara" & df_RJ$before_delim==1]<- "dungarpur"

df_RJ$District_Name[df_RJ$Constituency_Name=="ganganagar"
                    |df_RJ$Constituency_Name=="karanpur"
                    |df_RJ$Constituency_Name=="kesrisinghpur"
                    |df_RJ$Constituency_Name=="pilibanga"
                    |df_RJ$Constituency_Name=="raisinghnagar"
                    |df_RJ$Constituency_Name=="sangaria"
                    |df_RJ$Constituency_Name=="suratgarh" & df_RJ$before_delim==1]<- "ganganagar"

df_RJ$District_Name[df_RJ$Constituency_Name=="bhadra"
                    |df_RJ$Constituency_Name=="hanumangarh"
                    |df_RJ$Constituency_Name=="nohar"
                    |df_RJ$Constituency_Name=="pilibanga"
                    |df_RJ$Constituency_Name=="sangaria"
                    |df_RJ$Constituency_Name=="tibi" & df_RJ$before_delim==1]<- "hanumangarh"

df_RJ$District_Name[df_RJ$Constituency_Name=="amber"
                    |df_RJ$Constituency_Name=="bairath"
                    |df_RJ$Constituency_Name=="bani park"
                    |df_RJ$Constituency_Name=="bassi"
                    |df_RJ$Constituency_Name=="chomu"
                    |df_RJ$Constituency_Name=="dausa"
                    |df_RJ$Constituency_Name=="dudu"
                    |df_RJ$Constituency_Name=="jamwa ramgarh"
                    |df_RJ$Constituency_Name=="johribazar"
                    |df_RJ$Constituency_Name=="kishanpole"
                    |df_RJ$Constituency_Name=="kotputli"
                    |df_RJ$Constituency_Name=="phagi"
                    |df_RJ$Constituency_Name=="phulera"
                    |df_RJ$Constituency_Name=="sanganer" & df_RJ$before_delim==1]<- "jaipur"

df_RJ$District_Name[df_RJ$Constituency_Name=="jaisalmer"
                    |df_RJ$Constituency_Name=="sheo" & df_RJ$before_delim==1]<- "jaisalmer"

df_RJ$District_Name[df_RJ$Constituency_Name=="ahore"
                    |df_RJ$Constituency_Name=="bhinmal"
                    |df_RJ$Constituency_Name=="jalore"
                    |df_RJ$Constituency_Name=="raniwara"
                    |df_RJ$Constituency_Name=="sanchore" & df_RJ$before_delim==1]<- "jalor"

df_RJ$District_Name[df_RJ$Constituency_Name=="jhalrapatan"
                    |df_RJ$Constituency_Name=="khanpur"
                    |df_RJ$Constituency_Name=="manohar thana"
                    |df_RJ$Constituency_Name=="pirawa" & df_RJ$before_delim==1]<- "jhalawar"

df_RJ$District_Name[df_RJ$Constituency_Name=="gudha"
                    |df_RJ$Constituency_Name=="jhunjhunu"
                    |df_RJ$Constituency_Name=="khetri"
                    |df_RJ$Constituency_Name=="mandawa"
                    |df_RJ$Constituency_Name=="pilani"
                    |df_RJ$Constituency_Name=="surajgarh" & df_RJ$before_delim==1]<- "jhunjhunun"

df_RJ$District_Name[df_RJ$Constituency_Name=="bhopalgarh"
                    |df_RJ$Constituency_Name=="bilara"
                    |df_RJ$Constituency_Name=="jodhpur"
                    |df_RJ$Constituency_Name=="luni"
                    |df_RJ$Constituency_Name=="osian"
                    |df_RJ$Constituency_Name=="phalodi"
                    |df_RJ$Constituency_Name=="sardarpura"
                    |df_RJ$Constituency_Name=="shergarh"
                    |df_RJ$Constituency_Name=="sursagar" & df_RJ$before_delim==1]<- "jodhpur"

df_RJ$District_Name[df_RJ$Constituency_Name=="hindaun"
                    |df_RJ$Constituency_Name=="karauli"
                    |df_RJ$Constituency_Name=="sapotra"
                    |df_RJ$Constituency_Name=="toda bhim" & df_RJ$before_delim==1]<- "karauli"

df_RJ$District_Name[df_RJ$Constituency_Name=="digod"
                    |df_RJ$Constituency_Name=="ladpura"
                    |df_RJ$Constituency_Name=="pipalda"
                    |df_RJ$Constituency_Name=="ramganjmandi" & df_RJ$before_delim==1]<- "kota"

df_RJ$District_Name[df_RJ$Constituency_Name=="deedwana"
                    |df_RJ$Constituency_Name=="degana"
                    |df_RJ$Constituency_Name=="jayal"
                    |df_RJ$Constituency_Name=="ladnu"
                    |df_RJ$Constituency_Name=="makrana"
                    |df_RJ$Constituency_Name=="merta"
                    |df_RJ$Constituency_Name=="mundwa"
                    |df_RJ$Constituency_Name=="nagaur"
                    |df_RJ$Constituency_Name=="nawan"
                    |df_RJ$Constituency_Name=="parbatsar" & df_RJ$before_delim==1]<- "nagaur"

df_RJ$District_Name[df_RJ$Constituency_Name=="bali"
                    |df_RJ$Constituency_Name=="desuri"
                    |df_RJ$Constituency_Name=="jaitaran"
                    |df_RJ$Constituency_Name=="kharchi"
                    |df_RJ$Constituency_Name=="pali"
                    |df_RJ$Constituency_Name=="raipur"
                    |df_RJ$Constituency_Name=="sojat"
                    |df_RJ$Constituency_Name=="sumerpur" & df_RJ$before_delim==1]<- "pali"

df_RJ$District_Name[df_RJ$Constituency_Name=="bhim"
                    |df_RJ$Constituency_Name=="kumbhalgarh"
                    |df_RJ$Constituency_Name=="rajsamand" & df_RJ$before_delim==1]<- "rajsamand"

df_RJ$District_Name[df_RJ$Constituency_Name=="bamanwas"
                    |df_RJ$Constituency_Name=="khandar"
                    |df_RJ$Constituency_Name=="sawai madhopur" & df_RJ$before_delim==1]<- "sawai madhopur"

df_RJ$District_Name[df_RJ$Constituency_Name=="danta - ramgarh"
                    |df_RJ$Constituency_Name=="dhod"
                    |df_RJ$Constituency_Name=="fatehpur"
                    |df_RJ$Constituency_Name=="khandela"
                    |df_RJ$Constituency_Name=="lachhmangarh"
                    |df_RJ$Constituency_Name=="neem-ka-thana"
                    |df_RJ$Constituency_Name=="sikar"
                    |df_RJ$Constituency_Name=="srimadhopur" & df_RJ$before_delim==1]<- "sikar"

df_RJ$District_Name[df_RJ$Constituency_Name=="pindwara abu"
                    |df_RJ$Constituency_Name=="reodar"
                    |df_RJ$Constituency_Name=="sirohi" & df_RJ$before_delim==1]<- "sirohi"

df_RJ$District_Name[df_RJ$Constituency_Name=="malpura"
                    |df_RJ$Constituency_Name=="niwai"
                    |df_RJ$Constituency_Name=="todaraisingh"
                    |df_RJ$Constituency_Name=="tonk"
                    |df_RJ$Constituency_Name=="uniara" & df_RJ$before_delim==1]<- "tonk"

df_RJ$Constituency_Name[df_RJ$Constituency_Name=="gogunda"]<- "gongunda"

df_RJ$District_Name[df_RJ$Constituency_Name=="gongunda"
                    |df_RJ$Constituency_Name=="kherwara"
                    |df_RJ$Constituency_Name=="lasadia"
                    |df_RJ$Constituency_Name=="mavli"
                    |df_RJ$Constituency_Name=="nathdwara"
                    |df_RJ$Constituency_Name=="phalasia"
                    |df_RJ$Constituency_Name=="salumber"
                    |df_RJ$Constituency_Name=="sarada"
                    |df_RJ$Constituency_Name=="udaipur"
                    |df_RJ$Constituency_Name=="udaipur rural"
                    |df_RJ$Constituency_Name=="vallabhnagar" & df_RJ$before_delim==1]<- "udaipur"

df_RJ<- df_RJ%>%
  subset(District_Name!="")

################################ Tamil Nadu ###############################

df_TN<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Tamil_Nadu_2021-11-23.csv")

df_TN<- df_TN%>%
  subset(Year==1991|
           Year==1996|
           Year==2001|
           Year==2006|
           Year==2011|
           Year==2016)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_TN$District_Name[df_TN$Constituency_Name=="andimadam"
                    |df_TN$Constituency_Name=="ariyalur"
                    |df_TN$Constituency_Name=="jayankondam" & df_TN$before_delim==1]<- "ariyalur"

df_TN$District_Name[df_TN$Constituency_Name=="avanashi"
                    |df_TN$Constituency_Name=="dharapuram"
                    |df_TN$Constituency_Name=="kinathukkadavu"
                    |df_TN$Constituency_Name=="mettupalayam"
                    |df_TN$Constituency_Name=="palladam"
                    |df_TN$Constituency_Name=="perur"
                    |df_TN$Constituency_Name=="pollachi"
                    |df_TN$Constituency_Name=="pongalur"
                    |df_TN$Constituency_Name=="thondamuthur"
                    |df_TN$Constituency_Name=="tiruppur"
                    |df_TN$Constituency_Name=="udumalpet"
                    |df_TN$Constituency_Name=="valparai"
                    |df_TN$Constituency_Name=="coimbatore east"
                    |df_TN$Constituency_Name=="coimbatore west"
                    |df_TN$Constituency_Name=="singanallur" & df_TN$before_delim==1]<- "coimbatore"

df_TN$District_Name[df_TN$Constituency_Name=="bhuvanagiri"
                    |df_TN$Constituency_Name=="chidambaram"
                    |df_TN$Constituency_Name=="cuddalore"
                    |df_TN$Constituency_Name=="kattumannarkoil"
                    |df_TN$Constituency_Name=="kurinjipadi"
                    |df_TN$Constituency_Name=="mangalore"
                    |df_TN$Constituency_Name=="nellikuppam"
                    |df_TN$Constituency_Name=="panruti"
                    |df_TN$Constituency_Name=="ulundurpet"
                    |df_TN$Constituency_Name=="vridhachalam" & df_TN$before_delim==1]<- "cuddalore"

df_RJ$Constituency_Name[df_RJ$Constituency_Name=="dharamapuri"]<- "dharmapuri"

df_TN$District_Name[df_TN$Constituency_Name=="bargur"
                    |df_TN$Constituency_Name=="dharmapuri"
                    |df_TN$Constituency_Name=="harur"
                    |df_TN$Constituency_Name=="hosur"
                    |df_TN$Constituency_Name=="kaveripattinam"
                    |df_TN$Constituency_Name=="krishnagiri"
                    |df_TN$Constituency_Name=="morappur"
                    |df_TN$Constituency_Name=="palacode"
                    |df_TN$Constituency_Name=="pennagaram"
                    |df_TN$Constituency_Name=="thalli" & df_TN$before_delim==1]<- "dharmapuri"

df_TN$District_Name[df_TN$Constituency_Name=="athoor"
                    |df_TN$Constituency_Name=="dindigul"
                    |df_TN$Constituency_Name=="natham"
                    |df_TN$Constituency_Name=="nilakottai"
                    |df_TN$Constituency_Name=="oddanchatram"
                    |df_TN$Constituency_Name=="palani"
                    |df_TN$Constituency_Name=="periyakulam"
                    |df_TN$Constituency_Name=="vedasandur" & df_TN$before_delim==1]<- "dindigul"

df_TN$District_Name[df_TN$Constituency_Name=="andhiyur"
                    |df_TN$Constituency_Name=="bhavani"
                    |df_TN$Constituency_Name=="bhavanisagar"
                    |df_TN$Constituency_Name=="erode"
                    |df_TN$Constituency_Name=="gobichettipalayam"
                    |df_TN$Constituency_Name=="kangayam"
                    |df_TN$Constituency_Name=="modakurichi"
                    |df_TN$Constituency_Name=="perundurai"
                    |df_TN$Constituency_Name=="sathyamangalam"
                    |df_TN$Constituency_Name=="vellakoil" & df_TN$before_delim==1]<- "erode"

df_TN$District_Name[df_TN$Constituency_Name=="acharapakkam"
                    |df_TN$Constituency_Name=="chengalpattu"
                    |df_TN$Constituency_Name=="kancheepuram"
                    |df_TN$Constituency_Name=="maduranthakam"
                    |df_TN$Constituency_Name=="sriperumbudur"
                    |df_TN$Constituency_Name=="tirupporur"
                    |df_TN$Constituency_Name=="uthiramerur"
                    |df_TN$Constituency_Name=="alandur"
                    |df_TN$Constituency_Name=="tambaram" & df_TN$before_delim==1]<- "kancheepuram"

df_TN$District_Name[df_TN$Constituency_Name=="colachel"
                    |df_TN$Constituency_Name=="kanniyakumari"
                    |df_TN$Constituency_Name=="killiyoor"
                    |df_TN$Constituency_Name=="nagercoil"
                    |df_TN$Constituency_Name=="padmanabhapuram"
                    |df_TN$Constituency_Name=="thiruvattar"
                    |df_TN$Constituency_Name=="vilavancode" & df_TN$before_delim==1]<- "kanniyakumari"

df_TN$District_Name[df_TN$Constituency_Name=="aravakurichi"
                    |df_TN$Constituency_Name=="karur"
                    |df_TN$Constituency_Name=="krishnarayapuram"
                    |df_TN$Constituency_Name=="kulithalai" & df_TN$before_delim==1]<- "kapur"

df_TN$District_Name[df_TN$Constituency_Name=="aruppukottai"
                    |df_TN$Constituency_Name=="melur"
                    |df_TN$Constituency_Name=="samayanallur"
                    |df_TN$Constituency_Name=="sedapatti"
                    |df_TN$Constituency_Name=="sholavandan"
                    |df_TN$Constituency_Name=="thirumangalam"
                    |df_TN$Constituency_Name=="tirupparankundram"
                    |df_TN$Constituency_Name=="usilampatti"
                    |df_TN$Constituency_Name=="madurai central"
                    |df_TN$Constituency_Name=="madurai east"
                    |df_TN$Constituency_Name=="madurai west" & df_TN$before_delim==1]<- "madurai"

df_TN$District_Name[df_TN$Constituency_Name=="kuttalam"
                    |df_TN$Constituency_Name=="mayuram"
                    |df_TN$Constituency_Name=="nagapattinam"
                    |df_TN$Constituency_Name=="nannilam"
                    |df_TN$Constituency_Name=="poompuhar"
                    |df_TN$Constituency_Name=="sirkali"
                    |df_TN$Constituency_Name=="tiruvarur"
                    |df_TN$Constituency_Name=="vedaranyam" & df_TN$before_delim==1]<- "nagapattinam"

df_TN$District_Name[df_TN$Constituency_Name=="kapilamalai"
                    |df_TN$Constituency_Name=="namakkal"
                    |df_TN$Constituency_Name=="rasipuram"
                    |df_TN$Constituency_Name=="sankari"
                    |df_TN$Constituency_Name=="sendamangalam"
                    |df_TN$Constituency_Name=="tiruchengode" & df_TN$before_delim==1]<- "namakkal"

df_TN$District_Name[df_TN$Constituency_Name=="perambalur"
                    |df_TN$Constituency_Name=="varahur" & df_TN$before_delim==1]<- "perambalur"

df_RJ$Constituency_Name[df_RJ$Constituency_Name=="thirumaiyam"]<- "thirumayam"

df_TN$District_Name[df_TN$Constituency_Name=="alangudi"
                    |df_TN$Constituency_Name=="arantangi"
                    |df_TN$Constituency_Name=="kolathur"
                    |df_TN$Constituency_Name=="pudukkottai"
                    |df_TN$Constituency_Name=="thirumayam" & df_TN$before_delim==1]<- "pudukkottai"

df_TN$District_Name[df_TN$Constituency_Name=="ilayangudi"
                    |df_TN$Constituency_Name=="kadaladi"
                    |df_TN$Constituency_Name=="mudukulathur"
                    |df_TN$Constituency_Name=="paramakudi"
                    |df_TN$Constituency_Name=="ramanathapuram"
                    |df_TN$Constituency_Name=="tiruvadanai" & df_TN$before_delim==1]<- "ramanathapuram"

df_TN$District_Name[df_TN$Constituency_Name=="attur"
                    |df_TN$Constituency_Name=="edapadi"
                    |df_TN$Constituency_Name=="mettur"
                    |df_TN$Constituency_Name=="omalur"
                    |df_TN$Constituency_Name=="panamarathupatty"
                    |df_TN$Constituency_Name=="salem-i"
                    |df_TN$Constituency_Name=="salem-ii"
                    |df_TN$Constituency_Name=="sankari"
                    |df_TN$Constituency_Name=="talavasal"
                    |df_TN$Constituency_Name=="taramangalam"
                    |df_TN$Constituency_Name=="veerapandi"
                    |df_TN$Constituency_Name=="yercaud" & df_TN$before_delim==1]<- "salem"

df_TN$District_Name[df_TN$Constituency_Name=="ilayangudi"
                    |df_TN$Constituency_Name=="karaikudi"
                    |df_TN$Constituency_Name=="manamadurai"
                    |df_TN$Constituency_Name=="sivaganga"
                    |df_TN$Constituency_Name=="tiruppattur"
                    |df_TN$Constituency_Name=="tiruvadanai" & df_TN$before_delim==1]<- "sivaganga"

df_TN$District_Name[df_TN$Constituency_Name=="kumbakonam"
                    |df_TN$Constituency_Name=="orathanad"
                    |df_TN$Constituency_Name=="papanasam"
                    |df_TN$Constituency_Name=="pattukkottai"
                    |df_TN$Constituency_Name=="peravurani"
                    |df_TN$Constituency_Name=="thanjavur"
                    |df_TN$Constituency_Name=="thiruvidamarudur"
                    |df_TN$Constituency_Name=="thiruvonam"
                    |df_TN$Constituency_Name=="tiruvaiyaru"
                    |df_TN$Constituency_Name=="valangiman" & df_TN$before_delim==1]<- "thanjavur"

df_TN$District_Name[df_TN$Constituency_Name=="coonoor"
                    |df_TN$Constituency_Name=="gudalur"
                    |df_TN$Constituency_Name=="ootacamund" & df_TN$before_delim==1]<- "the nilgiris"

df_TN$District_Name[df_TN$Constituency_Name=="andipatti"
                    |df_TN$Constituency_Name=="bodinayakkanur"
                    |df_TN$Constituency_Name=="periyakulam"
                    |df_TN$Constituency_Name=="theni"
                    |df_TN$Constituency_Name=="cumbum" & df_TN$before_delim==1]<- "theni"

df_TN$District_Name[df_TN$Constituency_Name=="gummidipundi"
                    |df_TN$Constituency_Name=="pallipet"
                    |df_TN$Constituency_Name=="ponneri"
                    |df_TN$Constituency_Name=="poonamallee"
                    |df_TN$Constituency_Name=="thiruvottiyur"
                    |df_TN$Constituency_Name=="tiruttani"
                    |df_TN$Constituency_Name=="tiruvallur"
                    |df_TN$Constituency_Name=="villivakkam" & df_TN$before_delim==1]<- "thiruvallur"

df_TN$Constituency_Name[df_TN$Constituency_Name=="thiruthuraipundi"]<- "tiruthuraipundi"

df_TN$District_Name[df_TN$Constituency_Name=="kuttalam"
                    |df_TN$Constituency_Name=="mannargudi"
                    |df_TN$Constituency_Name=="nannilam"
                    |df_TN$Constituency_Name=="orathanad"
                    |df_TN$Constituency_Name=="tiruthuraipundi"
                    |df_TN$Constituency_Name=="tiruvarur"
                    |df_TN$Constituency_Name=="valangiman" & df_TN$before_delim==1]<- "thiruvarur"

df_TN$District_Name[df_TN$Constituency_Name=="koilpatti"
                    |df_TN$Constituency_Name=="pottapidaram"
                    |df_TN$Constituency_Name=="sattangulam"
                    |df_TN$Constituency_Name=="srivaikuntam"
                    |df_TN$Constituency_Name=="tiruchendur"
                    |df_TN$Constituency_Name=="vilathikulam"
                    |df_TN$Constituency_Name=="tuticorin" & df_TN$before_delim==1]<- "toothukudi"

df_TN$Constituency_Name[df_TN$Constituency_Name=="thiruchirappalli-i"]<- "tiruchirapalli-i"
df_TN$Constituency_Name[df_TN$Constituency_Name=="thiruchirappalli-ii"]<- "tiruchirapalli-ii"
df_TN$Constituency_Name[df_TN$Constituency_Name=="tiruchirappalli-i"]<- "tiruchirapalli-i"
df_TN$Constituency_Name[df_TN$Constituency_Name=="tiruchirappalli-ii"]<- "tiruchirapalli-ii"
df_TN$Constituency_Name[df_TN$Constituency_Name=="uppiliapuram(st)"]<- "uppiliapuram"

df_TN$District_Name[df_TN$Constituency_Name=="kulithalai"
                    |df_TN$Constituency_Name=="lalgudi"
                    |df_TN$Constituency_Name=="marungapuri"
                    |df_TN$Constituency_Name=="musiri"
                    |df_TN$Constituency_Name=="srirangam"
                    |df_TN$Constituency_Name=="thiruverambur"
                    |df_TN$Constituency_Name=="thottiam"
                    |df_TN$Constituency_Name=="tiruchirapalli-i"
                    |df_TN$Constituency_Name=="tiruchirapalli-ii"
                    |df_TN$Constituency_Name=="uppiliapuram" & df_TN$before_delim==1]<- "tiruchirappalli"

df_TN$District_Name[df_TN$Constituency_Name=="alangulam"
                    |df_TN$Constituency_Name=="ambasamudram"
                    |df_TN$Constituency_Name=="cheranmahadevi"
                    |df_TN$Constituency_Name=="kadayanallur"
                    |df_TN$Constituency_Name=="nanguneri"
                    |df_TN$Constituency_Name=="palayamcottai"
                    |df_TN$Constituency_Name=="radhapuram"
                    |df_TN$Constituency_Name=="sankaranayanarkoil"
                    |df_TN$Constituency_Name=="tenkasi"
                    |df_TN$Constituency_Name=="tirunelveli"
                    |df_TN$Constituency_Name=="vasudevanallur" & df_TN$before_delim==1]<- "tirunelveli"

df_TN$Constituency_Name[df_TN$Constituency_Name=="vandavashi"]<- "vandavasi"

df_TN$District_Name[df_TN$Constituency_Name=="arni"
                    |df_TN$Constituency_Name=="chengam"
                    |df_TN$Constituency_Name=="cheyyar"
                    |df_TN$Constituency_Name=="gingee"
                    |df_TN$Constituency_Name=="kalasapakkam"
                    |df_TN$Constituency_Name=="melmalayanur"
                    |df_TN$Constituency_Name=="peranamallur"
                    |df_TN$Constituency_Name=="polur"
                    |df_TN$Constituency_Name=="thandarambattu"
                    |df_TN$Constituency_Name=="tiruvannamalai"
                    |df_TN$Constituency_Name=="uthiramerur"
                    |df_TN$Constituency_Name=="vandavasi" & df_TN$before_delim==1]<- "tiruvanamalai"

df_TN$District_Name[df_TN$Constituency_Name=="anaicut"
                    |df_TN$Constituency_Name=="arcot"
                    |df_TN$Constituency_Name=="arakkonam"
                    |df_TN$Constituency_Name=="gudiyatham"
                    |df_TN$Constituency_Name=="katpadi"
                    |df_TN$Constituency_Name=="natrampalli"
                    |df_TN$Constituency_Name=="pernambut"
                    |df_TN$Constituency_Name=="ranipet"
                    |df_TN$Constituency_Name=="sholinghur"
                    |df_TN$Constituency_Name=="tiruppattur"
                    |df_TN$Constituency_Name=="vaniayambadi"
                    |df_TN$Constituency_Name=="vellore"
                    |df_TN$Constituency_Name=="arkonam" & df_TN$before_delim==1]<- "vellore"

df_TN$Constituency_Name[df_TN$Constituency_Name=="tiruvallur"]<- "thirunavalur"

df_TN$District_Name[df_TN$Constituency_Name=="chinnasalem"
                    |df_TN$Constituency_Name=="gingee"
                    |df_TN$Constituency_Name=="kandamangalam"
                    |df_TN$Constituency_Name=="melmalayanur"
                    |df_TN$Constituency_Name=="mugaiyur"
                    |df_TN$Constituency_Name=="rishivandiam"
                    |df_TN$Constituency_Name=="sankarapuram"
                    |df_TN$Constituency_Name=="thirunavalur"
                    |df_TN$Constituency_Name=="tindivanam"
                    |df_TN$Constituency_Name=="ulundurpet"
                    |df_TN$Constituency_Name=="vanur"
                    |df_TN$Constituency_Name=="villupuram" & df_TN$before_delim==1]<- "viluppuram"

df_TN$District_Name[df_TN$Constituency_Name=="aruppukottai"
                    |df_TN$Constituency_Name=="rajapalayam"
                    |df_TN$Constituency_Name=="sattur"
                    |df_TN$Constituency_Name=="sivakasi"
                    |df_TN$Constituency_Name=="srivilliputhur"
                    |df_TN$Constituency_Name=="virudhunagar" & df_TN$before_delim==1]<- "virudhunagar"

df_TN<- df_TN%>%
  subset(District_Name!="")

######################## Tripura ####################################

df_TR<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Tripura_2021-11-23.csv")

df_TR<- df_TR%>%
  subset(Year==1993|
           Year==1998|
           Year==2003|
           Year==2008|
           Year==2013)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_TR$District_Name[df_TR$Constituency_Name=="chhawmanu"
                    |df_TR$Constituency_Name=="kamalpur"
                    |df_TR$Constituency_Name=="pabiachhara"
                    |df_TR$Constituency_Name=="raima valley"
                    |df_TR$Constituency_Name=="surma" & df_TR$before_delim==1]<- "dhalai"

df_TR$District_Name[df_TR$Constituency_Name=="chandipur"
                    |df_TR$Constituency_Name=="dharmanagar"
                    |df_TR$Constituency_Name=="fatikroy"
                    |df_TR$Constituency_Name=="jubarajnagar"
                    |df_TR$Constituency_Name=="kadamtala - kurti"
                    |df_TR$Constituency_Name=="kailasahar"
                    |df_TR$Constituency_Name=="kanchanpur"
                    |df_TR$Constituency_Name=="panisagar"
                    |df_TR$Constituency_Name=="pencharthal" & df_TR$before_delim==1]<- "north tripura"

df_TR$District_Name[df_TR$Constituency_Name=="ampinagar"
                    |df_TR$Constituency_Name=="bagma"
                    |df_TR$Constituency_Name=="belonia"
                    |df_TR$Constituency_Name=="hrishyamukh"
                    |df_TR$Constituency_Name=="kakraban-salgarah"
                    |df_TR$Constituency_Name=="manu"
                    |df_TR$Constituency_Name=="matarbari"
                    |df_TR$Constituency_Name=="radhakishorepur"
                    |df_TR$Constituency_Name=="raima valley"
                    |df_TR$Constituency_Name=="rajnagar"
                    |df_TR$Constituency_Name=="sabroom"
                    |df_TR$Constituency_Name=="santirbazar" & df_TR$before_delim==1]<- "south tripura"

df_TR$District_Name[df_TR$Constituency_Name=="agartala"
                    |df_TR$Constituency_Name=="asharambari"
                    |df_TR$Constituency_Name=="badharghat"
                    |df_TR$Constituency_Name=="bamutia"
                    |df_TR$Constituency_Name=="barjala"
                    |df_TR$Constituency_Name=="bishalgarh"
                    |df_TR$Constituency_Name=="boxanagar"
                    |df_TR$Constituency_Name=="charilam"
                    |df_TR$Constituency_Name=="dhanpur"
                    |df_TR$Constituency_Name=="golaghati"
                    |df_TR$Constituency_Name=="kamalasagar"
                    |df_TR$Constituency_Name=="khayerpur"
                    |df_TR$Constituency_Name=="khowai"
                    |df_TR$Constituency_Name=="krishnapur"
                    |df_TR$Constituency_Name=="majlishpur"
                    |df_TR$Constituency_Name=="mandaibazar"
                    |df_TR$Constituency_Name=="mohanpur"
                    |df_TR$Constituency_Name=="nalchar"
                    |df_TR$Constituency_Name=="pratapgarh"
                    |df_TR$Constituency_Name=="ramchandraghat"
                    |df_TR$Constituency_Name=="simna"
                    |df_TR$Constituency_Name=="sonamura"
                    |df_TR$Constituency_Name=="takarjala" & df_TR$before_delim==1]<- "west tripura"

df_TR<- df_TR%>%
  subset(District_Name!="")

############################################ Uttar Pradesh #############################################

df_UP<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Uttar_Pradesh_2021-11-23.csv")

df_UP<- df_UP%>%
  subset(Year==1991|
           Year==1993|
           Year==1996|
           Year==2002|
           Year==2007|
           Year==2012)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_UP$District_Name[df_UP$Constituency_Name=="agra cantonment"
                    |df_UP$Constituency_Name=="agra east"
                    |df_UP$Constituency_Name=="agra west"
                    |df_UP$Constituency_Name=="ballia"
                    |df_UP$Constituency_Name=="bansgaon"
                    |df_UP$Constituency_Name=="gopalpur"
                    |df_UP$Constituency_Name=="maharajganj"
                    |df_UP$Constituency_Name=="mubarakpur"
                    |df_UP$Constituency_Name=="siswa" & df_UP$before_delim==1]<- "agra"

df_UP$Constituency_Name[df_UP$Constituency_Name=="isauli"]<- "issauli"

df_UP$District_Name[df_UP$Constituency_Name=="amethi"
                    |df_UP$Constituency_Name=="chanda"
                    |df_UP$Constituency_Name=="issauli"
                    |df_UP$Constituency_Name=="mohanlalganj"
                    |df_UP$Constituency_Name=="rae bareli"
                    |df_UP$Constituency_Name=="salon"
                    |df_UP$Constituency_Name=="sataon"
                    |df_UP$Constituency_Name=="aligarh" & df_UP$before_delim==1]<- "aligarh"

df_UP$District_Name[df_UP$Constituency_Name=="agota"
                    |df_UP$Constituency_Name=="barauli"
                    |df_UP$Constituency_Name=="garhmukteshwar"
                    |df_UP$Constituency_Name=="gokul"
                    |df_UP$Constituency_Name=="hapur"
                    |df_UP$Constituency_Name=="meerut cantonment"
                    |df_UP$Constituency_Name=="morna"
                    |df_UP$Constituency_Name=="nakur"
                    |df_UP$Constituency_Name=="sadabad"
                    |df_UP$Constituency_Name=="sardhana"
                    |df_UP$Constituency_Name=="siwalkhas"
                    |df_UP$Constituency_Name=="allahabad north"
                    |df_UP$Constituency_Name=="allahabad south"
                    |df_UP$Constituency_Name=="allahabad west" & df_UP$before_delim==1]<- "allahabad"

df_UP$District_Name[df_UP$Constituency_Name=="fatehpur"
                    |df_UP$Constituency_Name=="haswa"
                    |df_UP$Constituency_Name=="manjhanpur"
                    |df_UP$Constituency_Name=="pratappur"
                    |df_UP$Constituency_Name=="sarsaul" & df_UP$before_delim==1]<- "ambedakar nagar"

df_UP$District_Name[df_UP$Constituency_Name=="auraiya"
                    |df_UP$Constituency_Name=="mau" & df_UP$before_delim==1]<- "auraiya"

df_UP$District_Name[df_UP$Constituency_Name=="aliganj"
                    |df_UP$Constituency_Name=="chhibramau"
                    |df_UP$Constituency_Name=="jhansi"
                    |df_UP$Constituency_Name=="kalyanpur"
                    |df_UP$Constituency_Name=="kamalganj"
                    |df_UP$Constituency_Name=="lakhana"
                    |df_UP$Constituency_Name=="mehroni"
                    |df_UP$Constituency_Name=="rath"
                    |df_UP$Constituency_Name=="soron" & df_UP$before_delim==1]<- "azamgarh"

df_UP$District_Name[df_UP$Constituency_Name=="aonla"
                    |df_UP$Constituency_Name=="baheri"
                    |df_UP$Constituency_Name=="rampur" & df_UP$before_delim==1]<- "baghpat"

df_UP$Constituency_Name[df_UP$Constituency_Name=="sadullah nagar"|df_UP$Constituency_Name=="sadulla nagar"]<- "sadullanagar"

df_UP$District_Name[df_UP$Constituency_Name=="dariyabad"
                    |df_UP$Constituency_Name=="gorakhpur"
                    |df_UP$Constituency_Name=="haidergarh"
                    |df_UP$Constituency_Name=="kunda"
                    |df_UP$Constituency_Name=="sadullanagar"
                    |df_UP$Constituency_Name=="unnao"
                    |df_UP$Constituency_Name=="bahraich" & df_UP$before_delim==1]<- "bahraich"

df_UP$District_Name[df_UP$Constituency_Name=="charkhari"
                    |df_UP$Constituency_Name=="etah"
                    |df_UP$Constituency_Name=="fatehpur sikri"
                    |df_UP$Constituency_Name=="jalesar"
                    |df_UP$Constituency_Name=="jasrana"
                    |df_UP$Constituency_Name=="kishni"
                    |df_UP$Constituency_Name=="lalitpur" & df_UP$before_delim==1]<- "ballia"

df_UP$District_Name[df_UP$Constituency_Name=="ayodhya"
                    |df_UP$Constituency_Name=="fazilnagar"
                    |df_UP$Constituency_Name=="hainsarbazar"
                    |df_UP$Constituency_Name=="nawabganj"
                    |df_UP$Constituency_Name=="utraula" & df_UP$before_delim==1]<- "balrampur"

df_UP$District_Name[df_UP$Constituency_Name=="bah"
                    |df_UP$Constituency_Name=="hathras"
                    |df_UP$Constituency_Name=="kharkhauda"
                    |df_UP$Constituency_Name=="banda" & df_UP$before_delim==1]<- "banda"

df_UP$District_Name[df_UP$Constituency_Name=="azamgarh"
                    |df_UP$Constituency_Name=="dildarnagar"
                    |df_UP$Constituency_Name=="gangapur"
                    |df_UP$Constituency_Name=="ghazipur"
                    |df_UP$Constituency_Name=="maniram"
                    |df_UP$Constituency_Name=="mohammadabad gohna"
                    |df_UP$Constituency_Name=="mundera bazar" & df_UP$before_delim==1]<- "barabanki"

df_UP$Constituency_Name[df_UP$Constituency_Name=="bhagwant nagar"]<- "bhagwantnagar"
df_UP$Constituency_Name[df_UP$Constituency_Name=="machhrihta"]<- "machhrehta"

df_UP$District_Name[df_UP$Constituency_Name=="bangarmau"
                    |df_UP$Constituency_Name=="bhagwantnagar"
                    |df_UP$Constituency_Name=="hadha"
                    |df_UP$Constituency_Name=="hardoi"
                    |df_UP$Constituency_Name=="hargaon"
                    |df_UP$Constituency_Name=="machhrehta"
                    |df_UP$Constituency_Name=="misrikh"
                    |df_UP$Constituency_Name=="powayan"
                    |df_UP$Constituency_Name=="rudrapur"
                    |df_UP$Constituency_Name=="srinagar"
                    |df_UP$Constituency_Name=="bareilly city"
                    |df_UP$Constituency_Name=="bareilly cantonment" & df_UP$before_delim==1]<- "bareilly"

df_UP$Constituency_Name[df_UP$Constituency_Name=="shyam duerwa"]<- "shyamdeurawa"

df_UP$District_Name[df_UP$Constituency_Name=="bhadohi"
                    |df_UP$Constituency_Name=="machhlishahr"
                    |df_UP$Constituency_Name=="mughalsarai"
                    |df_UP$Constituency_Name=="paniara"
                    |df_UP$Constituency_Name=="shyamdeurawa"
                    |df_UP$Constituency_Name=="siar"
                    |df_UP$Constituency_Name=="zamania"
                    |df_UP$Constituency_Name=="basti" & df_UP$before_delim==1]<- "basti"

df_UP$District_Name[df_UP$Constituency_Name=="bilsi"
                    |df_UP$Constituency_Name=="hasanpur"
                    |df_UP$Constituency_Name=="moradabad"
                    |df_UP$Constituency_Name=="moradabad rural"
                    |df_UP$Constituency_Name=="moradabad west"
                    |df_UP$Constituency_Name=="sahaswan"
                    |df_UP$Constituency_Name=="sambhal"
                    |df_UP$Constituency_Name=="usehat"
                    |df_UP$Constituency_Name=="bijnor" & df_UP$before_delim==1]<- "bijnor"

df_UP$Constituency_Name[df_UP$Constituency_Name=="sarojini nagar"]<- "sarojininagar"

df_UP$District_Name[df_UP$Constituency_Name=="lucknow cantonment"
                    |df_UP$Constituency_Name=="lucknow central"
                    |df_UP$Constituency_Name=="lucknow east"
                    |df_UP$Constituency_Name=="lucknow west"
                    |df_UP$Constituency_Name=="malihabad"
                    |df_UP$Constituency_Name=="mallawan"
                    |df_UP$Constituency_Name=="rampur khas"
                    |df_UP$Constituency_Name=="sareni"
                    |df_UP$Constituency_Name=="sarojininagar"
                    |df_UP$Constituency_Name=="shahabad" & df_UP$before_delim==1]<- "budaun"

df_UP$Constituency_Name[df_UP$Constituency_Name=="mahmoodabad"]<- "mahmudabad"

df_UP$District_Name[df_UP$Constituency_Name=="ahirori"
                    |df_UP$Constituency_Name=="bawan"
                    |df_UP$Constituency_Name=="behta"
                    |df_UP$Constituency_Name=="beniganj"
                    |df_UP$Constituency_Name=="hasanganj"
                    |df_UP$Constituency_Name=="mahmudabad"
                    |df_UP$Constituency_Name=="mahona"
                    |df_UP$Constituency_Name=="purwa"
                    |df_UP$Constituency_Name=="bulandshahr" & df_UP$before_delim==1]<- "bulandshahar"

df_UP$District_Name[df_UP$Constituency_Name=="baghpat"
                    |df_UP$Constituency_Name=="khatauli"
                    |df_UP$Constituency_Name=="muradnagar"
                    |df_UP$Constituency_Name=="sarsawa" & df_UP$before_delim==1]<- "chandauli"

df_UP$District_Name[df_UP$Constituency_Name=="dadri"
                    |df_UP$Constituency_Name=="khekra" & df_UP$before_delim==1]<- "chitrakoot"

df_UP$Constituency_Name[df_UP$Constituency_Name=="govindnagar"]<- "govind nagar"

df_UP$District_Name[df_UP$Constituency_Name=="ajitmal"
                    |df_UP$Constituency_Name=="govind nagar"
                    |df_UP$Constituency_Name=="kanpur cantonment"
                    |df_UP$Constituency_Name=="khaga"
                    |df_UP$Constituency_Name=="rari"
                    |df_UP$Constituency_Name=="soraon"
                    |df_UP$Constituency_Name=="deoria" & df_UP$before_delim==1]<- "deoria"

df_UP$District_Name[df_UP$Constituency_Name=="akbarpur"
                    |df_UP$Constituency_Name=="birapur"
                    |df_UP$Constituency_Name=="gainsari"
                    |df_UP$Constituency_Name=="kaiserganj"
                    |df_UP$Constituency_Name=="mahsi"
                    |df_UP$Constituency_Name=="sidhaur"
                    |df_UP$Constituency_Name=="sultanpur" & df_UP$before_delim==1]<- "etah"

df_UP$District_Name[df_UP$Constituency_Name=="bansdih"
                    |df_UP$Constituency_Name=="dudhi"
                    |df_UP$Constituency_Name=="kopachit"
                    |df_UP$Constituency_Name=="mariahu" & df_UP$before_delim==1]<- "etawah"

df_UP$Constituency_Name[df_UP$Constituency_Name=="barasathi"]<- "barsathi"
df_UP$Constituency_Name[df_UP$Constituency_Name=="bayalsi"]<- "beyalsi"

df_UP$District_Name[df_UP$Constituency_Name=="barsathi"
                    |df_UP$Constituency_Name=="chunar"
                    |df_UP$Constituency_Name=="rajgarh" & df_UP$before_delim==1]<- "faizabad"

df_UP$District_Name[df_UP$Constituency_Name=="charda"
                    |df_UP$Constituency_Name=="itwa"
                    |df_UP$Constituency_Name=="mankapur"
                    |df_UP$Constituency_Name=="ramnagar"
                    |df_UP$Constituency_Name=="farrukhabad" & df_UP$before_delim==1]<- "farrukhabad"

df_UP$District_Name[df_UP$Constituency_Name=="dayalbagh"
                    |df_UP$Constituency_Name=="kalpi"
                    |df_UP$Constituency_Name=="mauranipur"
                    |df_UP$Constituency_Name=="sasni"
                    |df_UP$Constituency_Name=="shikohabad" & df_UP$before_delim==1]<- "fatehpur"

df_UP$District_Name[df_UP$Constituency_Name=="dhuriapar"
                    |df_UP$Constituency_Name=="gonda"
                    |df_UP$Constituency_Name=="salempur" & df_UP$before_delim==1]<- "firozabad"

df_UP$District_Name[df_UP$Constituency_Name=="bawan"
                    |df_UP$Constituency_Name=="hasanganj"
                    |df_UP$Constituency_Name=="laharpur" & df_UP$before_delim==1]<- "gautam buddha nagar"

df_UP$Constituency_Name[df_UP$Constituency_Name=="dhaurehara"]<- "dhaurehra"

df_UP$District_Name[df_UP$Constituency_Name=="dadraul"
                    |df_UP$Constituency_Name=="dhaurehra"
                    |df_UP$Constituency_Name=="pilibhit"
                    |df_UP$Constituency_Name=="puranpur"
                    |df_UP$Constituency_Name=="shahjahanpur"
                    |df_UP$Constituency_Name=="tilhar" & df_UP$before_delim==1]<- "ghaziabad"

df_UP$District_Name[df_UP$Constituency_Name=="jewar"
                    |df_UP$Constituency_Name=="khurja"
                    |df_UP$Constituency_Name=="koil"
                    |df_UP$Constituency_Name=="sikandara rao"
                    |df_UP$Constituency_Name=="tundla" & df_UP$before_delim==1]<- "ghazipur"

df_UP$Constituency_Name[df_UP$Constituency_Name=="natthupur"]<- "nathupur"

df_UP$District_Name[df_UP$Constituency_Name=="barhaj"
                    |df_UP$Constituency_Name=="lalganj"
                    |df_UP$Constituency_Name=="nathupur"
                    |df_UP$Constituency_Name=="naugarh"
                    |df_UP$Constituency_Name=="padrauna"
                    |df_UP$Constituency_Name=="sahjanwa" & df_UP$before_delim==1]<- "gonda"

df_UP$Constituency_Name[df_UP$Constituency_Name=="mohammdabad"]<- "mohammadabad"

df_UP$District_Name[df_UP$Constituency_Name=="aryanagar"
                    |df_UP$Constituency_Name=="aurai"
                    |df_UP$Constituency_Name=="chail"
                    |df_UP$Constituency_Name=="etawah"
                    |df_UP$Constituency_Name=="handia"
                    |df_UP$Constituency_Name=="mehnagar"
                    |df_UP$Constituency_Name=="mohammadabad"
                    |df_UP$Constituency_Name=="phulpur"
                    |df_UP$Constituency_Name=="robertsganj" & df_UP$before_delim==1]<- "gorakhpur"

df_UP$District_Name[df_UP$Constituency_Name=="fatehabad"
                    |df_UP$Constituency_Name=="kasganj"
                    |df_UP$Constituency_Name=="hamirpur" & df_UP$before_delim==1]<- "hamirpur"

df_UP$District_Name[df_UP$Constituency_Name=="bhinga"
                    |df_UP$Constituency_Name=="captainganj"
                    |df_UP$Constituency_Name=="kasia"
                    |df_UP$Constituency_Name=="khalilabad"
                    |df_UP$Constituency_Name=="masauli"
                    |df_UP$Constituency_Name=="pipraich"
                    |df_UP$Constituency_Name=="rudauli"
                    |df_UP$Constituency_Name=="rudrapur"
                    |df_UP$Constituency_Name=="shohratgarh" & df_UP$before_delim==1]<- "hardoi"

df_UP$District_Name[df_UP$Constituency_Name=="colonelganj"
                    |df_UP$Constituency_Name=="jahangirganj"
                    |df_UP$Constituency_Name=="katra bazar"
                    |df_UP$Constituency_Name=="nanpara"
                    |df_UP$Constituency_Name=="sohawal" & df_UP$before_delim==1]<- "hathras"

df_UP$Constituency_Name[df_UP$Constituency_Name=="umarda"]<- "umardha"

df_UP$District_Name[df_UP$Constituency_Name=="baberu"
                    |df_UP$Constituency_Name=="garoutha"
                    |df_UP$Constituency_Name=="ghatampur"
                    |df_UP$Constituency_Name=="umardha" & df_UP$before_delim==1]<- "jalaun"

df_UP$District_Name[df_UP$Constituency_Name=="atrauli"
                    |df_UP$Constituency_Name=="chhata"
                    |df_UP$Constituency_Name=="etmadpur"
                    |df_UP$Constituency_Name=="gangiri"
                    |df_UP$Constituency_Name=="karhal"
                    |df_UP$Constituency_Name=="khair"
                    |df_UP$Constituency_Name=="kheragarh"
                    |df_UP$Constituency_Name=="mathura"
                    |df_UP$Constituency_Name=="maudaha" & df_UP$before_delim==1]<- "jaunpur"

df_UP$Constituency_Name[df_UP$Constituency_Name=="jhunsi"]<- "jhansi"

df_UP$District_Name[df_UP$Constituency_Name=="iglas"
                    |df_UP$Constituency_Name=="jhansi"
                    |df_UP$Constituency_Name=="kithore"
                    |df_UP$Constituency_Name=="mat"
                    |df_UP$Constituency_Name=="patiali" & df_UP$before_delim==1]<- "jhansi"

df_UP$District_Name[df_UP$Constituency_Name=="binawar"
                    |df_UP$Constituency_Name=="faridpur"
                    |df_UP$Constituency_Name=="paila" & df_UP$before_delim==1]<- "jyotiba phule nagar"

df_UP$District_Name[df_UP$Constituency_Name=="naurangia"
                    |df_UP$Constituency_Name=="seorahi"
                    |df_UP$Constituency_Name=="zahoorabad" & df_UP$before_delim==1]<- "kannauj"

df_UP$District_Name[df_UP$Constituency_Name=="bidhuna"
                    |df_UP$Constituency_Name=="generalganj"
                    |df_UP$Constituency_Name=="jaunpur"
                    |df_UP$Constituency_Name=="meja" & df_UP$before_delim==1]<- "kanpur dehat"

df_UP$District_Name[df_UP$Constituency_Name=="chaubepur"
                    |df_UP$Constituency_Name=="derapur"
                    |df_UP$Constituency_Name=="jahanabad"
                    |df_UP$Constituency_Name=="jakhania"
                    |df_UP$Constituency_Name=="jhusi"
                    |df_UP$Constituency_Name=="kaimganj"
                    |df_UP$Constituency_Name=="khutahan"
                    |df_UP$Constituency_Name=="rajpur" & df_UP$before_delim==1]<- "kanpur nagar"

df_UP$Constituency_Name[df_UP$Constituency_Name=="debai"]<- "dabai"

df_UP$District_Name[df_UP$Constituency_Name=="debai"
                    |df_UP$Constituency_Name=="ghaziabad"
                    |df_UP$Constituency_Name=="goverdhan" & df_UP$before_delim==1]<- "kaushambi"

df_UP$District_Name[df_UP$Constituency_Name=="bachhrawan"
                    |df_UP$Constituency_Name=="bihar"
                    |df_UP$Constituency_Name=="biswan"
                    |df_UP$Constituency_Name=="patti"
                    |df_UP$Constituency_Name=="pihani"
                    |df_UP$Constituency_Name=="tiloi" & df_UP$before_delim==1]<- "kheri"

df_UP$Constituency_Name[df_UP$Constituency_Name=="sikanderpur"]<- "sikandarpur"

df_UP$District_Name[df_UP$Constituency_Name=="garwara"
                    |df_UP$Constituency_Name=="lakshmipur"
                    |df_UP$Constituency_Name=="sadat"
                    |df_UP$Constituency_Name=="saraimir"
                    |df_UP$Constituency_Name=="shahganj"
                    |df_UP$Constituency_Name=="sikandarpur" & df_UP$before_delim==1]<- "kushinagar"

df_UP$District_Name[df_UP$Constituency_Name=="harora"
                    |df_UP$Constituency_Name=="thana bhawan" & df_UP$before_delim==1]<- "lalitpur"

df_UP$District_Name[df_UP$Constituency_Name=="chiraigaon"
                    |df_UP$Constituency_Name=="lucknow cantonment"
                    |df_UP$Constituency_Name=="lucknow central"
                    |df_UP$Constituency_Name=="lucknow east"
                    |df_UP$Constituency_Name=="lucknow west"
                    |df_UP$Constituency_Name=="dhanapur"
                    |df_UP$Constituency_Name=="doaba"
                    |df_UP$Constituency_Name=="ghosi"
                    |df_UP$Constituency_Name=="gyanpur"
                    |df_UP$Constituency_Name=="kolasla"
                    |df_UP$Constituency_Name=="atraulia" & df_UP$before_delim==1]<- "lucknow"

df_UP$District_Name[df_UP$Constituency_Name=="mahoba"
                    |df_UP$Constituency_Name=="siana"
                    |df_UP$Constituency_Name=="sikandrabad" & df_UP$before_delim==1]<- "mahoba"

df_UP$District_Name[df_UP$Constituency_Name=="bansi"
                    |df_UP$Constituency_Name=="kauriram"
                    |df_UP$Constituency_Name=="nagar east"
                    |df_UP$Constituency_Name=="nizamabad"
                    |df_UP$Constituency_Name=="pharenda" & df_UP$before_delim==1]<- "maharajganj"

df_UP$District_Name[df_UP$Constituency_Name=="hata"
                    |df_UP$Constituency_Name=="khesraha"
                    |df_UP$Constituency_Name=="sagri" & df_UP$before_delim==1]<- "mainpuri"

df_UP$Constituency_Name[df_UP$Constituency_Name=="katehri"]<- "katehari"

df_UP$District_Name[df_UP$Constituency_Name=="balrampur"
                    |df_UP$Constituency_Name=="colonelganj"
                    |df_UP$Constituency_Name=="gauriganj"
                    |df_UP$Constituency_Name=="ikauna"
                    |df_UP$Constituency_Name=="katehari"
                    |df_UP$Constituency_Name=="mathura" & df_UP$before_delim==1]<- "mathura"

df_UP$District_Name[df_UP$Constituency_Name=="babina"
                    |df_UP$Constituency_Name=="bhongaon"
                    |df_UP$Constituency_Name=="chhibramau"
                    |df_UP$Constituency_Name=="karwi"
                    |df_UP$Constituency_Name=="kishni"
                    |df_UP$Constituency_Name=="manikpur" & df_UP$before_delim==1]<- "mau"

df_UP$Constituency_Name[df_UP$Constituency_Name=="gunnour"]<- "gunnaur"

df_UP$District_Name[df_UP$Constituency_Name=="bisauli"
                    |df_UP$Constituency_Name=="dataganj"
                    |df_UP$Constituency_Name=="gunnaur"
                    |df_UP$Constituency_Name=="jalalabad"
                    |df_UP$Constituency_Name=="shahabad"
                    |df_UP$Constituency_Name=="sunha"
                    |df_UP$Constituency_Name=="meerut"
                    |df_UP$Constituency_Name=="meerut cantonment" & df_UP$before_delim==1]<- "meerut"

df_UP$District_Name[df_UP$Constituency_Name=="baghra"
                    |df_UP$Constituency_Name=="charthawal"
                    |df_UP$Constituency_Name=="deoband"
                    |df_UP$Constituency_Name=="kairana"
                    |df_UP$Constituency_Name=="mirzapur" & df_UP$before_delim==1]<- "mirzapur"

df_UP$District_Name[df_UP$Constituency_Name=="bilaspur"
                    |df_UP$Constituency_Name=="binawar"
                    |df_UP$Constituency_Name=="haiderabad"
                    |df_UP$Constituency_Name=="kawar"
                    |df_UP$Constituency_Name=="lakhimpur"
                    |df_UP$Constituency_Name=="nawabganj"
                    |df_UP$Constituency_Name=="sandila"
                    |df_UP$Constituency_Name=="sidhauli"
                    |df_UP$Constituency_Name=="sitapur"
                    |df_UP$Constituency_Name=="moradabad rural"
                    |df_UP$Constituency_Name=="moradabad west" & df_UP$before_delim==1]<- "moradabad"

df_UP$Constituency_Name[df_UP$Constituency_Name=="kunderki"]<- "kundarki"
df_UP$Constituency_Name[df_UP$Constituency_Name=="suartanda"]<- "suar tanda"

df_UP$District_Name[df_UP$Constituency_Name=="amroha"
                    |df_UP$Constituency_Name=="bahjoi"
                    |df_UP$Constituency_Name=="chandausi"
                    |df_UP$Constituency_Name=="gangeshwari"
                    |df_UP$Constituency_Name=="kundarki"
                    |df_UP$Constituency_Name=="suar tanda"
                    |df_UP$Constituency_Name=="thakurdwara"
                    |df_UP$Constituency_Name=="muzaffarnagar" & df_UP$before_delim==1]<- "muzaffarnagar"

df_UP$District_Name[df_UP$Constituency_Name=="mohammadi"
                    |df_UP$Constituency_Name=="nighasan"
                    |df_UP$Constituency_Name=="nigohi"
                    |df_UP$Constituency_Name=="safipur"
                    |df_UP$Constituency_Name=="pilibhit" & df_UP$before_delim==1]<- "pilibhit"

df_UP$District_Name[df_UP$Constituency_Name=="firozabad"
                    |df_UP$Constituency_Name=="madhogarh"
                    |df_UP$Constituency_Name=="mainpuri"
                    |df_UP$Constituency_Name=="nidhauli kalan"
                    |df_UP$Constituency_Name=="orai"
                    |df_UP$Constituency_Name=="sakit" & df_UP$before_delim==1]<- "pratapgarh"

df_UP$District_Name[df_UP$Constituency_Name=="kannauj"
                    |df_UP$Constituency_Name=="sarvankhera"
                    |df_UP$Constituency_Name=="sirathu"
                    |df_UP$Constituency_Name=="tindwari" & df_UP$before_delim==1]<- "rae bareli"

df_UP$District_Name[df_UP$Constituency_Name=="barkhera"
                    |df_UP$Constituency_Name=="bhojipura"
                    |df_UP$Constituency_Name=="bisalpur"
                    |df_UP$Constituency_Name=="budaun" & df_UP$before_delim==1]<- "rampur"

df_UP$District_Name[df_UP$Constituency_Name=="afzalgarh"
                    |df_UP$Constituency_Name=="dhampur"
                    |df_UP$Constituency_Name=="kanth"
                    |df_UP$Constituency_Name=="nagina"
                    |df_UP$Constituency_Name=="nazibabad"
                    |df_UP$Constituency_Name=="seohara"
                    |df_UP$Constituency_Name=="saharanpur" & df_UP$before_delim==1]<- "saharanpur"

df_UP$Constituency_Name[df_UP$Constituency_Name=="chanvey"]<- "chhanvey"

df_UP$District_Name[df_UP$Constituency_Name=="chail"
                    |df_UP$Constituency_Name=="chhanvey"
                    |df_UP$Constituency_Name=="kerakat"
                    |df_UP$Constituency_Name=="saidpur" & df_UP$before_delim==1]<- "sant kabir nagar"

df_UP$District_Name[df_UP$Constituency_Name=="barnawa"
                    |df_UP$Constituency_Name=="modinagar" & df_UP$before_delim==1]<- "sant ravidas nagar"

df_UP$District_Name[df_UP$Constituency_Name=="bilgram"
                    |df_UP$Constituency_Name=="dalmau"
                    |df_UP$Constituency_Name=="gadwara"
                    |df_UP$Constituency_Name=="jaisinghpur"
                    |df_UP$Constituency_Name=="jalalpur" & df_UP$before_delim==1]<- "shahjahanpur"

df_UP$District_Name[df_UP$Constituency_Name=="jagdishpur"
                    |df_UP$Constituency_Name=="tulsipur" & df_UP$before_delim==1]<- "shrawasti"

df_UP$Constituency_Name[df_UP$Constituency_Name=="shyamdeurawa"|df_UP$Constituency_Name=="shyamdeurwa"]<- "shyam duerwa"

df_UP$District_Name[df_UP$Constituency_Name=="dixir"
                    |df_UP$Constituency_Name=="domariaganj"
                    |df_UP$Constituency_Name=="mehndawal"
                    |df_UP$Constituency_Name=="paniara"
                    |df_UP$Constituency_Name=="shyam duerwa" & df_UP$before_delim==1]<- "siddharthnagar"

df_UP$District_Name[df_UP$Constituency_Name=="bikapur"
                    |df_UP$Constituency_Name=="fakharpur"
                    |df_UP$Constituency_Name=="harraiya"
                    |df_UP$Constituency_Name=="kadipur"
                    |df_UP$Constituency_Name=="milkipur"
                    |df_UP$Constituency_Name=="mujehna"
                    |df_UP$Constituency_Name=="ramnagar"
                    |df_UP$Constituency_Name=="tanda" & df_UP$before_delim==1]<- "sitapur"

df_UP$District_Name[df_UP$Constituency_Name=="deoband"
                    |df_UP$Constituency_Name=="muzaffarabad" & df_UP$before_delim==1]<- "sonbhadra"

df_UP$District_Name[df_UP$Constituency_Name=="bharthana"
                    |df_UP$Constituency_Name=="bilhaur"
                    |df_UP$Constituency_Name=="jaswantnagar"
                    |df_UP$Constituency_Name=="kishunpur"
                    |df_UP$Constituency_Name=="mohammadabad"
                    |df_UP$Constituency_Name=="naraini"
                    |df_UP$Constituency_Name=="bara" & df_UP$before_delim==1]<- "sultanpur"

df_UP$Constituency_Name[df_UP$Constituency_Name=="chakiya"]<- "chakia"

df_UP$District_Name[df_UP$Constituency_Name=="bhognipur"
                    |df_UP$Constituency_Name=="bindki"
                    |df_UP$Constituency_Name=="chakia"
                    |df_UP$Constituency_Name=="chilkahar"
                    |df_UP$Constituency_Name=="karchana"
                    |df_UP$Constituency_Name=="rasra" & df_UP$before_delim==1]<- "unnao"

df_UP$District_Name[df_UP$Constituency_Name=="chhaprauli"
                    |df_UP$Constituency_Name=="hastinapur"
                    |df_UP$Constituency_Name=="jansath"
                    |df_UP$Constituency_Name=="kandhla"
                    |df_UP$Constituency_Name=="shikarpur"
                    |df_UP$Constituency_Name=="varanasi cantonment"
                    |df_UP$Constituency_Name=="varanasi north"
                    |df_UP$Constituency_Name=="varanasi south" & df_UP$before_delim==1]<- "varanasi"

df_UP<- df_UP%>%
  subset(District_Name!="")

####################### Uttarakhand ################################

df_UK<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_Uttarakhand_2021-12-29.csv")

df_UK<- df_UK%>%
  subset(Year==2002|
           Year==2007|
           Year==2012)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))

df_UK$District_Name[df_UK$Constituency_Name=="almora"
                    |df_UK$Constituency_Name=="bhikiasain"
                    |df_UK$Constituency_Name=="dwarahat"
                    |df_UK$Constituency_Name=="jageshwar"
                    |df_UK$Constituency_Name=="mukteshwar"
                    |df_UK$Constituency_Name=="ranikhet"
                    |df_UK$Constituency_Name=="salt"
                    |df_UK$Constituency_Name=="someshwar" & df_UK$before_delim==1]<- "almora"

df_UK$Constituency_Name[df_UK$Constituency_Name=="kapkote"]<- "kapkot"

df_UK$District_Name[df_UK$Constituency_Name=="bageshwar"
                    |df_UK$Constituency_Name=="gangolihat"
                    |df_UK$Constituency_Name=="kanda"
                    |df_UK$Constituency_Name=="kapkot" & df_UK$before_delim==1]<- "bageshwar"


df_UK$Constituency_Name[df_UK$Constituency_Name=="karnprayag"]<- "karanprayag"

df_UK$District_Name[df_UK$Constituency_Name=="badrinath"
                    |df_UK$Constituency_Name=="karanprayag"
                    |df_UK$Constituency_Name=="nandprayag"
                    |df_UK$Constituency_Name=="pindar" & df_UK$before_delim==1]<- "chamoli"

df_UK$District_Name[df_UK$Constituency_Name=="champawat"
                    |df_UK$Constituency_Name=="lohaghat" & df_UK$before_delim==1]<- "champawat"

df_UK$District_Name[df_UK$Constituency_Name=="chakrata"
                    |df_UK$Constituency_Name=="dehradun"
                    |df_UK$Constituency_Name=="doiwala"
                    |df_UK$Constituency_Name=="dhanolti"
                    |df_UK$Constituency_Name=="laxman chowk"
                    |df_UK$Constituency_Name=="mussoorie"
                    |df_UK$Constituency_Name=="rajpur"
                    |df_UK$Constituency_Name=="rishikesh"
                    |df_UK$Constituency_Name=="sahaspur"
                    |df_UK$Constituency_Name=="vikasnagar" & df_UK$before_delim==1]<- "dehradun"

df_UK$Constituency_Name[df_UK$Constituency_Name=="dhumakote"]<- "dhumakot"
df_UK$Constituency_Name[df_UK$Constituency_Name=="yamkeshwar"]<- "yamakeshwar"

df_UK$District_Name[df_UK$Constituency_Name=="bironkhal"
                    |df_UK$Constituency_Name=="kotdwar"
                    |df_UK$Constituency_Name=="dhumakot"
                    |df_UK$Constituency_Name=="lansdowne"
                    |df_UK$Constituency_Name=="pauri"
                    |df_UK$Constituency_Name=="srinagar"
                    |df_UK$Constituency_Name=="thalisain"
                    |df_UK$Constituency_Name=="yamakeshwar" & df_UK$before_delim==1]<- "garhwal"

df_UK$Constituency_Name[df_UK$Constituency_Name=="manglore"]<- "manglor"

df_UK$District_Name[df_UK$Constituency_Name=="bahadrabad"
                    |df_UK$Constituency_Name=="bhagwanpur"
                    |df_UK$Constituency_Name=="hardwar"
                    |df_UK$Constituency_Name=="iqbalpur"
                    |df_UK$Constituency_Name=="laksar"
                    |df_UK$Constituency_Name=="laldhang"
                    |df_UK$Constituency_Name=="landhaura"
                    |df_UK$Constituency_Name=="manglor"
                    |df_UK$Constituency_Name=="roorkee" & df_UK$before_delim==1]<- "hardwar"

df_UK$District_Name[df_UK$Constituency_Name=="dhari"
                    |df_UK$Constituency_Name=="haldwani"
                    |df_UK$Constituency_Name=="mukteshwar"
                    |df_UK$Constituency_Name=="nainital"
                    |df_UK$Constituency_Name=="ramnagar" & df_UK$before_delim==1]<- "nainital"

df_UK$District_Name[df_UK$Constituency_Name=="dharchula"
                    |df_UK$Constituency_Name=="didihat"
                    |df_UK$Constituency_Name=="gangolihat"
                    |df_UK$Constituency_Name=="kanalichhina"
                    |df_UK$Constituency_Name=="pithoragarh" & df_UK$before_delim==1]<- "pithoragarh"

df_UK$District_Name[df_UK$Constituency_Name=="kedarnath"
                    |df_UK$Constituency_Name=="rudraprayag" & df_UK$before_delim==1]<- "rudraprayag"

df_UK$Constituency_Name[df_UK$Constituency_Name=="ghanshali"]<- "ghansali"

df_UK$District_Name[df_UK$Constituency_Name=="deoprayag"
                    |df_UK$Constituency_Name=="dhanolti"
                    |df_UK$Constituency_Name=="ghansali"
                    |df_UK$Constituency_Name=="narendranagar"
                    |df_UK$Constituency_Name=="pratapnagar"
                    |df_UK$Constituency_Name=="tehri" & df_UK$before_delim==1]<- "tehri garhwal"

df_UK$District_Name[df_UK$Constituency_Name=="bajpur"
                    |df_UK$Constituency_Name=="jaspur"
                    |df_UK$Constituency_Name=="kashipur"
                    |df_UK$Constituency_Name=="khatima"
                    |df_UK$Constituency_Name=="pantnagar-gadarpur"
                    |df_UK$Constituency_Name=="rudrapur-kichha"
                    |df_UK$Constituency_Name=="sitarganj" & df_UK$before_delim==1]<- "udham singh nagar"

df_UK$District_Name[df_UK$Constituency_Name=="gangotri"
                    |df_UK$Constituency_Name=="purola"
                    |df_UK$Constituency_Name=="yamunotri" & df_UK$before_delim==1]<- "uttarkashi"

df_UK<- df_UK%>%
  subset(District_Name!="")

############################# West Bengal #########################

df_WB<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Election data\\TCPD_AE_West_Bengal_2021-11-23.csv")

df_WB<- df_WB%>%
  subset(Year==1991|
           Year==1996|
           Year==2001|
           Year==2006|
           Year==2011|
           Year==2016)%>%
  mutate(District_Name=tolower(District_Name),
         Constituency_Name=tolower(Constituency_Name),
         State_Name=tolower(State_Name),
         before_delim=ifelse(Year<2008,1,0))


df_WB$District_Name[df_WB$Constituency_Name=="bankura"
                    |df_WB$Constituency_Name=="barjora"
                    |df_WB$Constituency_Name=="chhatna"
                    |df_WB$Constituency_Name=="gangajalghati"
                    |df_WB$Constituency_Name=="indas"
                    |df_WB$Constituency_Name=="indpur"
                    |df_WB$Constituency_Name=="kotulpur"
                    |df_WB$Constituency_Name=="onda"
                    |df_WB$Constituency_Name=="raipur"
                    |df_WB$Constituency_Name=="ranibandh"
                    |df_WB$Constituency_Name=="sonamukhi"
                    |df_WB$Constituency_Name=="taldangra"
                    |df_WB$Constituency_Name=="vishnupur" & df_WB$before_delim==1]<- "bankura"

df_WB$District_Name[df_WB$Constituency_Name=="ausgram"
                    |df_WB$Constituency_Name=="barabani"
                    |df_WB$Constituency_Name=="bhatar"
                    |df_WB$Constituency_Name=="burdwan north"
                    |df_WB$Constituency_Name=="burdwan south"
                    |df_WB$Constituency_Name=="durgapur-i"
                    |df_WB$Constituency_Name=="durgapur-ii"
                    |df_WB$Constituency_Name=="galsi"
                    |df_WB$Constituency_Name=="jamalpur"
                    |df_WB$Constituency_Name=="jamuria"
                    |df_WB$Constituency_Name=="kalna"
                    |df_WB$Constituency_Name=="kanksa"
                    |df_WB$Constituency_Name=="katwa"
                    |df_WB$Constituency_Name=="ketugram"
                    |df_WB$Constituency_Name=="khandaghosh"
                    |df_WB$Constituency_Name=="mangalkot"
                    |df_WB$Constituency_Name=="manteswar"
                    |df_WB$Constituency_Name=="memari"
                    |df_WB$Constituency_Name=="nadanghat"
                    |df_WB$Constituency_Name=="purbasthali"
                    |df_WB$Constituency_Name=="raina"
                    |df_WB$Constituency_Name=="ukhra"
                    |df_WB$Constituency_Name=="raniganj" & df_WB$before_delim==1]<- "barddhaman"

df_WB$District_Name[df_WB$Constituency_Name=="bolpur"
                    |df_WB$Constituency_Name=="dubrajpur"
                    |df_WB$Constituency_Name=="hansan"
                    |df_WB$Constituency_Name=="labhpur"
                    |df_WB$Constituency_Name=="mahammad bazar"
                    |df_WB$Constituency_Name=="mayureswar"
                    |df_WB$Constituency_Name=="murarai"
                    |df_WB$Constituency_Name=="nalhati"
                    |df_WB$Constituency_Name=="nanur"
                    |df_WB$Constituency_Name=="rajnagar"
                    |df_WB$Constituency_Name=="rampurhat"
                    |df_WB$Constituency_Name=="suri" & df_WB$before_delim==1]<- "birbhum"

df_WB$District_Name[df_WB$Constituency_Name=="balurghat"
                    |df_WB$Constituency_Name=="gangarampur"
                    |df_WB$Constituency_Name=="itahar"
                    |df_WB$Constituency_Name=="kumarganj"
                    |df_WB$Constituency_Name=="kushmandi"
                    |df_WB$Constituency_Name=="tapan" & df_WB$before_delim==1]<- "dakshin dinajpur"

df_WB$District_Name[df_WB$Constituency_Name=="darjeeling"
                    |df_WB$Constituency_Name=="kalimpong"
                    |df_WB$Constituency_Name=="kurseong"
                    |df_WB$Constituency_Name=="phansidewa"
                    |df_WB$Constituency_Name=="siliguri" & df_WB$before_delim==1]<- "darjiling"

df_WB$District_Name[df_WB$Constituency_Name=="amta"
                    |df_WB$Constituency_Name=="bagnan"
                    |df_WB$Constituency_Name=="domjur"
                    |df_WB$Constituency_Name=="jagatballavpur"
                    |df_WB$Constituency_Name=="kalyanpur"
                    |df_WB$Constituency_Name=="panchla"
                    |df_WB$Constituency_Name=="sankrail"
                    |df_WB$Constituency_Name=="shyampur"
                    |df_WB$Constituency_Name=="udaynarayanpur"
                    |df_WB$Constituency_Name=="uluberia north"
                    |df_WB$Constituency_Name=="uluberia south"
                    |df_WB$Constituency_Name=="howrah central"
                    |df_WB$Constituency_Name=="howrah north"
                    |df_WB$Constituency_Name=="howrah south" & df_WB$before_delim==1]<- "haora"

df_WB$District_Name[df_WB$Constituency_Name=="arambagh"
                    |df_WB$Constituency_Name=="balagarh"
                    |df_WB$Constituency_Name=="bansberia"
                    |df_WB$Constituency_Name=="champdani"
                    |df_WB$Constituency_Name=="chinsurah"
                    |df_WB$Constituency_Name=="dhaniakhali"
                    |df_WB$Constituency_Name=="goghat"
                    |df_WB$Constituency_Name=="haripal"
                    |df_WB$Constituency_Name=="jangipara"
                    |df_WB$Constituency_Name=="khanakul"
                    |df_WB$Constituency_Name=="pandua"
                    |df_WB$Constituency_Name=="polba"
                    |df_WB$Constituency_Name=="pursurah"
                    |df_WB$Constituency_Name=="singur"
                    |df_WB$Constituency_Name=="tarakeswar"
                    |df_WB$Constituency_Name=="uttarpara"
                    |df_WB$Constituency_Name=="chandernagore"
                    |df_WB$Constituency_Name=="chanditala" & df_WB$before_delim==1]<- "hugli"

df_WB$District_Name[df_WB$Constituency_Name=="alipurduars"
                    |df_WB$Constituency_Name=="dhupguri"
                    |df_WB$Constituency_Name=="falakata"
                    |df_WB$Constituency_Name=="jalpaiguri"
                    |df_WB$Constituency_Name=="kalchini"
                    |df_WB$Constituency_Name=="kranti"
                    |df_WB$Constituency_Name=="kumargram"
                    |df_WB$Constituency_Name=="madarihat"
                    |df_WB$Constituency_Name=="mainaguri"
                    |df_WB$Constituency_Name=="mal"
                    |df_WB$Constituency_Name=="nagrakata"
                    |df_WB$Constituency_Name=="rajganj" & df_WB$before_delim==1]<- "jalpaiguri"

df_WB$District_Name[df_WB$Constituency_Name=="cooch behar north"
                    |df_WB$Constituency_Name=="cooch behar west"
                    |df_WB$Constituency_Name=="dinhata"
                    |df_WB$Constituency_Name=="mathabhanga"
                    |df_WB$Constituency_Name=="mekliganj"
                    |df_WB$Constituency_Name=="natabari"
                    |df_WB$Constituency_Name=="sitai"
                    |df_WB$Constituency_Name=="sitalkuchi"
                    |df_WB$Constituency_Name=="tufanganj" & df_WB$before_delim==1]<- "koch bihar"

df_WB$District_Name[df_WB$Constituency_Name=="araidanga"
                    |df_WB$Constituency_Name=="englishbazar"
                    |df_WB$Constituency_Name=="gajol"
                    |df_WB$Constituency_Name=="habibpur"
                    |df_WB$Constituency_Name=="harishchandrapur"
                    |df_WB$Constituency_Name=="kaliachak"
                    |df_WB$Constituency_Name=="kharba"
                    |df_WB$Constituency_Name=="malda"
                    |df_WB$Constituency_Name=="manikchak"
                    |df_WB$Constituency_Name=="ratua"
                    |df_WB$Constituency_Name=="suzapur" & df_WB$before_delim==1]<- "maldah"

df_WB$District_Name[df_WB$Constituency_Name=="bhagabanpur"
                    |df_WB$Constituency_Name=="binpur"
                    |df_WB$Constituency_Name=="chandrakona"
                    |df_WB$Constituency_Name=="contai north"
                    |df_WB$Constituency_Name=="contai south"
                    |df_WB$Constituency_Name=="dantan"
                    |df_WB$Constituency_Name=="daspur"
                    |df_WB$Constituency_Name=="debra"
                    |df_WB$Constituency_Name=="egra"
                    |df_WB$Constituency_Name=="garhbeta east"
                    |df_WB$Constituency_Name=="garhbeta west"
                    |df_WB$Constituency_Name=="ghatal"
                    |df_WB$Constituency_Name=="gopiballavpur"
                    |df_WB$Constituency_Name=="jhargram"
                    |df_WB$Constituency_Name=="keshiari"
                    |df_WB$Constituency_Name=="keshpur"
                    |df_WB$Constituency_Name=="khajuri"
                    |df_WB$Constituency_Name=="kharagpur rural"
                    |df_WB$Constituency_Name=="kharagpur town"
                    |df_WB$Constituency_Name=="mahishadal"
                    |df_WB$Constituency_Name=="midnapore"
                    |df_WB$Constituency_Name=="moyna"
                    |df_WB$Constituency_Name=="mugberia"
                    |df_WB$Constituency_Name=="nandanpur"
                    |df_WB$Constituency_Name=="nandigram"
                    |df_WB$Constituency_Name=="narayangarh"
                    |df_WB$Constituency_Name=="narghat"
                    |df_WB$Constituency_Name=="nayagram"
                    |df_WB$Constituency_Name=="panskura east"
                    |df_WB$Constituency_Name=="panskura west"
                    |df_WB$Constituency_Name=="pataspur"
                    |df_WB$Constituency_Name=="pingla"
                    |df_WB$Constituency_Name=="ramnagar"
                    |df_WB$Constituency_Name=="sabang"
                    |df_WB$Constituency_Name=="salbani"
                    |df_WB$Constituency_Name=="sutahata"
                    |df_WB$Constituency_Name=="tamluk" & df_WB$before_delim==1]<- "medinipur"

df_WB$District_Name[df_WB$Constituency_Name=="aurangabad"
                    |df_WB$Constituency_Name=="barwan"
                    |df_WB$Constituency_Name=="beldanga"
                    |df_WB$Constituency_Name=="berhampore"
                    |df_WB$Constituency_Name=="bhagabangola"
                    |df_WB$Constituency_Name=="bharatpur"
                    |df_WB$Constituency_Name=="domkal"
                    |df_WB$Constituency_Name=="farakka"
                    |df_WB$Constituency_Name=="hariharpara"
                    |df_WB$Constituency_Name=="jalangi"
                    |df_WB$Constituency_Name=="jangipur"
                    |df_WB$Constituency_Name=="kandi"
                    |df_WB$Constituency_Name=="khargram"
                    |df_WB$Constituency_Name=="lalgola"
                    |df_WB$Constituency_Name=="murshidabad"
                    |df_WB$Constituency_Name=="nabagram"
                    |df_WB$Constituency_Name=="naoda"
                    |df_WB$Constituency_Name=="sagardighi"
                    |df_WB$Constituency_Name=="suti" & df_WB$before_delim==1]<- "murshidabad"

df_WB$District_Name[df_WB$Constituency_Name=="chakdaha"
                    |df_WB$Constituency_Name=="chapra"
                    |df_WB$Constituency_Name=="hanskhali"
                    |df_WB$Constituency_Name=="haringhata"
                    |df_WB$Constituency_Name=="kaliaganj"
                    |df_WB$Constituency_Name=="karimpur"
                    |df_WB$Constituency_Name=="krishnaganj"
                    |df_WB$Constituency_Name=="krishnagar east"
                    |df_WB$Constituency_Name=="krishnagar west"
                    |df_WB$Constituency_Name=="nabadwip"
                    |df_WB$Constituency_Name=="nakashipara"
                    |df_WB$Constituency_Name=="palashipara "
                    |df_WB$Constituency_Name=="ranaghat east"
                    |df_WB$Constituency_Name=="ranaghat west"
                    |df_WB$Constituency_Name=="santipur"
                    |df_WB$Constituency_Name=="bijpur"
                    |df_WB$Constituency_Name=="palashipara" & df_WB$before_delim==1]<- "nadia"

df_WB$District_Name[df_WB$Constituency_Name=="amdanga"
                    |df_WB$Constituency_Name=="ashokenagar"
                    |df_WB$Constituency_Name=="baduria"
                    |df_WB$Constituency_Name=="bagdaha"
                    |df_WB$Constituency_Name=="barasat"
                    |df_WB$Constituency_Name=="bhangar"
                    |df_WB$Constituency_Name=="bongaon"
                    |df_WB$Constituency_Name=="deganga"
                    |df_WB$Constituency_Name=="dum dum"
                    |df_WB$Constituency_Name=="gaighata"
                    |df_WB$Constituency_Name=="habra"
                    |df_WB$Constituency_Name=="haroa"
                    |df_WB$Constituency_Name=="hasnabad"
                    |df_WB$Constituency_Name=="hingalganj"
                    |df_WB$Constituency_Name=="jagatdal"
                    |df_WB$Constituency_Name=="khardah"
                    |df_WB$Constituency_Name=="naihati"
                    |df_WB$Constituency_Name=="noapara"
                    |df_WB$Constituency_Name=="panihati"
                    |df_WB$Constituency_Name=="rajarhat"
                    |df_WB$Constituency_Name=="sandeshkhali"
                    |df_WB$Constituency_Name=="swarupnagar"
                    |df_WB$Constituency_Name=="titagarh"
                    |df_WB$Constituency_Name=="basirhat" & df_WB$before_delim==1]<- "north twentyfour parganas"

df_WB$District_Name[df_WB$Constituency_Name=="arsa"
                    |df_WB$Constituency_Name=="balrampur"
                    |df_WB$Constituency_Name=="banduan"
                    |df_WB$Constituency_Name=="hura"
                    |df_WB$Constituency_Name=="jaipur"
                    |df_WB$Constituency_Name=="jhalda"
                    |df_WB$Constituency_Name=="kashipur"
                    |df_WB$Constituency_Name=="manbazar"
                    |df_WB$Constituency_Name=="para"
                    |df_WB$Constituency_Name=="purulia"
                    |df_WB$Constituency_Name=="raghunathpur" & df_WB$before_delim==1]<- "puruliya"

df_WB$District_Name[df_WB$Constituency_Name=="baruipur"
                    |df_WB$Constituency_Name=="basanti"
                    |df_WB$Constituency_Name=="behala east"
                    |df_WB$Constituency_Name=="behala west"
                    |df_WB$Constituency_Name=="bhangar"
                    |df_WB$Constituency_Name=="bishnupur east"
                    |df_WB$Constituency_Name=="bishnupur west"
                    |df_WB$Constituency_Name=="budge budge"
                    |df_WB$Constituency_Name=="canning east"
                    |df_WB$Constituency_Name=="canning west"
                    |df_WB$Constituency_Name=="diamond harbour"
                    |df_WB$Constituency_Name=="falta"
                    |df_WB$Constituency_Name=="gosaba"
                    |df_WB$Constituency_Name=="jadavpur"
                    |df_WB$Constituency_Name=="joynagar"
                    |df_WB$Constituency_Name=="kakdwip"
                    |df_WB$Constituency_Name=="kulpi"
                    |df_WB$Constituency_Name=="kultali"
                    |df_WB$Constituency_Name=="magrahat east"
                    |df_WB$Constituency_Name=="magrahat west"
                    |df_WB$Constituency_Name=="maheshtala"
                    |df_WB$Constituency_Name=="mandirbazar"
                    |df_WB$Constituency_Name=="mathurapur"
                    |df_WB$Constituency_Name=="patharpratima"
                    |df_WB$Constituency_Name=="sagar"
                    |df_WB$Constituency_Name=="satgachia"
                    |df_WB$Constituency_Name=="sonarpur" & df_WB$before_delim==1]<- "south twentyfour parganas"

df_WB$Constituency_Name[df_WB$Constituency_Name=="rayganj"]<- "raiganj"

df_WB$District_Name[df_WB$Constituency_Name=="chopra"
                    |df_WB$Constituency_Name=="goalpokhar"
                    |df_WB$Constituency_Name=="islampur"
                    |df_WB$Constituency_Name=="itahar"
                    |df_WB$Constituency_Name=="kaliganj"
                    |df_WB$Constituency_Name=="karandighi"
                    |df_WB$Constituency_Name=="kharba"
                    |df_WB$Constituency_Name=="kushmandi"
                    |df_WB$Constituency_Name=="raiganj" & df_WB$before_delim==1]<- "uttar dinajpur"


df_WB<- df_WB%>%
  subset(District_Name!="")

############################################### Binding all the states together ######################################################

df_elections<- rbind(df_AP,
                     df_AR,
                     df_AS,
                     df_BR,
                     df_CH,
                     df_DL,
                     df_GJ,
                     df_GO,
                     df_HP,
                     df_HR,
                     df_JK,
                     df_JH,
                     df_KL,
                     df_KR,
                     df_MG,
                     df_MN,
                     df_MP,
                     df_MR,
                     df_MZ,
                     df_NG,
                     df_OR,
                     df_PU,
                     df_RJ,
                     df_TN,
                     df_TR,
                     df_UP,
                     df_UK,
                     df_WB)


rm(df_AP,
   df_AR,
   df_AS,
   df_BR,
   df_CH,
   df_DL,
   df_GJ,
   df_GO,
   df_HP,
   df_HR,
   df_JK,
   df_JH,
   df_KL,
   df_KR,
   df_MG,
   df_MN,
   df_MP,
   df_MR,
   df_MZ,
   df_NG,
   df_OR,
   df_PU,
   df_RJ,
   df_TN,
   df_TR,
   df_UP,
   df_UK,
   df_WB)

############### Correcting state names and district names to match with NFHS #########################

###################### State names made synchronous with NFHS-4 dataset #################

df_elections$State_Name[df_elections$State_Name=="andhra_pradesh"]<- "andhra pradesh"
df_elections$State_Name[df_elections$State_Name=="arunachal_pradesh"]<- "arunachal pradesh"
df_elections$State_Name[df_elections$State_Name=="himachal_pradesh"]<- "himachal pradesh"
df_elections$State_Name[df_elections$State_Name=="jammu_&_kashmir"]<- "jammu & kashmir"
df_elections$State_Name[df_elections$State_Name=="madhya_pradesh"]<- "madhya pradesh"
df_elections$State_Name[df_elections$State_Name=="odisha"]<- "odisha"
df_elections$State_Name[df_elections$State_Name=="tamil_nadu"]<- "tamil nadu"
df_elections$State_Name[df_elections$State_Name=="uttar_pradesh"]<- "uttar pradesh"
df_elections$State_Name[df_elections$State_Name=="west_bengal"]<- "west bengal"


############## Correcting district names to match with NFHS ##########################

###################### District names made synchronous with NFHS-4 dataset #################

df_elections$District_Name[df_elections$District_Name=="vishakapatnam" & df_elections$State_Name=="andhra pradesh"]<- "visakhapatnam"
df_elections$District_Name[df_elections$District_Name=="papum pare" & df_elections$State_Name=="arunachal pradesh"]<- "papumpare"
df_elections$District_Name[df_elections$District_Name=="marigaon" & df_elections$State_Name=="assam"]<- "morigaon"
df_elections$District_Name[df_elections$District_Name=="kamrup metro" & df_elections$State_Name=="assam"]<- "kamrup metropolitan"
df_elections$District_Name[df_elections$District_Name=="sibsagar" & df_elections$State_Name=="assam"]<- "sivsagar"
df_elections$District_Name[df_elections$District_Name=="buxor" & df_elections$State_Name=="bihar"]<- "buxar"
df_elections$District_Name[df_elections$District_Name=="jahanabad" & df_elections$State_Name=="bihar"]<- "jehanabad"
df_elections$District_Name[df_elections$District_Name=="east champaran" & df_elections$State_Name=="bihar"]<- "purba champaran"
df_elections$District_Name[df_elections$District_Name=="khagari a" & df_elections$State_Name=="bihar"]<- "khagaria"
df_elections$District_Name[df_elections$District_Name=="kaimur (bhabhua)" & df_elections$State_Name=="bihar"]<- "kaimur (bhabua)"
df_elections$District_Name[df_elections$District_Name=="purnea" & df_elections$State_Name=="bihar"]<- "purnia"
df_elections$District_Name[df_elections$District_Name=="west champaran" & df_elections$State_Name=="bihar"]<- "pashchim champaran"
df_elections$District_Name[df_elections$District_Name=="dantewada" & df_elections$State_Name=="chhattisgarh"]<- "dakshin bastar dantewaa"
df_elections$District_Name[df_elections$District_Name=="Korba" & df_elections$State_Name=="chhattisgarh"]<- "korba"
df_elections$District_Name[df_elections$District_Name=="kawardha" & df_elections$State_Name=="chhattisgarh"]<- "kabirdham"
df_elections$District_Name[df_elections$District_Name=="koria" & df_elections$State_Name=="chhattisgarh"]<- "koriya"
df_elections$District_Name[df_elections$District_Name=="ahmedabad" & df_elections$State_Name=="gujarat"]<- "ahmadabad"
df_elections$District_Name[df_elections$District_Name=="banas kantha" & df_elections$State_Name=="gujarat"]<- "banaskantha"
df_elections$District_Name[df_elections$District_Name=="dahod" & df_elections$State_Name=="gujarat"]<- "dohad"
df_elections$District_Name[df_elections$District_Name=="sabar kantha" & df_elections$State_Name=="gujarat"]<- "sabarkantha"
df_elections$District_Name[df_elections$District_Name=="lahul & spiti" & df_elections$State_Name=="himachal pradesh"]<- "lahul and spiti"
df_elections$District_Name[df_elections$District_Name=="sirmour" & df_elections$State_Name=="himachal pradesh"]<- "sirmaur"
df_elections$District_Name[df_elections$District_Name=="baramulla" & df_elections$State_Name=="jammu & kashmir"]<- "baramula"
df_elections$District_Name[df_elections$District_Name=="poonch" & df_elections$State_Name=="jammu & kashmir"]<- "punch"
df_elections$District_Name[df_elections$District_Name=="shopian" & df_elections$State_Name=="jammu & kashmir"]<- "shupiyan"
df_elections$District_Name[df_elections$District_Name=="chikballapur" & df_elections$State_Name=="karnataka"]<- "chikkaballapura"
df_elections$District_Name[df_elections$District_Name=="kasaragod" & df_elections$State_Name=="kerala"]<- "kasargod"
df_elections$District_Name[df_elections$District_Name=="ashok nagar" & df_elections$State_Name=="madhya pradesh"]<- "ashoknagar"
df_elections$District_Name[df_elections$District_Name=="ahmednagar" & df_elections$State_Name=="maharashtra"]<- "ahmadnagar"
df_elections$District_Name[df_elections$District_Name=="beed" & df_elections$State_Name=="maharashtra"]<- "bid"
df_elections$District_Name[df_elections$District_Name=="buldhana" & df_elections$State_Name=="maharashtra"]<- "buldana"
df_elections$District_Name[df_elections$District_Name=="raigad" & df_elections$State_Name=="maharashtra"]<- "raigarh"
df_elections$District_Name[df_elections$District_Name=="district sindhudurg" & df_elections$State_Name=="maharashtra"]<- "sindhudurg"
df_elections$District_Name[df_elections$District_Name=="ri bhoi" & df_elections$State_Name=="meghalaya"]<- "ribhoi"
df_elections$District_Name[df_elections$District_Name=="lawangtlai"|df_elections$District_Name=="lawngtlai" & df_elections$State_Name=="meghalaya"]<- "lawangtai"
df_elections$District_Name[df_elections$District_Name=="east delhi" & df_elections$State_Name=="delhi"]<- "east"
df_elections$District_Name[df_elections$District_Name=="north east delhi" & df_elections$State_Name=="delhi"]<- "north east"
df_elections$District_Name[df_elections$District_Name=="north west delhi" & df_elections$State_Name=="delhi"]<- "north west"
df_elections$District_Name[df_elections$District_Name=="south delhi" & df_elections$State_Name=="delhi"]<- "south"
df_elections$District_Name[df_elections$District_Name=="south west delhi" & df_elections$State_Name=="delhi"]<- "south west"
df_elections$District_Name[df_elections$District_Name=="west delhi" & df_elections$State_Name=="delhi"]<- "west"
df_elections$District_Name[df_elections$District_Name=="(east) singhbhum" & df_elections$State_Name=="jharkhand"]<- "purbi singhbhum"
df_elections$District_Name[df_elections$District_Name=="purbi singbhum" & df_elections$State_Name=="jharkhand"]<- "purbi singhbhum"
df_elections$District_Name[df_elections$District_Name=="Ranchi" & df_elections$State_Name=="jharkhand"]<- "ranchi"
df_elections$District_Name[df_elections$District_Name=="sahebganj" & df_elections$State_Name=="jharkhand"]<- "sahibganj"
df_elections$District_Name[df_elections$District_Name=="pakaur" & df_elections$State_Name=="jharkhand"]<- "pakur"
df_elections$District_Name[df_elections$District_Name=="palamau" & df_elections$State_Name=="jharkhand"]<- "palamu"
df_elections$District_Name[df_elections$District_Name=="rajauri" & df_elections$State_Name=="jammu & kashmir"]<- "rajouri"
df_elections$District_Name[df_elections$District_Name=="mumbai city" & df_elections$State_Name=="maharashtra"]<- "mumbai"
df_elections$District_Name[df_elections$District_Name=="lawangtai" & df_elections$State_Name=="mizoram"]<- "lawngtai"
df_elections$District_Name[df_elections$District_Name=="angul" & df_elections$State_Name=="odisha"]<- "anugul"
df_elections$District_Name[df_elections$District_Name=="balasore" & df_elections$State_Name=="odisha"]<- "baleshwar"
df_elections$District_Name[df_elections$District_Name=="boudh" & df_elections$State_Name=="odisha"]<- "baudh"
df_elections$District_Name[df_elections$District_Name=="jagatsinghpur" & df_elections$State_Name=="odisha"]<- "jagatsinghapur"
df_elections$District_Name[df_elections$District_Name=="jajpur" & df_elections$State_Name=="odisha"]<- "jajapur"
df_elections$District_Name[df_elections$District_Name=="kandhmal" & df_elections$State_Name=="odisha"]<- "kandhamal"
df_elections$District_Name[df_elections$District_Name=="keonjhar" & df_elections$State_Name=="odisha"]<- "kendujhar"
df_elections$District_Name[df_elections$District_Name=="khurda" & df_elections$State_Name=="odisha"]<- "khordha"
df_elections$District_Name[df_elections$District_Name=="nabarangpur" & df_elections$State_Name=="odisha"]<- "nabarangapur"
df_elections$District_Name[df_elections$District_Name=="bathinda" & df_elections$State_Name=="punjab"]<- "bhatinda"
df_elections$District_Name[df_elections$District_Name=="mukatsar" & df_elections$State_Name=="punjab"]<- "muktsar"
df_elections$District_Name[df_elections$District_Name=="rup nagar" & df_elections$State_Name=="punjab"]<- "rupnagar"
df_elections$District_Name[df_elections$District_Name=="s.a.s. nagar" & df_elections$State_Name=="punjab"]<- "sahibzada ajit singh nagar"
df_elections$District_Name[df_elections$District_Name=="tarn taran" & df_elections$State_Name=="punjab"]<- "tarntaran"
df_elections$District_Name[df_elections$District_Name=="chittorgarh" & df_elections$State_Name=="rajasthan"]<- "chittaurgarh"
df_elections$District_Name[df_elections$District_Name=="dholpur" & df_elections$State_Name=="rajasthan"]<- "dhaulpur"
df_elections$District_Name[df_elections$District_Name=="jalore" & df_elections$State_Name=="rajasthan"]<- "jalor"
df_elections$District_Name[df_elections$District_Name=="jhunjhunu" & df_elections$State_Name=="rajasthan"]<- "jhunjhunun"
df_elections$District_Name[df_elections$District_Name=="kanchipuram" & df_elections$State_Name=="tamil nadu"]<- "kancheepuram"
df_elections$District_Name[df_elections$District_Name=="kapur" & df_elections$State_Name=="tamil nadu"]<- "karur"
df_elections$District_Name[df_elections$District_Name=="thoothukudi" & df_elections$State_Name=="tamil nadu"]<- "thoothukkudi"
df_elections$District_Name[df_elections$District_Name=="toothukudi" & df_elections$State_Name=="tamil nadu"]<- "thoothukkudi"
df_elections$District_Name[df_elections$District_Name=="tirupur" & df_elections$State_Name=="tamil nadu"]<- "tiruppur"
df_elections$District_Name[df_elections$District_Name=="tiruvanamalai" & df_elections$State_Name=="tamil nadu"]<- "tiruvannamalai"
df_elections$District_Name[df_elections$District_Name=="villupuram" & df_elections$State_Name=="tamil nadu"]<- "viluppuram"
df_elections$District_Name[df_elections$District_Name=="ambedakar nagar" & df_elections$State_Name=="uttar pradesh"]<- "ambedkar nagar"
df_elections$District_Name[df_elections$District_Name=="barabanki" & df_elections$State_Name=="uttar pradesh"]<- "bara banki"
df_elections$District_Name[df_elections$District_Name=="badaun" & df_elections$State_Name=="uttar pradesh"]<- "budaun"
df_elections$District_Name[df_elections$District_Name=="bulandshahar" & df_elections$State_Name=="uttar pradesh"]<- "bulandshahr"
df_elections$District_Name[df_elections$District_Name=="gautam budh nagar" & df_elections$State_Name=="uttar pradesh"]<- "gautam buddha nagar"
df_elections$District_Name[df_elections$District_Name=="kanpur grameen" & df_elections$State_Name=="uttar pradesh"]<- "kanpur dehat"
df_elections$District_Name[df_elections$District_Name=="kaushambi" & df_elections$State_Name=="uttar pradesh"]<- "kaushambi"
df_elections$District_Name[df_elections$District_Name=="mahamaya nagar" & df_elections$State_Name=="uttar pradesh"]<- "mahamayanagar"
df_elections$District_Name[df_elections$District_Name=="maharajganj" & df_elections$State_Name=="uttar pradesh"]<- "mahrajganj"
df_elections$District_Name[df_elections$District_Name=="shravasti" & df_elections$State_Name=="uttar pradesh"]<- "shrawasti"
df_elections$District_Name[df_elections$District_Name=="siddharthnagar" & df_elections$State_Name=="uttar pradesh"]<- "siddharth nagar"
df_elections$District_Name[df_elections$District_Name=="bardhaman" & df_elections$State_Name=="west bengal"]<- "barddhaman"
df_elections$District_Name[df_elections$District_Name=="darjeeling" & df_elections$State_Name=="west bengal"]<- "darjiling"
df_elections$District_Name[df_elections$District_Name=="howrah" & df_elections$State_Name=="west bengal"]<- "haora"
df_elections$District_Name[df_elections$District_Name=="koch behar" & df_elections$State_Name=="west bengal"]<- "cooch behar"
df_elections$District_Name[df_elections$District_Name=="kolkata corporation" & df_elections$State_Name=="west bengal"]<- "kolkata"
df_elections$District_Name[df_elections$District_Name=="north 24 parganas"|df_elections$District_Name=="north twentyfour parganas" & df_elections$State_Name=="west bengal"]<- "north twenty four parganas"
df_elections$District_Name[df_elections$District_Name=="south 24 parganas"|df_elections$District_Name=="south twentyfour parganas" & df_elections$State_Name=="west bengal"]<- "south twenty four parganas"
df_elections$District_Name[df_elections$District_Name=="paschim medinipur" & df_elections$State_Name=="west bengal"]<- "paschim mednipur"
df_elections$District_Name[df_elections$District_Name=="purba medinipur" & df_elections$State_Name=="west bengal"]<- "purba mednipur"
df_elections$District_Name[df_elections$District_Name=="purulia" & df_elections$State_Name=="west bengal"]<- "puruliya"

write.csv(df_elections,"All_constituencies.csv")

################# Subsetting only first and second position holders #######################

df_first_second<- df_elections%>%
  subset(Position==1|Position==2)

write.csv(df_first_second,"First_second_constituency_elections.csv")

################## Subsetting Female winners or runners up #####################

df_female<- df_first_second%>%
  subset(Sex=="F")%>%
  select(State_Name,
         District_Name,
         Constituency_Name,
         Year,
         month,
         Position,
         Party,
         Votes,
         Valid_Votes,
         Electors,
         N_Cand,
         Turnout_Percentage,
         Vote_Share_Percentage,
         Margin,
         Margin_Percentage)%>%
  mutate(id=paste(State_Name,District_Name,Constituency_Name,Year,sep = "-"))

colnames(df_female)[which(names(df_female) == "Position")] <- "position_female"
colnames(df_female)[which(names(df_female) == "Party")] <- "party_female"
colnames(df_female)[which(names(df_female) == "Votes")] <- "votes_female"
colnames(df_female)[which(names(df_female) == "Vote_Share_Percentage")] <- "vote_percent_female"
colnames(df_female)[which(names(df_female) == "Margin")] <- "margin_female"
colnames(df_female)[which(names(df_female) == "Margin_Percentage")] <- "margin_percent_female"

################## Subsetting Male winners or runners up #####################

df_male<- df_first_second%>%
  subset(Sex=="M")%>%
  select(State_Name,
         District_Name,
         Constituency_Name,
         Year,
         month,
         Position,
         Party,
         Votes,
         Valid_Votes,
         Electors,
         N_Cand,
         Turnout_Percentage,
         Vote_Share_Percentage,
         Margin,
         Margin_Percentage)%>%
  mutate(id=paste(State_Name,District_Name,Constituency_Name,Year,sep = "-"))

colnames(df_male)[which(names(df_male) == "Position")] <- "position_male"
colnames(df_male)[which(names(df_male) == "Party")] <- "party_male"
colnames(df_male)[which(names(df_male) == "Votes")] <- "votes_male"
colnames(df_male)[which(names(df_male) == "Vote_Share_Percentage")] <- "vote_percent_male"
colnames(df_male)[which(names(df_male) == "Margin")] <- "margin_male"
colnames(df_male)[which(names(df_male) == "Margin_Percentage")] <- "margin_percent_male"

################################ Subsetting elections where either the male and female were the winner or runners up ###################################

df_final_const<- merge(df_female, 
                  df_male[, 
                                       c("id", 
                                         setdiff(colnames(df_male),
                                                 colnames(df_female)))], 
                  by="id")


write.csv(df_final_const,"Constituency_female_male.csv")


###################################### Aggregating to the district level ######################################

################### For any margin #################

df_any_margin<- df_final_const%>%
  mutate(female_winner=ifelse(vote_percent_female-vote_percent_male>0,1,0),
         winner_margin=(vote_percent_female-vote_percent_male)/100,
         close_election=ifelse(winner_margin>=-0.035 & winner_margin<=0.035,1,0))%>%
  group_by(State_Name,District_Name,Year)%>%
  summarise(female_winner=mean(female_winner,na.rm = TRUE),
            total_voters_female=sum(votes_female,na.rm = TRUE),
            total_voters_male=sum(votes_male,na.rm = TRUE),
            total_electors=sum(Electors,na.rm = TRUE),
            avg_cands=mean(N_Cand,na.rm = TRUE),
            margin=mean(winner_margin,na.rm = TRUE),
            close_election=mean(close_election,na.rm = TRUE))%>%
  mutate(female_vote_share=(total_voters_female)/(total_voters_female + total_voters_male))%>%
  mutate(id=paste(State_Name,District_Name,Year,sep = "-"))


write.csv(df_any_margin,"Margin_all_elections.csv")

################### For margin=2.5% #################

df_2_5_margin<- df_final_const%>%
  mutate(female_winner=ifelse(vote_percent_female-vote_percent_male>0,1,0),
         winner_margin=(vote_percent_female-vote_percent_male)/100)%>%
  subset(winner_margin>=-0.025 & winner_margin<=0.025)%>%
  group_by(State_Name,District_Name,Year)%>%
  summarise(female_winner_2_5=mean(female_winner,na.rm = TRUE),
            total_voters_female_2_5=sum(votes_female,na.rm = TRUE),
            total_voters_male_2_5=sum(votes_male,na.rm = TRUE),
            total_electors_2_5=sum(Electors,na.rm = TRUE),
            avg_cands_2_5=mean(N_Cand,na.rm = TRUE),
            margin_2_5=mean(winner_margin,na.rm = TRUE))%>%
  mutate(female_vote_share_2_5=(total_voters_female_2_5)/(total_voters_female_2_5 + total_voters_male_2_5))%>%
  mutate(id=paste(State_Name,District_Name,Year,sep = "-"))


write.csv(df_2_5_margin,"Margin_2_5_elections.csv")

################### For margin=3.5% #################

df_3_5_margin<- df_final_const%>%
  mutate(female_winner=ifelse(vote_percent_female-vote_percent_male>0,1,0),
         winner_margin=(vote_percent_female-vote_percent_male)/100)%>%
  subset(winner_margin>=-0.035 & winner_margin<=0.035)%>%
  group_by(State_Name,District_Name,Year)%>%
  summarise(female_winner_3_5=mean(female_winner,na.rm = TRUE),
            total_voters_female_3_5=sum(votes_female,na.rm = TRUE),
            total_voters_male_3_5=sum(votes_male,na.rm = TRUE),
            total_electors_3_5=sum(Electors,na.rm = TRUE),
            avg_cands_3_5=mean(N_Cand,na.rm = TRUE),
            margin_3_5=mean(winner_margin,na.rm = TRUE))%>%
  mutate(female_vote_share_3_5=(total_voters_female_3_5)/(total_voters_female_3_5 + total_voters_male_3_5))%>%
  mutate(id=paste(State_Name,District_Name,Year,sep = "-"))

write.csv(df_3_5_margin,"Margin_3_5_elections.csv")

################### For margin=4.5% #################

df_4_5_margin<- df_final_const%>%
  mutate(female_winner=ifelse(vote_percent_female-vote_percent_male>0,1,0),
         winner_margin=(vote_percent_female-vote_percent_male)/100)%>%
  subset(winner_margin>=-0.045 & winner_margin<=0.045)%>%
  group_by(State_Name,District_Name,Year)%>%
  summarise(female_winner_4_5=mean(female_winner,na.rm = TRUE),
            total_voters_female_4_5=sum(votes_female,na.rm = TRUE),
            total_voters_male_4_5=sum(votes_male,na.rm = TRUE),
            total_electors_4_5=sum(Electors,na.rm = TRUE),
            avg_cands_4_5=mean(N_Cand,na.rm = TRUE),
            margin_1_5=mean(winner_margin,na.rm = TRUE))%>%
  mutate(female_vote_share_4_5=(total_voters_female_4_5)/(total_voters_female_4_5 + total_voters_male_4_5))%>%
  mutate(id=paste(State_Name,District_Name,Year,sep = "-"))

write.csv(df_4_5_margin,"Margin_4_5_elections.csv")

df_analysis<- merge(df_any_margin,df_2_5_margin[, c("id", 
                                                    setdiff(colnames(df_2_5_margin),
                                                            colnames(df_any_margin)))], by="id",all.x = TRUE)

df_analysis<- merge(df_analysis,df_3_5_margin[, c("id", 
                                                setdiff(colnames(df_3_5_margin),
                                                        colnames(df_analysis)))], by="id",all.x = TRUE)

df_analysis<- merge(df_analysis,df_4_5_margin[, c("id", 
                                                setdiff(colnames(df_4_5_margin),
                                                        colnames(df_analysis)))], by="id",all.x = TRUE)

df_analysis<- df_analysis%>%
  mutate(id=paste(State_Name,District_Name,Year,sep="-"))


write.csv(df_analysis,"Final_election_data_for_analysis.csv")
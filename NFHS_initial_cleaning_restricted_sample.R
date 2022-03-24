library(dplyr)
library(foreign)
library(ggplot2)
library(Hmisc)
library(plm)
library(broom)
library(jtools)
library(tidyverse)
library(lmtest)
library(reshape2)
library(stargazer)
library(hrbrthemes)

setwd("C:\\Users\\das90\\OneDrive - City University of New York\\Research\\Women_politicians_and_child_survivability\\Datasets")

memory.limit(size = 40000)

df_1<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\NFHS-4\\idhs_00007.csv")

############### Naming the districts from the codes #######################


df_1$district_name[df_1$GEOALT_IA2015==1]<- "kupwara"
df_1$district_name[df_1$GEOALT_IA2015==2]<- "badgam"
df_1$district_name[df_1$GEOALT_IA2015==3]<- "leh"
df_1$district_name[df_1$GEOALT_IA2015==4]<- "kargil"
df_1$district_name[df_1$GEOALT_IA2015==5]<- "punch"
df_1$district_name[df_1$GEOALT_IA2015==6]<- "rajouri"
df_1$district_name[df_1$GEOALT_IA2015==7]<- "kathua"
df_1$district_name[df_1$GEOALT_IA2015==8]<- "baramula"
df_1$district_name[df_1$GEOALT_IA2015==9]<- "bandipore"
df_1$district_name[df_1$GEOALT_IA2015==10]<- "srinagar"
df_1$district_name[df_1$GEOALT_IA2015==11]<- "ganderbal"
df_1$district_name[df_1$GEOALT_IA2015==12]<- "pulwama"
df_1$district_name[df_1$GEOALT_IA2015==13]<- "shupiyan"
df_1$district_name[df_1$GEOALT_IA2015==14]<- "anantnag"
df_1$district_name[df_1$GEOALT_IA2015==15]<- "kulgam"
df_1$district_name[df_1$GEOALT_IA2015==16]<- "doda"
df_1$district_name[df_1$GEOALT_IA2015==17]<- "ramban"
df_1$district_name[df_1$GEOALT_IA2015==18]<- "kishtwar"
df_1$district_name[df_1$GEOALT_IA2015==19]<- "udhampur"
df_1$district_name[df_1$GEOALT_IA2015==20]<- "reasi"
df_1$district_name[df_1$GEOALT_IA2015==21]<- "jammu"
df_1$district_name[df_1$GEOALT_IA2015==22]<- "samba"
df_1$district_name[df_1$GEOALT_IA2015==23]<- "chamba"
df_1$district_name[df_1$GEOALT_IA2015==24]<- "kangra"
df_1$district_name[df_1$GEOALT_IA2015==25]<- "lahul and spiti"
df_1$district_name[df_1$GEOALT_IA2015==26]<- "kullu"
df_1$district_name[df_1$GEOALT_IA2015==27]<- "mandi"
df_1$district_name[df_1$GEOALT_IA2015==28]<- "hamirpur"
df_1$district_name[df_1$GEOALT_IA2015==29]<- "una"
df_1$district_name[df_1$GEOALT_IA2015==30]<- "bilaspur"
df_1$district_name[df_1$GEOALT_IA2015==31]<- "solan"
df_1$district_name[df_1$GEOALT_IA2015==32]<- "sirmaur"
df_1$district_name[df_1$GEOALT_IA2015==33]<- "shimla"
df_1$district_name[df_1$GEOALT_IA2015==34]<- "kinnaur"
df_1$district_name[df_1$GEOALT_IA2015==35]<- "gurdaspur"
df_1$district_name[df_1$GEOALT_IA2015==36]<- "kapurthala"
df_1$district_name[df_1$GEOALT_IA2015==37]<- "jalandhar"
df_1$district_name[df_1$GEOALT_IA2015==38]<- "hoshiarpur"
df_1$district_name[df_1$GEOALT_IA2015==39]<- "sangrur"
df_1$district_name[df_1$GEOALT_IA2015==40]<- "fatehgarh sahib"
df_1$district_name[df_1$GEOALT_IA2015==41]<- "ludhiana"
df_1$district_name[df_1$GEOALT_IA2015==42]<- "moga"
df_1$district_name[df_1$GEOALT_IA2015==43]<- "firozpur"
df_1$district_name[df_1$GEOALT_IA2015==44]<- "muktsar"
df_1$district_name[df_1$GEOALT_IA2015==45]<- "faridkot"
df_1$district_name[df_1$GEOALT_IA2015==46]<- "bhatinda"
df_1$district_name[df_1$GEOALT_IA2015==47]<- "mansa"
df_1$district_name[df_1$GEOALT_IA2015==48]<- "patiala"
df_1$district_name[df_1$GEOALT_IA2015==49]<- "amritsar"
df_1$district_name[df_1$GEOALT_IA2015==50]<- "tarntaran"
df_1$district_name[df_1$GEOALT_IA2015==51]<- "rupnagar"
df_1$district_name[df_1$GEOALT_IA2015==52]<- "sahibzada ajit singh nagar"
df_1$district_name[df_1$GEOALT_IA2015==53]<- "sahid bhagat singh nagar"
df_1$district_name[df_1$GEOALT_IA2015==54]<- "barnala"
df_1$district_name[df_1$GEOALT_IA2015==55]<- "chandigarh"
df_1$district_name[df_1$GEOALT_IA2015==56]<- "uttarkashi"
df_1$district_name[df_1$GEOALT_IA2015==57]<- "chamoli"
df_1$district_name[df_1$GEOALT_IA2015==58]<- "rudraprayag"
df_1$district_name[df_1$GEOALT_IA2015==59]<- "tehri garhwal"
df_1$district_name[df_1$GEOALT_IA2015==60]<- "dehradun"
df_1$district_name[df_1$GEOALT_IA2015==61]<- "garhwal"
df_1$district_name[df_1$GEOALT_IA2015==62]<- "pithoragarh"
df_1$district_name[df_1$GEOALT_IA2015==63]<- "bageshwar"
df_1$district_name[df_1$GEOALT_IA2015==64]<- "almora"
df_1$district_name[df_1$GEOALT_IA2015==65]<- "champawat"
df_1$district_name[df_1$GEOALT_IA2015==66]<- "nainital"
df_1$district_name[df_1$GEOALT_IA2015==67]<- "udham singh nagar"
df_1$district_name[df_1$GEOALT_IA2015==68]<- "hardwar"
df_1$district_name[df_1$GEOALT_IA2015==69]<- "panchkula"
df_1$district_name[df_1$GEOALT_IA2015==70]<- "ambala"
df_1$district_name[df_1$GEOALT_IA2015==71]<- "yamunanagar"
df_1$district_name[df_1$GEOALT_IA2015==72]<- "kurukshetra"
df_1$district_name[df_1$GEOALT_IA2015==73]<- "kaithal"
df_1$district_name[df_1$GEOALT_IA2015==74]<- "karnal"
df_1$district_name[df_1$GEOALT_IA2015==75]<- "panipat"
df_1$district_name[df_1$GEOALT_IA2015==76]<- "sonipat"
df_1$district_name[df_1$GEOALT_IA2015==77]<- "jind"
df_1$district_name[df_1$GEOALT_IA2015==78]<- "fatehabad"
df_1$district_name[df_1$GEOALT_IA2015==79]<- "sirsa"
df_1$district_name[df_1$GEOALT_IA2015==80]<- "hisar"
df_1$district_name[df_1$GEOALT_IA2015==81]<- "bhiwani"
df_1$district_name[df_1$GEOALT_IA2015==82]<- "rohtak"
df_1$district_name[df_1$GEOALT_IA2015==83]<- "jhajjar"
df_1$district_name[df_1$GEOALT_IA2015==84]<- "mahendragarh"
df_1$district_name[df_1$GEOALT_IA2015==85]<- "rewari"
df_1$district_name[df_1$GEOALT_IA2015==86]<- "gurgaon"
df_1$district_name[df_1$GEOALT_IA2015==87]<- "mewat"
df_1$district_name[df_1$GEOALT_IA2015==88]<- "faridabad"
df_1$district_name[df_1$GEOALT_IA2015==89]<- "palwal"
df_1$district_name[df_1$GEOALT_IA2015==90]<- "north west"
df_1$district_name[df_1$GEOALT_IA2015==91]<- "north"
df_1$district_name[df_1$GEOALT_IA2015==92]<- "north east"
df_1$district_name[df_1$GEOALT_IA2015==93]<- "east"
df_1$district_name[df_1$GEOALT_IA2015==94]<- "new delhi"
df_1$district_name[df_1$GEOALT_IA2015==95]<- "central"
df_1$district_name[df_1$GEOALT_IA2015==96]<- "west"
df_1$district_name[df_1$GEOALT_IA2015==97]<- "south west"
df_1$district_name[df_1$GEOALT_IA2015==98]<- "south"
df_1$district_name[df_1$GEOALT_IA2015==99]<- "ganganagar"
df_1$district_name[df_1$GEOALT_IA2015==100]<- "hanumangarh"
df_1$district_name[df_1$GEOALT_IA2015==101]<- "bikaner"
df_1$district_name[df_1$GEOALT_IA2015==102]<- "churu"
df_1$district_name[df_1$GEOALT_IA2015==103]<- "jhunjhunun"
df_1$district_name[df_1$GEOALT_IA2015==104]<- "alwar"
df_1$district_name[df_1$GEOALT_IA2015==105]<- "bharatpur"
df_1$district_name[df_1$GEOALT_IA2015==106]<- "dhaulpur"
df_1$district_name[df_1$GEOALT_IA2015==107]<- "karauli"
df_1$district_name[df_1$GEOALT_IA2015==108]<- "sawai madhopur"
df_1$district_name[df_1$GEOALT_IA2015==109]<- "dausa"
df_1$district_name[df_1$GEOALT_IA2015==110]<- "jaipur"
df_1$district_name[df_1$GEOALT_IA2015==111]<- "sikar"
df_1$district_name[df_1$GEOALT_IA2015==112]<- "nagaur"
df_1$district_name[df_1$GEOALT_IA2015==113]<- "jodhpur"
df_1$district_name[df_1$GEOALT_IA2015==114]<- "jaisalmer"
df_1$district_name[df_1$GEOALT_IA2015==115]<- "barmer"
df_1$district_name[df_1$GEOALT_IA2015==116]<- "jalor"
df_1$district_name[df_1$GEOALT_IA2015==117]<- "sirohi"
df_1$district_name[df_1$GEOALT_IA2015==118]<- "pali"
df_1$district_name[df_1$GEOALT_IA2015==119]<- "ajmer"
df_1$district_name[df_1$GEOALT_IA2015==120]<- "tonk"
df_1$district_name[df_1$GEOALT_IA2015==121]<- "bundi"
df_1$district_name[df_1$GEOALT_IA2015==122]<- "bhilwara"
df_1$district_name[df_1$GEOALT_IA2015==123]<- "rajsamand"
df_1$district_name[df_1$GEOALT_IA2015==124]<- "dungarpur"
df_1$district_name[df_1$GEOALT_IA2015==125]<- "banswara"
df_1$district_name[df_1$GEOALT_IA2015==126]<- "chittaurgarh"
df_1$district_name[df_1$GEOALT_IA2015==127]<- "kota"
df_1$district_name[df_1$GEOALT_IA2015==128]<- "baran"
df_1$district_name[df_1$GEOALT_IA2015==129]<- "jhalawar"
df_1$district_name[df_1$GEOALT_IA2015==130]<- "udaipur"
df_1$district_name[df_1$GEOALT_IA2015==131]<- "pratapgarh"
df_1$district_name[df_1$GEOALT_IA2015==132]<- "saharanpur"
df_1$district_name[df_1$GEOALT_IA2015==133]<- "muzaffarnagar"
df_1$district_name[df_1$GEOALT_IA2015==134]<- "bijnor"
df_1$district_name[df_1$GEOALT_IA2015==135]<- "moradabad"
df_1$district_name[df_1$GEOALT_IA2015==136]<- "rampur"
df_1$district_name[df_1$GEOALT_IA2015==137]<- "jyotiba phule nagar"
df_1$district_name[df_1$GEOALT_IA2015==138]<- "meerut"
df_1$district_name[df_1$GEOALT_IA2015==139]<- "baghpat"
df_1$district_name[df_1$GEOALT_IA2015==140]<- "ghaziabad"
df_1$district_name[df_1$GEOALT_IA2015==141]<- "gautam buddha nagar"
df_1$district_name[df_1$GEOALT_IA2015==142]<- "bulandshahr"
df_1$district_name[df_1$GEOALT_IA2015==143]<- "aligarh"
df_1$district_name[df_1$GEOALT_IA2015==144]<- "mahamayanagar"
df_1$district_name[df_1$GEOALT_IA2015==145]<- "mathura"
df_1$district_name[df_1$GEOALT_IA2015==146]<- "agra"
df_1$district_name[df_1$GEOALT_IA2015==147]<- "firozabad"
df_1$district_name[df_1$GEOALT_IA2015==148]<- "mainpuri"
df_1$district_name[df_1$GEOALT_IA2015==149]<- "budaun"
df_1$district_name[df_1$GEOALT_IA2015==150]<- "bareilly"
df_1$district_name[df_1$GEOALT_IA2015==151]<- "pilibhit"
df_1$district_name[df_1$GEOALT_IA2015==152]<- "shahjahanpur"
df_1$district_name[df_1$GEOALT_IA2015==153]<- "kheri"
df_1$district_name[df_1$GEOALT_IA2015==154]<- "sitapur"
df_1$district_name[df_1$GEOALT_IA2015==155]<- "hardoi"
df_1$district_name[df_1$GEOALT_IA2015==156]<- "unnao"
df_1$district_name[df_1$GEOALT_IA2015==157]<- "lucknow"
df_1$district_name[df_1$GEOALT_IA2015==158]<- "rae bareli"
df_1$district_name[df_1$GEOALT_IA2015==159]<- "farrukhabad"
df_1$district_name[df_1$GEOALT_IA2015==160]<- "kannauj"
df_1$district_name[df_1$GEOALT_IA2015==161]<- "etawah"
df_1$district_name[df_1$GEOALT_IA2015==162]<- "auraiya"
df_1$district_name[df_1$GEOALT_IA2015==163]<- "kanpur dehat"
df_1$district_name[df_1$GEOALT_IA2015==164]<- "kanpur nagar"
df_1$district_name[df_1$GEOALT_IA2015==165]<- "jalaun"
df_1$district_name[df_1$GEOALT_IA2015==166]<- "jhansi"
df_1$district_name[df_1$GEOALT_IA2015==167]<- "lalitpur"
df_1$district_name[df_1$GEOALT_IA2015==169]<- "mahoba"
df_1$district_name[df_1$GEOALT_IA2015==170]<- "banda"
df_1$district_name[df_1$GEOALT_IA2015==171]<- "chitrakoot"
df_1$district_name[df_1$GEOALT_IA2015==172]<- "fatehpur"
df_1$district_name[df_1$GEOALT_IA2015==174]<- "kaushambi"
df_1$district_name[df_1$GEOALT_IA2015==175]<- "allahabad"
df_1$district_name[df_1$GEOALT_IA2015==176]<- "bara banki"
df_1$district_name[df_1$GEOALT_IA2015==177]<- "faizabad"
df_1$district_name[df_1$GEOALT_IA2015==178]<- "ambedkar nagar"
df_1$district_name[df_1$GEOALT_IA2015==179]<- "sultanpur"
df_1$district_name[df_1$GEOALT_IA2015==180]<- "bahraich"
df_1$district_name[df_1$GEOALT_IA2015==181]<- "shrawasti"
df_1$district_name[df_1$GEOALT_IA2015==182]<- "balrampur"
df_1$district_name[df_1$GEOALT_IA2015==183]<- "gonda"
df_1$district_name[df_1$GEOALT_IA2015==184]<- "siddharth nagar"
df_1$district_name[df_1$GEOALT_IA2015==185]<- "basti"
df_1$district_name[df_1$GEOALT_IA2015==186]<- "sant kabir nagar"
df_1$district_name[df_1$GEOALT_IA2015==187]<- "mahrajganj"
df_1$district_name[df_1$GEOALT_IA2015==188]<- "gorakhpur"
df_1$district_name[df_1$GEOALT_IA2015==189]<- "kushinagar"
df_1$district_name[df_1$GEOALT_IA2015==190]<- "deoria"
df_1$district_name[df_1$GEOALT_IA2015==191]<- "azamgarh"
df_1$district_name[df_1$GEOALT_IA2015==192]<- "mau"
df_1$district_name[df_1$GEOALT_IA2015==193]<- "ballia"
df_1$district_name[df_1$GEOALT_IA2015==194]<- "jaunpur"
df_1$district_name[df_1$GEOALT_IA2015==195]<- "ghazipur"
df_1$district_name[df_1$GEOALT_IA2015==196]<- "chandauli"
df_1$district_name[df_1$GEOALT_IA2015==197]<- "varanasi"
df_1$district_name[df_1$GEOALT_IA2015==198]<- "sant ravidas nagar"
df_1$district_name[df_1$GEOALT_IA2015==199]<- "mirzapur"
df_1$district_name[df_1$GEOALT_IA2015==200]<- "sonbhadra"
df_1$district_name[df_1$GEOALT_IA2015==201]<- "etah"
df_1$district_name[df_1$GEOALT_IA2015==202]<- "kanshiram nagar"
df_1$district_name[df_1$GEOALT_IA2015==203]<- "pashchim champaran"
df_1$district_name[df_1$GEOALT_IA2015==204]<- "purba champaran"
df_1$district_name[df_1$GEOALT_IA2015==205]<- "sheohar"
df_1$district_name[df_1$GEOALT_IA2015==206]<- "sitamarhi"
df_1$district_name[df_1$GEOALT_IA2015==207]<- "madhubani"
df_1$district_name[df_1$GEOALT_IA2015==208]<- "supaul"
df_1$district_name[df_1$GEOALT_IA2015==209]<- "araria"
df_1$district_name[df_1$GEOALT_IA2015==210]<- "kishanganj"
df_1$district_name[df_1$GEOALT_IA2015==211]<- "purnia"
df_1$district_name[df_1$GEOALT_IA2015==212]<- "katihar"
df_1$district_name[df_1$GEOALT_IA2015==213]<- "madhepura"
df_1$district_name[df_1$GEOALT_IA2015==214]<- "saharsa"
df_1$district_name[df_1$GEOALT_IA2015==215]<- "darbhanga"
df_1$district_name[df_1$GEOALT_IA2015==216]<- "muzaffarpur"
df_1$district_name[df_1$GEOALT_IA2015==217]<- "gopalganj"
df_1$district_name[df_1$GEOALT_IA2015==218]<- "siwan"
df_1$district_name[df_1$GEOALT_IA2015==219]<- "saran"
df_1$district_name[df_1$GEOALT_IA2015==220]<- "vaishali"
df_1$district_name[df_1$GEOALT_IA2015==221]<- "samastipur"
df_1$district_name[df_1$GEOALT_IA2015==222]<- "begusarai"
df_1$district_name[df_1$GEOALT_IA2015==223]<- "khagaria"
df_1$district_name[df_1$GEOALT_IA2015==224]<- "bhagalpur"
df_1$district_name[df_1$GEOALT_IA2015==225]<- "banka"
df_1$district_name[df_1$GEOALT_IA2015==226]<- "munger"
df_1$district_name[df_1$GEOALT_IA2015==227]<- "lakhisarai"
df_1$district_name[df_1$GEOALT_IA2015==228]<- "sheikhpura"
df_1$district_name[df_1$GEOALT_IA2015==229]<- "nalanda"
df_1$district_name[df_1$GEOALT_IA2015==230]<- "patna"
df_1$district_name[df_1$GEOALT_IA2015==231]<- "bhojpur"
df_1$district_name[df_1$GEOALT_IA2015==232]<- "buxar"
df_1$district_name[df_1$GEOALT_IA2015==233]<- "kaimur (bhabua)"
df_1$district_name[df_1$GEOALT_IA2015==234]<- "rohtas"
df_1$district_name[df_1$GEOALT_IA2015==235]<- "aurangabad"
df_1$district_name[df_1$GEOALT_IA2015==236]<- "gaya"
df_1$district_name[df_1$GEOALT_IA2015==237]<- "nawada"
df_1$district_name[df_1$GEOALT_IA2015==238]<- "jamui"
df_1$district_name[df_1$GEOALT_IA2015==239]<- "jehanabad"
df_1$district_name[df_1$GEOALT_IA2015==240]<- "arwal"
df_1$district_name[df_1$GEOALT_IA2015==241]<- "north district"
df_1$district_name[df_1$GEOALT_IA2015==242]<- "west district"
df_1$district_name[df_1$GEOALT_IA2015==243]<- "south district"
df_1$district_name[df_1$GEOALT_IA2015==244]<- "east district"
df_1$district_name[df_1$GEOALT_IA2015==245]<- "tawang"
df_1$district_name[df_1$GEOALT_IA2015==246]<- "west kameng"
df_1$district_name[df_1$GEOALT_IA2015==247]<- "east kameng"
df_1$district_name[df_1$GEOALT_IA2015==248]<- "papumpare"
df_1$district_name[df_1$GEOALT_IA2015==249]<- "upper subansiri"
df_1$district_name[df_1$GEOALT_IA2015==250]<- "west siang"
df_1$district_name[df_1$GEOALT_IA2015==251]<- "east siang"
df_1$district_name[df_1$GEOALT_IA2015==252]<- "upper siang"
df_1$district_name[df_1$GEOALT_IA2015==253]<- "changlang"
df_1$district_name[df_1$GEOALT_IA2015==254]<- "tirap"
df_1$district_name[df_1$GEOALT_IA2015==255]<- "lower subansiri"
df_1$district_name[df_1$GEOALT_IA2015==256]<- "kurung kumey"
df_1$district_name[df_1$GEOALT_IA2015==257]<- "dibang valley"
df_1$district_name[df_1$GEOALT_IA2015==258]<- "lower dibang valley"
df_1$district_name[df_1$GEOALT_IA2015==259]<- "lohit"
df_1$district_name[df_1$GEOALT_IA2015==260]<- "anjaw"
df_1$district_name[df_1$GEOALT_IA2015==261]<- "mon"
df_1$district_name[df_1$GEOALT_IA2015==262]<- "mokokchung"
df_1$district_name[df_1$GEOALT_IA2015==263]<- "zunheboto"
df_1$district_name[df_1$GEOALT_IA2015==264]<- "wokha"
df_1$district_name[df_1$GEOALT_IA2015==265]<- "dimapur"
df_1$district_name[df_1$GEOALT_IA2015==266]<- "phek"
df_1$district_name[df_1$GEOALT_IA2015==267]<- "tuensang"
df_1$district_name[df_1$GEOALT_IA2015==268]<- "longleng"
df_1$district_name[df_1$GEOALT_IA2015==269]<- "kiphire"
df_1$district_name[df_1$GEOALT_IA2015==270]<- "kohima"
df_1$district_name[df_1$GEOALT_IA2015==271]<- "peren"
df_1$district_name[df_1$GEOALT_IA2015==272]<- "senapati"
df_1$district_name[df_1$GEOALT_IA2015==273]<- "tamenglong"
df_1$district_name[df_1$GEOALT_IA2015==274]<- "churachandpur"
df_1$district_name[df_1$GEOALT_IA2015==275]<- "bishnupur"
df_1$district_name[df_1$GEOALT_IA2015==276]<- "thoubal"
df_1$district_name[df_1$GEOALT_IA2015==277]<- "imphal west"
df_1$district_name[df_1$GEOALT_IA2015==278]<- "imphal east"
df_1$district_name[df_1$GEOALT_IA2015==279]<- "ukhrul"
df_1$district_name[df_1$GEOALT_IA2015==280]<- "chandel"
df_1$district_name[df_1$GEOALT_IA2015==281]<- "mamit"
df_1$district_name[df_1$GEOALT_IA2015==282]<- "kolasib"
df_1$district_name[df_1$GEOALT_IA2015==283]<- "aizawl"
df_1$district_name[df_1$GEOALT_IA2015==284]<- "champhai"
df_1$district_name[df_1$GEOALT_IA2015==285]<- "serchhip"
df_1$district_name[df_1$GEOALT_IA2015==286]<- "lunglei"
df_1$district_name[df_1$GEOALT_IA2015==287]<- "lawngtai"
df_1$district_name[df_1$GEOALT_IA2015==288]<- "saiha"
df_1$district_name[df_1$GEOALT_IA2015==289]<- "west tripura"
df_1$district_name[df_1$GEOALT_IA2015==290]<- "south tripura"
df_1$district_name[df_1$GEOALT_IA2015==291]<- "dhalai"
df_1$district_name[df_1$GEOALT_IA2015==292]<- "north tripura"
df_1$district_name[df_1$GEOALT_IA2015==293]<- "west garo hills"
df_1$district_name[df_1$GEOALT_IA2015==294]<- "east garo hills"
df_1$district_name[df_1$GEOALT_IA2015==295]<- "south garo hills"
df_1$district_name[df_1$GEOALT_IA2015==296]<- "west khasi hills"
df_1$district_name[df_1$GEOALT_IA2015==297]<- "ribhoi"
df_1$district_name[df_1$GEOALT_IA2015==298]<- "east khasi hills"
df_1$district_name[df_1$GEOALT_IA2015==299]<- "jaintia hills"
df_1$district_name[df_1$GEOALT_IA2015==300]<- "kokrajhar"
df_1$district_name[df_1$GEOALT_IA2015==301]<- "dhubri"
df_1$district_name[df_1$GEOALT_IA2015==302]<- "goalpara"
df_1$district_name[df_1$GEOALT_IA2015==303]<- "barpeta"
df_1$district_name[df_1$GEOALT_IA2015==304]<- "morigaon"
df_1$district_name[df_1$GEOALT_IA2015==305]<- "nagaon"
df_1$district_name[df_1$GEOALT_IA2015==306]<- "sonitpur"
df_1$district_name[df_1$GEOALT_IA2015==307]<- "lakhimpur"
df_1$district_name[df_1$GEOALT_IA2015==308]<- "dhemaji"
df_1$district_name[df_1$GEOALT_IA2015==309]<- "tinsukia"
df_1$district_name[df_1$GEOALT_IA2015==310]<- "dibrugarh"
df_1$district_name[df_1$GEOALT_IA2015==311]<- "sivsagar"
df_1$district_name[df_1$GEOALT_IA2015==312]<- "jorhat"
df_1$district_name[df_1$GEOALT_IA2015==313]<- "golaghat"
df_1$district_name[df_1$GEOALT_IA2015==314]<- "karbi anglong"
df_1$district_name[df_1$GEOALT_IA2015==315]<- "dima hasao"
df_1$district_name[df_1$GEOALT_IA2015==316]<- "cachar"
df_1$district_name[df_1$GEOALT_IA2015==317]<- "karimganj"
df_1$district_name[df_1$GEOALT_IA2015==318]<- "hailakandi"
df_1$district_name[df_1$GEOALT_IA2015==319]<- "bongaigaon"
df_1$district_name[df_1$GEOALT_IA2015==320]<- "chirang"
df_1$district_name[df_1$GEOALT_IA2015==321]<- "kamrup"
df_1$district_name[df_1$GEOALT_IA2015==322]<- "kamrup metripolitan"
df_1$district_name[df_1$GEOALT_IA2015==323]<- "nalbari"
df_1$district_name[df_1$GEOALT_IA2015==324]<- "baksa"
df_1$district_name[df_1$GEOALT_IA2015==325]<- "darrang"
df_1$district_name[df_1$GEOALT_IA2015==326]<- "udalguri"
df_1$district_name[df_1$GEOALT_IA2015==327]<- "darjiling"
df_1$district_name[df_1$GEOALT_IA2015==328]<- "jalpaiguri"
df_1$district_name[df_1$GEOALT_IA2015==329]<- "koch bihar"
df_1$district_name[df_1$GEOALT_IA2015==330]<- "uttar dinajpur"
df_1$district_name[df_1$GEOALT_IA2015==331]<- "dakshin dinajpur"
df_1$district_name[df_1$GEOALT_IA2015==332]<- "maldah"
df_1$district_name[df_1$GEOALT_IA2015==333]<- "murshidabad"
df_1$district_name[df_1$GEOALT_IA2015==334]<- "birbhum"
df_1$district_name[df_1$GEOALT_IA2015==335]<- "barddhaman"
df_1$district_name[df_1$GEOALT_IA2015==336]<- "nadia"
df_1$district_name[df_1$GEOALT_IA2015==337]<- "north twenty four parganas"
df_1$district_name[df_1$GEOALT_IA2015==338]<- "jalpaiguri"
df_1$district_name[df_1$GEOALT_IA2015==339]<- "bankura"
df_1$district_name[df_1$GEOALT_IA2015==340]<- "puruliya"
df_1$district_name[df_1$GEOALT_IA2015==341]<- "haora"
df_1$district_name[df_1$GEOALT_IA2015==342]<- "kolkata"
df_1$district_name[df_1$GEOALT_IA2015==343]<- "south twenty four parganas"
df_1$district_name[df_1$GEOALT_IA2015==344]<- "paschim mednipur"
df_1$district_name[df_1$GEOALT_IA2015==345]<- "purba mednipur"
df_1$district_name[df_1$GEOALT_IA2015==346]<- "garhwa"
df_1$district_name[df_1$GEOALT_IA2015==347]<- "chatra"
df_1$district_name[df_1$GEOALT_IA2015==348]<- "kodarma"
df_1$district_name[df_1$GEOALT_IA2015==349]<- "giridih"
df_1$district_name[df_1$GEOALT_IA2015==350]<- "deoghar"
df_1$district_name[df_1$GEOALT_IA2015==351]<- "godda"
df_1$district_name[df_1$GEOALT_IA2015==352]<- "sahibganj"
df_1$district_name[df_1$GEOALT_IA2015==353]<- "pakur"
df_1$district_name[df_1$GEOALT_IA2015==354]<- "dhanbad"
df_1$district_name[df_1$GEOALT_IA2015==355]<- "bokaro"
df_1$district_name[df_1$GEOALT_IA2015==356]<- "lohardaga"
df_1$district_name[df_1$GEOALT_IA2015==357]<- "purbi singhbhum"
df_1$district_name[df_1$GEOALT_IA2015==358]<- "palamu"
df_1$district_name[df_1$GEOALT_IA2015==359]<- "latehar"
df_1$district_name[df_1$GEOALT_IA2015==360]<- "hazaribagh"
df_1$district_name[df_1$GEOALT_IA2015==361]<- "ramgarh"
df_1$district_name[df_1$GEOALT_IA2015==362]<- "dumka"
df_1$district_name[df_1$GEOALT_IA2015==363]<- "jamtara"
df_1$district_name[df_1$GEOALT_IA2015==364]<- "ranchi"
df_1$district_name[df_1$GEOALT_IA2015==365]<- "khunti"
df_1$district_name[df_1$GEOALT_IA2015==366]<- "gumla"
df_1$district_name[df_1$GEOALT_IA2015==367]<- "simdega"
df_1$district_name[df_1$GEOALT_IA2015==370]<- "bargarh"
df_1$district_name[df_1$GEOALT_IA2015==371]<- "jharsuguda"
df_1$district_name[df_1$GEOALT_IA2015==372]<- "sambalpur"
df_1$district_name[df_1$GEOALT_IA2015==373]<- "debagarh"
df_1$district_name[df_1$GEOALT_IA2015==374]<- "sundargarh"
df_1$district_name[df_1$GEOALT_IA2015==375]<- "kendujhar"
df_1$district_name[df_1$GEOALT_IA2015==376]<- "mayurbhanj"
df_1$district_name[df_1$GEOALT_IA2015==377]<- "baleshwar"
df_1$district_name[df_1$GEOALT_IA2015==378]<- "bhadrak"
df_1$district_name[df_1$GEOALT_IA2015==379]<- "kendrapara"
df_1$district_name[df_1$GEOALT_IA2015==380]<- "jagatsinghapur"
df_1$district_name[df_1$GEOALT_IA2015==381]<- "cuttack"
df_1$district_name[df_1$GEOALT_IA2015==382]<- "jajapur"
df_1$district_name[df_1$GEOALT_IA2015==383]<- "dhenkanal"
df_1$district_name[df_1$GEOALT_IA2015==384]<- "anugul"
df_1$district_name[df_1$GEOALT_IA2015==385]<- "nayagarh"
df_1$district_name[df_1$GEOALT_IA2015==386]<- "khordha"
df_1$district_name[df_1$GEOALT_IA2015==387]<- "puri"
df_1$district_name[df_1$GEOALT_IA2015==388]<- "ganjam"
df_1$district_name[df_1$GEOALT_IA2015==389]<- "gajapati"
df_1$district_name[df_1$GEOALT_IA2015==390]<- "kandhamal"
df_1$district_name[df_1$GEOALT_IA2015==391]<- "baudh"
df_1$district_name[df_1$GEOALT_IA2015==392]<- "subarnapur"
df_1$district_name[df_1$GEOALT_IA2015==393]<- "balangir"
df_1$district_name[df_1$GEOALT_IA2015==394]<- "nuapada"
df_1$district_name[df_1$GEOALT_IA2015==395]<- "kalahandi"
df_1$district_name[df_1$GEOALT_IA2015==396]<- "rayagada"
df_1$district_name[df_1$GEOALT_IA2015==397]<- "nabarangapur"
df_1$district_name[df_1$GEOALT_IA2015==398]<- "koraput"
df_1$district_name[df_1$GEOALT_IA2015==399]<- "malkangiri"
df_1$district_name[df_1$GEOALT_IA2015==400]<- "koriya"
df_1$district_name[df_1$GEOALT_IA2015==401]<- "surguja"
df_1$district_name[df_1$GEOALT_IA2015==402]<- "jashpur"
df_1$district_name[df_1$GEOALT_IA2015==403]<- "raigarh"
df_1$district_name[df_1$GEOALT_IA2015==404]<- "korba"
df_1$district_name[df_1$GEOALT_IA2015==405]<- "janjgir-champa"
df_1$district_name[df_1$GEOALT_IA2015==407]<- "kabirdham"
df_1$district_name[df_1$GEOALT_IA2015==408]<- "rajnandgaon"
df_1$district_name[df_1$GEOALT_IA2015==409]<- "durg"
df_1$district_name[df_1$GEOALT_IA2015==410]<- "raipur"
df_1$district_name[df_1$GEOALT_IA2015==411]<- "mahasamund"
df_1$district_name[df_1$GEOALT_IA2015==412]<- "dhamtari"
df_1$district_name[df_1$GEOALT_IA2015==413]<- "uttar bastar kanker"
df_1$district_name[df_1$GEOALT_IA2015==414]<- "bastar"
df_1$district_name[df_1$GEOALT_IA2015==415]<- "narayanpur"
df_1$district_name[df_1$GEOALT_IA2015==416]<- "dakshin bastar dantewada"
df_1$district_name[df_1$GEOALT_IA2015==417]<- "bijapur"
df_1$district_name[df_1$GEOALT_IA2015==418]<- "sheopur"
df_1$district_name[df_1$GEOALT_IA2015==419]<- "morena"
df_1$district_name[df_1$GEOALT_IA2015==420]<- "bhind"
df_1$district_name[df_1$GEOALT_IA2015==421]<- "gwalior"
df_1$district_name[df_1$GEOALT_IA2015==422]<- "datia"
df_1$district_name[df_1$GEOALT_IA2015==423]<- "shivpuri"
df_1$district_name[df_1$GEOALT_IA2015==424]<- "tikamgarh"
df_1$district_name[df_1$GEOALT_IA2015==425]<- "chhatarpur"
df_1$district_name[df_1$GEOALT_IA2015==426]<- "panna"
df_1$district_name[df_1$GEOALT_IA2015==427]<- "sagar"
df_1$district_name[df_1$GEOALT_IA2015==428]<- "damoh"
df_1$district_name[df_1$GEOALT_IA2015==429]<- "satna"
df_1$district_name[df_1$GEOALT_IA2015==430]<- "rewa"
df_1$district_name[df_1$GEOALT_IA2015==431]<- "umaria"
df_1$district_name[df_1$GEOALT_IA2015==432]<- "neemuch"
df_1$district_name[df_1$GEOALT_IA2015==433]<- "mandsaur"
df_1$district_name[df_1$GEOALT_IA2015==434]<- "ratlam"
df_1$district_name[df_1$GEOALT_IA2015==435]<- "ujjain"
df_1$district_name[df_1$GEOALT_IA2015==436]<- "shajapur"
df_1$district_name[df_1$GEOALT_IA2015==437]<- "dewas"
df_1$district_name[df_1$GEOALT_IA2015==438]<- "dhar"
df_1$district_name[df_1$GEOALT_IA2015==439]<- "indore"
df_1$district_name[df_1$GEOALT_IA2015==440]<- "khargone"
df_1$district_name[df_1$GEOALT_IA2015==441]<- "barwani"
df_1$district_name[df_1$GEOALT_IA2015==442]<- "rajgarh"
df_1$district_name[df_1$GEOALT_IA2015==443]<- "vidisha"
df_1$district_name[df_1$GEOALT_IA2015==444]<- "bhopal"
df_1$district_name[df_1$GEOALT_IA2015==445]<- "sehore"
df_1$district_name[df_1$GEOALT_IA2015==446]<- "raisen"
df_1$district_name[df_1$GEOALT_IA2015==447]<- "betul"
df_1$district_name[df_1$GEOALT_IA2015==448]<- "harda"
df_1$district_name[df_1$GEOALT_IA2015==449]<- "hoshangabad"
df_1$district_name[df_1$GEOALT_IA2015==450]<- "katni"
df_1$district_name[df_1$GEOALT_IA2015==451]<- "jabalpur"
df_1$district_name[df_1$GEOALT_IA2015==452]<- "narsimhapur"
df_1$district_name[df_1$GEOALT_IA2015==453]<- "dindori"
df_1$district_name[df_1$GEOALT_IA2015==454]<- "mandla"
df_1$district_name[df_1$GEOALT_IA2015==455]<- "chhindwara"
df_1$district_name[df_1$GEOALT_IA2015==456]<- "seoni"
df_1$district_name[df_1$GEOALT_IA2015==457]<- "balaghat"
df_1$district_name[df_1$GEOALT_IA2015==458]<- "guna"
df_1$district_name[df_1$GEOALT_IA2015==459]<- "ashoknagar"
df_1$district_name[df_1$GEOALT_IA2015==460]<- "shahdol"
df_1$district_name[df_1$GEOALT_IA2015==461]<- "anuppur"
df_1$district_name[df_1$GEOALT_IA2015==462]<- "sidhi"
df_1$district_name[df_1$GEOALT_IA2015==463]<- "singrauli"
df_1$district_name[df_1$GEOALT_IA2015==464]<- "jhabua"
df_1$district_name[df_1$GEOALT_IA2015==465]<- "alirajpur"
df_1$district_name[df_1$GEOALT_IA2015==466]<- "khandwa"
df_1$district_name[df_1$GEOALT_IA2015==467]<- "burhanpur"
df_1$district_name[df_1$GEOALT_IA2015==468]<- "kachchh"
df_1$district_name[df_1$GEOALT_IA2015==469]<- "banaskantha"
df_1$district_name[df_1$GEOALT_IA2015==470]<- "patan"
df_1$district_name[df_1$GEOALT_IA2015==471]<- "mahesana"
df_1$district_name[df_1$GEOALT_IA2015==472]<- "sabarkantha"
df_1$district_name[df_1$GEOALT_IA2015==473]<- "gandhinagar"
df_1$district_name[df_1$GEOALT_IA2015==474]<- "ahmadabad"
df_1$district_name[df_1$GEOALT_IA2015==475]<- "surendranagar"
df_1$district_name[df_1$GEOALT_IA2015==476]<- "rajkot"
df_1$district_name[df_1$GEOALT_IA2015==477]<- "jamnagar"
df_1$district_name[df_1$GEOALT_IA2015==478]<- "porbandar"
df_1$district_name[df_1$GEOALT_IA2015==479]<- "junagadh"
df_1$district_name[df_1$GEOALT_IA2015==480]<- "amreli"
df_1$district_name[df_1$GEOALT_IA2015==481]<- "bhavnagar"
df_1$district_name[df_1$GEOALT_IA2015==482]<- "anand"
df_1$district_name[df_1$GEOALT_IA2015==483]<- "kheda"
df_1$district_name[df_1$GEOALT_IA2015==484]<- "panchmahal"
df_1$district_name[df_1$GEOALT_IA2015==485]<- "dohad"
df_1$district_name[df_1$GEOALT_IA2015==486]<- "vadodara"
df_1$district_name[df_1$GEOALT_IA2015==487]<- "narmada"
df_1$district_name[df_1$GEOALT_IA2015==488]<- "bharuch"
df_1$district_name[df_1$GEOALT_IA2015==489]<- "the dangs"
df_1$district_name[df_1$GEOALT_IA2015==490]<- "navsari"
df_1$district_name[df_1$GEOALT_IA2015==491]<- "valsad"
df_1$district_name[df_1$GEOALT_IA2015==492]<- "surat"
df_1$district_name[df_1$GEOALT_IA2015==493]<- "tapi"
df_1$district_name[df_1$GEOALT_IA2015==494]<- "diu"
df_1$district_name[df_1$GEOALT_IA2015==495]<- "daman"
df_1$district_name[df_1$GEOALT_IA2015==496]<- "dadra and nagar haveli"
df_1$district_name[df_1$GEOALT_IA2015==497]<- "nandurbar"
df_1$district_name[df_1$GEOALT_IA2015==498]<- "dhule"
df_1$district_name[df_1$GEOALT_IA2015==499]<- "jalgaon"
df_1$district_name[df_1$GEOALT_IA2015==500]<- "buldana"
df_1$district_name[df_1$GEOALT_IA2015==501]<- "akola"
df_1$district_name[df_1$GEOALT_IA2015==502]<- "washim"
df_1$district_name[df_1$GEOALT_IA2015==503]<- "amravati"
df_1$district_name[df_1$GEOALT_IA2015==504]<- "wardha"
df_1$district_name[df_1$GEOALT_IA2015==505]<- "nagpur"
df_1$district_name[df_1$GEOALT_IA2015==506]<- "bhandara"
df_1$district_name[df_1$GEOALT_IA2015==507]<- "gondiya"
df_1$district_name[df_1$GEOALT_IA2015==508]<- "gadchiroli"
df_1$district_name[df_1$GEOALT_IA2015==509]<- "chandrapur"
df_1$district_name[df_1$GEOALT_IA2015==510]<- "yavatmal"
df_1$district_name[df_1$GEOALT_IA2015==511]<- "nanded"
df_1$district_name[df_1$GEOALT_IA2015==512]<- "hingoli"
df_1$district_name[df_1$GEOALT_IA2015==513]<- "parbhani"
df_1$district_name[df_1$GEOALT_IA2015==514]<- "jalna"
df_1$district_name[df_1$GEOALT_IA2015==516]<- "nashik"
df_1$district_name[df_1$GEOALT_IA2015==517]<- "thane"
df_1$district_name[df_1$GEOALT_IA2015==518]<- "mumbai suburban"
df_1$district_name[df_1$GEOALT_IA2015==519]<- "mumbai"
df_1$district_name[df_1$GEOALT_IA2015==521]<- "pune"
df_1$district_name[df_1$GEOALT_IA2015==522]<- "ahmadnagar"
df_1$district_name[df_1$GEOALT_IA2015==523]<- "bid"
df_1$district_name[df_1$GEOALT_IA2015==524]<- "latur"
df_1$district_name[df_1$GEOALT_IA2015==525]<- "osmanabad"
df_1$district_name[df_1$GEOALT_IA2015==526]<- "solapur"
df_1$district_name[df_1$GEOALT_IA2015==527]<- "satara"
df_1$district_name[df_1$GEOALT_IA2015==528]<- "ratnagiri"
df_1$district_name[df_1$GEOALT_IA2015==529]<- "sindhudurg"
df_1$district_name[df_1$GEOALT_IA2015==530]<- "kolhapur"
df_1$district_name[df_1$GEOALT_IA2015==531]<- "sangli"
df_1$district_name[df_1$GEOALT_IA2015==532]<- "adilabad"
df_1$district_name[df_1$GEOALT_IA2015==533]<- "nizamabad"
df_1$district_name[df_1$GEOALT_IA2015==534]<- "karimnagar"
df_1$district_name[df_1$GEOALT_IA2015==535]<- "medak"
df_1$district_name[df_1$GEOALT_IA2015==536]<- "hyderabad"
df_1$district_name[df_1$GEOALT_IA2015==537]<- "rangareddy"
df_1$district_name[df_1$GEOALT_IA2015==538]<- "mahbubnagar"
df_1$district_name[df_1$GEOALT_IA2015==539]<- "nalgonda"
df_1$district_name[df_1$GEOALT_IA2015==540]<- "warangal"
df_1$district_name[df_1$GEOALT_IA2015==541]<- "khammam"
df_1$district_name[df_1$GEOALT_IA2015==542]<- "srikakulam"
df_1$district_name[df_1$GEOALT_IA2015==543]<- "vizianagaram"
df_1$district_name[df_1$GEOALT_IA2015==544]<- "visakhapatnam"
df_1$district_name[df_1$GEOALT_IA2015==545]<- "east godavari"
df_1$district_name[df_1$GEOALT_IA2015==546]<- "west godavari"
df_1$district_name[df_1$GEOALT_IA2015==547]<- "krishna"
df_1$district_name[df_1$GEOALT_IA2015==548]<- "guntur"
df_1$district_name[df_1$GEOALT_IA2015==549]<- "prakasam"
df_1$district_name[df_1$GEOALT_IA2015==550]<- "sri potti sriramulu nellore"
df_1$district_name[df_1$GEOALT_IA2015==551]<- "ysr"
df_1$district_name[df_1$GEOALT_IA2015==552]<- "kurnool"
df_1$district_name[df_1$GEOALT_IA2015==553]<- "anantapur"
df_1$district_name[df_1$GEOALT_IA2015==554]<- "chittoor"
df_1$district_name[df_1$GEOALT_IA2015==555]<- "belgaum"
df_1$district_name[df_1$GEOALT_IA2015==556]<- "bagalkot"
df_1$district_name[df_1$GEOALT_IA2015==558]<- "bidar"
df_1$district_name[df_1$GEOALT_IA2015==559]<- "raichur"
df_1$district_name[df_1$GEOALT_IA2015==560]<- "koppal"
df_1$district_name[df_1$GEOALT_IA2015==561]<- "gadag"
df_1$district_name[df_1$GEOALT_IA2015==562]<- "dharwad"
df_1$district_name[df_1$GEOALT_IA2015==563]<- "uttara kannada"
df_1$district_name[df_1$GEOALT_IA2015==564]<- "haveri"
df_1$district_name[df_1$GEOALT_IA2015==565]<- "bellary"
df_1$district_name[df_1$GEOALT_IA2015==566]<- "chitradurga"
df_1$district_name[df_1$GEOALT_IA2015==567]<- "davanagere"
df_1$district_name[df_1$GEOALT_IA2015==568]<- "shimoga"
df_1$district_name[df_1$GEOALT_IA2015==569]<- "udupi"
df_1$district_name[df_1$GEOALT_IA2015==570]<- "chikmagalur"
df_1$district_name[df_1$GEOALT_IA2015==571]<- "tumkur"
df_1$district_name[df_1$GEOALT_IA2015==572]<- "bangalore"
df_1$district_name[df_1$GEOALT_IA2015==573]<- "mandya"
df_1$district_name[df_1$GEOALT_IA2015==574]<- "hassan"
df_1$district_name[df_1$GEOALT_IA2015==575]<- "dakshina kannada"
df_1$district_name[df_1$GEOALT_IA2015==576]<- "kodagu"
df_1$district_name[df_1$GEOALT_IA2015==577]<- "mysore"
df_1$district_name[df_1$GEOALT_IA2015==578]<- "chamarajanagar"
df_1$district_name[df_1$GEOALT_IA2015==579]<- "gulbarga"
df_1$district_name[df_1$GEOALT_IA2015==580]<- "yadgir"
df_1$district_name[df_1$GEOALT_IA2015==581]<- "kolar"
df_1$district_name[df_1$GEOALT_IA2015==582]<- "chikkaballapura"
df_1$district_name[df_1$GEOALT_IA2015==583]<- "bangalore rural"
df_1$district_name[df_1$GEOALT_IA2015==584]<- "ramanagara"
df_1$district_name[df_1$GEOALT_IA2015==585]<- "north goa"
df_1$district_name[df_1$GEOALT_IA2015==586]<- "south goa"
df_1$district_name[df_1$GEOALT_IA2015==587]<- "lakshadweep"
df_1$district_name[df_1$GEOALT_IA2015==588]<- "kasargod"
df_1$district_name[df_1$GEOALT_IA2015==589]<- "kannur"
df_1$district_name[df_1$GEOALT_IA2015==590]<- "wayanad"
df_1$district_name[df_1$GEOALT_IA2015==591]<- "kozhikode"
df_1$district_name[df_1$GEOALT_IA2015==592]<- "malappuram"
df_1$district_name[df_1$GEOALT_IA2015==593]<- "palakkad"
df_1$district_name[df_1$GEOALT_IA2015==594]<- "thrissur"
df_1$district_name[df_1$GEOALT_IA2015==595]<- "ernakulam"
df_1$district_name[df_1$GEOALT_IA2015==596]<- "idukki"
df_1$district_name[df_1$GEOALT_IA2015==597]<- "kottayam"
df_1$district_name[df_1$GEOALT_IA2015==598]<- "alappuzha"
df_1$district_name[df_1$GEOALT_IA2015==599]<- "pathanamthitta"
df_1$district_name[df_1$GEOALT_IA2015==600]<- "kollam"
df_1$district_name[df_1$GEOALT_IA2015==601]<- "thiruvananthapuram"
df_1$district_name[df_1$GEOALT_IA2015==602]<- "thiruvallur"
df_1$district_name[df_1$GEOALT_IA2015==603]<- "chennai"
df_1$district_name[df_1$GEOALT_IA2015==604]<- "kancheepuram"
df_1$district_name[df_1$GEOALT_IA2015==605]<- "vellore"
df_1$district_name[df_1$GEOALT_IA2015==606]<- "tiruvannamalai"
df_1$district_name[df_1$GEOALT_IA2015==607]<- "viluppuram"
df_1$district_name[df_1$GEOALT_IA2015==608]<- "salem"
df_1$district_name[df_1$GEOALT_IA2015==609]<- "namakkal"
df_1$district_name[df_1$GEOALT_IA2015==610]<- "erode"
df_1$district_name[df_1$GEOALT_IA2015==611]<- "the nilgiris"
df_1$district_name[df_1$GEOALT_IA2015==612]<- "dindigul"
df_1$district_name[df_1$GEOALT_IA2015==613]<- "karur"
df_1$district_name[df_1$GEOALT_IA2015==614]<- "tiruchirappalli"
df_1$district_name[df_1$GEOALT_IA2015==615]<- "perambalur"
df_1$district_name[df_1$GEOALT_IA2015==616]<- "ariyalur"
df_1$district_name[df_1$GEOALT_IA2015==617]<- "cuddalore"
df_1$district_name[df_1$GEOALT_IA2015==618]<- "nagapattinam"
df_1$district_name[df_1$GEOALT_IA2015==619]<- "thiruvarur"
df_1$district_name[df_1$GEOALT_IA2015==620]<- "thanjavur"
df_1$district_name[df_1$GEOALT_IA2015==621]<- "pudukkottai"
df_1$district_name[df_1$GEOALT_IA2015==622]<- "sivaganga"
df_1$district_name[df_1$GEOALT_IA2015==623]<- "madurai"
df_1$district_name[df_1$GEOALT_IA2015==624]<- "theni"
df_1$district_name[df_1$GEOALT_IA2015==625]<- "virudhunagar"
df_1$district_name[df_1$GEOALT_IA2015==626]<- "ramanathapuram"
df_1$district_name[df_1$GEOALT_IA2015==627]<- "thoothukkudi"
df_1$district_name[df_1$GEOALT_IA2015==628]<- "tirunelveli"
df_1$district_name[df_1$GEOALT_IA2015==629]<- "kanniyakumari"
df_1$district_name[df_1$GEOALT_IA2015==630]<- "dharmapuri"
df_1$district_name[df_1$GEOALT_IA2015==631]<- "krishnagiri"
df_1$district_name[df_1$GEOALT_IA2015==632]<- "coimbatore"
df_1$district_name[df_1$GEOALT_IA2015==633]<- "tiruppur"
df_1$district_name[df_1$GEOALT_IA2015==634]<- "yanam"
df_1$district_name[df_1$GEOALT_IA2015==635]<- "puducherry"
df_1$district_name[df_1$GEOALT_IA2015==636]<- "mahe"
df_1$district_name[df_1$GEOALT_IA2015==637]<- "karaikal"
df_1$district_name[df_1$GEOALT_IA2015==638]<- "nicobars"
df_1$district_name[df_1$GEOALT_IA2015==639]<- "north and middle andaman"
df_1$district_name[df_1$GEOALT_IA2015==640]<- "south andaman"

################# Naming the states from the codes ####################

df_1$state_name[df_1$GEO_IA2015==2]<- "andhra pradesh"
df_1$state_name[df_1$GEO_IA2015==3]<- "arunachal pradesh"
df_1$state_name[df_1$GEO_IA2015==4]<- "assam"
df_1$state_name[df_1$GEO_IA2015==5]<- "bihar"
df_1$state_name[df_1$GEO_IA2015==7]<- "chhattisgarh"
df_1$state_name[df_1$GEO_IA2015==10]<- "goa"
df_1$state_name[df_1$GEO_IA2015==11]<- "gujarat"
df_1$state_name[df_1$GEO_IA2015==12]<- "haryana"
df_1$state_name[df_1$GEO_IA2015==13]<- "himachal pradesh"
df_1$state_name[df_1$GEO_IA2015==14]<- "jammu & kashmir"
df_1$state_name[df_1$GEO_IA2015==15]<- "jharkhand"
df_1$state_name[df_1$GEO_IA2015==16]<- "karnataka"
df_1$state_name[df_1$GEO_IA2015==17]<- "kerala"
df_1$state_name[df_1$GEO_IA2015==19]<- "madhya pradesh"
df_1$state_name[df_1$GEO_IA2015==20]<- "maharashtra"
df_1$state_name[df_1$GEO_IA2015==21]<- "manipur"
df_1$state_name[df_1$GEO_IA2015==22]<- "meghalaya"
df_1$state_name[df_1$GEO_IA2015==23]<- "mizoram"
df_1$state_name[df_1$GEO_IA2015==24]<- "nagaland"
df_1$state_name[df_1$GEO_IA2015==25]<- "delhi"
df_1$state_name[df_1$GEO_IA2015==26]<- "odisha"
df_1$state_name[df_1$GEO_IA2015==28]<- "punjab"
df_1$state_name[df_1$GEO_IA2015==29]<- "rajasthan"
df_1$state_name[df_1$GEO_IA2015==31]<- "tamil nadu"
df_1$state_name[df_1$GEO_IA2015==32]<- "tripura"
df_1$state_name[df_1$GEO_IA2015==33]<- "uttar pradesh"
df_1$state_name[df_1$GEO_IA2015==34]<- "uttarakhand"
df_1$state_name[df_1$GEO_IA2015==35]<- "west bengal"

df_1<- df_1%>%
  subset(!is.na(state_name))

####################### Variable cleaning ###########################


########### Keeping mother's with at least one birth #################

df_full<- df_1%>%
  subset(TOTBIRTHIST!=0)%>%
  subset(MAR1STYR!=9999)

################ Coding caste of the household #################

df_full$caste[df_full$ETHNICITYIA==10]<- "SC"
df_full$caste[df_full$ETHNICITYIA==20]<- "ST"
df_full$caste[df_full$ETHNICITYIA==31]<- "OBC"
df_full$caste[is.na(df_full$caste)]<- "Other"

############## Coding religion of the household ################

df_full$religion[df_full$RELIGION==1000]<- "muslim"
df_full$religion[df_full$RELIGION==4000]<- "hindu"
df_full$religion[df_full$RELIGION==2000]<- "christian"
df_full$religion[is.na(df_full$religion)]<- "other"


############### Coding wealth status of the household ###############

df_full$econ[df_full$WEALTHQ==1]<- "poorest"
df_full$econ[df_full$WEALTHQ==2]<- "poorer"
df_full$econ[df_full$WEALTHQ==3]<- "middle"
df_full$econ[df_full$WEALTHQ==4]<- "richer"
df_full$econ[df_full$WEALTHQ==5]<- "richest"


################## Coding rural-urban status ###################

df_full$rural[df_full$URBAN==1]<- "urban"
df_full$rural[df_full$URBAN==2]<- "rural"

############################## Selecting the main variables ##################################


main_data<- df_full%>%
  select(state_name,
         district_name,
         CASEID,
         HHID,
         PSU,
         STRATA,
         PERWEIGHT,
         POPWT,
         AWFACTT,
         DVWEIGHT,
         AGE,
         BIRTHMO,
         BIRTHYEAR,
         MAR1STMO,
         MAR1STYR,
         TOTBIRTHIST,
         SONSATHOME,
         SONSAWAYHOME,
         DAUSATHOME,
         DAUSAWAYHOME,
         SONSDIED,
         DAUSDIED,
         AGEAT1STBIRTH,
         HHEADSEX,
         BPLCARDHH,
         rural,
         religion,
         caste,
         econ,
         LIT2,
         EDUCLVL,
         EDYRTOTAL,
         IDEALBOYS,
         IDEALGIRLS,
         BIDX_01,
         BIDX_02,
         BIDX_03,
         BIDX_04,
         BIDX_05,
         BIDX_06,
         BIDX_07,
         BIDX_08,
         BIDX_09,
         BIDX_10,
         BIDX_11,
         BIDX_12,
         BIDX_13,
         BIDX_14,
         BIDX_15,
         KIDBORD_01,
         KIDBORD_02,
         KIDBORD_03,
         KIDBORD_04,
         KIDBORD_05,
         KIDBORD_06,
         KIDBORD_07,
         KIDBORD_08,
         KIDBORD_09,
         KIDBORD_10,
         KIDBORD_11,
         KIDBORD_12,
         KIDBORD_13,
         KIDBORD_14,
         KIDBORD_15,
         KIDBIRTHMO_01,
         KIDBIRTHMO_02,
         KIDBIRTHMO_03,
         KIDBIRTHMO_04,
         KIDBIRTHMO_05,
         KIDBIRTHMO_06,
         KIDBIRTHMO_07,
         KIDBIRTHMO_08,
         KIDBIRTHMO_09,
         KIDBIRTHMO_10,
         KIDBIRTHMO_11,
         KIDBIRTHMO_12,
         KIDBIRTHMO_13,
         KIDBIRTHMO_14,
         KIDBIRTHMO_15,
         KIDBIRTHMO_16,
         KIDBIRTHMO_17,
         KIDBIRTHYR_01,
         KIDBIRTHYR_02,
         KIDBIRTHYR_03,
         KIDBIRTHYR_04,
         KIDBIRTHYR_05,
         KIDBIRTHYR_06,
         KIDBIRTHYR_07,
         KIDBIRTHYR_08,
         KIDBIRTHYR_09,
         KIDBIRTHYR_10,
         KIDBIRTHYR_11,
         KIDBIRTHYR_12,
         KIDBIRTHYR_13,
         KIDBIRTHYR_14,
         KIDBIRTHYR_15,
         KIDSEX_01,
         KIDSEX_02,
         KIDSEX_03,
         KIDSEX_04,
         KIDSEX_05,
         KIDSEX_06,
         KIDSEX_07,
         KIDSEX_08,
         KIDSEX_09,
         KIDSEX_10,
         KIDSEX_11,
         KIDSEX_12,
         KIDSEX_13,
         KIDSEX_14,
         KIDSEX_15,
         KIDALIVE_01,
         KIDALIVE_02,
         KIDALIVE_03,
         KIDALIVE_04,
         KIDALIVE_05,
         KIDALIVE_06,
         KIDALIVE_07,
         KIDALIVE_08,
         KIDALIVE_09,
         KIDALIVE_10,
         KIDALIVE_11,
         KIDALIVE_12,
         KIDALIVE_13,
         KIDALIVE_14,
         KIDALIVE_15,
         KIDAGEDEATH_01,
         KIDAGEDEATH_02,
         KIDAGEDEATH_03,
         KIDAGEDEATH_04,
         KIDAGEDEATH_05,
         KIDAGEDEATH_06,
         KIDAGEDEATH_07,
         KIDAGEDEATH_08,
         KIDAGEDEATH_09,
         KIDAGEDEATH_10,
         KIDAGEDEATH_11,
         KIDAGEDEATH_12,
         KIDAGEDEATH_13,
         KIDAGEDEATH_14,
         KIDAGEDEATH_15)

############ Converting the survival status of each child into a binary variable ##########

main_data$KIDALIVE_02[main_data$KIDALIVE_02==9]<- NA
main_data$KIDALIVE_03[main_data$KIDALIVE_03==9]<- NA
main_data$KIDALIVE_04[main_data$KIDALIVE_04==9]<- NA
main_data$KIDALIVE_05[main_data$KIDALIVE_05==9]<- NA
main_data$KIDALIVE_06[main_data$KIDALIVE_06==9]<- NA
main_data$KIDALIVE_07[main_data$KIDALIVE_07==9]<- NA
main_data$KIDALIVE_08[main_data$KIDALIVE_08==9]<- NA
main_data$KIDALIVE_09[main_data$KIDALIVE_09==9]<- NA
main_data$KIDALIVE_10[main_data$KIDALIVE_10==9]<- NA
main_data$KIDALIVE_11[main_data$KIDALIVE_11==9]<- NA
main_data$KIDALIVE_12[main_data$KIDALIVE_12==9]<- NA
main_data$KIDALIVE_13[main_data$KIDALIVE_13==9]<- NA
main_data$KIDALIVE_14[main_data$KIDALIVE_14==9]<- NA
main_data$KIDALIVE_15[main_data$KIDALIVE_15==9]<- NA


main_data$alive_1_born<- ifelse(main_data$KIDALIVE_01==1,1,0)
main_data$alive_2_born<- ifelse(main_data$KIDALIVE_02==1,1,0)
main_data$alive_3_born<- ifelse(main_data$KIDALIVE_03==1,1,0)
main_data$alive_4_born<- ifelse(main_data$KIDALIVE_04==1,1,0)
main_data$alive_5_born<- ifelse(main_data$KIDALIVE_05==1,1,0)
main_data$alive_6_born<- ifelse(main_data$KIDALIVE_06==1,1,0)
main_data$alive_7_born<- ifelse(main_data$KIDALIVE_07==1,1,0)
main_data$alive_8_born<- ifelse(main_data$KIDALIVE_08==1,1,0)
main_data$alive_9_born<- ifelse(main_data$KIDALIVE_09==1,1,0)
main_data$alive_10_born<- ifelse(main_data$KIDALIVE_10==1,1,0)
main_data$alive_11_born<- ifelse(main_data$KIDALIVE_11==1,1,0)
main_data$alive_12_born<- ifelse(main_data$KIDALIVE_12==1,1,0)
main_data$alive_13_born<- ifelse(main_data$KIDALIVE_13==1,1,0)
main_data$alive_14_born<- ifelse(main_data$KIDALIVE_14==1,1,0)
main_data$alive_15_born<- ifelse(main_data$KIDALIVE_15==1,1,0)

############################## Neonatal/Infant mortality ###################################

main_data$KIDAGEDEATH_01[main_data$KIDAGEDEATH_01==999]<- NA
main_data$KIDAGEDEATH_02[main_data$KIDAGEDEATH_02==999]<- NA
main_data$KIDAGEDEATH_03[main_data$KIDAGEDEATH_03==999]<- NA
main_data$KIDAGEDEATH_04[main_data$KIDAGEDEATH_04==999]<- NA
main_data$KIDAGEDEATH_05[main_data$KIDAGEDEATH_05==999]<- NA
main_data$KIDAGEDEATH_06[main_data$KIDAGEDEATH_06==999]<- NA
main_data$KIDAGEDEATH_07[main_data$KIDAGEDEATH_07==999]<- NA
main_data$KIDAGEDEATH_08[main_data$KIDAGEDEATH_08==999]<- NA
main_data$KIDAGEDEATH_09[main_data$KIDAGEDEATH_09==999]<- NA
main_data$KIDAGEDEATH_10[main_data$KIDAGEDEATH_10==999]<- NA
main_data$KIDAGEDEATH_11[main_data$KIDAGEDEATH_11==999]<- NA
main_data$KIDAGEDEATH_12[main_data$KIDAGEDEATH_12==999]<- NA
main_data$KIDAGEDEATH_13[main_data$KIDAGEDEATH_13==999]<- NA
main_data$KIDAGEDEATH_14[main_data$KIDAGEDEATH_14==999]<- NA
main_data$KIDAGEDEATH_15[main_data$KIDAGEDEATH_15==999]<- NA

main_data<- main_data%>%
  mutate(neonat_1=ifelse(alive_1_born==1|KIDAGEDEATH_01>201,1,0),
         neonat_2=ifelse(alive_2_born==1|KIDAGEDEATH_02>201,1,0),
         neonat_3=ifelse(alive_3_born==1|KIDAGEDEATH_03>201,1,0),
         neonat_4=ifelse(alive_4_born==1|KIDAGEDEATH_04>201,1,0),
         neonat_5=ifelse(alive_5_born==1|KIDAGEDEATH_05>201,1,0),
         neonat_6=ifelse(alive_6_born==1|KIDAGEDEATH_06>201,1,0),
         neonat_7=ifelse(alive_7_born==1|KIDAGEDEATH_07>201,1,0),
         neonat_8=ifelse(alive_8_born==1|KIDAGEDEATH_08>201,1,0),
         neonat_9=ifelse(alive_9_born==1|KIDAGEDEATH_09>201,1,0),
         neonat_10=ifelse(alive_10_born==1|KIDAGEDEATH_10>201,1,0),
         neonat_11=ifelse(alive_11_born==1|KIDAGEDEATH_11>201,1,0),
         neonat_12=ifelse(alive_12_born==1|KIDAGEDEATH_12>201,1,0),
         neonat_13=ifelse(alive_13_born==1|KIDAGEDEATH_13>201,1,0),
         neonat_14=ifelse(alive_14_born==1|KIDAGEDEATH_14>201,1,0),
         neonat_15=ifelse(alive_15_born==1|KIDAGEDEATH_15>201,1,0),
         infant_1=ifelse(alive_1_born==1|KIDAGEDEATH_01>212,1,0),
         infant_2=ifelse(alive_2_born==1|KIDAGEDEATH_02>212,1,0),
         infant_3=ifelse(alive_3_born==1|KIDAGEDEATH_03>212,1,0),
         infant_4=ifelse(alive_4_born==1|KIDAGEDEATH_04>212,1,0),
         infant_5=ifelse(alive_5_born==1|KIDAGEDEATH_05>212,1,0),
         infant_6=ifelse(alive_6_born==1|KIDAGEDEATH_06>212,1,0),
         infant_7=ifelse(alive_7_born==1|KIDAGEDEATH_07>212,1,0),
         infant_8=ifelse(alive_8_born==1|KIDAGEDEATH_08>212,1,0),
         infant_9=ifelse(alive_9_born==1|KIDAGEDEATH_09>212,1,0),
         infant_10=ifelse(alive_10_born==1|KIDAGEDEATH_10>212,1,0),
         infant_11=ifelse(alive_11_born==1|KIDAGEDEATH_11>212,1,0),
         infant_12=ifelse(alive_12_born==1|KIDAGEDEATH_12>212,1,0),
         infant_13=ifelse(alive_13_born==1|KIDAGEDEATH_13>212,1,0),
         infant_14=ifelse(alive_14_born==1|KIDAGEDEATH_14>212,1,0),
         infant_15=ifelse(alive_15_born==1|KIDAGEDEATH_15>212,1,0))

############# Mother's age at birth for different birth orders ##############

main_data$KIDBIRTHYR_02[main_data$KIDBIRTHYR_02==9999]<- NA
main_data$KIDBIRTHYR_03[main_data$KIDBIRTHYR_03==9999]<- NA
main_data$KIDBIRTHYR_04[main_data$KIDBIRTHYR_04==9999]<- NA
main_data$KIDBIRTHYR_05[main_data$KIDBIRTHYR_05==9999]<- NA
main_data$KIDBIRTHYR_06[main_data$KIDBIRTHYR_06==9999]<- NA
main_data$KIDBIRTHYR_07[main_data$KIDBIRTHYR_07==9999]<- NA
main_data$KIDBIRTHYR_08[main_data$KIDBIRTHYR_08==9999]<- NA
main_data$KIDBIRTHYR_09[main_data$KIDBIRTHYR_09==9999]<- NA
main_data$KIDBIRTHYR_10[main_data$KIDBIRTHYR_10==9999]<- NA
main_data$KIDBIRTHYR_11[main_data$KIDBIRTHYR_11==9999]<- NA
main_data$KIDBIRTHYR_12[main_data$KIDBIRTHYR_12==9999]<- NA
main_data$KIDBIRTHYR_13[main_data$KIDBIRTHYR_13==9999]<- NA
main_data$KIDBIRTHYR_14[main_data$KIDBIRTHYR_14==9999]<- NA
main_data$KIDBIRTHYR_15[main_data$KIDBIRTHYR_15==9999]<- NA

main_data$KIDBIRTHMO_02[main_data$KIDBIRTHMO_02==99]<- NA
main_data$KIDBIRTHMO_03[main_data$KIDBIRTHMO_03==99]<- NA
main_data$KIDBIRTHMO_04[main_data$KIDBIRTHMO_04==99]<- NA
main_data$KIDBIRTHMO_05[main_data$KIDBIRTHMO_05==99]<- NA
main_data$KIDBIRTHMO_06[main_data$KIDBIRTHMO_06==99]<- NA
main_data$KIDBIRTHMO_07[main_data$KIDBIRTHMO_07==99]<- NA
main_data$KIDBIRTHMO_08[main_data$KIDBIRTHMO_08==99]<- NA
main_data$KIDBIRTHMO_09[main_data$KIDBIRTHMO_09==99]<- NA
main_data$KIDBIRTHMO_10[main_data$KIDBIRTHMO_10==99]<- NA
main_data$KIDBIRTHMO_11[main_data$KIDBIRTHMO_11==99]<- NA
main_data$KIDBIRTHMO_12[main_data$KIDBIRTHMO_12==99]<- NA
main_data$KIDBIRTHMO_13[main_data$KIDBIRTHMO_13==99]<- NA
main_data$KIDBIRTHMO_14[main_data$KIDBIRTHMO_14==99]<- NA
main_data$KIDBIRTHMO_15[main_data$KIDBIRTHMO_15==99]<- NA

main_data<- main_data%>%
  mutate(wom_age_1=KIDBIRTHYR_01-BIRTHYEAR,
         wom_age_2=KIDBIRTHYR_02-BIRTHYEAR,
         wom_age_3=KIDBIRTHYR_03-BIRTHYEAR,
         wom_age_4=KIDBIRTHYR_04-BIRTHYEAR,
         wom_age_5=KIDBIRTHYR_05-BIRTHYEAR,
         wom_age_6=KIDBIRTHYR_06-BIRTHYEAR,
         wom_age_7=KIDBIRTHYR_07-BIRTHYEAR,
         wom_age_8=KIDBIRTHYR_08-BIRTHYEAR,
         wom_age_9=KIDBIRTHYR_09-BIRTHYEAR,
         wom_age_10=KIDBIRTHYR_10-BIRTHYEAR,
         wom_age_11=KIDBIRTHYR_11-BIRTHYEAR,
         wom_age_12=KIDBIRTHYR_12-BIRTHYEAR,
         wom_age_13=KIDBIRTHYR_13-BIRTHYEAR,
         wom_age_14=KIDBIRTHYR_14-BIRTHYEAR,
         wom_age_15=KIDBIRTHYR_15-BIRTHYEAR)

############################### Redefining the sex of child variables #############################

main_data$KIDSEX_02[main_data$KIDSEX_02==9]<- NA
main_data$KIDSEX_03[main_data$KIDSEX_03==9]<- NA
main_data$KIDSEX_04[main_data$KIDSEX_04==9]<- NA
main_data$KIDSEX_05[main_data$KIDSEX_05==9]<- NA
main_data$KIDSEX_06[main_data$KIDSEX_06==9]<- NA
main_data$KIDSEX_07[main_data$KIDSEX_07==9]<- NA
main_data$KIDSEX_08[main_data$KIDSEX_08==9]<- NA
main_data$KIDSEX_09[main_data$KIDSEX_09==9]<- NA
main_data$KIDSEX_10[main_data$KIDSEX_10==9]<- NA
main_data$KIDSEX_11[main_data$KIDSEX_11==9]<- NA
main_data$KIDSEX_12[main_data$KIDSEX_12==9]<- NA
main_data$KIDSEX_13[main_data$KIDSEX_13==9]<- NA
main_data$KIDSEX_14[main_data$KIDSEX_14==9]<- NA
main_data$KIDSEX_15[main_data$KIDSEX_15==9]<- NA

main_data<- main_data%>%
  mutate(sex_1_born=ifelse(KIDSEX_01==2,1,0),
         sex_2_born=ifelse(KIDSEX_02==2,1,0),
         sex_3_born=ifelse(KIDSEX_03==2,1,0),
         sex_4_born=ifelse(KIDSEX_04==2,1,0),
         sex_5_born=ifelse(KIDSEX_05==2,1,0),
         sex_6_born=ifelse(KIDSEX_06==2,1,0),
         sex_7_born=ifelse(KIDSEX_07==2,1,0),
         sex_8_born=ifelse(KIDSEX_08==2,1,0),
         sex_9_born=ifelse(KIDSEX_09==2,1,0),
         sex_10_born=ifelse(KIDSEX_10==2,1,0),
         sex_11_born=ifelse(KIDSEX_11==2,1,0),
         sex_12_born=ifelse(KIDSEX_12==2,1,0),
         sex_13_born=ifelse(KIDSEX_13==2,1,0),
         sex_14_born=ifelse(KIDSEX_14==2,1,0),
         sex_15_born=ifelse(KIDSEX_15==2,1,0))

######################### Selecting the variables needed for constructing outcome variables ##################

df_2<- main_data%>%
  select(CASEID,
         KIDBORD_01,
         KIDBORD_02,
         KIDBORD_03,
         KIDBORD_05,
         KIDBORD_06,
         KIDBORD_07,
         KIDBORD_08,
         KIDBORD_09,
         KIDBORD_10,
         KIDBORD_11,
         KIDBORD_12,
         KIDBORD_13,
         KIDBORD_14,
         KIDBORD_15,
         alive_1_born,
         alive_2_born,
         alive_3_born,
         alive_4_born,
         alive_5_born,
         alive_6_born,
         alive_7_born,
         alive_8_born,
         alive_9_born,
         alive_10_born,
         alive_8_born,
         alive_9_born,
         alive_10_born,
         alive_11_born,
         alive_12_born,
         alive_13_born,
         alive_14_born,
         alive_15_born,
         neonat_1,
         neonat_2,
         neonat_3,
         neonat_4,
         neonat_5,
         neonat_6,
         neonat_7,
         neonat_8,
         neonat_9,
         neonat_10,
         neonat_11,
         neonat_12,
         neonat_13,
         neonat_14,
         neonat_15,
         infant_1,
         infant_2,
         infant_3,
         infant_4,
         infant_5,
         infant_6,
         infant_7,
         infant_8,
         infant_9,
         infant_10,
         infant_11,
         infant_12,
         infant_13,
         infant_14,
         infant_15,
         KIDBIRTHMO_01,
         KIDBIRTHMO_02,
         KIDBIRTHMO_03,
         KIDBIRTHMO_04,
         KIDBIRTHMO_05,
         KIDBIRTHMO_06,
         KIDBIRTHMO_07,
         KIDBIRTHMO_08,
         KIDBIRTHMO_09,
         KIDBIRTHMO_10,
         KIDBIRTHMO_11,
         KIDBIRTHMO_12,
         KIDBIRTHMO_13,
         KIDBIRTHMO_14,
         KIDBIRTHMO_15,
         KIDBIRTHYR_01,
         KIDBIRTHYR_02,
         KIDBIRTHYR_03,
         KIDBIRTHYR_04,
         KIDBIRTHYR_05,
         KIDBIRTHYR_06,
         KIDBIRTHYR_07,
         KIDBIRTHYR_08,
         KIDBIRTHYR_09,
         KIDBIRTHYR_10,
         KIDBIRTHYR_11,
         KIDBIRTHYR_12,
         KIDBIRTHYR_13,
         KIDBIRTHYR_14,
         KIDBIRTHYR_15,
         sex_1_born,
         sex_2_born,
         sex_3_born,
         sex_4_born,
         sex_5_born,
         sex_6_born,
         sex_7_born,
         sex_8_born,
         sex_9_born,
         sex_10_born,
         sex_11_born,
         sex_12_born,
         sex_13_born,
         sex_14_born,
         sex_15_born,
         wom_age_1,
         wom_age_2,
         wom_age_3,
         wom_age_4,
         wom_age_5,
         wom_age_6,
         wom_age_7,
         wom_age_8,
         wom_age_9,
         wom_age_10,
         wom_age_11,
         wom_age_12,
         wom_age_13,
         wom_age_14,
         wom_age_15,
         MAR1STYR,
         MAR1STMO,
         TOTBIRTHIST)

################## Latest order births ###########################

df_2_1<- df_2%>%
  select(CASEID,KIDBIRTHYR_01)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_01"))%>%
  mutate(cohort=KIDBIRTHYR_01)%>%
  select(CASEID,cohort)

df_2_2<- df_2%>%
  select(CASEID,sex_1_born)%>%
  melt(id.vars=c("CASEID","sex_1_born"))%>%
  mutate(sex=sex_1_born)%>%
  select(CASEID,sex)

df_2_3<- df_2%>%
  select(CASEID,alive_1_born)%>%
  melt(id.vars=c("CASEID","alive_1_born"))%>%
  mutate(alive=alive_1_born)%>%
  select(CASEID,alive)

df_2_4<- df_2%>%
  select(CASEID,neonat_1)%>%
  melt(id.vars=c("CASEID","neonat_1"))%>%
  mutate(neonat=neonat_1)%>%
  select(CASEID,neonat)

df_2_5<- df_2%>%
  select(CASEID,infant_1)%>%
  melt(id.vars=c("CASEID","infant_1"))%>%
  mutate(infant=infant_1)%>%
  select(CASEID,infant)

df_2_6<- df_2%>%
  select(CASEID,wom_age_1)%>%
  melt(id.vars=c("CASEID","wom_age_1"))%>%
  mutate(mothers_age_at_birth=wom_age_1)%>%
  select(CASEID,mothers_age_at_birth)

df_2_7<- df_2%>%
  select(CASEID,KIDBIRTHYR_01,KIDBIRTHMO_01,KIDBIRTHYR_02,KIDBIRTHMO_02)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_01","KIDBIRTHMO_01","KIDBIRTHYR_02","KIDBIRTHMO_02"))%>%
  mutate(wait_time=(KIDBIRTHYR_01*12+KIDBIRTHMO_01)-(KIDBIRTHYR_02*12+KIDBIRTHMO_02))%>%
  select(CASEID,wait_time)

df_2_8<- df_2%>%
  select(CASEID,TOTBIRTHIST)%>%
  melt(id.vars=c("CASEID","TOTBIRTHIST"))%>%
  mutate(birth_order=TOTBIRTHIST)%>%
  select(CASEID,birth_order)

df_latest_order<- merge(df_2_1,df_2_2[, c("CASEID", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="CASEID")

df_latest_order<- merge(df_latest_order,df_2_3[, c("CASEID", setdiff(colnames(df_2_3),colnames(df_latest_order)))], by="CASEID")

df_latest_order<- merge(df_latest_order,df_2_4[, c("CASEID", setdiff(colnames(df_2_4),colnames(df_latest_order)))], by="CASEID")

df_latest_order<- merge(df_latest_order,df_2_5[, c("CASEID", setdiff(colnames(df_2_5),colnames(df_latest_order)))], by="CASEID")

df_latest_order<- merge(df_latest_order,df_2_6[, c("CASEID", setdiff(colnames(df_2_6),colnames(df_latest_order)))], by="CASEID")

df_latest_order<- merge(df_latest_order,df_2_7[, c("CASEID", setdiff(colnames(df_2_7),colnames(df_latest_order)))], by="CASEID")

df_latest_order<- merge(df_latest_order,df_2_8[, c("CASEID", setdiff(colnames(df_2_8),colnames(df_latest_order)))], by="CASEID")

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8)

################## Second latest order births ###########################

df_2_1<- df_2%>%
  select(CASEID,KIDBIRTHYR_02)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_02"))%>%
  mutate(cohort=KIDBIRTHYR_02)%>%
  select(CASEID,cohort)

df_2_2<- df_2%>%
  select(CASEID,sex_2_born)%>%
  melt(id.vars=c("CASEID","sex_2_born"))%>%
  mutate(sex=sex_2_born)%>%
  select(CASEID,sex)

df_2_3<- df_2%>%
  select(CASEID,alive_2_born)%>%
  melt(id.vars=c("CASEID","alive_2_born"))%>%
  mutate(alive=alive_2_born)%>%
  select(CASEID,alive)

df_2_4<- df_2%>%
  select(CASEID,neonat_2)%>%
  melt(id.vars=c("CASEID","neonat_2"))%>%
  mutate(neonat=neonat_2)%>%
  select(CASEID,neonat)

df_2_5<- df_2%>%
  select(CASEID,infant_2)%>%
  melt(id.vars=c("CASEID","infant_2"))%>%
  mutate(infant=infant_2)%>%
  select(CASEID,infant)

df_2_6<- df_2%>%
  select(CASEID,wom_age_2)%>%
  melt(id.vars=c("CASEID","wom_age_2"))%>%
  mutate(mothers_age_at_birth=wom_age_2)%>%
  select(CASEID,mothers_age_at_birth)

df_2_7<- df_2%>%
  select(CASEID,KIDBIRTHYR_02,KIDBIRTHMO_02,KIDBIRTHYR_03,KIDBIRTHMO_03)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_02","KIDBIRTHMO_02","KIDBIRTHYR_03","KIDBIRTHMO_03"))%>%
  mutate(wait_time=(KIDBIRTHYR_02*12+KIDBIRTHMO_02)-(KIDBIRTHYR_03*12+KIDBIRTHMO_03))%>%
  select(CASEID,wait_time)

df_2_8<- df_2%>%
  select(CASEID,TOTBIRTHIST)%>%
  melt(id.vars=c("CASEID","TOTBIRTHIST"))%>%
  mutate(birth_order=TOTBIRTHIST-1)%>%
  select(CASEID,birth_order)

df_2_latest_order<- merge(df_2_1,df_2_2[, c("CASEID", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="CASEID")

df_2_latest_order<- merge(df_2_latest_order,df_2_3[, c("CASEID", setdiff(colnames(df_2_3),colnames(df_2_latest_order)))], by="CASEID")

df_2_latest_order<- merge(df_2_latest_order,df_2_4[, c("CASEID", setdiff(colnames(df_2_4),colnames(df_2_latest_order)))], by="CASEID")

df_2_latest_order<- merge(df_2_latest_order,df_2_5[, c("CASEID", setdiff(colnames(df_2_5),colnames(df_2_latest_order)))], by="CASEID")

df_2_latest_order<- merge(df_2_latest_order,df_2_6[, c("CASEID", setdiff(colnames(df_2_6),colnames(df_2_latest_order)))], by="CASEID")

df_2_latest_order<- merge(df_2_latest_order,df_2_7[, c("CASEID", setdiff(colnames(df_2_7),colnames(df_2_latest_order)))], by="CASEID")

df_2_latest_order<- merge(df_2_latest_order,df_2_8[, c("CASEID", setdiff(colnames(df_2_8),colnames(df_2_latest_order)))], by="CASEID")

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8)

df_2_latest_order<- df_2_latest_order%>%
  subset(birth_order>0)

################## Third latest order births ###########################

df_2_1<- df_2%>%
  select(CASEID,KIDBIRTHYR_03)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_03"))%>%
  mutate(cohort=KIDBIRTHYR_03)%>%
  select(CASEID,cohort)

df_2_2<- df_2%>%
  select(CASEID,sex_3_born)%>%
  melt(id.vars=c("CASEID","sex_3_born"))%>%
  mutate(sex=sex_3_born)%>%
  select(CASEID,sex)

df_2_3<- df_2%>%
  select(CASEID,alive_3_born)%>%
  melt(id.vars=c("CASEID","alive_3_born"))%>%
  mutate(alive=alive_3_born)%>%
  select(CASEID,alive)

df_2_4<- df_2%>%
  select(CASEID,neonat_3)%>%
  melt(id.vars=c("CASEID","neonat_3"))%>%
  mutate(neonat=neonat_3)%>%
  select(CASEID,neonat)

df_2_5<- df_2%>%
  select(CASEID,infant_3)%>%
  melt(id.vars=c("CASEID","infant_3"))%>%
  mutate(infant=infant_3)%>%
  select(CASEID,infant)

df_2_6<- df_2%>%
  select(CASEID,wom_age_3)%>%
  melt(id.vars=c("CASEID","wom_age_3"))%>%
  mutate(mothers_age_at_birth=wom_age_3)%>%
  select(CASEID,mothers_age_at_birth)

df_2_7<- df_2%>%
  select(CASEID,KIDBIRTHYR_04,KIDBIRTHMO_04,KIDBIRTHYR_03,KIDBIRTHMO_03)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_04","KIDBIRTHMO_04","KIDBIRTHYR_03","KIDBIRTHMO_03"))%>%
  mutate(wait_time=(KIDBIRTHYR_03*12+KIDBIRTHMO_03)-(KIDBIRTHYR_04*12+KIDBIRTHMO_04))%>%
  select(CASEID,wait_time)

df_2_8<- df_2%>%
  select(CASEID,TOTBIRTHIST)%>%
  melt(id.vars=c("CASEID","TOTBIRTHIST"))%>%
  mutate(birth_order=TOTBIRTHIST-2)%>%
  select(CASEID,birth_order)

df_3_latest_order<- merge(df_2_1,df_2_2[, c("CASEID", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="CASEID")

df_3_latest_order<- merge(df_3_latest_order,df_2_3[, c("CASEID", setdiff(colnames(df_2_3),colnames(df_3_latest_order)))], by="CASEID")

df_3_latest_order<- merge(df_3_latest_order,df_2_4[, c("CASEID", setdiff(colnames(df_2_4),colnames(df_3_latest_order)))], by="CASEID")

df_3_latest_order<- merge(df_3_latest_order,df_2_5[, c("CASEID", setdiff(colnames(df_2_5),colnames(df_3_latest_order)))], by="CASEID")

df_3_latest_order<- merge(df_3_latest_order,df_2_6[, c("CASEID", setdiff(colnames(df_2_6),colnames(df_3_latest_order)))], by="CASEID")

df_3_latest_order<- merge(df_3_latest_order,df_2_7[, c("CASEID", setdiff(colnames(df_2_7),colnames(df_3_latest_order)))], by="CASEID")

df_3_latest_order<- merge(df_3_latest_order,df_2_8[, c("CASEID", setdiff(colnames(df_2_8),colnames(df_3_latest_order)))], by="CASEID")

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8)

df_3_latest_order<- df_3_latest_order%>%
  subset(birth_order!=0)


################## Fourth latest order births ###########################

df_2_1<- df_2%>%
  select(CASEID,KIDBIRTHYR_04)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_04"))%>%
  mutate(cohort=KIDBIRTHYR_04)%>%
  select(CASEID,cohort)

df_2_2<- df_2%>%
  select(CASEID,sex_4_born)%>%
  melt(id.vars=c("CASEID","sex_4_born"))%>%
  mutate(sex=sex_4_born)%>%
  select(CASEID,sex)

df_2_3<- df_2%>%
  select(CASEID,alive_4_born)%>%
  melt(id.vars=c("CASEID","alive_4_born"))%>%
  mutate(alive=alive_4_born)%>%
  select(CASEID,alive)

df_2_4<- df_2%>%
  select(CASEID,neonat_4)%>%
  melt(id.vars=c("CASEID","neonat_4"))%>%
  mutate(neonat=neonat_4)%>%
  select(CASEID,neonat)

df_2_5<- df_2%>%
  select(CASEID,infant_4)%>%
  melt(id.vars=c("CASEID","infant_4"))%>%
  mutate(infant=infant_4)%>%
  select(CASEID,infant)

df_2_6<- df_2%>%
  select(CASEID,wom_age_4)%>%
  melt(id.vars=c("CASEID","wom_age_4"))%>%
  mutate(mothers_age_at_birth=wom_age_4)%>%
  select(CASEID,mothers_age_at_birth)

df_2_7<- df_2%>%
  select(CASEID,KIDBIRTHYR_04,KIDBIRTHMO_04,KIDBIRTHYR_05,KIDBIRTHMO_05)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_04","KIDBIRTHMO_04","KIDBIRTHYR_05","KIDBIRTHMO_05"))%>%
  mutate(wait_time=(KIDBIRTHYR_04*12+KIDBIRTHMO_04)-(KIDBIRTHYR_05*12+KIDBIRTHMO_05))%>%
  select(CASEID,wait_time)

df_2_8<- df_2%>%
  select(CASEID,TOTBIRTHIST)%>%
  melt(id.vars=c("CASEID","TOTBIRTHIST"))%>%
  mutate(birth_order=TOTBIRTHIST-3)%>%
  select(CASEID,birth_order)

df_4_latest_order<- merge(df_2_1,df_2_2[, c("CASEID", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="CASEID")

df_4_latest_order<- merge(df_4_latest_order,df_2_3[, c("CASEID", setdiff(colnames(df_2_3),colnames(df_4_latest_order)))], by="CASEID")

df_4_latest_order<- merge(df_4_latest_order,df_2_4[, c("CASEID", setdiff(colnames(df_2_4),colnames(df_4_latest_order)))], by="CASEID")

df_4_latest_order<- merge(df_4_latest_order,df_2_5[, c("CASEID", setdiff(colnames(df_2_5),colnames(df_4_latest_order)))], by="CASEID")

df_4_latest_order<- merge(df_4_latest_order,df_2_6[, c("CASEID", setdiff(colnames(df_2_6),colnames(df_4_latest_order)))], by="CASEID")

df_4_latest_order<- merge(df_4_latest_order,df_2_7[, c("CASEID", setdiff(colnames(df_2_7),colnames(df_4_latest_order)))], by="CASEID")

df_4_latest_order<- merge(df_4_latest_order,df_2_8[, c("CASEID", setdiff(colnames(df_2_8),colnames(df_4_latest_order)))], by="CASEID")

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8)

df_4_latest_order<- df_4_latest_order%>%
  subset(birth_order>0)

################## Fifth latest order births ###########################

df_2_1<- df_2%>%
  select(CASEID,KIDBIRTHYR_05)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_05"))%>%
  mutate(cohort=KIDBIRTHYR_05)%>%
  select(CASEID,cohort)

df_2_2<- df_2%>%
  select(CASEID,sex_5_born)%>%
  melt(id.vars=c("CASEID","sex_5_born"))%>%
  mutate(sex=sex_5_born)%>%
  select(CASEID,sex)

df_2_3<- df_2%>%
  select(CASEID,alive_5_born)%>%
  melt(id.vars=c("CASEID","alive_5_born"))%>%
  mutate(alive=alive_5_born)%>%
  select(CASEID,alive)

df_2_4<- df_2%>%
  select(CASEID,neonat_5)%>%
  melt(id.vars=c("CASEID","neonat_5"))%>%
  mutate(neonat=neonat_5)%>%
  select(CASEID,neonat)

df_2_5<- df_2%>%
  select(CASEID,infant_5)%>%
  melt(id.vars=c("CASEID","infant_5"))%>%
  mutate(infant=infant_5)%>%
  select(CASEID,infant)

df_2_6<- df_2%>%
  select(CASEID,wom_age_5)%>%
  melt(id.vars=c("CASEID","wom_age_5"))%>%
  mutate(mothers_age_at_birth=wom_age_5)%>%
  select(CASEID,mothers_age_at_birth)

df_2_7<- df_2%>%
  select(CASEID,KIDBIRTHYR_06,KIDBIRTHMO_06,KIDBIRTHYR_05,KIDBIRTHMO_05)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_06","KIDBIRTHMO_06","KIDBIRTHYR_05","KIDBIRTHMO_05"))%>%
  mutate(wait_time=(KIDBIRTHYR_05*12+KIDBIRTHMO_05)-(KIDBIRTHYR_06*12+KIDBIRTHMO_06))%>%
  select(CASEID,wait_time)

df_2_8<- df_2%>%
  select(CASEID,TOTBIRTHIST)%>%
  melt(id.vars=c("CASEID","TOTBIRTHIST"))%>%
  mutate(birth_order=TOTBIRTHIST-4)%>%
  select(CASEID,birth_order)

df_5_latest_order<- merge(df_2_1,df_2_2[, c("CASEID", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="CASEID")

df_5_latest_order<- merge(df_5_latest_order,df_2_3[, c("CASEID", setdiff(colnames(df_2_3),colnames(df_5_latest_order)))], by="CASEID")

df_5_latest_order<- merge(df_5_latest_order,df_2_4[, c("CASEID", setdiff(colnames(df_2_4),colnames(df_5_latest_order)))], by="CASEID")

df_5_latest_order<- merge(df_5_latest_order,df_2_5[, c("CASEID", setdiff(colnames(df_2_5),colnames(df_5_latest_order)))], by="CASEID")

df_5_latest_order<- merge(df_5_latest_order,df_2_6[, c("CASEID", setdiff(colnames(df_2_6),colnames(df_5_latest_order)))], by="CASEID")

df_5_latest_order<- merge(df_5_latest_order,df_2_7[, c("CASEID", setdiff(colnames(df_2_7),colnames(df_5_latest_order)))], by="CASEID")

df_5_latest_order<- merge(df_5_latest_order,df_2_8[, c("CASEID", setdiff(colnames(df_2_8),colnames(df_5_latest_order)))], by="CASEID")

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8)

df_5_latest_order<- df_5_latest_order%>%
  subset(birth_order>0)


################## Sixth latest order births ###########################

df_2_1<- df_2%>%
  select(CASEID,KIDBIRTHYR_06)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_06"))%>%
  mutate(cohort=KIDBIRTHYR_06)%>%
  select(CASEID,cohort)

df_2_2<- df_2%>%
  select(CASEID,sex_6_born)%>%
  melt(id.vars=c("CASEID","sex_6_born"))%>%
  mutate(sex=sex_6_born)%>%
  select(CASEID,sex)

df_2_3<- df_2%>%
  select(CASEID,alive_6_born)%>%
  melt(id.vars=c("CASEID","alive_6_born"))%>%
  mutate(alive=alive_6_born)%>%
  select(CASEID,alive)

df_2_4<- df_2%>%
  select(CASEID,neonat_6)%>%
  melt(id.vars=c("CASEID","neonat_6"))%>%
  mutate(neonat=neonat_6)%>%
  select(CASEID,neonat)

df_2_5<- df_2%>%
  select(CASEID,infant_6)%>%
  melt(id.vars=c("CASEID","infant_6"))%>%
  mutate(infant=infant_6)%>%
  select(CASEID,infant)

df_2_6<- df_2%>%
  select(CASEID,wom_age_6)%>%
  melt(id.vars=c("CASEID","wom_age_6"))%>%
  mutate(mothers_age_at_birth=wom_age_6)%>%
  select(CASEID,mothers_age_at_birth)

df_2_7<- df_2%>%
  select(CASEID,KIDBIRTHYR_06,KIDBIRTHMO_06,KIDBIRTHYR_07,KIDBIRTHMO_07)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_06","KIDBIRTHMO_06","KIDBIRTHYR_07","KIDBIRTHMO_07"))%>%
  mutate(wait_time=(KIDBIRTHYR_06*12+KIDBIRTHMO_06)-(KIDBIRTHYR_07*12+KIDBIRTHMO_07))%>%
  select(CASEID,wait_time)

df_2_8<- df_2%>%
  select(CASEID,TOTBIRTHIST)%>%
  melt(id.vars=c("CASEID","TOTBIRTHIST"))%>%
  mutate(birth_order=TOTBIRTHIST-5)%>%
  select(CASEID,birth_order)

df_6_latest_order<- merge(df_2_1,df_2_2[, c("CASEID", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="CASEID")

df_6_latest_order<- merge(df_6_latest_order,df_2_3[, c("CASEID", setdiff(colnames(df_2_3),colnames(df_6_latest_order)))], by="CASEID")

df_6_latest_order<- merge(df_6_latest_order,df_2_4[, c("CASEID", setdiff(colnames(df_2_4),colnames(df_6_latest_order)))], by="CASEID")

df_6_latest_order<- merge(df_6_latest_order,df_2_5[, c("CASEID", setdiff(colnames(df_2_5),colnames(df_6_latest_order)))], by="CASEID")

df_6_latest_order<- merge(df_6_latest_order,df_2_6[, c("CASEID", setdiff(colnames(df_2_6),colnames(df_6_latest_order)))], by="CASEID")

df_6_latest_order<- merge(df_6_latest_order,df_2_7[, c("CASEID", setdiff(colnames(df_2_7),colnames(df_6_latest_order)))], by="CASEID")

df_6_latest_order<- merge(df_6_latest_order,df_2_8[, c("CASEID", setdiff(colnames(df_2_8),colnames(df_6_latest_order)))], by="CASEID")

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8)

df_6_latest_order<- df_6_latest_order%>%
  subset(birth_order>0)

################## Seventh latest order births ###########################

df_2_1<- df_2%>%
  select(CASEID,KIDBIRTHYR_07)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_07"))%>%
  mutate(cohort=KIDBIRTHYR_07)%>%
  select(CASEID,cohort)

df_2_2<- df_2%>%
  select(CASEID,sex_7_born)%>%
  melt(id.vars=c("CASEID","sex_7_born"))%>%
  mutate(sex=sex_7_born)%>%
  select(CASEID,sex)

df_2_3<- df_2%>%
  select(CASEID,alive_7_born)%>%
  melt(id.vars=c("CASEID","alive_7_born"))%>%
  mutate(alive=alive_7_born)%>%
  select(CASEID,alive)

df_2_4<- df_2%>%
  select(CASEID,neonat_7)%>%
  melt(id.vars=c("CASEID","neonat_7"))%>%
  mutate(neonat=neonat_7)%>%
  select(CASEID,neonat)

df_2_5<- df_2%>%
  select(CASEID,infant_7)%>%
  melt(id.vars=c("CASEID","infant_7"))%>%
  mutate(infant=infant_7)%>%
  select(CASEID,infant)

df_2_6<- df_2%>%
  select(CASEID,wom_age_7)%>%
  melt(id.vars=c("CASEID","wom_age_7"))%>%
  mutate(mothers_age_at_birth=wom_age_7)%>%
  select(CASEID,mothers_age_at_birth)

df_2_7<- df_2%>%
  select(CASEID,KIDBIRTHYR_08,KIDBIRTHMO_08,KIDBIRTHYR_07,KIDBIRTHMO_07)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_08","KIDBIRTHMO_08","KIDBIRTHYR_07","KIDBIRTHMO_07"))%>%
  mutate(wait_time=(KIDBIRTHYR_07*12+KIDBIRTHMO_07)-(KIDBIRTHYR_08*12+KIDBIRTHMO_08))%>%
  select(CASEID,wait_time)

df_2_8<- df_2%>%
  select(CASEID,TOTBIRTHIST)%>%
  melt(id.vars=c("CASEID","TOTBIRTHIST"))%>%
  mutate(birth_order=TOTBIRTHIST-6)%>%
  select(CASEID,birth_order)

df_7_latest_order<- merge(df_2_1,df_2_2[, c("CASEID", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="CASEID")

df_7_latest_order<- merge(df_7_latest_order,df_2_3[, c("CASEID", setdiff(colnames(df_2_3),colnames(df_7_latest_order)))], by="CASEID")

df_7_latest_order<- merge(df_7_latest_order,df_2_4[, c("CASEID", setdiff(colnames(df_2_4),colnames(df_7_latest_order)))], by="CASEID")

df_7_latest_order<- merge(df_7_latest_order,df_2_5[, c("CASEID", setdiff(colnames(df_2_5),colnames(df_7_latest_order)))], by="CASEID")

df_7_latest_order<- merge(df_7_latest_order,df_2_6[, c("CASEID", setdiff(colnames(df_2_6),colnames(df_7_latest_order)))], by="CASEID")

df_7_latest_order<- merge(df_7_latest_order,df_2_7[, c("CASEID", setdiff(colnames(df_2_7),colnames(df_7_latest_order)))], by="CASEID")

df_7_latest_order<- merge(df_7_latest_order,df_2_8[, c("CASEID", setdiff(colnames(df_2_8),colnames(df_7_latest_order)))], by="CASEID")

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8)

df_7_latest_order<- df_7_latest_order%>%
  subset(birth_order>0)

################## Eighth latest order births ###########################

df_2_1<- df_2%>%
  select(CASEID,KIDBIRTHYR_08)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_08"))%>%
  mutate(cohort=KIDBIRTHYR_08)%>%
  select(CASEID,cohort)

df_2_2<- df_2%>%
  select(CASEID,sex_8_born)%>%
  melt(id.vars=c("CASEID","sex_8_born"))%>%
  mutate(sex=sex_8_born)%>%
  select(CASEID,sex)

df_2_3<- df_2%>%
  select(CASEID,alive_8_born)%>%
  melt(id.vars=c("CASEID","alive_8_born"))%>%
  mutate(alive=alive_8_born)%>%
  select(CASEID,alive)

df_2_4<- df_2%>%
  select(CASEID,neonat_8)%>%
  melt(id.vars=c("CASEID","neonat_8"))%>%
  mutate(neonat=neonat_8)%>%
  select(CASEID,neonat)

df_2_5<- df_2%>%
  select(CASEID,infant_8)%>%
  melt(id.vars=c("CASEID","infant_8"))%>%
  mutate(infant=infant_8)%>%
  select(CASEID,infant)

df_2_6<- df_2%>%
  select(CASEID,wom_age_8)%>%
  melt(id.vars=c("CASEID","wom_age_8"))%>%
  mutate(mothers_age_at_birth=wom_age_8)%>%
  select(CASEID,mothers_age_at_birth)

df_2_7<- df_2%>%
  select(CASEID,KIDBIRTHYR_08,KIDBIRTHMO_08,KIDBIRTHYR_09,KIDBIRTHMO_09)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_08","KIDBIRTHMO_08","KIDBIRTHYR_09","KIDBIRTHMO_09"))%>%
  mutate(wait_time=(KIDBIRTHYR_08*12+KIDBIRTHMO_08)-(KIDBIRTHYR_09*12+KIDBIRTHMO_09))%>%
  select(CASEID,wait_time)

df_2_8<- df_2%>%
  select(CASEID,TOTBIRTHIST)%>%
  melt(id.vars=c("CASEID","TOTBIRTHIST"))%>%
  mutate(birth_order=TOTBIRTHIST-7)%>%
  select(CASEID,birth_order)

df_8_latest_order<- merge(df_2_1,df_2_2[, c("CASEID", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="CASEID")

df_8_latest_order<- merge(df_8_latest_order,df_2_3[, c("CASEID", setdiff(colnames(df_2_3),colnames(df_8_latest_order)))], by="CASEID")

df_8_latest_order<- merge(df_8_latest_order,df_2_4[, c("CASEID", setdiff(colnames(df_2_4),colnames(df_8_latest_order)))], by="CASEID")

df_8_latest_order<- merge(df_8_latest_order,df_2_5[, c("CASEID", setdiff(colnames(df_2_5),colnames(df_8_latest_order)))], by="CASEID")

df_8_latest_order<- merge(df_8_latest_order,df_2_6[, c("CASEID", setdiff(colnames(df_2_6),colnames(df_8_latest_order)))], by="CASEID")

df_8_latest_order<- merge(df_8_latest_order,df_2_7[, c("CASEID", setdiff(colnames(df_2_7),colnames(df_8_latest_order)))], by="CASEID")

df_8_latest_order<- merge(df_8_latest_order,df_2_8[, c("CASEID", setdiff(colnames(df_2_8),colnames(df_8_latest_order)))], by="CASEID")

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8)

df_8_latest_order<- df_8_latest_order%>%
  subset(birth_order>0)


################## Ninth latest order births ###########################

df_2_1<- df_2%>%
  select(CASEID,KIDBIRTHYR_09)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_09"))%>%
  mutate(cohort=KIDBIRTHYR_09)%>%
  select(CASEID,cohort)

df_2_2<- df_2%>%
  select(CASEID,sex_9_born)%>%
  melt(id.vars=c("CASEID","sex_9_born"))%>%
  mutate(sex=sex_9_born)%>%
  select(CASEID,sex)

df_2_3<- df_2%>%
  select(CASEID,alive_9_born)%>%
  melt(id.vars=c("CASEID","alive_9_born"))%>%
  mutate(alive=alive_9_born)%>%
  select(CASEID,alive)

df_2_4<- df_2%>%
  select(CASEID,neonat_9)%>%
  melt(id.vars=c("CASEID","neonat_9"))%>%
  mutate(neonat=neonat_9)%>%
  select(CASEID,neonat)

df_2_5<- df_2%>%
  select(CASEID,infant_9)%>%
  melt(id.vars=c("CASEID","infant_9"))%>%
  mutate(infant=infant_9)%>%
  select(CASEID,infant)

df_2_6<- df_2%>%
  select(CASEID,wom_age_9)%>%
  melt(id.vars=c("CASEID","wom_age_9"))%>%
  mutate(mothers_age_at_birth=wom_age_9)%>%
  select(CASEID,mothers_age_at_birth)

df_2_7<- df_2%>%
  select(CASEID,KIDBIRTHYR_10,KIDBIRTHMO_10,KIDBIRTHYR_09,KIDBIRTHMO_09)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_10","KIDBIRTHMO_10","KIDBIRTHYR_09","KIDBIRTHMO_09"))%>%
  mutate(wait_time=(KIDBIRTHYR_09*12+KIDBIRTHMO_09)-(KIDBIRTHYR_10*12+KIDBIRTHMO_10))%>%
  select(CASEID,wait_time)

df_2_8<- df_2%>%
  select(CASEID,TOTBIRTHIST)%>%
  melt(id.vars=c("CASEID","TOTBIRTHIST"))%>%
  mutate(birth_order=TOTBIRTHIST-8)%>%
  select(CASEID,birth_order)

df_9_latest_order<- merge(df_2_1,df_2_2[, c("CASEID", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="CASEID")

df_9_latest_order<- merge(df_9_latest_order,df_2_3[, c("CASEID", setdiff(colnames(df_2_3),colnames(df_9_latest_order)))], by="CASEID")

df_9_latest_order<- merge(df_9_latest_order,df_2_4[, c("CASEID", setdiff(colnames(df_2_4),colnames(df_9_latest_order)))], by="CASEID")

df_9_latest_order<- merge(df_9_latest_order,df_2_5[, c("CASEID", setdiff(colnames(df_2_5),colnames(df_9_latest_order)))], by="CASEID")

df_9_latest_order<- merge(df_9_latest_order,df_2_6[, c("CASEID", setdiff(colnames(df_2_6),colnames(df_9_latest_order)))], by="CASEID")

df_9_latest_order<- merge(df_9_latest_order,df_2_7[, c("CASEID", setdiff(colnames(df_2_7),colnames(df_9_latest_order)))], by="CASEID")

df_9_latest_order<- merge(df_9_latest_order,df_2_8[, c("CASEID", setdiff(colnames(df_2_8),colnames(df_9_latest_order)))], by="CASEID")

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8)

df_9_latest_order<- df_9_latest_order%>%
  subset(birth_order>0)

################## Tenth latest order births ###########################

df_2_1<- df_2%>%
  select(CASEID,KIDBIRTHYR_10)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_10"))%>%
  mutate(cohort=KIDBIRTHYR_10)%>%
  select(CASEID,cohort)

df_2_2<- df_2%>%
  select(CASEID,sex_10_born)%>%
  melt(id.vars=c("CASEID","sex_10_born"))%>%
  mutate(sex=sex_10_born)%>%
  select(CASEID,sex)

df_2_3<- df_2%>%
  select(CASEID,alive_10_born)%>%
  melt(id.vars=c("CASEID","alive_10_born"))%>%
  mutate(alive=alive_10_born)%>%
  select(CASEID,alive)

df_2_4<- df_2%>%
  select(CASEID,neonat_10)%>%
  melt(id.vars=c("CASEID","neonat_10"))%>%
  mutate(neonat=neonat_10)%>%
  select(CASEID,neonat)

df_2_5<- df_2%>%
  select(CASEID,infant_10)%>%
  melt(id.vars=c("CASEID","infant_10"))%>%
  mutate(infant=infant_10)%>%
  select(CASEID,infant)

df_2_6<- df_2%>%
  select(CASEID,wom_age_10)%>%
  melt(id.vars=c("CASEID","wom_age_10"))%>%
  mutate(mothers_age_at_birth=wom_age_10)%>%
  select(CASEID,mothers_age_at_birth)

df_2_7<- df_2%>%
  select(CASEID,KIDBIRTHYR_10,KIDBIRTHMO_10,KIDBIRTHYR_11,KIDBIRTHMO_11)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_10","KIDBIRTHMO_10","KIDBIRTHYR_11","KIDBIRTHMO_11"))%>%
  mutate(wait_time=(KIDBIRTHYR_10*12+KIDBIRTHMO_10)-(KIDBIRTHYR_11*12+KIDBIRTHMO_11))%>%
  select(CASEID,wait_time)

df_2_8<- df_2%>%
  select(CASEID,TOTBIRTHIST)%>%
  melt(id.vars=c("CASEID","TOTBIRTHIST"))%>%
  mutate(birth_order=TOTBIRTHIST-9)%>%
  select(CASEID,birth_order)

df_10_latest_order<- merge(df_2_1,df_2_2[, c("CASEID", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="CASEID")

df_10_latest_order<- merge(df_10_latest_order,df_2_3[, c("CASEID", setdiff(colnames(df_2_3),colnames(df_10_latest_order)))], by="CASEID")

df_10_latest_order<- merge(df_10_latest_order,df_2_4[, c("CASEID", setdiff(colnames(df_2_4),colnames(df_10_latest_order)))], by="CASEID")

df_10_latest_order<- merge(df_10_latest_order,df_2_5[, c("CASEID", setdiff(colnames(df_2_5),colnames(df_10_latest_order)))], by="CASEID")

df_10_latest_order<- merge(df_10_latest_order,df_2_6[, c("CASEID", setdiff(colnames(df_2_6),colnames(df_10_latest_order)))], by="CASEID")

df_10_latest_order<- merge(df_10_latest_order,df_2_7[, c("CASEID", setdiff(colnames(df_2_7),colnames(df_10_latest_order)))], by="CASEID")

df_10_latest_order<- merge(df_10_latest_order,df_2_8[, c("CASEID", setdiff(colnames(df_2_8),colnames(df_10_latest_order)))], by="CASEID")

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8)

df_10_latest_order<- df_10_latest_order%>%
  subset(birth_order>0)


################## Eleventh latest order births ###########################

df_2_1<- df_2%>%
  select(CASEID,KIDBIRTHYR_11)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_11"))%>%
  mutate(cohort=KIDBIRTHYR_11)%>%
  select(CASEID,cohort)

df_2_2<- df_2%>%
  select(CASEID,sex_11_born)%>%
  melt(id.vars=c("CASEID","sex_11_born"))%>%
  mutate(sex=sex_11_born)%>%
  select(CASEID,sex)

df_2_3<- df_2%>%
  select(CASEID,alive_11_born)%>%
  melt(id.vars=c("CASEID","alive_11_born"))%>%
  mutate(alive=alive_11_born)%>%
  select(CASEID,alive)

df_2_4<- df_2%>%
  select(CASEID,neonat_11)%>%
  melt(id.vars=c("CASEID","neonat_11"))%>%
  mutate(neonat=neonat_11)%>%
  select(CASEID,neonat)

df_2_5<- df_2%>%
  select(CASEID,infant_11)%>%
  melt(id.vars=c("CASEID","infant_11"))%>%
  mutate(infant=infant_11)%>%
  select(CASEID,infant)

df_2_6<- df_2%>%
  select(CASEID,wom_age_11)%>%
  melt(id.vars=c("CASEID","wom_age_11"))%>%
  mutate(mothers_age_at_birth=wom_age_11)%>%
  select(CASEID,mothers_age_at_birth)

df_2_7<- df_2%>%
  select(CASEID,KIDBIRTHYR_12,KIDBIRTHMO_12,KIDBIRTHYR_11,KIDBIRTHMO_11)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_12","KIDBIRTHMO_12","KIDBIRTHYR_11","KIDBIRTHMO_11"))%>%
  mutate(wait_time=(KIDBIRTHYR_11*12+KIDBIRTHMO_11)-(KIDBIRTHYR_12*12+KIDBIRTHMO_12))%>%
  select(CASEID,wait_time)

df_2_8<- df_2%>%
  select(CASEID,TOTBIRTHIST)%>%
  melt(id.vars=c("CASEID","TOTBIRTHIST"))%>%
  mutate(birth_order=TOTBIRTHIST-10)%>%
  select(CASEID,birth_order)

df_11_latest_order<- merge(df_2_1,df_2_2[, c("CASEID", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="CASEID")

df_11_latest_order<- merge(df_11_latest_order,df_2_3[, c("CASEID", setdiff(colnames(df_2_3),colnames(df_11_latest_order)))], by="CASEID")

df_11_latest_order<- merge(df_11_latest_order,df_2_4[, c("CASEID", setdiff(colnames(df_2_4),colnames(df_11_latest_order)))], by="CASEID")

df_11_latest_order<- merge(df_11_latest_order,df_2_5[, c("CASEID", setdiff(colnames(df_2_5),colnames(df_11_latest_order)))], by="CASEID")

df_11_latest_order<- merge(df_11_latest_order,df_2_6[, c("CASEID", setdiff(colnames(df_2_6),colnames(df_11_latest_order)))], by="CASEID")

df_11_latest_order<- merge(df_11_latest_order,df_2_7[, c("CASEID", setdiff(colnames(df_2_7),colnames(df_11_latest_order)))], by="CASEID")

df_11_latest_order<- merge(df_11_latest_order,df_2_8[, c("CASEID", setdiff(colnames(df_2_8),colnames(df_11_latest_order)))], by="CASEID")

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8)

df_11_latest_order<- df_11_latest_order%>%
  subset(birth_order>0)


################## Twelveth latest order births ###########################

df_2_1<- df_2%>%
  select(CASEID,KIDBIRTHYR_12)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_12"))%>%
  mutate(cohort=KIDBIRTHYR_12)%>%
  select(CASEID,cohort)

df_2_2<- df_2%>%
  select(CASEID,sex_12_born)%>%
  melt(id.vars=c("CASEID","sex_12_born"))%>%
  mutate(sex=sex_12_born)%>%
  select(CASEID,sex)

df_2_3<- df_2%>%
  select(CASEID,alive_12_born)%>%
  melt(id.vars=c("CASEID","alive_12_born"))%>%
  mutate(alive=alive_12_born)%>%
  select(CASEID,alive)

df_2_4<- df_2%>%
  select(CASEID,neonat_12)%>%
  melt(id.vars=c("CASEID","neonat_12"))%>%
  mutate(neonat=neonat_12)%>%
  select(CASEID,neonat)

df_2_5<- df_2%>%
  select(CASEID,infant_12)%>%
  melt(id.vars=c("CASEID","infant_12"))%>%
  mutate(infant=infant_12)%>%
  select(CASEID,infant)

df_2_6<- df_2%>%
  select(CASEID,wom_age_12)%>%
  melt(id.vars=c("CASEID","wom_age_12"))%>%
  mutate(mothers_age_at_birth=wom_age_12)%>%
  select(CASEID,mothers_age_at_birth)

df_2_7<- df_2%>%
  select(CASEID,KIDBIRTHYR_12,KIDBIRTHMO_12,KIDBIRTHYR_13,KIDBIRTHMO_13)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_12","KIDBIRTHMO_12","KIDBIRTHYR_13","KIDBIRTHMO_13"))%>%
  mutate(wait_time=(KIDBIRTHYR_12*12+KIDBIRTHMO_12)-(KIDBIRTHYR_13*12+KIDBIRTHMO_13))%>%
  select(CASEID,wait_time)

df_2_8<- df_2%>%
  select(CASEID,TOTBIRTHIST)%>%
  melt(id.vars=c("CASEID","TOTBIRTHIST"))%>%
  mutate(birth_order=TOTBIRTHIST-11)%>%
  select(CASEID,birth_order)

df_12_latest_order<- merge(df_2_1,df_2_2[, c("CASEID", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="CASEID")

df_12_latest_order<- merge(df_12_latest_order,df_2_3[, c("CASEID", setdiff(colnames(df_2_3),colnames(df_12_latest_order)))], by="CASEID")

df_12_latest_order<- merge(df_12_latest_order,df_2_4[, c("CASEID", setdiff(colnames(df_2_4),colnames(df_12_latest_order)))], by="CASEID")

df_12_latest_order<- merge(df_12_latest_order,df_2_5[, c("CASEID", setdiff(colnames(df_2_5),colnames(df_12_latest_order)))], by="CASEID")

df_12_latest_order<- merge(df_12_latest_order,df_2_6[, c("CASEID", setdiff(colnames(df_2_6),colnames(df_12_latest_order)))], by="CASEID")

df_12_latest_order<- merge(df_12_latest_order,df_2_7[, c("CASEID", setdiff(colnames(df_2_7),colnames(df_12_latest_order)))], by="CASEID")

df_12_latest_order<- merge(df_12_latest_order,df_2_8[, c("CASEID", setdiff(colnames(df_2_8),colnames(df_12_latest_order)))], by="CASEID")

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8)

df_12_latest_order<- df_12_latest_order%>%
  subset(birth_order>0)


################## Thirteenth latest order births ###########################

df_2_1<- df_2%>%
  select(CASEID,KIDBIRTHYR_13)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_13"))%>%
  mutate(cohort=KIDBIRTHYR_13)%>%
  select(CASEID,cohort)

df_2_2<- df_2%>%
  select(CASEID,sex_13_born)%>%
  melt(id.vars=c("CASEID","sex_13_born"))%>%
  mutate(sex=sex_13_born)%>%
  select(CASEID,sex)

df_2_3<- df_2%>%
  select(CASEID,alive_13_born)%>%
  melt(id.vars=c("CASEID","alive_13_born"))%>%
  mutate(alive=alive_13_born)%>%
  select(CASEID,alive)

df_2_4<- df_2%>%
  select(CASEID,neonat_13)%>%
  melt(id.vars=c("CASEID","neonat_13"))%>%
  mutate(neonat=neonat_13)%>%
  select(CASEID,neonat)

df_2_5<- df_2%>%
  select(CASEID,infant_13)%>%
  melt(id.vars=c("CASEID","infant_13"))%>%
  mutate(infant=infant_13)%>%
  select(CASEID,infant)

df_2_6<- df_2%>%
  select(CASEID,wom_age_13)%>%
  melt(id.vars=c("CASEID","wom_age_13"))%>%
  mutate(mothers_age_at_birth=wom_age_13)%>%
  select(CASEID,mothers_age_at_birth)

df_2_7<- df_2%>%
  select(CASEID,KIDBIRTHYR_14,KIDBIRTHMO_14,KIDBIRTHYR_13,KIDBIRTHMO_13)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_14","KIDBIRTHMO_14","KIDBIRTHYR_13","KIDBIRTHMO_13"))%>%
  mutate(wait_time=(KIDBIRTHYR_13*12+KIDBIRTHMO_13)-(KIDBIRTHYR_14*12+KIDBIRTHMO_14))%>%
  select(CASEID,wait_time)

df_2_8<- df_2%>%
  select(CASEID,TOTBIRTHIST)%>%
  melt(id.vars=c("CASEID","TOTBIRTHIST"))%>%
  mutate(birth_order=TOTBIRTHIST-12)%>%
  select(CASEID,birth_order)

df_13_latest_order<- merge(df_2_1,df_2_2[, c("CASEID", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="CASEID")

df_13_latest_order<- merge(df_13_latest_order,df_2_3[, c("CASEID", setdiff(colnames(df_2_3),colnames(df_13_latest_order)))], by="CASEID")

df_13_latest_order<- merge(df_13_latest_order,df_2_4[, c("CASEID", setdiff(colnames(df_2_4),colnames(df_13_latest_order)))], by="CASEID")

df_13_latest_order<- merge(df_13_latest_order,df_2_5[, c("CASEID", setdiff(colnames(df_2_5),colnames(df_13_latest_order)))], by="CASEID")

df_13_latest_order<- merge(df_13_latest_order,df_2_6[, c("CASEID", setdiff(colnames(df_2_6),colnames(df_13_latest_order)))], by="CASEID")

df_13_latest_order<- merge(df_13_latest_order,df_2_7[, c("CASEID", setdiff(colnames(df_2_7),colnames(df_13_latest_order)))], by="CASEID")

df_13_latest_order<- merge(df_13_latest_order,df_2_8[, c("CASEID", setdiff(colnames(df_2_8),colnames(df_13_latest_order)))], by="CASEID")

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8)

df_13_latest_order<- df_13_latest_order%>%
  subset(birth_order>0)

################## Fourteenth latest order births ###########################

df_2_1<- df_2%>%
  select(CASEID,KIDBIRTHYR_14)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_14"))%>%
  mutate(cohort=KIDBIRTHYR_14)%>%
  select(CASEID,cohort)

df_2_2<- df_2%>%
  select(CASEID,sex_14_born)%>%
  melt(id.vars=c("CASEID","sex_14_born"))%>%
  mutate(sex=sex_14_born)%>%
  select(CASEID,sex)

df_2_3<- df_2%>%
  select(CASEID,alive_14_born)%>%
  melt(id.vars=c("CASEID","alive_14_born"))%>%
  mutate(alive=alive_14_born)%>%
  select(CASEID,alive)

df_2_4<- df_2%>%
  select(CASEID,neonat_14)%>%
  melt(id.vars=c("CASEID","neonat_14"))%>%
  mutate(neonat=neonat_14)%>%
  select(CASEID,neonat)

df_2_5<- df_2%>%
  select(CASEID,infant_14)%>%
  melt(id.vars=c("CASEID","infant_14"))%>%
  mutate(infant=infant_14)%>%
  select(CASEID,infant)

df_2_6<- df_2%>%
  select(CASEID,wom_age_14)%>%
  melt(id.vars=c("CASEID","wom_age_14"))%>%
  mutate(mothers_age_at_birth=wom_age_14)%>%
  select(CASEID,mothers_age_at_birth)

df_2_7<- df_2%>%
  select(CASEID,KIDBIRTHYR_14,KIDBIRTHMO_14,KIDBIRTHYR_15,KIDBIRTHMO_15)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_14","KIDBIRTHMO_14","KIDBIRTHYR_15","KIDBIRTHMO_15"))%>%
  mutate(wait_time=(KIDBIRTHYR_14*12+KIDBIRTHMO_14)-(KIDBIRTHYR_15*12+KIDBIRTHMO_15))%>%
  select(CASEID,wait_time)

df_2_8<- df_2%>%
  select(CASEID,TOTBIRTHIST)%>%
  melt(id.vars=c("CASEID","TOTBIRTHIST"))%>%
  mutate(birth_order=TOTBIRTHIST-13)%>%
  select(CASEID,birth_order)

df_14_latest_order<- merge(df_2_1,df_2_2[, c("CASEID", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="CASEID")

df_14_latest_order<- merge(df_14_latest_order,df_2_3[, c("CASEID", setdiff(colnames(df_2_3),colnames(df_14_latest_order)))], by="CASEID")

df_14_latest_order<- merge(df_14_latest_order,df_2_4[, c("CASEID", setdiff(colnames(df_2_4),colnames(df_14_latest_order)))], by="CASEID")

df_14_latest_order<- merge(df_14_latest_order,df_2_5[, c("CASEID", setdiff(colnames(df_2_5),colnames(df_14_latest_order)))], by="CASEID")

df_14_latest_order<- merge(df_14_latest_order,df_2_6[, c("CASEID", setdiff(colnames(df_2_6),colnames(df_14_latest_order)))], by="CASEID")

df_14_latest_order<- merge(df_14_latest_order,df_2_7[, c("CASEID", setdiff(colnames(df_2_7),colnames(df_14_latest_order)))], by="CASEID")

df_14_latest_order<- merge(df_14_latest_order,df_2_8[, c("CASEID", setdiff(colnames(df_2_8),colnames(df_14_latest_order)))], by="CASEID")

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8)

df_14_latest_order<- df_14_latest_order%>%
  subset(birth_order>0)

################## Fifteenth latest order births ###########################

df_2_1<- df_2%>%
  select(CASEID,KIDBIRTHYR_15)%>%
  melt(id.vars=c("CASEID","KIDBIRTHYR_15"))%>%
  mutate(cohort=KIDBIRTHYR_15)%>%
  select(CASEID,cohort)

df_2_2<- df_2%>%
  select(CASEID,sex_15_born)%>%
  melt(id.vars=c("CASEID","sex_15_born"))%>%
  mutate(sex=sex_15_born)%>%
  select(CASEID,sex)

df_2_3<- df_2%>%
  select(CASEID,alive_15_born)%>%
  melt(id.vars=c("CASEID","alive_15_born"))%>%
  mutate(alive=alive_15_born)%>%
  select(CASEID,alive)

df_2_4<- df_2%>%
  select(CASEID,neonat_15)%>%
  melt(id.vars=c("CASEID","neonat_15"))%>%
  mutate(neonat=neonat_15)%>%
  select(CASEID,neonat)

df_2_5<- df_2%>%
  select(CASEID,infant_15)%>%
  melt(id.vars=c("CASEID","infant_15"))%>%
  mutate(infant=infant_15)%>%
  select(CASEID,infant)

df_2_6<- df_2%>%
  select(CASEID,wom_age_15)%>%
  melt(id.vars=c("CASEID","wom_age_15"))%>%
  mutate(mothers_age_at_birth=wom_age_15)%>%
  select(CASEID,mothers_age_at_birth)

df_2_7<- df_2%>%
  select(CASEID,MAR1STYR,MAR1STMO,KIDBIRTHYR_15,KIDBIRTHMO_15)%>%
  melt(id.vars=c("CASEID","MAR1STYR","MAR1STMO","KIDBIRTHYR_15","KIDBIRTHMO_15"))%>%
  mutate(wait_time=(KIDBIRTHYR_15*12+KIDBIRTHMO_15)-(MAR1STYR*12+MAR1STMO))%>%
  select(CASEID,wait_time)

df_2_8<- df_2%>%
  select(CASEID,TOTBIRTHIST)%>%
  melt(id.vars=c("CASEID","TOTBIRTHIST"))%>%
  mutate(birth_order=TOTBIRTHIST-14)%>%
  select(CASEID,birth_order)

df_15_latest_order<- merge(df_2_1,df_2_2[, c("CASEID", setdiff(colnames(df_2_2),colnames(df_2_1)))], by="CASEID")

df_15_latest_order<- merge(df_15_latest_order,df_2_3[, c("CASEID", setdiff(colnames(df_2_3),colnames(df_15_latest_order)))], by="CASEID")

df_15_latest_order<- merge(df_15_latest_order,df_2_4[, c("CASEID", setdiff(colnames(df_2_4),colnames(df_15_latest_order)))], by="CASEID")

df_15_latest_order<- merge(df_15_latest_order,df_2_5[, c("CASEID", setdiff(colnames(df_2_5),colnames(df_15_latest_order)))], by="CASEID")

df_15_latest_order<- merge(df_15_latest_order,df_2_6[, c("CASEID", setdiff(colnames(df_2_6),colnames(df_15_latest_order)))], by="CASEID")

df_15_latest_order<- merge(df_15_latest_order,df_2_7[, c("CASEID", setdiff(colnames(df_2_7),colnames(df_15_latest_order)))], by="CASEID")

df_15_latest_order<- merge(df_15_latest_order,df_2_8[, c("CASEID", setdiff(colnames(df_2_8),colnames(df_15_latest_order)))], by="CASEID")

rm(df_2_1,
   df_2_2,
   df_2_3,
   df_2_4,
   df_2_5,
   df_2_6,
   df_2_7,
   df_2_8)

df_15_latest_order<- df_15_latest_order%>%
  subset(birth_order>0)


df_panel<- rbind(df_latest_order,
                 df_2_latest_order,
                 df_3_latest_order,
                 df_4_latest_order,
                 df_5_latest_order,
                 df_6_latest_order,
                 df_7_latest_order,
                 df_8_latest_order,
                 df_9_latest_order,
                 df_10_latest_order,
                 df_11_latest_order,
                 df_12_latest_order,
                 df_13_latest_order,
                 df_14_latest_order,
                 df_15_latest_order)

df_panel<- df_panel[order(df_panel$CASEID,df_panel$cohort),]

df_panel<- df_panel%>%
  subset(cohort>=1990)


df_controls<- df_full%>%
  select(CASEID,
         state_name,
         district_name,
         PERWEIGHT,
         POPWT,
         AWFACTT,
         rural,
         religion,
         caste,
         econ,
         EDUCLVL,
         EDYRTOTAL,
         BPLCARDHH,
         AGE,
         AGEFRSTMAR,
         AGEAT1STBIRTH,
         MAR1STYR,
         MAR1STMO)

df_panel<- merge(df_panel,df_controls[, c("CASEID", setdiff(colnames(df_controls),colnames(df_panel)))], by="CASEID")

df_panel$wait_time[is.na(df_panel$wait_time)]<- (df_panel$AGEAT1STBIRTH-df_panel$AGEFRSTMAR)*12

############################# Matching the cohort years with the election years for each state separately ###############################

df_panel$election_year<- 0

################### Andhra Pradesh ################

df_panel$election_year[df_panel$state_name=="andhra pradesh" & df_panel$cohort==1990]<- 1989
df_panel$election_year[df_panel$state_name=="andhra pradesh" & df_panel$cohort==1995]<- 1994
df_panel$election_year[df_panel$state_name=="andhra pradesh" & df_panel$cohort==2000]<- 1999
df_panel$election_year[df_panel$state_name=="andhra pradesh" & df_panel$cohort==2005]<- 2004
df_panel$election_year[df_panel$state_name=="andhra pradesh" & df_panel$cohort==2010]<- 2009
df_panel$election_year[df_panel$state_name=="andhra pradesh" & df_panel$cohort==2015]<- 2014

############### Arunachal Pradesh ###################

df_panel$election_year[df_panel$state_name=="arunachal pradesh" & df_panel$cohort==1991]<- 1990
df_panel$election_year[df_panel$state_name=="arunachal pradesh" & df_panel$cohort==1996]<- 1995
df_panel$election_year[df_panel$state_name=="arunachal pradesh" & df_panel$cohort==2000]<- 1999
df_panel$election_year[df_panel$state_name=="arunachal pradesh" & df_panel$cohort==2005]<- 2004
df_panel$election_year[df_panel$state_name=="arunachal pradesh" & df_panel$cohort==2010]<- 2009
df_panel$election_year[df_panel$state_name=="arunachal pradesh" & df_panel$cohort==2015]<- 2014

##################### Assam ###########################

df_panel$election_year[df_panel$state_name=="assam" & df_panel$cohort==1992]<- 1991
df_panel$election_year[df_panel$state_name=="assam" & df_panel$cohort==1997]<- 1996
df_panel$election_year[df_panel$state_name=="assam" & df_panel$cohort==2002]<- 2001
df_panel$election_year[df_panel$state_name=="assam" & df_panel$cohort==2007]<- 2006
df_panel$election_year[df_panel$state_name=="assam" & df_panel$cohort==2012]<- 2011

###################### Bihar ###################

df_panel$election_year[df_panel$state_name=="bihar" & df_panel$cohort==1991]<- 1990
df_panel$election_year[df_panel$state_name=="bihar" & df_panel$cohort==1996]<- 1995
df_panel$election_year[df_panel$state_name=="bihar" & df_panel$cohort==2001]<- 2000
df_panel$election_year[df_panel$state_name=="bihar" & df_panel$cohort==2006]<- 2005
df_panel$election_year[df_panel$state_name=="bihar" & df_panel$cohort==2011]<- 2010

##################### Chhattisgarh ##################################

df_panel$election_year[df_panel$state_name=="chhattisgarh" & df_panel$cohort==2004]<- 2003
df_panel$election_year[df_panel$state_name=="chhattisgarh" & df_panel$cohort==2009]<- 2008
df_panel$election_year[df_panel$state_name=="chhattisgarh" & df_panel$cohort==2014]<- 2013

###################### Delhi #########################

df_panel$election_year[df_panel$state_name=="delhi" & df_panel$cohort==1994]<- 1993
df_panel$election_year[df_panel$state_name=="delhi" & df_panel$cohort==1999]<- 1998
df_panel$election_year[df_panel$state_name=="delhi" & df_panel$cohort==2004]<- 2003
df_panel$election_year[df_panel$state_name=="delhi" & df_panel$cohort==2009]<- 2008

###################### Goa ########################

df_panel$election_year[df_panel$state_name=="goa" & df_panel$cohort==1990]<- 1989
df_panel$election_year[df_panel$state_name=="goa" & df_panel$cohort==1995]<- 1994
df_panel$election_year[df_panel$state_name=="goa" & df_panel$cohort==2000]<- 1999
df_panel$election_year[df_panel$state_name=="goa" & df_panel$cohort==2003]<- 2002
df_panel$election_year[df_panel$state_name=="goa" & df_panel$cohort==2008]<- 2007
df_panel$election_year[df_panel$state_name=="goa" & df_panel$cohort==2013]<- 2012

##################### Gujarat #########################

df_panel$election_year[df_panel$state_name=="gujarat" & df_panel$cohort==1991]<- 1990
df_panel$election_year[df_panel$state_name=="gujarat" & df_panel$cohort==1996]<- 1995
df_panel$election_year[df_panel$state_name=="gujarat" & df_panel$cohort==1999]<- 1998
df_panel$election_year[df_panel$state_name=="gujarat" & df_panel$cohort==2003]<- 2002
df_panel$election_year[df_panel$state_name=="gujarat" & df_panel$cohort==2008]<- 2007
df_panel$election_year[df_panel$state_name=="gujarat" & df_panel$cohort==2013]<- 2012

#################### Haryana ###########################

df_panel$election_year[df_panel$state_name=="haryana" & df_panel$cohort==1992]<- 1991
df_panel$election_year[df_panel$state_name=="haryana" & df_panel$cohort==1997]<- 1996
df_panel$election_year[df_panel$state_name=="haryana" & df_panel$cohort==2001]<- 2000
df_panel$election_year[df_panel$state_name=="haryana" & df_panel$cohort==2006]<- 2005
df_panel$election_year[df_panel$state_name=="haryana" & df_panel$cohort==2010]<- 2009
df_panel$election_year[df_panel$state_name=="haryana" & df_panel$cohort==2015]<- 2014

########################## Himachal Pradesh ##################

df_panel$election_year[df_panel$state_name=="himachal pradesh" & df_panel$cohort==1991]<- 1990
df_panel$election_year[df_panel$state_name=="himachal pradesh" & df_panel$cohort==1994]<- 1993
df_panel$election_year[df_panel$state_name=="himachal pradesh" & df_panel$cohort==1999]<- 1998
df_panel$election_year[df_panel$state_name=="himachal pradesh" & df_panel$cohort==2004]<- 2003
df_panel$election_year[df_panel$state_name=="himachal pradesh" & df_panel$cohort==2008]<- 2007
df_panel$election_year[df_panel$state_name=="himachal pradesh" & df_panel$cohort==2013]<- 2012

############################# Jammu and Kashmir ####################

df_panel$election_year[df_panel$state_name=="jammu & kashmir" & df_panel$cohort==1997]<- 1996
df_panel$election_year[df_panel$state_name=="jammu & kashmir" & df_panel$cohort==2003]<- 2002
df_panel$election_year[df_panel$state_name=="jammu & kashmir" & df_panel$cohort==2009]<- 2008
df_panel$election_year[df_panel$state_name=="jammu & kashmir" & df_panel$cohort==2015]<- 2014

##################### Jharkhand ##################################

df_panel$election_year[df_panel$state_name=="jharkhand" & df_panel$cohort==2006]<- 2005
df_panel$election_year[df_panel$state_name=="jharkhand" & df_panel$cohort==2010]<- 2009
df_panel$election_year[df_panel$state_name=="jharkhand" & df_panel$cohort>2014]<- 2014

################################## Karnataka ###########################

df_panel$election_year[df_panel$state_name=="karnataka" & df_panel$cohort==1990]<- 1989
df_panel$election_year[df_panel$state_name=="karnataka" & df_panel$cohort==1995]<- 1994
df_panel$election_year[df_panel$state_name=="karnataka" & df_panel$cohort==2000]<- 1999
df_panel$election_year[df_panel$state_name=="karnataka" & df_panel$cohort==2005]<- 2004
df_panel$election_year[df_panel$state_name=="karnataka" & df_panel$cohort==2009]<- 2008
df_panel$election_year[df_panel$state_name=="karnataka" & df_panel$cohort==2014]<- 2013

############################## Kerala ##############################

df_panel$election_year[df_panel$state_name=="kerala" & df_panel$cohort==1992]<- 1991
df_panel$election_year[df_panel$state_name=="kerala" & df_panel$cohort==1997]<- 1996
df_panel$election_year[df_panel$state_name=="kerala" & df_panel$cohort==2002]<- 2001
df_panel$election_year[df_panel$state_name=="kerala" & df_panel$cohort==2007]<- 2006
df_panel$election_year[df_panel$state_name=="kerala" & df_panel$cohort==2012]<- 2011

############################## Madhya Pradesh #######################

df_panel$election_year[df_panel$state_name=="madhya pradesh" & df_panel$cohort==1991]<- 1990
df_panel$election_year[df_panel$state_name=="madhya pradesh" & df_panel$cohort==1994]<- 1993
df_panel$election_year[df_panel$state_name=="madhya pradesh" & df_panel$cohort==1999]<- 1998
df_panel$election_year[df_panel$state_name=="madhya pradesh" & df_panel$cohort==2004]<- 2003
df_panel$election_year[df_panel$state_name=="madhya pradesh" & df_panel$cohort==2009]<- 2008
df_panel$election_year[df_panel$state_name=="madhya pradesh" & df_panel$cohort==2014]<- 2013

################################ Maharashtra #########################

df_panel$election_year[df_panel$state_name=="maharashtra" & df_panel$cohort==1991]<- 1990
df_panel$election_year[df_panel$state_name=="maharashtra" & df_panel$cohort==1996]<- 1995
df_panel$election_year[df_panel$state_name=="maharashtra" & df_panel$cohort==2000]<- 1999
df_panel$election_year[df_panel$state_name=="maharashtra" & df_panel$cohort==2005]<- 2004
df_panel$election_year[df_panel$state_name=="maharashtra" & df_panel$cohort==2010]<- 2009
df_panel$election_year[df_panel$state_name=="maharashtra" & df_panel$cohort==2015]<- 2014


################################ Manipur ##############################

df_panel$election_year[df_panel$state_name=="manipur" & df_panel$cohort==1991]<- 1990
df_panel$election_year[df_panel$state_name=="manipur" & df_panel$cohort==1996]<- 1995
df_panel$election_year[df_panel$state_name=="manipur" & df_panel$cohort==2001]<- 2000
df_panel$election_year[df_panel$state_name=="manipur" & df_panel$cohort==2003]<- 2002
df_panel$election_year[df_panel$state_name=="manipur" & df_panel$cohort==2008]<- 2007
df_panel$election_year[df_panel$state_name=="manipur" & df_panel$cohort==2013]<- 2012

########################## Meghalaya ################################

df_panel$election_year[df_panel$state_name=="meghalaya" & df_panel$cohort==1993]<- 1993
df_panel$election_year[df_panel$state_name=="meghalaya" & df_panel$cohort==1999]<- 1998
df_panel$election_year[df_panel$state_name=="meghalaya" & df_panel$cohort==2004]<- 2003
df_panel$election_year[df_panel$state_name=="meghalaya" & df_panel$cohort==2009]<- 2008
df_panel$election_year[df_panel$state_name=="meghalaya" & df_panel$cohort==2014]<- 2013

########################## Mizoram ##############################

df_panel$election_year[df_panel$state_name=="mizoram" & df_panel$cohort==1990]<- 1989
df_panel$election_year[df_panel$state_name=="mizoram" & df_panel$cohort==1994]<- 1993
df_panel$election_year[df_panel$state_name=="mizoram" & df_panel$cohort==2009]<- 2008
df_panel$election_year[df_panel$state_name=="mizoram" & df_panel$cohort==2014]<- 2013

############################## Nagaland ############################

df_panel$election_year[df_panel$state_name=="nagaland" & df_panel$cohort==2004]<- 2003
df_panel$election_year[df_panel$state_name=="mizoram" & df_panel$cohort==2009]<- 2008

################################# Odisha ###########################

df_panel$election_year[df_panel$state_name=="odisha" & df_panel$cohort==1991]<- 1990
df_panel$election_year[df_panel$state_name=="odisha" & df_panel$cohort==1996]<- 1995
df_panel$election_year[df_panel$state_name=="odisha" & df_panel$cohort==2001]<- 2000
df_panel$election_year[df_panel$state_name=="odisha" & df_panel$cohort==2005]<- 2004
df_panel$election_year[df_panel$state_name=="odisha" & df_panel$cohort==2010]<- 2009
df_panel$election_year[df_panel$state_name=="odisha" & df_panel$cohort==2015]<- 2014

############################## Punjab ###############################

df_panel$election_year[df_panel$state_name=="punjab" & df_panel$cohort==1993]<- 1992
df_panel$election_year[df_panel$state_name=="punjab" & df_panel$cohort==1998]<- 1997
df_panel$election_year[df_panel$state_name=="punjab" & df_panel$cohort==2003]<- 2002
df_panel$election_year[df_panel$state_name=="punjab" & df_panel$cohort==2008]<- 2007
df_panel$election_year[df_panel$state_name=="punjab" & df_panel$cohort==2013]<- 2012

############################# Rajasthan ###############################

df_panel$election_year[df_panel$state_name=="rajasthan" & df_panel$cohort==1991]<- 1990
df_panel$election_year[df_panel$state_name=="rajasthan" & df_panel$cohort==1994]<- 1993
df_panel$election_year[df_panel$state_name=="rajasthan" & df_panel$cohort==1999]<- 1998
df_panel$election_year[df_panel$state_name=="rajasthan" & df_panel$cohort==2004]<- 2003
df_panel$election_year[df_panel$state_name=="rajasthan" & df_panel$cohort==2009]<- 2008
df_panel$election_year[df_panel$state_name=="rajasthan" & df_panel$cohort==2014]<- 2013

################################# Tamil Nadu ##########################

df_panel$election_year[df_panel$state_name=="tamil nadu" & df_panel$cohort==1992]<- 1991
df_panel$election_year[df_panel$state_name=="tamil nadu" & df_panel$cohort==1997]<- 1996
df_panel$election_year[df_panel$state_name=="tamil nadu" & df_panel$cohort==2002]<- 2001
df_panel$election_year[df_panel$state_name=="tamil nadu" & df_panel$cohort==2007]<- 2006
df_panel$election_year[df_panel$state_name=="tamil nadu" & df_panel$cohort==2012]<- 2011

################################### Tripura #######################

df_panel$election_year[df_panel$state_name=="tripura" & df_panel$cohort==1994]<- 1993
df_panel$election_year[df_panel$state_name=="tripura" & df_panel$cohort==1999]<- 1998
df_panel$election_year[df_panel$state_name=="tripura" & df_panel$cohort==2004]<- 2003
df_panel$election_year[df_panel$state_name=="tripura" & df_panel$cohort==2009]<- 2008
df_panel$election_year[df_panel$state_name=="tripura" & df_panel$cohort==2014]<- 2013

################################ Uttar Pradesh ##########################

df_panel$election_year[df_panel$state_name=="uttar pradesh" & df_panel$cohort==1992]<- 1991
df_panel$election_year[df_panel$state_name=="uttar pradesh" & df_panel$cohort==1994]<- 1993
df_panel$election_year[df_panel$state_name=="uttar pradesh" & df_panel$cohort==1997]<- 1996
df_panel$election_year[df_panel$state_name=="uttar pradesh" & df_panel$cohort==2003]<- 2002
df_panel$election_year[df_panel$state_name=="uttar pradesh" & df_panel$cohort==2008]<- 2007
df_panel$election_year[df_panel$state_name=="uttar pradesh" & df_panel$cohort==2013]<- 2012

##################### Uttarakhand ##################################

df_panel$election_year[df_panel$state_name=="uttarakhand" & df_panel$cohort==2003]<- 2002
df_panel$election_year[df_panel$state_name=="uttarakhand" & df_panel$cohort==2008]<- 2007
df_panel$election_year[df_panel$state_name=="uttarakhand" & df_panel$cohort==2013]<- 2012

############################### West Bengal #############################

df_panel$election_year[df_panel$state_name=="west bengal" & df_panel$cohort==1991]<- 1991
df_panel$election_year[df_panel$state_name=="west bengal" & df_panel$cohort==1997]<- 1996
df_panel$election_year[df_panel$state_name=="west bengal" & df_panel$cohort==2002]<- 2001
df_panel$election_year[df_panel$state_name=="west bengal" & df_panel$cohort==2007]<- 2006
df_panel$election_year[df_panel$state_name=="west bengal" & df_panel$cohort==2012]<- 2011


df_panel<- df_panel%>%
  subset(election_year!=0)

df_panel<- df_panel%>%
  mutate(id=paste(state_name,district_name,election_year,sep = "-"))

df_panel$birth_order[df_panel$birth_order>=3]<- 3

write.csv(df_panel,"NFHS__restricted_all_birth_orders.csv")


############################## Data cleaning for first and second order comparison ##########################

df_first_second_match<- df_panel%>%                               ##### Selecting the data for birth order 2 vs 1 analysis ###########
subset(birth_order==1|birth_order==2)%>%
  group_by(CASEID)%>%
  summarise(group_size=length(CASEID))%>%
  subset(group_size==2)

df_first_second_total<- df_panel%>%
  subset(birth_order==1|birth_order==2)

df_first_second<- merge(df_first_second_total,df_first_second_match,by="CASEID")

df_first_second<- df_first_second[order(df_first_second$CASEID,df_first_second$cohort),]

write.csv(df_first_second,"NFHS_restricted_1_2_birth_orders.csv")

############################## Data cleaning for first and third order comparison ##########################

df_first_third_match<- df_panel%>%                               ##### Selecting the data for birth order 3 vs 1 analysis ###########
subset(birth_order==1|birth_order==3)%>%
  group_by(CASEID)%>%
  summarise(group_size=length(CASEID))%>%
  subset(group_size>=2)

df_first_third_total<- df_panel%>%
  subset(birth_order==1|birth_order==3)

df_first_third<- merge(df_first_third_total,df_first_third_match,by="CASEID")

df_first_third<- df_first_third[order(df_first_third$CASEID,df_first_third$cohort),]

write.csv(df_first_third,"NFHS_1_3_restricted_birth_orders.csv")

############################## Data cleaning for second and third order comparison ##########################

df_second_third_match<- df_panel%>%                               ##### Selecting the data for birth order 3 vs 2 analysis ###########
subset(birth_order==2|birth_order==3)%>%
  group_by(CASEID)%>%
  summarise(group_size=length(CASEID))%>%
  subset(group_size>=2)

df_second_third_total<- df_panel%>%
  subset(birth_order==2|birth_order==3)

df_second_third<- merge(df_second_third_total,df_second_third_match,by="CASEID")

df_second_third<- df_second_third[order(df_second_third$CASEID,df_second_third$cohort),]

write.csv(df_second_third,"NFHS_2_3_restricted_birth_orders.csv")


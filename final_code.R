library(stringr)
setwd("~/LocalDocuments/UCDavis/Year3/STA141A/assignment 5")
directory="messy"
files=list.files(directory, full.names = T)
file=files[10]
readLines(file)


read_post= function(file){
  raw_text= readLines(file)
  #we need to read in the file, then split up the title and text. After we have the title and text,
  #then we can start extracting the necessary features. 
  
  title= str_squish(raw_text[1])
  #start=which(str_detect(raw_text,"QR Code Link to This Post")) + 1
  #text = str_squish(paste(raw_text[start:length(raw_text)],collapse="\n"))
  #text = str_squish(raw_text[2:length(raw_text)])
  text= str_squish(paste(raw_text[2:length(raw_text)],collapse="\n"))
  
  #begin extracting info from post
  
  #make numbers by removing the $ and ,.  numeric (in the read post)
  price_title= str_extract(title, "\\$[0-9,..]+")
  price_title = str_remove_all(price_title, "[^0-9.]")
  price_title= as.numeric(price_title)
  
  #price_user = "Price: $1,600.00"
  price_user= str_extract(text, "Price: \\$[0-9,.]+")
  price_user= str_remove_all(price_user, "[^0-9.]")
  price_user= as.numeric(price_user)
  
  #deposit = "$34,222.12.. deposit only in cash."
  deposit_text = str_extract(tolower(text),"(deposit(:| )\\s\\$[0-9,.]+)|(\\$[0-9,.]+[^ ] deposit)|
                             (deposit is \\$[0-9,.]+)|(security deposit(:| )\\s\\$[0-9,.]+)|security deposit starting at \\$[0-9,.]+")
  deposit_text = str_remove_all(deposit_text,"[^0-9.]")
  deposit_text = str_remove_all(deposit_text, "[.]+$")
  deposit_text = as.numeric(deposit_text)
  
  
  #pet_deposit="$53,222.23 pet deposit and then " 
  pet_deposit = str_extract(tolower(text), "((pet[s] deposit fee max[:] \\$[0-9,.]+)|(pet(s| )deposit(:| )\\$[0-9,.]+)
                            |(pet(s| )security deposit(:| )\\$[0-9,.]+)|(pet[s] security deposit[:] \\$[0-9,.]+)
                            |(deposit for pet(s| )\\$[0-9,.]+)|(deposit for pet(s| ): \\$[0-9,.]+)|(\\$[0-9,.]+\\spet\\sdeposit))")
  pet_deposit= str_remove_all(pet_deposit, "[^0-9.]")
  pet_deposit=as.numeric(pet_deposit)
  
  num_bedrooms = str_extract(text, "Bedrooms: [0-9]+")
  num_bedrooms= str_remove_all(num_bedrooms, "[^0-9.]")
  num_bedrooms= as.numeric(num_bedrooms)
  
  num_bath = str_extract(text,"Bathrooms: [0-9]+")
  num_bath = str_remove_all(num_bath, "[^0-9.]")
  num_bath = as.numeric(num_bath)
  
  sqft= str_extract(text, "Sqft: [0-9,.]+")
  sqft= str_remove_all(sqft, "[^0-9.]")
  sqft=as.numeric(sqft)
  
  longit= str_extract(text, "Longitude: -[0-9,.]+")
  longit = str_remove_all(longit,"Longitude: ")
  longit = as.numeric(longit)
  
  latitude = str_extract(text, "Latitude: [0-9,.]+")
  latitude = str_remove_all(latitude, "Latitude: ")
  latitude = as.numeric(latitude)
  
  date_posted = str_extract(text,"(Date\\sPosted(:| ) (January|February|March|April|May|June|July|
                            August|September|October|November|December)\\s(\\d\\d?).+?(\\d\\d\\d\\d))")
  date_posted= str_remove_all(date_posted, "Date Posted(:| )\\s")
  
  pets=NA
  
  terms_other = "(hamster)|(guinea pig)|rabbit|turtle|chickens|lizards|amphibians|reptiles|([^ ]fish[^ ])"
  if(str_detect(tolower(text), terms_other)) pets="other"
  
  terms_dogs= "(dog-friendly)|(dogs allowed)|(dogs only)|(dog-only)|(dog friendly)|(dogs are welcome)|(dogs welcome)|(dog park)|(dog lover)|(dogs ok)" 
  terms_no_cats=("(no cats)|(no felines)|(cats not allowed)|(allergic to cats)|(cat allergies)|(cats aren't allowed)|(cats are not allowed)")
  search_dogs = str_extract(tolower(text), terms_dogs)
  search_no_cats= str_extract(tolower(text), terms_no_cats)
  if(!is.na(search_dogs) & !is.na(search_no_cats)) pets="dogs"   
  
  terms_cats= "(cat-friendly)|(cats allowed)|(feline friendly)|(cats only)|(cats-only)|(cat friendly)| (cats are welcome)|(cats welcome)|(cat lover)|(cats ok)" 
  terms_no_dogs=("(no dogs)|(no canines)|(dogs not allowed)|(allergic to dogs)|(dog allergies)|(dogs are not allowed)| (dogs aren't allowed)")
  search_cats = str_extract(tolower(text), terms_cats)
  search_no_dogs= str_extract(tolower(text), terms_no_dogs)
  if(!is.na(search_cats) & !is.na(search_no_dogs)) pets="cats"
  
  terms_both= "(pets welcome)| (cats and dogs welcome)|(dogs and cats welcome)|(pets allowed)|(pets are allowed)|(pets are welcome)|(pet deposit)|(pet 
  security deposit)|(animals( | are )allowed)|(animals( | are )welcome)|(pet-friendly)|(pet friendly)|(small pets ok(ay| ))"
  if(str_detect(tolower(text), terms_both)) pets="both"
  
  terms_none= "(no pets)|(pets not allowed)|(not pet-friendly)|(not pet friendly)|(no animals)|(pets aren't allowed)|(pets are not allowed)"
  if(!is.na(str_extract(tolower(text), terms_none)))  pets="none"
  
  heat = NA
  terms_fireplace="(fireplace)|(fireplaces)|(wood (|- )burning stove)"
  if(str_detect(tolower(text), terms_fireplace)) heat="fireplace"
  
  terms_heater ="(heater)|(heating unit)|(central heating)|(central air heating)|heating"
  if(str_detect(tolower(text), terms_heater)) heat="heater"
  
  if(str_detect(tolower(text), terms_fireplace)&str_detect(tolower(text), terms_heater)) heat="both"
  
  terms_no_heat="no (fireplace|(fireplaces)|(heater)|heating|(central heat))"
  if(str_detect(tolower(text), terms_no_heat)) heat="none"
  
  ac=NA
  terms_ac= "(air conditioning)|(air-conditioning)|(central air)|$(ac)|a/c|cooling"
  if(str_detect(tolower(text), terms_ac)) ac="air conditioning"
  
  terms_no_ac=" no (heating (\\&|and|or)|((air conditioning)|(air-conditioning)|(central air)|ac|a/c))"
  if( str_detect(tolower(text), terms_no_ac)) ac="none"
  
  #phone_number= str_extract(tolower(text), "\\(?\\d{3}\\)?[.-]? *\\d{3}[.-]? *[.-]?\\d{4}")
  phone_number= str_extract(tolower(text), "\\(?\\d{3}\\)?[.-]? *\\d{3}[.-]? *[\\.-]?\\d{4}")
  
  email= str_extract(tolower(text),"^[[:alnum:].-_]+@[[:alnum:].-]+$") 
  
  #now that we have all the features we need to return all these. B/c return is used jsut with one,
  #I will create a variable features of all the variables that I want to return. 
  features=list(Title=title,Text=text,Latitude=latitude,Longitude= longit,Date_Posted=date_posted,Price=price_title,
                User_Price=price_user,Deposit=deposit_text,Sqft= sqft, Bedrooms=num_bedrooms,Bathrooms=num_bath,
                Pets=pets,Pet_Deposit=pet_deposit,Heat=heat, AC=ac,Email=email,Phone_Number=phone_number)
  
  return(features)
}
read_post(file)

read_all_posts=function(directory){
  
  #here we have a function that reads all posts from a directory
  files=list.files(directory, full.names = T)
  #set each column as a character or numeric 
  n=length(files)
  Title=character(n)
  Text=character(n)
  Price = numeric(n)
  User_Price= numeric(n)
  Deposit= numeric(n)
  Pets= character(n)
  Pet_Deposit= numeric(n)
  Heat= character(n)
  AC = character(n)
  Bedrooms = numeric(n)
  Bathrooms= numeric(n)
  Sqft = numeric(n)
  Longitude=numeric(n)
  Latitude=numeric(n)
  Email=character(n)
  Phone_Number= character(n)
  Date_Posted = character(n)
  Region = character(n)
  
  #to keep track of loading, print the directory
  print(directory)
  
  #for length of each subfolder read each post and do the steps above from read posts function
  for(i in 1:n){
    file=files[i]  # set a specific file to a number from files
    file_info=read_post(file)
    #if(is.null(file_info)) next
    #region = basename(file[i])
    
    Title[i]= file_info$Title
    Text[i] = file_info$Text
    Price[i] = file_info$Price
    User_Price[i]= file_info$User_Price
    Deposit[i]=file_info$Deposit
    Pets[i]= (file_info$Pets)
    Pet_Deposit[i] =file_info$Pet_Deposit
    Heat[i] = file_info$Heat
    AC[i] = file_info$AC
    Sqft[i]= file_info$Sqft
    Bedrooms[i]= file_info$Bedrooms
    Bathrooms[i] =file_info$Bathrooms
    Longitude[i]= file_info$Longitude
    Latitude[i] = file_info$Latitude
    Email[i]= file_info$Email
    Phone_Number[i]= file_info$Phone_Number
    Date_Posted[i]= file_info$Date_Posted 
    #Region[i] = file_info$Region
  }
  results = data.frame(Title,Text,Region=basename(directory),Latitude,Longitude,Date_Posted,Price,User_Price,Deposit,Sqft, Bedrooms,Bathrooms,Pets = as.factor(Pets),
                       Pet_Deposit,Heat=as.factor(Heat), AC= as.factor(AC),Email,Phone_Number,stringsAsFactors = F) 
  return (results)  
}
data=read_all_posts(directory)

dirs = list.files(directory, full.names = TRUE)
post_all= lapply(dirs, read_all_posts)
posts_all = do.call(rbind, post_all)
str(posts_all)
#check that all regions are there should have 9
unique(posts_all$Region)

#after data is loaded, save the dataframe
saveRDS(posts_all,file="~/Documents/UC Davis/Year 3/STA 141A/assignment 5/posts_all.RDS")
posts_all = readRDS("/Users/aboeriu/Documents/UC Davis/Year 3/STA 141A/assignment 5/posts_all.RDS")
########################################################################################################
#clean up whole dataframe created
#fix the prices / user price
range(posts_all$Price,na.rm = T)
posts_all$Price[34055] = 3742
posts_all$User_Price[34055]=3742

posts_all$Price[8984] = 1095
posts_all$User_Price[8984]=1095

#fix num beds, remove all beds>4 because they are homes
range(posts_all$Bedrooms,na.rm=T)
posts_all=posts_all[(is.na(posts_all$Bedrooms) | posts_all$Bedrooms<5),]

#fix num bathrooms
range(posts_all$Bathrooms,na.rm=T)
#posts_all[which(posts_all$Bathrooms==16),]
posts_all$Bathrooms[41801]=1
#posts_all[which(posts_all$Bathrooms==12),] 
posts_all$Bathrooms[45665]=1
posts_all[15081,]
posts_all$Bathrooms[15081]=2
#which(posts_all$Bathrooms==5)
posts_all$Bathrooms[1978]=2.5
#remove all 5 bathrooms, b/c they advertise 1-3 beds and focus is 1 apartm/post
posts_all=posts_all[(is.na(posts_all$Bathrooms) | posts_all$Bathrooms<5),] 

#are there posts where beds<baths
which(posts_all$Bedrooms > 0 & posts_all$Bedrooms < posts_all$Bathrooms)
#posts_all[556,]

#where bath > bedrooms
index = which(posts_all$Bedrooms > 0 & posts_all$Bedrooms < posts_all$Bathrooms)
posts_all$Bathrooms[index] = NA

#user price
range(posts_all$User_Price,na.rm = T)

#Deposit
range(posts_all$Deposit,na.rm=T)
#which(posts_all$Deposit==27950)
#posts_all[41661,]
posts_all$Deposit[41661]=2795

#which(posts_all$Deposit==21000)
#posts_all[27684,]
#remove because these are all homes,comment after removing 
#posts_all=posts_all[-c(27682,27683,27684),]
which(posts_all$Deposit==17545)
posts_all[25521,]

#pet deposits
range(posts_all$Pet_Deposit,na.rm=T)
#which(posts_all$Pet_Deposit==2750)
#posts_all[1431,]
posts_all$Pet_Deposit[1431]=250

#which(posts_all$Pet_Deposit==2150)
#posts_all[729,]
posts_all$Pet_Deposit[729]=250

#which(posts_all$Pet_Deposit==1895)
#posts_all[38312,]
posts_all$Pet_Deposit[38312]=250

#which(posts_all$Pet_Deposit==1600)
#posts_all[13826,]
posts_all$Pet_Deposit[13826]=500

#which(posts_all$Pet_Deposit==1525)
#posts_all[14122,]
posts_all$Pet_Deposit[14122]=250

#which(posts_all$Pet_Deposit==1500)
#posts_all[11867,]
posts_all$Pet_Deposit[11867]=400

#which(posts_all$Pet_Deposit==1350)
#posts_all[13829,]
posts_all$Pet_Deposit[13829]=500


#check range for long and lat, longit range should be -180->+180 and lat 0-90
range(posts_all$Latitude,na.rm = T)
range(posts_all$Longitude,na.rm=T)

range(posts_all$Sqft,na.rm = T)
which(posts_all$Sqft==15000)
posts_all[17906,] #actually the building size
#18367 18373 21607 22143 25045 25082 25083
 
####################################################################################################
#check to see what causes NA coercion error after removing the necessary outliers
sum(is.na(posts_all$Longitude))
sum(is.na(posts_all$Date_Posted))
sum(is.na(posts_all$Price))
sum(is.na(posts_all$User_Price)) 
########################################################################################################
library(ggplot2)
library(ggrepel)
library(ggrepel)
library(viridis)
###############################################################################################################
#user and rental price
length(posts_all$Price)
length(posts_all$User_Price)
sum(!is.na(posts_all$Price))
sum(!is.na(posts_all$User_Price))

table(posts_all$Price == posts_all$User_Price)
length(which(posts_all$Price != posts_all$User_Price))
unique(posts_all[which(posts_all$Price != posts_all$User_Price), c("Price", "User_Price")])

#find the avg difference of all posts that differ
differ=posts_all[which(posts_all$Price != posts_all$User_Price), c("Price", "User_Price")]
subt=differ[,1]-differ[,2]
mean(subt)


#################################################################################################
#rental price and deposit amounts
posts_all$Region = as.factor(posts_all$Region)
library(plyr)
#rename the regions to make it look nicer
posts_all$Region=revalue(posts_all$Region, c("losangeles"="Los Angeles","sacramento"="Sacramento","sandiego"="San Diego",
"sfbay"="San Francisco Bay","sfbay_eby"="San Francisco East Bay","sfbay_nby"="San Francisco North Bay",
"sfbay_pen"="San Francisco Bay Penninsula","sfbay_sby"="San Francisco South Bay","sfbay_sfc"="San Francisco City"))

ggplot(posts_all[!is.na(posts_all$Deposit ) & !is.na(posts_all$Price), ])+
  geom_point(aes(x=Price,y=Deposit),color="darkslategray4",alpha=0.3)+
  facet_wrap(Region~.)+
  ylim(0,10000)+
  theme_minimal()+
  ylab("Deposit ($)")+
  xlab("Price ($)")+
  #xlim(0,10000)+
  theme(strip.background =element_rect(fill="floralwhite"))+
  theme(strip.text = element_text(colour = 'black'))+
  ggtitle("Comparing Rental Price and Deposit")+
  theme(panel.spacing = unit(0.5, "lines"))+
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12,face="bold"))+
  ggsave("deposit vs price.png",height=5 ,width=10)

####################################################################################################
#pets
table(posts_all$Pets)

#to get region of 18 other pet posts
table(posts_all$Region[which(posts_all$Pets=="other")])

ggplot(posts_all[which(posts_all$Pets=="other"),])+
  geom_bar(aes(x=Region),fill="cadetblue3")+
  geom_text(stat='count', aes(x=Region,label=..count..), vjust=-1)+
  theme(axis.text.x=element_text(angle=30,hjust=1))+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))+
   ylab("Number of Apartments with Other Pets")+
  scale_y_continuous(breaks = seq(0, 14, by =2))+
  ggsave("num other pets by region.png",height=7,width=9)

which(posts_all$Region=="San Francisco East Bay" & posts_all$Pets=="other")

  
# table(posts_all$Pets!="none")
# length(which(posts_all$Pets != "none"))
# length(unique(posts_all[which(posts_all$Pets!="none"),]))

ggplot(posts_all[which(posts_all$Pets!="none"), ])+
 geom_boxplot(aes(x=Pets,y=Pet_Deposit))+
  xlab("Type of Pets")+
  ylab("Pet Deposit")+
  ggtitle("Exploring Pets and Pet Deposit ($)")+
  theme_minimal()+
  ylim(0,1000)+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))+
  ggsave("pet$ vs pets.png",height=5,width=7)
 
which(posts_all$Pets=='both' & posts_all$Pet_Deposit==1000)
 
aggregate(Pet_Deposit~Pets,posts_all,mean)
aggregate(Pet_Deposit~Pets,posts_all,median)



############################################################################################

#7 HVAC
table(posts_all$Heat)
table(posts_all$AC=="none")

#to see if there are more AC than heat, need to exclude the both apartments 
#which posts have some sort of heat and no AC
table(posts_all$Region[which(posts_all$Heat!='none' & posts_all$AC=='none')])

#some kind of AC and no Heat
length(which(posts_all$Heat=='none' & posts_all$AC!='none'))

which(posts_all$Heat=='none' & posts_all$AC!='air conditioning')

#which have both heat and AC
d=length(which(posts_all$Heat=='both' & posts_all$AC=='air conditioning'))
e=length(which(posts_all$Heat=='heater' & posts_all$AC=='air conditioning'))
f=length(which(posts_all$Heat=='fireplace' & posts_all$AC=='air conditioning'))
sum(d,e,f)
sum(d,e,f)/45757*100 # to find %

ggplot(posts_all[!is.na(posts_all$Heat) &!is.na(posts_all$AC),])+
  geom_bar(aes(x=Heat, fill=AC))+
  facet_wrap(Region~.)+
  scale_fill_viridis_d(option="plasma")+
  ggtitle(" Heat vs AC")+
  ylab("Number of Apartments")+
  ylim(0,800)+
  theme_minimal()+
  geom_text(stat='count', aes(x=Heat,label=..count..),vjust=-.5)+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(strip.background =element_rect(fill="snow"))+
  theme(strip.text = element_text(colour = 'black'))+
  ggsave("heat and AC.png",height=6,width=9)


ggplot(posts_all[!is.na(posts_all$Heat) &!is.na(posts_all$AC),])+
  geom_bar(aes(x=AC, fill=Heat),position = 'dodge')+
  facet_wrap(Region~.)+
  scale_fill_viridis_d(option="plasma")+
  ggtitle("AC vs Heat")+
  ylab("Number of Apartments")+
  ylim(0,800)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(strip.background =element_rect(fill="snow"))+
  theme(strip.text = element_text(colour = 'black'))+
  ggsave("ac and heat.png",height=6,width=9)
 
 
####################################################################################################
#8 email and phone numbers
length(posts_all$Email)
sum(!is.na(posts_all$Email))

length(posts_all$Phone_Number)
sum(!is.na(posts_all$Phone_Number))
table(posts_all$Phone_Number)
which(posts_all$Phone_Number=="0107740054")



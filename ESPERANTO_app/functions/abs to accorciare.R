library(collections)

dict_keys <-rbind(
               data.frame( label="sex" , lab_syn ="gender", allowed_features=c("M","F"), syn_features =c("Male,male","Female,female")  ) ,
               data.frame( label="organism" , lab_syn =NA, allowed_features=c("Homo Sapiens","Mus Musculus"), syn_features =NA)) %>%
               tidyr::separate_rows(.,c(syn_features))
 new_entries <- rbind(data.frame( label="disease" , lab_syn ="illness", allowed_features="COVID-19", syn_features ="Covid-19"),
                   data.frame( label="disease" , lab_syn ="sickness", allowed_features="COVID-19", syn_features ="SARS-CoV-2"),
                   data.frame( label="disease" , lab_syn =NA, allowed_features=NA, syn_features =NA),
                   data.frame( label="organism" , lab_syn ="being", allowed_features="Mus Musculus", syn_features ="mouse"),
                   data.frame( label="organism" , lab_syn =NA, allowed_features="Mustela putorius", syn_features ="Ferrets"),
                   data.frame( label="organism" , lab_syn =NA, allowed_features="Mustela putorius", syn_features ="ferret"),
                   data.frame( label="sex" , lab_syn =NA, allowed_features="F", syn_features ="femine") )
 new_OBJdict <-collections::dict(NULL)
 new_OBJdict$set("sex_*_gender_**_M", "Male,male")$set("sex_*_gender_**_F", "Female,female")$set("organism_*_being_**_Homo Sapiens", NA)$set("organism_*_being_**_Mus Musculus","mouse")$set("disease_*_illness|sickness_**_COVID-19", "Covid-19,SARS-CoV-2")$set("disease_*_illness|sickness",NA)$set("organism_*_being_**_Mustela putorius","Ferret,ferret")
 convert_dict_to_df(new_OBJdict)

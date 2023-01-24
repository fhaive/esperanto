#cerca per label(ormai i sinonimi non ci sono piu), poi guardo i sinonimi delle feature, omogenizzo, eventualmente salvo.
column_recoder2 <- function (dict_keys, clean_homog_df){
  #cerco per le label le colonne per le quali posso agire omogenizzandole; poi ripulisco da tag, e guardo sinonimi delle feature, omogenizzo, eventualmente salvo.
  # toberecoded identifies names of the dataset columns containing the labels
  # toberecoded_name identifies names of the dataset columns with matching cell content
  labels<-paste0(unique(dict_keys$label),collapse="|")
  u<-(apply(clean_homog_df, 2, function(x) grepl(paste0("^",labels), x)))[1,]
  tobe_recoded_content <- names(u[u])
  # toberecoded_col identifies names of the dataset columns with matching colname
  v<-(apply(data.frame(names(clean_homog_df)), 2, function(x) grepl(paste0("^",labels), x)))
  tobe_recoded_col <- names(clean_homog_df)[v]
  tobe_recoded <- unique(c(tobe_recoded_content,tobe_recoded_col))
  
  #remove eventual tags (usando label) present in the columns
  #df_tmp <-data.frame(clean_homog_df[tobe_recoded])   
  df_tmp <-((clean_homog_df)[tobe_recoded])
  k <-(apply(df_tmp, 2, function(x) grepl("^.+: ", x)))
  kT <- apply(k, 2, function(x) unique(x)==TRUE)
  
  
  df_tmp[kT]<- (lapply( df_tmp[kT], function(x) gsub("^.+: ","", x)))
  df_tmp[kT]  <- (lapply( df_tmp[kT], function(x) gsub(" NA", NA, x)))
  
  # homogenization steåp for each cell of each column is performed by column_feature function
  for (i in 1:ncol(df_tmp)){
    nam<-names(df_tmp[,i, drop=FALSE])
    df_tmp[[i]]<- (nam=column_feature2 (dict_keys,df_tmp[[i]]))     ##NEW
   
  
    }
  #}
  
  
  #sostituisco le colonne elaborate in recoder nel df originale
  clean_homog_df<-data.frame(clean_homog_df)
  new_col_names <- names(df_tmp)
  clean_homog_df[new_col_names] <- df_tmp
  clean_homog_df
}

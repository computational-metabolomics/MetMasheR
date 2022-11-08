#' @eval get_description('cd_source')
#' @include import_annotation_class.R
#' @export
cd_source = function(
    input_file, 
    compounds_file,
    tag = 'CD',
    cd_version = 3.1,
    add_cols=list(),
    ...) {
    # new object
    out = new_struct('cd_source',
        input_file = input_file,
        compounds_file = compounds_file,
        cd_version = cd_version,
        add_cols = add_cols,
        tag=tag,
        ...
    )
    return(out)
}

.cd_source<-setClass(
    "cd_source",
    contains = c('annotation_source'),
    slots = c(
        compounds_file = 'entity',
        cd_version = 'enum',
        imported='entity'
    ),
    prototype = list(
        compounds_file = entity(
            name = 'Compounds file',
            description = "The file of compounds output by Compound Discoverer",
            value=character(0),
            type='character',
            max_length = 1
        ),
        cd_version = enum(
            name = 'Version',
            description = "The version of Compound Discoverer used to generate the input files.",
            value=3.1,
            type='numeric',
            max_length = 1,
            allowed=c(3.1,3.3)
        ),
        imported = entity(
            name = 'Imported annotations',
            description=paste0('The imported annotations as an annotation_table object'),
            type='lcms_table'
        ),
        .params=c('compounds_file','cd_version')
    )
)


#' @export
setMethod(f = "model_apply",
          signature = c("cd_source",'lcms_table'),
          definition = function(M,D) {

              CD_data <- readWorkbook(xlsxFile=M$input_file, sheet=2)
              CD_isomers <- readWorkbook(xlsxFile=M$compounds_file, sheet=2)
              
              if (M$cd_version==3.3) {
                  # replace all 'Tags' with NA
                  CD_data[CD_data=='Tags']=NA
                  CD_isomers[CD_isomers=='Tags']=NA
                  # remove Tags column
                  CD_data$Tags = NULL
                  CD_isomers$Tags = NULL
              }
              unique_cd_compounds_ids <-which(!is.na(CD_data$Checked))
              cd_compounds <- CD_data$Name[unique_cd_compounds_ids]
              cd_formula <- gsub(" ", "", CD_data$Formula[unique_cd_compounds_ids])
              cd_RT <- as.numeric(CD_data$`RT.[min]`[unique_cd_compounds_ids]) * 60
              cd_mzcloud_score <- as.numeric(CD_data$mzCloud.Best.Match[unique_cd_compounds_ids])
              cd_mz <- as.numeric(CD_data$`m/z`[unique_cd_compounds_ids])
              
              if (length(unique_cd_compounds_ids)>1){
                  unique_cd_compounds_ids_end <- c((unique_cd_compounds_ids-1)[2:length(unique_cd_compounds_ids)], nrow(CD_data))
              } else {
                  unique_cd_compounds_ids_end <- nrow(CD_data)
              }
              
              # CD output is stored in weird tree structure, compounds -> compounds_per_files -> features
              # this loop will create summary feature table
              cd_compounds_table <- vector("list", length(cd_compounds))
              
              for (cmpd in seq_len(length(unique_cd_compounds_ids))){
                  sub_df <- CD_data[unique_cd_compounds_ids[cmpd]:unique_cd_compounds_ids_end[cmpd],]
                  sub_df$Checked <- NULL
                  
                  sub_df <- sub_df[-c(1), ]
                  
                  sub_colnames <- sub_df[1,]
                  names(sub_colnames) <- NULL
                  
                  sub_df <- sub_df[-c(1), ]
                  #colnames(sub_df) <- sub_colnames
                  sub_df$Name <- NULL
                  
                  file_hits <- which(sub_df$Formula!="Checked" & sub_df$Formula!="FALSE")
                  if (length(file_hits)!=1){
                      file_hits_end <- c((file_hits-1)[2:length(file_hits)], nrow(sub_df))
                  } else {
                      file_hits_end <- nrow(sub_df)
                  }
                  
                  features_list <- vector ("list", length(file_hits))
                  
                  for (ff in 1:length(file_hits)){
                      file_df <- sub_df[file_hits[ff]:file_hits_end[ff], ]
                      file_df_colnames <- file_df[2, ]
                      names(file_df_colnames) <- NULL
                      
                      no_na_names <- which(!is.na(file_df_colnames))
                      
                      file_df <- file_df[-c(1,2),no_na_names]
                      colnames(file_df) <- file_df_colnames[no_na_names]
                      
                      features_list[[ff]] <- data.frame(Compound=cd_compounds[cmpd],
                                                        Formula=cd_formula[cmpd], RT=as.numeric(file_df$`RT [min]`)*60, #RT = cd_RT[cmpd],
                                                        mzcloud_score=cd_mzcloud_score[cmpd], # gets replaced
                                                        Ion=file_df$Ion, Charge=file_df$Charge, 
                                                        mz=as.numeric(file_df$`m/z`),
                                                        area=as.numeric(file_df$Area))
                  }
                  
                  features_list <- do.call(rbind, features_list)
                  features_list <- as.data.frame(features_list %>% dplyr::group_by(Ion) %>%
                                                     dplyr::summarise(Compound      = unique(Compound),
                                                                      Formula       = unique(Formula),
                                                                      RT            = mean(RT),
                                                                      mzcloud_score = mean(mzcloud_score),
                                                                      Charge        = unique(Charge),
                                                                      mz            = mean(mz),
                                                                      area          = max(area),
                                                                      file_count    = n()))
                  
                  # flag most common adduct across input files
                  hi = max(features_list$file_count)
                  w=which(features_list$file_count==hi)
                  file_count_n=features_list[w,]
                  # if multiple with same count, choose largest area
                  v = which.max(file_count_n$area)
                  
                  features_list$ref_ion = 0
                  features_list$ref_ion[w[v]] = 1
                  
                  cd_compounds_table[[cmpd]] <- features_list
              }
              
              #cd_compounds_table <- do.call(rbind, cd_compounds_table)
              
              #Similarly parse table of compounds with similar hits as the highest
              unique_cd_compounds_ids <-which(!is.na(CD_isomers$Checked))
              
              if (length(unique_cd_compounds_ids)>1){
                  unique_cd_compounds_ids_end <-
                      c((unique_cd_compounds_ids-1)[2:length(unique_cd_compounds_ids)],
                        nrow(CD_isomers))
              } else {
                  unique_cd_compounds_ids_end <- nrow(CD_isomers)
              }
              
              cd_isomers_table <- vector("list", length(unique_cd_compounds_ids))
              
              for (cmpd in seq_len(length(unique_cd_compounds_ids))){
                  sub_df <- CD_isomers[unique_cd_compounds_ids[cmpd]:unique_cd_compounds_ids_end[cmpd],]
                  sub_df$Checked <- NULL

                  sub_df <- sub_df[-c(1), ]
                  
                  sub_df$Name <- NULL
                  
                  sub_colnames <- unlist(sub_df[1,])
                  names(sub_colnames) <- NULL
                  
                  sub_df <- sub_df[-c(1), ]
                  #colnames(sub_df) <- sub_colnames
                    
                  name_col=which(sub_colnames=='Name')
                  score_col=which(sub_colnames=='Match')
                  kegg_col=which(sub_colnames=='KEGG ID')
                  cpd_match=which(sub_colnames=='Compound Match')
                  ppm_diff=which(sub_colnames=='DeltaMass [ppm]')
                  
                  #cmpd_names <- paste (sub_df[[name_col]], collapse=" || ")
                  #cmpd_cd_score <- paste (round(as.numeric(sub_df[[score_col]]), 2), collapse=" || ")
                  #cpmd_kegg_id <- paste (sub_df[[kegg_col]], collapse=" || ")


                  
                  cd_isomers_table[[cmpd]] <- data.frame(Compound=sub_df[[name_col]], 
                                                         mzcloud_score=sub_df[[score_col]],
                                                         kegg_id = sub_df[[kegg_col]],
                                                         compound_match=sub_df[[cpd_match]],
                                                         ppm_diff = sub_df[[ppm_diff]])
              }
              
              # Replace compound names and score in cd_compound_table with all matching names and CD scores
              for (cmpd in seq_len(length(cd_compounds_table))){
                  # duplicate records
                  record=cd_compounds_table[[cmpd]]
                  record_list=rep(list(record),nrow(cd_isomers_table[[cmpd]]))
                  record_list=do.call(rbind,record_list)
                  # update with isomers
                  record_list=record_list[order(record_list$Ion),]
                  record_list$Compound = cd_isomers_table[[cmpd]]$Compound
                  record_list$mzcloud_score = cd_isomers_table[[cmpd]]$mzcloud_score
                  record_list$kegg_id = cd_isomers_table[[cmpd]]$kegg_id
                  record_list$compound_match = cd_isomers_table[[cmpd]]$compound_match
                  record_list$compound_no = cmpd
                  cd_compounds_table[[cmpd]]=record_list
              }
              
              cd_compounds_table <- do.call(rbind, cd_compounds_table)
              
              cd_compounds_table[cd_compounds_table=='NA']=NA
              
              # add id column
              cd_compounds_table$id=as.character(1:nrow(cd_compounds_table))
             
              # add extra columns if requested
              if (length(M$add_cols)>0){
                  for (g in 1:length(M$add_cols)) {
                      cd_compounds_table[[names(M$add_cols)[g]]]=M$add_cols[[g]]
                  }
              }

              # update objects              
              D$annotations=as.data.frame(cd_compounds_table)
              D$annotations$mzcloud_score=as.numeric(D$annotations$mzcloud_score)
              D$annotations$mz=as.numeric(D$annotations$mz)
              D$annotations$RT=as.numeric(D$annotations$RT)
              D$annotations$Charge=as.numeric(D$annotations$Charge)
              D$mz_column = 'mz'
              D$rt_column = 'RT'
              D$id_column = 'id'
              D$tag = M$tag
              
              M$imported=D
        
              return(M)
              
          })



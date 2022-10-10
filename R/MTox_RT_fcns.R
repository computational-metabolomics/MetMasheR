
#' @export
import_MTox_RT_df = function(){
    # read csv
    IN = read.csv(
        system.file(package='structReportsPCB','annotations','combined_MTox_rt_ms2_lib.csv',mustWork = TRUE))
    IN$hmdb_ms2=NULL
    IN$X=NULL
    IN$id=as.character(1:nrow(IN))
    
    # make ions consistent with CD
    ions=IN$adduct
    # any ion ending with + or - becomes +1 or -1
    ions=gsub("[\\+]+$","+1",ions,perl=TRUE)
    ions=gsub("[\\-]+$","-1",ions,perl=TRUE)
    ions=gsub("\\+acetate","-H+HAc",ions,perl=TRUE)
    ions=gsub("\\+formate","-H+CHO2",ions,perl=TRUE)
    IN$adduct=ions
    
    return(IN)
}

#' @eval get_description('import_annotation')
#' @export
#' @include annotation_class.R
import_MTox_RT = function(add_cols=list(),tag='MTox_RT',source=c('rt_only','spectral_lib'),assay,...) {
    out=struct::new_struct('import_MTox_RT',
        add_cols=add_cols,
        tag=tag,
        source=source,
        assay=assay,
        ...)
    return(out)
}


.import_MTox_RT<-setClass(
    "import_MTox_RT",
    contains = c('model'),
    slots = c(
            imported='entity',
            add_cols='entity',
            tag='entity',
            source='entity',
            assay='entity'
        ),
    prototype=list(
        name = 'Import MTox RT library',
        description = paste0('Imports the MTox retention time library as an annotation source.'),
        type = 'univariate',
        predicted = 'imported',
        .outputs=c('imported'),
        .params=c('add_cols','tag','source','assay'),
        imported = entity(
            name = 'Imported annotations',
            description=paste0('The imported annotations as an annotation_source object'),
            type='annotation_source'
        ),
        add_cols=entity(
            name='Add columns',
            description = 'A named list of values to add to all records.',
            type='list',
            max_length=Inf
        ),
        tag=entity(
            name='Tag name',
            description = 'The tag name used to indentify this annotation source in e.g. column names.',
            type='character',
            max_length=1
        ),
        source=entity(
            name='Sources',
            description = 'The source names to retain in the imported library. Values can be: "spectral_lib", "rt_only" or c("spectral_lib","rt_only")',
            type='character',
            max_length=2
        ),
        assay=entity(
            name='Assay',
            description = 'The assay names to retain in the imported library (HILIC_NEG, HILIC_POS, LIPIDS_POS, LIPIDS_NEG)',
            type='character',
            max_length=4
        )
    )
)


#' @export
setMethod(f="model_apply",
    signature=c("import_MTox_RT","annotation_source"),
    definition=function(M,D) {
        
        IN = import_MTox_RT_df()
        
        # add extra columns
        if (length(M$add_cols)>0){
            if (any(names(M$add_cols) %in% colnames(IN))){
                warning("Columns in the imported database have been replaced")
            }
            for (k in names(M$add_cols)){
                IN[[k]]=M$add_cols[[k]]
            }
        }
        
        A=lcms_source(
            input_file='structReportsPCB package',
            name='MTox retention time library',
            description = 'For retention time only matching',
            mz_column='std_mz',rt_column='std_rt',id_column='id',
            tag=M$tag)
        
        A$annotations=IN
        
        # filter to assay
        N = filter_levels(
                column_name='assay',
                levels = M$assay,
                mode = 'include') +
            filter_levels(
                column_name='source',
                levels=M$source,
                mode='include'
            )
        N=model_apply(N,A)
        
        M$imported=predicted(N)
        
        return(M)
    }
)

#' @export
MTox700 = function(){
    IN=read.csv(system.file(package='structReportsPCB','annotations','MTox700+_metabolites.csv',mustWork = TRUE))
    return(IN)
}



update_mtox_rt_inchikey = function(outpath='C:/Users/Gavin/Documents/GitHub/structReportsPCB/inst/annotations') {
    
    M = import_MTox_RT(
            assay=c('HILIC_NEG','HILIC_POS','LIPIDS_NEG','LIPIDS_POS')) +
        id_db_lookup(
            annotation_column = 'hmdb_rt',
            database_column = 'hmdb_id',
            id_database=MTox700(),
            include = c('pubchem_cid','kegg_cid','inchikey'),
        )
    
    M = model_apply(M,lcms_source(input_file=''))
    
    A=predicted(M)
    df=A$annotations
    df$PUBCHEM_ID=NULL
    df$inchikey.x=NULL
    df$cid=NULL
    colnames(df)[1]='hmdb_id'
    colnames(df)[9]='cid'
    colnames(df)[10]='kegg_id'
    colnames(df)[11]='inchikey'
    
    
    write.csv(df,file.path(outpath,'combined_MTox_rt_ms2_lib.csv'),row.names = FALSE)
    
}
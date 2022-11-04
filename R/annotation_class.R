#' @eval get_description("annotation_table")
#' @import dplyr
#' @export annotation_table
#' @import struct
#' @importFrom methods is new slot slotNames
#' @importFrom stats p.adjust
#' @importFrom utils URLencode read.csv read.table write.table
#' @include zzz.R
#' @include generics.R
annotation_table = function(annotations=data.frame(),tag='',id_column='',...) {
    
    check=id_column %in% colnames(annotations)
    
    if (!check){
        stop('"id_column" must be a column name in the "annotations" data.frame')
    }
    
    # new object
    out = new_struct('annotation_table',
        annotations=annotations,
        id_column=id_column,
        tag=tag,
        ...)
    return(out)
}

.annotation_table<-setClass(
    "annotation_table",
    contains = c('struct_class'),
    slots = c(
        annotations = 'entity',
        tag = 'entity',
        id_column = 'character'),
    prototype = list(
        name = 'An annotation source',
        description = 'Base class for annotation sources.',
        annotations = entity(
            name = 'Annotations',
            description = 'Data frame of imported annotations.',
            type = 'data.frame'
        ),
        tag = entity(
            name = 'Annotation source id tag.',
            description=paste0('A (short) character string that is used to ',
                'represent this table e.g. in column names, or source columns.'),
            type='character',
            max_length=1
        ),
        .params=c('tag','id_column'),
        .outputs=c('annotations')
    )
)

#' @eval get_description("annotation_table")
#' @export lcms_table
lcms_table = function(annotations=data.frame(),tag='',id_column='',mz_column='',rt_column='',...) {
    
    msg=character(0)
    
    check=id_column %in% colnames(annotations)
    
    if (!check[1]){
        msg=c(msg,('"id_column" must be a column name in the "annotations" data.frame\n'))
    }
    
    check=mz_column %in% colnames(annotations)
    
    if (!check){
        msg=c(msg,'"mz_column" must be a column name in the "annotations" data.frame\n')
    }
    
    
    check=rt_column %in% colnames(annotations)
    
    if (!check){
        msg=c(msg,'"rt_column" must be a column name in the "annotations" data.frame\n')
    }
    
    if (!length(msg)==0){
        stop(msg)
    }
    
    
    # new object
    out = new_struct('lcms_table',
        annotations=annotations,
        id_column=id_column,
        tag=tag,
        mz_column=mz_column,
        rt_column=rt_column,
        ...)
    return(out)
}

.lcms_table<-setClass(
    "lcms_table",
    contains = c('annotation_table'),
    slots = c(
        mz_column = 'character',
        rt_column = 'character'
    ),
    prototype = list(
        .outputs=c('mz_column','rt_column')
    )
)

#' @export
setMethod(f = 'show',
    signature = c('annotation_table'),
    definition = function(object) {
        
        # print struct generic info
        callNextMethod()
        
        cat('annotations:   ',nrow(object$annotations),' rows x ', 
            ncol(object$annotations),' columns\n',sep='')
        
        head(object$annotations)

    }
)


rename_matching = function(a,matching_columns) {
    
    for (k in 1:length(matching_columns)) {
        
        w=which(colnames(a) %in% matching_columns[[k]])
        
        if (length(w)>1) {
            warning('combine_annotations: more than one matching column for ',
                '"', names(matching_columns)[k], '". Column "', colnames(a)[w[1]],
                '" will be used.')
            # rename extra columns so that not two columns with same name
            colnames(a)[w[2:length(w)]]=paste0('.',colnames(a)[w[2:length(w)]])
        }
        
        if (length(w) > 0) {
            colnames(a)[w[1]] = names(matching_columns)[k]
        }

        # silently ignore no matching columns, as not sources will have all columns
    }
    return(a)
    
}

do_combine = function(A,B,matching_columns,keep_cols,tag_ids,source_col) {
    
    a = A$annotations
    b = B$annotations
    
    a = lapply(a,as.character)
    a = as.data.frame(a)
    
    b = lapply(b,as.character)
    b = as.data.frame(b)
    
    
    if (tag_ids) {
        if (nrow(a)>0){
            a[[A$id_column]]=paste(A$tag,a[[A$id_column]],sep='_')
        }
        if (nrow(b)>0) {
            b[[B$id_column]]=paste(B$tag,b[[B$id_column]],sep='_')
        }
    }
    
    if (!is.null(matching_columns)) {
        a = rename_matching(a,matching_columns)
        b = rename_matching(b,matching_columns)
    }
    
    
    G=list()
    if (nrow(a)>0) {
        G[[A$tag]]=a
    }
    if (nrow(b)>0) {
        G[[B$tag]]=b
    }

    g=dplyr::bind_rows(G,.id=source_col)
    
    w=which((colnames(g) %in% colnames(a) & colnames(g) %in% colnames(b)) | colnames(g) %in% c(keep_cols,source_col))
    g=g[,w]
    
    return(g)
}

#' @export
setMethod(f = "combine_annotations",
    signature = c("annotation_table","annotation_table"),
    definition = function(A,B,matching_columns=NULL,keep_cols=NULL,tag_ids=FALSE,source_col='annotation_table') {
        
        # force matching for known columns defined by object
        all_matching = list(
            id = c(A$id_column,B$id_column)
        )
        # include the user defined ones
        all_matching = c(all_matching,matching_columns)
        
        g = do_combine(A,B,all_matching,keep_cols,tag_ids,source_col)
        
        OUT = struct::new_struct('annotation_table')
        OUT$annotations = g
        OUT$tag = 'combined'
        OUT$name = 'Combined annotation source'
        OUT$description = 'This annotation_table object was generated by combining two other sources.'
        OUT$id_column = 'id'
        
        return(OUT)
    }
)

#' @export
setMethod(f = "combine_annotations",
    signature = c("list","missing"),
    definition = function(A,B,matching_columns=NULL,keep_cols=NULL,tag_ids=FALSE,source_col='annotation_table') {
        
        J = A[[1]]
        annotation_table = rep(J$tag,nrow(J$annotations))
        for (k in 2:length(A)) {
            B = A[[k]]
            C = combine_annotations(J,B,matching_columns=matching_columns,keep_cols=keep_cols,tag_ids=tag_ids,source_col=source_col)
            
            annotation_table_new = C$annotations[[source_col]]
            annotation_table_new[1:length(annotation_table)] = annotation_table
            annotation_table = annotation_table_new
            
            J = C
        }
        J$annotations[[source_col]] = annotation_table
        return(J)
    })


#' @export
setMethod(f = "combine_annotations",
    signature = c("lcms_table","lcms_table"),
    definition = function(A,B,matching_columns=NULL,keep_cols=NULL,tag_ids=FALSE,source_col='annotation_table') {
        
        # force matching for known columns defined by object
        all_matching=list(
            id = c(A$id_column,B$id_column),
            rt = c(A$rt_column,B$rt_column),
            mz = c(A$mz_column,B$mz_column)
        )
        
        # include the user defined ones
        all_matching = c(all_matching,matching_columns)
        
        g = do_combine(A,B,all_matching,keep_cols,tag_ids,source_col)
        
        OUT = struct::new_struct('lcms_table')
        OUT$annotations = g
        OUT$tag = 'combined'
        OUT$name = 'Combined annotation source'
        OUT$description = 'This annotation_table object was generated by combining two other sources.'
        OUT$id_column = 'id'
        OUT$mz_column = 'mz'
        OUT$rt_column = 'rt'
        
        return(OUT)
    }
)



#' @export
setMethod(f = "model_apply",
    signature = c("model_seq","annotation_table"),
    definition = function(M,D) {
        # for each method in the list
        S = D # for first in list the input D is the data object
        
        for (i in seq_len(length(M))) {
            if (M[i]@seq_in != 'data') {
                # apply transformation
                S=M[i]@seq_fcn(S)
                # set
                param_value(M[i],M[i]@seq_in)=S
            }
            # use current data
            M[i] = model_apply(M[i],D)
            
            # set the output of this method as the input for the next method
            S = predicted(M[i])
            if (is(S,'annotation_table')) {
                # if its a dataset then update current D
                D = predicted(M[i])
            }
        }
        return(M)
    }
)


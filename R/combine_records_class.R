#' @eval MetMasheR:::get_description('combine_records')
#' @export
#' @include annotation_class.R
combine_records = function(
        group_by,
    default_fcn=function(x) {paste0(x,collapse=' || ')},
    fcns = list(),
    ...) {
    
    # check fcns are all functions
    if (length(fcns)>0) {
        check=all(unlist(lapply(fcns,function(x){
            is(x,'function') | is(x,'call')
        })))
        if (!check) {
            stop("all fcns list items must be functions or calls.")
        }
    }
    
    out=struct::new_struct('combine_records',
        default_fcn=default_fcn,
        fcns = fcns,
        group_by=group_by,
        ...)
    return(out)
}



.combine_records<-setClass(
    "combine_records",
    contains = c('model'),
    slots=c(
        updated='entity',
        fcns='entity',
        group_by='entity',
        default_fcn='entity'
    ),
    
    prototype=list(
        name = 'Combine annotation records (rows)',
        description = paste0('Combine annotation records (rows) based on a key. ',
            'All records with the same key will be combined. A number of helper ',
            'functions are provided for common approaches to merging records.'),
        type = 'univariate',
        predicted = 'updated',
        .params=c('fcns','group_by','default_fcn'),
        .outputs=c('updated'),
        updated=entity(
            name='Updated annotations',
            description = 'The input annotation source with the newly generated column.',
            type='annotation_table'
        ),
        fcns=entity(
            name='Functions',
            description = 'A named list of functions to use for summarising named columns when combining records. Names should correspond to the columns in the annotation table.',
            type='list'
        ),
        default_fcn=entity(
            name='Default functions',
            description = 'The default function to use for summarising columns when combining records and a specific function has not been provided in fcns.',
            type=c('function'),
            value=function(x){}
        ),
        group_by=entity(
            name='Group by column',
            description = 'The column used as the key for grouping records.',
            type='character'
        )
    )
)


#' @export
setMethod(f="model_apply",
    signature=c("combine_records","annotation_table"),
    definition=function(M,D) {
        
        X=D$annotations
        
        # for any NA, generate a unique id so that we dont lose them during grouping
        for (k in M$group_by) {
            w=which(is.na(X[[k]]))
            if (length(w)>0) {
                X[[k]][w]=paste0('._',k,'_NA_',w)
            }
        }
        
        # if length(group_by) > 1 then combine into single column
        clean = FALSE
        orig_group=M$group_by
        if (length(M$group_by)>1) {
            str=paste(M$group_by,collapse='_x_')
            X[[str]]=do.call(paste,c(as.list(X[,M$group_by]),sep='_'))
            M$group_by = str
            clean = TRUE
        }
        
        # create list of default functions
        FCNS = rep(list(M$default_fcn),ncol(X))
        names(FCNS) = colnames(X)
        
        # add functions for specific columns
        FCNS[names(M$fcns)]=M$fcns
        
        # prep for summarise
        for (k in names(FCNS)) {
            if (k %in% colnames(X)) {
                FCNS[[k]]=expr(across(all_of(!!k),!!FCNS[[k]],.names=!!paste0('.',k)))
            } else {
                FCNS[[k]]=as.call(FCNS[k])
            }
        }
        
        # remove group by column fcn
        FCNS[M$group_by]=NULL
        
        # split into existing and new columns
        FCNSE = FCNS[names(FCNS) %in% colnames(X)]
        FCNSN = FCNS[!(names(FCNS) %in% colnames(X))]
        Y = X %>% group_by_at(M$group_by) 
        Z = do.call(summarise,c(list('.data'=Y),unname(FCNSE),FCNSN))
        
        Z = as.data.frame(Z)
        
        colnames(Z)[2:ncol(Z)]=names(FCNS)
        
        # remove extra column if created
        if (clean) {
            Z[[M$group_by]]=NULL
            M$group_by = orig_group
        }
        
        # remove generated ids for NA if created
        for (k in M$group_by) {
            w=which(grepl(pattern = "^\\.\\_",x = Z[[k]],perl = TRUE)) # match ._ at start of id
            if (length(w)>0) {
                Z[[k]][w]=NA
            }
        }
        
        D$annotations=Z
        M$updated=D
        
        
        return(M)
    }
)

#' Mode
#' returns the most common value, excluding NA
#'
#' @export
.mode= function() {
    fcn=expr(function(x) {
        if ( length(x) <= 2 ) return(x[1])
        if ( anyNA(x) ) x = x[!is.na(x)]
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
    }
    )
    return(eval(fcn))
}


#' Mean
#' for use with collapse_records
#' returns a function for calculating the mean value, excluding NA
#'
#' @export
.mean = function(){
    fcn = expr(function(x){mean(x,na.rm = TRUE)})
    return(eval(fcn))
}

#' Median
#' for use with collapse_records
#' returns a function for calculating the median value, excluding NA
#'
#' @export
.median = function(){
    fcn = expr(function(x){median(x,na.rm = TRUE)})
    return(eval(fcn))
}

#' Collapse
#' for use with combine_records
#' returns a function to collapse a vector into a string using the provided separator 
#'
#' @export
.collapse = function(separator,na_string='NA'){
    fcn = expr(function(x) {
        x[is.na(x)]=!!na_string
        paste0(x,collapse=!!separator)})
    return(eval(fcn))
}

#' select max
#' for use with combine_records
#' returns a function to select the value of a list based on the index of the maximum in a second list
#'
#' @export
.select_max = function(max_col,use_abs=FALSE){
    fcn = expr(function(x) {
        # get values
        vals=as.numeric(cur_data()[[!!max_col]])
        # if all NA, return all records
        if (all(is.na(vals))) {
            return(x)
        }
        # use abs if requested
        if (!!use_abs) {
            vals=abs(vals)
        } 
        # get index of max
        w=which.max(vals)
        w2=which(vals==max(vals))        
        # return all records with max value
        return(x[w2])
    })
    return(eval(fcn))
}

#' select min
#' for use with combine_records
#' returns the value of a list based on the index of the maximum in a second list
#'
#' @export
.select_min = function(min_col,use_abs=FALSE) {
    fcn = expr(function(x) {
        # get values
        vals=as.numeric(cur_data()[[!!min_col]])
        # if all NA, return all records
        if (all(is.na(vals))) {
            return(x)
        }
        # use abs if requested
        if (!!use_abs) {
            vals=abs(vals)
        } 
        # get index of max
        w=which.min(vals)
        w2=which(vals==min(vals))        
        # return all records with max value
        return(x[w2])
    })
    return(eval(fcn))
}

#' select match
#' for use with combine_records
#' returns the value(s) of a list based on the index of identical matches in a second list
#'
#' @export
.select_match = function(match_col,search_col,separator,na_string='NA') {
    fcn = expr(function(x){
        x=x[which(cur_data()[[!!search_col]]==cur_data()[[!!match_col]])]
        if (!is.null(!!separator)) {
            x[is.na(x)]=!!na_string
            x=unique(x)
            paste0(x,collapse=!!separator)
        } else {
            return(x)
        }
    })
    return(eval(fcn))
}

#' select match
#' for use with combine_records
#' returns the value(s) of a list based on the index of identical matches
#'
#' @export
.select_exact = function(match_col,match,separator,na_string='NA') {
    fcn = expr(function(x){
        x=x[which(cur_data()[[!!match_col]]==!!match)]
        
        if (!is.null(!!separator)) {
            x[is.na(x)]=!!na_string
            x=unique(x)
            paste0(x,collapse=!!separator)
        } else {
            return(x)
        }
    })
    return(eval(fcn))
}

#' unique values
#' for use with combine_records
#' reduces a list to unique values, then collapses using the separator
#'
#' @export
.unique = function(separator,na_string='NA',digits=6){
    fcn=expr(function(x){
        x[is.na(x)]=!!na_string
        if (is(x,'numeric')) {
            x=round(x,digits)
        }
        x=unique(x)
        paste0(x,collapse=!!separator)})
    return(eval(fcn))
}

#' prioritise
#' #' for use with combine records
#' reduces a list by prioritizing values according to the input
#' if multiple matches to the priority then collapse using separator
#' 
#' @export
.prioritise = function(match_col,priority,separator,no_match=NA,na_string='NA'){
    fcn=expr(function(x){
        w = NULL
        p = !!priority
        for (k in 1:length(p)) {
            w=which(cur_data()[[!!match_col]]==p[k])
            if (length(w)>0) {break}
        }
        # if no matches
        if (length(w)==0){
            # return x unchanged
            if (is.null(!!no_match)){
                return(x)
                # or replace with no_match
            } else {
                return(!!no_match)
            }
        } else {
            x=x[w]
            
            # if separator is not NULL then collapse
            if (!is.null(!!separator))  {
                x[is.na(x)]=!!na_string
                x=unique(x)
                return(paste0(x,collapse=!!separator))
            } else {
                # return all matches
                return(x)
            }
        }
    })
    return(eval(fcn))
}

#' @export
.nothing = function() {
    fcn=expr(function(x){
        return(x)
    })
    return(eval(fcn))
}

#' @export
.count_levels=function(count_col,levels) {
    fcn=expr(function(){
        df=cur_data()[[!!count_col]]
        return(sum(df == !!levels))
    })
    return(eval(fcn))
    
}





test_that("id_lookup works", {
  
    df=data.frame(
            a=1:10,
            b=1:10,
            id=1:10
    )
    
    AN = annotation_table(annotations=df,id_column='id')
    
    db = data.frame(
            id=c(1,3,5),
            c = rep('cake',3),
            in_db = c(1,1,1)
    )
    
    M = id_db_lookup(
        annotation_column = 'id',
        database_column = 'id',
        id_database = db,
        include = NULL, # include all db columns
        tag = 'db',     # tag db colnames
        not_found = NA  # out found gets NA
    )
    
    M = model_apply(M,AN)
    
    out = predicted(M)$annotations
    
    expect_true(all(c('db_c','db_in_db') %in% colnames(out)))
    expect_true(out[1,'db_c']=='cake')
    expect_true(out[3,'db_c']=='cake')
    expect_true(out[5,'db_c']=='cake')
    expect_true(out[1,'db_in_db']==1)
    expect_true(out[3,'db_in_db']==1)
    expect_true(out[5,'db_in_db']==1)
    expect_true(is.na(out[2,'db_in_db']))
    expect_true(is.na(out[4,'db_in_db']))
    expect_true(is.na(out[6,'db_in_db']))
    
    # check include columns
    M = id_db_lookup(
        annotation_column = 'id',
        database_column = 'id',
        id_database = db,
        include = 'c', # include c column only
        tag = 'db',     # tag db colnames
        not_found = NA  # out found gets NA
    )
    
    M = model_apply(M,AN)
    
    out = predicted(M)$annotations
    expect_true('db_c' %in% colnames(out))
    expect_false('db_in_db' %in% colnames(out))
    
    # check tag
    M = id_db_lookup(
        annotation_column = 'id',
        database_column = 'id',
        id_database = db,
        include = 'c', # include c column only
        tag = NULL,     # dont tag db colnames
        not_found = NA  # out found gets NA
    )
    
    M = model_apply(M,AN)
    
    out = predicted(M)$annotations
    
    expect_false('db_c' %in% colnames(out))
    expect_false('db_in_db' %in% colnames(out))
    expect_true('c' %in% colnames(out))
    expect_false('in_db' %in% colnames(out))
    
    # check not_found
    M = id_db_lookup(
        annotation_column = 'id',
        database_column = 'id',
        id_database = db,
        include = 'in_db', # include in_db column only
        tag = NULL,     # dont tag db colnames
        not_found = 0  # out found gets 0
    )
    
    M = model_apply(M,AN)
    
    out = predicted(M)$annotations

    
})

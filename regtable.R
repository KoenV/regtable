# ##############################################################################
# function to make tables for lm and glm objects
# koen.vanbrabant@kuleuven.be
# date: 13/04/2017
################################################################################


reg_table = function(fit=fit,log=FALSE,roundings=3){
    
    require(Hmisc)
    require(car)
    suppressMessages(require(tibble))
    
    format_pval.table <- function(x){
        if (x < .001) return(paste('<', '.001'))
        else paste(round(x, 3))   
    }

    

    model_variables = attr(fit$terms,"term.labels")
    
    summary_table = summary(fit)$coef
    
    if(log==TRUE){
            summary_table[,1]=exp(summary_table[,1])
    }
    
    
    data = fit$data
    
    table = vector('list',length(model_variables)+1)
    
    
    
    # @ get the intercept correct
    table[[1]] = as_tibble(matrix(NA,1,5))
            
    names(table[[1]]) = c('Variable','Value',
                ifelse(log==TRUE,'Odds Ratio','Estimate'),'95% CI',
                'P-value')
            
            table[[1]][1,1] = '(intercept)'
            
            table[[1]][1,2] = ''
            
            table[[1]][1,3] = round(summary_table[1,1],roundings)
            
            ci = suppressMessages(confint(fit))
            
            if(log==TRUE){
                ci=exp(ci)
            }
            
            lower_ci = unname(round(ci[1,1],roundings))
            upper_ci = unname(round(ci[1,2],roundings))
            
            table[[1]][1,4] = paste0(lower_ci,';',upper_ci)
            

            table[[1]][1,5] = format_pval.table(summary_table[1,4])
    

    

    for (i in 1:length(model_variables)){
        if (is.factor(data[,model_variables[i]])){
            i_v2 = i + 1
            table[[i_v2]] = as_tibble(matrix(NA,
                nlevels(data[,model_variables[i]])+1,5))
            
            names(table[[i_v2]]) = c('Variable','Value',
                ifelse(log==TRUE,'Odds Ratio','Estimate'),'95% CI',
                'P-value')
            
            table[[i_v2]][1,1] = ifelse(label(data[,model_variables[i]])=='',
                model_variables[i],label(data[,model_variables[i]]))
            
            # @ overal p-value
            test = if(log==TRUE){
                anova(fit,test='LRT')
            }else{anova(fit)}
            
            logical_row = grepl(model_variables[i],rownames(test))

    
            table[[i_v2]][1,5] = format_pval.table(test[logical_row,5])
            
        
            table[[i_v2]][1,2:4] = ''
            
            table[[i_v2]][2:nrow(table[[i_v2]]),1] = ''
            
            table[[i_v2]][2:nrow(table[[i_v2]]),2] = levels(data[,model_variables[i]]) 
            
            table[[i_v2]][2,3:5] = '#'
            
            spfc_smmry = grepl(model_variables[i],rownames(summary_table))
            
            table[[i_v2]][3:nrow(table[[i_v2]]),3] = round(
                summary_table[spfc_smmry,1],roundings)
            
            ci = suppressMessages(confint(fit))
            
            if(log==TRUE){
                ci=exp(ci)
            }
            
            lower_ci = unname(round(ci[spfc_smmry,1],roundings))
            upper_ci = unname(round(ci[spfc_smmry,2],roundings))
            
            table[[i_v2]][3:nrow(table[[i_v2]]),4] = paste0(lower_ci,';',upper_ci)
            
            
            table[[i_v2]][3:nrow(table[[i_v2]]),5] = sapply(1:sum(spfc_smmry),
                function(x){
                format_pval.table(summary_table[spfc_smmry,4][x])
            
            })
            
        
        }else{
            
            table[[i_v2]] = as_tibble(matrix(NA,1,5))
            
            names(table[[i_v2]]) = c('Variable','Value',
                ifelse(log==TRUE,'Odds Ratio','Estimate'),'95% CI',
                'P-value')
            
            table[[i_v2]][1,1] = ifelse(label(data[,model_variables[i]])=='',
                model_variables[i],label(data[,model_variables[i]]))
            
            table[[i_v2]][1,2] = ''
            
            spfc_smmry = grepl(model_variables[i],rownames(summary_table))
            
            table[[i_v2]][1,3] = round(summary_table[spfc_smmry,1],roundings)
            
            ci = suppressMessages(confint(fit))
            
            if(log==TRUE){
                ci=exp(ci)
            }
            
            lower_ci = unname(round(ci[spfc_smmry,1],roundings))
            upper_ci = unname(round(ci[spfc_smmry,2],roundings))
            
            table[[i_v2]][1,4] = paste0(lower_ci,';',upper_ci)
            

            table[[i_v2]][1,5] = format_pval.table(summary_table[spfc_smmry,4])
            
        }
    }
    return(do.call('rbind',table))
}







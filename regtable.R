# ##############################################################################
# function to make tables for lm and glm objects
# koen.vanbrabant@kuleuven.be
# date: 30/03/2017
################################################################################

library(tibble)

regtable = function(fit=fit,log=FALSE,caption=caption){
    variables = attr(fit$terms,'term.labels')

    estimates = round(sapply(1:(length(variables)+1),
        function(x){ifelse(log==FALSE,unname(summary(fit)$coef[x,1]),
        exp(unname(summary(fit)$coef[x,1])))}),3)

    if (log==FALSE){
        confint_v1 = confint(fit)
    }
    if(log==TRUE){
        confint_v1 = exp(confint(fit))
    }


    confint = paste0(
        round(confint_v1[,1],2),
        ';',
        round(confint_v1[,2],2))

    p_value = sapply(1:length(estimates),function(x){format_pval.table(
        summary(fit)$coef[x,4])})

    variable_names = c('(intercept)',unname(sapply(variables,function(x){
        ifelse(label(fit$model[,x])=='',x,label(fit$model[,x]))
    })))
    
    hold.df = data.frame(
        variable_names,
        estimates,
        confint,
        p_value
    )
    
    hold.df_v2 = as_tibble(hold.df)
    names(hold.df_v2) = c('Variables','Estimates','95% CI','p-value')
    return(hold.df_v2)
    
}




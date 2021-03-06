# Bayesian Binomial tests for formstack data
library(magrittr)
library(foreach)
library(doParallel)

#source functions
source("src/functions/binom_bayes_funcs.R")
source("src/creds.R")

new.cols <- c("submit_time", "info_found", "problem_desc", "site", "content_author",
              "child_content_author", "referrer", "ip_addr", "id", "long", "lat",
              "browser", "os")

# parameters for the prior
PRIOR.MEAN <- .8
PRIOR.N.SITE <- 1000
PRIOR.N.PAGE <- 10


#### IMPORT DATA ####
formstack.master <- readr::read_csv("data/formstack/formstack_master.csv") %>%
  dplyr::rename_(.dots = setNames(object = paste0("`", names(.), "`"), nm = new.cols)) %>%
  purrr::map_if(is.character, stringr::str_trim) %>%
  data.frame() %>%
  dplyr::filter(ip_addr != ip.range,
                info_found %in% c("Yes", "No"),
                referrer != "http://<!--") %>%
  dplyr::mutate(info_found = droplevels(info_found),
                referrer = droplevels(referrer))

rm(new.cols)

#### RESPONSE SUMMARIES ####
# summaries of responses including proportion affirmative, and standard error (binomial)

# group by site
response.summary.site <- formstack.master %>%
  dplyr::filter(!is.na(site)) %>%
  dplyr::group_by(site) %>%
  dplyr::summarise(n_affirmative = sum(info_found == "Yes", na.rm = T),
                   n_negative = sum(info_found == "No", na.rm = T),
                   n_total_responses = n()) %>%
  dplyr::mutate(site = droplevels(site))

# group by page
response.summary.page <- formstack.master %>%
  dplyr::filter(!is.na(referrer)) %>%
  dplyr::group_by(referrer) %>%
  dplyr::summarise(n_affirmative = sum(info_found == "Yes", na.rm = T),
                   n_negative = sum(info_found == "No", na.rm = T),
                   n_total_responses = n()) %>%
  dplyr::filter(n_total_responses > 1) %>%
  dplyr::mutate(referrer = droplevels(referrer))


#### BAYESIAN MODELLING ####
cl <- makeForkCluster(4)
registerDoParallel(cl)

response.bayes.site <- foreach(interest.site = iter(response.summary.site$site)) %dopar% {
  interest.pop = response.summary.site[response.summary.site$site == interest.site, ]
  posterior = betaPosterior(interest.pop,  prior.mean = PRIOR.MEAN,
                             prior.n = PRIOR.N.SITE,
                             sample.n = "n_total_responses",
                             affirm.n = "n_affirmative")
  posterior.mean = betaPosteriorMean(interest.pop, prior.mean = PRIOR.MEAN,
                                       prior.n = PRIOR.N.SITE,
                                       sample.n = "n_total_responses",
                                       affirm.n = "n_affirmative")
  cred.int = emdbook::ncredint(pvec = posterior$domain, npost = posterior$prob_dens,
                    level = .95, tol = 0.01, verbose = FALSE)
  prior.variance = betaVariance(prior.mean = PRIOR.MEAN, prior.n = PRIOR.N.SITE, prior = T)
  posterior.variance = betaVariance(df = interest.pop,
                                    intereprior.mean = PRIOR.MEAN,
                                    prior.n = PRIOR.N.SITE,
                                    sample.n = "n_total_responses",
                                    affirm.n = "n_affirmative",
                                    prior = F)
  list("site" = interest.site,
       "posterior" = posterior,
       "posterior_mean" = posterior.mean,
       "credible_interval" = cred.int,
       "prior_variance" = prior.variance,
       "posterior_variance" = posterior.variance
       )
}

response.bayes.page <- foreach(interest.page = iter(response.summary.page$referrer),
                               .errorhandling = "remove") %dopar% {
  interest.pop = response.summary.page[response.summary.page$referrer == interest.page, ]
  posterior = betaPosterior(interest.pop,  prior.mean = PRIOR.MEAN,
                             prior.n = PRIOR.N.PAGE,
                             sample.n = "n_total_responses",
                             affirm.n = "n_affirmative")
  posterior.mean = betaPosteriorMean(interest.pop, prior.mean = PRIOR.MEAN,
                                       prior.n = PRIOR.N.PAGE,
                                       sample.n = "n_total_responses",
                                       affirm.n = "n_affirmative")
  cred.int = emdbook::ncredint(pvec = posterior$domain, npost = posterior$prob_dens,
                               level = .95, tol = 0.01, verbose = FALSE)
  prior.variance = betaVariance(prior.mean = PRIOR.MEAN, prior.n = PRIOR.N.SITE, prior = T)
  posterior.variance = betaVariance(df = interest.pop,
                                    prior.mean = PRIOR.MEAN,
                                    prior.n = PRIOR.N.SITE,
                                    sample.n = "n_total_responses",
                                    affirm.n = "n_affirmative",
                                    prior = F)
  list("page" = interest.page,
       "posterior" = posterior,
       "posterior_mean" = posterior.mean,
       "credible_interval" = cred.int,
       "prior_variance" = prior.variance,
       "posterior_variance" = posterior.variance
       )
}

# stop the cluster
stopCluster(cl)
gc()

#### SAVE THE DATA ####
rm(formstack.master, response.summary.page, response.summary.site, cl, PRIOR.MEAN, PRIOR.N.PAGE,
   PRIOR.N.SITE, beta.posterior, beta.posterior.mean, createTimeBucket)

saveRDS(response.bayes.site, "data/response.bayes.site.RDS")
saveRDS(response.bayes.page, "data/response.bayes.page.RDS")

rm(list = ls())
gc()
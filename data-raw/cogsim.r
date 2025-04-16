################################################################################
## SETUP ##
################################################################################

library(data.table)
library(nnet)
library(rms, include.only = "rcs")

# ADNI data is private, so the ADNI_PATH environment variable points to a
# local directory that needs to be set by the user.
adnimerge <-
  fread(file.path(Sys.getenv("ADNI_PATH"), "ADNIMERGE_01Nov2024.csv")) |>
  _[, .(RID, DX, mmse = MMSE_bl, moca = MOCA_bl)] |> # select/rename variables
  _[DX != ""] |> # drop missing diagnoses
  na.omit()

# Retrieve baseline observation for each individual
adni_bl <- adnimerge[rowid(RID) == 1]

# Collapse CN/MCI into a binary indicator of a dementia diagnosis
adni_bl[, dementia := as.numeric(DX == "Dementia")]


################################################################################
## SIMULATE DATA ##
################################################################################

set.seed(1066)
N <- nrow(adni_bl)

cogsim <- data.table(subid = seq_len(N))

# Dementia
PROB_DEMENTIA <- mean(adni_bl$dementia)
cogsim[, dementia := rbinom(N, 1, prob = PROB_DEMENTIA)]

# MMSE
PROB_MMSE_D0 <- prop.table(table(adni_bl[dementia == 0, mmse]))
PROB_MMSE_D1 <- prop.table(table(adni_bl[dementia == 1, mmse]))

cogsim[dementia == 0, mmse := {
  sample(
    as.integer(names(PROB_MMSE_D0)),
    size = .N,
    replace = TRUE,
    prob = PROB_MMSE_D0
  )
}]

cogsim[dementia == 1, mmse := {
  sample(
    as.integer(names(PROB_MMSE_D1)),
    size = .N,
    replace = TRUE,
    prob = PROB_MMSE_D1
  )
}]

## check
par(mfrow = c(1, 2))
hist(cogsim[dementia == 0, mmse])
hist(cogsim[dementia == 1, mmse])

# MOCA

## choose model with lowest AIC
moca_fmls <- list(formula(moca ~ dementia * factor(mmse)),
                  formula(moca ~ dementia + factor(mmse)),
                  formula(moca ~ dementia + rms::rcs(mmse)),
                  formula(moca ~ dementia * mmse),
                  formula(moca ~ dementia + mmse))

moca_fits <- lapply(moca_fmls, nnet::multinom, data = adni_bl, maxit = 1000)
cat("all fits converged:", all(sapply(moca_fits, \(x) x$convergence) == 0), "\n")

moca_aics <- sapply(moca_fits, AIC)
CHOOSE_FIT <- which(moca_aics == min(moca_aics))
cat("chosen model:", deparse(moca_fmls[[CHOOSE_FIT]]), "\n")

cogsim[, moca := {
  PROBS <- predict(
    moca_fits[[CHOOSE_FIT]],
    newdata = data.table(dementia = dementia, mmse = mmse),
    type = "probs"
  )
  sample(as.integer(names(PROBS)), size = 1, prob = PROBS)
}, keyby = subid] # does the operation rowwise

## compare the original and simulated data
par(mfrow = c(1, 2))
adni_bl[, plot(moca ~ mmse,
               main = "ADNIMERGE",
               col = fcase(dementia == 0, "red",
                           dementia == 1, "blue"))]
cogsim[, plot(moca ~ mmse,
              main = "Simulated Data",
              col = fcase(dementia == 0, "red",
                          dementia == 1, "blue"))]

# Write simulated dataset to data/
usethis::use_data(cogsim, overwrite = TRUE)

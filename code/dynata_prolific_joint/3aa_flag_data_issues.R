source(here::here('code', 'setup.R'))

# Load the data set----

df <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_clean_variables.parquet"
))

# Data Issues----
df <- df %>%
  mutate(
    issue_trap = case_when(
      attitudes_2_a_attention_check_agree != "strongly_agree" ~ 1,
      T ~ 0
    )
  )
table(df$issue_trap)

likert_cols <- df %>%
  select(starts_with("attitudes"), -"attitudes_2_a_attention_check_agree") %>%
  colnames()

## entropy ----
compute_entropy <- function(x) {
  tab <- table(x) / length(x) # probabilities
  -sum(tab * log(tab + 1e-10)) # avoid log(0)
}
df$issue_entropy <- apply(df[, likert_cols], 1, compute_entropy)

## straightlining ----
df$issue_straightlining_var <- apply(df[, likert_cols], 1, var, na.rm = TRUE)
df$issue_straightlining_sd <- apply(df[, likert_cols], 1, sd, na.rm = TRUE)

## Within-Respondent Variance ----
df$issue_within_var <- df$issue_straightlining_var
df$issue_within_var_scaled <- apply(df[, likert_cols], 1, function(x) {
  var(x, na.rm = TRUE) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE) + 1e-10)
})
df$issue_within_mad <- apply(df[, likert_cols], 1, function(x) {
  mean(abs(x - mean(x, na.rm = TRUE)), na.rm = TRUE)
})

# Matrix of responses
X <- as.matrix(df[, likert_cols])

# Mean vector and covariance matrix
mu <- colMeans(X, na.rm = TRUE)
Sigma <- cov(X, use = "pairwise.complete.obs")

# Mahalanobis distance
df$mahalanobis <- mahalanobis(X, center = mu, cov = Sigma)

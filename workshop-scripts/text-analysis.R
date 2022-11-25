# European Society of International Law (ESIL)
# Graduate Institute of International and Development Studies (IHEID)
# Social Science Methods for Legal Scholars
# Quantitative Text Analysis
# Joshua C. Fjelstul, Ph.D.

# Install packages
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("patchwork")
# install.packages("tidytext")
# install.packages("quanteda")
# install.packages("quanteda.textstats")
# install.packages("quanteda.textmodels")
# install.packages("seededlda")
# devtools::install_github("jfjelstul/ggminimal")

# Load packages
library(tidyverse)
library(lubridate)
library(patchwork)
library(tidytext)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)
library(seededlda)
library(ggminimal)

# Read in data -----------------------------------------------------------------

# All judgments delivered by the Court of Justice between 1 January, 2019
# and 12 December, 2021
load("data/text_corpus.RData")

# Select variables
text_corpus <- text_corpus |>
  select(
    ecli, text
  )

# This is "tidy" data:
# 1. Each variable is a column
# 2. Each observation is a row
# 3. Each unit of observation is a table

# Code the judge-rapporteur ----------------------------------------------------

# Let's try a simple regular expression first
judge_rapporteurs <- text_corpus |>
  filter(str_detect(text, "^composed of")) |>
  mutate(
    judge_rapporteur = text |>
      str_extract("[A-Z]\\. [[:alpha:]]+ \\(Rapporteur\\)") |>
      str_remove("\\([Rr]apporteur\\)") |>
      str_remove("^[A-Z]\\.") |>
      str_squish()
  )

# Check how many are missing
table(is.na(judge_rapporteurs$judge_rapporteur))

# What are the problems?
# Some names have spaces
# One initial doesn't have a period after them
# One judgment has a comma before "(Rapporteur)"
# Some judgments have "(Rapporteure)"
# Some judges have ", President of the Chamber" before "Rapporteur"

# Now let's make it flexible enough to work for all judgments
judge_rapporteurs <- text_corpus |>
  filter(str_detect(text, "^composed of")) |>
  mutate(
    judge_rapporteur = text |>
      str_extract("[A-Z](\\.)? [[:alpha:]' ]+(, President of the Chamber)?,? *\\([Rr]apporteure?\\)") |>
      str_remove("\\([Rr]apporteure?\\)") |>
      str_remove(", President of the Chamber") |>
      str_remove("^[A-Z](\\.)?") |>
      str_remove(",") |>
      str_squish()
  )

# Check how many are missing
table(is.na(judge_rapporteurs$judge_rapporteur))

# Check values to make sure the names are clean
unique(judge_rapporteurs$judge_rapporteur)

# Now we can merge in the judge-rapporteurs based on the ECLI number
text_corpus <- text_corpus |>
  left_join(
    judge_rapporteurs |>
      select(ecli, judge_rapporteur),
    by = "ecli"
  ) |>
  filter(
    !is.na(judge_rapporteur)
  )

# Prepare the text for analysis ------------------------------------------------

# Create a "tidy" text corpus
tidy_text_corpus <- text_corpus |>
  group_by(
    ecli, judge_rapporteur
  ) |>
  unnest_tokens(
    output = "word",
    input = text,
    token = "words",
    to_lower = TRUE
  )

# Remove stop words, short words, numbers, and words with punctuation
tidy_text_corpus <- tidy_text_corpus |>
  anti_join(
    get_stopwords(),
    by = "word"
  ) |>
  filter(
    !str_detect(word, "[0-9]")
  ) |>
  filter(
    str_length(word) >= 3
  ) |>
  filter(
    !str_detect(word, "[[:punct:]]")
  )

# Make a list of procedural words to remove
to_remove <- tibble(
  word = c(
    "article", "court", "paragraph", "judgment", "case",
    "proceedings", "apeal", "application",
    "directive", "regulation", "law",
    "member", "state", "states", "commission", "european", "union"
  )
)

# Remove words
tidy_text_corpus <- tidy_text_corpus |>
  anti_join(
    to_remove,
    by = "word"
  )

# Frequency analysis -----------------------------------------------------------

# Identify the most common words
counts <- tidy_text_corpus |>
  group_by(word) |>
  summarize(count = n()) |>
  arrange(desc(count))

# Plot most frequent words
plot <- tidy_text_corpus |>
  group_by(word) |>
  summarize(count = n()) |>
  arrange(desc(count)) |>
  slice_head(n = 25) |>
  mutate(
    word = word |>
      factor() |>
      fct_reorder(count)
  ) |>
  ggplot() +
  geom_bar(aes(x = word, y = count), stat = "identity", color = "black", fill = "gray90", width = 0.7) +
  scale_y_continuous(breaks = seq(0, 30000, 5000), expand = expansion(mult = c(0, 0.1))) +
  coord_flip() +
  labs(
    title = "Most frequent words in CJEU judgments (2019-2021)",
    x = NULL,
    y = "Frequency"
  ) +
  theme_minimal()

# Plot most frequent words
plot <- tidy_text_corpus |>
  group_by(word) |>
  summarize(count = n()) |>
  arrange(desc(count)) |>
  slice_head(n = 25) |>
  mutate(
    word = word |>
      factor() |>
      fct_reorder(count)
  ) |>
  ggplot() +
  geom_segment(aes(x = word, xend = word, y = 0, yend = count), color = "gray90") +
  geom_point(aes(x = word, y = count), size = 3, color = "#3498db") +
  scale_y_continuous(breaks = seq(0, 30000, 5000), expand = expansion(mult = c(0, 0.1))) +
  coord_flip() +
  labs(
    title = "Most frequent words in CJEU judgments",
    x = NULL,
    y = "Frequency"
  ) +
  theme_minimal()

# Plot frequency by rank
# Zipf's law: A word's frequency is inversely proporational to its rank.
# The word at rank n appears 1/n times as often as the most frequent one.
plot <- tidy_text_corpus |>
  group_by(word) |>
  summarize(count = n()) |>
  arrange(desc(count)) |>
  ungroup() |>
  mutate(
    rank = row_number(),
    frequency = count / sum(count)
  ) |>
  ggplot() +
  geom_line(aes(x = rank, y = frequency), size = 1, color = "#3498db") +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Zipf's law for CJEU judgments",
    x = "Rank",
    y = "Frequency"
  ) +
  theme_minimal()

# Document feature matrix ------------------------------------------------------

# Create a DFM
dfm <- tidy_text_corpus |>
  group_by(word, ecli) |>
  summarize(count = n()) |>
  arrange(desc(count)) |>
  cast_dfm(
    document = ecli,
    term = word,
    value = count
  )

# Check the dimensions
dim(dfm)

# Trim the DF
dfm_trimmed <- dfm |>
  dfm_trim(min_termfreq = 500)

# Check the dimensions
dim(dfm_trimmed)

# Topic model ------------------------------------------------------------------

# Estimate a model with 2 categories
topic_model_2 <- textmodel_lda(dfm_trimmed, k = 2)
save(topic_model_2, file = "models/topic_model_2.RData")
load("models/topic_model_2.RData")

# See the top 20 tokens that define each topic
terms(topic_model_2, n = 50)

# Estimate a model with 10 categories
topic_model_10 <- textmodel_lda(dfm_trimmed, k = 10)
save(topic_model_10, file = "models/topic_model_10.RData")
load("models/topic_model_10.RData")

# See the top 20 tokens that define each topic
terms(topic_model_10, n = 20)

# Estimate a seeded model
dictionary <- dictionary(file = "workshop-scripts/topics.yml")
seeded_topic_model <- textmodel_seededlda(dfm_trimmed, dictionary, residual = TRUE)
save(seeded_topic_model, file = "models/seeded_topic_model.RData")
load("models/seeded_topic_model.RData")

# See the top 20 tokens that define each topic
terms(seeded_topic_model, n = 40)

## Interpret estimates ---------------------------------------------------------

# Make a tibble with estimates
topic_estimates <- seeded_topic_model$theta |>
  as_tibble() |>
  mutate(
    ecli = rownames(dfm_trimmed),
  ) |>
  left_join(
    judge_rapporteurs |>
      select(
        ecli, judge_rapporteur
      ),
    by = "ecli"
  ) |>
  group_by(judge_rapporteur) |>
  summarize(
    count = n(),
    economy = mean(economy),
    social = mean(social),
    tax = mean(tax),
    ip = mean(ip),
    competition = mean(competition)
  ) |>
  ungroup() |>
  mutate(
    judge_rapporteur = judge_rapporteur |>
      factor()
  ) |>
  filter(
    count >= 10
  )

# Plot average positions by judge-rapporteur for the taxation topic
panel_1 <- ggplot(topic_estimates, aes(x = tax, y = fct_reorder(judge_rapporteur, tax), size = count)) +
  geom_point(color = "#3498db") +
  scale_size_continuous(range = c(1, 4), guide = "none") +
  ggtitle("Taxation topic") +
  ylab(NULL) +
  xlab("Average estimated probability") +
  theme_minimal()

# Plot average positions by judge-rapporteur for the intellectual property topic
panel_2 <- ggplot(topic_estimates, aes(x = ip, y = fct_reorder(judge_rapporteur, ip), size = count)) +
  geom_point(color = "#3498db") +
  scale_size_continuous(range = c(1, 4), name = "Number of judgments") +
  ggtitle("Intellectual property topic") +
  ylab(NULL) +
  xlab("Average estimated probability") +
  theme_minimal()

# Combine panels
plot <- panel_1 + panel_2 + plot_layout(nrow = 1, ncol = 2)

# Save plot
ggsave(plot, filename = "plots/judge_topics.png", device = "png", width = 10, height = 6, scale = 1.25)

# Wordfish model ---------------------------------------------------------------

# Estimate model
wordfish_model <- textmodel_wordfish(dfm_trimmed)
save(wordfish_model, file = "models/wordfish_model.RData")
load("models/wordfish_model.RData")

# There are 4 parameters
# theta = estimated document positions
# beta = estimated word positions
# alpha = estimated document fixed effect (some documents are just longer than others)
# psi = estimated word fixed effect (some words are just more common)

## Interpret word estimates ----------------------------------------------------

# List of procedure-oriented words
procedural_words <- c(
  "appeal", "appellant", "unsuccessful", "error", "plea", "pleas", "unfounded",
  "erred", "annulment", "infringement", "arguments", "alleging"
)

# List of policy-oriented words
policy_words <- c(
  "worker", "workers", "social", "pension", "taxation", "income", "credit",
  "employ", "contract", "person", "work", "tax", "service", "insurance",
  "consumer", "employee", "citizen", "passenger", "passengers", "loan",
  "family", "child", "residence"
)

# Make a tibble with the word parameters
word_estimates <- tibble(
  word = colnames(dfm_trimmed),
  position = wordfish_model$beta,
  fixed_effect = wordfish_model$psi,
)

# Word type
word_estimates <- word_estimates |>
  mutate(
    word_type = case_when(
      word %in% procedural_words ~ "Procedural word",
      word %in% policy_words ~ "Policy word",
      TRUE ~ "Other"
    ),
    word_type = factor(
      word_type,
      levels = c("Policy word", "Procedural word", "Other")
    )
  )

# We can graph these to interpret the latent dimension
plot <- ggplot(word_estimates, aes(x = position, y = fixed_effect, label = word, color = word_type, size = word_type)) +
  geom_text(alpha = 0.7) +
  geom_vline(xintercept = 0, size = 0.5, linetype = "dashed") +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  scale_y_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  scale_color_manual(values = c("#3498db", "#2ecc71", "gray80"), name = NULL) +
  scale_size_manual(values = c(4, 4, 2.5), guide = "none") +
  ggtitle("Wordfish estimates for the positions of words in CJEU judgments (2019-2021)") +
  ylab("Fixed effect") +
  xlab("Word position") +
  theme_minimal()

# Save plot
ggsave(plot, filename = "plots/word_positions.png", device = "png", width = 10, height = 10, scale = 1)

## Interpret document estimates ------------------------------------------------

# Create a table of document estimates
document_estimates <- tibble(
  ecli = wordfish_model$docs,
  position = wordfish_model$theta
)

# Merge in judge rapporteur
document_estimates <- document_estimates |>
  left_join(
    judge_rapporteurs |>
      select(
        ecli, judge_rapporteur
      ),
    by = "ecli"
  )

# Collapse by judge
judge_estimates <- document_estimates |>
  group_by(judge_rapporteur) |>
  summarize(
    position = mean(position),
    count = n()
  ) |>
  ungroup() |>
  mutate(
    judge_rapporteur = judge_rapporteur |>
      factor() |>
      fct_reorder(position)
  ) |>
  filter(
    count >= 10
  )

# Plot average positions by judge-rapporteur
plot <- ggplot(judge_estimates, aes(x = position, y = judge_rapporteur, size = count)) +
  geom_point(color = "#3498db") +
  geom_vline(xintercept = 0, size = 0.5, linetype = "dashed") +
  scale_size_continuous(range = c(1, 4), name = "Number of judgments") +
  ggtitle("Wordfish estimates for the positions CJEU judges (2019-2021)") +
  ylab(NULL) +
  xlab("Average judge-rapporteur position") +
  theme_minimal()

# Save plot
ggsave(plot, filename = "plots/judge_positions.png", device = "png", width = 10, height = 10, scale = 1)

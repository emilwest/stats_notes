library(text)

# # Install text required python packages in a conda environment (with defaults).
# text::textrpp_install()
# 
# # Show available conda environments.
# reticulate::conda_list()
# 
# # Initialize the installed conda environment.
# # save_profile = TRUE saves the settings so that you don't have to run textrpp_initialize() after restarting R. 
# text::textrpp_initialize(save_profile = TRUE)

text::textrpp_initialize()

texts <- c("I feel great!")

# Defaults
embeddings <- textEmbed(texts)
embeddings




# View example data including both text and numerical variables
Language_based_assessment_data_8

# Transform the text data to BERT word embeddings
word_embeddings <- textEmbed(
  texts = Language_based_assessment_data_8[3],
  model = "bert-base-uncased",
  layers = -2,
  aggregation_from_tokens_to_texts = "mean",
  aggregation_from_tokens_to_word_types = "mean",
  keep_token_embeddings = FALSE)

word_embeddings


# Pre-process data
projection_results <- textProjection(
  words = Language_based_assessment_data_8$harmonywords,
  word_embeddings = word_embeddings$texts,
  word_types_embeddings = word_embeddings$word_types,
  x = Language_based_assessment_data_8$hilstotal,
  y = Language_based_assessment_data_8$age
)
projection_results$word_data


# Supervised Dimension Projection Plot
# To avoid warnings -- and that words do not get plotted, first increase the max.overlaps for the entire session: 
options(ggrepel.max.overlaps = 1000)

# Supervised Dimension Projection Plot
plot_projection_2D <- textProjectionPlot(
  word_data = projection_results,
  min_freq_words_plot = 1,
  plot_n_word_extreme = 10,
  plot_n_word_frequency = 5,
  plot_n_words_middle = 5,
  y_axes = TRUE,
  p_alpha = 0.05,
  p_adjust_method = "fdr",
  title_top = "Harmony Words Responses (Supervised Dimension Projection)",
  x_axes_label = "Low vs. High Harmony in Life Scale Score",
  y_axes_label = "Low vs.High Age",
  bivariate_color_codes = c("#E07f6a", "#60A1F7", "#85DB8E",
                            "#FF0000", "#EAEAEA", "#5dc688",
                            "#E07f6a", "#60A1F7", "#85DB8E"
  ))
# View plot
plot_projection_2D$final_plot




# Examine the relationship between harmonytext word embeddings and the harmony in life rating scale
model_htext_hils <- textTrain(word_embeddings$texts$harmonywords, 
                              Language_based_assessment_data_8$hilstotal)

# Examine the correlation between predicted and observed Harmony in life scale scores
model_htext_hils$results
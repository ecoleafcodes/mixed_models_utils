library(dplyr)
library(lme4)
library(ggplot2)


plot_random_effects <- function(model, random_effect_group, label = NULL) {
    # Extract the random effects for the specified group
    random_effects <- ranef(model)[[random_effect_group]]

    if (is.null(random_effects)) {
        stop("Random effect group not found in the model.")
    }

    # Set the label if not provided
    if (is.null(label)) {
        label <- random_effect_group
    }

    # Create a data frame with random effects and the corresponding levels
    random_effects_df <- as.data.frame(random_effects) %>%
        mutate(Level = rownames(random_effects)) %>%
        rename(Effect = `(Intercept)`)

    # If there are more than 40 levels, filter to get the top 20 and bottom 20
    if (nrow(random_effects_df) > 40) {
        random_effects_df <- random_effects_df %>%
            arrange(Effect) %>%
            slice(c(1:20, (n() - 19):n()))  # Select the lowest 20 and highest 20
    }

    # Plot using ggplot2
    ggplot(random_effects_df, aes(x = reorder(Level, Effect), y = Effect)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + # Horizontal line at zero
        geom_segment(aes(
            x = reorder(Level, Effect), xend = reorder(Level, Effect),
            y = 0, yend = Effect
        ),
        color = "blue", size = 1
        ) +
        coord_flip() +
        labs(x = label, y = "Random Effect (Estimate)") +
        theme_minimal()
        
}
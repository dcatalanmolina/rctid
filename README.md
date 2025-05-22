# RCTID

Code to get and analyze MLS stats. RCTID stands for 'Rose City 'til I Die', for all of us Portland Timbers fans. 

## Get and Clean Data

`get_2024_football_data.R` gets 2024 matches and season-level stats for each team.

`get_2025_football_data.R` gets 2025 matches and season-level stats for each team.

`clean_football_data.R` cleans 2024 and 2025 data to prep for model predictions.

## Model Predictions

`mls_model_v01.R` runs matchup models.

`predict_matches.R` uses model outputs to predict goal differences for each match and expected standings.

`communicate_predictions.qmd` renders an HTML summarizing match and standing predictions.
# Replication Summary

## Paper 10 — Selecting the Most Effective Nudge

**Result: Replicated with minor differences.**

The deterministic empirical results reproduced successfully.

The three substitution regression tables match the original on coefficients, standard errors, significance stars, control means, and sample sizes. `descriptive_statistics.txt` matches on all reported values, with only line-ending differences across operating systems. The available `shots_per_dollar_best_policy_WC_adjusted.txt` result is identical to the original.

The uninteracted treatment-effect figures, saturated OLS results, policy-profile plots, and heatmaps reproduce the original estimates, confidence intervals, pruning/pooling classifications, colors, and significance markers. Minor cosmetic differences include legend ordering and regenerated LaTeX timestamps.

The substantive differences are concentrated in the stochastic bootstrap analysis:

- For `shot_Measles1`, best-policy selection accuracy is **0.77** in both the original and the replication.
- For `shots_per_dollar`, best-policy selection accuracy is **0.955** in the replication versus **0.96** in the original.
- The selected best policy for shots per dollar is unchanged.
- The replicated shots-per-dollar bootstrap realizes one additional support category.
- Bootstrap-support colors differ because the cleaned plotting code dynamically assigns colors.
- The original shots-per-dollar winning-policy x-axis label is red-highlighted, while the cleaned replication does not reproduce that label formatting.

The cleaned workflow additionally generates:

- `Heatmap_noSeed_Seed_shot_Measles1.pdf`
- `postLASSO-costeffectiveness_COEF.png`

The original `shot_Measles1_best_policy_WC_adjusted.txt` was not included in the final validation comparison set, so it is recorded as **not checked**.

The methodological simulation exercises from the original `Code/Simulation/` directory are outside the scope of this replication.

## Final assessment

**The substantive empirical conclusions are reproduced.**

The replication is therefore classified as:

**Replicated with minor stochastic and cosmetic differences.**

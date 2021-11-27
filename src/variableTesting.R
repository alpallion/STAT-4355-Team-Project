## This file is used to measure the impact each variable as on the full model
## Using the partial F test
## H_0: No significant difference in SSE of full and reduced models.
## The models do not differ significantly.
## H_1: Full model has significantly lower SSE than the reduced model.
## The full model is significantly better than the reduced model
## The larger the value of the test statistic, the larger the change in SSE,
## or the larger the difference between the two models
## anova command takes in the reduced model first and then the full model

fit <- dummy_model_1
full_model <- fit
reduced_model <- lm(SalePrice ~ GarageArea +GrLivArea +TotalBsmtSF +YearBuilt, data = dummy_df_1)
anova(reduced_model,full_model)

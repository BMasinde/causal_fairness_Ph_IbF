# Causal Models and Counterfactuals to Detect Biases in Predicting Impact of Tropical Cyclones in the Philippines.


## Description:
This project aims to use causal reasoning and models to identify potential biases while using machine learning to predict impact of tropical cyclones in the Philippines. The concerns for biases come from the spatial opposing gradients problem whereby the Northern region of the Philippines expereinces more tropical cyclones compared to the southern regions while southern regions exhibit more socio-economic vulnerability (Baldwin et al., 2023) and also increasing housing vulnerability because of building typologies used (Healey et al., 2022). 

We implement three models, two causal models based on directed acyclic graphs (DAGs) and structural causal models (SCMs), and one traditonal associational model based on XGBoost. One causal model is adjusted for the regional confounder that accounts for the spatial opposing gradients problem. The unadjusted SCM model is a causal surrogate of the associationall XGBoost model.


## Results

### Classification (damage => 10 %)
These are the results to Table 2 in the manuscript. 

Classification metrics for adjusted SCM:

├── adjusted SCM/

│	├── xgb classifier and training/

│	│	└── adj_scm_testing_xgb_classifier.pdf

Classification metrics for unadjusted SCM:

├──  unadjusted SCM/

│	└── unadj_scm_testing_xgb_classifier.pdf

Classification metrics for associational XGBoost:

├──  associational XGBOOST/

│	└── model___training_xgb_classifier.pdf


### Hurdle Testing: Binned metrics
These are the resutls for Table 4 in the manuscript

Binned metrics for adjusted SCM:

├──  adjusted SCM/

│	├──  hurdle testing/

│	│	└── adj_scm_hurdle_testing.pdf


Binned metrics for unadjusted SCM:

├── unadjusted SCM/

│	└── unadj_scm_hurdle_testing.pdf


Binned metrics for associational XGBoost:

├── associational XGBOOST/

│	└── model___hurdle_testing.pdf


### Counterfactuals
Results to Table 5 counterfactuals based on clusters:

adjusted SCM Table 5 results:
├── adjusted SCM/

│	├── counterfactuals/

│	│	└── adj_scm_counterfactual2.pdf


unadjusted SCM Table 5 results:
├──  unadjusted SCM/

│	└── unadj_scm_counterfactual_2.pdf


associtional XGBoost Table 5 results:
├──  associational XGBOOST/

│	└── ass___counterfactual_testing2.pdf


Results for Table 6 to 



## References:
1. Baldwin, J. W., Lee, C. Y., Walsh, B. J., Camargo, S. J., & Sobel, A. H. (2023). Vulnerability in a tropical cyclone risk model: Philippines case study. Weather, climate, and society, 15(3), 503-523.

2. Healey, S., Lloyd, S., Gray, J., & Opdyke, A. (2022). A census-based housing vulnerability index for typhoon hazards in the Philippines. Progress in disaster science, 13, 100211.
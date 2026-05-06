# stat5243-project4
All datasets we used are in the file "datasets"
The raw data of fraudTest and poverty rate are large which cannot be uploaded. In this case, we uploaded the dataset of first 100 rows and you can see it in the dataset file. The link of Kaggle data fraudTestis shown below
https://www.kaggle.com/datasets/kartik2112/fraud-detection/data?select=fraudTrain.csv

The cleaned dataset (our final dataset) called fraudTest_clean.csv is already uploaded. 

The project includes introduction, data cleaning and preprocessing, EDA, feature engineering, modeling and validation. Our goal is to examine the relationships between credit card and features. In addition to the report, we make a dashboard which may give a quick view of the whole project. The link is shown below: 
https://mia123.shinyapps.io/fraud-detection-dashboard/

When it comes to running the code, since the cleaned data fraudTest_clean.csv is uploaded and raw datasets are too large, you may directly load the cleaned data and skipped data cleaning parts (2-5) and directly run the code beginning with part 6, after you run imports part, which is part 1.

In the code file, the first part is Imports which imports all the required packages.

For the second part, we uploaded our dataset and dropped unused columns.

3-5 is data cleaning and preprocessing. We recoded all the US states to 4 regions, and then generated feature "age" and "late night" based on other existed features. At last, we chose amount which larger or equal to 600, and handled missing value.

6-10 is EDA. We visualized the data by creating barplot, boxplots, contingency tables, correlation heatmap, and did PCA.

11 is feature engineering, which related to zero variance predictor and one-hot encoding.

12-15 is splitting the data and modeling part. We got the data of models, such as coefficients of logit model, importance score of decision tree & XGBoost, correlation matrix and so on.

16 is model comparison. There is plot related to accuracy and other validation metrics, also ROC Curve.

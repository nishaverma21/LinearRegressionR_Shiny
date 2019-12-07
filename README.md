# LinearRegressionR_Shiny
Linear Regression Model on R Shiny Web Application

Application Tab Details :

1. Dashboard is containing Model Result,Model summary,Model F Static, Model T Static and Fitted plot. 
   User can click on action button "Click Here" to generate all of these but after completing all other tabs of application. 

2. Navigate to Upload Tab to upload any .csv file

3. Navigate to Table Tab to view data ( First few Rows)

4. Navigate to Summary Tab to view summary of data

5. Navigate to Plots to generate different plots (Box,Scatter,Bar Graph) on data uploaded 
    1. select Variable 1 and Variable 2
    2. select plot type
    3. select Legends
    View graph on main panel

6. Navigate to clean data tab, this tab is used to remove rows from main dataframe.
    1. select variable ( this contain list of all continous variables of dataframe)
    2. select airthmetic operator you want to apply to clean dataframe as per need, "Equal" -> "=", "Less"->"<" and 
       "Greater"-  > ">"
    3. input value ( numeric ) to provide information to application which data need to be removed.
    4. click on "Clean Data" action button
    5. click on "Data Summary" to see current summary of dataframe

7. Navigate to Model Tab to generate Linear regression model
   1. select % of data user want to pass to train model
   2. select target from dropdown(only continous variables are available in target tab)
   3. select multiple predictors, variable selected in target will not allowed available in predictors drop down.
   4. click on submit to generate model summary
   5. click on reset if you want to develop model again with differen target and predictors.
   
8. Navigate to Predictions Tab to generate predictions on complete data uploaded
    1. click on Prediction Tab
    2. Traget is populated on this tab to let user know what is predicted from model

9. Navigate to Download Predictions Tab to download Dataframe with actual and predicted values.

NOTE : TO NAVIGATE CODE

Search with NV_UI_TABNAME, you will reach to UI part of code of that tab
Search with NV_SERVER_TABNAME, you will reach to SERVER part of code of that tab

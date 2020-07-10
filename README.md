# Precise-demand-prediction-model-for-new-retail-target-products
本人在2020MathorCup数学建模挑战杯中D题的解决方案及模型代码，最终获得全国二等奖。 
*My solution and model code for the problem D in the 2020 MathorCup Mathematical Modeling Challenge Cup and finally won the national second prize.*

![images](https://github.com/Leo1998-Lu/Precise-demand-prediction-model-for-new-retail-target-products/blob/master/National%20Second%20Prize.PNG)

### 预测方法包括多元线性回归模型、优化后的多元线性回归模型、回归决策树模型、随机森林模型、KNN模型后，对比这五个模型预测结果，以MAPE值作为判断依据，最终选定基于变量是否节日（is_holiday）、所处年份（year_id）、标签价（tag_price）、折扣力度（discount）和上周累计销售额（last_week_cost）的多元线性回归模型作为最优模型，其在周销售额预测的MAPE为0.07159，优于其他模型，可作为企业对销售预测的应用模型。充分挖掘了节日、库存与打折力度对销售情况的影响，找到了隐含的高需求时间段、最优库存与打折带来的业绩提升效果。
*The prediction methods include multiple linear regression model, optimized multiple linear regression model, regression decision tree model, random forest model, and KNN model. The prediction results of these five models are compared, and the MAPE value is used as the basis for judgment. The multiple linear regression model of festival (is_holiday), year (id_year), tag price (tag_price), discount strength (discount) and last week’s cumulative sales (last_week_cost) is used as the optimal model, and its MAPE for weekly sales forecast is 0.07159, which is better than other models and can be used as an application model for enterprises to forecast sales. Fully tapped the impact of festivals, inventory and discount efforts on sales, and found the implied high demand period, optimal inventory and the performance improvement effect brought by discounts.*


## 个人感想：本次比赛结果我个人并不满意，但也是合理的结果，因为我这次只负责了除ARIMA-GARCH模型之外内容，投入在比赛中的时间最多只有15个小时（因为当时个人行程比较满），大约有25%的论文让队友完成， 而不像之前自己一个人全部完成，所以论文质量上看并不可观。
*Personal impression: I am not satisfied with the result of this competition, but it is also a reasonable result, because I am only responsible for the content other than the ARIMA-GARCH model this time, and the time invested in the competition is only up to 15 hours (because of the personal Itinerary is relatively full).*

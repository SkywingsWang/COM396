# COMP396 Final Report
Contributors: Tianyi Wang, Shengying Li, Zheyu Huang, Kechen Shi, Zhangyuan Xu

### Contents
[Section 1: Final choice of submitted strategy](#Section1:Finalchoiceofsubmittedstrategy)  
* [Section 1.1 About the strategy](#Section1.1Aboutthestrategy)
* [Section 1.2 Collaboration of the different parts of the strategy](#Section1.2Collaborationofthedifferentpartsofthestrategy)
* [Section 1.3 Optimising and checking the robustness of your strategy](#Section1.3Optimisingandcheckingtherobustnessofyourstrategy)

[Section 2: Justification of submitted strategy](#Section2:Justificationofsubmittedstrategy)
* [Section 2.1 The reason why we choose this particular strategy and the combination of these sub-strategies](#Section2.1Thereasonwhywechoosethisparticularstrategyandthecombinationofthesesub-strategies)
* [Section 2.2 Justification of the choice of position size and other key elements of the strategy](#Section2.2Justificationofthechoiceofpositionsizeandotherkeyelementsofthestrategy)
* [Section 2.3 Comparison of the final strategy with alternatives](#Section2.3Comparisonofthefinalstrategywithalternatives)
* [Section 2.4 Risk management](#Section2.4Riskmanagement)

[Section 3: Evaluation and analysis of performance on part 3](#Section3:Evaluationandanalysisofperformanceonpart3)
* [Section 3.1 Comparison of the result of the strategy in part 3 with the expected result](#Section3.1Comparisonoftheresultofthestrategyinpart3withtheexpectedresult)
* [Section 3.2 Mistakes (at the technical level mainly, but also in terms of planning and teamwork)](#Section3.2Mistakes(atthetechnicallevelmainly,butalsointermsofplanningandteamwork))
* [Section 3.3 Learning from this module and Improvement](#Section3.3LearningfromthismoduleandImprovement)

[Section 4: Breakdown of team work](#Section4:Breakdownofteamwork)
* [Tianyi Wang](#TianyiWang)
* [Kechen Shi](#KechenShi)
* [Shengying Li](#ShengyingLi)
* [Zheyu Huang](#ZheyuHuang)
* [Zhangyuan Xu](#ZhangyuanXu)

[Reference List](#ReferenceList)

## Section 1: Final choice of submitted strategy

*The final strategy is a diversified strategy including the momentum strategy, the Donchian Channel mean reversion strategy and the 3-factors mean reversion strategy. In this section, the final strategy will be presented in detail.*

### Section 1.1 About the strategy

* **3-factors mean reversion strategy**  

1. **KDJ indicator**  
This indicator is not contained in the design part. However, in the plan part we suggested that we may need more indicators to restrict the decision-making. That is the main reason for this indicator.  

In a sentence, **this indicator is to help us to detect the lowest (or highest) point in a period.** The KDJ contains three elements, K, D and J line respectively. When J line is below 0.2 or is higher than 0.8 we determine that it is a trigger point (Details will be discussed in section 1.3) [1]. Compared with other indicators, KDJ is more accurate in finding the enter point and it has a lot of optional parameters which means we have several approaches to use this indicator [2]. The purpose of this indicator is to enhance the accuracy and eliminate the wrong decision. Therefore, this indicator interacts with the others, and only exists in the if condition.  

2. **CCI indicator**  
The Commodity Channel Index (CCI) is not an indicator that is often used in mean reversion strategies. But in this mean reversion strategy, we use this indicator for **three main reasons**.  
   1. The CCI indicator itself does not have a strong tendency, it can be used for both mean reversion ideas and trend following strategies. Therefore, the CCI indicator can be suitable for mean reversion strategies, as long as its parameters are modified.  
   2. The CCI indicator is mainly calculated from the difference between typical prices (the average of high, low and close prices) and moving averages, which can be viewed as a comprehensive measure of the average prices [3]. Since the core of a mean reversion strategy is to define the “averages” that prices would reverse to, we think it is sensible to use the CCI indicator in a mean reversion strategy. And during the process of making this strategy, the results are found surprisingly promising, so we carried it over.  
   3. The CCI indicator is not directly related to the price of the time series. So, when using the CCI indicator difference within two days for flexible position control, there is no need to worry about being affected by price fluctuations and making the position uncontrolled.   
3. **BBands indicator**  
**The Bollinger Bands (BBands) indicator is used to stop loss when prices of time series exceed the upper band or lower band.** After the position sizing is done by CCI indicator, the BBands indicator will determine whether a stop loss is required. If needed, BBands will fill a market order with the exact opposite amount of all the 3-factors mean reversion strategy's current position.  

We also considered the use of other indicators such as MACD, which is explained in detail in Section 2.3 Alternatives, but ultimately, we believe that the BBands can work well intuitively with our parameter optimisation approaches, especially the two visualisation methods. It is because every change in the parameters of the BBands indicator can be mapped into our visualisations. Another advantage of BBands indicator lies in the fact that as a stop loss indicator, it will have as little impact as possible on day-to-day transactions. As long as the look back of BBands is reduced to make the two bands of BBands more flexible, and the standard deviation is increased to make them wide enough, the impact of the BBands indicator on most normal markets can be reduced as much as possible.  

**Overall, the 3- factors mean reversion strategy consists of three indicators: CCI, KDJ and Bbands.** The first 2 indicators control the decision making while the last one is for stop loss. The purpose of this strategy is to trade in short term perspective with low positions and gain when there is a clear signal for entry. In this way, we could have a low risk but active trading strategy to complete the diversity.   
To be more specific, through the CCI function embedded in the TTR package, this strategy first calculates daily CCI values through daily close prices and CCI related parameters. when the CCI value is lower than the oversold line and the J line crosses the low line we set, we will perform a long operation, and flexibly control the position according to the difference between the CCI values between today and yesterday, and vice versa. Besides, this flexible position control takes the weighted average of prices across different time series into account. Regarding to stop loss, this strategy uses the BBands indicator: when the close price is greater than the upper or lower band of the BBands, a close position operation is performed.

* **Momentum strategy**  
**Our momentum trading strategy is a relatively long-term trend following strategy. The basic idea is that we apply a correlation test on the series,** and if the series passes the test, it means that in the past x days, the return of past y days are positively correlated with the next z days (x, y, z are all parameters set by us). **So if the series passes the test, the strategy will decide whether to long or short according to past y day’s return using limit order. And in the next z days, we will check whether we should hold the position or stop loss every day.**

* **Donchian Channel mean reversion strategy**  
The Donchian Channel part of the strategy is originally a trend-following strategy, following the momentum of the stock price tendency (i.e., if the strategy identifies the trend to be going upward, the strategy thinks that the stock will continue to rise, hence long the position, and vice versa) [4]. However, after testing the data with Donchian Channel strategy, we found that **the mean-reversion way of manipulating the strategy will give a lot more return and PD (5.59 PD in part 1 and 5.3 in part 2) than trend-following.** In terms of the execution of the strategy, it firstly finds the highest and lowest closing price of the series in the last 20 days to be the **upper and lower bound of the Donchian Channel**, and calculates the simple moving average price of the last 3 days. If the SMA line is bigger than the upper line of Donchian Channel, then short the position; if the SMA line is smaller than lower bound, long the position.

### Section 1.2 Collaboration of the different parts of the strategy

Firstly, this strategy can be divided into two components, one of which is **long-term strategies (the momentum strategy and the Donchian Channel mean reversion strategy)** and the other is **short-term strategy (the 3-factors mean reversion strategy)**. Then all series will be run by the strategy, except for series 3 (not be run in the momentum strategy).  
The long-term strategies and the short-term strategy will work simultaneously and will not affect each other. In terms of the long-term strategies, the Donchian Channel mean reversion strategy and the momentum strategy are exclusive to each other.

When the long-term strategies are applied to one series, only the Donchian Channel mean reversion strategy runs until the test length of the momentum strategy is satisfied. To test whether it applies to a trend following strategy or a mean reversion strategy, every series will be conducted by the correlation test. The result will decide which strategy will be operated. So, we can **get a position of the long-term strategy**. Meanwhile, we will **get the other position of the short-term strategy**.

Finally, we **combine these two positions together and get a whole position for this series. Then repeat the above steps for the other series**.  
![Figure 1](/pic/Figure 1.png)

### Section 1.3 Optimising and checking the robustness of your strategy

## Section 2: Justification of submitted strategy
### Section 2.1 The reason why we choose this particular strategy and the combination of these sub-strategies
### Section 2.2 Justification of the choice of position size and other key elements of the strategy
### Section 2.3 Comparison of the final strategy with alternatives
### Section 2.4 Risk management

## Section 3: Evaluation and analysis of performance on part 3
### Section 3.1 Comparison of the result of the strategy in part 3 with the expected result
### Section 3.2 Mistakes (at the technical level mainly, but also in terms of planning and teamwork)
### Section 3.3 Learning from this module and Improvement

## Section 4: Breakdown of team work
### Tianyi Wang
### Kechen Shi
### Shengying Li
### Zheyu Huang
### Zhangyuan Xu

## Reference List

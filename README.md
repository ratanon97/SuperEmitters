# Techno-Economic Assessment of Options to Mitigate Super-Emitters in the Natural Gas Supply Chain

<p align="center">
  <img width="400" height="400" src="https://media.licdn.com/dms/image/C4D0BAQG2VVxQgyx2Wg/company-logo_200_200/0?e=2159024400&v=beta&t=DHxdpvVzqcQXB5e_OiS0wLzyznvjXFw266enGSFcrsM">
</p>

This is the README file of my MSc Advanced Chemical Engineering with Process Systems Engineering's thesis project in the academic year 2018-2019 of Imperial College London

This project was conducted under the [Sustainable Gas Institute](https://www.sustainablegasinstitute.org/) (SGI), part of Imperial College London. SGI incorporates world-class research in the themes of science, engineering, economics, policy and business to support the energy industry in hopes of transitioning into the low-carbon economy. 

This project was supervised by [Dr. Adam Hawkes](https://www.imperial.ac.uk/people/a.hawkes) as my primary supervisor. My co-supervisors  were [Dr. Paul Balcombe](https://www.imperial.ac.uk/people/p.balcombe) and [Dr. Jasmin Cooper](https://www.imperial.ac.uk/people/jasmin.cooper), whom I spent most of my time with during the project. All relevant information and reports were submitted to Imperial College London's Department of Chemical Engineering. 

The nature of the project was to model the methane emissions in the natural gas supply chain, mainly using statistics in the technical assessment part of the project to identify super-emitters. The programming language of choice for this project is R. The language was chosen for its excellent statistical computing abilities, a wide range of unique statistical packages and efficient handling of big data. A Python version is currently being written for learning purposes.

## Abstract
Super-emitters are infamously known for emitting high amounts of methane in the oil/gas industry. They are stochastic by nature as methane emissions vary in the natural gas supply chain by numerous factors, hence it is difficult to predict their existence.

In this study, real-world methane emissions are collected to evaluate which methane emissions are classified as super-emitters. Just collecting emissions data is not enough, data needs to be reviewed on how, when and where it was collected, type of emissions and its underlying source. Methane emissions that followed the aforementioned criteria is ideal for efficient analysis of the super-emitters.

A modelling workflow for this study is outlined to construct a high-resolution probabilistic emissions model. From data collection, data wrangling, algorithm implementation, data visualisation and analysis. Initial steps of the model are used to characterise methane emissions with statistical distributions to appropriately quantify the distribution’s heavy-tail, in which the heavy-tail characteristic indicates a super-emitter.

A Monte Carlo simulation was then performed to produce 10,000 curves of methane emissions as a percentage of the EUR (Estimated Ultimate Recovery). In which the EUR is also fitted with a distribution of the same type as the methane emissions’ best fitted distribution. The incentive for the Monte Carlo simulation is to statistically signify that a low EUR and a high methane emission influences the existence of the super-emitters significantly.

In-depth analysis of the super-emitters is conducted by constructing Lorenz curves to identify the skewness within the data. Results showed that majority of the datasets showed significant skewness, with average emissions by the top 5% emitters were on the lower end of the emission range. However, the average emissions did not skew the data as immensely as expected, but the presence of a few top 5% emitters with magnitudes larger than the average emissions were the main influence in skewness contribution.

Economic assessment was conducted to evaluate low-cost, methane monitoring/reduction technologies against a reduced emissions scenario. Results from the assessment show that mitigation costs were negative, indicating the amount of money being saved rather than spent. An optimisation model is implemented and results showed that there is virtually no cost required to mitigate a tonne of CO2 eq. Sensitivity analysis is performed on the natural gas price and how it affects the mitigation costs in 10 years. The analysis indicated that the natural gas price is likely to increase relative to the current gas price and this increases the costeffectiveness of the innovative mitigation technologies. This demonstrates that low-cost methane monitoring/reduction technologies are efficient and cost-effective.

## Acknowledgements
I would like to express my gratitude to Dr Adam Hawkes for giving me the opportunity to undertake this MSc project. I would also like to express my sincere gratitude to Dr Paul Balcombe and Dr Jasmin Cooper for their supervision, guidance and continual support throughout the whole project. Not only that, both of you have given me full control of the project and I managed to gain new skills and knowledge in this process.

I would also like to thank the companies and research institutes that took their time to provide me some data input for this study. Thank you to Yori Jamin from Clearstone Engineering, Joshua Anhalt and Mike D’Antoni from GreenPath Energy for providing me their methane emissions datasets. Thank you to Hendrik Hamann and Levente Klein from IBM and James Scherer from Aeris Technologies for providing me economic information for the economic assessment of this study. Thank you to Evar Umeozor from CERI for providing me the ICERM database and guidance on the report “Economic and Environmental Impacts of Methane Emissions Reduction In The Natural Gas Supply Chain”. Without all this information, this study would not be possible.

Lastly, I would like to express my sincere thanks to my girlfriend Cat, my family and all my friends, who have conversed with me about the project, for their continual and moral support.
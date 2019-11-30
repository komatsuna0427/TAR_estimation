## Threshold autoregressive model (TAR) estimation with R

This repository contains R codes that estimate the parameters of TAR model. In fact, there are R libraries that such as ```tsDyn```. But the problem is I cannot estimate the following model with the existing libraries:
$$
\begin{align}
m_{t} - m{t-1} = 
	\begin{cases}
	\varepsilon_{t} & (\lvert m_{t-1} \rvert \leq \theta_{t-1}) \\
	\rho m_{t-1} + \varepsilon_{t} & (\lvert m_{t-1} \rvert > \theta_{t-1})
	\end{cases}.
\end{align}
$$
This model is based on the Law of One Price. That is, if two markets are linked by trade and arbitrage, those two markets will have a common, unique price, provided there is no transportation cost.

Let $p_{i,t}$ and p j,t be prices of a commodity at time t in market i and j, respectively, and ci j,t be the
transportation cost between market i and j at time t. If market i and j are integrated, then

It takes time because the code computes the values of the objective function for all possible threshold candidates (grid search). 
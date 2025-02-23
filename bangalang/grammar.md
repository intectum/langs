$$
\begin{align}
program\ &\to\ statement^*\\
statement\ &\to\ scope\ |\ declaration\ |\ assignment\ |\ exit \\
scope\ &\to\ \text{"\{"}\ statement^*\ \text{"\}"}\\
declaration\ &\to\ identifier\ \text{":"}\ \text{"="}\ expression\\
assignment\ &\to\ identifier\ \text{"="}\ expression\\
exit\ &\to\ \text{"exit("}\ expression\ \text{")"}\\
expression\ &\to\ primary\ (\ (\ \text{"+"}\ |\ \text{"-"}\ |\ \text{"*"}\ |\ \text{"/"}\ )\ primary\ )^*\\
primary\ &\to\ \text{"("}\ expression\ \text{")"}\ |\ identifier\ |\ integer\_literal\ |\ \text{"-"}\ primary\\
\end{align}
$$

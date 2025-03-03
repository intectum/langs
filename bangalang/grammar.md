$$
\begin{align}
program\ &\to\ (\ procedure\ |\ statement\ )^*\\
procedure\ &\to\ identifier\ \text{":"}\ \text{"="}\ \text{"proc"}\ \text{"("}\ (\ identifier\ (\ \text{","}\ identifier\ )^*\ )?\ \text{")"} scope\\
statement\ &\to\ if\ |\ scope\ |\ declaration\ |\ assignment\ |\ return\ |\ call \\
if\ &\to\ \text{"if"}\ expression\ scope\ (\ \text{"else"}\ \text{"if"}\ expression\ scope\ )^*\ (\ \text{"else"}\ scope\ )?\\
for\ &\to\ \text{"for"}\ (\ expression\ |\ declaration\ \text{","}\ expression\ \text{","}\ assignment\ )\ scope\\
scope\ &\to\ \text{"\{"}\ statement^*\ \text{"\}"}\\
declaration\ &\to\ identifier\ \text{":"}\ \text{"="}\ expression\\
assignment\ &\to\ identifier\ \text{"="}\ expression\\
return\ &\to\ \text{"return"}\ expression\\
expression\ &\to\ primary\ (\ (\ \text{"+"}\ |\ \text{"-"}\ |\ \text{"*"}\ |\ \text{"/"}\ )\ primary\ )^*\\
primary\ &\to\ \text{"("}\ expression\ \text{")"}\ |\ call\ |\ identifier\ |\ integer\_literal\ |\ \text{"-"}\ primary\\
call\ &\to\ identifier\ \text{"("}\ (\ expression\ (\ \text{","}\ expression\ )^*\ )?\ \text{")"}\\
\end{align}
$$

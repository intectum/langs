$$
\begin{align}
  [\text{Program}] &\to [\text{Statement}]^* \\
  [\text{Statement}] &\to
  \begin{cases}
    [\text{Scope}] \\
    \text{identifier} := [\text{Expression}] \\
    \text{identifier} = [\text{Expression}] \\
    \text{exit}([\text{Expression}]) \\
  \end{cases} \\
  [\text{Scope}] &\to \{[\text{Statement}]^*\} \\
  [\text{Expression}] &\to
  \begin{cases}
    [\text{Primary}] \\
    [\text{Primary}] + [\text{Expression}] \\
    [\text{Primary}] - [\text{Expression}] \\
    [\text{Primary}] * [\text{Expression}] \\
    [\text{Primary}] / [\text{Expression}] \\
  \end{cases} \\
  [\text{Primary}] &\to
  \begin{cases}
    \text{identifier} \\
    \text{integer literal} \\
  \end{cases}
\end{align}
$$

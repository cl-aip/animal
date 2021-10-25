# animal
This animal program from Winston's Lisp 1st ver. is rewritten for Common Lisp.

The aim of animal.lisp is to illustrate the features of forward-chaining and backward-chaining in reasoning of rule-base system.
The forward reasoning is fact-driven like the Failure Mode and Effects Analysis (FMEA), and we may obtain many results, but we may need a vast search space to get satisfiable results. The backward reasoning is goal-driven like the Fault Tree Analysis (FTA), and we may perform satisfiable reasoning in small search space, but we need hypotheses as goals. The former is good at batch-mode systems and the latter is good to introduce interactive-mode into the systems.

# EndogenousVisualPriorEntry-BayesianHierarchicalModel
Stan and R code for bayesian hierarchical model analysis of temporal order judgement and color wheel data in pair of repeated measures experiments.

Two experiments are run, the second of which has 'FollowUp' in the name of both its '.stan' files and its single '.R' file. Despite this dichotomization, the analyses for the data of both experiments are broadly identical. Futhermore, the procedure of both experiments can be summarized as follows: 

On each trial, human participants each watch pairs of stimuli, one on each side (left/right) of the screen, appear in sequence. The time interval between the differential onsets of the stimlui takes one of five values on a given trial. The order of the stimuli onsets is randomized across trials, but each order is presented an equal amount times in total. 

On each trial, participants are prompted to make one of two responses: indicate which stimulus appeared first (temporal order judgement; TOJ), or indicate color of a probe that appeared over one of the two stimuli using a full spectrum color wheel. In a given block of trials, color probes are more likely to appear on one side of the screen. This presentation bias defines the attention condition of all trials within that block. Each participant is equally exposed to both biases. Thus, the outcome measures - TOJs, color wheel accuracy - are each compared within-subject, between attention conditions. 

For TOJ trials, performance in each condition is modeled as a sigmoid curve using the normal CDF where the mean of the normal ("pss") specifies a bias for either left or right, and the standard deviation of the normal ("jnd") inversely specifies the sensitivity of discrimination. Overall pss/jnd and effects of the condition variable thereon are modeled hierarchically across participants, including inference on the possible correlations.

For color wheel trials, performance is modeled with a mixture model consisting of a uniform and a Von Mises (circular normal) distribution, from which parameters representative of the probability ("rho") and the fidelity ("kappa") of memory are derived. 

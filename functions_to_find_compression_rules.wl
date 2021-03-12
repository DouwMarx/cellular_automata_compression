(* ::Package:: *)

getEvolutionsForAllPermutations::usage = "Solves for all the evolution paths for the permutations possible for a given data dimensionality";
getEvolutionsForAllPermutations[ruleNumber_, ruleRange_,startTime_ ,dataDimensionality_]:=(
p = Tuples[{1,0},dataDimensionality];(*All possible permutations for this given data dimensionality and number of colors*);
evolutions = CellularAutomaton[{ruleNumber,2,ruleRange},#,{{startTime,dataDimensionality-1 + startTime},All}]&/@p (* Only evolve up to number of data dimensionality, any more would be a waste (not lead to compression) *)
(* Notice that initial state of CA is not returned. Assumption is made that CA step is made before reading bit from read index*)
)


numberOfUpdatesToDistinguishPermutation::usage = "Check the number of updates required to distinguish a permutation from one of the other permutations";
numberOfUpdatesToDistinguishPermutation[evolutions_,permutationnumber_]:=( (*Evolutions are the evolutions of all the possible permutations, permutationnumber specifies which of the permutation we are considering*)
splitup = TakeDrop[evolutions,{permutationnumber}]; (*Remove the permutation we are looking at from the list of evolutions*)
permevol = Last@splitup; (*The evolution of all the other permutations*)
testperm = First@splitup; (*The evolution of the current permutation under investigation*)
a = Abs[permevol-ConstantArray[First@testperm,Length@permevol]]; (*Identify if the permutation evolution under investigation is different from the other permutation evolutions. Zeros means it is the same at that update timestep*)
accum = Accumulate/@a; (*When the evolution changes from 1 to something larger than one, it means the sequence from a given read index can uniquely define a permutation from the permutation it is being compared to*)
p =Product[accum[[i]],{i,1,Length[accum]}]/.n_Integer/;n>1->1; (*Taking the product accross all permutation evolutions, we can find the numer of updates required to define a permutations uniquely compared to all other permutations*)
Total[Abs[p-1],{1}] + 1 (*Number of updates required for this permutation to be uniquely defined for a given read index, ones are switched with zeros and then rows are added*)
(* One is added since the permutation is only defined after the bit is sent *)
)


numberOfUpdatesToDistinguishAnyPermutation::usage = "Find the number of updates required to distinguish any  of the permutations";
numberOfUpdatesToDistinguishAnyPermutation[evolutions_,dataDimensionality_]:=(
allperm = numberOfUpdatesToDistinguishPermutation[evolutions,#]&/@Range[2^dataDimensionality] (*Find the shortest update sequence for all permuatations*)
)


testRuleRangeDimForReduction[ruleNumber_,ruleRange_,startTime_,dataDimensionality_]:=(
	evols = getEvolutionsForAllPermutations[ruleNumber,ruleRange,startTime, dataDimensionality];
	nouta = numberOfUpdatesToDistinguishAnyPermutation[evols,dataDimensionality];
	(*compression = Min[Max/@Transpose[nouta]]< dataDimensionality;*)
	permutationsCompressable = UnitStep[dataDimensionality  -1 - nouta]; (*If smaller than 0: 0, if largereq than 0: 1*)
	fracCompressed = First@Total[permutationsCompressable]/Length[permutationsCompressable]//N (*Compression for all read index the same?*)
	)
	


numberOfGeneralRules::usage = "Computes the number of possible 1D 2 color CA rules for given range";
numberOfGeneralRules[ruleRange_]:=(
2^(2^(2*ruleRange+1))
)

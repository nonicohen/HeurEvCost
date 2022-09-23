:- dynamic arc/2, nodes/1, attackers/2, potentialEvidence/1, argEvidence/2, individualArgEvidence/2,
	impactFactor/1, str_evid/4, activeEvidence/1, missingEvidence/1, currentEvidence/1,
	local_str_evid/5, evid_cost/2, strVal/1, costVal/1, randomVal/1, lastAttacker/1,
	listConfigCall/1, nodeCount/1, activeEvidIndiv/1, remainingPotentialEvidence/1.

/* ---------------------------------------------------------
    Definition of parameters used for empirical evaluation of CGPT:
    * TreeNodeCount = 100, 300, 500, 700, 900 (# argNodes per AFFE)
    * MaxBranchFactor = 5, 10, 20 (Max attacker count per argument)
    * EvidUnivSize = 0.5, 1, 2 (Constant by which TreeNodeCount is
    multiplied)
    * MaxArgEvid = 10, 20 (# pieces of evidence per argument)
    * CIF = 0.5 (Cost Impact Factor - Constant by which the cost of a
    piece of evidence is reduced when increasing depth on the AFFE)
    * MaxEvidCost = 1, 10, 100, 10000 (Max cost of each piece of evid)
    * DeactivationQuota = 1, 10, 20, 30 (% of missing evidence) - Set
    from the outside
*/

treeNodeCountParameter([900, 700, 500, 300, 100]).
maxBranchFactorParameter([5, 10, 20]).
evidUnivSizeParameter([0.5, 1, 2]).
maxArgEvidParameter([10, 20]).
cifParameter([0.5]).
maxEvidCostParameter([1, 10, 100, 1000]).


configTest(ConfigCall):-
	treeNodeCountParameter(TreeNodeCountParamList),
	maxBranchFactorParameter(MaxBranchFactorParamList),
	evidUnivSizeParameter(EvidUnivSizeParamList),
	maxArgEvidParameter(MaxArgEvidParamList),
	cifParameter(CIFParamList),
	maxEvidCostParameter(MaxEvidCostParamList),
	member(TreeNodeCount, TreeNodeCountParamList),
	member(MaxBranchFactor, MaxBranchFactorParamList),
	member(EvidUnivSize, EvidUnivSizeParamList),
	member(MaxArgEvid, MaxArgEvidParamList),
	member(CIF, CIFParamList),
	member(MaxEvidCost, MaxEvidCostParamList),
	ConfigCall =..[generateAFFE, TreeNodeCount, MaxBranchFactor, EvidUnivSize, MaxArgEvid, CIF, MaxEvidCost].


runTestsTopLevelDQ(DeactivationQuota):-
	findall(C, configTest(C), ConfigList),
	length(ConfigList, LengthConfigList),
	assert(listConfigCall(ConfigList)),
	printStats(['TreeNodeCount', 'MaxBranchFactor', 'EvidUnivSize', 'MaxArgEvid',
		    'CIF', 'MaxEvidCost', 'DeactivationQuota', 'TreeNodeCountReal',
		    'GainCGPTvsNGPT', 'GainCGPTvsSGPT', 'AvgCostCGPT', 'AvgCostNGPT', 'AvgCostSGPT']),
	nl,
	forall(
	    between(1, LengthConfigList, _),
	    (
		listConfigCall(LCC),
		select(ConfigCall, LCC, LCCNew),
		ConfigCall =..[generateAFFE, TreeNodeCount, MaxBranchFactor, EvidUnivSize, MaxArgEvid, CIF, MaxEvidCost],
		printStats([TreeNodeCount, MaxBranchFactor, EvidUnivSize, MaxArgEvid, CIF, MaxEvidCost, DeactivationQuota]),
		multiTestOneConfig(ConfigCall, 10, 5, DeactivationQuota), % CHANGE! 500 AFFE w/ 100 deact
		retractall(listConfigCall(_)),
		assert(listConfigCall(LCCNew))
	    )
	).


multiTestOneConfig(ConfigCall, TimesPerConfig, TimesDeact, DeactivationQuota):-
	resetVals,
	forall(
	    between(1, TimesPerConfig, _),
	    (
		call(ConfigCall),
		testBigTrees(DeactivationQuota, TimesDeact)
	    )
	),
	printAvgVals.


testBigTrees(DeactivationQuota, TimesDeact):-
	forall(
	    between(1, TimesDeact, _),
	    (
		deactivate_evidence(DeactivationQuota),
		testAllOnce(1)
	    )
	).


testAllOnce(RootArgNumber):-
	singleTestPrune(RootArgNumber, costPrune, CostVal),
	assert(costVal(CostVal)),
	singleTestPrune(RootArgNumber, strPrune, StrVal),
	assert(strVal(StrVal)),
	singleTestPrune(RootArgNumber, randomPrune, RandomVal),
	assert(randomVal(RandomVal)).


resetVals:-
	retractall(costVal(_)),
	retractall(strVal(_)),
	retractall(randomVal(_)),
	retractall(nodeCount(_)).


printAvgVals:-
	avgFacts(costVal, CostAvg),
	avgFacts(strVal, StrAvg),
	avgFacts(randomVal, RandomAvg),
	avgFacts(nodeCount, NodeCountAvg),
	CostvsRandom is 1 - (CostAvg/RandomAvg),
	CostvsStr is 1 - (CostAvg/StrAvg),
	printStats([NodeCountAvg, CostvsRandom, CostvsStr, CostAvg, RandomAvg, StrAvg]),
	nl.


printStats([]).
printStats([H|T]):- write(H), write('; '), printStats(T).


avgFacts(StatName, StatAvgValue):-
	Facts =.. [StatName, X],
	findall(X, Facts, List),
	sumlist(List,Sum),
	length(List,Cant),
	StatAvgValue is Sum/Cant.


singleTestPrune(RootArgNumber, PruneType, TotalCost):-
	argEvidence(RootArgNumber,CE),
	retractall(missingEvidence(_)),
	assert(missingEvidence([])),
	retractall(currentEvidence(_)),
	assert(currentEvidence(CE)),
	(   defeat(RootArgNumber,[RootArgNumber], PruneType),!; true),
	currentEvidence(CEF),
	missingEvidence(MEF),
	sumEvidenceCost(CEF, Sum1),
	sumEvidenceCost(MEF, Sum2),
	TotalCost is Sum1 + Sum2.


sumEvidenceCost(EvidList,TotalEvidCost):-
	findall(Cost, (member(E,EvidList), evid_cost(E,Cost)), CostList),
	sumlist(CostList,TotalEvidCost).


generateAFFE(TreeNodeCount, MaxBranchFactor, EvidUnivSize, MaxArgEvid, CIF, MaxEvidCost):-
	retractall(nodes(_)),
	retractall(attackers(_,_)),
	retractall(arc(_,_)),
	retractall(lastAttacker(_)),
	numlist(1,TreeNodeCount,NodeList),
	assert(nodes(NodeList)),
	retractall(potentialEvidence(_)),
	retractall(argEvidence(_,_)),
	retractall(evid_cost(_,_)),
	retractall(individualArgEvidence(_,_)),
	retractall(impactFactor(_)),
	retractall(str_evid(_,_,_,_)),
	retractall(local_str_evid(_,_,_,_,_)),
	assert(impactFactor(CIF)),
	EvidUniverseSize is floor(TreeNodeCount * EvidUnivSize),
	numlist(1,EvidUniverseSize,EvidList),
	assert(potentialEvidence(EvidList)),
	assignEvidCost(MaxEvidCost),
	assert(lastAttacker(1)),
	forall(
	    member(Node,NodeList),
	    (
		build_node_evid(Node,MaxArgEvid),
		build_node_arcs(Node,MaxBranchFactor)
	    )
	),
	assertNodeCount,
	build_attackers,
	build_strength_evidRel_DP.


assertNodeCount:-
	findall(N, arc(N,_), L),
	sort(L, LS),
	length(LS, Length),
	NodeCount is Length + 1,
	assert(nodeCount(NodeCount)).


assignEvidCost(MaxEvidCost):-
	potentialEvidence(PE),
	forall(
	    member(E,PE),
	    (
		MaxEvidCostForRandom is MaxEvidCost + 1,
		random(1,MaxEvidCostForRandom,Cost),
		assert(evid_cost(E,Cost))
	    )
        ).


build_node_evid(Node, MaxCountEvidArg):-
		MaxCantEvidArgForRandom is MaxCountEvidArg + 1,
		random(1, MaxCantEvidArgForRandom, CountEvArg),
		build_arg_evid(Node,CountEvArg).


build_arg_evid(Node, CountEvidArg):-
        forall(
	    between(1,CountEvidArg,_),
            selectEvidenceArg(Node)
	),
        findall(E,individualArgEvidence(Node,E),EvidListArg),
        assert(argEvidence(Node,EvidListArg)).


selectEvidenceArg(ArgNumber):-
        potentialEvidence(PE),
        length(PE,LPE),
        EvidPos is random(LPE),
        nth0(EvidPos,PE,Evid),
        not(individualArgEvidence(ArgNumber,Evid)),!,
        assert(individualArgEvidence(ArgNumber,Evid)).

selectEvidenceArg(ArgNumber):- selectEvidenceArg(ArgNumber).


build_node_arcs(1, MaxBranchFactor):-
	build_arcs(1, 1, MaxBranchFactor),!.

build_node_arcs(ArgNumber,MaxBranchFactor):-
	IsAttacked is random(5),
	build_arcs(ArgNumber,IsAttacked,MaxBranchFactor).


build_arcs(ArgNumber, IsAttacked, MaxBranchFactor):-
	IsAttacked > 0,!,
	MaxBranchFactorForRandom is MaxBranchFactor + 1,
	random(1, MaxBranchFactorForRandom, ArcAmount),
	forall(
	    between(1, ArcAmount, _),
	    find_attack(ArgNumber)
	).

build_arcs(_,0,_).


find_attack(ArgNumber):-
	updateLastAttacker(A),
  assert(arc(A,ArgNumber)).

find_attack(_).


updateLastAttacker(NewAttacker):-
	lastAttacker(OldAttacker),
	nodes(NodeList),
	length(NodeList,NodeAmount),
	OldAttacker < NodeAmount,
	retractall(lastAttacker(_)),
	NewAttacker is OldAttacker + 1,
	assert(lastAttacker(NewAttacker)).


build_attackers:-
        nodes(Nodes),
        forall(
                member(Node,Nodes),
                (
		    findall(Y,arc(Y,Node),L),
		    list_to_set(L,Attackers),
		    assert(attackers(Node,Attackers))
		)
	).


build_strength_evidRel_DP:-
	nodes(Nodes),
	forall(
	    member(N,Nodes),
	    get_strength_evidRel_DP(N,[N],_,_,_,_,N)
	).

get_strength_evidRel_DP(X, Visited, XStr, [X|XUsed], [], XERel, _Root):-
	str_evid(X,XUsed,XStr,XERel),
	intersection(XUsed,Visited,[]), !.

get_strength_evidRel_DP(X, Visited, XStr, [X|XUsed], Excluded, XERel, Root):-
	X \= Root,
	local_str_evid(X, Excluded, XUsed,XStr,XERel),
	subset(Excluded, Visited),
	intersection(XUsed,Visited,[]), !.

get_strength_evidRel_DP(X, Visited, XStr, [X| XUsed], XHDC, XERel, Root):-
	attackers(X,L),
	subtract(L, Visited, Atts),
	attsStrEvid(Atts,Visited,StrDen,XUsed,HDC,ERelAtts,Root),
	obtainERel(X,ERelAtts,XERel),
        XStr is 1/(1+StrDen),
        determineHDC(L,Atts,HDC,XHDC),
        decideAssertion(XHDC,X,XStr,XUsed,XERel,Root),!.


obtainERel(X,ERelAtts,ERel):-
	argEvidence(X,ListEvidX),
	impactFactor(Cte),
	findall(
	    (NewCost,Evid),
	    (
		member((OldCost,Evid),ERelAtts),
		not(member(Evid,ListEvidX)),
		NewCost is OldCost * Cte
	    ),
            NewAttsERel
	),
	findall(
	    (CX,EX),
	    (
		member(EX, ListEvidX),
		evid_cost(EX,CX)
	    ),
	    ERelX
	),
	append(ERelX, NewAttsERel,ERel).


attsStrEvid([A|RAtts], Visited, TStr, TUsed, THDC,ERelAtts,Root):-
	get_strength_evidRel_DP(A, [A|Visited], AStr, AUsed, AHDC,AERel,Root),
        attsStrEvid(RAtts,Visited, RStr, RUsed, RHDC, RERel,Root),
        mergeEvid(AERel,RERel,ERelAtts),
        TStr is AStr + RStr,
        union(AUsed, RUsed, TUsed),
        union(AHDC, RHDC, THDC).


attsStrEvid([],_,0,[],[],[],_).


mergeEvid([],RERel,RERel):-!.

mergeEvid([(F,E)|RestAERel],RERel,[(F,E)|Evid]):-
	not(member((_,E), RERel)), !,
	mergeEvid(RestAERel,RERel,Evid).

mergeEvid([(F,E)|RestAERel],RERel,[(F,E)|Evid]):-
	member((FR,E), RERel),
	F > FR, !,
	delete(RERel, (FR,E), NewRERel),
	mergeEvid(RestAERel,NewRERel,Evid).

mergeEvid([(_F,E)|RestAERel],RERel,Evid):-
	member((_,E), RERel),
	mergeEvid(RestAERel,RERel,Evid).


determineHDC(L,Atts,HDC, HDC):-
	L = Atts,!.

determineHDC(L,Atts,HDC,NHDC):-
	subtract(L,Atts, LHDC),
	union(HDC,LHDC,NHDC).


decideAssertion([],X,XStr,XUsed,XERel,_Root):-
	dassert(str_evid(X,XUsed,XStr,XERel)),!.

decideAssertion(_,Root,XStr,XUsed,XERel,Root):-
	dassert(str_evid(Root,XUsed,XStr,XERel)),!.

decideAssertion(L,X,XStr,XUsed,XERel,_Root):-
	assert(local_str_evid(X, L, XUsed,XStr,XERel)),!.

decideAssertion(_,_,_,_,_,_).


dassert(str_evid(X,_XUsed,_XStr,_XERel)):-
	str_evid(X,_,_,_),!.

dassert(str_evid(X,XUsed,XStr,XERel)):-
	assert(str_evid(X,XUsed,XStr,XERel)),!.


deactivate_evidence(Percent):-
	retractall(activeEvidence(_)),
	retractall(currentEvidence(_)),
	retractall(missingEvidence(_)),
	retractall(activeEvidIndiv(_)),
	retractall(remainingPotentialEvidence(_)),
	potentialEvidence(EvidenceList),
	length(EvidenceList,AmountPotentialEvidence),
	argEvidence(1,EvidListArg1),
	length(EvidListArg1,AmountEvidArg1),
	forall(member(EA1, EvidListArg1), assert(activeEvidIndiv(EA1))),
	AmountToDeactivate is ceiling(AmountPotentialEvidence * Percent /100),
	AmountToActivate is AmountPotentialEvidence - AmountEvidArg1 - AmountToDeactivate,
	ord_subtract(EvidenceList, EvidListArg1, RemainingPotentialEvid),
	assert(remainingPotentialEvidence(RemainingPotentialEvid)),
	forall(between(1, AmountToActivate, _), activate_individual_evidence),
	findall(AEI, activeEvidIndiv(AEI), ActiveEvidence),
	assert(activeEvidence(ActiveEvidence)),
	assert(currentEvidence([])),
	assert(missingEvidence([])).


activate_individual_evidence:-
	remainingPotentialEvidence(RPE),
	random_member(E, RPE),
	select(E, RPE, RPENew),
	retractall(remainingPotentialEvidence(_)),
	assert(activeEvidIndiv(E)),
	assert(remainingPotentialEvidence(RPENew)).


filterAttackers(PotentialAttackers, Atts):-
	missingEvidence(FailedEvid),
	findall(
	    A,
	    (
		member(A,PotentialAttackers),
		argEvidence(A,E),
		intersection(E,FailedEvid,[])
	    ),
	    Atts
	).


obtainEvidenceCost(ERel, CurrentEvid, Cost):-
	findall(
	    F,
	    (
		member((F,E), ERel),
		not(member(E,CurrentEvid))
	    ),
	    IFCostList
	),
	sumlist(IFCostList,Cost).


buildArg(Arg):-
	argEvidence(Arg,ArgE),
	processArgEvidence(ArgE).


processArgEvidence([]).

processArgEvidence([E|RestArgE]):-
	processEvidence(E),
	processArgEvidence(RestArgE).


processEvidence(E):-
	currentEvidence(CurrentE),
	member(E,CurrentE).

processEvidence(E):-
	currentEvidence(CurrentE),
	not(member(E,CurrentE)),
	activeEvidence(ActiveE),
	member(E,ActiveE),
	retractall(currentEvidence(_)),
	assert(currentEvidence([E|CurrentE])).

processEvidence(E):-
	currentEvidence(CurrentE),
	activeEvidence(ActiveE),
	missingEvidence(MissingE),
	not(member(E,CurrentE)),
	not(member(E,ActiveE)),
	retractall(missingEvidence(_)),
	assert(missingEvidence([E|MissingE])),
	fail.


defeat(ArgNumber,Visited, Heuristics):-
	attackers(ArgNumber,PotentialAtts),
	filterAttackers(PotentialAtts,Atts),
	subtract(Atts,Visited, UnconstrainedAtts),
	UnconstrainedAtts \= [],
	heuristicSort(Heuristics, UnconstrainedAtts, InitiallySortedAtts),
	heuristicMember(Heuristics, Arg, InitiallySortedAtts),
	buildArg(Arg),
	not(defeat(Arg,[Arg|Visited],Heuristics)),!.



heuristicMember(_,X,[X|_]).

heuristicMember(Heuristics, X,[_|R]):-
	heuristicSort(Heuristics, R,S),
	heuristicMember(Heuristics, X,S).

removeHeuristicVals([],[]).

removeHeuristicVals([_-X|R1],[X|R2]):-
	removeHeuristicVals(R1,R2).


heuristicSort(costPrune, L, SortedList):-
	currentEvidence(CurrentEvid),
	findall(
	    AtCost-AtArg,
	    (
		 member(AtArg,L),
		 str_evid(AtArg,_,_,ERel),
		 obtainEvidenceCost(ERel,CurrentEvid,AtCost)
	    ),
	    CostList
	),
	keysort(CostList,SortedCostList),
	removeHeuristicVals(SortedCostList, SortedList).

heuristicSort(costStrPrune, L, SortedList):-
	currentEvidence(CurrentEvid),
	findall(
	    Heur-AtArg,
	    (
		 member(AtArg,L),
		 str_evid(AtArg,_,Str,ERel),
		 obtainEvidenceCost(ERel,CurrentEvid,AtCost),
		 Heur is AtCost/Str
	    ),
	    HeurList
	),
	keysort(HeurList,SortedHeurList),
	removeHeuristicVals(SortedHeurList, SortedList).

heuristicSort(strPrune, L, SortedList):-
	findall(
	    Str-AtArg,
	    (
		 member(AtArg,L),
		 str_evid(AtArg,_,Str,_)
	    ),
	    StrList
	),
	keysort(StrList,ReverseSortedStrList),
	reverse(ReverseSortedStrList,SortedStrList),
	removeHeuristicVals(SortedStrList, SortedList).

heuristicSort(randomPrune, L, SortedList):-
	shuffle_children(L,SortedList).


shuffle_children(C,SC):-
	random_permutation(C,SC),
	Aux is random(20),
	Aux=5,!.

shuffle_children(C,SC):-
	shuffle_children(C,SC).

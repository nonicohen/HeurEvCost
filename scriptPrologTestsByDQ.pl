#!/usr/bin/env swipl

/*
    How to use the Prolog script:

    swipl -s  <Complete Path of 'scriptPrologTestsByDQ.pl'> <RestArgs>

    <RestArgs> = [<Path of Output Files and the Prolog Implmeentation File>, <DeactivationQuota>]

------------------------
Example of use:

    swipl -s /User/Desktop/scriptPrologTestsByDQ /User/Desktop 10

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialization(readArgs).

%-------------------

readArgs:-
	current_prolog_flag(argv, Args),
	lastTwoArgs(Args, ArgumentsList),
	start(ArgumentsList).

lastTwoArgs([X,Y],[X,Y]):- !.

lastTwoArgs([_H|T], L):-
	lastTwoArgs(T,L).

%------------------

start(ArgumentsList):-
	nth1(1, ArgumentsList, Path),
	atom_concat(Path,'/heurEvCostImpl',PrologFile),
	consult(PrologFile),
	nth1(2, ArgumentsList, DQ),
	getNumber(DQ, DeactivationQuota),
	runTestsTopLevelDQ(DeactivationQuota),
	halt.

%----------------

getNumber(NA,NA):-
	number(NA),!.

getNumber(NA,NN):-
	atom_number(NA, NN).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

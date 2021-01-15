
%%%%%%%%%%%%% COURSEWORK 2019 AAIS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% PDDL+ SIMULATOR - L McCluskey 27/11/19 %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% VERSION 1.0.0  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% SWI Prolog %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%  THIS VERSION HAS A BIT OF AN ANSWER...

% PRELIMINARY FACTS

% contains pre-compiled definitions of list functions ..
:- use_module(library(lists)).

% windowns or mac
computer(windows).
% windows.
% linux

% trace ON or OFF
sim_trace(on).

% WHERE INPUT COMES FROM
% Comment and uncomment scenarios as needed to test
%initial('C:/Users/Waqas/Desktop/AAIS_C2_PL/initial_state.pl') :- computer(windows).
initial('C:/Users/Waqas/Desktop/AAIS_C2_PL/scenario1.pl') :- computer(windows).
%initial('C:/Users/Waqas/Desktop/AAIS_C2_PL/scenario2.pl') :- computer(windows).
%initial('C:/Users/Waqas/Desktop/AAIS_C2_PL/scenario3.pl') :- computer(windows).
%initial('C:/Users/Waqas/Desktop/AAIS_C2_PL/scenario4.pl') :- computer(windows).
%initial('C:/Users/Waqas/Desktop/AAIS_C2_PL/scenario5.pl') :- computer(windows).

% Domain Model File
domainmodel('C:/Users/Waqas/Desktop/AAIS_C2_PL/domain_model.pl') :- computer(windows).

% WHERE to put log information 
tracefile('C:/Users/Waqas/Desktop/AAIS_C2_PL/trace.txt') :-computer(windows).


%%%%% ************* INITIALISE + go   **************************************
%% Calling predicate .... Time = Simulation time, Inc = Time increment (stick to 1)
sim(Time,Inc) :-
% read in the initial state file and domain model
    initial(I),
    domainmodel(D),
    compile(I), 
    compile(D),
% do some consistency checking of links
    check_links,
    check_links_max,
% call simulation
    simulate(Time,Inc).

%%%%% *******************  SIMULATION CODE ********************************

% e.g. simulate(900,1).
% Stops after Sim_Time no of clicks  

% get ready for simulation loop
simulate(Sim_Time,Delta) :-
% assert initial node ready for simulation loop:
        init(_, _, _, Init_DYN, STATICS, Init_PLAN, Init_PRESSES, _),
        assert(statics(STATICS)),
        assert(node(1,0,Init_DYN,Init_PLAN,Init_PRESSES)),
% for logging status of roads:
        assert_saturation_levels,
        click_on(Sim_Time,Delta), 
        !.

%%%%% END OF SIMULATION - WRITE OUT INFORMATION / RESULTS TO SCREEN AND FILE

click_on(Sim_Time,_) :-
        node(_,Actual_Time,DYN,PLAN,_),
        Sim_Time =< Actual_Time,
        get_statics(STAT),
        write_end_stuff(STAT,PLAN,DYN,Sim_Time),
        tracefile(Trace),
        tell(Trace),nl,nl,
        write_end_stuff(STAT,PLAN,DYN,Sim_Time),
        write_out_turns(STAT),
        told,
        !.
        
% MAIN SIMULATION LOOP  *********** 
click_on(Sim_Time,Delta) :-
%
        do_actions, 
        do_events,
        do_processes(Delta),
%
% now move on time T to T1 and give node N a new number N1
        retract(node(N,T,DYN,PL,PR)),
        N1 is N+1,
        T1 is T+Delta,
        assert(node(N1,T1,DYN,PL,PR)),
        log_trace(T,N,DYN,PL,PR),
% iterate
        click_on(Sim_Time,Delta),
        !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* My Added Predicates */
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Initialises the current input file - from Week 9
initialise_ :-
	initial(I),
	compile(I),
	init(_,_,_,DYN,STATICS,PLAN,_,_),
	assert(dyn(DYN)),
	assert(statics(STATICS)),
	assert(plan(PLAN)).

% Maximises the input links stage length - from Week 9
maximise(X,V) :-
	statics(STATICS),
	member(equals(maxgreentime(X),V),STATICS),
	retract( plan(PLAN) ),
	delete(PLAN, equals(defaultgreentime(X),_), REDUCED_PLAN),
	assert( plan( [equals(defaultgreentime(X),V) | REDUCED_PLAN])).	

% Minimises the input links stage length - from Week 9	
minimise(X,V) :-
	statics(STATICS),
	member(equals(mingreentime(X),V),STATICS),
	retract( plan(PLAN) ),
	delete(PLAN, equals(defaultgreentime(X),_), REDUCED_PLAN),
	assert( plan( [equals(defaultgreentime(X),V) | REDUCED_PLAN])).

% Retrieves the current occupancy of the input link	- from Week 11
% Edited to include DYN parameter on call
occ_link(L,DYN,V) :-
	member( equals( occupancy(L),V),DYN).

% Retrieves the junction at the end of the input link - from Week 11
junction_name_end(L,J) :-
	init(_,_,_,_,STATICS,_,_,_),
	member(equals(turnrate(Stage,L,_),_),STATICS),
	member(contains(J,Stage),STATICS).

% Retrieves the stage length of the input stage - from Week 11
stage_len(S,SL) :-
	init(_,_,_,_,_,PLAN,_,_),
	member(equals(defaultgreentime(S),SL),PLAN).

% Retrieves a list of stages associated with the input junction - from Week 11
list_of_stages(J,SList) :-
	init(_,_,_,_,STATICS,_,_,_),
	get_stages(J,STATICS,SList).

get_stages(_,[],[]).
get_stages(J,[contains(J,S)|RestSTATICS],[S|RestS]):-
	get_stages(J,RestSTATICS, RestS),!.	
get_stages(J,[_|RestSTATICS],RestS) :-
	get_stages(J,RestSTATICS,RestS),!.

% Retrieves the length of the input junctions cycle - from Week 11
cyc_length(J,CL) :-
	init(_,_,_,_,STATICS,PLAN,_,_),
	get_cyc_length(J,PLAN,STATICS,0,CL).
	
get_cyc_length(_,[],_,CL,CL).	
get_cyc_length(J,[equals(defaultgreentime(S),SL)|RS],STATICS,CLx,CL) :-
	member(contains(J,S),STATICS),
	member(equals(interlimit(S),V),STATICS),
	CLx1 is CLx + SL + V,
	get_cyc_length(J,RS,STATICS,CLx1,CL),!.	
get_cyc_length(J,[_|RestS],STATICS,CLx,CL) :-
	get_cyc_length(J,RestS,STATICS,CLx,CL),!.

% Retrieves the total turn rate from the input link during the input stage - from Week 11
total_flow_rate(L,S,FR) :-
	init(_,_,_,_,STATICS,_,_,_),
	get_total_flow_rate(L,S,STATICS,0,FR).

get_total_flow_rate(_,_,[],FR,FR).
get_total_flow_rate(L,S,[equals(turnrate(S,L,_),V)|RestS],FRx,FR) :-
	FRx1 is FRx + V,
	get_total_flow_rate(L,S,RestS,FRx1,FR),!.
get_total_flow_rate(L,S,[_|RestS],FRx,FR) :- 
	get_total_flow_rate(L,S,RestS,FRx,FR),!.	

% Retrieves the total out-flow of the input link during a cycle - from Week 11
total_flow_out_cycle(L,FL) :-
	junction_name_end(L,J),
	list_of_stages(J,SList),
	accumulate_flow(L,SList,FL).

accumulate_flow(L,[S|SR],FL) :-
	total_flow_rate(L,S,TFR),
	stage_len(S,SL),
	Stage_flow is SL * TFR,
	accumulate_flow(L,SR,FL1),
	FL is FL1 + Stage_flow,!.
accumulate_flow(_,[],0).

% Retrieves the average out-flow of the input link per second - from Week 11
average_flow_out(L,AV) :-
	junction_name_end(L,J),
	cyc_length(J,CL),
	total_flow_out_cycle(L,FL),
	AV is FL/CL,!.
	%/
	


% Retrieves the stage associated with the input link 
stage_name(L,S) :-
	init(_,_,_,_,STATICS,_,_,_),
	member(equals(turnrate(S,L,_),_),STATICS).

% Retrieves the total in-flow of the input link during a cycle
total_flow_in_rate(L,S,FI) :-
	init(_,_,_,_,STATICS,_,_,_),
	get_total_flow_in_rate(L,S,STATICS,0,FI).

get_total_flow_in_rate(_,_,[],FI,FI).
get_total_flow_in_rate(L,S,[equals(turnrate(_,_,L),V)|RestS],FIx,FI) :-
	FIx1 is FIx + V,
	get_total_flow_in_rate(L,S,RestS,FIx1,FI),!.
get_total_flow_in_rate(L,S,[_|RestS],FIx,FI) :- 
	get_total_flow_in_rate(L,S,RestS,FIx,FI),!.

opposite_stage(L,OS) :-
% Get junction (J) of input link (L)
	junction_name_end(L,J),
% Get stage list (SList) of calculated junction (J)
	list_of_stages(J,SList),
% Get stage (S) from input link (L) 
	stage_name(L,S),
% Create new list (OSList) from stage list (SList) removing calculated stage (S)
	delete(SList,S,OSList),
% Set output stage (OS) as first element of new list (OSList)
% n.b. OSList should only have one element after removing S
	nth0(0,OSList,OS).

question1c(L,GTList) :-
% Get junction (J) of input link (L)
	junction_name_end(L,J),
% Get stage (S) from input link (L)
	stage_name(L,S),
% Get stage length / greentime (SL) from calculated stage (S)
	stage_len(S,SL),
% Get cycle length (CL) of calculated junction (J)
	cyc_length(J,CL),
% Use input link (L) and calculated stage (S) to get in-flow rate (FI)
	total_flow_in_rate(L,S,FI),
% Use input link (L) and calculated stage (S) to get out-flow rate (FR)
	total_flow_rate(L,S,FR),
% Calculate greentime (GT) as per below calculation:
	GT is (CL * FI) / FR,
% Calculate greentime difference (D) as per below calculation:
	D is GT - SL,
% Derive opposite stage (OS) from input link (L)
	opposite_stage(L,OS),
% Get stage length / greentime (OSL) from calculated opposite stage (OS)
	stage_len(OS,OSL),
% Calculate opposite greentime (OGT) as per below calculation:
	OGT is OSL - D,
% Store greentime values (GT,OGT) in output list (GTList)
	GTList = [GT,OGT],
% Retract previous plan
	retract( plan(PLAN) ),
% Delete previous greentime (GT) from previous plan
	delete(PLAN, equals(defaultgreentime(S),_), PLAN_1),
% Assert plan with new greentime (GT) for calculated stage (S)
	asserta( plan( [equals(defaultgreentime(S),GT) | PLAN_1])),
% Delete previous opposite greentime (OGT) from previous plan
	delete(PLAN_1, equals(defaultgreentime(OS),_), PLAN_2),
% Assert plan with new opposite greentime (OGT) for opposite stage (OS)
	asserta( plan( [equals(defaultgreentime(OS),OGT) | PLAN_2])).
	
question2(L,DYN,PLAN,PLAN_1) :-
% Check current occupancy link (V) of input link (L) using DYN
	occ_link(L,DYN,V),
% TEST: print L and calculated V to console on call
%	nl,write(L),write(::),write(V),
% If occupancy (V) greater than 10:
	V > 10,
% Get junction (J) of input link (L)
	junction_name_end(L,J),
% Get stage list (SList) of calculated junction (J)
	list_of_stages(J,SList),
% Per stage of stage list:
	member(S,SList),
% Use input link (L) and iterated stage (S) to get out-flow rate (FR)
	total_flow_rate(L,S,FR),
% If out-flow rate (FR) greater than 0:
% n.b. FlowRate(FR) > 0 works here as only one flow out will be > 0
	FR > 0,
% Use question1c to calculate optimal greentime (GT) of input link (L)
	question1c(L,GTList),
	nth0(0,GTList,GT),
% Retract previous plan
	retract( plan(PLAN) ),
% Delete previous greentime from previous plan
	delete(PLAN, equals(defaultgreentime(S),_), PLAN_1),
% Assert plan with calculated greentime (GT) for link (L)
	assert( plan( [equals(defaultgreentime(S),GT) | PLAN_1])).

question2(_,_,_,_).

question3(L,DYN,STATICS,PLAN,PLAN_1) :-
% Check current occupancy link (V) of input link (L) using DYN
	occ_link(L,DYN,V),
% If occupancy (V) greater than 20:
	V > 20,
% Get junction (J) of input link (L)
	junction_name_end(L,J),
% Get stage list (SList) of calculated junction (J)
	list_of_stages(J,SList),
% Per stage of stage list:
	member(S,SList),
% Use input link (L) and iterated stage (S) to get out-flow rate (FR)
	total_flow_rate(L,S,FR),
% If out-flow rate (FR) greater than 0:
% n.b. FlowRate(FR) > 0 works here as only one flow out will be > 0
	FR > 0,
% Calculate maximum greentime (GT) of iterated stage (S)
	member(equals(maxgreentime(S),GT),STATICS),
% Retract previous plan
	retract( plan(PLAN) ),
% Delete previous greentime from previous plan
	delete(PLAN, equals(defaultgreentime(S),_), PLAN_1),
% Assert plan with calculated greentime (GT) for link (L)
	assert( plan( [equals(defaultgreentime(S),GT) | PLAN_1])).
	
question3(_,_,_,_,_).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* Simulaton of actions */
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_actions :-
        retract(node(N,T,DYN,PLAN,PRESS)),
        get_statics(STATICS),		
%
 %		question2(albert_south,DYN,PLAN,PLAN_1),
 %		question2(callans_south,DYN,PLAN_1,PLAN_2),
 %		question2(hepton_south,DYN,PLAN_2,PLAN_3),
 %		question2(trades_north,DYN,PLAN_3,PLAN_4),
 %		PLAN = PLAN_4,
%
 %		question3(stubbings_east,DYN,STATICS,PLAN,PLAN_1),
 %		question3(station_west,DYN,STATICS,PLAN_1,PLAN_2),
 %		PLAN = PLAN_2,
%
%       if any default time is reached at J, put a 'trigger(J)' in dynamics
%       to signal that the stage must change (going through intergreen first)
        do_plan_defaults(PLAN,DYN,STATICS, Triggers),
        append(Triggers,DYN, DYN1),
%
%       if any junctions (pelicans) have been waiting till now (T) to go on, put a 'tigger(J)' in dynamics
%       implement any outside - sourced flow rate changes that are time depedent waiting to happen
        do_plan_waiting(PLAN,T, PLAN1,MoreTriggers),
        append(MoreTriggers,DYN1, DYN2),
%
%       if any pelican is pushed at this instant, record a 'waiting' change_state as
%       long as its not already on red OR it has not already been pushed and is 
%       waiting to turn on
        do_presses(PRESS,PLAN1,DYN2,T,  BIT,PRESS2),
        append(PLAN1,BIT, PLAN2),
%
        N1 is N+1,	
        assert(node(N1,T,DYN2,PLAN2,PRESS2)),	
        !.

%   if any pelican is pushed at this instant, record a 'waiting' change_state 
%     assume presses are supplied in ASCENDING order for efficiency
%     assume same buttun CANNOT be pressed > 1 in the same instant

do_presses([press(T,J)|Rest],PLAN,DYN,T, [change_stage(T1,J)],Rest) :-
% crossing not already activated 
        \+ member(inter(J),DYN),
% not already been pushed and waiting to go on
        \+ member(change_stage(_,J),PLAN),
% not already about to change state
        \+ member(trigger(J),DYN),
% get the wait time, which must be greater than 0
        member(wait(J,WT), PLAN),
        T1 is T + WT,
        !.
% presses that are redundant
do_presses([press(T,_)|Rest],_,_,T, [],Rest) :- !.
% no need to look through all list as presses are in ascending order
do_presses(P,_,_,_,[],P) :- !.


% if (active ?p) (contains ?i ?p) (> (greentime ?i) default ?p )
% add (trigger ?i)
do_plan_defaults([equals(defaultgreentime(Stage),DT)|Rest],DYN,ST, [trigger(J)|RestT] ) :-
        member(contains(J,Stage),ST),
        member(active(Stage), DYN),
        member(equals(greentime(J),GT),DYN),
        GT >= DT, 
        do_plan_defaults(Rest,DYN,ST, RestT),
        !.
do_plan_defaults([_|Rest],DYN,S, Trig) :-
        do_plan_defaults(Rest,DYN,S, Trig),
        !.
do_plan_defaults([],_,_,[]) :- !.

% iterate through PLAN and execute anything timed to happen now (that is T)
% last argument is the 'list of triggers' to add to DYN to change stages
do_plan_waiting([change_stage(T,J)|Rest],T, Rest2,[trigger(J)|RestT]) :-
        do_plan_waiting(Rest,T, Rest2,RestT),
        !.
do_plan_waiting([change_flow(T,Link_in,NewFlow)|Rest],T, Rest2,Trig) :-
% DYN, PLAN and PRESSES now all stored in NODE so no need to keep them in init ..
        member(equals(turnrate(fake,outside,Link_in),INrate),STAT),
        retract( statics(STAT) ),
        delete(STAT,equals(turnrate(fake,outside,Link_in),INrate),NEW_STAT),
        assert( statics( [equals(turnrate(fake,outside,Link_in),NewFlow) | NEW_STAT] ) ),
        tell(user),nl,write(" CHANGED FLOW RATE ON "),write(Link_in),nl,
        do_plan_waiting(Rest,T, Rest2,Trig),
        !.
do_plan_waiting([R|Rest],T, [R|Rest2],Trig) :-
        do_plan_waiting(Rest,T, Rest2,Trig),
        !.
do_plan_waiting([],_,[],[]) :- !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 	simulation of events
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_events :-
        assert(event_flag(0)),
        do_events1,
        retract(event_flag(N)),
        N > 0,
% keep repeating until no more events have happened
        do_events.
do_events.

do_events1 :-
        get_current_state_facts(N,T,ALL),
        event(_Name, _Types, Precons,Pos,Neg,Assign),
        satisfied1(Precons,ALL),
        satisfied2(Precons,ALL),
        write_out_if_its_a_trigger(T,Precons),
        retract(node(N,T,DYN1,PL,PR)),
        event_change(Pos,Neg,Assign,DYN1, DYN2),
        assert(node(N,T,DYN2,PL,PR)),
        retract(event_flag(Count)), N1 is Count+1, assert(event_flag(N1)),
        fail.
do_events1.

write_out_if_its_a_trigger(T,Precons) :-
        member(trigger(J),Precons),
        tell(user),nl,write("Time is: "),write(T), write(" seconds"),nl,
        write(J),write("  going to intergreen"),!.
write_out_if_its_a_trigger(_,_) :-!.

event_change(Pos,Neg,Assign,DYN1, DYN2) :-
        event_changeP(Pos, DYN1,DYN11),
        event_changeN(Neg, DYN11,DYN12),
        event_changeA(Assign, DYN12,DYN2),!.

event_changeP([],DYN,DYN) :- !.
event_changeP([A|B],DYN,[A | DYN2]) :- 
        event_changeP(B,DYN,DYN2),!.

event_changeN([],DYN,DYN) :- !.
event_changeN([A|B],DYN,DYN2) :-
        delete(DYN,A, DYN1), 
        event_changeN(B,DYN1,DYN2),!.

event_changeA([],DYN,DYN) :- !.
event_changeA([assign(FUN,V)|B],DYN,[equals(FUN,V1)|DYN2]) :-        
        get_statics(ST),
        append(DYN,ST,ALL),
        value_of(V,ALL,V1),
        delete(DYN,equals(FUN,_), DYN1), 
        event_changeA(B,DYN1,DYN2),!.
        
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 	simulation of processes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
do_processes(Delta) :-
        get_current_state_facts(N1,T1,ALL),
        process(_Name,_Types,Pre,Ch),
% are the pres satisfied in the current state?
        satisfied1(Pre, ALL),
        satisfied2(Pre, ALL),
        retract(node(N1,T1,DYN1,PL,PR)),
% note that ALL keeps ALL the old state dynamic values whereas values in DYN1 .. DYN2.. are updated
        progress_change(Delta,DYN1,ALL,Ch, DYN2),
        assert(node(N1,T1,DYN2,PL,PR)),
       fail.
do_processes(_) :- !.

progress_change(Delta,DYN,ALL,[increase(FUN,EXP) | CR],  DYN2) :-
        value_of_op(Delta,ALL,EXP,V),
        increase_val(FUN,V,DYN,DYN1),
        progress_change(Delta,DYN1,ALL,CR,  DYN2),!.
progress_change(Delta,DYN,ALL,[decrease(FUN,EXP) | CR],  DYN2) :-
        value_of_op(Delta,ALL,EXP,V),
        decrease_val(FUN,V,DYN,DYN1),
	progress_change(Delta,DYN1,ALL,CR,  DYN2),!.
progress_change(_,DYN,_,[],  DYN) :- !.
progress_change(A,B,C,D,  E) :- nl,nl,nl,write('*** FAIL *** '),nl,
       write(A),nl,
       write(B),nl,
       write(C),nl,
       write(D),nl,
       write(E),nl,
       !.


increase_val(FUN,V,DYN,[equals(FUN,V2)|DYN1]) :- 
        member(equals(FUN,V1),DYN),
        V2 is V1+V,
	delete(DYN,equals(FUN,_), DYN1),!.
decrease_val(FUN,V,DYN,[equals(FUN,V2)|DYN1]) :- 
        member(equals(FUN,V1),DYN),
        V2 is V1-V,
	delete(DYN,equals(FUN,_), DYN1),!.


% deals with predicates first - leaves 'compare' till after
satisfied1([],_).
satisfied1( [compare(_, _, _) | R ], D) :- 
        satisfied1(R,D).
satisfied1( [ X | R ], D) :- 
        member(X,D),
        satisfied1(R, D).

% now deals with 'compare' - compare two expressions that will evaluate to a number
% get 2 and 3rd arg matched with equals(_, V and V1), then check V op V1.
satisfied2([],_).
satisfied2( [compare(Op, E1, E2) | R ], D) :- 
        do_compare(Op,E1,E2, D),
        satisfied2(R,D).
satisfied2( [J | R ] , D) :- 
        J \= compare(_,_,_),
        satisfied2(R, D).


do_compare(>,E1,E2, D) :-
         value_of(E1,D,V1),
         value_of(E2,D,V2),
         V1 > V2.
do_compare(<,E1,E2, D) :-
        value_of(E1,D,V1),
         value_of(E2,D,V2),        
         V1 < V2.
do_compare(>=,E1,E2, D) :-
        value_of(E1,D,V1),
         value_of(E2,D,V2),        
         V1 >= V2.
do_compare(=<,E1,E2, D) :-
        value_of(E1,D,V1),
         value_of(E2,D,V2),        
         V1 =< V2.

% find the value of a function or constant - note if a function, some args could be vars
% this may need to be backtrackable - if we are evaluating in the preconditions it
% may depend on something like member(equals(greentime(X),ALL) where X needs to range through
value_of(op(-,E1,E2),ALL,E3) :- 
      value_of(E1,ALL,V1),
      value_of(E2,ALL,V2),
      E3 is V1-V2.
value_of(op(+,E1,E2),ALL,E3) :- 
      value_of(E1,ALL,V1),
      value_of(E2,ALL,V2),
      E3 is V1+V2.
value_of(op(*,E1,E2),ALL,E3) :- 
      value_of(E1,ALL,V1),
      value_of(E2,ALL,V2),
      E3 is V1*V2.
value_of(op(/,E1,E2),ALL,E3) :- 
      value_of(E1,ALL,V1),
      value_of(E2,ALL,V2),
      E3 is V1/V2.
value_of(E,_,E) :- float(E),!.
value_of(E,_,E) :- integer(E),!.
value_of(E,ALL,V) :- member(equals(E,V),ALL).

% deal with special case op(_,_,_)
% could put in value_of_op … X, X is integer or float, = X
value_of_op(Delta, _, op(*,t,X), V) :-
        integer(X),
        V is X*Delta,!.
value_of_op(Delta, _, op(*,t,X), V) :-
        float(X),
        V is X*Delta,!.
value_of_op(Delta,ALL,op(*,t,X), V) :- 
        member(equals(X,VX),ALL),
        V is VX * Delta,!.
value_of_op(Delta,ALL,op(*,t,X), V) :- 
        value_of(X,ALL,VX),
        V is VX * Delta,!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% utilities

get_links(C) :- init(_,_,LC,_,_,_, _, _),member(link(C),LC),!.
get_stages(C) :- init(_,_,LC,_,_,_, _, _),member(stage(C),LC),!.
get_junctions(C) :- init(_,_,LC,_,_,_, _, _),member(junction(C),LC),!.
get_initial(II) :- init(_,_,_,J,I,_, _, _),append(J,I,II),!.
get_statics(STATICS) :-  statics(STATICS),!.
get_current_state_facts(N1,T1,ALL) :-
        get_statics(ST),
        node(N1,T1,DYN,_,_),
        append(ST,DYN,ALL),!.

memberNOC(X,[X|_]).
memberNOC(X,[_|R]) :- memberNOC(X,R).

%%%%% *************CHECKS   **************************************
/* check link one way */

check_links :- get_links(C), check(C),!.
check([ ]):- nl, write('All links checked for occupancy value'), nl,nl.
check([C1 | C]):- get_initial(I), member(equals(occupancy(C1),_), I), check(C), !.
check([C1 | C]):- write('error: link '),write(C1),write(' has no occupancy value'), nl, check(C).
  
check_links_max :- get_links(C), checkm(C),!.
checkm([ ]):- nl, write('All links checked for maximum occupancy value'), nl.
checkm([C1 | C]):- get_initial(I), member(equals(capacity(C1),_), I),checkm(C), !.
checkm([C1 | C]):- write('error: link '),write(C1),write(' has no maximum occupancy value'), nl, checkm(C).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% recording and writing out stuff ......... 
        
% write out each time step if trace is on:
log_trace(T,N,DYN,PL,PR) :-
         sim_trace(on),
         tracefile(Trace),
         tell(Trace),
         nl,nl,write("===================="),nl,
         write(N), write("   "), write(T),nl,
         get_statics(STAT),
% write trace records saturated links also......
         write_trace(N,T,DYN,STAT),
         write_traceX(DYN,STAT),
         write_traceY(DYN),
         nl,write("CURRENT PLAN:  "),nl,write(PL),
         nl,write("CURRENT PRESSES: "),nl,write(PR),
         tell(user),
         !.
log_trace(_,_,_,_,_,_) :-
         !.

write_end_stuff(STAT,PLAN,DYN,Sim_Time) :-
        nl,write(' END OF SIMULATION TIME' ),nl,
        member( equals( occupancy(station_east), Occ1),  DYN),
        member( equals( occupancy(station_west), Occ2),  DYN),
        member( equals( occupancy(stubbings_east), Occ3),  DYN),
        member( equals( occupancy(stubbings_west), Occ4),  DYN),
        member( equals( occupancy(albert_south), Occ5),  DYN),
        member( equals( occupancy(callans_south), Occ6),  DYN),
        member( equals( occupancy(trades_north), Occ7),  DYN),
        member( equals( occupancy(hepton_south), Occ8),  DYN),
        member(equals(turnrate(fake,outside,station_west),INrateE),STAT),
        member(equals(turnrate(fake,outside,stubbings_east),INrateW),STAT),
        member(equals(defaultgreentime(centre_stage0) , LongCentre),PLAN),
        member(equals(defaultgreentime(centre_stage1) , ShortCentre),PLAN),
        member(equals(defaultgreentime(vocation_stage0) , LongVocation),PLAN),
        member(equals(defaultgreentime(vocation_stage1) , ShortVocation),PLAN),
        member(equals(defaultgreentime(fox_stage0) , LongFox),PLAN),
        member(equals(defaultgreentime(fox_stage1) , ShortFox),PLAN),
        member(equals(defaultgreentime(pelican_stage0) , LongPelican),PLAN),
        member(equals(interlimit(pelican_stage0) , ShortPelican),STAT),
        domainmodel(DM), 
        nl,write("  Domain Model is: "),write(DM),nl,
        initial(ISS), 
        nl,write("  Initial State is: "),write(ISS),nl,
        nl,write("  Centre Timings: "),write(LongCentre),write(" / "),write(ShortCentre),
        write("  Vocation Timings: "),write(LongVocation),write(" / "),write(ShortVocation),
        write("  Fox Timings: "),write(LongFox),write(" / "),write(ShortFox),
        write("  Pelican Timings: "),write(LongPelican),write(" / "),write(ShortPelican),
        nl,nl,
        write("Saturation levels: "),nl,
        write_saturation_levels,nl,
        write("No of vehicles enter the region from the EAST: "),
        R is Sim_Time*INrateE - Occ2,write(R),nl,
        write("No of vehicles left the region in the WEST END: "),
        write(Occ4),nl,
        write("No of vehicles enter the region from the WEST: "),
        R1 is Sim_Time*INrateW - Occ3,write(R1),nl,
        write("No of vehicles left the region in the EAST END: "),
        write(Occ1),nl, nl,
        write("Queue waiting in the EAST: "),
        Occ2R is round(Occ2),write(Occ2R),nl,
        write("Queue waiting in the WEST: "),
        Occ3R is round(Occ3),write(Occ3R),nl,
        write("Queue waiting in ALBERT STREET: "),
        Occ5R is round(Occ5),write(Occ5R),nl,
        write("Queue waiting in HEPTON ROAD: "),
        Occ8R is round(Occ8),write(Occ8R),nl,
        write("Queue waiting in the CALLANS: "),
        Occ6R is round(Occ6),write(Occ6R),nl,
        write("Queue waiting in the TRADES: "),
        Occ7R is round(Occ7),write(Occ7R),nl,
        nl,
        !. 

write_out_turns(STAT) :-
         member( equals(turnrate(A,B,C),TR), STAT),
         write(TR),write("  TURNRATE FOR: "),write(A),write("  "),write(B),write("  "),write(C),nl,
         fail.
write_out_turns(_) :- !.


assert_saturation_levels :-
        assert(saturation(park_east,0)),
        assert(saturation(park_west,0)),
        assert(saturation(new_east,0)),
        assert(saturation(new_west,0)),
        assert(saturation(market_east,0)),
        assert(saturation(market_west,0)).

write_saturation_levels :-
        saturation(X,LI),
        write("Time Saturated "), write(X), write(" is "),write(LI),nl,
        fail.
write_saturation_levels :- !.
        
write_list([ ]) :- nl,nl,write('END'),nl.
write_list([X|Y]) :- nl, write(X), write_list(Y),!.

write_listX([]) :- nl,write("["),nl,write("]"),nl.
write_listX([X]) :- nl,write("     "),write(X),nl,write("]"),nl.
write_listX([X|Y]) :- nl, write("     "),write(X), write(","),write_listX(Y),!.

write_trace(_,_,DYN,STAT) :-
         member(equals(occupancy(L),V), DYN),
         write_trace_1(L,V,STAT),
         fail.
write_trace(_,_,_,_).

write_traceX(DYN,ST) :-
         member( equals(greentime(J),GT), DYN),
         member(contains(J,S),ST), member(active(S),DYN),
         nl,write("GREEN TIME: "),write(J),write(" "),write("stage "),write(S),write(" "),write(GT),nl,
         fail.
write_traceX(_,_) :- !.
write_traceY(DYN) :-
         member( equals(queue(S),GT), DYN),
         nl,write("QUEUE: "),write(S),write(":   "),write(GT),nl,
         fail.
write_traceY(_) :- !.

record_sat(_,S) :- S < 85,!.
record_sat(L,_) :- 
        retract(saturation(L,X)),
        X1 is X + 1,
        assert(saturation(L,X1)),!. 

write_trace_1(L,V,STAT) :-
         member(equals(capacity(L),C), STAT),
         C < 9000,
         SAT is round(100*V/C),
%         V1 is round(V),
         V1 is V,
         record_sat(L,SAT),
         write(L),write("   OCCUPANCY:  "),write(V1), write(" SATURATION LEVEL:   "),write(SAT),nl,!.
write_trace_1(L,V,STAT) :-
         member(equals(capacity(L),C), STAT),
         C >= 9000,
%         V1 is round(V),
         V1 is V,
         write(L),write("   OCCUPANCY:  "),write(V1),nl,!.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

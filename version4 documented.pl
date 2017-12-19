% ==========================================================================================================================================================
%
% Notes to the Examiner
%
% you can run the predicates required by typing them into the console
% however, to use a more 'interactive' version, simply type: main. into the console
% this will present you with a menu that allows you to:
%
%  * play the coursework implementation of the game
%
%  * play a more complex version of the game (taking from more columns)
%
%  * run the analyseall predicate
%
%  * run a more advanced version of the analyseall predicate (not limited to just 2 columns)
%
%  * run benchmarking tests
%
% While predicates do share dependent predicates (and some are dependent on each other) I have tried to group the code together to the question it is most relevant to
%
%


main :-
	cls
	,
	menu(["Play Default Coursework Rules","Play With Custom Complexity","Analyse Win States","Benchmark Test","Benchmark State","Exit"],C)
	,
	main_choice(C)
	,
	!
	;
	main
	.




% ==========================================================================================================================================================
%
%  ###   #   # ####  #### ##### ###  ###  #   #    ##
% #   #  #   # #    #       #    #  #   # ##  #     #
% #   #  #   # ###   ###    #    #  #   # # # #     #
% #  ##  #   # #        #   #    #  #   # #  ##     #
%  #####  ###  #### ####    #   ###  ###  #   #     #
%
% ==========================================================================================================================================================
%
% =============================================================================
%
% move(S1,S2)
%
% A non deterministic predicate the upon backtracking returns all states S2
% that are reachable from S1 in one move

move(S1,S2) :- 
	get_game_complexity(N)					% get the current game complexity (note - to satisify the coursework requirements, I can remove this step and substitute the number 2 for N, however I wanted to demonstrate the flexibility of my solution)
	,
	move_all_N(S1,S2,N)						% process all the possible moves for that level of game complexity
	.


% Predicates for game complexity
%
% The game complexity is determined as the maximum number of stacks that a player can remove counters from
%
% The complexity required for the coursework is 2
% 
% However, as my predicates allow for any complexity I have included the option to set that.
%
% game_depth(N) can be asserted, and the most recent assertion is used as the present game depth.
%
% The reason I have done it this way is so I can modify the behaviour of the move(S,S2) predicate dynamically
% and therefore can use all future predicates without modification.
%
:- dynamic game_depth/1.
game_depth(2).								% there needs to be a game_depth asserted or the code will fail

set_game_complexity(N) :-
	N > 0									% checks state being asserted is greater than zero
	,
	retractall(game_depth(_))
	,
	assert(game_depth(N))					% asserts state
	,
	init_winning							% clears any asserted win/lose states
	.

old_set_game_complexity(N) :-
	N > 0									% checks state being asserted is greater than zero
	,
	assert(game_depth(N))					% asserts state
	.

get_game_complexity(X) :-
	findall(N,game_depth(N),N)				% gets a list of all the game_depths asserted
	,
	game_status(N,X)						% sets X to the most recent
	,
	!										% cut
	.

game_status([N|_],X) :- X = N.
	
old_game_status([],X) :- X = 2.					% defaults to complexity 2 if there are no asserted game depths

% finds the most recently asserted game complexity
old_game_status([N|Ns],X) :- 
	Ns == []								% end of list found
	,
	X = N									% set X to N
	,
	!										% cut
	
	;										% OR...
	
	not(Ns == [])							% still items in list
	,
	game_status(Ns,X)						% recurse until the end of list
	.


% move_all_N(S1,S2,X,N)
%
% this will return all of the moves for the provided state S1 (in the form [S1|S1s]) from 
% game complexity 1 up to game complexity N and all complexities inbetween
%
% S1 - a list of numbers representing each stack of counters (in the form [S1|S1s]
%
% S2 - a legal move (backtracking will produce all legal moves)
%
% N - the maximum game complexity to be checked for
%
move_all_N([],[],_).						% empty state can only return an empty state (this should never be called by my code, but I have included for completeness)

move_all_N([S1|S1s],S2,N) :-
	N >= 1									% Only complexities greater than zero allowed
	,
	between(1,N,I)							% loops I from 1 to N and returns all possible moves upon backtracking
	,
	move_X_init([S1|S1s],S2,I)				% calls the initialisation predicate for checking possible move states and assigns result to S2
	.

	


% move_X_init(S1,S2,X,N)
%
% This initialises the steps to remove counters from a stack/stacks
%
% [S1|S1s] - a list representing the stacks
%
% S2       - a possible move state (ie - this is returned)
%
% X        - X the number of stacks to remove counters from
move_X_init([S1|S1s],S2,X) :-
	(
		length([S1|S1s],L)					% checks the number of stacks
		,
		L >= X								% can only produce moves if there are enough stacks to remove counters from
	)
	,
	(
		move_X_start([S1|S1s],S2,X,1)		% removes all possible counters starting with this stack and returns all possible moves upon backtracking
		
		;									% OR...
		
		not(S1s == [])						% providing there are more stacks
		,
		move_X_init(S1s,S3,X)				% recurses this predicate for the resulting sub-stack
		,
		S2 = [S1|S3]						% appends results to head of stack and returns as S2 (which will be carried up the recursion to provide a complete stack)
	)
	.
	
% move_X_start([S1|S1s],S2,X,N)
%
% This predicate starts removing counters from multiple stacks.
% It can remove beans from any number of stacks, from 1 to however many stacks are present.
%
% X are the number of stacks to have stones removed from , N are the number of counters to remove from all those stacks.
%
% It will backtrack to calling itself again, but with N increased by one and keep doing this until N is greater than
% the stack size.
%
% If the starting stack is reduced to zero, then it will remove the stack from the return state.
% (Note, additional stacks are handled by move_X_remove, so that process for them occurs there)
% 
% [S1|S1s] - a list representing the stacks
%
% S2       - a possible move state (ie - this is returned)
%
% X        - X the number of stacks to remove counters from
%
% N        - N the number of counters to remove on this pass
%
move_X_start([S1|S1s],S2,X,N) :- 
	(
		S1 >= N								% if there aren't at least X counters in the stack then it can't remove them
		,
		S1a is S1 - N						% subtract this number from the stack to create the new quantity
		,
		X2 is X - 1							% decrement counter by 1
		,
		(
			X2 > 0							% if the counter is still greater than zero, we need to look for more columns
			,
			length(S1s,L) , L >= X2			% only possible if there are still stacks in the sub-stacks to search
			,
			move_X_remove(S1s,S2a,X2,N)		% get the possible sub-stacks with N removed
			,
			(
				S1a > 0						% if the initial stack still has counters it needs to be included
				,
				S2 = [S1a|S2a]				% create resulting stack
				
				;							% OR...
				
				S1a == 0					% if the initial stack is empty, we need to remove it
				,
				S2 = S2a					% create the resulting stack
			)
			
			;								% OR, if the counter is complete:
			
			X2 == 0							% X2 will be zero
			,
			(
				S1a > 0						% if initial stack still has counters it needs to be included
				,
				S2 = [S1a|S1s]				% stack will be this reduced stack, plus remaining sub-stack
				
				;							% OR...
				
				S1a == 0					% if the initial stack is empty, we need to remove it
				,
				S2 = S1s					% return just the remaining sub-stack
			)
		)
	)
	
	;										% OR...
	
	N2 is N + 1								% increment N
	,
	S1 >= N2								% check S1 is still valid for this new N2 or we can abort now for efficiency
	,
	move_X_start([S1|S1s],S2,X,N2)			% recurse with incremented N value
	.


	
% move_X_remove([S1|S1s],S2,X,N)
%
% This predicate removes counters from sub-stacks, once an initial stack has been selected.
% It recurses until X is 1,removing N from every possible combination of stacks within the substack on each backtrack.
%
% If a stack is reduced to zero, it is removed.
% 
% [S1|S1s] - a list representing the stacks
%
% S2       - a possible state (ie - this is returned)
%
% X        - X the number of remaining stacks to remove counters from
%
% N        - N the number of counters to remove on this pass
%
move_X_remove([S1|S1s],S2,X,N) :-
	(
		S1 >= N								% can only remove counters from the stack, if it contains at least N counters
		,
		S1a is S1 - N						% new stack value will be N less
		,
		X2 is X - 1							% decrement X
		,
		(
			X2 > 0							% if X2 is greater than 0 then it still needs to remove counters from more substacks
			,
			length(S1s,L) , L >= X2			% check there are enough stacks left
			,
			move_X_remove(S1s,S2a,X2,N)		% recurse this predicate to get all possible sub-stacks
			,
			(
				S1a > 0						% if initial stack still contains counters, it needs to be included
				,
				S2 = [S1a|S2a]				% returned stack is initial stack, plus the modified sub-stack
				
				;							% OR...
				
				S1a == 0					% is the initial stack is empty it should be removed
				,
				S2 = S2a					% returned stack is just the modified substack
			)
			
			;								% OR...
			
			X2 == 0							% this means no further stacks need counters removing from them
			,
			(
				S1a > 0						% if initial stack contains counters, it needs to be included
				,
				S2 = [S1a|S1s]				% returned stack is the initial stack, plus unmodified sub-stack
				
				;							% OR...
				
				S1a == 0					% if the initial stack is empty, it should be removed
				,
				S2 = S1s					% returned stack is just the unmodified stack
			)
		)
	)
	
	;										% OR...
	
	not(S1s == [])							% if the remaining stacks are not empty
	,
	(
		length(S1s,L)						% check number of remaining stacks
		,
		L >= X								% there must be at least enough to continue removing counters	
	)
	,
	move_X_remove(S1s,S2b,X,N)				% recurse this predicate on the sub-stack
	,
	S2 = [S1|S2b]							% append result to head, and pass back up the recursion to produce a complete move
	.
	

	
% LEGACY CODE BELOW
%
% Below is code I wrote during development, which works but is either less efficient or less 	
	

% pmove(S,S2)
%
% A predicate I used during debugging that printed the move to the console.
%
pmove(S1,S2) :- move(S1,S2) , write(S2) , nl.
	
	
% old_move
%
% This is my original implementation of move(S,S2) that used different predicates for a single stack and multi stack removal
%
% old_move_three(S,S2) represents a version of the game of game complexity 3, before I replaced the predicates with a dynamic version of the game
%
old_move([],[]).
old_move(S,S2) :- not(S==[]) , (move1(S,S2) ; move2(S,S2)).

old_move_three([],[]).
old_move_three(S,S2) :- not(S==[]) , move1(S,S2) , move2(S,S2) , move3(S,S2).


% a stack of a single bean can only move to an empty stack
move1([1],[]).

% a stack of a more than a single bean and reduce to a stack one less,
% or all the possibilities for a stack that is one smaller
move1([S1|[]],S2) :- S1 > 1 ,  (S3 is S1 - 1 ,S2 = [S3] ; S3 is S1 - 1 , move1([S3],S2)).
move1([S1|S1s],S2) :- not(S1s == []) , S1 == 1 , S2 = S1s.
move1([S1|S1s],S2) :- not(S1s == []) , S1 > 1 , (S3 is S1 - 1, S2 = [S3|S1s] ; S3 is S1 - 1, move1([S3],S4)) , (S4 == [] , S2 = S1s ; not(S4 == []) , append(S4,S1s,S2)).
move1([S1|S1s],S2) :- not(S1s == []) , move1(S1s,S3) , S2 = [S1|S3].

% moving more than 2 requires a more advanced algorithm that checks future removals are possible
move2([S1|S1s],S2) :- move_X_init([S1|S1s],S2,2).
move3([S1|S1s],S2) :- move_X_init([S1|S1s],S2,3).

% ==========================================================================================================================================================
%
%  ###   #   # ####  #### ##### ###  ###  #   #     ##
% #   #  #   # #    #       #    #  #   # ##  #    #  #
% #   #  #   # ###   ###    #    #  #   # # # #      #
% #  ##  #   # #        #   #    #  #   # #  ##     #
%  #####  ###  #### ####    #   ###  ###  #   #    ####
%
% ==========================================================================================================================================================
%
% win(S)
%
% A predicate that succeeds if S is a winning position for the player whose
% turn it is and fails if it isn't
%
% S - a list containing the number of counters in each stack

win(S) :- can_force_win(S) , !.		% this does presently just called  - can_force_win/1 so in theory I could just rename can_force_win/1 to win/1
									%
									% However, I originally intended to make can_force_win/2 where the second input was to specify the player and win/1
									% called can_force_win(S,"player2A") with the idea of expanding the predicate to allow for multiple player games
									% ie, 3 player or 4 player, with the players also having a next_player/2 predicate
									%
									% my can_force_win predicate also differed slightly and was used by my analyse predicate so my original code needed
									% it separate then as well. When I started optimising my predicates I could speed up win/1 by cutting out some of the
									% operations needed for analyse/1
									%
									% I debated renaming can_force_win/1 to win/1 before submitting, however I didn't for the following reasons:
									%
									%  * it shows the scope for expanding the code in the future to add more players and is therefore more maintainable
									%
									%  * can_force_win describes what that predicate is doing semantically
									%
									%  * there is no real performance hit (literally, just calls another predicate so one extra operation in a hundreds of thousands)
									%
									%  * I have explained that the optimisation could be made here in this text, and justified my reasons for not doing it




% I need to have a winning_position and losing_position defined so Prolog knows predicates by those names exist
% however,they need to be declared as dynamic so retractall works later on

% this is ALWAYS a losing position regardless of complexity, so we can make it a fact.
:- dynamic losing_position/1.
losing_position([1]).

% this is ALWAYS a winning position regardless of complexity, so we can make it a fact.
:- dynamic winning_position/1.
winning_position([1,1]).

% creates a database of losing positions and winning positions.
% losing positions either lose or only move to winning positions.
% Winning positions are positions that move to these positions.
init_winning :- retractall(losing_position(_)) , retractall(winning_position(_)) ,  assert(losing_position([1])).
init_winning(S) :- retractall(losing_position(_)) , retractall(winning_position(_)) ,  assert(losing_position(S)).

% predicate for analysing whether a winning condition has already been calculated
% because the order of the stacks is not important, this will msort the stacks, and then check if the sorted stack has been asserted
is_winning_position(S) :- msort(S,S2) , winning_position(S2).

% predicate for asserting a winning position
% msorts it first, as stack order is not important
record_position_as_winning(S) :- msort(S,S2) , not(winning_position(S2)) , assert(winning_position(S2)) ; true.

% equivalent predicates for losing positions below
is_losing_position(S) :- msort(S,S2) , losing_position(S2).
record_position_as_losing(S) :- msort(S,S2) , not(losing_position(S2)) , assert(losing_position(S2)) ; true.

% can_force_win(S)
%
% A predicate for determining if the current state is a winning position or not. Returns true if it is but fails if it isn't.
%
% S - a list containing the number of counters in each stack
can_force_win(S) :-
	is_winning_position(S)				% if the position has already been calculated as winning then we don't need to check further
	, 
	!
	
	;									% OR....
	
	not(is_winning_position(S))			% backtracking will check this, so it's more efficient to stop also running this half of the predicate
	,
	not(is_losing_position(S))			% the position can't be a losing position
	,
	move(S,M)
	,
	determine_position_is_losing(M)		% collect a list of the moves that leave the next player in a guaranteed losing position
	,
	record_position_as_winning(S)		% record this position as winning
	.


% determine_position_is_losing(S)
%
% A predicate that determines if the state is losing (by recursing on win)
%
% S - List of states to test
% W - List of those states that are winning moves
	
determine_position_is_losing([]) :- fail. 			% no winning moves for empty move.


determine_position_is_losing(S) :-
	not(S==[])							% S is not empty (to stop backtracking on empty state, which is picked up above)
	,
	is_losing_position(S)				% S is an existing discovered losing condition
	,
	!
	.

determine_position_is_losing(S) :- 
	not(S==[])							% S is not empty (to stop backtracking on empty state, which is picked up above)
	,
	is_winning_position(S)				% S is an existing discovered winning condition
	,
	!
	,
	fail
	.


determine_position_is_losing(S) :-
	not(S==[])							% S is not empty (to stop backtracking on empty state, which is picked up above)
	,
	not(is_losing_position(S))			% not a winning position (to stop backtracking on previous successes)
	, 
	not(is_winning_position(S))			% not a winning position (to stop backtracking on previous successes)
	,
	not(can_force_win(S))				% recurse on can_force_win until a known won or lose state is determined
	,
	!
	,
	record_position_as_losing(S)		% if the position can't win it will now be asserted as losing
	.
	

determine_position_is_losing(S) :-
	not(S==[])							% S is not empty (to stop backtracking on empty state, which is picked up above)
	,
	not(is_losing_position(S))			% not a winning position (to stop backtracking on previous successes)
	, 
	not(is_winning_position(S))			% not a winning position (to stop backtracking on previous successes)
	,
	record_position_as_winning(S)		% if the predicate gets this far, then it means it was a winning position
	,
	fail
	.



% ==========================================================================================================================================================
%
%  ###   #   # ####  #### ##### ###  ###  #   #    ###
% #   #  #   # #    #       #    #  #   # ##  #       #
% #   #  #   # ###   ###    #    #  #   # # # #     ##
% #  ##  #   # #        #   #    #  #   # #  ##       #
%  #####  ###  #### ####    #   ###  ###  #   #    ###
%
% ==========================================================================================================================================================
%
% =============================================================================
%
% analyse(S)
%
% A predicate that determines for any game state S whether it's a win or a loss
% for the player whose turn it is, given the best play on both sides
%
% If it is a win, it should print out all the winning moves that can be made
% for that state, otherwise it should return that there are no winning moves

analyse(S) :-
	analyse_move(S,W)
	,
	not(W==[])							% return true if that list is not empty
	,
	sort(W,W2)							% sorts and eliminates duplicates
	,
	write(W2)							% output all the winning moves
	,
	!									% stop backtracking
	
	;									% OR.... if the above fails, it means there are no winning moves
	
	write("No winning moves")			% output message that 
	,
	!
	.
	

analyse_move(S,R) :-
	not(is_losing_position(S))			% the position can't be a losing position
	,
	findall(M,move(S,M),M)				% collect all possible moves
	,
	sort(M,M2)							% remove duplicates (simplify NOT used because we need human readable output - simplify is algoritmically better, but switches columns to reduce further)
	,
	not(M2==[[]])						% can't win if only move is to empty board
	,
	find_all_losing_states(M2,W)		% collect a list of the moves that leave the next player in a guaranteed losing position
	,
	R = W
	,
	!
	
	;
	
	R = []
	,
	!
	
	.


% find_all_losing_states(S,W)
%
% A predicate that returns a list of all the losing states (ie, winning moves) from a list of states (ie, possible moves)
%
% S - List of states to test
% W - List of those states that are winning moves
	
find_all_losing_states([],[]). 			% no winning moves for empty move list.

% iterates through a list of moves and discovers all losing states (which are therefore winning moves):
find_all_losing_states([S|Ss],W) :- 
	S==[]								% emptying the board is a winning state for the opponent
	,
	find_all_losing_states(Ss,W2)		% therefore, recurse on the rest of the list and return the winning moves for the remaining states
	,
	W = W2								% assign W2 to W (as S is a winning state and therefore a losing move)
	.

find_all_losing_states([S|Ss],W) :-
	not(S==[])							% S is not empty (to stop backtracking on empty state, which is picked up above)
	,
	is_losing_position(S)				% S is an existing discovered losing condition
	,
	find_all_losing_states(Ss,W2)		% find all losing states for the rest of the list
	,
	W = [S|W2]							% append those states to a list headed by S, (as S is a losing state and therefore a winning move)
	.

find_all_losing_states([S|Ss],W) :- 
	not(S==[])							% S is not empty (to stop backtracking on empty state, which is picked up above)
	,
	is_winning_position(S)				% S is an existing discovered winning condition
	,
	find_all_losing_states(Ss,W2)		% therefore, recurse on the rest of the list and return the winning moves for the remaining states
	,
	W = W2								% assign W2 to W (as S is a winning state and therefore a losing move)
	.


find_all_losing_states([S|Ss],W) :-
	not(S==[])							% S is not empty (to stop backtracking on empty state, which is picked up above)
	,
	not(is_losing_position(S))			% not a winning position (to stop backtracking on previous successes)
	, 
	not(is_winning_position(S))			% not a winning position (to stop backtracking on previous successes)
	,
	find_all_losing_states(Ss,W2)		% find the losing states of the remain states
	,
	add_if_no_winning_moves(S,W2,W3)	% to minimise the returning of the above state, this has been moved to a different predicate
	,
	W = W3
	.


% add_if_no_winning_moves(S,W,R)
%
% A predicate that adds a state to a list of states if that state has no winning moves
%
% S - state to be checked
%
% W - list of states that state should be added to if successful
%
% R - the returned list
%
add_if_no_winning_moves(S,W,R) :-
	can_force_win(S)					% if S can force a win then it is not a winning state
	,
	R = W								% R is simply the existing list
	.
	
add_if_no_winning_moves(S,W,R) :-
	not(can_force_win(S))				% if S cannot force a win, then it has no winning moves
	,
	R = [S|W]							% add S to the head of the list and return
	,
	record_position_as_losing(S)		% record S as a losing state (speeds up later checks)
	.


	
% ==========================================================================================================================================================
%
%  ###   #   # ####  #### ##### ###  ###  #   #    #
% #   #  #   # #    #       #    #  #   # ##  #    # 
% #   #  #   # ###   ###    #    #  #   # # # #    # #
% #  ##  #   # #        #   #    #  #   # #  ##    ####
%  #####  ###  #### ####    #   ###  ###  #   #      #
%
% ==========================================================================================================================================================
%
% =============================================================================
%
% analyseall(N)
%
% Performs the above analysis for every game state with one or two heaps of
% every possible size up to N
%
% N - maximum game size to be checked
%
analyseall(N):- 
	N > 0										% N must be greater than zero
	,
	findall(T,analyse_single_games(N),T)		% return all the single games
	,
	findall(T2,analyse_double_games(N),T2)		% return all the double games
	.

% analyse_single_games(N)
%
% Predicate for outputting all variations of a single stack
%
% N - maximum game size to be checked
%
analyse_single_games(N) :-
	between(1,N,I)								% loop I from 1 to N
	,
	analyse_game([I])							% run analyse_game(S) predicate for game [I]
	.

% analyse_double_games(N)
%
% Predicate for outputting all variations of a double stack
%
% N - maximum game size to be checked
%
analyse_double_games(N) :-
	between(1,N,J)								% loop J from 1 to N
	,
	between(1,J,I)								% loop I from J to N (looping from 1 to N will duplicate mirror games already processed)
	,
	analyse_game([I,J])							% run analyse_game(S) predicate for game [I,J]
	.

% analyse_game(S)
%
% A wrapper predicate that merely runs the analyse(S) predicate, but formats it in a nice way for the console ouput
%
% S - game state to be analysed
%
analyse_game(S) :- 
	write(S) , write(":") , tab(5)				% prints the state, with a colon and white space
	,
	analyse(S)									% prints the winning moves
	,
	nl											% new line
	.



:- dynamic analysedX/1
analysedX([]).					% I need to define one for the retractall to work in analyse_all_for_X_stacks/2

% EXTRA STUFF BELOW - MORE ADVANCED PREDICATE TO RECURSIVELY CHECK MORE THAN 2 STACKS
	
% analyse_all_for_X_stacks(N,X)
%
% Performs the above analysis for every game state up to X stacks
% every possible size up to N
%
% N - maximum game size to be checked
%
% X - check up to this many stacks
%	
analyse_all_for_X_stacks(N,X) :-
	retractall(analysedX(_))											% clears this predicate that is used to prevent duplication
	,
	init_winning														% clears the existing win / lose states
	,
	X2 is X + 1 														% this is just done this way so the predicate is called using human relatable numbers
	,
	findall(T,analyseallX(N,X2),T)
	,
	nl 																	% below is code just for producing an output of the results
	,
	findall(W,winning_position(W),W)									% count winning positions
	,
	length(W,WC)
	,
	findall(L,losing_position(L),L)										% count losing positions
	,
	length(L,LC)
	,
	GC is LC + WC 														% Game Count is the sum of these
	,
	write("Analysed ") , write(GC) , write(" games and found:") , nl 	% output textual summary of results follows
	,
	write(WC) , write(" Winning Positions") , nl
	,
	write(LC) , write(" Losing Positions") , nl
	.
	
	

analyseallX(N,X) :-
	X > 1 						% this half of the predicate is just to recurse down to a 1 stack first, so if we've got to 1 we want  to jump to the next part
	,
	X2 is X - 1 				% reduce by one and recurse
	,
	analyseallX(N,X2)			% we want to recurse down to the smallest X stack (ie 1 column, before doing the larger columns)

	;							% OR...

	analyseallXr(N,X,1,0,[]) 	% run the actual meat - this will automatically be run on X = 1, and run on Xs greater than 1 on backtracking
	.


% analyseallXr
%
% This predicate builds up a list until it is length X and tries every permutation of those games
%
analyseallXr(N,X,S,D,L) :-
	D2 is D + 1 					% keep iterating D until it's less than X as that means we've got long enough
	,
	D2 =< X 						% if it's exceeded X we've gone too far, so fail!
	,
	(
		between(S,N,I) 				% loop for all values
		,
		append(L,[I],L2)			% add to the end of L
		,
		analyseallXr(N,X,I,D2,L2)	% recurse until failstate (that's where we're too many columns deep) and analyse game produced

		;							% OR state to backtrack to

		not(L==[])					% if it's an empty list, don't bother
		,
		not(analysedX(L))			% check it's not already analysed this game
		,
		analyse_game(L)				% analyse the game
		,
		assert(analysedX(L))		% assert it as analysed so it doesn't get repeated
	)
	.
	
	
% ==========================================================================================================================================================
%
%  ###   #   # ####  #### ##### ###  ###  #   #    ####
% #   #  #   # #    #       #    #  #   # ##  #    #  
% #   #  #   # ###   ###    #    #  #   # # # #    ###
% #  ##  #   # #        #   #    #  #   # #  ##       #
%  #####  ###  #### ####    #   ###  ###  #   #    ###
%
% ==========================================================================================================================================================
%
% =============================================================================
%
% play(S)
%
% Given the game state S it will play the game interactively, making the best
% move each time.
%
% It should:
%
%  * Detect when the game is over and who won
%
%  * If the game is not over, choose a winning move if possible, otherwise
%    select any legal move
%
%  * Should detect if a user tries to make an illegal move and ask them to try
%    again


play(S) :- 
	print_game_rules						% this is just displaying some nice information to the player
	,
	cls 									% after the player has pressed enter, the game starts, clearing the game
	,
	!										% the print_game_rules predicate will backtrack due to user input, so this is to stop that happening
	,
	play_game(S,W)							% runs the state S through the play_game predicate, which assigns the winner's identity to W
	,
	!										% to prevent backtracking to the play_game predicate, which contains user inputs which are backtrackable
	,
	cls 									% clear the screen of the game data and print the winner's identity
	,
	write("GAME OVER") , nl
	,
	write("Winner: ") , write(W) , nl
	.


play_game(S,R) :-
	draw_board(S)											% draw_board displays a nice ASCII art reprentation of the game state S provided
	,
	write("What move would you like to make?") , nl , nl 
	,
	move_from_text_command(S,M)								% predicate requests a move from the player, which it assigns to M
	,
	!														% prevents backtracking as move_from_text_command has user inputs
	,
	try_move(S,M,W)											% tries playing the player's move (in current implementation, player's move is always possible due to textual interpretation, however the game DOES still check, in case this predicate requesting the user move is replaced with a state input version
	,
	R = W 													% assign W to the return
	.

print_game_rules :-
	cls
	,
	get_game_complexity(G)													% this gets the game complexity and gives slightly different move explanations for different complexities
	,
	write("NEW GAME - Complexity ") , write(G) ,nl
	,
	write("=======================") ,nl ,nl
	,
	write("To make a move, please describe your move in the form:") ,nl,nl
	,
	(
		G == 1 																% if complexity is 1 then no need to explain multiple column complexity
		,
		!
		
		;
		
		G == 2 																% if complexity is 2 then there is only two column variant
		,
		write("take 3 from A and C") ,nl
		,
		!
		
		;
		
		write("take 2 from D and C and G") ,nl 								% all other cases, show multiple columns require multiple ands
	)
	,
	write("take 1 from B") ,nl,nl
	,
	write("Press enter after typing your move to play it.") , nl ,nl
	,
	write("Note, spaces are essential, and instructions are case sensitive (so UPPERCASE for column letters and lowercase for words)") , nl , nl
	,
	user_press_enter("Press Enter to start the game")
	.
	
% max_pile_size([S|Ss],N)
%
% searches a pile and sets N to the largest size
max_pile_size([],0).
max_pile_size([S|Ss],N) :-
	max_pile_iterate([S|Ss],0,M)
	,
	N = M
	.
	
% max_pile_iterate([S|Ss],N,R)
%
% used by max_pile_size to iterate through a list and return the largest pile size (called from max_pile_size)
max_pile_iterate([],N,R) :- R = N.
max_pile_iterate([S|Ss],N,R) :-
	S > N
	,
	max_pile_iterate(Ss,S,R)
	,
	!
	;
	max_pile_iterate(Ss,N,R)
	.

% render_level([S|Ss],L)
%
% predicate for printing a level of the ASCII graphical representation of game state [S|Ss] at level L
render_level([],_) :- nl.
render_level([S|Ss],L) :-
	S == L
	,
	write(" ____  ")
	,
	render_level(Ss,L)
	;
	S > L
	,
	write("(____) ")
	,
	render_level(Ss,L)
	;
	S < L
	,
	write("       ")
	,
	render_level(Ss,L)
	.

	
% render_state(S,M)
%
% renders an ASCII graphical representation of the game state up to level M
% if M is the size of the highest pile, it will render the entire game state.
render_state(S,M) :-
	between(-1,M,I)
	,
	L is M - I
	,
	render_level(S,L)
	.

% write_labels([S|Ss])
%
% writes the quantities for each state below
write_labels([]) :- nl.
write_labels([S|Ss]) :-
	S > 9
	,
	write("  ") , write(S) , write("   ")
	,
	write_labels(Ss)
	,
	!
	;
	write("   ") , write(S) , write("   ")
	,
	write_labels(Ss)
	.
	
% write_labels([S|Ss],[L|Ls])
%
% writes the quantities for each state below the column, but also with a Label from L|Ls (make sure these are a list of single characters)
write_labels([],_) :- nl.
write_labels([S|Ss],[L|Ls]) :-
	S > 9
	,
	write(" ") , write(L) , write(":") , write(S) , write("  ")
	,
	write_labels(Ss,Ls)
	,
	!
	;
	write(" ") , write(L) , write(": ") , write(S) , write("  ")
	,
	write_labels(Ss,Ls)
	.

	
% draw_board(S)
%
% draws the state S to the console, using an ASCII style graphical representation,with numbers underneath
draw_board(S) :-
	max_pile_size(S,M)
	,
	findall(R,render_state(S,M),R)
	,
	nl
	,
	write_labels(S,["A","B","C","D","E","F","G","H","I","J"])
	,
	nl
	.


	
	
	
	
	
	
	
	

try_move(S,[],R) :-
	move(S,[])
	,
	!
	,
	R = "The Computer"
	,
	true
	.

try_move(S,M,R) :-
	move(S,M)
	,
	!
	,
	write("You successfully make the move: ") , write(M) , nl , nl
	,
	draw_board(M)
	,
	ai_move(M,W)
	,
	R = W
	.

try_move(S,M,R) :-
	cls , write(M) , write("is an illegal move!!") , nl
	,
	!
	,
	play_game(S,W)
	,
	R = W
	.
	
ai_move(S,R) :-
	analyse_move(S,M)
	,
	!
	,
	make_ai_move(S,M,W)
	,
	R = W
	.
	
make_ai_move(S,[],R) :-
	move(S,M)
	,
	(
		M == []
		,
		!
		,
		R = "You"
		,
		true
		
		;
		
		write("The Commputer worriedly makes the move ") , write(M) , write(": ") , nl
		,
		!
		,
		play_game(M,W)
		,
		R = W
	)
	,
	!
	.
	
	
make_ai_move(_,[M|_], R) :-
	write("The Computer confidentally makes the move ") , write(M) , write(": ") , nl
	,
	play_game(M,W)
	,
	R = W
	,
	!
	.

	
	

% own predicates:

% =============================================================================
%
% simplify(S1,S2)
%
% Takes a list of game states,sorts all of them and then removes duplicates (ie [1,2,3] / [2,3,1] / [3,2,1])
simplify(S1,S2) :- sort_states(S1,S3) , sort(S3,S4) , S2 = S4.


% sort_states([S1|S1s],S2)
%
% iterate through a list of states and puts them all in numerical order.
%
% eg [[1,3,2],[1,3,5],[5,1,3]] will become [[1,2,3],[1,3,5],[1,3,5]]  
sort_states([],[]).
sort_states([S1|S1s],S2) :- msort(S1,S1a) , (S1s == [] , S2 = [S1a] ; not(S1s == []) , sort_states(S1s,S2t) , S2 = [S1a|S2t]).

% input_valid_move
%
% requests a valid move state from a user (and repeats until one is received)
%
% ie - any valid list of numbers will work, but if non numbers are used then it's not a valid state
%
input_valid_move(M) :-
	readln(I)
	,
	atom_string(M,I)											% converts string into a atom (ie a list of numbers)
	,
	move(M,_)													% if the move predicate can be used without failing, then it's a valid state
	,
	!															% avoid backtrack on readln

	;															% OR (failed to get valid move state)

	nl , nl , write("Please enter a valid move state") , nl
	,
	input_valid_move(M2)										% recurse until valid move is found
	,
	M = M2
	,
	!															% avoid any backtracking on previous user inputs
	.
	


% column_in_list
%
% this is a predicate for checking a column exists in a list
%
% eg - for a 3 column layout (ie, [3,2,1]) then "A" "B" or "C" is true, any other column will fail
column_in_list(_,_,[],_,_) :- fail.
column_in_list(I,COLS,[CL|CLs],CLR,CL2) :-
	column_id(CL,COLS,0,CID)
	,
	CID == I
	,
	append(CLs,CLR,CL2)
	
	;
	
	column_in_list(I,COLS,CLs,[CL|CLR],CL2)
	.

% column_id(CL,[COLS|COLSs],I,R)
%
% this is goes through a list of letters and returns the zero indexed positional number if the letter being searched for is in it
%
% eg, searching for B in ["A","B","C"] will be 1 / searching for "G" in ["A","B","C","D"] will fail
%
%
% CL  			Column Letter being searched for
%
% [COLS|COLSs] 	List of column letters in order , eg ["A","B","C","D"]
%
% I 			Iterator (will increment on recursion)
%
% R 			Return value (if found, otherwise fail)
%
column_id(_,[],_,_) :- fail.
column_id(CL,[COLS|COLSs],I,R) :-
	(
		CL == COLS
		
		;
		
		atom_string(CL , CLs)
		,
		CLs == COLS
	)
	,
	R = I
	
	;
	
	I2 is I + 1
	,
	column_id(CL,COLSs,I2,R)
	.
	

reduce_columns(_, [] , _ , [] , SB , S2, _) :- S2 = SB , !.
reduce_columns(_, [_|_] , _ , [] , _ , _ , _) :- fail.
	
reduce_columns(Q, CL , COLS , [S1|S1s] , SB , S2 ,I) :-
	(
		column_in_list(I,COLS,CL,[],CL2)
		,
		(
			S1 < Q
			,
			!
			,
			fail
			
			;
			
			S1a is S1 - Q
			,
			(
				S1a > 0
				,
				append(SB,[S1a],SBs)
				
				;
				
				SBs = SB
			
			)
			
		)
		
		;
		
		append(SB,[S1],SBs)
		,
		CL2 = CL
	)
	,
	(
		length(CL2,L) , L == 0
		,
		append(SBs,S1s,S2)
		,
		!
		,
		true
		
		
		;
		
		!
		,
		I2 is I + 1
		,
		reduce_columns(Q, CL2 , COLS , S1s , SBs , S2 , I2)
	)
	.

% apply_move_command(Q,CL,S1,S2)
%
% this takes a list of column letters (CL), a quantity to be removed (Q), and a start state (S1)
%
% it recurses through the state, removing Q from each stack identified, and returns it as S2
%
% Q  = Quantity to remove from each stack - eg 3 for the command (take 3 from A and D)
%
% CL = Column List to remove quanity from - eg ["A","D"] for the command (take 3 from A and D)
%
% S1 = Start state (this will the state to apply the move to)
%
% S2 = the returned state, if the move is possible (ie, take 3 from A and B for the state [3,1,4] will fail as B only has one counter)
%
apply_move_command(Q,CL,S1,S2) :-
	!
	,
	reduce_columns(Q, CL,["A","B","C","D","E","F","G","H","I","J"] , S1 , [] , S2 , 0).
	

% move_from_text_command(S1,S2)
%
% predicate for requesting and processing a user's move for the provided state
%
% S1 = Start state (this will the state to apply the move to)
%
% S2 = the returned state, if the move is possible
%
move_from_text_command(S1,S2) :-
	readln(T)														% read the user's input - in the form: take 4 from A and C
	,
	!
	,
	(
		process_move_command(T,Q,CL)								% process the text input to produce a Quantity to be removed (Q) and a list of Columns (CL) to remove that quantity from. eg: Q = 4 , CL = [A,C]
		
		
		;
		
		CL = []														% if an invalid state is provided, this will set CL to a blank list, which will trigger a recursion of this predicate to get a valid state
		
	
	)
	,
	!
	,
	(
		length(CL,L) , L > 0 , get_game_complexity(G) , L =< G		% check for a positive number of columns that doesn't exceed the game complexity (2 for coursework rules)
		,
		apply_move_command(Q,CL,S1,S2)								% apply this move to the game state
		,
		move(S1,S2)													% double check this move is legal but ensuring it exists within the valid moves list
		
		;
		
		write("Invalid move! Please re-enter your move...") , nl	% on fail, ask user to input a valid move
		,
		move_from_text_command(S1,S2R)
		,
		S2 = S2R
	)
	.


% process_move_command(S,Q,CL)
%
% This is a predicate for processing a move that is provided as a list of words
%
% it then sets Q to the quanity and CL to a list of columns to remove stones from
%
% The move must start as the form:
% 
% ["take",N,"from",C]
%
% where N is the number of stones to be removed and C is a string of a column id, eg "A"
%
% this list can then continue with unlimited: ["and",C] for additional columns
%
% for:
%		S == ["take",3,"from","B","and","H","and","E"]
%
% will return:
%		Q == 3
%		CL == ["B","H","E"]
%
process_move_command(S,Q,CL) :-
	next_word_from_list(S,S2,W1)
	,
	W1 == take
	,
	next_word_from_list(S2,S3,W2)
	,
	integer(W2)
	,
	Q = W2
	,
	next_word_from_list(S3,S4,W3)
	,
	W3 == from
	,
	next_word_from_list(S4,S5,W4)
	,
	WL = [W4]
	,
	additional_columns(S5,WL,WL2)
	,
	CL = WL2
	.


additional_columns(S,WL,R) :-
	next_word_from_list(S,S2,B)
	,
	B == and
	,
	next_word_from_list(S2,S3,C)
	,
	WL2 = [C|WL]
	,
	additional_columns(S3,WL2,R2)
	,
	R = R2
	
	;
	
	R = WL
	.
	
next_word_from_list([],LR,R) :- LR = [] , R = "".
next_word_from_list([L|Ls],LR,R) :- LR = Ls , R = L.
	
next_word(C,CR,R) :-
	W = []
	,
	string_chars(C,C2)
	,
	next_word(C2,CR2,R2,W)
	,
	string_chars(R,R2)
	,
	string_chars(CR,CR2)
	.

next_word([],CR,R,W) :- R = W , CR = [] , !.
next_word([C|Cs],CR,R,W):-
	char_type(C,white)
	,
	CR = Cs
	,
	R = W
	,
	!
	
	;
	
	append(W ,[C] , W2)
	,
	next_word(Cs,CR2,R2,W2)
	,
	R = R2
	,
	CR = CR2
	.
	
% benchmark(X)
%
% runs the performance_text(X) for all instances
benchmark(X) :- findall(N,performance_test(X),N).

performance_test(X) :-
	X > 0
	,
	between(1,X,N)
	,
	(
		init_winning([1]), write("Test ") , write(N) , write(" done for  1 stack: ") , nl , time(win([N])),win([N]) ,nl,nl
		;
		init_winning([1]), write("Test ") , write(N) , write(" done for  2 stacks: ") , nl , time(win([N,N])),win([N,N]) ,nl,nl
		;
		init_winning([1]), write("Test ") , write(N) , write(" done for 3 stacks: ") , nl , time(win([N,N,N])),win([N,N,N]) ,nl,nl
		;
		init_winning([1]), write("Test ") , write(N) , write(" done for 4 stacks: ") , nl , time(win([N,N,N,N])),win([N,N,N,N]) ,nl,nl
		;
		init_winning([1]), write("Test ") , write(N) , write(" done for 5 stacks: ") , nl , time(win([N,N,N,N,N])),win([N,N,N,N,N]) ,nl,nl
	)
	.
% benchmark_state(S)
%
% runs a benchmark on a specific state
benchmark_state(S) :-
	write("Testing custom state: ") , write(S) , nl
	,
	(time(win(S)) , win(S) ; true) , nl , nl
	.
	

	
% menu([L|Ls],R)
%
% Creates a console menu from the list [L|Ls] - numbering them accordingly.
%
% The will then prompt the user for an input, only accepting a valid number, which it sets to R
%	
menu([L|Ls],R) :-
	cls
	,
	menu_write_line([L|Ls],0,N)
	,
	menu_get_choice(N,C)
	,
	!
	,
	R = C
	.

% menu_write_line([L|Ls],P,R)
%
% iterates through a list of menu numbers until the list is empty, printing them to the console and numbering them accordingly
%
% [L|Ls] = list of menu lines 
%
% P      = menu number iterator (will increment at start, so start from 0 if you want your first line to be 1)
%
% R      = returns the final line number so the predicate calling it knows how big the list is
%
menu_write_line([],P,R) :- R = P.
menu_write_line([L|Ls],P,R) :-
	P2 is P + 1
	,
	write(P2) , write(": ") , write(L) , nl , nl
	,
	menu_write_line(Ls,P2,R2)
	,
	R = R2
	.

	
% menu_get_choice(N,R)
%
% a predicate for returning an option from 1 to N inclusive
%
% N = the maximum number allowed
%
% R = the option selected by the user
%
menu_get_choice(N,R) :-
	write("Choose an option (1 - ") , write(N) , write(")") , nl
	,
	user_get_integer(C)
	,
	(
		C >= 1 , C =< N 		% ensures C is a valid option
		,
		! 						% no backtracking please!
		,
		R = C 					% sets R to C
		
		;						% OR - on fail

		menu_get_choice(N,C2) 	% recurse until a valid number is found
		,
		!
		,
		R = C2
	)
	.
	
user_input(R) :-
	readln(I)
	,
	head(I,R)
	.
	

head([H|_],H).
	
user_input(T,R) :-
	write(T) , nl
	,
	readln(I)
	,
	R = I
	.

% a predicate that just waits for a user to press enter (displays T as text)	
%
% for use in UI, such as displaying some text before continuing
user_press_enter(T) :-
	write(T) , nl
	,
	readln(_)
	,
	!
	,
	true
	;
	!
	,
	true
	.

% recurses until it gets an integer from the user
user_get_integer(N) :-
	readln(X)
	,
	head(X,I)
	,
	integer(I)
	,
	N = I
	
	;
	
	write("Invalid, please enter an Integer") , nl
	,
	user_get_integer(N)
	.

% does user_get_integer/1 but with T being some text to display to the user
user_get_integer(T,N) :-
	write(T) , nl
	,
	user_get_integer(N)
	.

% is a list a valid game state?	
valid_game_state([]).
valid_game_state([M|Ms]) :- integer(M) , M > 0 , valid_game_state(Ms).


% sets M to a valid game state provided by the user as a space separated
input_valid_state(M) :-
	readln(M)
	,
	not(M == [])			% empty game state pointless as game is already over
	,
	length(M,L) , L < 11	% we only deal with games that are 10 or less columns
	,
	valid_game_state(M)
	
	;
	
	write("Invalid Game State, please re-enter!") , nl
	,
	input_valid_state(M)
	.
	
% request a valid integer from the user than is greater than 0 and less than equal to M
% assigns that integer to I
input_valid_integer(I,M) :-
	readln(T)
	,
	head(T,U)
	,
	integer(U)
	,
	U =< M
	,
	U > 0
	,
	I = U
	
	;
	
	write("Invalid integer, please reenter...") , nl
	,
	input_valid_integer(I,M)
	.	


% main_choice/1
%
% set of menu actions for the main menu
%
% these are mostly either text being displayed, or menu choices, or combiniations of those that then call other options within the program
%
main_choice(1) :-
	set_game_complexity(2)
	,
	init_winning
	,
	cls
	,
	write("You will now play a game that follows the rules set out in the coursework specification") , nl , nl
	,
	write("These rules are:") , nl , nl
	,
	write(" * There are several stacks of counters") , nl , nl
	,
	write(" * You may remove any number of counters from a single stack") , nl , nl
	,
	write(" * You may remove the same number of counters from two of the stacks") , nl , nl
	,
	write(" * The game ends when there are no counters left") , nl , nl
	,
	write(" * The last player to remove counters loses the game") , nl , nl
	,
	write("Please enter the starting game state as numbers separated by spaces") ,nl
	,
	write("eg: 3 2 1") , nl ,nl
	,
	input_valid_state(M)
	,
	(
		play(M)
		,
		!
		,
		readln(_)
		,
		fail
		
		;
		
		write("GAME ABORTED") , nl
		,
		!
		,
		readln(_)
		,
		fail
	)
	.

	

main_choice(2) :-
	cls
	,
	write("You will now play a game using a custom complexity.") , nl , nl
	,
	write("These rules are:") , nl , nl
	,
	write(" * There are several stacks of counters") , nl , nl
	,
	write(" * You may remove any number of counters from a single stack") , nl , nl
	,
	write(" * You may remove the same number of counters from up to N of the stacks, where N is the game complexity") , nl , nl
	,
	write(" * The game ends when there are no counters left") , nl , nl
	,
	write(" * The last player to remove counters loses the game") , nl , nl
	,
	write("Please enter the game complexity that you would like to play:") , nl
	,
	user_get_integer(N)
	,
	set_game_complexity(N)
	,
	init_winning
	,
	write("Please enter the starting game state as numbers separated by spaces") ,nl
	,
	write("eg: 3 2 1") , nl ,nl
	,
	input_valid_state(M)
	,
	(
		play(M)
		,
		!
		,
		readln(_)
		,
		fail
		
		;
		
		write("GAME ABORTED") , nl
		,
		!
		,
		readln(_)
		,
		fail
	)
	.
	
main_choice(3) :-
	cls
	,
	init_winning
	,
	write("Analyse Win States") , nl
	,
	write("==================") , nl , nl
	,
	menu(["Default Coursework Analysis","More Advanced Analysis"],C)
	,
	(
		C == 1
		,
		D = 2
		,
		X = 2
		,
		!
		;
		C == 2
		,
		user_get_integer("Enter Number of Stacks",D)
		,
		D > 0
		,
		user_get_integer("Enter Game Complexity (max number of stacks player can take from)",X)
		,
		X > 0
		,
		!
	)
	,
	set_game_complexity(X)
	,
	user_get_integer("Enter number of stones",S)
	,
	analyse_all_for_X_stacks(S,D)
	,
	user_press_enter("Press Enter to return to Main Menu")
	,
	!
	,
	fail
	.


	
main_choice(4) :-
	cls
	,
	write("Benchmark Range Of States") , nl
	,
	write("=========================") , nl , nl
	,
	write("This will run a series of benchmarks for 1 to 5 columns, from stone sizes 1 to the size provided (Max 10)") , nl ,nl
	,
	write("Please enter the size you would like to test for:"), nl ,nl
	,
	input_valid_integer(M,10)
	,
	cls
	,
	write("Running Benchmarks...") , nl
	,
	write("=====================") , nl , nl
	,
	benchmark(M)
	,
	nl , nl
	,
	!
	,
	user_press_enter("Benchmarks Complete - press enter to return")
	,
	fail
	.



main_choice(5) :-
	cls
	,
	write("Benchmark specific state") , nl
	,
	write("========================") , nl , nl
	,
	write("If you provide a state the program will benchmark it and provide the computational data.") , nl ,nl
	,
	write("Please write your state as numbers separated by spaces and press enter"), nl ,nl
	,
	input_valid_state(M)
	,
	cls
	,
	write("Running Benchmark...") , nl
	,
	write("====================") , nl , nl
	,
	write("Testing computational data for state: ") , write(M) , nl , nl
	,
	benchmark_state(M)
	,
	nl , nl
	,
	!
	,
	user_press_enter("Benchmarks Complete - press enter to return")
	,
	fail
	.

	
	
main_choice(6) :-
	cls
	,
	write("Thank you for playing this game!")
	,
	set_game_complexity(2)
	,
	init_winning
	,
	!
	,
	true
	.
	
	
% get_text
%
% some now redundant predicates I wrote when experimenting with getting text data from the user
%	
get_text(R) :-
	get_single_char(C)
	,
	get_text([],C,R2)
	,
	R = R2
	.
	
get_text(L,C,R) :-
	C == '.'
	,
	R = L
	,
	!
	;
	append(L,[C],L2)
	,
	get_single_char(C2)
	,
	get_text(L2,C2,R2)
	,
	R = R2
	.
	
	
% predicates researched

% cls
%
% clears the console to blank
%
% this was found on StackOverflow
% https://stackoverflow.com/questions/16908764/clearing-screen-in-swipl-prolog-in-windows
%
% by users Orbling and CapelliC 
cls :- write('\e[2J').
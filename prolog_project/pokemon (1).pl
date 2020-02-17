% mehmet erdinc oguz 	
% 2017400267
% compiling: yes
% complete: yes

:- [pokemon_data].

%
%
%	FIRST EXPLANATIONS ABOVE THE PREDICATES ARE COPY-PASTED FROM THE PDF FILE SINCE EACH PREDICATE IS FUNCTIONING AS EXPLAINED IN PDF. 
%	HOWEVER, HOW THEY WORK AND MY OWN COMMENTS ARE NEXT TO THE CODELINES.
%
%
%This predicate is to find the evolved version of Pokemon given its level. If there is no evolution, then
%EvolvedPokemon = Pokemon. Pokemon can evolve more than one time if it has enough levels.
%

%find_pokemon_evolution(+,+,-) 
find_pokemon_evolution(PokemonLevel, Pokemon, EvolvedPokemon) :-			
	pokemon_evolution(Pokemon, New_Poke, Req_Lvl),							%% FIND THE REQUIRED LEVEL TO EVOLVE
	( Req_Lvl =< PokemonLevel 												%% IF POKEMON'S LEVEL IS SUFFICIENT, CONTINUE TO CHECK IF THERE'S ANOTHER EVOLUTION
	-> find_pokemon_evolution(PokemonLevel, New_Poke, EvolvedPokemon);	
	EvolvedPokemon = Pokemon).												%% ELSE IT EVOLVED POKEMON WILL BE THE LAST POKEMON THAT IS BEING CHECKED
find_pokemon_evolution(_, Pokemon, EvolvedPokemon) :-
	EvolvedPokemon = Pokemon.												%% BASE CASE %%

%
%This predicate evaluates the statistics of a Pok´emon for the given level. With every level a Pok´emon
%gains 2 health points, 1 attack point and 1 defense point. You can get the base statistics from pokemon stats	
%

%poke_lvl_stats(+,?,-,-,-)	
pokemon_level_stats(PokemonLevel, Pokemon, PokemonHp, PokemonAttack, PokemonDefense) :-
	pokemon_stats(Pokemon,_,Hp,Atk,Dfs),				%% TAKE THE NECESSARY DATA FROM STATS, AND EVALUATE HP, ATK, DFS ACCORDING TO LEVEL, RESPECTIVELY
	PokemonHp is (Hp + (2*PokemonLevel)),
	PokemonAttack is (Atk + (PokemonLevel)),
	PokemonDefense is (Dfs + (PokemonLevel)).

%
%This predicate will be used to find single-type advantage/disadvantage multiplier. It can also be used
%to find types that achieves a given multiplier.
%

%single_type_multiplier(?,+,?)
single_type_multiplier(AttackerType, DefenderType, Multiplier) :-
	pokemon_types(P_types),											%% GET ALL THE TYPES
	find_index(DefenderType,P_types, 0, Index),						%% FIND THE INDEX OF THE DEFENDERS TYPE(see find_index at the bottom)
	type_chart_attack(AttackerType, Type_mult_list),				%% GET THE TYPE CHART LIST ACCORDING TO ATKTYPE TO OBTAIN THE VALUE AT THE INDEX OF THE DEFENDERS TYPE
	nth0(Index, Type_mult_list, X),									%% GET THE VALUE FROM THE INDEX
	Multiplier = X. 												%% DONE

%
%This predicate will be used to find double-type advantage/disadvantage multiplier. It can also be
%used to find types that achieves a given multiplier. DefenderTypeList can only have one item or two
%items.
%

%type_multiplier(?,+,?)
type_multiplier(AttackerType, [H|T], Multiplier) :-
	type_multiplier_helper(AttackerType, [H|T], Multiplier, _, _, 0).		%% HELPER METHOD
%type_multiplier_helper(?,+,?,?,?,+)
type_multiplier_helper(_, [], Multiplier, X, Y, Count) :-		%% BASE CASE	
	(Count == 1 															%% IF ONE TYPE -> Y(STORED), TWO TYPES -> MULTIPLY
	-> Multiplier = Y;
	Multiplier is X*Y ).
type_multiplier_helper(AttackerType, [H|T], Multiplier, X, Y, Count) :-		%% 
	single_type_multiplier(AttackerType, H, X),								%% MAKE THE X MULTIPLIER OF THE ATK TYPE
	incr(Count, Count2),													%% COUNT IS THE AMOUNT OF TYPES 
	type_multiplier_helper(AttackerType, T, Multiplier, Y, X, Count2).		%% SWAP X WITH Y, POKEMON MAY HAVE ANOTHER TYPE WE WILL KEEP FIRST ONE AT Y

%
%This predicate will be used to find type multiplier between two Pok´emon. It can also be used to find
%different attacker/defender Pok´emon that achieves a given multiplier. If an attacker Pok´emon has two
%types, then the Pok´emon uses the type that gives the higher multiplier against the defender Pok´emon.
%

%pokemon_type_multiplier(?, ?, ?)
pokemon_type_multiplier(AttackerPokemon, DefenderPokemon, Multiplier) :-	
	pokemon_stats(AttackerPokemon, AtkTypes, _, _, _),				%% LETS GET THE STATS OF THE POKEMONS
	pokemon_stats(DefenderPokemon, DefTypes, _, _, _),
	length(AtkTypes, L),													%% FIND HOW MANY ATKTYPES
	nth0(0, AtkTypes, T1),													%% GET THE FIRST TYPE
	type_multiplier(T1, DefTypes, M1),										%% CALCULATE FIRST
	(L == 2 																%% IF THERE IS ANOTHER TYPE
	-> nth0(1, AtkTypes, T2),												%% GET THE SECOND TYPE
	type_multiplier(T2, DefTypes, M2),										%% CALCULATE SECOND
	(M1 > M2 -> Multiplier = M1; Multiplier = M2);							%% COMPARE AND GET THE BIGGER MULTIPLIER
	Multiplier = M1).

%
%This predicate finds the damage dealt from the attack of the AttackerPokemon to the DefenderPokemon.
%

%pokemon_attack(+,+,+,+,-)
pokemon_attack(AttackerPokemon, AttackerPokemonLevel, DefenderPokemon, DefenderPokemonLevel, Damage) :-
	pokemon_level_stats(AttackerPokemonLevel, AttackerPokemon, _, ATK1, _),		%% GET THE NEEDED STATS OF THE POKEMONS
	pokemon_level_stats(DefenderPokemonLevel, DefenderPokemon, _, _, DEF2),
	pokemon_type_multiplier(AttackerPokemon, DefenderPokemon, M),				%% GET THE MULTIPLIER BETWEEN POKEMONS
	Damage is ((0.5 * AttackerPokemonLevel * (ATK1 / DEF2) * M) + 1).			%% CALCULATE

%
%This predicate simulates a fight between two Pok´emon then finds health points of each Pok´emon at
%the end of the fight and the number of rounds. Each Pok´emon attacks at the same time and each
%attack sequence count as one round. After each attack, health points of each Pok´emon reduced by
%the amount of calculated damage points. When a Pok´emon’s health points reach or drop below zero
%(HP <= 0), the fight ends.
%

%pokemon_fight(+,+,+,+,-,-,-)
pokemon_fight(Pokemon1, Pokemon1Level, Pokemon2, Pokemon2Level, Pokemon1Hp, Pokemon2Hp, Rounds) :-
	pokemon_level_stats(Pokemon1Level, Pokemon1, HP1, _, _),													%% GET THE NECESSARY STATS(HPS TO CALC REM HPS)
	pokemon_level_stats(Pokemon2Level, Pokemon2, HP2, _, _),
	pokemon_fight_helper(Pokemon1, Pokemon1Level, Pokemon2, Pokemon2Level, Pokemon1Hp, Pokemon2Hp, HP1, HP2, 0, Rounds).
%pokemon_fight_helper(+,+,+,+,-,-,+,+,+,-)
pokemon_fight_helper(Pokemon1, Pokemon1Level, Pokemon2, Pokemon2Level, Pokemon1Hp, Pokemon2Hp, Temp1Hp, Temp2Hp, Count, R) :-
	pokemon_attack(Pokemon1, Pokemon1Level, Pokemon2, Pokemon2Level, DMG1),		%% CALCULATE DMGS
	pokemon_attack(Pokemon2, Pokemon2Level, Pokemon1, Pokemon1Level, DMG2),
	incr(Count, Count2),														%% COUNT IS AMOUNT OF ROUNDS BUT TEMPORARILY, INCREASES WITH EVERY RECURSION
	P1_hp_left is (Temp1Hp - DMG2),												%% LEFT HPS
	P2_hp_left is (Temp2Hp - DMG1),
	( P1_hp_left > 0
		-> ( P2_hp_left > 0			%% IF BOTH OF THEM ARE STILL ALIVE, KEEP FIGHTING TILL DEATH
			->	pokemon_fight_helper(Pokemon1, Pokemon1Level, Pokemon2, Pokemon2Level, Pokemon1Hp, Pokemon2Hp, P1_hp_left, P2_hp_left, Count2, R)
			;	R = Count2,			%% ELSE, INIT THE ROUND AND LEFT HPS
				Pokemon1Hp = P1_hp_left,
				Pokemon2Hp = P2_hp_left )
		;	R = Count2,
		    Pokemon1Hp = P1_hp_left,
	    	Pokemon2Hp = P2_hp_left).

%
%This predicate simulates a tournament between two Pok´emon trainers then finds the winner Pok´emon
%trainer of each fight. Pok´emon trainers must have the same number of Pok´emon. Pok´emon fights
%in order. First Pok´emon of the first Pok´emon trainer fights with the first Pok´emon of the second
%Pok´emon trainer, second Pok´emon of the first Pok´emon trainer fights with the second Pok´emon of
%the second Pok´emon trainer. A fight ends when a Pok´emon’s health points drop below zero. At the
%end of the fight, Pok´emon with more health points win the fight, so does the Pok´emon trainer that
%owns the winner Pok´emon. In case of a tie, PokemonTrainer1 wins. Important Note: Pok´emon
%trainers force their Pok´emon to evolve (if possible) before tournament fights to gain maximum efficiency. So you should check evolution status of each Pok´emon.
%

%pokemon_tournament(+,+,-) %-- SOMETIMES SEMICOLON GIVES DIFF OUTPUTS --%
pokemon_tournament(PokemonTrainer1, PokemonTrainer2, WinnerTrainerList) :-
	pokemon_trainer(PokemonTrainer1, Team1, Levels1),				%% GET POKEMONS OF TRAINERS, ALSO LEVELS
	pokemon_trainer(PokemonTrainer2, Team2, Levels2),
	tournament_maker(PokemonTrainer1, PokemonTrainer2, Team1, Team2, Levels1, Levels2, WinnerTrainerList). 	%% LETS FIGHT
%tournament_maker(+,+,+,+,+,+,-)
tournament_maker(_, _, [], [], [], [], []). 
	%% BASE CASE %%
tournament_maker(PokemonTrainer1, PokemonTrainer2, [P1|T1], [P2|T2], [L1|V1], [L2|V2], [B|T]) :-
	find_pokemon_evolution(L1,P1,Pokemon1),															%% CHECK IF ANY EVOLUTION CAN BE DONE FO BOTH POKEMONS
	find_pokemon_evolution(L2,P2,Pokemon2),
	pokemon_level_stats(L1, Pokemon1, _,_,_),														%% GET THE STATS OF EACH POKEMON
	pokemon_level_stats(L2, Pokemon2, _,_,_),
	pokemon_fight(Pokemon1, L1, Pokemon2, L2, HP1, HP2,_),											%% MAKE THEM FIGHT
	(HP1 < HP2															%% CHECK WHO WINS
	-> B = PokemonTrainer2,												%% PUT THE WINNER TO THE WinnerTrainerList AND CONTINUE THE TOURNAMENT UNTIL EVERYONE FIGHTS
	   tournament_maker(PokemonTrainer1,PokemonTrainer2,T1,T2,V1,V2,T)
	;  B = PokemonTrainer1,
	   tournament_maker(PokemonTrainer1,PokemonTrainer2,T1,T2,V1,V2,T)
	).

%
%This predicate finds the best Pok´emon against the given EnemyPokemon where the both Pok´emon’s
%levels are LevelCap. Do not evolve Pokemon. We define the best Pok´emon as the Pok´emon with the
%most remaining health points after the fight.
%

%best_pokemon(+,+,-,-)
best_pokemon(EnemyPokemon, LevelCap, RemainingHP, BestPokemon) :-
	findall(Pokemon, pokemon_stats(Pokemon, _, _, _, _), PokemonList),									%% GET ALL POKEMONS
	best_pokemon_helper(EnemyPokemon, LevelCap, PokemonList, -9999999, none, RemainingHP, BestPokemon).	%% START FINDING BEST FROM THE POKEMONLIST(-999999->low enough to be lowest)
%best_pokemon_helper(+,+,+,+,+,-,-)																		%% none->if no pokemon is found
best_pokemon_helper(_, _, [], TempHP, TempPoke, RemainingHP, BestPokemon) :-			%% BASE CASE
	BestPokemon = TempPoke,
	RemainingHP = TempHP.
best_pokemon_helper(EnemyPokemon, LevelCap, [P|T], TempHP, TempPoke, RemainingHP, BestPokemon) :-
	pokemon_fight(P, LevelCap, EnemyPokemon, LevelCap, Rem, _, _),									%% GET P FROM POKEMONS, MAKE THEM FIGHT
	(Rem > TempHP 																						%% IF THE REM HP FROM FIGHT IS BIGGER THAN PREVIOUS REM, NEW BEST POKEMON IS CURRENT POKEMON FROM THE LIST
	->	best_pokemon_helper(EnemyPokemon, LevelCap, T, Rem, P, RemainingHP, BestPokemon)
	;	best_pokemon_helper(EnemyPokemon, LevelCap, T, TempHP, TempPoke, RemainingHP, BestPokemon)).	%% ELSE KEEP CHECKING WITH THE OLDEST POKEMON SITTING ON THE THRONE 

best_pokemon_evolve(EnemyPokemon, LevelCap, RemainingHP, BestPokemon) :-								%% SAME HELPER FOR THE BEST POKEMON TEAM PREDICATE TO FIND THE BEST POKEMON
	findall(Pokemon, pokemon_stats(Pokemon, _, _, _, _), PokemonList),									%% WITH EVOLUTION BEFORE THE FIGHT
	best_pokemon_evolve_helper(EnemyPokemon, LevelCap, PokemonList, -9999999, none, RemainingHP, BestPokemon).
best_pokemon_evolve_helper(_, _, [], TempHP, TempPoke, RemainingHP, BestPokemon) :-
	BestPokemon = TempPoke,
	RemainingHP = TempHP.
best_pokemon_evolve_helper(EnemyPokemon, LevelCap, [P|T], TempHP, TempPoke, RemainingHP, BestPokemon) :-
	find_pokemon_evolution(LevelCap,P,EP),
	pokemon_fight(EP, LevelCap, EnemyPokemon, LevelCap, Rem, _, _),
	(Rem > TempHP 
	->	best_pokemon_evolve_helper(EnemyPokemon, LevelCap, T, Rem, EP, RemainingHP, BestPokemon)
	;	best_pokemon_evolve_helper(EnemyPokemon, LevelCap, T, TempHP, TempPoke, RemainingHP, BestPokemon)).

%
%This predicate finds the best Pok´emon Team against the given OpponentTrainer where the levels of
%each Pok´emon of our best Pok´emon Team are the same with the corresponding Opponent’s Pok´emon
%levels (e.g. Level of the first Pok´emon of the best Pok´emon Team is same with the level of the first
%Pok´emon of the Opponent Trainer). Both Pokemon should be evolved according to their level before
%fighting like in the tournament. We define the best Pok´emon as the Pok´emon with the most remaining
%health points after the fight.
%

%best_pokemon_team(+,-)
best_pokemon_team(OpponentTrainer, PokemonTeam) :-
	pokemon_trainer(OpponentTrainer, PokeTeam, Levels),					%% GET THE TRAINERS TEAM
	best_pokemon_team_helper(PokeTeam, Levels, PokemonTeam).			%% USE THE HELPER
%best_pokemon_team_helper(+,+,-)
best_pokemon_team_helper([], [], []).
	%% BASE CASE %%
best_pokemon_team_helper([P|T1], [L|T2], [Poke|T]) :-
	find_pokemon_evolution(L, P, EvolvedP),				%% FIND THE EVOLVED VERSION OF THE TRAINERS POKEMON
	best_pokemon_evolve(EvolvedP, L, _, Poke),			%% FIND THE BEST POKEMON WITH CURRENT LEVEL IN THE EVOLVED VERSION
	best_pokemon_team_helper(T1, T2, T).				%% CONTINUE TO CHECK OTHER POKEMONS

%
%This predicate will be used to find every Pok´emon from InitialPokemonList that are at least one of
%the types from TypeList.
%

%pokemon_types(+,+,-)
pokemon_types(TypeList, InitialPokemonList, PokemonList) :-
	findall(Pokemon, 
		   (member(Pokemon, InitialPokemonList), pokemon_types_helper(TypeList, Pokemon)), 
		    PokemonList).
	
pokemon_types_helper([H|Tail], Pokemon) :-
	pokemon_stats(Pokemon, PokemonTypeList, _, _, _),
	((member(H, PokemonTypeList), !); pokemon_types_helper(Tail, Pokemon)).

%
%This predicate generates a Pok´emon team based on liked and disliked types and some criteria. This
%team can only have Pok´emon from LikedTypes and can’t have Pok´emon from DislikedTypes. The
%predicate sorts Pok´emon according to one of the three criterion in descending order: health points
%(h), attack (a), defense (d). Then selects Count number of Pok´emon that have highest values in the
%selected criterion. If two or more Pok´emon has the same value, the order is not important between
%these Pok´emon.
%

%generate_pokemon_team(+,+,+,+,-) 																							
generate_pokemon_team(LikedTypes, DislikedTypes, Criterion, Count, PokemonTeam) :-		
	findall(Pokemon, pokemon_stats(Pokemon,_,_,_,_), PokemonList),							%% GET ALL POKEMONS TO POKEMONLIST
	pokemon_types(LikedTypes, PokemonList, LikedPokemons),									%% GET THE POKEMONS WITH LIKED AND DISLIKED POKEMONS
	pokemon_types(DislikedTypes, PokemonList, DislikedPokemons),							
	subtract(LikedPokemons, DislikedPokemons, FinalList),									%% SUBTRACT THE DISLIKED POKEMONS FROM THE LIKED POKEMONS
	(Criterion = 'h'																		%% SORT THE FINAL LIST OF POKEMONS ACCORDING TO CRITERION
	-> merge_with_hp(FinalList, MergedList),					%% CREATE VALUES WHICH CAN BE COMPARED
	   sort_by_hp(MergedList, SortedFinalList)					%% SORT BY THE KEY OF THE EACH MEMBER OF THE LIST
	;  (Criterion = 'a'
		-> merge_with_atk(FinalList, MergedList),
		   sort_by_atk(MergedList, SortedFinalList)
	    ;  merge_with_def(FinalList, MergedList),
	       sort_by_def(MergedList, SortedFinalList)
		)	
	),
	generate_pokemon_team_helper(SortedFinalList, PokemonTeam, Count, 0).
%generate_pokemon_team_helper(+,-,+,+)								
generate_pokemon_team_helper(_, [], Count, Count).
	%% BASE CASE %%
generate_pokemon_team_helper([P|T1], [H|T], Count, TempCount) :-	%% LOOP PREDICATE THAT EXECUTES UNTIL COUNT EQUALS TO TEMPCOUNT
	pokemon_stats(P,_,HP,ATK,DEF),									%% PUT THE POKEMONS WITH STATS TO FINAL LIST
	H = [P,HP,ATK,DEF],												%% CREATE A LIST THAT INCLUDE POKEMON AND ITS STATS
	incr(TempCount, Count2),										
	generate_pokemon_team_helper(T1,T,Count,Count2).

%
%
%	MERGE PREDICATES MERGE THE POKEMONS NAME WITH THE REQUESTED CRITERION VALUE TO USE THE BUILTIN SORT ACCORDING TO THAT CRITERION  
%
%

merge_with_hp([], []).
	%% BASE CASE %%
merge_with_hp([H|T], [(HP-H)|TempList]) :-		%% GET HP AND PUT THE PAIR TO RESULTING LIST
	pokemon_stats(H,_,HP,_,_),
	merge_with_hp(T, TempList).
merge_with_atk([], []).
	%% BASE CASE %%
merge_with_atk([H|T], [(ATK-H)|TempList]) :-	%% GET ATK AND PUT THE PAIR TO RESULTING LIST
	pokemon_stats(H,_,_,ATK,_),
	merge_with_atk(T, TempList).
merge_with_def([], []).
	%% BASE CASE %%
merge_with_def([H|T], [(DEF-H)|TempList]) :-	%% GET DEF AND PUT THE PAIR TO RESULTING LIST
	pokemon_stats(H,_,_,_,DEF),
	merge_with_def(T, TempList).
%
%
%	SORTING ACCORDING TO THE STANDARD PROCEDURE. FOR THIS PROJECT, OUR LISTS WILL INCLUDE PAIRS MERGED WITH THE REQUESTED CRITERION AND WILL BE SORTED ACCORDINGLY
%	AND THEN, THIS PAIRS WILL BE SEPERATED TO CREATE THE FINAL LIST
%
%
sort_by_hp(List, SortedList) :-					
	sort(0, @>, List, SortedPairs),			%% SORT IN DESCENDING ORDER
	pairs_values(SortedPairs, SortedList).	%% SEPERATE
sort_by_atk(List, SortedList) :-
	sort(0, @>, List, SortedPairs),
	pairs_values(SortedPairs, SortedList).
sort_by_def(List, SortedList) :-
	sort(0, @>, List, SortedPairs),
	pairs_values(SortedPairs, SortedList).

find_index(H, [H|_], Res, Index) :- 
	Index = Res.
find_index(H, [_|T], Res, Index) :-
	incr(Res, Res1),
	find_index(H, T, Res1, Index).
incr(X, X1) :-
	X1 is X+1.
decr(X,X1) :-
	X1 is X-1.



	




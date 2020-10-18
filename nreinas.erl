%% "♕"
%% "■"
%% "□"
-module(nreinas).
-export([geneticosNReinas/1,cruce/3, rellenar/2]).

	%% Otras funciones

elementAt([], _N)->ok;
elementAt([H|_T], 0)->H;
elementAt([_H|T], N)->elementAt(T,N-1).

shuffle([])->[];
shuffle(Lista)->[A||{_B,A} <- lists:sort([{rand:uniform(), V} || V<-Lista])].

	%% Fitness

fitness(P) -> lists:sort(fun({_,A},{_,B}) -> A < B end, [{X, evaluate(X)} || X<-P]).

	%% Nuevo cruzamiento
		%% Nueva generación

crossOver([{E,F}|T], N) ->  mutation([E] ++ [crossOverAux([{E,F}|T], N) || _X<-lists:seq(0, N*N - 2)],N).

crossOverAux(Apt, N) -> A = selection(Apt, rand:uniform(N) - 1),
			%cruce(element(1,lists:split(N div 2, element(1,A))), element(1,selection(Apt -- [A], rand:uniform(N) - 1)), rand:uniform(N-1)).
			cruce(element(1,lists:split(N div 2,element(1,A))), element(1,selection(Apt -- [A], rand:uniform(N) - 1)),N).
%			cruce(element(1,lists:split(N div 2,element(1,selection(Apt, rand:uniform(N) - 1)))), element(1,selection(Apt, 	rand:uniform(N) - 1)),N).
			%cruceA(element(1,lists:split(N div 2,element(1,selection(Apt, rand:uniform(N) - 1)))), element(1,selection(Apt, 	rand:uniform(N) - 1)),N).
			%cruce(selection(Apt, rand:uniform(N) - 1),selection(Apt, rand:uniform(N) - 1),N).

%CRUCE ++ (lists:seq(0,N-1) -- CRUCE) #################VA GANANDO
%cruce(I1,I2,N) -> A = sets:to_list(sets:from_list(I2)),%%%%%%%%%
%		C = I1 ++ [X || X<-(A -- I1)],%%%%%%%%%%%%%
%		C ++ (lists:seq(0,N-1) -- C).%%%%%%%%%%%%
		%QUITAR LO DE ABAJO
		%B ++ rellenar(B,N).




cruce(I1, I2, N) -> 	A = I1 ++ [X || X<-I2 ,not lists:member(X, I1)], 
			element(1, lists:split(N, A ++ rellenar(A, N))).
rellenar(I, N) when length(I) < N ->  lists:seq(0,N-1) -- I;
rellenar(_,_)->[].
%% Prueba 1
%cruce(I1,I2,S)-> element(1, lists:split(S,I1)) ++ element(2,lists:split(S,I2)).


selection([{I,F}|_T], Fl) when F >= Fl -> {I,F};
selection([{I,F}|[]], _Fl) -> {I,F}; % {[4,1,3,5,7,6,0,2],1}
selection([_H|T], Fl) -> selection(T, Fl).

replaceAt([], _E, _N) -> [];
replaceAt([_H|T], E, 0) -> [E] ++ T;
replaceAt([H|T], E, N) -> [H] ++ replaceAt(T,E, N-1).

mutation(P, N) -> mutation(P, N, round(N*N * 0.05)).

mutation(P, _N, 0) -> P;
mutation(P, N, PC) -> A1 = rand:uniform(N*N) - 1,
			mutation(replaceAt(P, replaceAt(elementAt(P, A1), rand:uniform(N) - 1, rand:uniform(N) - 1), A1), N, PC - 1).



%% Función de evaluación
	%% ¿Quién es más apto?
		%% Dominio: Un individuo I (arreglo con las columnas en las que
	        %% se encuentran las n reinas)
		%% Codominio: Un número natural menor que N.
evaluateIdx(J, J, _Vj, _Vi, _I) -> 0;
evaluateIdx(_J, _Idx, Vj, Vj, _I) -> 1;
evaluateIdx(J, Idx, Vj, Vi, _I) when abs(J - Idx) == abs(Vj - Vi), Idx /= J -> 1;
evaluateIdx(J, Idx, _Vj, Vi, I) -> evaluateIdx(J+1, Idx, elementAt(I,J+1), Vi, I).

evaluateAux(Coll) -> lists:foldl(fun(A, SUM) -> A + SUM end, 0, Coll). 

evaluate([])->0;
evaluate(I)->evaluateAux([evaluateIdx(0,X, elementAt(I, 0),elementAt(I, X), I) || X<-lists:seq(0, length(I) - 1)]).


	%% Imprimir tablero
	
printI(I,N) -> printI(I,N,0,0).

printI(_In, N, N, _J) -> io:format("~n", []);
printI(In, N, I, N) -> io:format("~n", []), printI(In, N, I + 1, 0);
printI(In, N, I, J) -> printIAux(elementAt(In, J),I,J), printI(In, N, I, J + 1).

printIAux(E, E, _J) ->  io:format("~ts ", [unicode:characters_to_binary("♕")]);
printIAux(_E, I, J) when (I + J) rem 2 == 0 -> io:format("~ts ", [unicode:characters_to_binary("■")]);
printIAux(_E, _I, _J) -> io:format("~ts ", [unicode:characters_to_binary("□")]).

	%% Población

cruceA(I1,I2,N) -> element(1,lists:split(N, I1 ++ [X || X<-(I2--I1)])).

	%% Población Inicial
% Esta función recibe un número natural y retorna una lista de individuos (listas de tableros)
poblacion(0)->[];
poblacion(N)->[shuffle(lists:seq(0,N - 1)) || _X<-lists:seq(1,N*N)].

%% Otras pruebas que no sé si son "ilegalísimas"
geneticosNReinas(N) -> geneticosNReinas(N, fitness(poblacion(N)), 0).

printaux([H|T]) -> io:format("[~p,",[H]), printauxx(T).
printauxx([H|[]]) -> io:format("~p]~n",[H]);
printauxx([H|T]) -> io:format("~p,",[H]), printauxx(T).

geneticosNReinas(N, [{E,0}|_T], G) ->
						io:format("~p~n",["--- Solución --- Generación: " ++ integer_to_list(G)]),
						printI(E, N),
						printaux(E);
geneticosNReinas(N, [{E,F}|T], G) ->
					io:format("~p~n",["--- Élite --- Generación: " ++ integer_to_list(G)]),
					io:format("~p~n",["    Aptitud: " ++ integer_to_list(F)]),
					io:format("~p~n",["Longitud: " ++ integer_to_list(length(E))
					]),
					printI(E,N),
					printaux(E),
					geneticosNReinas(N, fitness(crossOver([{E,F}|T], N)), G + 1).

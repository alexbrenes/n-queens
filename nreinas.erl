%% "♕"
%% "■"
%% "□"
-module(nreinas).
-export([
 poblacion/1, shuffle/1,evaluate/1,aptitudes/1,listaColl/1,indexOf/2,elementAt/2,nextGen/3]).
-export([printI/2, fitness/1,crossOver/2,mutation/3,geneticosNReinas/1,cruce/3,cruceA/3,replaceAt/3]).

	%% Otras funciones

elementAt([], _N)->ok;
elementAt([H|_T], 0)->H;
elementAt([_H|T], N)->elementAt(T,N-1).

indexOfAux(_E,[], _Idx)->-1;
indexOfAux(E, [E|_T], Idx)->Idx;
indexOfAux(E, [_H|T], Idx)-> indexOfAux(E, T, Idx + 1).
indexOf(E,L) -> indexOfAux(E,L,0).

shuffle([])->[];
shuffle(Lista)->[A||{_B,A} <- lists:sort([{rand:uniform(), V} || V<-Lista])].

	%% Fitness

fitness(P) -> lists:sort(fun({_,A},{_,B}) -> A < B end, [{X, evaluate(X)} || X<-P]).

	%% Nuevo cruzamiento
		%% Nueva generación



crossOver([{E,F}|T], N) ->  mutation([E] ++ [crossOverAux([{E,F}|T], N) || _X<-lists:seq(0, N*4 - 2)],N).

crossOverAux(Apt, N) -> A = selection(Apt, rand:uniform(N) - 1),
%			cruce(A, selection(Apt -- [A], rand:uniform(N) - 1), rand:uniform(N-1)).
			cruceA(element(1,lists:split(N div 2,A)), selection(Apt -- [A], rand:uniform(N) - 1),N).
%% Prueba 1
cruce(I1,I2,S)-> element(1, lists:split(S,I1)) ++ element(2,lists:split(S,I2)).
%% Prueba 2
%cruce(I1,I2,S)-> element(1, lists:split(S,I1)) ++ (I2 -- element(1, lists:split(S,I1))).



selection([{I,F}|_T], Fl) when F >= Fl -> I;
selection([{I,_F}|[]], _Fl) -> I; % {[4,1,3,5,7,6,0,2],1}
selection([_H|T], Fl) -> selection(T, Fl).

replaceAt([], _E, _N) -> [];
replaceAt([_H|T], E, 0) -> [E] ++ T;
replaceAt([H|T], E, N) -> [H] ++ replaceAt(T,E, N-1).

mutation(P, N) -> mutation(P, N, round(N*4 * 0.05)).

mutation(P, _N, 0) -> P;
mutation(P, N, PC) -> A1 = rand:uniform(N*4) - 1,
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


%% Calcular las aptitudes de todos los individuos de la pooblación P
aptitudes(P)->[evaluate(X) || X<-P].
%%%%%%%%
listaColl(I)->[evaluateIdx(0,X, elementAt(I, 0),elementAt(I, X), I) || X<-lists:seq(0, length(I) - 1)].
%%%%%%%%

	%% Imprimir tablero
	
printI(I,N) -> printI(I,N,0,0).

printI(_In, N, N, _J) -> io:format("~n", []);
printI(In, N, I, N) -> io:format("~n", []), printI(In, N, I + 1, 0);
printI(In, N, I, J) -> printIAux(elementAt(In, J),I,J), printI(In, N, I, J + 1).

printIAux(E, E, _J) ->  io:format("~ts ", [unicode:characters_to_binary("♕")]);
printIAux(_E, I, J) when (I + J) rem 2 == 0 -> io:format("~ts ", [unicode:characters_to_binary("■")]);
printIAux(_E, _I, _J) -> io:format("~ts ", [unicode:characters_to_binary("□")]).
%printI([H|T], N, H, K) -> io:format("~ts ", [unicode:characters_to_binary("♕")]),
%			   printI(T, N, H, K + 1);
%printI(_I, N, N, _K) -> io:format("~n", []);
%printI(I, N, J, N) ->  io:format("~n", []),
%			printI(I,N,J+1,0);
%printI(I, N, J, K) when (J + K) rem 2 == 0 -> io:format("~ts ", [unicode:characters_to_binary("■")]),
%			printI(I, N, J, K + 1);
%printI(I, N, J, K) -> io:format("~ts ", [unicode:characters_to_binary("□")]),
%			printI(I, N, J, K + 1).

	%% Población

cruceA(I1,I2,N) -> element(1,lists:split(N, I1 ++ [X || X<-(I2--I1)])).

	%% Siguiente generación
nextGen(P, E, N)->
		[E] ++ [cruceA(element(2,lists:split(N div 2, elementAt(P, rand:uniform(N*4) - 1))), elementAt(P, rand:uniform(N*4) - 1),N) 
		|| _X<-lists:seq(0, N*4 - 2)].

	%% Población Inicial
% Esta función recibe un número natural y retorna una lista de individuos (listas de tableros)
poblacion(0)->[];
poblacion(N)->[shuffle(lists:seq(0,N - 1)) || _X<-lists:seq(1,N*4)].

	%% Elitismo

%% Obtener el individuo élite de la población P
%%getEliteAux(P, ApEl, Idx) -> {elementAt(P, Idx), ApEl, Idx}.
%%getElite(P,Aptitudes,ApEl) -> getEliteAux(P, ApEl, indexOf(ApEl, Aptitudes)).
%%findElite(P, Aptitudes) -> getElite(P, Aptitudes, lists:foldl(fun(B, A) -> min(A, B) end, 100000000000, Aptitudes)).

	%% Calcular diferentes generaciones
%generacionAux(_N, _P, _Aptitudes, {E, 0, _Idx})->E;
%generacionAux(_N, _P, _Aptitudes, E) -> E. % Solución
%generacion(N, P, Aptitudes)->generacionAux(N, P, Aptitudes, findElite(P, Aptitudes)).

	%% Función inicial
%%geneticosNReinasAux(N, P) -> generacion(N, P, aptitudes(P)).
%%geneticosNReinas(N) -> geneticosNReinasAux(N, poblacion(N)).



%% Otras pruebas que no sé si son "ilegalísimas"

geneticosNReinas(N) -> geneticosNReinas(N, fitness(poblacion(N)), 0).

printaux([H|T]) -> io:format("[~p,",[H]), printauxx(T).
printauxx([H|[]]) -> io:format("~p]~n",[H]);
printauxx([H|T]) -> io:format("~p,",[H]), printauxx(T).
%%solucion(E)->

geneticosNReinas(N, [{E,0}|_T], G) ->
						io:format("~p~n",["--- Solución --- Generación: " ++ integer_to_list(G)]),
						printI(E, N),
						printaux(E);
geneticosNReinas(N, [{E,F}|T], G) ->
					io:format("~p~n",["--- Élite --- Generación: " ++ integer_to_list(G)]),
					io:format("~p~n",["    Aptitud: " ++ integer_to_list(F)]),
					printI(E,N),
					printaux(E),
					geneticosNReinas(N, fitness(crossOver([{E,F}|T], N)), G + 1).

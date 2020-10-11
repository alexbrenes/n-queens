%% "♕"
%% "■"
%% "□"
-module(nreinas).
-export([
 poblacion/1, shuffle/1,evaluate/1,aptitudes/1,listaColl/1,indexOf/2,elementAt/2,nextGen/3]).
-export([printI/2, fitness/1,crossOver/2]).

	%% Otras funciones

elementAt([], _N)->ok;
elementAt([H|_T], 0)->H;
elementAt([_H|T], N)->elementAt(T,N-1).

indexOfAux(_E,[], _Idx)->-1;
indexOfAux(E, [E|_T], Idx)->Idx;
indexOfAux(E, [_H|T], Idx)-> indexOfAux(E, T, Idx + 1).
indexOf(E,L) -> indexOfAux(E,L,0).

shuffle([])->[];
shuffle(Lista)->[A||{_B,A} <- lists:sort([ {rand:uniform(), V} || V<-Lista])].

	%% Fitness

fitness(P) -> lists:sort(fun({_,A},{_,B}) -> A < B end, [{X, evaluate(X)} || X<-P]).

	%% Nuevo cruzamiento
		%% Nueva generación



crossOver([{E,F}|T], N) -> [E] ++ [crossOverAux([{E,F}|T], N) || _X<-lists:seq(0, N*4 - 2)].

crossOverAux(Apt, N) -> A = selection(Apt, rand:uniform(N) - 1),
			cruce(A, selection(Apt -- [A], rand:uniform(N) - 1), rand:uniform(N-1)).

cruce(I1,I2,S)-> element(1, lists:split(S,I1)) ++ element(2,lists:split(S,I2)).

selection([{I,F}|T], Fl) when F >= Fl -> I;
selection([{I,_F}|[]], _Fl) -> I; % {[4,1,3,5,7,6,0,2],1}
selection([H|T], Fl) -> selection(T, Fl).

mutation(P, N) -> mutation(P, round(N * 0.05)).
mutation(P, PC)

%% Función de evaluación
	%% ¿Quién es más apto?
		%% Dominio: Un individuo I (arreglo con las columnas en las que
	        %% se encuentran las n reinas)
		%% Codominio: Un número natural menor que N.
evaluateIdx(J, J, _Vj, _Vi, _I) -> 0;
evaluateIdx(_J, _Idx, Vj, Vj, _I) -> 1;
evaluateIdx(J, Idx, Vj, Vi, _I) when abs(J - Idx) == abs(Vj - Vi) -> 1;
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
	
printI(I,N) -> printI(lists:reverse(I),N,0,0).
printI([H|T], N, J, H) -> io:format("~ts ", [unicode:characters_to_binary("♕")]),
			   printI([H|T], N, J, H + 1);
printI(_I, N, N, _K) -> io:format("~n", []);
printI([_H|T], N, J, N) ->  io:format("~n", []),
			printI(T,N,J+1,0);
printI(I, N, J, K) when (J + K) rem 2 == 0 -> io:format("~ts ", [unicode:characters_to_binary("■")]),
			printI(I, N, J, K + 1);
printI(I, N, J, K) -> io:format("~ts ", [unicode:characters_to_binary("□")]),
			printI(I, N, J, K + 1).

	%% Población

cruce(I1,I2) -> I1 ++ [X || X<-I2--I1].

	%% Siguiente generación
nextGen(P, E, N)->
		[E] ++ [cruce(element(2,lists:split(N div 2, elementAt(P, rand:uniform(N*4) - 1))), elementAt(P, rand:uniform(N*4) - 1)) 
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

%geneticosNReinas(N) -> P = poblacion(N),
%			Aptitudes = aptitudes(P),
%			E = findElite(P, Aptitudes),
%			geneticosNReinas(N,P,Aptitudes,E).

%%solucion(E)->

%geneticosNReinas(N, _P, _Aptitudes, {E,0,_}) ->
%						io:format("~p~n",["Solución:"]),
%						printI(E, N);
%geneticosNReinas(N, P, _B, {E,_,_}) ->
%					io:format("~p~n",["Elite:"]),
%					printI(E,N),
%					Pn = nextGen(P, E, N),
%					Apt = aptitudes(Pn),
%					El = findElite(Pn, Apt),
%					geneticosNReinas(N, P, Apt, El).

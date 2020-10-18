%% "♕"
%% "■"
%% "□"

-module(proyecto).
-export([geneticosNReinas/1]).

	%% Otras funciones
%% Dominio: Una lista y un número natural N
%% Codominio: El elemento en la posición N-ésima de la lista

elementAt([], _N)->ok;
elementAt([H|_T], 0)->H;
elementAt([_H|T], N)->elementAt(T,N-1).

%% Dominio: Una lista de elementos.
%% Codominio: La lista pero "ordenada" de manera aleatoria

shuffle([])->[];
shuffle(Lista)->[A||{_B,A} <- lists:sort([{rand:uniform(), V} || V<-Lista])].

	%% Fitness

%% Dominio: Una lista de individuos (listas con número naturales entre 0 y N - 1)
%% Codominio: Una lista de tuplas de la forma {A,B} donde B es el individuo y B es su aptitud.

fitness(P) -> lists:sort(fun({_,A},{_,B}) -> A < B end, [{X, evaluate(X)} || X<-P]).

	%% Nuevo cruzamiento
		%% Nueva generación

%% Dominio: Una lista de tuplas {A,B} y un número natural N
%% Codominio: Una nueva lista de individuos generados a partir del cruce y selección aleatoria de otros dos individuos padres 

crossOver([{E,F}|T], N) ->  mutation([E] ++ [crossOverAux([{E,F}|T], N) || _X<-lists:seq(0, N*N - 2)],N).

%% Dominio: Una lista de tuplas {A,B} y un número natural N
%% Codominio: Una nueva lista de individuos generados a partir del cruce y selección aleatoria de otros dos individuos padres

crossOverAux(Apt, N) -> A = selection(Apt, rand:uniform(N) - 1),
			cruce(element(1,lists:split(N div 2,element(1,A))), element(1,selection(Apt -- [A], rand:uniform(N) - 1)),N).

%% Dominio: Dos individuos I1 e I2 y un número natural N.
%% Codominio: Un nuevo individuo generado a partir de I1 y la mitad o menos de los elementos de I2. 

cruce(I1, I2, N) -> 	A = I1 ++ [X || X<-I2 ,not lists:member(X, I1)], 
			element(1, lists:split(N, A ++ rellenar(A, N))).

%% Dominio: Recibe una lista de número naturales entre 0 y N - 1.
%% Codominio: El complemento de la lista.

rellenar(I, N) when length(I) < N ->  lists:seq(0,N-1) -- I;
rellenar(_,_)->[].

%% Dominio: Una lista de individuos y un número entre 0 y N - 1.
%% Codominio: La primer tupla con una aptitud F >= Fl o el último elemento de la lista.

selection([{I,F}|_T], Fl) when F >= Fl -> {I,F};
selection([{I,F}|[]], _Fl) -> {I,F};
selection([_H|T], Fl) -> selection(T, Fl).

%% Dominio: Una lista, un número natural N y un símbolo E.
%% Codominio: Una lista con el elemento E en la posición N de la lista.

replaceAt([], _E, _N) -> [];
replaceAt([_H|T], E, 0) -> [E] ++ T;
replaceAt([H|T], E, N) -> [H] ++ replaceAt(T,E, N-1).

%% Dominio: Una lista de individuos y un número natural N.
%% Codominio: Una lista de inviduos con el 5% de ellos modificados aleatoriamente.

mutation(P, N) -> mutation(P, N, round(N*N * 0.05)).

%% Dominio: Una lista de individuos, un número natural N y un número PC indicando la cantidad de mutaciones restantes.
%% Codominio: Una lista de inviduos con el 5% de ellos modificados aleatoriamente.

mutation(P, _N, 0) -> P;
mutation(P, N, PC) -> A1 = rand:uniform(N*N) - 1,
			mutation(replaceAt(P, replaceAt(elementAt(P, A1), rand:uniform(N) - 1, rand:uniform(N) - 1), A1), N, PC - 1).



%% Función de evaluación

%% Dominio: 4 índices indicando la posición de una reina y el individuo I
%% Codominio: Aptitud del individuo I, un número natural entre 0 y N - 1.

evaluateIdx(J, J, _Vj, _Vi, _I) -> 0;
evaluateIdx(_J, _Idx, Vj, Vj, _I) -> 1;
evaluateIdx(J, Idx, Vj, Vi, _I) when abs(J - Idx) == abs(Vj - Vi), Idx /= J -> 1;
evaluateIdx(J, Idx, _Vj, Vi, I) -> evaluateIdx(J+1, Idx, elementAt(I,J+1), Vi, I).

%% Dominio: Una lista binaria con las colisiones de cada reina en el individuo.
%% Codominio: Aptitud del individuo I, un número natural entre 0 y N - 1.

evaluateAux(Coll) -> lists:foldl(fun(A, SUM) -> A + SUM end, 0, Coll). 

%% Dominio: Un individuo I.
%% Codominio: Aptitud del individuo I, un número natural entre 0 y N - 1.

evaluate([])->0;
evaluate(I)->evaluateAux([evaluateIdx(0,X, elementAt(I, 0),elementAt(I, X), I) || X<-lists:seq(0, length(I) - 1)]).


	%% Imprimir tablero
	
%% Dominio: Un individuo I y un número natural N.
%% Codominio: Un valor ok indicando que se finalizó la función con éxito.

printI(I,N) -> printI(I,N,0,0).

%% Dominio: Un individuo, el tamaño del individuo y dos indices.
%% Codominio: Un valor ok indicando que se finalizó la función con éxito.

printI(_In, N, N, _J) -> io:format("~n", []);
printI(In, N, I, N) -> io:format("~n", []), printI(In, N, I + 1, 0);
printI(In, N, I, J) -> printIAux(elementAt(In, J),I,J), printI(In, N, I, J + 1).

%% Dominio: Dos indices que indican la coordenada del tablero y otro indicando la posición de la reina.
%% Codominio: Un valor ok indicando que se finalizó la función con éxito.

printIAux(E, E, _J) ->  io:format("~ts ", [unicode:characters_to_binary("♕")]);
printIAux(_E, I, J) when (I + J) rem 2 == 0 -> io:format("~ts ", [unicode:characters_to_binary("■")]);
printIAux(_E, _I, _J) -> io:format("~ts ", [unicode:characters_to_binary("□")]).

%% Dominio: Una lista de número naturales.
%% Codominio: Un valor ok indicando que se finalizó la función con éxito.

printaux([H|T]) -> io:format("[~p,",[H]), printauxx(T).
printauxx([H|[]]) -> io:format("~p]~n",[H]);
printauxx([H|T]) -> io:format("~p,",[H]), printauxx(T).

	%% Población

	%% Población Inicial
	
%% Dominio: Un número natural N
%% Codominio: Una lista de longitud N² con Individuos de longitud N.

poblacion(0)->[];
poblacion(N)->[shuffle(lists:seq(0,N - 1)) || _X<-lists:seq(1,N*N)].

%% Dominio: Dos número naturales N y G, con N el número de reinas y G la generación actual. Además, una lista de tuplas
%%          de la forma {A,B} donde A es el individuo y B es su respectiva aptitud. 
%% Codominio: Un valor ok indicando que se finalizó la función con éxito.

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

%% Dominio: Un número natural N indicando el tamaño de reinas.
%% Codominio: Un valor ok indicando que se finalizó la función con éxito.

geneticosNReinas(N) -> geneticosNReinas(N, fitness(poblacion(N)), 0).





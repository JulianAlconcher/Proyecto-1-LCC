:- module(proylcc, 
	[  
		join/4
	]).
/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 * A partir de un path dado, calcula la suma final truncando a una potencia de dos y retorna una nueva grid. 
*/	
join(Grid, NumOfColumns, Path, RGrids):-
    lista_de_posiciones(Path,NumOfColumns,Posiciones),
    sort(Posiciones,PosicionesOrdenadas),
    convertir_en_valorX(Grid,PosicionesOrdenadas,GridEliminados,0),
    suma_valores(Grid,PosicionesOrdenadas,Suma),
    ultimo(Ultimo,Posiciones),
    truncar_a_potencia_de_2(Suma, Resultado),
    convertir_en_valorX(GridEliminados,[Ultimo],GridSuma,Resultado),
    obtener_lista_columnas(GridSuma,NumOfColumns,ListaColumnas),
    obtener_cant_filas(Grid,NumOfColumns,CantidadFilas),
    renovar_Columnas(ListaColumnas,ListaRenovada,CantidadFilas),
    intercalar(ListaRenovada,[],GridFinal),
    RGrids=[GridSuma,GridFinal].
    
% Predicado que retorna el ultimo elemento de una lista.     
ultimo(X, [X]).
ultimo(X, [_|T]) :- ultimo(X, T).

% Predicado que calcula la suma de todos los elementos de una lista. 
suma_valores([], [], 0).
suma_valores([X|L], [P|Ps], Suma) :-
   	nth0(P, [X|L], Elemento), 
    suma_valores([X|L], Ps, Suma0), 
	Suma is Suma0 + Elemento.
suma_valores([_|L], P, Suma) :-
    suma_valores(L, P, Suma).

% Predicado que realiza un redondeo hacia arriba de potencias de dos. 
truncar_a_potencia_de_2(N, Resultado) :-
    Exponente is ceiling(log(N) / log(2)), 
    Resultado is 2 ** Exponente.

% Predicado que dado un path de posiciones X,Y, convierte y retorna las coordenadas X,Y en posiciones de una lista. 
lista_de_posiciones([],_,[]).
lista_de_posiciones([[X,Y]|Resto],NumOfColumns,[P|Posiciones]):-
    P is Y+X*NumOfColumns,
    lista_de_posiciones(Resto,NumOfColumns,Posiciones).

              
% Predicado para convertir las Posiciones de una lista en 0.
convertir_en_valorX(Lista, Posiciones, NuevaLista,Valor) :-
    convertir_en_valorX_aux(Lista, Posiciones, 0, NuevaLista,Valor).

convertir_en_valorX_aux(Lista, [], _, Lista,_).
convertir_en_valorX_aux([_|Resto], [Posicion|RestoPosiciones], PosicionActual, [Valor|RestoNuevaLista],Valor) :-
    PosicionActual =:= Posicion,
    NuevaPosicionActual is PosicionActual + 1,
    convertir_en_valorX_aux(Resto, RestoPosiciones, NuevaPosicionActual, RestoNuevaLista,Valor).
convertir_en_valorX_aux([Elemento|Resto], Posiciones, PosicionActual, [Elemento|RestoNuevaLista],Valor) :-
    \+ member(PosicionActual, Posiciones),
    NuevaPosicionActual is PosicionActual + 1,
    convertir_en_valorX_aux(Resto, Posiciones, NuevaPosicionActual, RestoNuevaLista,Valor).

/**
 *Predicado que dada una Lista y un numero de columnas, retorna una lista con X listas
 *donde X es el numero de columnas y cada sublista es una columna de la matriz.
*/
obtener_lista_columnas(Grilla,NumOfColumns,ListaColumnas):-
    crear_lista_columnas(NumOfColumns,ListaVacia),
    obtener_X_Primeros(Grilla,NumOfColumns,X_Primeros),
    insertar_en_columnas(X_Primeros,ListaVacia,ListaColumnas).

% Predicado que inserta elementos de una lista, en una lista de sublistas(columnas)
insertar_en_columnas([], Resultado, Resultado).
insertar_en_columnas([X|Xs], L, Resultado):-
    insertar_elementos(X, L, ListaAux),
    insertar_en_columnas(Xs, ListaAux, Resultado).

%Inserta elementos en una lista de sublistas.
insertar_elementos([], Ls, Ls).
insertar_elementos([X|Xs], [L|Ls], [L2|Ls2]) :-
    append(L, [X], L2),
    insertar_elementos(Xs, Ls, Ls2).

% Predicado que crea una lista de sublistas.
crear_lista_columnas(0, []).

crear_lista_columnas(NumOfColumns, [ [] | ListaColumnas ]) :-
        AUX is NumOfColumns - 1,
        crear_lista_columnas(AUX, ListaColumnas).

% Predicado que retorna los X primeros elementos de una lista.
obtener_X_Primeros([], _, []).
obtener_X_Primeros([H|T], NumOfColumns, [Col|Cols]) :-
    length(Col, NumOfColumns),
    append(Col, Rest, [H|T]),
    obtener_X_Primeros(Rest, NumOfColumns, Cols).

% Predicado que actualiza los valores de una lista.
renovar_Columnas([],[],_).
renovar_Columnas([L|Ls],[L2|Ls2],CantFilas):-
    eliminar_ceros(L,X),
    completar_lista(X,L2,CantFilas),
    renovar_Columnas(Ls,Ls2,CantFilas).

% Predicado que completa a la lista entregada con numeros aleatorios que son potencia de 2.
completar_lista(Lista, ListaCompleta,CantFilas) :-
    length(Lista, Longitud),
    Longitud >= CantFilas,
    ListaCompleta = Lista.
completar_lista(Lista, ListaCompleta,CantFilas) :-
    length(Lista, Longitud),
    Longitud < CantFilas,
    generar_numero_aleatorio(Num),
    completar_lista([Num|Lista], ListaCompleta,CantFilas).

% Predicado que retorna un numero aleatorio potencia de 2. 
generar_numero_aleatorio(N) :-
    random_between(1,6, R),
    N is 2^R.

% Predicado que dada una lista de numeros, elimina todas las apariciones de ceros de la misma.  
eliminar_ceros([], []).
eliminar_ceros([0|T], L) :- eliminar_ceros(T, L).
eliminar_ceros([H|T], [H|L]) :- H \= 0, eliminar_ceros(T, L).

% Predicado que retorna una lista conteniendo todas las columnas intercaladas.
intercalar([], Res, R):-
    reverse(Res,R).
intercalar([[]|Xs],Res,R) :-
intercalar(Xs,Res,R) .
intercalar(L, R1, R) :-
    L \= [],
    tomar_cabeceras(L, Cabeceras),
    reverse(Cabeceras,Cabeceras_Inv),
    quitar_cabeceras(L,Resto),
    concatenar(Cabeceras_Inv, R1, RAUX),
    intercalar(Resto, RAUX, R).

%Predicado que retorna las cabeceras de una lista
tomar_cabeceras([], []).
tomar_cabeceras([[X|_]|Ls], [X|Rs]) :-
    tomar_cabeceras(Ls, Rs).

%Predicado que elimina las cabeceras de una lista
quitar_cabeceras([], []).
quitar_cabeceras([[_|Xs]|Ls], [Xs|Rs]) :-
    quitar_cabeceras(Ls, Rs).

%Predicado que concatena dos listas
concatenar([], L, L).
concatenar([X|L1], L2, [X|L3]) :-
    concatenar(L1, L2, L3).

%Predicado que retorna la cantidad de filas de la matriz.
obtener_cant_filas(Grid, NumColumnas, NumFilas) :-
    length(Grid, Longitud),
    NumFilas is Longitud // NumColumnas.    

    
%Preciado que retorna una grid con todos los caminos adyacentes calculados.
booster(Grid,NumOfColumns,RGrids):-
    adyacentes(0,Grid,NumOfColumns,[],[],ListaARetornar),
    agrupar_sublistas(ListaARetornar,[],ListaResultado),
    join_de_sublistas(Grid,NumOfColumns,ListaResultado,GridBooster),

    obtener_lista_columnas(GridBooster,NumOfColumns,ListaColumnas),
    obtener_cant_filas(GridBooster,NumOfColumns,CantidadFilas),
    renovar_Columnas(ListaColumnas,ListaRenovada,CantidadFilas),
    intercalar(ListaRenovada,[],GridFinal),
    RGrids=[GridBooster,GridFinal].

% Predicado que para cada sublista de una lista calcula la suma del camino.
join_de_sublistas(X,_,[],X).
join_de_sublistas(Grid,NumOfColumns,[L|Resto],RGrids):-
    join_booster(Grid,NumOfColumns,L,GridSuma),
    join_de_sublistas(GridSuma,NumOfColumns,Resto,RGrids).

% Predicado que convierte en cero el camino y coloca en el ultimo lugar la suma resultante.
join_booster(Grid, _, Lista, GridSuma):-
    sort(Lista,PosicionesOrdenadas),
    convertir_en_valorX(Grid,PosicionesOrdenadas,GridEliminados,0),
    suma_valores(Grid,PosicionesOrdenadas,Suma),
    ultimo(Ultimo,Lista),
    truncar_a_potencia_de_2(Suma, Resultado),
    convertir_en_valorX(GridEliminados,[Ultimo],GridSuma,Resultado).

%Obtiene todos los pares adyacentes iguales para todos los elementos de la matriz. 
adyacentes(Pos, Grid, _,_, ListaPath, ListaRetornar) :-
    length(Grid, Length),
    Pos >= Length,
    ListaRetornar = ListaPath.
adyacentes(Pos,Grid,NumOfColumns,ListaVisitados,ListaPath,ListaRetornar):-
    obtener_adyacentes(Pos,Grid,NumOfColumns,Path,ListaVisitados),
    append(ListaPath,Path,ListaNueva),
    PosNueva is Pos+1,
    append(ListaVisitados,[Pos],ListaVisitadosN),
    adyacentes(PosNueva,Grid,NumOfColumns,ListaVisitadosN,ListaNueva,ListaRetornar).

%Predicado que retorna todos los elementos adyacentes a una posicion.
obtener_adyacentes(Pos,Grid,NumOfColumns,Path,ListaVisitados):-
    findall(X,(ady(Pos,Grid,NumOfColumns,ListaVisitados,X)),Path).

%Caso Izquierda Arriba 1°
ady(Pos,Grid,NumOfColumns,ListaVisitados,ListaPath):-
    X is (Pos mod NumOfColumns),
    X \= 0, 
    Posicion_ady is Pos-(NumOfColumns+1),
    nth0(Pos,Grid,Elem),
    nth0(Posicion_ady,Grid,Ady),
    Elem =:= Ady,
    not(member(Posicion_ady, ListaVisitados)),
    append([Pos,Posicion_ady],[],ListaPath).

%Caso Arriba 2°
ady(Pos,Grid,NumOfColumns,ListaVisitados,ListaPath):-
    Posicion_ady is Pos-NumOfColumns,
    nth0(Pos,Grid,Elem),
    nth0(Posicion_ady,Grid,Ady),
    Elem =:= Ady,
    not(member(Posicion_ady, ListaVisitados)), 
    append([Pos,Posicion_ady],[],ListaPath).


%Caso Derecha Arriba 3°
ady(Pos,Grid,NumOfColumns,ListaVisitados,ListaPath):-
    X is (Pos mod NumOfColumns) - (NumOfColumns-1),
    X \= 0, 
    Posicion_ady is Pos-(NumOfColumns-1),
    nth0(Pos,Grid,Elem),
    nth0(Posicion_ady,Grid,Ady),
    Elem =:= Ady,
    not(member(Posicion_ady, ListaVisitados)), 
    append([Pos,Posicion_ady],[],ListaPath).

%Caso Izquierda 4°
ady(Pos,Grid,NumOfColumns,ListaVisitados,ListaPath):-
    X is (Pos mod NumOfColumns),
    X \= 0,  
    Posicion_ady is Pos-1,
    nth0(Pos,Grid,Elem),
    nth0(Posicion_ady,Grid,Ady),
    Elem =:= Ady,
    not(member(Posicion_ady, ListaVisitados)), 
    append([Pos,Posicion_ady],[],ListaPath).


%Caso Derecha 5°
ady(Pos,Grid,NumOfColumns,ListaVisitados,ListaPath):-
    X is (Pos mod NumOfColumns) - (NumOfColumns-1),
    X \= 0, 
    Posicion_ady is Pos+1,
    nth0(Pos,Grid,Elem),
    nth0(Posicion_ady,Grid,Ady),
    Elem =:= Ady,
    not(member(Posicion_ady, ListaVisitados)), 
    append([Pos,Posicion_ady],[],ListaPath).

%Caso Izquierda Abajo 6°
ady(Pos,Grid,NumOfColumns,ListaVisitados,ListaPath):-
    X is (Pos mod NumOfColumns),
    X \= 0,
    Posicion_ady is Pos+(NumOfColumns-1),
    nth0(Pos,Grid,Elem),
    nth0(Posicion_ady,Grid,Ady),
    Elem =:= Ady,
    not(member(Posicion_ady, ListaVisitados)), 
    append([Pos,Posicion_ady],[],ListaPath).

%Caso Abajo 7°
ady(Pos,Grid,NumOfColumns,ListaVisitados,ListaPath):-
    Posicion_ady is Pos+NumOfColumns,
    nth0(Pos,Grid,Elem), 
    nth0(Posicion_ady,Grid,Ady),
    Elem =:= Ady, 
    not(member(Posicion_ady, ListaVisitados)), 
    append([Pos,Posicion_ady],[],ListaPath).

%Caso Derecha Abajo 8°
ady(Pos,Grid,NumOfColumns,ListaVisitados,ListaPath):-
    X is (Pos mod NumOfColumns) - (NumOfColumns-1),
    X \= 0, 
    Posicion_ady is Pos+(NumOfColumns+1),
    nth0(Pos,Grid,Elem),
    nth0(Posicion_ady,Grid,Ady),
    Elem =:= Ady,
    not(member(Posicion_ady, ListaVisitados)), 
    append([Pos,Posicion_ady],[],ListaPath).

%Predicado que dada una lista de listas, agrupa las sublistas.
agrupar_sublistas([],ListaResultado,ListaResultado).
agrupar_sublistas([[X,Y]|Resto],ListaRetorno,ListaResultado):-
    not(esta_en_la_lista(Y,ListaRetorno)),
    encontrar_transitivos([X,Y],Resto,[X,Y],Resul),
    agrupar_sublistas(Resto,[Resul|ListaRetorno],ListaResultado).
agrupar_sublistas([_|Resto],ListaRetorno,ListaResultado):-
    agrupar_sublistas(Resto,ListaRetorno,ListaResultado).

%Predicado que agrupa pares transitivos en una misma lista.
encontrar_transitivos(_,[],X,X).
encontrar_transitivos([_,Y],[[X1,Y1]|Resto],Path,Resultado):-
    Y=:=X1,
    append(Path,[Y1],NuevoPath),
    encontrar_transitivos([X1,Y1],Resto,NuevoPath,Resultado).
encontrar_transitivos([X,_],[[X1,Y1]|Resto],Path,Resultado):-
    X=:=X1,
    append(Path,[Y1],NuevoPath),
    encontrar_transitivos([X1,Y1],Resto,NuevoPath,Resultado).
encontrar_transitivos([X,Y],[_|Resto],Path,Resultado):-
    encontrar_transitivos([X,Y],Resto,Path,Resultado).

%Predicado que verifica si un elemento pertenece a una lista de sublistas.
esta_en_la_lista(Elemento, ListaSublistas) :-
    append(ListaSublistas, ListaAplanada),
    member(Elemento, ListaAplanada).
    
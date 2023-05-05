:- module(proylcc, 
	[  
		join/4
	]).
/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
	join(Grid, _NumOfColumns, _Path, RGrids):-
		Grid = [N | Resto],	% La implementación actual es simplemente a modo de muestra, y no tiene sentido, debe reepmplazarla
		N2 is N * 2,		% por una implementación válida.
		RGrids = [[0 | Resto], [N2 | Resto]].
*/
	
join(Grid, NumOfColumns, Path, RGrids):-
    lista_de_posiciones(Path,NumOfColumns,Posiciones),
    sort(Posiciones,PosicionesOrdenadas),
    convertir_en_ceros(Grid,PosicionesOrdenadas,GridEliminados,0),
    suma_valores(Grid,PosicionesOrdenadas,Suma),
    ultimo(Ultimo,Posiciones),
    truncar_a_potencia_de_2(Suma, Resultado),
    convertir_en_ceros(GridEliminados,[Ultimo],GridSuma,Resultado),
    obtener_lista_columnas(GridSuma,NumOfColumns,ListaColumnas),
    obtener_cant_filas(Grid,NumOfColumns,CantidadFilas),
    renovar_Columnas(ListaColumnas,ListaRenovada,CantidadFilas),
    intercalar(ListaRenovada,[],NUEVAGRID),
    RGrids=[GridSuma,NUEVAGRID].
    
    
ultimo(X, [X]).
ultimo(X, [_|T]) :- ultimo(X, T).

   
suma_valores([], [], 0).

suma_valores([X|L], [P|Ps], Suma) :-
   	nth0(P, [X|L], Elemento), 
    suma_valores([X|L], Ps, Suma0), 
	Suma is Suma0 + Elemento.

suma_valores([_|L], P, Suma) :-
    suma_valores(L, P, Suma).

truncar_a_potencia_de_2(N, Resultado) :-
    Exponente is ceiling(log(N) / log(2)),  % calcular el exponente de la potencia de 2 y redondea para arriba
    Resultado is 2 ** Exponente.


lista_de_posiciones([],_,[]).
lista_de_posiciones([[X,Y]|Resto],NumOfColumns,[P|Posiciones]):-
    P is Y+X*NumOfColumns,
    lista_de_posiciones(Resto,NumOfColumns,Posiciones).

              
% Predicado para convertir en 0 una lista en varias posiciones
convertir_en_ceros(Lista, Posiciones, NuevaLista,Valor) :-
    convertir_en_ceros_aux(Lista, Posiciones, 0, NuevaLista,Valor).

% Caso base: hemos terminado de recorrer la lista y no hay más posiciones a modificar
convertir_en_ceros_aux(Lista, [], _, Lista,_).

% Caso recursivo: la posición actual está en la lista de posiciones a modificar
convertir_en_ceros_aux([_|Resto], [Posicion|RestoPosiciones], PosicionActual, [Valor|RestoNuevaLista],Valor) :-
    PosicionActual =:= Posicion,
    NuevaPosicionActual is PosicionActual + 1,
    convertir_en_ceros_aux(Resto, RestoPosiciones, NuevaPosicionActual, RestoNuevaLista,Valor).

% Caso recursivo: la posición actual no está en la lista de posiciones a modificar
convertir_en_ceros_aux([Elemento|Resto], Posiciones, PosicionActual, [Elemento|RestoNuevaLista],Valor) :-
    \+ member(PosicionActual, Posiciones),
    NuevaPosicionActual is PosicionActual + 1,
    convertir_en_ceros_aux(Resto, Posiciones, NuevaPosicionActual, RestoNuevaLista,Valor).

obtener_lista_columnas(Grilla,NumOfColumns,ListaColumnas):-
    crear_lista_columnas(NumOfColumns,ListaVacia),
    obtener_X_Primeros(Grilla,NumOfColumns,X_Primeros),
    insertar_en_columnas(X_Primeros,ListaVacia,ListaColumnas).
    
insertar_en_columnas([], Resultado, Resultado).
insertar_en_columnas([X|Xs], L, Resultado):-
    insertar_elementos(X, L, ListaAux),
    insertar_en_columnas(Xs, ListaAux, Resultado).

insertar_elementos([], Ls, Ls).
insertar_elementos([X|Xs], [L|Ls], [L2|Ls2]) :-
    append(L, [X], L2),
    insertar_elementos(Xs, Ls, Ls2).

crear_lista_columnas(0, []).
    crear_lista_columnas(NumOfColumns, [ [] | ListaColumnas ]) :-
    AUX is NumOfColumns - 1,
    crear_lista_columnas(AUX, ListaColumnas).

obtener_X_Primeros([], _, []).
obtener_X_Primeros([H|T], NumOfColumns, [Col|Cols]) :-
    length(Col, NumOfColumns),
    append(Col, Rest, [H|T]),
    obtener_X_Primeros(Rest, NumOfColumns, Cols).

renovar_Columnas([],[],_).
renovar_Columnas([L|Ls],[L2|Ls2],CantFilas):-
    eliminar_ceros(L,X),
    completar_lista(X,L2,CantFilas),
    renovar_Columnas(Ls,Ls2,CantFilas).

completar_lista(Lista, ListaCompleta,CantFilas) :-
    length(Lista, Longitud),
    Longitud >= CantFilas,
    ListaCompleta = Lista.
completar_lista(Lista, ListaCompleta,CantFilas) :-
    length(Lista, Longitud),
    Longitud < CantFilas,
    generar_numero_aleatorio(Num),
    completar_lista([Num|Lista], ListaCompleta,CantFilas).
    
    generar_numero_aleatorio(N) :-
    random_between(1,6, R),
    obtener_numero(R, N).
    obtener_numero(1, 2).
    obtener_numero(2, 4).
    obtener_numero(3, 8).
    obtener_numero(4, 16).
    obtener_numero(5, 32).
    obtener_numero(6, 64).

eliminar_ceros([], []).
eliminar_ceros([0|T], L) :- eliminar_ceros(T, L).
eliminar_ceros([H|T], [H|L]) :- H \= 0, eliminar_ceros(T, L).

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


tomar_cabeceras([], []).
tomar_cabeceras([[X|_]|Ls], [X|Rs]) :-
    tomar_cabeceras(Ls, Rs).

quitar_cabeceras([], []).
quitar_cabeceras([[_|Xs]|Ls], [Xs|Rs]) :-
    quitar_cabeceras(Ls, Rs).

concatenar([], L, L).
concatenar([X|L1], L2, [X|L3]) :-
    concatenar(L1, L2, L3).


obtener_cant_filas(Grid, NumColumnas, NumFilas) :-
    length(Grid, Longitud),
    NumFilas is Longitud // NumColumnas.    

    
%Metodo booster
booster(Grid,NumOfColumns,RGrids):-
    adyacentes(0,Grid,NumOfColumns,[],[],ListaARetornar),
    agrupar_sublistas(ListaARetornar,[],ListaResultado),
    join_de_sublistas(Grid,NumOfColumns,ListaResultado,GridBooster),
    obtener_lista_columnas(GridBooster,NumOfColumns,ListaColumnas),
    obtener_cant_filas(GridBooster,NumOfColumns,CantidadFilas),
    renovar_Columnas(ListaColumnas,ListaRenovada,CantidadFilas),
    intercalar(ListaRenovada,[],NUEVAGRID),
    RGrids=[GridBooster,NUEVAGRID].

join_de_sublistas(X,_,[],X).

join_de_sublistas(Grid,NumOfColumns,[L|Resto],RGrids):-
    join_booster(Grid,NumOfColumns,L,GridSuma),
    join_de_sublistas(GridSuma,NumOfColumns,Resto,RGrids).

join_booster(Grid, _, Lista, GridSuma):-
    sort(Lista,PosicionesOrdenadas),
    convertir_en_ceros(Grid,PosicionesOrdenadas,GridEliminados,0),
    suma_valores(Grid,PosicionesOrdenadas,Suma),
    ultimo(Ultimo,Lista),
    truncar_a_potencia_de_2(Suma, Resultado),
    convertir_en_ceros(GridEliminados,[Ultimo],GridSuma,Resultado).

%Obtiene todos los pares adyacentes iguales para todos los nodos de la matriz. 
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

agrupar_sublistas([],ListaResultado,ListaResultado).

agrupar_sublistas([[X,Y]|Resto],ListaRetorno,ListaResultado):-
    not(esta_en_la_lista(Y,ListaRetorno)),
    encontrar_transitivos([X,Y],Resto,[X,Y],Resul),
    agrupar_sublistas(Resto,[Resul|ListaRetorno],ListaResultado).

agrupar_sublistas([_|Resto],ListaRetorno,ListaResultado):-
    agrupar_sublistas(Resto,ListaRetorno,ListaResultado).


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


esta_en_la_lista(Elemento, ListaSublistas) :-
    append(ListaSublistas, ListaAplanada),
    member(Elemento, ListaAplanada).
    
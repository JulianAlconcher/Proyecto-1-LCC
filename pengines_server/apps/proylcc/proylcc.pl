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
    obtener_columnas(GridSuma,Col1,Col2,Col3,Col4,Col5),
    renovar_Columna(Col1,ColRen1),renovar_Columna(Col2,ColRen2),renovar_Columna(Col3,ColRen3),renovar_Columna(Col4,ColRen4),renovar_Columna(Col5,ColRen5),
    nueva_lista(ColRen1,ColRen2,ColRen3,ColRen4,ColRen5,NUEVAGRID),
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


renovar_Columna(Lista,ListaNueva):-
    eliminar_ceros(Lista,X),
    completar_lista(X,ListaNueva).
    
eliminar_ceros([], []).
eliminar_ceros([0|T], L) :- eliminar_ceros(T, L).
eliminar_ceros([H|T], [H|L]) :- H \= 0, eliminar_ceros(T, L).
    
completar_lista(Lista, ListaCompleta) :-
    length(Lista, Longitud),
    Longitud >= 8,
    ListaCompleta = Lista.
completar_lista(Lista, ListaCompleta) :-
    length(Lista, Longitud),
    Longitud < 8,
    generar_numero_aleatorio(Num),
    completar_lista([Num|Lista], ListaCompleta).
    
generar_numero_aleatorio(N) :-
    random_between(1,6, R),
    obtener_numero(R, N).
    
    obtener_numero(1, 2).
    obtener_numero(2, 4).
    obtener_numero(3, 8).
    obtener_numero(4, 16).
    obtener_numero(5, 32).
    obtener_numero(6, 64).
    
obtener_columnas([], [], [], [], [], []).
obtener_columnas([C1,C2,C3,C4,C5|Resto], [C1|Col1], [C2|Col2], [C3|Col3], [C4|Col4], [C5|Col5]) :-
    obtener_columnas(Resto, Col1, Col2, Col3, Col4, Col5).
    
    
nueva_lista(L1, L2, L3, L4, L5, Resultado) :-
    intercalar_listas(L1, L2, L3, L4, L5, [], Resultado).
    
intercalar_listas([], [], [], [], [], Resultado, Resultado).
intercalar_listas([X1|L1], [X2|L2], [X3|L3], [X4|L4], [X5|L5], Temp, Resultado) :-
    append(Temp, [X1, X2, X3, X4, X5], NuevaLista),
    intercalar_listas(L1, L2, L3, L4, L5, NuevaLista, Resultado).

 /**RGrids = [[0 | Ns], [0 |Ns] ,[N2 | Ns]].
       _Path = [[2,0],[3,0],[4,1],[3,1],[2,1]]
join([64,4,64,32,16,
     64,8,16,2,32,
     2,4,64,64,2,
     2,4,32,16,4,
     16,4,16,16,16,
     16,64,2,32,32,
     64,2,64,32,64,
     32,2,64,32,4],
*/




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
    %suma_valores(Grid,PosicionesOrdenadas,Suma),
    ultimo(Ultimo,Posiciones),
    convertir_en_ceros(GridEliminados,[Ultimo],GridSuma,256),
    RGrids=[GridEliminados,GridSuma].
    
  % suma_valores(Grid,PosicionesOrdenadas,Suma):-
    
   ultimo(X, [X]).
   ultimo(X, [_|T]) :- ultimo(X, T).

   %suma_valores([], [], 0).
   %suma_valores([X|L], [P|Ps], Suma) :-
   % nth0(P, [X|L], Elemento), 
    %suma_valores(L, Ps, Suma0), 
   % Suma is Suma0 + Elemento.

 %  suma_valores([_|L], P, Suma) :-
   % suma_valores(L, P, Suma).


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




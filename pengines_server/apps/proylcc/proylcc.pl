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


agrupar_sublistas(ListaActual,ListaModificada,ListaActual):-
    ListaActual=ListaModificada.
agrupar_sublistas(ListaActual,_,ListaResultado):-
    agrupar_iguales(ListaActual,[],[],ListaRet),
    agrupar_sublistas(ListaRet,ListaActual,ListaResultado).

    agrupar_iguales([],_,X,X).
agrupar_iguales([H|T],ListaVisitados,ListaResultado,ListaRetornar):-
        not(algun_elemento_en_comun(H,ListaVisitados)),
        existen_iguales(H,T,Resul),
        append(ListaResultado,[Resul],NuevaListaResultado),
        append(ListaVisitados,Resul,NuevaListaVisitados),
        agrupar_iguales(T,NuevaListaVisitados,NuevaListaResultado,ListaRetornar).
    agrupar_iguales([_|T],ListaVisitados,ListaResultado,ListaRetornar):-
        agrupar_iguales(T,ListaVisitados,ListaResultado,ListaRetornar).
            
existen_iguales(Nueva,[],Nueva).

existen_iguales(L,[H|T],ListaAux):-
    comparten_elementos(L,L,H,Nueva),
    append(L,Nueva,ListaRepes),
    list_to_set(ListaRepes,NuevoResul),
    existen_iguales(NuevoResul,T,ListaAux).

comparten_elementos([],_,_,[]).
comparten_elementos([H|_],Original,L,NuevaLista) :-
    member(H,L),
    append(Original,L,NuevaLista).
comparten_elementos([_|T],Original,L,NuevaLista):-
    comparten_elementos(T,Original,L,NuevaLista).               

algun_elemento_en_comun(Lista1, Lista2) :-
    member(Elemento, Lista1),
    member(Elemento, Lista2).  

%PARTE 2 DEL PROYECTO

% Predicado para convertir una posición en coordenadas X, Y
posiciones_a_coordenadas_XY(Position, _, Cols, X, Y) :-
    Y is Position mod Cols,
    X is Position div Cols.

% Predicado para convertir una lista de posiciones en una lista de coordenadas X, Y
posiciones_a_coordenadas_XY([], _, _, []).
posiciones_a_coordenadas_XY([Position|Positions], Rows, Cols, [[X,Y]|Coordinates]) :-
    posiciones_a_coordenadas_XY(Position, Rows, Cols, X, Y),
    posiciones_a_coordenadas_XY(Positions, Rows, Cols, Coordinates).



generar_lista_de_posiciones_de_grid_completa(0, X,X).

%Predicado que genera una lista de posiciones del 0 al CantidadElementos-1.
%Ejemplo: Para la Grid de inicio genera una lista del 0..39.
generar_lista_de_posiciones_de_grid_completa(N, List,Resultado) :-
    N > 0,
    N1 is N - 1,
    append([N1],List,NuevaLista),
    generar_lista_de_posiciones_de_grid_completa(N1, NuevaLista,Resultado).

%Predicado principal que ejecuta otros predicados para llevar a cabo la funcionalidad.
ayuda_movida_maxima(Grid,NumOfColumns,Resultado):-
    length(Grid,CantElementos),
    generar_lista_de_posiciones_de_grid_completa(CantElementos,[],Posiciones),
    obtener_path_maximo(Posiciones,Grid,NumOfColumns,[],Resul),
    obtener_cant_filas(Grid,NumOfColumns,NumOfRows),
    posiciones_a_coordenadas_XY(Resul,NumOfRows,NumOfColumns,Resultado).


obtener_path_maximo([],_,_,ListaPath,X):-
    path_maximo(ListaPath,[0],[X|_]),!.

obtener_path_maximo([P|Ps],Grid,NumOfColumns,ListaPath,Resultado):-
    obtener_adyacentes(P,Grid,NumOfColumns,[],[],Adyacentes,1),   
    findall(X,metodo(P,Grid,NumOfColumns,[P],Adyacentes,X),PathSuma),
    append(ListaPath,PathSuma,NuevaListaPath),
    obtener_path_maximo(Ps,Grid,NumOfColumns,NuevaListaPath,Resultado).       
    
    
path_maximo([],X,X).

path_maximo([X|Xs],Maximo,Res):-
    ultimo_elemento(X,SumaAct),
    ultimo_elemento(Maximo,SumaMax),
    SumaAct > SumaMax,
    path_maximo(Xs,X,Res).

path_maximo([_|Xs],Maximo,Res):-
    path_maximo(Xs,Maximo,Res).
    
ultimo_elemento(List, Last) :-
    reverse(List, [Last|_]).

metodo(_,Grid,_,PathActual,[],PathSuma):-
    controlar_suma(Grid,PathActual,PathSuma),!.

metodo(_,Grid,NumOfColumns,PathActual,[X|_],PathSuma):-
    append(PathActual,[X],NuevoPath),
    obtener_adyacentes(X,Grid,NumOfColumns,[],NuevoPath,NuevosAdyacentes,1),
    metodo(X,Grid,NumOfColumns,NuevoPath,NuevosAdyacentes,PathSuma).


metodo(Posicion,Grid,NumOfColumns,PathActual,[_|Xs],PathSuma):-
    length(Xs,Res),
    Res>0,
    metodo(Posicion,Grid,NumOfColumns,PathActual,Xs,PathSuma).

primer_elemento([X|_], X).
        
controlar_suma(Grid,H,ResultadoParcial):-
    length(H,Cant),
    Cant > 1,
    suma_valores(Grid,H,Suma), 
    truncar_a_potencia_de_2(Suma,SumaRedondeada),
    append([H],[SumaRedondeada],ResultadoParcial).
        
obtener_bloques_posibles([],X,Resul):-
    sort(X,Aux),
    reverse(Aux,Resul).

obtener_bloques_posibles([X|Xs],ListaBloques,Resul):-
    not(member(X,ListaBloques)),
    append(ListaBloques,[X],NuevaListaBloques),
    obtener_bloques_posibles(Xs,NuevaListaBloques,Resul),!.

obtener_bloques_posibles([_|Xs],ListaBloques,Resul):-
    obtener_bloques_posibles(Xs,ListaBloques,Resul).          

ayuda_maximos_iguales_adyacentes(Grid,NumOfColumns,Path):-
    obtener_bloques_posibles(Grid,[],BloquesPosibles),
    length(Grid,CantElementos),
    generar_lista_de_posiciones_de_grid_completa(CantElementos,[],Posiciones),
    buscar_path_valor(Grid,NumOfColumns,BloquesPosibles,Posiciones,Posiciones,[],Res),
    obtener_cant_filas(Grid,NumOfColumns,NumOfRows),
    posiciones_a_coordenadas_XY(Res,NumOfRows,NumOfColumns,Path).
    
buscar_path_valor(Grid,NumOfColumns,_,[],_,ListaCaminos,Path):-
    buscar_path_adyacente(Grid,NumOfColumns,ListaCaminos,[],Path),!.

buscar_path_valor(Grid,NumOfColumns,[_|Bs],[],ListaPosCompleta,_,Path):-
    buscar_path_valor(Grid,NumOfColumns,Bs,ListaPosCompleta,ListaPosCompleta,[],Path),!.


buscar_path_valor(Grid,NumOfColumns,[B|Bs],[X|Xs],ListaPosCompleta,ListaCaminos,Path):-
    obtener_adyacentes(X,Grid,NumOfColumns,[],[],Adyacentes,1),
    findall(P,obtener_path_valor(X,Grid,NumOfColumns,[X],Adyacentes,P,B),Lista),
    append(ListaCaminos,Lista,NuevaListaCaminos),
    buscar_path_valor(Grid,NumOfColumns,[B|Bs],Xs,ListaPosCompleta,NuevaListaCaminos,Path).
    

buscar_path_adyacente(_,_,[],X,X):-
    length(X,Cantidad),
    Cantidad>0.

buscar_path_adyacente(_,_,[],_,_):-fail.
    

buscar_path_adyacente(Grid,NumOfColumns,[[Path|Valor]|_],_,Res):-
    join_maximos_iguales(Grid,NumOfColumns,Path,Grids),
    ultimo_elemento(Grids,GridConGravedad),
    posicion_gravedad(Path,NumOfColumns,NuevaPos),
    obtener_adyacentes(NuevaPos,GridConGravedad,NumOfColumns,[],[],ListaDeAdyacentes,0),
    es_adyacente_a_uno_igual(Grid,ListaDeAdyacentes,Valor),
    buscar_path_adyacente(Grid,NumOfColumns,[],Path,Res),!.

buscar_path_adyacente(Grid,NumOfColumns,[_|Ls],Resultado,Res):-
    buscar_path_adyacente(Grid,NumOfColumns,Ls,Resultado,Res),!.

es_adyacente_a_uno_igual(Grid,[X|_],Valor):-
    nth0(X,Grid,Elem),
    nth0(0,Valor,Val),
    Elem =:= Val.

es_adyacente_a_uno_igual(Grid,[_|Xs],Valor):-
    es_adyacente_a_uno_igual(Grid,Xs,Valor).

posicion_gravedad(Path,NumOfColumns,PosicionNueva):-
    ultimo_elemento(Path,Ultimo),
    Resto is (Ultimo mod NumOfColumns),
    controlar_resto(Path,NumOfColumns,Ultimo,Resto,0,Cantidad),
    PosicionNueva is (Ultimo + (Cantidad*NumOfColumns)).

controlar_resto([],_,_,_,X,X).    
    
controlar_resto([P|Ps],NumOfColumns,Pos,Resto,Cantidad,Res):-
    P > Pos,
    X  is (P mod NumOfColumns),
    X = Resto,
    NuevaCantidad is (Cantidad + 1),
    controlar_resto(Ps,NumOfColumns,Pos,Resto,NuevaCantidad,Res),!.

controlar_resto([_|Ps],NumOfColumns,Pos,Resto,Cantidad,Res):-
    controlar_resto(Ps,NumOfColumns,Pos,Resto,Cantidad,Res).

obtener_path_valor(_,Grid,_,PathActual,[],PathSuma,Valor):-
    controlar_suma_valor(Grid,PathActual,PathSuma,Valor).

obtener_path_valor(_,Grid,NumOfColumns,PathActual,[X|_],PathSuma,Valor):-
    append(PathActual,[X],NuevoPath),
    obtener_adyacentes(X,Grid,NumOfColumns,[],NuevoPath,NuevosAdyacentes,1),
    obtener_path_valor(X,Grid,NumOfColumns,NuevoPath,NuevosAdyacentes,PathSuma,Valor).


obtener_path_valor(Posicion,Grid,NumOfColumns,PathActual,[_|Xs],PathSuma,Valor):-
    obtener_path_valor(Posicion,Grid,NumOfColumns,PathActual,Xs,PathSuma,Valor).
        
obtener_adyacentes(Posicion,Grid,NumOfColumns,Adyacentes,PathActual,AdyacentesResultado,If):-
    ady_rec(Posicion,Grid,NumOfColumns,Adyacentes,PathActual,NuevosAdyacentes,If),
    obtener_adyacentes(Posicion,Grid,NumOfColumns,NuevosAdyacentes,PathActual,AdyacentesResultado,If),!.

obtener_adyacentes(_,_,_,X,_,X,_).

%1°
ady_rec(Posicion,Grid,NumOfColumns,Adyacentes,PathActual,AdyacentesResultado,If):-
    X is (Posicion mod NumOfColumns),
    X \= 0,
    Posicion_ady is Posicion - (NumOfColumns+1),
    comprobar_posicion(Posicion,Grid,Posicion_ady,PathActual,Adyacentes,AdyacentesResultado,If),!.

%2°
ady_rec(Posicion,Grid,NumOfColumns,Adyacentes,PathActual,AdyacentesResultado,If):-
    Posicion_ady is Posicion-NumOfColumns,
    comprobar_posicion(Posicion,Grid,Posicion_ady,PathActual,Adyacentes,AdyacentesResultado,If),!.

%3°
ady_rec(Posicion,Grid,NumOfColumns,Adyacentes,PathActual,AdyacentesResultado,If):-
    X is (Posicion mod NumOfColumns) - (NumOfColumns-1),
    X \= 0, 
    Posicion_ady is Posicion-(NumOfColumns-1),
    comprobar_posicion(Posicion,Grid,Posicion_ady,PathActual,Adyacentes,AdyacentesResultado,If),!.

%4°
ady_rec(Posicion,Grid,NumOfColumns,Adyacentes,PathActual,AdyacentesResultado,If):-
    X is (Posicion mod NumOfColumns),
    X \= 0,  
    Posicion_ady is Posicion-1,
    comprobar_posicion(Posicion,Grid,Posicion_ady,PathActual,Adyacentes,AdyacentesResultado,If),!.

%5°
ady_rec(Posicion,Grid,NumOfColumns,Adyacentes,PathActual,AdyacentesResultado,If):-
    X is (Posicion mod NumOfColumns) - (NumOfColumns-1),
    X \= 0, 
    Posicion_ady is Posicion+1,
    comprobar_posicion(Posicion,Grid,Posicion_ady,PathActual,Adyacentes,AdyacentesResultado,If),!.

%6°
ady_rec(Posicion,Grid,NumOfColumns,Adyacentes,PathActual,AdyacentesResultado,If):-
    X is (Posicion mod NumOfColumns),
    X \= 0,
    Posicion_ady is Posicion+(NumOfColumns-1),
    comprobar_posicion(Posicion,Grid,Posicion_ady,PathActual,Adyacentes,AdyacentesResultado,If),!.

%7°
ady_rec(Posicion,Grid,NumOfColumns,Adyacentes,PathActual,AdyacentesResultado,If):-
    Posicion_ady is Posicion+NumOfColumns,
    comprobar_posicion(Posicion,Grid,Posicion_ady,PathActual,Adyacentes,AdyacentesResultado,If),!.

%8°
ady_rec(Posicion,Grid,NumOfColumns,Adyacentes,PathActual,AdyacentesResultado,If):-
    X is (Posicion mod NumOfColumns) - (NumOfColumns-1),
    X \= 0, 
    Posicion_ady is Posicion+(NumOfColumns+1),
    comprobar_posicion(Posicion,Grid,Posicion_ady,PathActual,Adyacentes,AdyacentesResultado,If),!.

comprobar_posicion(Posicion,Grid,Posicion_ady,PathActual,Adyacentes,AdyacentesResultado,If):-
    If =:= 1,
    nth0(Posicion,Grid,Elem),
    nth0(Posicion_ady,Grid,Ady),
    not(member(Posicion_ady,PathActual)),
    not(member(Posicion_ady,Adyacentes)),
    comprobar_iguales_o_dobles(PathActual,Elem,Ady),
    append(Adyacentes,[Posicion_ady],AdyacentesResultado).

    comprobar_posicion(_,_,Posicion_ady,_,Adyacentes,AdyacentesResultado,If):-
    If =:=0,
    not(member(Posicion_ady,Adyacentes)),
    append(Adyacentes,[Posicion_ady],AdyacentesResultado).
            

controlar_suma_valor(Grid,H,ResultadoParcial,ValorCheck):-
    length(H,Cant),
    Cant > 1,
    suma_valores(Grid,H,Suma), 
    truncar_a_potencia_de_2(Suma,SumaRedondeada),
    SumaRedondeada=ValorCheck,
    append([H],[SumaRedondeada],ResultadoParcial).
    
son_iguales(X,X).

comprobar_iguales_o_dobles(_,X,Y):- (X=:=Y),!,true.
comprobar_iguales_o_dobles(Path,X,Y):-(comprobarLong(Path,Longitud),Longitud >0),!,X*2 =:= Y.

comprobarLong(Path,Longitud):-length(Path, Longitud).

% Predicado que calcula la suma de todos los elementos de una lista. 
suma_valores([], [], 0).
suma_valores([X|L], [P|Ps], Suma) :-
        nth0(P, [X|L], Elemento), 
    suma_valores([X|L], Ps, Suma0), 
    Suma is Suma0 + Elemento,!.
suma_valores([_|L], P, Suma) :-
    suma_valores(L, P, Suma).

truncar_a_potencia_de_2(N, Resultado) :-
    Exponente is ceiling(log(N) / log(2)), 
    Resultado is 2 ** Exponente.

join_maximos_iguales(Grid, NumOfColumns, Path, RGrids):-
    sort(Path,PosicionesOrdenadas),
    convertir_en_valorX(Grid,PosicionesOrdenadas,GridEliminados,0),
    suma_valores(Grid,PosicionesOrdenadas,Suma),
    ultimo(Ultimo,Path),
    truncar_a_potencia_de_2(Suma, Resultado),
    convertir_en_valorX(GridEliminados,[Ultimo],GridSuma,Resultado),
    obtener_lista_columnas(GridSuma,NumOfColumns,ListaColumnas),
    intercalar(ListaColumnas,[],GridFinal),
    RGrids=[GridSuma,GridFinal],!.
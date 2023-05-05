import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import { joinResult, numberToColor, valueInPos } from './util';
import soundBlock from "./SoundFx/selectionSound.mp3"
import soundSucces from "./SoundFx/Succes.mp3"
//import soundBooster from "./SoundFx/BoosterFx.mp3"

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [numOfColumns, setNumOfColumns] = useState(null);
  const [score, setScore] = useState(0);
  const [texto, setTexto] = useState("");
  const [PossiblePathAdd, setPossiblePathAdd] = useState(0);
  const [path, setPath] = useState([]);
  const [waiting, setWaiting] = useState(false);
  var elem = document.getElementById('recuadro');
  const boton = document.querySelector('#boton');

  useEffect(() => {
    // This is executed just once, after the first render.
    PengineClient.init(onServerReady);
  }, []);

  /**
   * Called when the server was successfully initialized
   */
  function onServerReady(instance) {
    pengine = instance;
    const queryS = 'init(Grid, NumOfColumns)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setNumOfColumns(response['NumOfColumns']);
      }
    });
  }

  /**
   * Called while the user is drawing a path in the grid, each time the path changes.
   */
  function onPathChange(newPath) {
    // No effect if waiting.
    if (waiting) {
      return;
    }
    setPossiblePathAdd(round(addPathInProgess(newPath)));
    var elem = document.getElementById('recuadro');
    if (elem) {
    elem.style.backgroundColor  = numberToColor(PossiblePathAdd);
    elem.style.visibility = 'visible';
    }
    console.log("Score:" + PossiblePathAdd);
    setPath(newPath);
    console.log(JSON.stringify(newPath));
    playSound(soundBlock);
  }

  function addPathInProgess(newPath){
    var suma = 0;
    for (var i = 0; i < newPath.length; i++) {
      suma = suma + valueInPos(newPath[i],grid,5);
    }
    return suma;
  }
  
  if(boton!=null)
  boton.addEventListener("click", 
  function() {
    elem.style.visibility = 'hidden';
    setTexto("El Booster Fue Activado!");
    booster();
    //playSound(soundBooster)
  });

  function round(num){
    const log2num = Math.floor(Math.log2(num));
    return Math.pow(2, log2num) === num ? num : Math.pow(2, log2num + 1);
  }

  function playSound(sonido) {
    new Audio(sonido).play()
  }
  /**
   * Called when the user finished drawing a path in the grid.
   */
  function onPathDone() {
    /*
    Build Prolog query, which will be like:
    join([
          64,4,64,32,16,
          64,8,16,2,32,
          2,4,64,64,2,
          2,4,32,16,4,
          16,4,16,16,16,
          16,64,2,32,32,
          64,2,64,32,64,
          32,2,64,32,4
          ], 
          5, 
          [[2, 0], [3, 0], [4, 1], [3, 1], [2, 1], [1, 1], [1, 2], [0, 3]],
          RGrids
        ).
    */
    setPossiblePathAdd(0);
    elem.style.visibility = 'hidden';
    const gridS = JSON.stringify(grid);
    const pathS = JSON.stringify(path);
    const queryS = "join(" + gridS + "," + numOfColumns + "," + pathS + ", RGrids)";
    setWaiting(true);
    playSound(soundSucces);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setScore(score + joinResult(path, grid, numOfColumns));
        setPath([]);
        animateEffect(response['RGrids']);
        setTexto("");
      } else {
        setWaiting(false);
      }
    });
  }

   /**
   * Called when the user clicked booster button.
   */
   function booster() {
    setPossiblePathAdd(0);
    elem.style.visibility = 'hidden';
    const gridS = JSON.stringify(grid);
    const pathS = JSON.stringify(path);
    const queryS = "join(" + gridS + "," + numOfColumns + "," + pathS + ", RGrids)";
    setWaiting(true);
    playSound(soundSucces);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setScore(score + joinResult(path, grid, numOfColumns));
        setPath([]);
        animateEffect(response['RGrids']);
        setTexto("");
      } else {
        setWaiting(false);
      }
    });
  }

  /**
   * Displays each grid of the sequence as the current grid in 1sec intervals.
   * @param {number[][]} rGrids a sequence of grids.
   */
  function animateEffect(rGrids) {
    setGrid(rGrids[0]);
    const restRGrids = rGrids.slice(1);
    if (restRGrids.length > 0) {
      setTimeout(() => {
        animateEffect(restRGrids);
      }, 1000);
    } else {
      setWaiting(false);
    }
  }

  if (grid === null) {
    return null;
  }
  return (
    <div className="game">
      <div className="header">
        <div className="container">
          <div className="score">{score}</div>
          <div className="recuadro" id='recuadro'> 
            <div className="suma">{PossiblePathAdd}</div>
          </div>
        </div>
      </div>
      <Board
        grid={grid}
        numOfColumns={numOfColumns}
        path={path}
        onPathChange={onPathChange}
        onDone={onPathDone}
      />
      <p className="textoDeBooster" id='boosterActivado'>{texto}</p>
      <button className="boton" id='boton'> BOOST</button>     
    </div>
  );
}

export default Game;

let questions = [];
let currentQuestionIndex = 0;
let score = 0;
let timer;
let timeLeft = 30; // Temps en secondes

// Fonction pour mélanger les éléments d'un tableau
function shuffle(array) {
  for (let i = array.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [array[i], array[j]] = [array[j], array[i]];
  }
  return array;
}

// Fonction pour charger les questions depuis un fichier CSV
function loadQuestions(csvFile) {
  return fetch(csvFile)
    .then(response => response.text())
    .then(data => {
      const lines = data.trim().split('\n');
      questions = lines.slice(1).map(line => {
        const [text, options, answers] = line.split('","');
        return {
          text: text.replace(/"/g, ''),
          options: options.split(','),
          answers: answers.replace(/"/g, '').split(',')
        };
      });
      shuffle(questions);
      loadQuestion();
    });
}

function allowDrop(ev) {
  ev.preventDefault();
}

function drag(ev) {
  ev.dataTransfer.setData("text", ev.target.id);
}

function drop(ev) {
  ev.preventDefault();
  var data = ev.dataTransfer.getData("text");
  var element = document.getElementById(data);
  if (ev.target.classList.contains('droppable') && ev.target.children.length === 0) {
    ev.target.appendChild(element);
    checkAnswer(ev.target, element);
  }
}

function checkAnswer(dropZone, draggedElement) {
  const correctAnswers = questions[currentQuestionIndex].answers;
  if (correctAnswers.includes(draggedElement.textContent)) {
    dropZone.style.backgroundColor = 'lightgreen';
    dropZone.style.borderColor = 'green';
    score++;
  } else {
    dropZone.style.backgroundColor = 'lightcoral';
    dropZone.style.borderColor = 'red';
  }
}

function nextQuestion() {
  currentQuestionIndex++;
  if (currentQuestionIndex < questions.length) {
    loadQuestion();
  } else {
    clearInterval(timer);
    document.getElementById('score').textContent = `Score final: ${score} / ${questions.length}`;
    document.getElementById('question-container').style.display = 'none';
    document.getElementById('end-buttons').style.display = 'block';
  }
}

function loadQuestion() {
  const questionData = questions[currentQuestionIndex];
  document.getElementById('question').textContent = questionData.text;
  const dragContainer = document.getElementById('drag-container');
  const dropContainer = document.getElementById('drop-container');
  dragContainer.innerHTML = '';
  dropContainer.innerHTML = '';

  questionData.options.forEach(option => {
    const dragItem = document.createElement('div');
    dragItem.id = 'item-' + option;
    dragItem.className = 'draggable';
    dragItem.draggable = true;
    dragItem.ondragstart = drag;
    dragItem.textContent = option;
    dragContainer.appendChild(dragItem);
  });

  questionData.answers.forEach(answer => {
    const dropItem = document.createElement('div');
    dropItem.className = 'droppable';
    dropItem.ondrop = drop;
    dropItem.ondragover = allowDrop;
    dropItem.textContent = `Déposez ici : ${answer}`;
    dropContainer.appendChild(dropItem);
  });

  resetTimer();
  startTimer();
}

function startTimer() {
  timer = setInterval(function() {
    if (timeLeft <= 0) {
      clearInterval(timer);
      nextQuestion();
    } else {
      document.getElementById('timer').textContent = `Temps restant: ${timeLeft} s`;
    }
    timeLeft -= 1;
  }, 1000);
}

function resetTimer() {
  clearInterval(timer);
  timeLeft = 30; // Réinitialiser le temps à 30 secondes
  document.getElementById('timer').textContent = `Temps restant: ${timeLeft} s`;
}

function replay() {
  currentQuestionIndex = 0;
  score = 0;
  shuffle(questions);
  document.getElementById('end-buttons').style.display = 'none';
  document.getElementById('question-container').style.display = 'block';
  loadQuestion();
}

function returnToMenu() {
  window.location.href = 'index.html';
}

document.getElementById('replay-button').addEventListener('click', replay);
document.getElementById('menu-button').addEventListener('click', returnToMenu);

// Déterminer quel jeu charger en fonction du paramètre URL
const urlParams = new URLSearchParams(window.location.search);
const game = urlParams.get('game');

let csvFile;
let gameTitle;

if (game === 'villes-de-france') {
  csvFile = 'questions-villes-de-france.csv';
  gameTitle = 'Jeu culturel : les villes de France';
} else if (game === 'passe-compose') {
  csvFile = 'questions-passe-compose.csv';
  gameTitle = 'Jeu de grammaire : Le passé composé';
} else {
  window.location.href = 'index.html';
  
}

document.getElementById('game-title').textContent = gameTitle;
loadQuestions(csvFile);
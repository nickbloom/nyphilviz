(function() {
  fuzzy.analyzeSubTerms = true;

  var options = ["Aaron Copland","Edouard Lalo","Edvard Grieg","Hector Berlioz","Richard Strauss","Anton Bruckner","John Adams","Béla Bartok","Leonard Bernstein","Ludwig van Beethoven","Cole Porter","George Gershwin","Traditional","Charles Ives","William Schuman","John Philip Sousa","Unspecified","Claude Debussy","Deems Taylor","Demonstration","Henri Vieuxtemps","Igor Stravinsky","Johann Christian Bach","Richard Wagner","Antonín Dvorak","Sergei Rachmaninoff","Anthem","Manuel de Falla","Maurice Ravel","Carlos Chavez","Franz Joseph Haydn","Christoph Willibald Gluck","Frederick Loewe","Gustav Mahler","Dmitri Shostakovich","Jerome Kern","Morton Gould","Roy Harris","Sir Edward Elgar","Jr. Johann Strauss","Pyotr Ilyich Tchaikovsky","Wolfgang Amadeus Mozart","Johannes Brahms","Alexander Glazunov","Gunther Schuller","Georges Bizet","Giacomo Puccini","Anton von Webern","Louis Moreau Gottschalk","Max Bruch","Nikolai Rimsky-Korsakov","William Walton","Stephen Collins Foster","Aram Khachaturian","Leroy Anderson","Antonio Vivaldi","Ottorino Respighi","Henry Purcell","Samuel Barber","Edward Kennedy \"Duke\" Ellington","Johann Sebastian Bach","Arnold Schoenberg","Felix Mendelssohn","Robert Schumann","Sergei Prokofiev","Modest Musorgsky","Frederick Delius","Alban Berg","Carl Maria Von Weber","Heitor Villa-Lobos","Camille Saint-Saens","Charles Martin Loeffler","Virgil Thomson","Ned Rorem","Bedrich Smetana","Reinhold Gliere","Paul Hindemith","Jr. John Corigliano","Mikhail Glinka","Gaetano Donizetti","Leoš Janacek","George Frideric Handel","Ruggero Leoncavallo","Arthur Honegger","Joseph Turrin","Lukas Foss","Richard Rodgers","Emil Nikolaus von Reznicek","Samuel Augustus Ward","Benjamin Britten","Andre  Ernest Modeste Gretry","Ferde Grofe","Jean Sibelius","Vincent d' Indy","César Franck","Franz Schubert","Gioachino Rossini","Johann Hummel","Domenico Cimarosa","Franz Xaver Gruber","Arkady Dubensky","Alan Hovhaness","Gian Carlo Menotti","Paul Creston","Amilcare Ponchielli","Dmitri Kabalevsky","Percy Grainger","Walter Piston","Franz Liszt","Giovanni Gabrieli","Alexander Borodin","Jaromir Weinberger","Christopher Rouse","Isaac Albeniz","Alberto Ginastera","Kurt Weill","Arcangelo Corelli","Ermanno Wolf-Ferrari","Jacques Offenbach","Jr. & Josef Johann Strauss","Ernest Bloch","Edgard Varese","Darius Milhaud","Ralph Vaughan Williams","Bohuslav Martinu","Gabriel-Urbain Faure","Zoltán Kodaly","Nicolò Paganini","Anatoli Lyadov","Leone Sinigaglia","Gabriel Pierne","Emmerich Kalman","Henry F. Gilbert","Jules Massenet","Ernest Chausson","Erich Korngold","Franz Von Suppe","Sr. Johann Strauss","Victor Herbert","Elliott Carter","Franz Lehar","Charles Tomlinson Griffes","Otto Nicolai","Edward A. MacDowell","Paul Dukas","Lew Pollack","Giuseppe Verdi","Sigmund Romberg","David Diamond","Patty and Mildred Hill","Erno Dohnanyi","Georges Enesco","Albert Roussel","John Alden Carpenter","Rudolph Ganz","Irving Berlin","Francis Poulenc","Emmanuel Chabrier","Karol Szymanowski","Adolphe Charles Adam","Enrique Granados","Charles Gounod","Claudio Monteverdi","Frank Loesser","Marvin Hamlisch","Alexander Gretchaninoff","Benjamin Godard","Léo Delibes","Olivier Messiaen","Georg Philipp Telemann","Luigi Cherubini","Ernest Schelling","Alfredo Casella","Erik Satie","Henry Kimball Hadley","Jean-Philippe Rameau","Alexandre Tansman","Joaquin Turina","Alexander Scriabin","Karl Goldmark","Carl Nielsen","Vincenzo Bellini","Ernesto Lecuona","William Grant Still","Max Reger","Ilderbando Pizzetti","Pablo de Sarasate","Rubin Goldmark","Luigi Boccherini","Mikhail Ippolitov-Ivanov","Stephen Sondheim","Fritz Kreisler","Anton Rubinstein","Alphonse Charles Renaud de Vilbac","Henri Wieniawski","Eugene Goossens","Giacomo Meyerbeer","Engelbert Humperdinck","Frederic Chopin","Gustave Charpentier","Moritz Moszkowski","Dimitri Tiomkin","Emil Waldteufel","Rudolf Sieczynski","Ambroise Thomas","Ignacy Jan Paderewski","Antonin Dvorak","Johan Svendsen","György Ligeti","Gustav Holst","Jacques Ibert","Alfred Schnittke","Sir Arnold Bax","Giuseppe Martucci","Francesco Cilea","Walter Damrosch","Joachim Raff","Josef Strauss","Pietro Mascagni","Edward MacDowell","Henri Duparc","Arthur Seymour Sullivan","Daniel Gregory Mason","George Whitefield Chadwick","John Williams","Magnus Lindberg","Sofia Gubaidulina","Sir Charles Villiers Stanford","Francois  Couperin","Bernard Herrmann","Jacob Druckman","Louis Spohr","Leopold Damrosch","Niels Gade","Pierre Boulez","Luciano Berio","Hugo Wolf","Krzysztof Penderecki","Josef Stransky","Howard Hanson","Robert Stolz","Ferruccio Busoni","Ellen Taaffe Zwilich","Henri Dutilleux","Tylman Susato","Witold Lutoslawski","Nikos Skalkottas","Gerry Mulligan","Roger Machado"]
,
    maxResults = 5,
    input = document.querySelector('div.fuzzy-search-wrapper input'),
    output = document.querySelector('.fuzzy-search-wrapper ul'),
    demoItemOutput = document.getElementById('demoItems');

  function doFilterOptions() {
    var query = input.value,
      filterResult = [],
      i;

    // removing all child elements the easy way
    output.innerHTML = '';

    if (query === '') {
      return;
    }

    for (i = 0; i < options.length; i++) {
      filterResult[i] = fuzzy(options[i], query);
    }

    filterResult.sort(fuzzy.matchComparator);

    for (i = 0; i < filterResult.length && i < maxResults; i++) {
      var option = filterResult[i],
        el = document.createElement('li');

      el.dataset.score = option.score;
      el.dataset.term = option.term;
      el.innerHTML = option.highlightedTerm;

      if (i === 0) {
        el.classList.add('selected');
      }

      output.appendChild(el);
    }
  };

  function getSelectedNode() {
    return document.querySelector('.selected');
  };

  function useSelectedOption() {
    var selectedNode = getSelectedNode();
    if (selectedNode !== null) {
      compfind(selectedNode.dataset.term);
      input.value = '';
    }
  };

  function moveSelection(down) {
    var selectedNode = getSelectedNode(),
      newSelectedNode = null;

    if (down) {
      newSelectedNode = selectedNode.nextSibling;
    } else {
      newSelectedNode = selectedNode.previousSibling;
    }

    if (newSelectedNode !== null) {
      selectedNode.classList.remove('selected');
      newSelectedNode.classList.add('selected');
    }
  };

  input.addEventListener('keyup', function(e) {
    if (e.keyCode === 13) { // enter
      useSelectedOption();
      doFilterOptions();
    } else if (e.keyCode === 40) { // down
      moveSelection(true);
    } else if (e.keyCode === 38) { // up
      moveSelection(false);
    } else if (e.keyCode === 27) { // ESC
      output.innerHTML = '';
      input.value = '';
    } else {
      doFilterOptions();
    }
  }, false);
})();

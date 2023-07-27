/*
Tarea MongoDB
Alejandro Lema Fernández

Dataset: top 250 imdb movies
https://www.kaggle.com/datasets/ashishjangra27/imdb-top-250-movies?datasetId=2350899
*/

// 1. Cargar / Importar dataset --> con la función "Import"
db.imdbmovies.find() // 250 movies

// 2. Ejercicios sobre inserción, actualización, proyección y filtrado
db.imdbmovies.aggregate([ {$out: "pelis"} ]) // duplicamos para trabajar sobre pelis y así tener la database original siempre disponible

db.pelis.find()
db.pelis.find().sort({rank: -1})

// UPDATE (format)
/*
db.pelis.updateMany(                            // (*) $rename tmb sirve... si no se aplica en un pipeline
   { },                                         // select all
   [                                            // necesario [ ] para indicar que se pasa una lista de operaciones y no solo una
    { $set: { title: "$name" } },                // $addFields (*) --> se desordena el campo, necesario $project
    { $unset: [ "id", "img_link", "name" ] },    // borrar campos que no nos interesan: id, img_link
    { $project: { rank:1, title:1, year:1, imdb_votes:1, imdb_rating:1, certificate:1, duration:1, // reordenar
                genre:1, cast_id:1, cast_name:1, director_id:1, director_name:1, writter_id:1, writter_name:1}}
   ]
)
*/

db.pelis.find() // PROBLEMA: no se cumple el orden indicado en $project, entonces "title" se queda al final :(

// sin anidar
/*
db.pelis.updateMany(  // pasar campos de String a Array con sus valores separados, por ej genre
    { },
    [ {$set: { genre: { $split: ["$genre", ","] }}} ] // no sé pq pero los [ ] son necesarios, sino no me actualiza bien
    )

db.pelis.updateMany( // lo mismo pero con varios campos a la vez
    { },
    [
    {$set:{
    cast_id: { $split: ["$cast_id", ","] },
    cast_name: { $split: ["$cast_name", ","] },
    director_id: { $split: ["$director_id", ","] },
    director_name: { $split: ["$director_name", ","] },
    writter_name: { $split: ["$writter_name", ","] },
    writter_id: { $split: ["$writter_id", ","] }
    }
    ] // Aquí [ ] para indicar que se pasa una lista de operaciones y no solo una
)
*/

// anidado
/*
db.pelis.updateMany(
    { },      // select all
    [         // necesario [ ] para indicar que se pasa una lista de operaciones y no solo una
    { $set: {
        genres: { $split: ["$genre", ","] }, // reescribir genre
        cast: // cast tiene 2 subcampos
            {
            id: { $split: ["$cast_id", ","] },
            name: { $split: ["$cast_name", ","] }
            },
        director:
            {
            id: { $split: ["$director_id", ","] },
            name: { $split: ["$director_name", ","] }
            },
        writter:
            {
            id: { $split: ["$writter_id", ","] },
            name: { $split: ["$writter_name", ","] }
            }
        }
    },
    { $unset:
        ["genre", "cast_id", "cast_name", "director_id", "director_name", "writter_id", "writter_name"]
    ]
)*/

// ejemplo de uso: $zip & $out
/*
// poner cada id con su actor ($zip)
db.pelis.aggregate([
  {
    $addFields: {
      cast: {
        $zip: {
          inputs: ["$cast_id", "$cast_name"]
        }
      },
     director : { // nota: zip para director pq algunas pelis (27) tienen más de 1. Si todas tuviesen solo 1, podríamos hacerlo diferente
         $zip: {inputs: ["$director_id", "$director_name"]}
      },
     writter : {
        $zip: {inputs: ["$writter_id", "$writter_name"]}
      }
    }
  },
  {$project: {_id: 0, cast_id: 0, cast_name: 0, director_id: 0, director_name: 0, writter_id: 0, writter_name: 0}},
  {$out: "pelis_edit"} // guardar en nueva colección
]) // no sale resultado por el $out 

db.pelis_edit.find()
*/

// los dos updateMany anteriores se pueden poner en uno solo. Pero tenemos el problema de que, a pesar de $project, "title"" se queda al final.

/* SOLUCIÓN --> $replaceRoot sí lo hace bien
hace que la estructura de los documentos "raíz" sea igual a la estructura de un documento según newRoot (que o bien indicamos o bien definimos) */
/* (Nota: he visto que si redefines los campos en $project en lugar de "llamarlos", sí se cumple el orden indicado) */

db.pelis.updateMany(
   { }, // select all
   [
      {
         $replaceRoot: {
            newRoot: {
               rank: "$rank",
               title: "$name", // renombrar
               year: "$year",
               imdb_votes: "$imbd_votes", // corregir el nombre
               imdb_rating: "$imdb_rating",
               certificate: "$certificate",
               duration: "$duration",
               genres: { $split: ["$genre", ","] }, // reescribir genre
               cast: // cast tiene 2 subcampos
                {
                id: { $split: ["$cast_id", ","] },
                name: { $split: ["$cast_name", ","] }
                },
               director:
                {
                id: { $split: ["$director_id", ","] },
                name: { $split: ["$director_name", ","] }
                },
               writter:
                {
                id: { $split: ["$writter_id", ","] },
                name: { $split: ["$writter_name", ","] }
                }
            }
         }
      },
      { // no olvidar $unset
         $unset: [ "id","img_link","name","genre","cast_id","cast_name"
            "director_id","director_name","writter_id","writter_name"]
      }
   ]
)

db.pelis.find()

// Filtrado y proyección:

// - mejores películas de animación según la opinión popular (nº elevado de votos)
db.GhibliSupremacy.find(
    { genres : "Animation", imdb_votes : { $gt: 800000 } }, // query
    { _id: 0, title: 1, rank: 1, imdb_rating: 1, total_votes: "$imdb_votes" } // projection
    ).sort({rank: 1}) // ordenar por rank

// - pelis con 3 o más directores
db.pelis.find(
    { $expr: {$gte: [ { $size: "$director.name" }, 3] } }, // query
    { _id: 0, title: 1, directors: "$director.name" } ) // projection
// .count() // hay 27 películas con más de 1 director (23 con 2, y 4 con 3)


    // imdb_rating posible problema --> algunos son tipo Double y otros Int--> forzar todos a Double? NO! No es un problema, Mongo es flexible ;)

// INSERT
db.pelis.insertMany( // insertOne solo vale para añadir 1 docu
    [ // lista de documentos a insertar
      {
        rank: null, // omitimos intencionadamente algunos campos
        title: "Millennium Actress",
        year: 2001,
        imdb_vote: 29000,
        imdb_rating: 9.8,
        duration: 87,
        genres: [ "Drama", "Animation", "Fantasy" ],
        cast: [
          { id: 1234, name: "Miyoko Shoji" },
          { id: null, name: "Mami Koyama" } ],  // null
        director: [
          { id: "", name: "Satoshi Kon" } ], // ""
        writter: [
          { name: "Satoshi Kon" }, // no especificar el campo
          { name: "Sadayuki Murai" } ],
      },
      {
        rank: null,
        title: "From up on poppy hill",
        year: 2001,
        imdb_vote: 46000,
        imdb_rating: 9.7,
        duration: 91,
        genres: [ "Animation", "Comedy", "Drama" ],
        cast: [
          { id: null, name: "Masami Nagasawa" },
          { id: null, name: "Junichi Okada" },
          { id: 5678, name: "Miyoko Shoji" } ],
        director: [
          { id: "", name: "Goro Miyazaki" } ],
        writter: [
          { name: "Hayao Miyazaki" },
          { name: "Keiko Niwa" } ],
      }
    ]
)

db.pelis.find( { "director.name": "Satoshi Kon" } ) // comprobación
db.pelis.find( { "director.name": "Hayao Miyazaki" } ) // ya hay algunas de Ghibli!

// AGGREGATE

// Encontrar las pelis que no tengan un valor de rango asignado, y asignarlos según el imdb_rating

db.pelis.find().sort({rank: 1}) // echemos un vistazo...

// a) antes de agregar, cambiamos el imdb_rating a alguna peli, para poder luego hacer comprobación 

db.pelis.updateOne(
    { title: { $regex: /Chihiro.*/ } }, // ejemplo de filtrar con regex
    { $set: { imdb_rating : 10,
              title: "El viaje de Chihiro" } ) // tenía el nombre en japonés
    
db.pelis.find({ title: { $regex: /Chihiro.*/ } }) // comprobación

// b) ahora sí podemos reasignar el rank según el imdb_rating

db.pelis.aggregate([ // ejemplos de: match, sort, group, unwind, set, replaceRoot, sort, limit, out
  { $match: { imdb_rating: { $exists: true } } },               // (lo cumplen todos, pero lo pongo como ejemplo de $match y $exists)
  { $sort: { imdb_rating: -1 } },                               // reordenar
  { $group: { _id: null, pelis: { $push: "$$ROOT" } } },        // array con todas las pelis ordenadas
  { $unwind: { path: "$pelis", includeArrayIndex: "index" } },  // deshacemos el array, pero extrayendo el index, que es lo que queremos
  { $set: { "pelis.rank": { $toInt: { $add: [ "$index", 1 ] } } } },        // actualizamos el rank (el $add +1 es para empezar en 1 y no en 0)
  { $replaceRoot: { newRoot: "$pelis" } },                                  // el $toInt es pq estaba devolviendo tipo Long, y lo forzamos a Int
/* esto hace que la estructura de los documentos "raíz" sea igual a la estructura de un documento "$peli", que no es otra cosa que la estructura "raíz"
original; es decir, volvemos a la estructura original (se pierde el campo index, pero eso nos viene bien), pero ya con el campo rank actualizado ;) */
  { $sort: {rank: 1}},        // reordenar por rank
  { $limit : 250 },           // mantenemos la database como las 250 mejores películas
  { $out: "GhibliSupremacy" } // guardamos la lista ordenada en una nueva colección (ya que aggregate no modifica realmente la colección original)
]) //abajo repito el código SIN COMENTARIOS

db.GhibliSupremacy.find() // haremos consultas sobre esta colección

db.pelis.aggregate([
  { $match: { imdb_rating: { $exists: true } } },
  { $sort: { imdb_rating: -1 } },
  { $group: { _id: null, pelis: { $push: "$$ROOT" } } },
  { $unwind: { path: "$pelis", includeArrayIndex: "index" } },
  { $set: { "pelis.rank": { $toInt: { $add: [ "$index", 1 ] } } } },
  { $replaceRoot: { newRoot: "$pelis" } },
  { $sort: {rank: 1}},
  { $limit : 250 },
  { $out: "GhibliSupremacy" }
])

//--------------------------------------------------------------------------------------

// - personas que han participado en los 3 posibles roles (actor, director, escritor) a la vez en una película. Mostrar la persona y las películas
db.GhibliSupremacy.aggregate([
    { $unwind: "$cast.name" },
    { $unwind: "$director.name" },
    { $unwind: "$writter.name" },
    { $match: {
      $expr: {
         $and: [
          { $eq: [ "$cast.name", "$director.name" ] }, // no te líes, recuerda debidamente lo que hace $unwind
          { $eq: [ "$director.name", "$writter.name" ] }
          ]
      } }
    },
    { $group: { _id: { actor: "$cast.name", director: "$director.name", writter: "$writter.name" }, movie: { $push: "$title" } } },
    { $project: {
        _id: 0, 
        person: "$_id.actor",
        movies: "$movie"
    } } 
])

/* - Variación de la anterior, pero más difícil: personas que han participado en películas en al menos 2 de los 3 posibles roles
    mostrar la persona, y embebidos: cada película y sus roles en ella */

/*
// Alternativa al $or --> uso ingenioso de $in (incluye 2 iguales pero tmb los 3 iguales!)
    { $match: {
      $expr: {
         $or: [
          { $in: [ "$actor", ["$director", "$writter"] ] },
          { $in: [ "$director", ["$actor", "$writter"] ] },
        // { $in: [ "$writter", ["$actor", "$director"] ] } // esta línea ya está implícita en las dos anteriores ;)
          ]
      } }
    }
*/

// LO LOGRÉ :D (diseñar esta consulta me ha llevado más de 5 horas...)
db.GhibliSupremacy.aggregate([
    { $unwind: "$cast.name" },
    { $unwind: "$director.name" },
    { $unwind: "$writter.name" },
    { $match:
      { $expr: { $or: [
        { $eq: [ "$cast.name", "$director.name" ] }, // (*) se puede hacer con $in en 2 líneas (ver arriba)
        { $eq: [ "$director.name", "$writter.name" ] },
        { $eq: [ "$cast.name", "$writter.name" ] },
      ] } }
    },
    { $set: {
        person: { $cond: {
                      if: { $or: [ { $eq: ["$cast.name", "$director.name"] }, { $eq: ["$cast.name", "$writter.name"] } ] }, then: "$cast.name",
                      else: "$director.name" },  // si no es el actor con otro, es el director con writter, así que ponemos aquí director y listo
                }
    } },
    { $group: {_id: { person: "$person", movie: "$title" },
                actors: { $push: "$cast.name"},
                directors: { $push: "$director.name"},
                writters: { $push: "$writter.name"}
            }   
    }, // hasta aquí, tenemos seleccionadas las 181 pelis en que ocurre lo de 2 o 3 roles. Falta añadir los roles
    { $project: {
        _id: 0,
        person: "$_id.person",
        movie: "$_id.movie"
        roles: {
            $map: { // este $map no es realmente necesario, pero así pongo un ejemplo de su uso
                input: [
                   { $cond: { if: { $in: ["$_id.person", "$actors"] }, then: "Actor", else: null } },
                   { $cond: { if: { $in: ["$_id.person", "$directors"] }, then: "Director", else: null } },
                   { $cond: { if: { $in: ["$_id.person", "$writters"] }, then: "Writer", else: null } }
                ],
                as: "role",
                in: "$$role"
            }
        }
    }},
    { $sort: { person: 1} }
    // { $match: { roles: { $ne: null } }, // para mostrar los que tienen los 3 roles (tmb podríamos $group by movie)
])

// repito código pero más estrecho, para el pdf de la entrega:
db.GhibliSupremacy.aggregate([
    { $unwind: "$cast.name" },
    { $unwind: "$director.name" },
    { $unwind: "$writter.name" },
    { $match:
      { $expr: { $or: [
        { $eq: [ "$cast.name", "$director.name" ] },
        { $eq: [ "$director.name", "$writter.name" ] },
        { $eq: [ "$cast.name", "$writter.name" ] },
      ] } }
    },
    { $set: {
        person: { $cond: {
                      if: { $or: [
                                  { $eq: ["$cast.name", "$director.name"] },
                                  { $eq: ["$cast.name", "$writter.name"] }
                                ] },
                      then: "$cast.name",
                      else: "$director.name" },
                }
    } },
    { $group: {_id: { person: "$person", movie: "$title" },
                actors: { $push: "$cast.name"},
                directors: { $push: "$director.name"},
                writters: { $push: "$writter.name"}
            }   
    },
    { $project: {
        _id: 0,
        person: "$_id.person",
        movie: "$_id.movie"
        roles: {
            $map: {
                input: [
                   { $cond: { if: { $in: ["$_id.person", "$actors"] },
                            then: "Actor", else: null } },
                   { $cond: { if: { $in: ["$_id.person", "$directors"] },
                            then: "Director", else: null } },
                   { $cond: { if: { $in: ["$_id.person", "$writters"] },
                            then: "Writer", else: null } }
                ],
                as: "role",
                in: "$$role"
            }
        }
    }},
    { $sort: { person: 1} }
])



//--------------------------------------------------------------------------------------
// 3. Ejercicios sobre pipeline de agregación

// $match, $group, $project, $sort, $limit, $skip, $lookup
// $merge...

/* 1) cuántos géneros diferentes hay? Ordénalos de mayor a menor nº apariciones y, en caso de empate, alfabéticamente (fijarse por ej en
appearances = 23). Incluir las 3 mejores películas de cada género */

db.GhibliSupremacy.aggregate([                  // (vemos que hay 21 resultados en la barra negra de info del resultado ;)
    { $match: {rank: { $ne: null } } },         // (*) asegurarnos que solo se consideran pelis con un valor de rank válido
    { $unwind: "$genres" },
    { $sort: {rank : 1} },                      // (*) ordenar las pelis por rank, para luego mostrar las 3 mejores
    { $group: { _id: "$genres", appearances: {$sum: 1},
        movies: { $push: { title: "$title", rank: "$rank" } } } },
    { $project:
        { _id: 0, genre: "$_id", appearances: "$appearances",
            best_movies: { $slice: [ "$movies", 3 ] } }
    },                                          // ejemplo de $slice (como limit pero en arrays)
    { $sort: { appearances: -1, genre: 1 } }    // se pueden poner varias condiciones en $sort, por orden de prioridad
])                                              // los pasos marcados con (*) no son necesarios en nuestro dataset particular

// modificación 1:
    // por ser didácticos, se muestra cómo podríamos agrupar todo en un único docu con los campos numGenres = 21 y allGenres = [array_de_genres]
    // { $group: { _id: null, numGenres: { $sum: 1 }, allGenres: { $push: "$_id" } } }, //como ya están ordenados, se colocan ordenados dentro del Array :D
    // { $project: { _id: 0 } }
 
// 2) Actores con más géneros diferentes (5 actores, mostrar los géneros en los que han participado)
db.GhibliSupremacy.aggregate([
    { $unwind: "$genres" },
    { $unwind: "$cast.name" },
    { $group: { _id: { actor: "$cast.name", genre: "$genres" } } }, // agrupar por cada combinación de actor y género
    { $group: {
        _id: "$_id.actor", numgenres: { $sum: 1 },                  // agrupar por actor, contar sus géneros
        genres: { $push: "$_id.genre" }                             // incluir los géneros con $push
        } },
    { $project : {_id : 0, actor: "$_id",
        numgenres: "$numgenres", genres: "$genres"} },
    { $sort: { numgenres: -1 } },
    { $limit: 5 }
])

db.GhibliSupremacy.find()

// 3) Actores con más películas y mostrar la mejor película en la que han participado
db.GhibliSupremacy.aggregate([  
    { $unwind: "$cast.name" },
    { $group: { _id: "$cast.name", numMovies: { $sum: 1 },
            movies: { $push: { title: "$title", rank: "$rank" } } } },
    { $sort: { numMovies: -1 } },
    { $project: { _id: 0, actor: "$_id", numMovies: "$numMovies", 
        best_movie: { 
            $filter: {
              input: "$movies",
              as: "movie",
              cond: { $eq: [ "$$movie.rank", { $min: "$movies.rank" } ] }
            }
        }
      }
    }
])






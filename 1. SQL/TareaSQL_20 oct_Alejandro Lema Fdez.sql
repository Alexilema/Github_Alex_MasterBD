/* Tarea MySQL Workbench
Creado por Alejandro Lema Fernández
Entrega: 20 oct 2022
Casos de corrupción */

-- resetear la base de datos:
DROP DATABASE IF EXISTS CasosCorrupcion;

CREATE DATABASE CasosCorrupcion;
USE CasosCorrupcion;

/*DROP TABLE IF EXISTS afiliado_cargo;
DROP TABLE IF EXISTS relacion_caso_implicado;
DROP TABLE IF EXISTS ambitoCaso;
DROP TABLE IF EXISTS telefono;
DROP TABLE IF EXISTS familia;
DROP TABLE IF EXISTS implicado;
DROP TABLE IF EXISTS caso;
DROP TABLE IF EXISTS periodico;
DROP TABLE IF EXISTS juez;
DROP TABLE IF EXISTS partido;
*/

-- PASO 1: PASO A TABLAS DEL MODELO RELACIONAL:

-- nota: crear primero las tablas que NO tienen foreign keys, para que no nos dé el error de que intenta llamar a una tabla que no existe

CREATE TABLE partido_1 (
	nombrePartido varchar(20),
	direccion varchar(40),
    ciudad varchar(20),
	 PRIMARY KEY(nombrePartido)
);

CREATE TABLE juez_2 (
	CodJuez char(3),
	nombreJuez varchar(30) not null,
	ape1 varchar(30) not null,
	ape2 varchar(30),
	direccion varchar(40),
	ciudad varchar(20),
	fechaNacim date, -- fmto de date es: año-mes-día
	fechaInicio date,
	 PRIMARY KEY(CodJuez)
);
  
CREATE TABLE periodico_3 (
	CodPeriodico int AUTO_INCREMENT,
    nombrePeriodico varchar(20) not null,
    direccion varchar(40),
    ciudad varchar(20),
    tipo varchar(7),
    web varchar(40),
    ambitoPeriodico varchar(15),
    partido_afin varchar(20),
	 PRIMARY KEY(CodPeriodico),
		FOREIGN KEY(partido_afin) REFERENCES partido_1(nombrePartido)
);

CREATE TABLE caso_4 (
	CodCaso int AUTO_INCREMENT,
	nombreCaso varchar(20) not null UNIQUE,
    idJuez char(3) NOT NULL,
	descripcion varchar(400),
	millonesDesv numeric(12,0),
	descubiertoPor int,
	fechaDescubrim date,
    Cdictamen varchar(400) DEFAULT '',
	 PRIMARY KEY(CodCaso),
		FOREIGN KEY(idJuez) REFERENCES juez_2(CodJuez),
		FOREIGN KEY(descubiertoPor) REFERENCES periodico_3(CodPeriodico)
);

CREATE TABLE implicado_5 (
	DNI_Implic char(9),
	nombreImplic varchar(30) not null,
	ape1 varchar(30) not null,
	ape2 varchar(30),
	direccion varchar(60),
	ciudad varchar(20),
	patrimonio numeric(12,0),
	 PRIMARY KEY(DNI_Implic)
);

CREATE TABLE familia_6 (
	idImplic char(9),
	parentesco varchar(20),
    idFamiliar char(9),
	 PRIMARY KEY(idImplic, idFamiliar),
		FOREIGN KEY(idImplic) REFERENCES implicado_5(DNI_Implic) on delete CASCADE,
		FOREIGN KEY(idFamiliar) REFERENCES implicado_5(DNI_Implic) on delete CASCADE     
);

CREATE TABLE telefono_7 (
	contacto varchar(20),
    tlfno numeric (9,0),
	 PRIMARY KEY(contacto, tlfno),
		FOREIGN KEY(contacto) REFERENCES partido_1(nombrePartido) on delete CASCADE
);

CREATE TABLE ambitoCaso_8 (
	idCaso int,
    ambito varchar(20),
	 PRIMARY KEY(idCaso, ambito),
		FOREIGN KEY(idCaso) REFERENCES caso_4(CodCaso) on delete CASCADE
);

CREATE TABLE relacion_caso_implicado_9 (
	idCaso int,
    NIF_Implic char(9),
	 PRIMARY KEY(idCaso, NIF_Implic),
		FOREIGN KEY(idCaso) REFERENCES caso_4(CodCaso) on delete CASCADE,
		FOREIGN KEY(NIF_Implic) REFERENCES implicado_5(DNI_Implic) on delete CASCADE -- tengo dudas con éste CASCADE
);

-- nota: siguiendo el consejo de la profe, pondré el partidoAfil en la tabla afiliado_cargo_10 en lugar de en tabla implicado_5
CREATE TABLE afiliado_cargo_10 (
	NIF_implic char(9),
    partidoAfil varchar(20) not null,
	cargoPolitico varchar(40) DEFAULT '',
    PRIMARY KEY(NIF_implic),
		FOREIGN KEY(NIF_implic) REFERENCES implicado_5(DNI_Implic) on delete CASCADE,
        FOREIGN KEY(partidoAfil) REFERENCES partido_1(nombrePartido)
);

/* NOTA: para sacar el modelo relacional con ingeniería inversa:
seleccionas la base de datos a la izq, en Schemas --> casoscorrupcion
arriba, pulsas en Database --> Reverse Engineer
(...) cuando tengas el modelo relacional, pulsas en File --> export --> export as PNG
*/

-- PASO 2: ALMACENAR LOS DATOS EN LAS TABLAS: #############################################################################################################

/* nota para el alumno: el orden de todas las acciones es importante, por ej, al intentar rellenar las tablas, si intentas rellenar caso sin tener periodico 
relleno, te dará error, porque no encuentra con qué llenar la foreign key
nota 2: los datos que se tengas correctamente dispuestos en archivos .csv, pueden cargarse con el Import Wizard (y es super fácil y no da el endiablado error 1452...)
*/

-- Forma 1: insert into
	-- tabla 1:
INSERT INTO partido_1 VALUES ('PP','Calle Génova 13, 28004','Madrid');
INSERT INTO partido_1 VALUES ('PSOE','Calle Ferraz 70, 28008','Madrid');
INSERT INTO partido_1 VALUES ('Ciudadanos','Calle Pantomima 4, 28000','Madrid');
	-- tabla 2:
INSERT INTO juez_2 VALUES('001','José','Castro','Aragón','Av. de Jaume III, 26, 07012','Palma de Mallorca','1947-12-20','1974-10-23');
INSERT INTO juez_2 VALUES('002','Eloy','Velasco','Núñez','Calle de Albuquerque, 3, 28010','Madrid','1963-01-02','1988-04-01');
INSERT INTO juez_2 VALUES('003','Baltasar','Garzón','Real','Paseo de los Melancólicos, 12, 28005','Madrid','1955-10-26','1979-03-31');
	-- tabla 3:
INSERT INTO periodico_3(nombrePeriodico,direccion,ciudad,tipo,web,ambitoPeriodico,partido_afin) -- debido al auto_increment, podemos hacerlo así o como abajo con el null
	VALUES ('El País','Calle Miguel Yuste 40, 28037','Madrid','papel','www.elpais.com','nacional','PSOE');
INSERT INTO periodico_3 VALUES (null,'El Mundo','Avenida de San Luis, 25, 28033','Madrid','papel','www.elmundo.es','nacional','PP');
INSERT INTO periodico_3 VALUES (null,'ABC','Calle de Josefa Valcárcel, 40 bis, 28027','Madrid','papel','www.abc.es','nacional','PP');

-- Forma 2: load data
-- directorio donde guardar los archivos de datos:
-- SELECT @@GLOBAL.secure_file_priv;
	-- tabla 4:
LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/casos_corrupcion.csv'
	INTO TABLE caso_4
    CHARACTER SET latin1
    FIELDS TERMINATED BY ';'
    LINES TERMINATED BY '\n'
    IGNORE 1 ROWS
    (nombreCaso,idJuez,descripcion,millonesDesv,descubiertoPor,fechaDescubrim,Cdictamen) -- hay que poner esto porque CodCaso no lo da el fichero, se genera con el auto_increment
;
/* nota para el alumno: si en el fichero .csv no relleno el campo Cdictamen, me da un error de que tal fila no tiene suficientes campos
No entiendo que pase esto, porque al poner "blabla;" en lugar de "blabla" , ya le estoy diciendo que ahí hay un campo cuyo contenido es null
En fin, escribiendo un "espacio" en blanco, ya no me da ese problema...
También se soluciona poniendo DEFAULT '' en el atributo al crear la tabla ;)
*/

	-- tabla 5:
LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/implicados.csv'
	INTO TABLE implicado_5
    CHARACTER SET latin1
    FIELDS TERMINATED BY ';'
    LINES TERMINATED BY '\n'
    IGNORE 1 ROWS
;

    -- tabla 6:
INSERT INTO familia_6 VALUES('30228572D', 'mujer', '11708734H');
INSERT INTO familia_6 VALUES('11708734H', 'marido', '30228572D');
INSERT INTO familia_6 VALUES('44544403T', 'hermano', '46294768K');
INSERT INTO familia_6 VALUES('46294768K', 'hermano', '44544403T');
INSERT INTO familia_6 VALUES('29076564P', 'padre', '82773010L');
INSERT INTO familia_6 VALUES('82773010L', 'hija', '29076564P');

-- tabla 7:
LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/telefonos.csv'
	INTO TABLE telefono_7
    CHARACTER SET latin1
    FIELDS TERMINATED BY ';'
    LINES TERMINATED BY '\n'
    IGNORE 1 ROWS
;

INSERT INTO ambitocaso_8 VALUES ('1','Comunidad');
INSERT INTO ambitocaso_8 VALUES ('1','Estado');
INSERT INTO ambitocaso_8 VALUES ('2','Comunidad');
INSERT INTO ambitocaso_8 VALUES ('2','Estado');
INSERT INTO ambitocaso_8 VALUES ('3','ONG');
INSERT INTO ambitocaso_8 VALUES ('3','Comunidad');
INSERT INTO ambitocaso_8 VALUES ('4','Caja');
INSERT INTO ambitocaso_8 VALUES ('4','Estado');
INSERT INTO ambitocaso_8 VALUES ('5','Ayuntamiento');
INSERT INTO ambitocaso_8 VALUES ('5','Banco');
INSERT INTO ambitocaso_8 VALUES ('5','Comunidad');

	-- tabla 9:
INSERT INTO relacion_caso_implicado_9 VALUES ('1','70878435Z');
INSERT INTO relacion_caso_implicado_9 VALUES ('1','41672062C');
INSERT INTO relacion_caso_implicado_9 VALUES ('2','58822985G');
INSERT INTO relacion_caso_implicado_9 VALUES ('2','91791795T');
INSERT INTO relacion_caso_implicado_9 VALUES ('3','30228572D');
INSERT INTO relacion_caso_implicado_9 VALUES ('3','11708734H');
INSERT INTO relacion_caso_implicado_9 VALUES ('3','44544403T');
INSERT INTO relacion_caso_implicado_9 VALUES ('4','29076564P');
INSERT INTO relacion_caso_implicado_9 VALUES ('4','82773010L');
INSERT INTO relacion_caso_implicado_9 VALUES ('5','46294768K');
INSERT INTO relacion_caso_implicado_9 VALUES ('5','43624524S');
INSERT INTO relacion_caso_implicado_9 VALUES ('5','70878435Z');
INSERT INTO relacion_caso_implicado_9 VALUES ('5','41672062C');
INSERT INTO relacion_caso_implicado_9 VALUES ('5','91791795T');

	-- tabla 10:
INSERT INTO afiliado_cargo_10 VALUES ('70878435Z','PP','Presidenta PP Madrid');
INSERT INTO afiliado_cargo_10 VALUES ('44544403T','PP','Diputado');
INSERT INTO afiliado_cargo_10 VALUES ('41672062C','PP',null);
INSERT INTO afiliado_cargo_10 VALUES ('58822985G','PP','Miembro PP Valencia');
INSERT INTO afiliado_cargo_10 VALUES ('91791795T','PSOE','Secretario');
INSERT INTO afiliado_cargo_10 VALUES ('29076564P','PP','Tesorero');
INSERT INTO afiliado_cargo_10 VALUES ('82773010L','PP',null);
INSERT INTO afiliado_cargo_10 VALUES ('46294768K','Ciudadanos','Diputado');
INSERT INTO afiliado_cargo_10 VALUES ('43624524S','Ciudadanos',null);

/*LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/afiliados.csv'
	INTO TABLE afiliado_cargo_10
    CHARACTER SET latin1
    FIELDS TERMINATED BY ';'
    LINES TERMINATED BY '\n'
    IGNORE 1 ROWS
; -- PROBLEMA: si escribo un espacio en blanco en el .csv, ya no me da error de que no hay datos en las columnas. Lo que pasa es que entonces no me rellena con null, sino con un espacio en blanco, y no es lo mismo
*/

/* -- nota alumno: podríamos evitar el error 1452 de esta forma (aunque no es recomendable hacer esto, pues puedes estar cometiendo un error y no darte cuenta...):
SET FOREIGN_KEY_CHECKS=0;
LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/implicados.csv'
	INTO TABLE implicado_5
    CHARACTER SET latin1
    FIELDS TERMINATED BY ';'
    LINES TERMINATED BY '\n'
    IGNORE 1 ROWS
;
SET FOREIGN_KEY_CHECKS=1;
*/

-- PAS0 3: REALIZAR CONSULTAS: #############################################################################################################

-- nota: cada consulta está indicada con un número seguido de un paréntesis. En algunas consultas, he escrito varias consultas previas para "pensar" cómo hacer la consulta final.

-- Las primeras 6 consultas son las que se mencionan en en enunciado de la tarea:

-- 1) total de implicados que son familia de otro/s implicados:
select count(idImplic)
from familia_6;

-- 2) Consultar la ciudad con más implicados:
select ciudad, count(DNI_Implic)
from implicado_5
group by ciudad
order by count(DNI_Implic) desc
limit 1;
/* el problema de dejarlo así es que si hay 2 o más ciudades empatadas con el mayor número, esto solo nos dice una!!
pero esto nos sirve para encontrar el numImplic máx en una de esas ciudades, luego solo tenemos que pedir
que nos muestre todas las que tengan ese número: */
select ciudad, count(DNI_Implic) as numImplic
from implicado_5
group by ciudad
having numImplic = (select count(DNI_Implic)
					from implicado_5
                    group by ciudad
					order by count(DNI_Implic) desc
					limit 1);

-- Nota: de forma similar, podríamos consultar el máx/mín de ciertas cosas. Por ej, el juez/jueces con más casos investigados (o menos casos, si usamos order by asc)

-- 3) total de dinero robado por cada partido:
-- paso 1: agrupo los implicados que sí tienen cargo político
select partidoAfil as partido, NIF_implic as político
from afiliado_cargo_10
where cargoPolitico != ' ' or cargoPolitico != null;
-- aquí necesito poner ' ' además de null debido a la forma en que escribí los datos en el .csv (para que no me diera problemas el LOAD DATA)

-- paso 2: consideraremos dinero robado por un partido siempre que un implicado en el caso tenga un cargo en ese partido
/* tenemos que relacionar caso_4 con relación_9, y a su vez mantener la relación con cargo_10;
esto me saca una tabla con los implic que son políticos, el partido al que pertenecen, y los millones robados en el caso en el que están implicados
*/
select partidoAfil as partido, af.NIF_implic as DNI, millonesDesv as millonesRobados
from afiliado_cargo_10 as af join relacion_caso_implicado_9 as rel on af.NIF_implic = rel.NIF_Implic
	join caso_4 as c on rel.idCaso = c.CodCaso
where cargoPolitico != ' ' or cargoPolitico != null;

-- paso 3: quito los DNIs, pues quiero agrupar por partido el dinero (usando sum):
select partidoAfil as partido, sum(millonesDesv) as millonesRobados
from afiliado_cargo_10 as af join relacion_caso_implicado_9 as rel on af.NIF_implic = rel.NIF_Implic
	join caso_4 as c on rel.idCaso = c.CodCaso
where cargoPolitico != ' ' or cargoPolitico != null
group by partidoAfil;

-- Nota: las siguientes consultas 4 y 5 son análogas a la número 2; solo las pongo porque se mencionan en el enunciado de la tarea (pero no las cuento de cara a las 15 consultas requeridas en el mismo)

-- 4) periódico con más casos descubiertos:
select distinct nombrePeriodico, count(CodCaso) as numCasosDescub 
from periodico_3 join caso_4 on CodPeriodico = descubiertoPor
group by CodPeriodico
having numCasosDescub = (select count(CodCaso)
					from caso_4
                    group by descubiertoPor
					order by count(CodCaso) desc
					limit 1);

-- 5) año en que se descubrieron más casos:
select year(fechaDescubrim) as año, count(CodCaso) as numCasos
from caso_4
group by year(fechaDescubrim)
having numCasos = (select count(CodCaso)
					from caso_4
                    group by year(fechaDescubrim)
					order by count(CodCaso) desc
					limit 1);  

-- 6) número de implicados en cada caso:
select nombreCaso, count(DNI_Implic) as totalImplicados
from caso_4 as c join relacion_caso_implicado_9 as rel on c.CodCaso = rel.idCaso
	join implicado_5 as i on rel.NIF_Implic = i.DNI_Implic
group by nombreCaso
order by totalImplicados desc;

-- Hasta aquí las consultas mencionadas en el enunciado de la tarea.
-- A continuación se realizan otras consultas adicionales.

-- 7) todos los jueces que investiguen al menos 2 casos:
select CodJuez as Juez, count(CodCaso) as 'nº casos investigados'
from caso_4 join juez_2 on idJuez = CodJuez
group by CodJuez;

-- 8) implicados que participan en más de 1 caso:
select NIF_Implic as Implicado, count(idCaso)
from relacion_caso_implicado_9
group by NIF_Implic
having count(idCaso) > 1;

-- Ya lo tenemos, pero para poner los nombres en la tabla y no solo los DNIs, hacemos este join:
select CONCAT_WS(' ',nombreImplic,ape1,ape2) as Implicado, count(idCaso)
from relacion_caso_implicado_9 join implicado_5 on NIF_Implic = DNI_Implic
group by NIF_Implic
having count(idCaso) > 1;

-- 9) Casos descubiertos por los periodicos afines a cierto partido (por ej al PP):
select nombreCaso as Caso, nombrePeriodico as 'Periódico que lo descubrió', partido_afin as 'Partido vinculado'
from caso_4 join periodico_3 on descubiertoPor = CodPeriodico
where partido_Afin = 'PP';

-- 10) implicados que son familiares de algún otro implicado y partido al que están afiliados en cada caso
-- paso 1: implicados y partido al que están afiliados en cada caso
-- --> ejemplo de left join:
select nombreCaso as Caso, CONCAT_WS(' ',nombreImplic,ape1,ape2) as Implicado, nombrePartido as 'Afiliado a'
from caso_4 as c join relacion_caso_implicado_9 as rel on c.CodCaso = rel.idCaso
	join implicado_5 as i on rel.NIF_Implic = i.DNI_Implic
    left join afiliado_cargo_10 as af on i.DNI_Implic = af.NIF_implic
	left join partido_1 as p on p.nombrePartido = af.partidoAfil
order by nombreCaso asc, nombreImplic asc;

-- añadir condición "y que son familiares de algún otro implicado"
select nombreCaso as Caso, CONCAT_WS(' ',nombreImplic,ape1,ape2) as Implicado,
		CONCAT_WS(' ',nombreImplic,ape1,ape2) as familiar, parentesco, nombrePartido as 'Afiliado a'
from caso_4 as c join relacion_caso_implicado_9 as rel on c.CodCaso = rel.idCaso
	join implicado_5 as i on rel.NIF_Implic = i.DNI_Implic
    left join afiliado_cargo_10 as af on i.DNI_Implic = af.NIF_implic
	left join partido_1 as p on p.nombrePartido = af.partidoAfil
		join familia_6 on idImplic = DNI_Implic
order by nombreCaso asc, nombreImplic asc;

-- variante 10.1) o si quisiéramos saber de ciertos casos en particular (pero sin limitar que sean familiares)
-- (se añade la condición del where):
select nombreCaso as Caso, CONCAT_WS(' ',nombreImplic,ape1,ape2) as Implicado, nombrePartido as 'Afiliado a'
from caso_4 as c join relacion_caso_implicado_9 as rel on c.CodCaso = rel.idCaso
	join implicado_5 as i on rel.NIF_Implic = i.DNI_Implic
    left join afiliado_cargo_10 as af on i.DNI_Implic = af.NIF_implic
	left join partido_1 as p on p.nombrePartido = af.partidoAfil
where nombreCaso = 'Bankia' or nombreCaso = 'Noós'
order by nombreCaso asc, nombreImplic asc;

-- 11) jueces que investigan casos en los que hay miembros del partido X:
-- paso 1: sacamos los casos que investiga cada juez
select CodJuez, nombreJuez, nombreCaso -- , partidoAfil
from juez_2 join caso_4 on idJuez = CodJuez
order by CodJuez asc;

-- paso 2: escogemos solo los casos que tienen implicados que son afiliados al partido que indiquemos:
select CodJuez, nombreJuez, nombreCaso, 'PP' as 'Partido'
from juez_2 join caso_4 on idJuez = CodJuez
having nombreCaso in (select nombreCaso -- (*)
					from caso_4 as c join relacion_caso_implicado_9 as rel on c.CodCaso = rel.idCaso
						join afiliado_cargo_10 as af on af.NIF_implic = rel.NIF_Implic
                        where partidoAfil = 'PP'
                        group by nombreCaso)
order by CodJuez asc;
-- (*) necesario poner "in" y no "=" porque esta subquery puede devolver varias filas, y por tanto el "=" nos daría el error 1242


-- 12) implicados que han participado solamente en casos de ámbito 'Banco':

-- nota: hago el ejemplo con Banco porque es el caso de mis datos que refleja la dificultad de esta consulta,
-- ya que el caso con ámbito Banco tiene también otros ámbitos, y esto hace que la lógica de la consulta
-- en Workbench no sea trivial (o yo al menos he estado devanándome los sesos durante más de 2 horas...)

-- paso 1: sacamos los implicados de casos con ámbito 'Banco':
select NIF_Implic, CodCaso, ambito
from relacion_caso_implicado_9 as rel join caso_4 on rel.idCaso = CodCaso
	join ambitocaso_8 as ac on CodCaso = ac.idCaso
where ambito = 'Banco'; 

-- -- -- paso 2:
-- (a) esto me dice los casos que tienen ALGÚN ámbito distinto de Banco (OJO, pueden también tener Banco)
select CodCaso
from caso_4 join relacion_caso_implicado_9 as rel on rel.idCaso = CodCaso
	join ambitocaso_8 as ac on CodCaso = ac.idCaso
where ambito != 'Banco'
group by CodCaso;

-- (b) esto me dice los casos que SÍ tienen ámbito 'Banco':
select CodCaso
from caso_4 join relacion_caso_implicado_9 as rel on rel.idCaso = CodCaso
	join ambitocaso_8 as ac on CodCaso = ac.idCaso
where ambito = 'Banco'
group by CodCaso;

-- (c) cogiendo a) excepto b), elijo los casos que NO tienen ámbito Banco:
select CodCaso
from caso_4 join relacion_caso_implicado_9 as rel on rel.idCaso = CodCaso
	join ambitocaso_8 as ac on CodCaso = ac.idCaso
where ambito != 'Banco' and CodCaso not in (select CodCaso
from caso_4 join relacion_caso_implicado_9 as rel on rel.idCaso = CodCaso
	join ambitocaso_8 as ac on CodCaso = ac.idCaso
where ambito = 'Banco'
group by CodCaso)
group by CodCaso;

-- paso 3: combinamos todo y listo:
select NIF_Implic, CodCaso, ambito
from relacion_caso_implicado_9 as rel join caso_4 on rel.idCaso = CodCaso
	join ambitocaso_8 as ac on CodCaso = ac.idCaso
where ambito = 'Banco'
having NIF_Implic not in (select NIF_Implic
					from relacion_caso_implicado_9 as rel join caso_4 on rel.idCaso = CodCaso
						join ambitocaso_8 as ac on CodCaso = ac.idCaso
                    where CodCaso in ( 
										select CodCaso
										from caso_4 join relacion_caso_implicado_9 as rel on rel.idCaso = CodCaso
											join ambitocaso_8 as ac on CodCaso = ac.idCaso
										where ambito != 'Banco' and CodCaso not in (select CodCaso
										from caso_4 join relacion_caso_implicado_9 as rel on rel.idCaso = CodCaso
											join ambitocaso_8 as ac on CodCaso = ac.idCaso
										where ambito = 'Banco'
										group by CodCaso)
                                        )
						)
;

-- 13) Implicados que solo pertenecen a un caso:
select DNI_Implic, idCaso
from implicado_5 join relacion_caso_implicado_9 as rel on DNI_Implic = rel.NIF_Implic
group by DNI_Implic
having count(NIF_Implic) < 2
order by idCaso asc;

-- 14) Casos descubiertos entre el 2000 y el 2010 (con el periodico por el que fueron descubiertos):
select nombreCaso as Caso, year(fechaDescubrim) as Año, nombrePeriodico as 'Periódico que lo descubrió'
from caso_4 join periodico_3 on descubiertoPor = CodPeriodico
where year(fechaDescubrim) between 2010 and 2016;

-- 15) decir los OTROS casos investigados por el juez que investiga el caso Púnica:
select nombreJuez as 'Juez del caso Púnica', CodJuez, nombreCaso as 'Otros casos que investiga'
from caso_4, juez_2
where idJuez = (select idJuez from caso_4 where nombreCaso = 'Púnica') and CodJuez = idJuez and nombreCaso != 'Púnica'; -- quitamos el caso que le hemos dicho, porque nos interesan los OTROS casos

-- 16) jueces que investigan menos casos que el juez de CodJuez = 001:

-- paso 1: para saber cuántos casos investiga, cojo la fila del juez con el idJuez, y hago count(CodCaso),
-- ya que estoy aplicando count() a un nº de filas igual al número de casos que lleva el juez
select a1.idJuez 'Juez', count(a1.CodCaso) as "nº casos que investiga"
from caso_4 as a1
where a1.idJuez = '001';

-- paso 2: sacar cuántos casos lleva cada juez
select nombreJuez as Juez, count(CodCaso) 'nº casos'
from caso_4 join juez_2 on idJuez = CodJuez
group by idJuez;

-- paso 3: combinar ambas ideas:
select CONCAT_WS(' ',nombreJuez,ape1,ape2) as 'Jueces con más casos que el 003', count(CodCaso) 'nº casos'
from caso_4 join juez_2 on idJuez = CodJuez
group by idJuez
having count(CodCaso) < (select count(a1.CodCaso)
						from caso_4 as a1
						where a1.idJuez = '001');


-- 17) implicados cuyo patrimonio esta por encima de la media y que además están afiliados al PP:
select i2.DNI_Implic, CONCAT_WS(' ',i2.nombreImplic,i2.ape1) as Implicado, i2.ciudad, i2.patrimonio, sum(i1.patrimonio)/count(i1.DNI_Implic) as 'patrimonio promedio', partidoAfil
from implicado_5 as i1, implicado_5 as i2 join afiliado_cargo_10 on i2.DNI_Implic = NIF_implic
where i2.patrimonio < (select avg(patrimonio)
					from implicado_5)
		and partidoAfil = 'PP'
group by i2.patrimonio
order by i2.patrimonio desc;

-- ======================================================================================
-- VISTA:
create view infoCasos as
select  CodCaso, nombreCaso as Caso, count(NIF_Implic) as 'Nº de implicados', CONCAT_WS(' ',nombreJuez,ape1,ape2) as 'Juez asignado', nombrePeriodico as 'Descubierto por', year(fechaDescubrim) as Año, millonesDesv as 'Millones desviados'
from caso_4 join juez_2 on idJuez = CodJuez
	join periodico_3 on descubiertoPor = codPeriodico
    join relacion_caso_implicado_9 on idCaso = CodCaso
group by nombreCaso
order by CodCaso asc;

-- comprobamos que se ha creado la vista:
select * from infoCasos;

-- ======================================================================================
-- TRIGGER:

/* trigger de actualización (before update):
Al cambiar el dato de los millonesDesv asociado a un caso, haremos una copia de backup
antes de producirse la actualización, guardaremos la información anterior:
identificadores del caso (CodCaso y nombreCaso), millonesDesv antes y después de la actualización, quién y en qué momento la realizado la modificación
*/

 -- creamos la tabla de respaldo:
 drop table if exists backup_caso_4;
    create table backup_caso_4(
		CodCaso int,
		nombreCaso varchar(20),
		millonesDesvViejo numeric(12,0),
		millonesDesvActualizado numeric(12,0),
		fechaActualizacion datetime,
		usuario varchar(50)
 );
  
  -- creamos el trigger:
  
delimiter //
drop trigger if exists actualizaMillonesDesvBU;
	create trigger actualizaMillonesDesvBU
	before update on caso_4
	for each row
	begin
		insert into backup_caso_4 values (old.CodCaso,old.nombreCaso,old.millonesDesv,new.millonesDesv,now(),current_user());

	end //
delimiter ;
    
-- hacemos la prueba: actualizamos un dato de millonesDesv:
update caso_4 set millonesDesv = millonesDesv+3000 where CodCaso='4';
update caso_4 set millonesDesv = millonesDesv+1000 where CodCaso='2';



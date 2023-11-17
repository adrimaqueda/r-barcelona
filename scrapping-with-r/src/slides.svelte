<script lang="ts">
	import { Presentation, Slide, Code, FitText, Notes } from '@components'
	import SlideLayout from '@lib/layouts/slideLayout.svelte'
	import SlideInfo from '@lib/layouts/slideInfo.svelte';	
</script>

<Presentation>
	<Slide>
		<div>
			<p class="font-extrabold text-6xl">Scrapping con R y automatización con GitHub</p>
		</div>
	</Slide>

	<Slide animate>
			<div class="text-left">
				<p class="font-extrabold text-7xl">Adrián Maqueda</p>
				<p class="font-thin text-lg" style="padding-top: 1rem;">Data Analyst, Dataviz & front-end @ Civio</p>
			</div>
			<div class="font-thin text-base text-left">
				<p>adrimaqueda.com</p><p>@adrimaqueda</p>
			</div>
	</Slide>

	<Slide animate>
		<div class="absolute text-left">
			<p class="font-bold text-5xl pb-5">Brevemente...</p>
			<p class="text-2xl">👨🏻‍🎓 Graduado en Periodismo</p>
		</div>
	</Slide>

	<Slide animate>
		<div class="absolute text-left">
			<p class="font-bold text-5xl pb-5">Brevemente...</p>
			<p class="text-2xl">👨🏻‍🎓 Graduado en Periodismo</p>
			<p class="text-2xl">👨🏻‍🎓 Máster en Periodismo de Investigación, Datos y Visualización</p>
		</div>

		<Notes>
			<p>Excel, Illustrator, Tableau...</p>
		</Notes>
	</Slide>

	<Slide animate>
		<div class="absolute text-left flex w-[900px] flex-col gap-y-20">
			<div>
				<p class="font-bold text-5xl pb-5">Brevemente...</p>
				<p class="text-2xl">👨🏻‍🎓 Graduado en Periodismo</p>
				<p class="text-2xl">👨🏻‍🎓 Máster en Periodismo de Investigación, Datos y Visualización</p>
				<p class="text-2xl">👨🏻‍💻 Datos y verificación en Newtral</p>
			</div>

			<div class="w-[100%] flex flex-wrap justify-around h-[300px]">

				<div class="w-[22%]">
					<img src="personal/radial_espana.jpg" />
					<img src="personal/ola-excepcional-julio-2022.jpg"  class="pt-5" />
				</div>

				<div class="w-[25%] flex flex-col items-center">
					<video src="personal/especial_nobel.mp4" autoplay loop/>
					<img src="personal/productos-inflacion-abril.png" class="pt-5" />
				</div>

				<div class="w-[22%] flex flex-col justify-around">
					<img src="personal/ganadores-roland-garros-1925.png"  class="pb-5" />
					<video src="personal/terminaciones-gordo.mp4" autoplay loop />
				</div>

				<div class="w-[25%] flex flex-col justify-around">
					<video src="personal/variacion_ocupacion_incidencia.mp4" autoplay loop />
					<img src="personal/img-calc.png"  />
				</div>

			</div>			
		</div>

		<Notes>
			<p>Analizando datos, visualizando información, contando historias: introducción al análisis y visualización de datos con R - Curso de Invierno de la Universidad Complutense de Madrid - 2022</p>
			<p>Semi-automatización de las elecciones, análisis climáticos, deportes, INE, lotería de navidad...</p>
		</Notes>
	</Slide>

	<Slide animate>
		<div class="absolute text-left flex w-[900px] flex-col gap-y-20">
			<div>
				<p class="font-bold text-5xl pb-5">Brevemente...</p>
				<p class="text-2xl">👨🏻‍🎓 Graduado en Periodismo</p>
				<p class="text-2xl">👨🏻‍🎓 Máster en Periodismo de Investigación, Datos y Visualización</p>
				<p class="text-2xl">👨🏻‍💻 Datos y verificación en Newtral</p>
				<p class="text-2xl">👨🏻‍💻 Datos y visualización interactiva en Civio</p>
			</div>

			<div class="w-[100%] flex flex-wrap justify-around">

				<div class="w-[18%]">
					<img src="civio/contratos_emergencia.png" class="pb-5" />
					<img src="civio/mascarillas.png" />
				</div>

				<div class="w-[18%] flex items-center"><img src="civio/atencion_primaria.png" /></div>

				<div class="w-[18%]">
					<img src="civio/cita-previa.png"  class="pb-5" />
					<video src="civio/telefono-imv.mp4" autoplay loop />
				</div>

				<div class="w-[18%] flex"><video src="civio/spain-fwi.mp4" autoplay loop /></div>

				<div class="w-[18%] flex"><video src="civio/pge.mp4" autoplay loop /></div>

			</div>
		</div>

		<Notes>
			Qué es Civio. Contratos de emergencia, no yo. Limpieza y análisis de datos. Desde Excel, csv grandes, NETCDF (x primera vez MILLONES DE DATOS)
		</Notes>

	</Slide>

	<Slide animate>
		<div class="absolute text-left flex w-[900px] flex-col gap-y-20">
			<div>
				<p class="font-bold text-5xl pb-5">Brevemente...</p>
				<p class="text-2xl">👨🏻‍🎓 Graduado en Periodismo</p>
				<p class="text-2xl">👨🏻‍🎓 Máster en Periodismo de Investigación, Datos y Visualización</p>
				<p class="text-2xl">👨🏻‍💻 Datos y verificación en Newtral</p>
				<p class="text-2xl">👨🏻‍💻 Datos y visualización interactiva en Civio</p>
				<p class="text-2xl">👨🏻‍💻 Entretanto...</p>
			</div>

			<div class="w-[100%] flex flex-wrap justify-around">

				<div class="w-[22%] flex flex-col items-center">
					<img src="personal/30daychartchallenge.png" />
					<img src="personal/elo_nba_teams.jpg" class="pt-5"/>
				</div>

				<div class="w-[22%] flex items-center"><img src="personal/champions-league-rounds.png" /></div>

				<div class="w-[25%] flex items-center"><video src="personal/video-new-day-painting.mp4" autoplay loop/></div>

				<div class="w-[25%] flex"><video src="personal/pokedex.mp4" autoplay loop /></div>

			</div>
		</div>

		<Notes>
			<p>Autodidacta de visualización con D3, desarrollo web con Svelte.</p>
			<p>Calculadora de 2ª dosis, pokemon, champions, challenges de R...</p>
		</Notes>
	</Slide>

	<Slide animate>
		<div class="absolute text-left">
			<p class="font-bold text-5xl pb-5">¿Qué vamos a hacer?</p>
			<p class="text-2xl">Scrapping con R</p>
			<p class="text-2xl">Entender los básicos de GitHub Actions</p>
			<p class="text-2xl">Montarnos nuestro csv actualizable</p>
		</div>

		<Notes>https://www.adrimaqueda.com/projects/champions-league-rounds</Notes>
	</Slide>

	<Slide animate>
		<SlideLayout title="Scrapping con R" subtitle={"Básicos de HTML"}>

			<SlideInfo>
			<p class="text-xl">El cuerpo de un archivo HTML se compone de:</p>
			<div class="pt-5">
				<p class="text-xl">Etiquetas</p>
				<div class="text-2xl">
					<Code lang="html" lines="1">
						{`
						  <h1></h1> <p></p> <table></table> <span></span> <ul></ul>...
						`}
					</Code>
				</div>
			</div>
			</SlideInfo>

		</SlideLayout>

		<Notes>Comprendamos lo que vamos a analizar, cómo funciona el HTML: etiquetas, ids, clases...</Notes>
	</Slide>

	<Slide animate>
		<SlideLayout title="Scrapping con R" subtitle={"Básicos de HTML"}>
			
			<SlideInfo>
			<p class="text-xl">El cuerpo de un archivo HTML se compone de:</p>
			<div class="pt-5">
				<p class="text-xl">Etiquetas, <b>clases</b></p>
				<div class="text-2xl">
					<Code lang="html" lines="3-4">
						{`
						  <h1></h1> <p></p> <table></table> <span></span> <ul></ul>...

						  <h1 class="clase"></h1>
						  <style>.clase</style>
						`}
					</Code>
				</div>
			</div>
			</SlideInfo>

		</SlideLayout>

		<Notes><p>Sin el asterisco, pero es que si no no me lo leía la presentación</p></Notes>
	</Slide>

	<Slide animate>
		<SlideLayout title="Scrapping con R" subtitle={"Básicos de HTML"}>

			<SlideInfo>
			<p class="text-xl">El cuerpo de un archivo HTML se compone de:</p>
			<div class="pt-5">
				<p class="text-xl">Etiquetas, clases, <b>ids</b></p>
				<div class="text-2xl">
					<Code lang="html" lines="6-7">
						{`
						  <h1></h1> <p></p> <table></table> <span></span> <ul></ul>...
						  
						  <h1 class="clase"></h1>
						  <style>.clase</style>
						  
						  <h1 id="clase"></h1>
						  <style>#clase</style>
						`}
					</Code>
				</div>
			</div>
			</SlideInfo>

		</SlideLayout>
	</Slide>

	<Slide animate>
		<SlideLayout title="Scrapping con R" subtitle={"{rvest} y {tidyverse}"}>

			<SlideInfo>
				<p class="text-xl pb-2">Para hacer scrapping con R vamos a usar {@html "{rvest} y {tidyverse}"}</p>
				<div class="text-2xl">
					<Code lang="r" lines="1-2|4-5">
						{`
						  install.packages("tidyverse")
						  install.packages("rvest")
						  
						  library(tidyverse)
						  library(rvest)
						`}
					</Code>
				</div>
			</SlideInfo>
			
		</SlideLayout>
	</Slide>

	<Slide animate>
		<SlideLayout title="Scrapping con R" subtitle={"{rvest} y {tidyverse}"}>

			<SlideInfo>
				<p class="text-xl pb-2">Scrapeamos toda la página</p>
				<div class="text-2xl">
					<Code lang="r" lines="1-2">
						{`
						# Primero leemos el HTML con read_html():
						starwars <- read_html("https://rvest.tidyverse.org/articles/starwars.html")
						`}
					</Code>
				</div>
			</SlideInfo>

		</SlideLayout>

		<Notes>Abrir url para ir enseñando con el inspector</Notes>
	</Slide>

	<Slide animate>
		<SlideLayout title="Scrapping con R" subtitle={"{rvest} y {tidyverse}"}>

			<SlideInfo>
				<p class="text-xl pb-2">Scrapeamos toda la página</p>
				<div class="text-2xl">
					<Code lang="r" lines="4-8">
						{`
						# Primero leemos el HTML con read_html():
						starwars <- read_html("https://rvest.tidyverse.org/articles/starwars.html")

						starwars
						# {html_document}
						# <html lang="en">
						# [1] <head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"><meta charset="utf-8"><meta http- ...
						# [2] <body><a href="#container" class="visually-hidden-focusable">Skip to content</a><nav class=" ...
						`}
					</Code>
				</div>
			</SlideInfo>

		</SlideLayout>
	</Slide>

	<Slide animate>
		<SlideLayout title="Scrapping con R" subtitle={"{rvest} y {tidyverse}"}>

			<SlideInfo>
				<p class="text-xl pb-2">Seleccionamos solo los tags {"<section>"}</p>
				<div class="text-2xl" >
					<Code lang="r">
						{`
						# Seleccionamos los tags que queremos con html_elements():
						films <- starwars |> html_elements("section")
						`}
					</Code>
				</div>
			</SlideInfo>

		</SlideLayout>
	</Slide>

	<Slide animate>
		<SlideLayout title="Scrapping con R" subtitle={"{rvest} y {tidyverse}"}>

			<SlideInfo>
				<p class="text-xl pb-2">Seleccionamos solo los tags {"<section>"}</p>
				<div class="text-2xl" >
					<Code lang="r" lines="4-12">
						{`
						# Seleccionamos los tags que queremos con html_elements():
						films <- starwars |> html_elements("section")
						
						films
						# {xml_nodeset (7)}
						# [1] <section><h2 data-id="1">The Phantom Menace</h2><p>Released: 1999-05-19</p><p>Director: <span clas ...
						# [2] <section><h2 data-id="2">Attack of the Clones</h2><p>Released: 2002-05-16</p><p>Director: <span cl ...
						# [3] <section><h2 data-id="3">Revenge of the Sith</h2><p>Released: 2005-05-19</p><p>Director: <span cla ...
						# [4] <section><h2 data-id="4">A New Hope</h2><p>Released: 1977-05-25</p><p>Director: <span class="direc ...
						# [5] <section><h2 data-id="5">The Empire Strikes Back</h2><p>Released: 1980-05-17</p><p>Director: <span ...
						# [6] <section><h2 data-id="6">Return of the Jedi</h2><p>Released: 1983-05-25</p><p>Director: <span clas ...
						# [7] <section><h2 data-id="7">The Force Awakens</h2><p>Released: 2015-12-11</p><p>Director: <span class ...
						`}
					</Code>
				</div>
			</SlideInfo>

		</SlideLayout>
	</Slide>

	<Slide animate>
		<SlideLayout title="Scrapping con R" subtitle={"{rvest} y {tidyverse}"}>

			<SlideInfo>
				<p class="text-xl pb-2">Para terminar, extraemos solo los nombres de las películas</p>
				<div class="text-2xl" >
					<Code lang="r">
						{`
						# Usamos html_text2() para extraer solo el texto
						film_names <- films |> html_elements('h2') |> html_text2()
						`}
					</Code>
				</div>
			</SlideInfo>

		</SlideLayout>
	</Slide>

	<Slide animate>
		<SlideLayout title="Scrapping con R" subtitle={"{rvest} y {tidyverse}"}>

			<SlideInfo>
				<p class="text-xl pb-2">Para terminar, extraemos solo los nombres de las películas</p>
				<div class="text-2xl" >
					<Code lang="r" lines="4-6">
						{`
						# Usamos html_text2() para extraer solo el texto
						film_names <- films |> html_elements('h2') |> html_text2()

						film_names
						# [1] "The Phantom Menace"      "Attack of the Clones"    "Revenge of the Sith"     "A New Hope"             
						# [5] "The Empire Strikes Back" "Return of the Jedi"      "The Force Awakens"      
						`}
					</Code>
				</div>
			</SlideInfo>
			
		</SlideLayout>
	</Slide>

	<Slide animate>
		<SlideLayout title="Scrapping con R" subtitle={"{rvest} y {tidyverse}"}>

			<SlideInfo>
				<p class="text-2xl font-bold">¿Cómo extraemos los nombres de los directores?</p>
			</SlideInfo>
			
		</SlideLayout>
	</Slide>

	<Slide animate>
		<SlideLayout 
			title="Scrapping con R" 
			subtitle={"{rvest} y {tidyverse}"}>

			<SlideInfo>
				<p class="text-xl pb-2">Seleccionamos todos los párrafos y, dentro de ellos, los que tienen la clase <b>.director</b></p>
				<div class="text-2xl" >
					<Code lang="r">
						{`
						films |> html_elements('p') |> html_elements(".director") |> html_text()
						# [1] "George Lucas"     "George Lucas"     "George Lucas"     "George Lucas"     "Irvin Kershner"   "Richard Marquand"
						# [7] "J. J. Abrams" 
						`}
					</Code>
				</div>
			</SlideInfo>
			
		</SlideLayout>
	</Slide>

	<Slide animate>
		<SlideLayout 
			title="Scrapping con R" 
			subtitle={"{rvest} y {tidyverse}"}>

			<SlideInfo>
				<div class="flex justify-between">
					<div class="w-[52%]">
						<p class="text-xl pb-2">Hay otra forma de seleccionar contenido, usando el <b>XPath</b></p>
						<div class="text-2xl" >
							<Code lang="r" >
								{`
								films |> 
									html_elements(xpath = '//*[@id="main"]/section[1]/p[1]') |> 
									html_text2()
								`}
							</Code>
						</div>
					</div>
					<img src="xpath.png" class="w-[45%]">
				</div>
			</SlideInfo>
		</SlideLayout>
	</Slide>

	<Slide animate>
		<SlideLayout 
			title="Scrapping con R" 
			subtitle={"{rvest} y {tidyverse}"}>

			<SlideInfo>
				<div class="flex justify-between">
					<div class="w-[52%]">
						<p class="text-xl pb-2">Hay otra forma de seleccionar contenido, usando el <b>xpath</b></p>
						<div class="text-2xl" >
							<Code lang="r" lines="5">
								{`
								films |> 
									html_elements(xpath = '//*[@id="main"]/section[1]/p[1]') |> 
									html_text2()
								
								# [1] "Released: 1999-05-19"
								`}
							</Code>
						</div>
					</div>
					<img src="xpath.png" class="w-[45%]">
				</div>
			</SlideInfo>
		</SlideLayout>

		<Notes>
			<p>Ejemplo simple, pero hay extensiones que nos sirven incluso para seleccionar varios elementos con este método</p>
			<b>¿Alguna duda hasta aquí?</b>
		</Notes>
	</Slide>

	<Slide animate>
		<SlideLayout 
			title="GitHub Actions" 
			subtitle="¿Qué son?">
			
			<SlideInfo>
				<p class="text-xl">"GitHub Actions es una plataforma de integración y despliegue continuos (IC/DC) que te permite automatizar tu mapa de compilación, pruebas y despliegue. Puedes crear flujos de trabajo y crear y probar cada solicitud de cambios en tu repositorio o desplegar solicitudes de cambios fusionadas a producción."</p>
			</SlideInfo>

		</SlideLayout>
	</Slide>

	<Slide animate>
		<SlideLayout 
			title="GitHub Actions" 
			subtitle="¿Qué son?">
			
			<SlideInfo>
				<p class="text-xl">Workflows: "Un flujo de trabajo es un proceso automatizado configurable que ejecutará uno o más jobs"</p>
				<p class="text-xl pt-2">Podemos hacer que se ejecute cuando haya algún cambio en nuestro repositorio, cada cierto tiempo...</p>
				<p class="text-4xl pt-2 font-bold">Gratis hasta los 2.000 minutos al mes</p>
			</SlideInfo>

		</SlideLayout>

		<Notes>Muy últil por la versatilidad para elegir cuando queremos que se ejecute</Notes>
	</Slide>

	<Slide animate>
		<SlideLayout 
			title="GitHub Actions" 
			subtitle="¿Qué necesitamos?">
			
			<SlideInfo>
				<p class="text-xl">Un repositorio</p>
				<p class="text-xl pt-5">Una carpeta llamada <span class="code">.github/workflows</span></p>
				<p class="text-xl pt-5">Un archivo <span class="code">.yml</span></p>
			</SlideInfo>

		</SlideLayout>
	</Slide>

	<Slide animate>
		<SlideLayout 
			title="GitHub Actions" 
			subtitle="¿Qué son?">
			
			<SlideInfo>
				<div class="flex w-[900px] justify-between">
					<img src="steps.png" class="h-[500px]"/>
					<img src="steps-github.png" class="h-[500px]"/>
				</div>
			</SlideInfo>

		</SlideLayout>

		<Notes>
			<p>Importante la sangría y el orden</p>
			<b>¿Alguna duda hasta aquí?</b>
		</Notes>
	</Slide>

	<Slide animate>
		<SlideLayout 
			title="Uniendo todo" 
			subtitle="">

			<SlideInfo>
				<p class="text-xl">Ahora aplicamos lo aprendido en scrapping y en las GitHub Actions:</p>
				<p class="text-xl pt-5">- Un archivo de R para extraer la información</p>
				<p class="text-xl">- Un archivo YAML para automatizar su ejecución</p>
			</SlideInfo>
			
		</SlideLayout>
	</Slide>

	<Slide animate>
		<p class="font-bold text-5xl pb-5">¡Empecemos con el scrapping!</p>
	</Slide>

	<Slide animate>
		<SlideLayout
			title="Configurando nuestro proyecto de R" 
			subtitle=""
			>
			<SlideInfo>
				<p class="text-xl">Creamos una nueva carpeta</p>
				<p class="text-xl pt-5">Abrimos R Studio</p>
				<p class="text-xl pt-5">Creamos un nuevo proyecto y comenzamos con el scrapping</p>
			</SlideInfo>
		</SlideLayout>
	</Slide>

	<Slide>
		<p>Ejemplos de scrapping en <a style="border-bottom: solid 2px" href="https://github.com/adrimaqueda/r-barcelona/tree/main/ejemplos-scrapping">este repositorio</a></p>
	</Slide>

	<Slide animate>
		<p class="font-bold text-5xl pb-5">Ahora la configuración en GitHub</p>
	</Slide>

	<Slide animate>
		<SlideLayout
			title="Inicializar GitHub en nuestro ordenador" 
			subtitle=""
			>
			<SlideInfo>
				<p class="text-xl">Instalamos GitHub: <span class="code">brew install gh</span></p>
				<p class="text-xl pt-5">Creamos una nueva carpeta</p>
				<p class="text-xl pt-5">Pegamos el archivo de R</p>
				<p class="text-xl pt-5">Abrimos la capeta con Visual Studio Code</p>
				<p class="text-xl pt-5">La conectamos con GitHub: <span class="code">git init</span></p>
				<p class="text-xl pt-5">Publicamos la rama</p>
			</SlideInfo>
		</SlideLayout>

		<Notes>
			<p>Si no recuerdo mal, ahora tendréis que autenticaros con vuestra cuenta de GitHub</p>
			<p>Si ya tenemos GitHub instalado en el ordenador no hace falta que lo hagamos</p>
		</Notes>
	</Slide>

	<Slide animate>
		<SlideLayout
			title="Creamos el YAML" 
			subtitle=""
			>
			<SlideInfo>
				<div class="flex flex-row w-[960px] justify-between">
					<div class="w-[450px]">
						<p class="text-xl">Nombramos el proceso</p>
					</div>
					<div class="w-[450px] text-2xl">
						<Code lang="yml">
							{`
							name: ucl_scraping
							`}
						</Code>
					</div>
				</div>
			</SlideInfo>
		</SlideLayout>
	</Slide>

	<Slide animate>
		<SlideLayout
			title="Creamos el YAML" 
			subtitle=""
			>
			<SlideInfo>
				<div class="flex flex-row w-[960px] justify-between">
					<div class="w-[450px]">
						<p class="text-xl opacity-40">Nombramos el proceso</p>
						<p class="text-xl pt-5">Indicamos cuándo se va a ejecutar</p>
					</div>
					<div class="w-[450px] text-2xl">
						<Code lang="yml" lines="3-6">
							{`
							name: ucl_scraping

							on:
								push:
								schedule:
									- cron: '0 12 * * FRI'
							`}
						</Code>
					</div>
				</div>
			</SlideInfo>
		</SlideLayout>

		<Notes>Cada viernes a las 12:00 -> https://crontab.guru </Notes>
	</Slide>

	<Slide animate>
		<SlideLayout
			title="Creamos el YAML" 
			subtitle=""
			>
			<SlideInfo>
				<div class="flex flex-row w-[960px] justify-between">
					<div class="w-[450px]">
						<p class="text-xl opacity-40">Nombramos el proceso</p>
						<p class="text-xl opacity-40 pt-5">Indicamos cuándo se va a ejecutar</p>
						<p class="text-xl pt-5">Permitimos que sobrescriba nuestro repositorio</p>
					</div>
					<div class="w-[450px] text-2xl">
						<Code lang="yml" lines="7-9">
							{`
							name: ucl_scraping

							on:
								push:
								schedule:
									- cron: '0 12 * * FRI'
							
							permissions:
								contents: write
							`}
						</Code>
					</div>
				</div>
			</SlideInfo>
		</SlideLayout>
	</Slide>

	<Slide animate>
		<SlideLayout
			title="Creamos el YAML" 
			subtitle=""
			>
			<SlideInfo>
				<div class="flex flex-row w-[960px] justify-between">
					<div class="w-[450px]">
						<p class="text-xl opacity-40">Nombramos el proceso</p>
						<p class="text-xl opacity-40 pt-5">Indicamos cuándo se va a ejecutar</p>
						<p class="text-xl opacity-40 pt-5">Permitimos que sobrescriba nuestro repositorio</p>
						<p class="text-xl pt-5">Iniciamos el workflow</p>
					</div>
					<div class="w-[450px] text-2xl">
						<Code lang="yml" lines="11-13">
							{`
							name: ucl_scraping

							on: 
								push:
								schedule:
									- cron: '0 12 * * FRI'
							
							permissions:
								contents: write
							
							jobs:
								autoscrape:
							`}
						</Code>
					</div>
				</div>
			</SlideInfo>
		</SlideLayout>

		<Notes>
			crontab guru
		</Notes>
	</Slide>

	<Slide animate>
		<SlideLayout
			title="Creamos el YAML" 
			subtitle=""
			>
			<SlideInfo>
				<div class="flex flex-row w-[960px] justify-between">
					<div class="w-[450px]">
						<p class="text-xl">Determinamos el sistema operativo</p>
					</div>
					<div class="w-[450px] text-2xl">
						<Code lang="yml" offset="11" lines="3">
							{`
							jobs:
								autoscrape:
									runs-on: macos-latest
							`}
						</Code>
					</div>
				</div>
			</SlideInfo>
		</SlideLayout>
	</Slide>

	<Slide animate>
		<SlideLayout
			title="Creamos el YAML" 
			subtitle=""
			>
			<SlideInfo>
				<div class="flex flex-row w-[960px] justify-between">
					<div class="w-[450px]">
						<p class="text-xl opacity-40">Determinamos el sistema operativo</p>
						<p class="text-xl pt-5">Pasos:</p>
						<ul class="list-inside text-xl list-decimal">
							<li>Leemos nuestro repositorio</li>
						</ul>
					</div>
					<div class="w-[450px] text-2xl">
						<Code lang="yml" offset="11" lines="5-8">
							{`
							jobs:
								autoscrape:
									runs-on: macos-latest

									steps:
										- uses: actions/checkout@v4
											with:
												fetch-depth: 0
							`}
						</Code>
					</div>
				</div>
			</SlideInfo>
		</SlideLayout>

		<Notes>Con focus-depth: 0 solo comprueba el último commit</Notes>
	</Slide>

	<Slide animate>
		<SlideLayout
			title="Creamos el YAML" 
			subtitle=""
			>
			<SlideInfo>
				<div class="flex flex-row w-[960px] justify-between">
					<div class="w-[450px]">
						<p class="text-xl opacity-40">Determinamos el sistema operativo</p>
						<p class="text-xl pt-5">Pasos:</p>
						<ul class="list-inside text-xl list-decimal">
							<li class="opacity-40">Leemos nuestro repositorio</li>
							<li>Instalamos R</li>
						</ul>
					</div>
					<div class="w-[450px] text-2xl">
						<Code lang="yml" offset="11" lines="5,9">
							{`
							jobs:
								autoscrape:
									runs-on: macos-latest

									steps:
										- uses: actions/checkout@v4
											with:
												fetch-depth: 0
										- uses: r-lib/actions/setup-r@v2
							`}
						</Code>
					</div>
				</div>
			</SlideInfo>
		</SlideLayout>
	</Slide>

	<Slide animate>
		<SlideLayout
			title="Creamos el YAML" 
			subtitle=""
			>
			<SlideInfo>
				<div class="flex flex-row w-[960px] justify-between">
					<div class="w-[450px]">
						<p class="text-xl opacity-40">Determinamos el sistema operativo</p>
						<p class="text-xl pt-5">Pasos:</p>
						<ul class="list-inside text-xl list-decimal">
							<li class="opacity-40">Leemos nuestro repositorio</li>
							<li class="opacity-40">Instalamos R</li>
							<li>Instalamos las librerías necesarias</li>
						</ul>
					</div>
					<div class="w-[450px] text-2xl">
						<Code lang="yml" offset="11" lines="5,10-14">
							{`
							jobs:
								autoscrape:
									runs-on: macos-latest

									steps:
										- uses: actions/checkout@v4
											with:
												fetch-depth: 0
										- uses: r-lib/actions/setup-r@v2

										- name: Install packages
											run: |
												R -e 'install.packages("tidyverse")'
												R -e 'install.packages("rvest")'
							`}
						</Code>
					</div>
				</div>
			</SlideInfo>
		</SlideLayout>
	</Slide>

	<Slide animate>
		<SlideLayout
			title="Creamos el YAML" 
			subtitle=""
			>
			<SlideInfo>
				<div class="flex flex-row w-[960px] justify-between">
					<div class="w-[450px]">
						<p class="text-xl opacity-40">Determinamos el sistema operativo</p>
						<p class="text-xl pt-5">Pasos:</p>
						<ul class="list-inside text-xl list-decimal">
							<li class="opacity-40">Leemos nuestro repositorio</li>
							<li class="opacity-40">Instalamos R</li>
							<li class="opacity-40">Instalamos las librerías necesarias</li>
							<li>Ejecutamos el archivo de R</li>
						</ul>
					</div>
					<div class="w-[450px] text-2xl">
						<Code lang="yml" offset="11" lines="5,16-18">
							{`
							jobs:
								autoscrape:
									runs-on: macos-latest

									steps:
										- uses: actions/checkout@v4
											with:
												fetch-depth: 0
										- uses: r-lib/actions/setup-r@v2

										- name: Install packages
											run: |
												R -e 'install.packages("tidyverse")'
												R -e 'install.packages("rvest")'
										
										- name: Scrape
											run: Rscript 'path/file.R'
							`}
						</Code>
					</div>
				</div>
			</SlideInfo>
		</SlideLayout>
	</Slide>

	<Slide animate>
		<SlideLayout
			title="Creamos el YAML" 
			subtitle=""
			>
			<SlideInfo>
				<div class="flex flex-row w-[960px] justify-between">
					<div class="w-[350px]">
						<p class="text-xl opacity-40">Determinamos el sistema operativo</p>
						<p class="text-xl pt-5">Pasos:</p>
						<ul class="list-inside text-xl list-decimal">
							<li class="opacity-40">Leemos nuestro repositorio</li>
							<li class="opacity-40">Instalamos R</li>
							<li class="opacity-40">Instalamos las librerías necesarias</li>
							<li class="opacity-40">Ejecutamos el archivo de R</li>
							<li>Actualizamos el repositorio</li>
						</ul>
					</div>
					<div class="w-[550px] text-2xl">
						<Code lang="yml" offset="11" lines="5">
							{`
							jobs:
								autoscrape:
									runs-on: macos-latest

									steps:
							...
							`}
						</Code>
						<Code lang="yml" offset="28" lines="2-11">
							{`
							...
										- name: Commit files
											run: |
													git config --local user.name actions-user
													git config --local user.email "actions@github.com"
													git add 'path/*'
													git commit -am "GH ACTION UCL rounds: $(date)"
													git push origin main
												env:
													REPO_KEY: $ {{secrets.GITHUB_TOKEN}}
													username: github-actions
							`}
						</Code>
					</div>
				</div>
			</SlideInfo>
		</SlideLayout>

		<Notes><p>El dólar pegado a la llave</p></Notes>
	</Slide>

	<Slide>
		<SlideLayout
			title="Momento de la verdad"
			subtitle=""
			>
			<SlideInfo>
				<div class="flex justify-around items-center">
					<div class="text-2xl w-[50%] h-fit">
						<p>Hacemos commit y comprobamos que funciona bien</p>
						<p class="pt-5">GitHub > Repositorio > Actions</p>
					</div>
					<img src="actions-historial.png" class="h-[550px]"/>
				</div>
			</SlideInfo>
		</SlideLayout>
	</Slide>

	<Slide>
		<FitText class="uppercase" type="h1">¡gracias!</FitText>
	</Slide>

</Presentation>
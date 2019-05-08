# separa_trecho_biblico

Um estudo em como tratar arquivos de texto e tabelas de grandes proporções. Aqui, usando a Bíblia com seus mais de 31 mil versos.

O código permite entrar com passagens bíblicas, retornando a passagem por extenso.

Exemplos das possibilidades de entrada são:

<ul>
  <li>Sl (retorna todo o livro de Salmos)</li>
  <li>Sl 80 (retorna o capítulo 80 de Salmos)</li>
  <li>Sl 80, 81, 134 (retorna os capítulos especificados)</li>
  <li>Sl 80: 2-8 (retorna os versículos definidos do capítulo)</li>
  <li>Sl 80: 2-8, 10 / Sl 80: 2-8, 10-15</li>
  <li>Sl 80: 8 - 89:6 (começa no capítulo 80:8 e adiciona capítulos inteiros até chegar ao final, 89:6)</li>
</ul>

O código retorna um arquivo de texto com as passagens separadas.

<h3>Exemplo:</h3>

**Entrada:**

lec <- "D 1: Mc 16:1-6; Sl 118: 1, 14-15"

**Saída:**

D 1:


 Mc 16:1-6
 
1 E, passado o sábado, Maria Madalena, e Maria, mãe de Tiago, e Salomé, compraram aromas para irem ungi-lo. 
2 E, no primeiro dia da semana, foram ao sepulcro, de manhã cedo, ao nascer do sol. 
3 E diziam umas às outras: Quem nos revolverá a pedra da porta do sepulcro? 
4 E, olhando, viram que já a pedra estava revolvida; e era ela muito grande. 
5 E, entrando no sepulcro, viram um jovem assentado à direita, vestido de uma roupa comprida, branca; e ficaram espantadas. 
6 Ele, porém, disse-lhes: Não vos assusteis; buscais a Jesus Nazareno, que foi crucificado; já ressuscitou, não está aqui; eis aqui o lugar onde o puseram. 


 Sl 118: 1, 14-15
 
1 Louvai ao SENHOR, porque ele é bom, porque a sua benignidade dura para sempre. 

14 O SENHOR é a minha força e o meu cântico; e se fez a minha salvação. 
15 Nas tendas dos justos há voz de júbilo e de salvação; a destra do SENHOR faz proezas. 


-----

<h3>Sobre o código:</h3>

É necessário que o arquivo *2.confere_biblia.csv* esteja na mesma pasta do código.

Pode-se usar qualquer tradução para pescar as passagens, contando que esteja no mesmo estilo do arquivo apresentado por mim (*1.Biblia ACF.csv*). É uma ideia futura expandir essa funcionalidade.

A tradução aqui apresentada pertence à Sociedade Bíblica Trinitariana do Brasil.

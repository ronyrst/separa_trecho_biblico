##### R E S U M O   D A S   F U N � � E S ###################
# e_numero_: checa se um string � n�mero ou n�o.
# captura_verso: partindo de um data.frame da B�blia, pega o �ltimo vers�culo do cap�tulo do livro entrado.
# captura_verso_2: partindo do data.frame 'confere_biblia', pega o �ltimo vers�culo do cap�tulo do livro entrado.
# checa_biblia: entra com um data.frame da B�blia, e confere se h� vers�culos faltantes em cada um dos cap�tulos de todos os livros.
# separa_lecionario: entra com o string do lecion�rio do m�s, retornando ele em linhas separadas.
# divide_verso: faz com que um verso do tipo "Sl 90: 1-2" seja separado em livro, cap�tulo, come�o e fim.
# biblifica: com o que � feito na fun��o anterior, retorna um data.frame contendo a passagem.
# Lectionary: faz uso das outras fun��es para criar um arquivo de texto do lecion�rio do m�s.
#####

##### F U N � � E S #########################################
e_numero_ <- function(string){
  ####
  # Entra com um caractere. Se for numero, retorna TRUE, se n�o for, FALSE.
  #
  ####
  
  if( string == "1" ){
    return(T)
  } else if( string == "2" ){
    return(T)
  } else if( string == "3" ){
    return(T)
  } else if( string == "4" ){
    return(T)
  } else if( string == "5" ){
    return(T)
  } else if( string == "6" ){
    return(T)
  } else if( string == "7" ){
    return(T)
  } else if( string == "8" ){
    return(T)
  } else if( string == "9" ){
    return(T)
  } else if( string == "0" ){
    return(T)
  } else { # se n�o � string de n�mero, retorna F.
    return(F)
  }
}

#############################################################
captura_verso <- function(livro, capitulo, biblia){
  ####
  # Entra com o livro em string e o cap�tulo como valor num�rico, e a b�blia em um data.frame.
  #  Retorna o n�mero do �ltimo vers�culo desse cap�tulo. Se o cap�tulo n�o existir, retorna 0.
  ####
  
  # 'captura_verso' pega o �ltimo verso a partir do data.frame 'biblia'.
  # serve para comparar a b�blia com a tabela 'confere_biblia'.
  
  vetor_linha <- ( as.character(biblia[,1]) == livro ) # vetor l�gico que pega s� o 'livro' que foi entrado na fun��o.
  Livrao <- biblia[vetor_linha,] # a b�blia passa agora a ter apenas o 'livro' entrado na fun��o.
  bkleber <- ( Livrao[,2] == capitulo ) # vetor l�gico que pega o 'cap�tulo' do 'livro' entrado na fun��o.
  Livrao_certo <- Livrao[bkleber,] # resta apenas o 'cap�tulo' do 'livro' em 'Livrao_certo'.
  
  passagem <- Livrao_certo[ nrow(Livrao_certo), 3 ] # pega o �ltimo vers�culo do cap�tulo. Estrutura: < "Livro" as.num('capitulo') as.num(versiculo) "...." >
  
  if( length(passagem) == 0 ){ # caso entre uma passagem que nem mesmo exista, retorna 0. Como Sl 119: 200.
    return(0)
  }
  return(passagem)
}

#############################################################
captura_verso_2 <- function(livro, capitulo){
  ####
  # Entra com o livro em string e o cap�tulo como valor num�rico.
  #  Retorna o n�mero do �ltimo vers�culo desse cap�tulo. Se o cap�tulo n�o existir, retorna 0.
  ####
  
  # 'captura_verso_2' pega o �ltimo verso a partir do data.frame 'confere_biblia', mais veloz para conferir que o data.frame 'biblia'.
  
  confere_biblia <- read.csv("2. confere_biblia.csv", header = T, sep = ";")
  
  vetor_livros <- confere_biblia$Livro == livro # vetor l�gico que pega a linha que � a do 'livro' entrado na fun��o.
  confefe <- confere_biblia[vetor_livros, ] # resta um data.frame de uma linha s�.
  
  passagem <- as.numeric( confefe[capitulo + 1] ) # as.numeric permite que n�o entre com t�tulo da linha e coluna em 'passagem'. Apenas o valor num�rico
                                                   # do �ltimo vers�culo do cap�tulo.
  return(passagem)
}

#############################################################
checa_biblia <- function(biblia){
  ####
  # Entra com um data.frame de b�blia. E confere se cada cap�tulo de cada livro da b�blia tem o n�mero certo de vers�culos.
  #
  ####
  
  confere_biblia <- read.csv("2. confere_biblia.csv", header = T, sep = ";") # compara o data.frame 'biblia' com este. Olhando o �ltimo verso de cada cap�tulo
                                                                              # da b�blia.
  
  for( i in 1:66 ) { # 66 livros.
    
    trabalha <- confere_biblia[i,] # pega a linha i, referente ao livro i do data.frame 'confere_biblia'. 1 = Gn; 66 = "Ap".
    capitulo <- 1 # itera nos cap�tulos de cada livro da b�blia. Exemplo: Salmos vai de 1 a 150.
    
    for( j in 2:151 ){ # a coluna 2 indica o primeiro cap�tulo de cada livro, 3 o segundo, e assim por diante.
      
      termina_em <- captura_verso( as.character(confere_biblia[i,1]), capitulo, biblia ) # fazer 'trabalha[1]' n�o retornava o nome do livro. Por isso usa-se
                                                                                          # as.char('confere_biblia[i,1]').
                                                                                         # pega o verso final do cap�tulo em quest�o.
      if( (trabalha[j] == 0) & (length(termina_em) == 0) ){ # nem todos livros terminam no cap. 150. Se chega a um cap. com 0 vers�culos, quebra o loop.
        break # o break faz sair apenas do for( j ... ). Ele volta para o for maior: for( i ... ).
      } else if( termina_em == trabalha[j] ){ # se o cap�tulo da 'biblia' est� igual ao do 'confere_biblia', d� bom.
        capitulo <- capitulo + 1
        next
      } else { # se o cap�tulo da 'biblia' est� diferente ao do 'confere_biblia', d� ruim, e uma mensagem de erro � printada.
        print( paste( "H� um erro em", as.character( confere_biblia[i,1] ), "no cap�tulo", capitulo) )
        capitulo <- capitulo + 1
      }
    }
  }
}

#############################################################
separa_lecionario <- function(lecionario){
  ####
  # Entra com o string do Lecion�rio do m�s.
  #  Retorna lista com as linhas separadas.
  ####
  
  resposta <- vector("list", 400) # 400 para passar um "lecion�rio" de um ano com folga.
  lec <- strsplit(lecionario, "")[[1]]
  
  arroba <- ""
  nurroba <- 1
  for(i in lec){
    if( i == "\n") { # se achou um \n, quer dizer que a linha acabou, e adiciona 'arroba' � lista 'resposta'.
      resposta[nurroba] <- arroba
      arroba <- "" # 'arroba' � zerado a cada nova linha.
      nurroba <- nurroba + 1
    } else { # se n�o � \n, continua adicionando texto a 'arroba'.
      arroba <- paste(arroba, i, sep = "")
    }
  }
  resposta[nurroba] <- arroba # o �ltimo string que 'arroba' recebe n�o � adicionado � resposta no for. Essa a��o � feita aqui.
  
  salva <- 0
  for( i in 1:length(resposta) ){ # as partes da lista 'resposta' que n�o foram usadas s�o descobertas aqui.
    if( length(resposta[i][[1]]) == 0 ){
      salva <- i
      break
    }
  }
  
  if( salva != 0 ){
    resposta[salva:length(resposta)] <- NULL # e caso realmente hajam partes que n�o foram usadas, s�o jogadas fora aqui.
  }
  
  return(resposta)
}

#############################################################
divide_verso <- function(verso){
  ####
  # Entra com um verso do tipo "<<livro>> <<cap�tulo(s)>>:<<vers�culo(s)>>".
  #  Retorna lista com livro, cap�tulo e vers�culos separados.
  #
  # Exemplos:
  #  1. "Sl"
  #  2. "Sl 80"
  #  3. "Sl 80, 81, 82, 83, 84"
  #  4. "Sl 80: 2-8" (ou "Sl 80: 2a-8b", e outras combina��es)
  #  5. "Sl 80: 2-8, 10" / "Sl 80: 2-8, 10-15" (ou combina��es usando letras, como "Sl 80: 2a-8b, 10c-15d")
  #  6. "Sl 80: 8 - 89: 6" (ou combina��es usando letras, como "Sl 80: 8b - 89: 6c")
  ####
  
  resposta <- vector("list", 151) # uma lista de tamanho 151 implica que seria poss�vel adicionar o livro de Salmos inteiro.
  resposta_contador <- 1 # itera na lista 'resposta'.
  lec <- strsplit(verso, "")[[1]]
  
  arroba <- ""
  nurroba <- 0
  for(i in lec){ # este for recebe o nome do livro ao qual as passagens se referem. Como Sl, 1Co, etc.
    if( !e_numero_(i) & (i != " ") ){ # se n�o � n�mero e n�o � espa�o, � adicionado a 'arroba'.
      arroba <- paste(arroba, i, sep = "")
      nurroba <- nurroba + 1
    } else if( e_numero_(i) & (arroba == "") ){ # se � n�mero e 'arroba' est� vazio, � o valor 1,2,3 de livros como 1Jo, 2Cr, etc.
      arroba <- paste(arroba, i, sep = "")
      nurroba <- nurroba + 1
    } else if( i == " " ){
      nurroba <- nurroba + 1 # 'nurroba' soma a quantidade de strings h� at� a primeira letra de cap�tulo do string 'verso'.
    } else{
      break
    }
  }
  
  livro <- arroba # entra o livro sem espa�os em 'livro'. Este valor � usado em todo o programa.
  lec <- lec[-(1:nurroba)] # lec deixa de ter os strings de livro, e primeiros espa�os, tbm.
  resposta[resposta_contador][[1]] <- arroba # entra o primeiro valor na lista resposta. O nome do livro em quest�o.
  resposta_contador <- resposta_contador + 1
  
  if( length(lec) == 0 ){ # se o verso entrado for simplesmente o nome de um livro, os cap�tulos deste livro ser�o adicionados aqui.
    zuerroba <- ""
    
    for( i in 1:150 ){
      if( captura_verso_2(livro, i) == 0 ){ # se encontra um cap�tulo com 0 vers�culos, acaba o loop.
        break
      } else if( zuerroba == "" ){ # quando se tem um s� vers�culo, ou para come�ar o string com "1 ,2,3,4, ..."
        zuerroba <- "1"
      } else {
        zuerroba <- paste(zuerroba,",", i, sep = "") # adiciona o ",2,3,4..."
      }
    }
    
    lec <- strsplit(zuerroba, "")[[1]]
  }
  
  arriba <- c()
  dois_pontos <- 0
  virgula <- 0
  for( i in lec ){ # este for considera quantos ":" e "," aparecem em 'lec', ap�s retirar o nome do livro.
    if( i == " "){ # o for retira os " " de 'lec', tamb�m.
      next
    } else if( i == ":" ){
      dois_pontos <- dois_pontos + 1
      arriba <- c(arriba, i)
    } else if( i == "," ){
      virgula <- virgula + 1
      arriba <- c(arriba, i)
    } else{
      arriba <- c(arriba, i) # 'arriba' faz o resultado permanecer um vetor de strings, no lugar de juntar tudo em um grande string,
    }                         # o qual seria necess�rio separar novamente.
  }
  
  ##
  if( (dois_pontos == 0) & (virgula == 0) ){ # primeiro caso, 0 ":" e 0 ",". Como em "Sl 80".
    arroba <- ""
    for(i in arriba){ # resta somente 8 e 0 em 'arriba'.
      arroba <- paste(arroba, i, sep = "")
    }
    
    capitulo <- as.numeric(arroba) # 'capitulo' recebe o 80.
    a_cap <- c(capitulo, 0, 1, captura_verso_2(livro, capitulo) ) # 0 ser� a separa��o entre o cap�tulo e as duplas de vers�culos.
                                                                   # <cap> 0 <versiculo inicial> <versiculo final> ou
    resposta[resposta_contador][[1]] <- a_cap
    
  ##
  } else if( (dois_pontos == 0) & (virgula != 0) ){ # segundo caso, 0 ":" e 1+ ",". Como em "Sl 80, 81, 82, 83".
    arroba <- ""
    arroba_caps <- c()
    for( i in arriba ){ # como s� o que entra � "80, 81, 82, 83" separados em strings, aqui eles s�o separados entre v�rgulas. "80" "81" etc.
      if( i != "," ){
        arroba <- paste(arroba, i, sep = "")
      } else {
        arroba_caps <- c(arroba_caps, as.numeric(arroba))
        arroba <- ""
      }
    }
    arroba_caps <- c(arroba_caps, as.numeric(arroba)) # �ltimo valor em 'arroba' n�o � pego dentro do for, � pego aqui.
    
    for( i in arroba_caps ){ # para cada cap�tulo em 'arroba_caps', � feita uma entrada na lista resposta...
      capitulo <- i
      a_cap <- c(capitulo, 0, 1, captura_verso_2(livro, capitulo) ) # ...com o valor 0, o primeiro vers�culo (1), e o �ltimo, recebido de 'captura_verso_2'.
      
      resposta[resposta_contador][[1]] <- a_cap
      resposta_contador <- resposta_contador + 1
    }
    
    
  #####
  } else if( (dois_pontos == 1) & (virgula == 0) ){ # terceiro caso, 1 ":" e 0 ",". Como em "Sl 80: 3 - 9".
    
    arroba <- ""
    nurroba <- 0
    for( i in arriba ){ # nesse for, a fun��o pega o "80", sobrando apenas os vers�culos "3-9".
      if( i == ":" ){
        nurroba <- nurroba + 1
        break
      } else {
        arroba <- paste(arroba, i, sep = "")
        nurroba <- nurroba + 1
      }
    }
    
    capitulo <- as.numeric(arroba)
    arriba <- arriba[-(1:nurroba)]
    
    conta_traco <- 0 # conta quantos tra�os h� no que restou.
    for( i in arriba ){
      if( (i == "-") || (i == "-") ){
        conta_traco <- conta_traco + 1
      }
    }
    
    if( conta_traco == 0 ){ # se 0 "-"s, quer dizer que h� apenas um vers�culo no texto.
      arroba <- ""
      for( i in arriba ){
        if( e_numero_(i) ){ # o 'e_numero_' retira as poss�veis letras que hajam.
          arroba <- paste(arroba, i, sep = "")
        }
      }
      
      verso <- as.numeric(arroba)
      a_cap <- c(capitulo, 0, verso, verso) # se � somente um verso. Ele � colocado 2 vezes no vetor que vai para a resposta.
      
    } else { # se h� 1 "-", h� 2 vers�culos no texto.
      arrow_0 <- "" # 'arrow_0' � o primeiro vers�culo entrado.
      arrow_2 <- "" # 'arrow_2' � o �ltimo vers�culo entrado.
      conta <- 0 # conta indica se � o primeiro ou �ltimo vers�culo, que est� sendo captado.
      for( i in arriba ){
        if( (i == "-") || (i == "-") ){
          conta <- 2
        } else if( !e_numero_(i) ){ # se n�o � n�mero, n�o entra. Mas � checado anteriormente se � "-", para n�o ter problemas.
          next
        } else if( conta == 0 ){
          arrow_0 <- paste(arrow_0, i, sep = "")
        } else if( conta == 2 ){
          arrow_2 <- paste(arrow_2, i, sep = "")
        }
      }
      
      verso_0 <- as.numeric(arrow_0)
      verso_2 <- as.numeric(arrow_2)
      
      a_cap <- c(capitulo, 0, verso_0, verso_2)
    }
    
    resposta[resposta_contador][[1]] <- a_cap
      
  ##  
  } else if( (dois_pontos == 1) & (virgula != 0) ){ # quarto caso, 1 ":" e 1+ ",". Como em "Sl 80: 3-9, 12-15".
    
    arroba <- ""
    nurroba <- 0
    for( i in arriba ){ # pega o valor "80", o cap�tulo.
      if( i == ":" ){
        nurroba <- nurroba + 1
        break
      } else {
        arroba <- paste(arroba, i, sep = "")
        nurroba <- nurroba + 1
      }
    }
    
    capitulo <- as.numeric(arroba)
    arriba <- arriba[-(1:nurroba)]
    
    arroba <- ""
    arroba_pass <- c()
    for( i in arriba ){ # separa as v�rgulas. Ficando ent�o: "3-9" e "12-15", no vetor.
      if( i != "," ){
        arroba <- paste(arroba, i, sep = "")
      } else {
        arroba_pass <- c(arroba_pass, arroba)
        arroba <- ""
      }
    }
    arroba_pass <- c(arroba_pass, arroba)
    
    
    a_cap <- c(capitulo) # 'a_cap' recebe inicialmente apenas o cap�tulo.
    
    for( i in arroba_pass ){ # para cada entrada de 'arroba_pass'...
      passagem <- strsplit(i, "")[[1]]
      
      conta_traco <- 0 # conta quantos "-" h� em cada peda�o separado de 'arroba_pass'.
      for( j in passagem ){
        if( (j == "-") || (j == "-") ){
          conta_traco <- conta_traco + 1
        }
      }
      
      if( conta_traco == 0 ){ # se zero "-"s, � apenas um verso.
        arroba <- ""
        for( j in passagem ){
          if( e_numero_(j) ){
            arroba <- paste(arroba, j, sep = "")
          }
        }
        
        verso <- as.numeric(arroba)
        a_cap <- c(a_cap, 0, verso, verso) # adiciona o verso em 'a_cap', que ainda pode receber outros valores.
        
      } else { # se 1 "-", s�o dois versos.
        arrow_0 <- ""
        arrow_2 <- ""
        conta <- 0
        for( j in passagem ){
          if( (j == "-") || (j == "-") ){
            conta <- 2
          } else if( !e_numero_(j) ){ # tira os valores n�o num�ricos, como 13'a', etc.
            next
          } else if( conta == 0 ){
            arrow_0 <- paste(arrow_0, j, sep = "")
          } else if( conta == 2 ){
            arrow_2 <- paste(arrow_2, j, sep = "")
          }
        }
      
        verso_0 <- as.numeric(arrow_0)
        verso_2 <- as.numeric(arrow_2)
        
        a_cap <- c(a_cap, 0, verso_0, verso_2) # adiciona os versos "0" e "2" em 'a_cap', que ainda pode receber outros valores.
      }
    }
    
    resposta[resposta_contador][[1]] <- a_cap # com o fim do for, 'a_cap' finalmente � adicionado a 'resposta'.
    
    
  #####
  } else if( (dois_pontos == 2) & (virgula == 0) ){ # quinto caso, 2 ":" e 0 ",". Como em "Sl 80:3 - 83:9".
    
    cap_inicio <- ""
    cap_fim <- ""
    
    verso_inicio <- ""
    verso_fim <- ""
    
    nurroba <- 0
    for( i in arriba ){ # roda at� achar o primeiro ":", pegando o primeiro cap�tulo
      if( i != ":" ){
        cap_inicio <- paste(cap_inicio, i, sep = "")
        nurroba <- nurroba + 1
      } else {
        nurroba <- nurroba + 1
        break
      }
    }
    arriba <- arriba[-(1:nurroba)] # retira os strings do primeiro cap�tulo.
    
    nurroba <- 0
    for( i in arriba ){ # vai at� o primeiro "-", que pega o vers�culo do primeiro cap�tulo que come�a a passagem.
      if( (i == "-") || (i == "-") ){
        nurroba <- nurroba + 1
        break
      } else if ( !e_numero_(i) ){ # aqui tamb�m h� a prote��o contra a presen�a de 13'a', por exemplo. Os retirando.
        nurroba <- nurroba + 1
      } else {
        verso_inicio <- paste(verso_inicio, i, sep = "")
        nurroba <- nurroba + 1
      }
    }
    arriba <- arriba[-(1:nurroba)]
    
    nurroba <- 0 # aqui, vai at� o segundo ":" no total, mas primeiro do que sobrou. � o cap�tulo que termina a passagem.
    for( i in arriba ){
      if( i != ":" ){
        cap_fim <- paste(cap_fim, i, sep = "")
        nurroba <- nurroba + 1
      } else {
        nurroba <- nurroba + 1
        break
      }
    }
    arriba <- arriba[-(1:nurroba)]
    
    for( i in arriba ){ # por fim, sobra apenas o vers�culo do cap�tulo final, que � pego aqui.
      if( e_numero_(i) ){
        verso_fim <- paste(verso_fim, i, sep = "")
      }
    }
    
    cap_inicio <- as.numeric(cap_inicio) # os valores s�o passados para as.numeric.
    cap_fim <- as.numeric(cap_fim)
    
    verso_inicio <- as.numeric(verso_inicio)
    verso_fim <- as.numeric(verso_fim)
    
    
    if( cap_inicio == cap_fim ){ # se por ventura o cap�tulo inicial for o mesmo que o cap�tulo final...
      
      a_cap <- c(cap_inicio, 0, verso_inicio, verso_fim ) # ... � salvo em 'a_cap' como sendo verso inical e final do mesmo cap�tulo.
      resposta[resposta_contador][[1]] <- a_cap
    
    } else { # caso o cap�tulo inicial seja diferente do cap�tulo final.
      
      for( i in cap_inicio:cap_fim ){
        
        if( i == cap_inicio ){ # cap�tulo inicial � posto em uma posi��o da lista 'resposta'.
          a_cap <- c(i, 0, verso_inicio, captura_verso_2(livro, i) )
          resposta[resposta_contador][[1]] <- a_cap
          resposta_contador <- resposta_contador + 1
        
        } else if( i == cap_fim ){ # cap�tulo final � colocado aqui, por fim.
          a_cap <- c(i, 0, 1, verso_fim )
          resposta[resposta_contador][[1]] <- a_cap
          resposta_contador <- resposta_contador + 1
          
        } else { # os cap�tulos entre o inicial e final s�o postos aqui.
          a_cap <- c(i, 0, 1, captura_verso_2(livro, i))
          resposta[resposta_contador][[1]] <- a_cap
          resposta_contador <- resposta_contador + 1
        }
      }
    }
  }
  
  salva <- 0
  for( i in 1:length(resposta) ){ # as partes da lista 'resposta' que n�o foram usadas s�o descobertas aqui.
    if( length(resposta[i][[1]]) == 0 ){
      salva <- i
      break
    }
  }
  
  if( salva != 0 ){
    resposta[salva:length(resposta)] <- NULL # e caso realmente hajam partes que n�o foram usadas, s�o jogadas fora aqui.
  }
  
  return(resposta)
}

#############################################################
biblifica <- function(livro, vetur, biblia){
  ####
  # Entra com o nome de um livro em string, um vetor de valores que indica cap�tulo e vers�culos deste livro, e um data.frame da b�blia.
  #  Retorna um data.frame menor contendo as passagens entradas.
  ####
  
  capitulo <- vetur[1] # o primeiro valor de 'vetur' � o cap�tulo.
  vetur <- vetur[-(1:2)] # � retirado de vetur as duas primeiras entradas. O cap�tulo e uma entrada 0.
  
  conta_zero <- 0
  conta <- 0
  for( i in vetur ){ # checa-se quantos zeros ainda est�o presentes em 'vetur', al�m de contar as outras entradas presentes nele.
    if( i == 0 ){
      conta_zero <- conta_zero + 1
    } else {
      conta <- conta + 1
    }
  }
  
  if( conta_zero == 0 ){ # se n�o h� zeros, h� s� mais duas entradas em 'vetur'. O come�o do cap�tulo, e seu fim.
    comeco <- vetur[1]
    fim <- vetur[2]
    
  } else {
    comeco <- c() # em caso de haverem zero(s), 'comeco' e 'fim' tornam-se vetores.
    fim <- c()
    rodagem <- conta/2 # sup�e-se que conta seja um m�ltiplo de 2.
    quinta <- 1 # quinta itera para os valores entrarem em 'comeco' e 'fim'.
    
    for( i in 1:(rodagem) ){ # aqui, 'vetur' � da seguinte estrutura: comeco fim 0 comeco fim 0 comeco fim ...
      comeco <- c(comeco, vetur[quinta])
      fim <- c(fim, vetur[quinta + 1])
      quinta <- quinta + 3
    }
  }
  
  vetor_linha <- ( as.character(biblia[,1]) == livro ) # vetor l�gico que pega s� o 'livro' que foi entrado na fun��o.
  Livrao <- biblia[vetor_linha,] # a b�blia passa agora a ter apenas o 'livro' entrado na fun��o.
  bkleber <- ( Livrao[,2] == capitulo ) # vetor l�gico que pega o 'capitulo' entrado em 'vetur'.
  bookap <- Livrao[bkleber,] # resta apenas o 'cap�tulo' do 'livro' em 'bookap'.
  
  if( conta_zero == 0 ){
    resposta <- bookap[comeco:fim,] # se n�o haviam mais zeros, a resposta � s� pegar o come�o e fim do cap�tulo em quest�o.
    
  } else {
    o_cole <- vector("list", rodagem)
    
    for( i in 1:rodagem ){
      o_cole[[i]] <- bookap[comeco[i]:fim[i],] # caso hajam zeros, haver�o 'rodagem' entradas na lista 'o_cole'.
    }
    
    resposta <- do.call(rbind, o_cole) # a fun��o 'do.call' junta os diferentes data.frames presentes em 'o_cole' e os une em um �nico.
  }
  
  return(resposta)
}

#############################################################
Lectionary <- function(lec, linguagem = "acf"){
  ####
  # Entra com um string com todas as linhas de um lecion�rio do m�s.
  #  Retorna um arquivo de texto com as passagens separadas.
  ####
  
  if( linguagem == "acf" ){ # quem sabe adicionamos mais tradu��es.
    biblia <- read.csv("1. B�blia ACF.csv", header = F, sep = "/")
  }
  
  tilibra <- "Lecionario.txt" # criado o arquivo.
  
  resp <- separa_lecionario(lec) # separa o string �nico do lecion�rio do m�s em linhas diferentes, dentro de uma lista.
  
  for( i in 1:length(resp) ){ # itera na lista, linha a linha.
    linha <- resp[i][[1]]
    
    ##
    linha_sep <- strsplit(linha, "")[[1]]
    arroba <- ""
    for( i in linha_sep ){ # pega o come�o da linha, ou com o "Quaresma"; ou com o "Q 1:".
      if( i == ":" ) {
        arroba <- paste(arroba, i, sep = "")
        break # para no ":". Se � do tipo "Quaresma", n�o para.
      } else {
        arroba <- paste(arroba, i, sep = "")
      }
    }
    
    if( arroba == linha ) { # aqui, � o caso de a linha ter sido usada inteira no for anterior, como "Quaresma".
      write( c(arroba, "\n"), file = tilibra, append = T )
      next # ele pula para a pr�xima linha, caso positivo.
    } else {
      write( c(arroba, "\n"), file = tilibra, append = T ) # escreve o "Q 1:", e continua.
      arroba_sep <- length( strsplit(arroba, "")[[1]] )
      linha_sep <- linha_sep[-c( 1:arroba_sep )] # retira o "Q 1:" e segue com a linha "menor".
    }
  
    ##
    conta_linha <- sum( linha_sep %in% ";" ) # v� quantas passagens h�, a depender do n�mero de ";".
    trouble <- vector("list", conta_linha + 1) # ser�o ";" + 1 passagens.
    
    arroba <- ""
    conta_linha <- 1
    for( i in linha_sep ){ # aqui, cada passagem � entrada em trouble, separadas por ";".
      if( i == ";"){
        
        trouble[conta_linha] <- arroba
        conta_linha <- conta_linha + 1
        arroba <- ""
        
      } else {
        arroba <- paste(arroba, i, sep = "")
      }
    }
    trouble[conta_linha] <- arroba # a �ltima passagem entra por aqui, que estava em arroba.
    
    ##
    for( j in 1:length(trouble) ){ # agora, itera em trouble, cada passagem por vez.
      verso <- trouble[j][[1]] # verso � a passagem, como "Sl 80: 2-13".
      write( c(verso), file = tilibra, append = T)
      
      samba <- divide_verso( verso ) # samba recebe o livro, cap�tulo e vers�culos separados.
      livro <- samba[1][[1]]
      
      concorre <- 1
      for( k in samba ){ # para cada entrada de samba, uma passagem � tirada.
        if( k[1] == livro ){ # se � o 'livro', o primeiro valor de samba, n�o faz nada.
          next
          
        } else {
          
          if(concorre > 1){ # concorre itera caso entre mais de um cap�tulo, permitindo que saia um espa�o entre cap�tulos. Como em "Sl 80:13-82:2".
            write( c(""), file = tilibra, append = T)
          }
          passagens <- biblifica(livro, k, biblia) # recebe um data.frame das passagens entradas no vetor 'k'.
          
          lado_a <- passagens[1, 3] - 1 # 'lado_a' e 'lado_b' permitem ver se pularam linhas, como "1-5, 9-15".
          for( l in 1:nrow(passagens) ){
            lado_b <- passagens[l, 3]
            if( is.na(lado_b) ){ # prote��o contra entradas NA.
              lado_b <- lado_a + 1
            }
            
            if( lado_b == (lado_a + 1) ){
              write( c( as.character(passagens[l,4])), file = tilibra, append = T)
              if( !is.na(passagens[l, 3]) ){ # prote��o contra entradas NA.
                lado_a <- passagens[l, 3]
              }
              
            } else{
              write( c( "", as.character(passagens[l,4])), file = tilibra, append = T) # aqui, pula um espa�o, caso haja um pulo de passagens.
              if( !is.na(passagens[l, 3]) ){
                lado_a <- passagens[l, 3]
              }
            }
          }
          
          concorre <- concorre + 1
        }
      }
      
      write( c("\n"), file = tilibra, append = T)
    }
    
    write( c("-----"), file = tilibra, append = T)
  }
  
}

#############################################################



##### U S O S ###############################################

#biblia <- read.csv("1. B�blia ACF.csv", header = F, sep = "/")
#checa_biblia(biblia)

lec <- "D 1: Sl 118: 1-2, 14-24; Is 25:6-9; 1 Co 15:1-11; Mc 16:1-8
S 2: Sl 118: 1-2, 14-24; Gn 1: 1-19; 1 Co 15: 35-49
T 3: Sl 118: 1-2, 14-24; Gn 1: 20-2: 4a; 1 Co 15: 50-58
Q 4: Sl 118: 1-2, 14-24; Ct 3: 1-11; Mc 16: 1-8
Q 5: Sl 133; Dn 1: 1-21; At 2: 42-47
S 6: Sl 133; Dn 2: 1-23; At 4: 23-31
S 7: Sl 133; Dn 2: 24-49; Jo 12: 44-50
D 8: Sl 133; At 4:32-35; 1 Jo 1:1-2:2; Jo 20:19-31
S 9: Sl 135; Dn 3: 1-30; 1 Jo 2: 3-11
T 10: Sl 135; Dn 6: 1-28; 1 Jo 2: 12-17
Q 11: Sl 135; Is 26: 1-15; Mc 12: 18-27
Q 12: Sl 4; Dn 9: 1-19; 1 Jo 2: 18-25
S 13: Sl 4; Dn 10: 2-19; 1 Jo 2: 26-28
S 14: Sl 4; At 3: 1-10; Lc 22: 24-30
D 15: Sl 4; At 3:12-19; 1 Jo 3:1-7; Lc 24:36b-48
S 16: Sl 150; Jr 30: 1-11a; 1 Jo 3: 10-16
T 17: Sl 150; Os 5: 15-6: 6; 2 Jo 1: 1-6
Q 18: Sl 150; Pv 9: 1-6; Mc 16: 9-18
Q 19: Sl 23; Gn 30: 25-43; At 3: 17-26
S 20: Sl 23; Gn 46: 28-47: 6; At 4: 1-4
S 21: Sl 23; Gn 48: 8-19; Mc 6: 30-34
D 22: Sl 23; At 4:5-12; 1 Jo 3:16-24; Jo 10:11-18
S 23: Sl 95; 1 Sm 16: 1-13; 1 Pe 5: 1-5
T 24: Sl 95; 1 Cr 11: 1-9; Ap 7: 13-17
Q 25: Sl 95; Mq 7: 8-20; Mc 14: 26-31
Q 26: Sl 22: 25-31; Am 8: 1-7; At 8: 1b-8
S 27: Sl 22: 25-31; Am 8: 11-13; At 8: 9-25
S 28: Sl 22: 25-31; Am 9: 7-15; Mc 4: 30-32
D 29: Sl 22: 25-31; At 8:26-40; 1 Jo 4:7-21; Jo 15:1-8
S 30: Sl 80; Is 5: 1-7; Gl 5: 16-26"

Lectionary(lec, linguagem = "acf")
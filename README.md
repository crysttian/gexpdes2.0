# GExpDes - Interface Gráfica para o Pacote ExpDes

**Nova versão disponível 2.0 - Beta**

**Versão em testes!**


![](logo.png)

Os dados gerados a partir de um experimento são analisados segundo metodologias de Estatística Experimental. Para realizar essas análises, destacam-se a utilização da linguagem R, particularmente com o pacote **ExpDes**. Para quem não quer se preocupar com a linguagem R, esse trabalho apresenta uma aplicação em **Shiny** que possibilita o uso dos pacotes **ExpDes**, **labestData** e outros pacotes voltados para Estatística Experimental. A interface reduz a complexidade da análise, sendo um auxiliar para a montagem dos comandos da linguagem R para realizá-la. Ao final, os comandos que foram necessários também são disponibilizados ao usuário.

Artigo do pacote publicado na Sigmae - [link](https://publicacoes.unifal-mg.edu.br/revistas/index.php/sigmae/article/view/951)

**Lançamento da Versão 1.0 na 64ª Reunião Anual da Região Brasileira da Sociedade Internacional de Biometria (RBras) e 18º Simpósio de Estatística Aplicada à Experimentação Agronômica (SEAGRO)** - [link](https://www.rbras64.com.br/)

## Equipe de Desenvolvimento

**Universidade Federa da Bahia - UFBA**
- Crysttian Arantes Paixão - crysttianpaixao@ufba.br
- Aretha da Silva dos Santos (Bacharelado Interdisciplinar) - aretha.silva@ufba.br

**Universidade Federa de Santa Catarina - UFSC**
- Rogério Kormann (Agronomia) - rogerio.kormann@ufsc.br
- Eduardo Nunes Rosa (Agronomia) - eduardo.nunes@grad.ufsc.br

**Universidade Federal de Alfenas**
- Eric Batista Ferreira - eric.ferreira@unifal-mg.edu.br
- Denismar Alves Nogueira - denismar.nogueira@unifal-mg.edu.br.

# Como usar?

Existem três possibilidades para utilizarem o pacote GExpDes, sendo:

## Executando direto de um servidor Shiny 

Para utilizá-la, por favor, acesse o endereço [**gexpdes.ufsc.br**](http://gexpdes.ufsc.br)
 
A interface é executada direto de um servidor web não sendo necessário ter o R instalado em sua máquina.

## Instalando o pacote

É necessário ter o R instalado ([**Instalação do R**](http://cran.r-project.org/)). Recomendamos que o RStudio também seja instalado ([**Instalação do RStudio**](https://www.rstudio.com/)).

Melhoramos a instalação do pacote, que agora é mais simples.

Abra o R (ou RStudio):

Passo 1. Execute o comando 

```r
install.packages("devtools",dependencies = TRUE)
```

Passo 2. Execute os comandos

```r
library(devtools)
install_git(url = "https://github.com/crysttian/gexpdes2.0.git")
```
Passo 3. Após a confirmação de instalação do pacote, execute o comando **instalar()** para realizar a instalação dos pacotes necessários para o GExpDes. 

```r
library(gexpdes)

instalar()
```

Passo 4. Se o comando **instalar()** realizou toda a instalação corretamente, execute o próximo comando para abrir a interface do GExpDes:

```r
gexpdesView()
```

# Pacotes desatualizados

Caso os pacotes do R no seu computador estejam desatualizados, recomendamos que rode o comando:

```r
update.packages(ask = FALSE)
```
A atualização poderá levar alguns minutos.

# Pacotes utilizados

Para informações sobre os pacotes, digite **citation("pacote").

- shiny

- shinyalert

- shinyjs

- readxl

- readODS

- labestData

- ExpDes.pt

- plotly

- agricolae

- agricolaeplotr


# Sugestões e erros

O R por ser um software colaborativo, gostaríamos de ter a sua colabração seja relatando erros que encontrou ou sugestões na interface. Essa nova versão só foi possível em decorrência dos relatos que recebemos de vários usuários.

- Caso encontre algum erro ou tenha uma sugestão, solicitamos que nos envie um e-mail para **gexpdes@gmail.com** 

- Caso deseje, temos um grupo do Telegram, que pode ser acessado pelo qrcode abaixo. Os desenvolvedores estão no grupo e você poderá falar diretamente conosco.

![](ajustada.png)

# Como citar a interface em seus trabalhos?

KORMANN, R.; MASSAD, E.; PAIXÃO, C. A.; FERREIRA, E. B.; NOGUEIRA, D. A. GExpDes: Interface Gráfica para o ExpDes. SIGMAE, v. 8, p. 170-179, 2019.

# Notas da versão

Melhorias implementadas:

1 - Importação de arquivos nos formatos, csv, tabulares, arquivos do Excel (xls ou xlsx) e, OpenOffice ou Libre Office (ods).

2 - Gráficos de Interação para avaliação de Experimentos Fatoriais

3 - Criação dos croquis experimentais

4 - Geração dos relatórios

5 - Melhoria no processo de instalação

6 - Remodelagem das telas


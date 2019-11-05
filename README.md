# Repositório de dados

## Disciplina de Modelagem de Distribuição de Espécies

**Ministrante** <br>
Maurício Vancine

**Carga horária** <br>


**Participantes** <br>


**Ementa** <br>


---

### Informações aos participantes

**Datas e horários** <br>

**Local** <br>

**Plano de ensino** <br> 

**Contato** <br>
Para mais informações ou dúvidas, envie e-mail para Maurício Vancine (mauricio.vancine@gmail.com)

---

### Instruções aos participantes

**Softwares**<br>
R, RStudio e QGIS <br>

1. Instalar a versão mais recente do [R (3.6.1)](https://www.r-project.org) e [RStudio (1.2.5001)](https://www.rstudio.com) <br>
[Vídeo de instalação do R e do RStudio](https://youtu.be/l1bWvZMNMCM) <br>
[Curso da linguagem R](https://www.youtube.com/playlist?list=PLucm8g_ezqNq0RMHvzZ8M32xhopFhmsr6)

2. Instalar a versão mais estável do [QGIS (3.4 LTR)](https://qgis.org/pt_BR/site) <br>
[Vídeo de instalação do QGIS](https://youtu.be/QjhCkX2sVI4) <br>
[Curso Básico de QGIS](https://www.youtube.com/playlist?list=PLRrETkwtvTrMEeicAyYABdNwPpnzZdw5q) <br>
[Curso de QGIS 3.6 - MasterGIS](https://www.youtube.com/watch?v=anSaq5pbCpk&list=PLjHRAtOKOOLhHyQHUXBCfSqOWHFJZ1Pnf)

#### Linux (Ubuntu e Linux Mint)
```
# r
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/" # mint 19 ou ubuntu 18
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu disco-cran35/" # ubuntu 19
sudo apt update
sudo apt install -y r-base-core # r
sudo apt install -y r-base-dev # devtools
sudo apt install -y libssl-dev # tidyverse
sudo apt install -y libxml2-dev # tidyverse
sudo apt install -y libcurl4-openssl-dev # tidyverse
sudo apt install -y libgdal-dev # gdal
sudo apt install -y libproj-dev # gdal
sudo apt install -y libudunits2-dev # units

# rstudio
wget https://download1.rstudio.org/desktop/bionic/amd64/rstudio-1.2.1335-amd64.deb
sudo dpkg -i rstudio-1.2.1335-amd64.deb
sudo apt install -fy
rm rstudio-1.2.1335-amd64.deb

# qgis
sudo add-apt-repository ppa:ubuntugis/ppa && sudo apt update # mint 19 ou ubuntu 18
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 6B827C12C2D425E227EDCA75089EBE08314DF160 # ubuntu 19
sudo add-apt-repository "deb http://ppa.launchpad.net/ubuntugis/ppa/ubuntu bionic main" # ubuntu 19
sudo apt install -y qgis qgis-plugin-grass

```

**Instalar os pacotes no R** <br>
Com o R e o RStudio instalados, baixe esse [script](https://github.com/mauriciovancine/disciplina-modelagem-distribuicao-especies/blob/master/01_scripts/00_script_install_packages.R) (scripts são roteiros que possuem comandos, como um rascunho). <br>
Abra o script baixado (**00_script_install_packages.R**) no software RStudio e rode cada linha de comando para instalar os pacotes. <br>
Para rodar as linhas, basta colocar o cursor na linha de código a ser executada e precionar: `Crtl + Enter`, como mostra o gif abaixo:

![Alt Text](https://appsilon.com/wp-content/uploads/2019/03/blog_code_execution_optimized.gif)
---

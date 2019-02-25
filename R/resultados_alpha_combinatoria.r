install.packages("psych")
library(psych)
library(readxl)
codemaster_com_NA <- read_excel("C:/Temp/dados/codemaster_com_NA.xlsx", na = "NA")
df <- codemaster_com_NA

----------------------------------------------------------------------------------------------
TESTE 01: TODAS AS COMBINAÇÕES POSSÍVEIS DE ITENS EM CONJUNTOS DE 4 A 21 
----------------------------------------------------------------------------------------------
alpha_comp <- function(){
	alphas <- array(dim=c(1000000,4))
	max_alpha = -100
	max_std_alpha = -100
	colnames_max_alpha = array(dim=c(22))
	num_alphas = 0
	for (c1 in 1:22) {
		df_wc <- df
		df_col_alvo <- df_wc[,c1:c1]
		df_wc[c1] <- NULL
		for(ini_jan in 1:21){
			for(tam in 2:20){
				df_alpha <- df_col_alvo
				if((ini_jan+tam) <= length(df_wc)){
					for (col in ini_jan:(ini_jan+tam)){
						df_alpha <- cbind(df_alpha, df_wc[,col:col])
					}
					alpha_ <- suppressWarnings(alpha(df_alpha, check.keys=TRUE))
					if ((alpha_$total$std.alpha > max_std_alpha) || (alpha_$total$raw_alpha > max_alpha) || (alpha_$total$raw_alpha > .6) ){
						num_alphas = (num_alphas+1)
						print("-")
						print(paste("!!!!!!!!!! NOVO MAX ALPHA: ", alpha_$total$raw_alpha))
						print(colnames(df_alpha))
						if (alpha_$total$std.alpha > max_std_alpha){
							max_std_alpha <- alpha_$total$std.alpha
						}
						if (alpha_$total$raw_alpha > max_alpha){
							max_alpha <- alpha_$total$raw_alpha
						}
						colnames_max_alpha <- colnames(df_alpha)
						alphas[num_alphas,1] <- alpha_$total$raw_alpha
						alphas[num_alphas,2] <- alpha_$total$std.alpha
						alphas[num_alphas,3] <- alpha_$total$G6
						alphas[num_alphas,4] <- paste(colnames(df_alpha),collapse=" ")
					}
				}
			}
		}
	}
	return(alphas)
}

alphas <- alpha_comp()

[1] "raw alpha:  -0.117250822422824"
[1] "std alpha:  -0.0819920083795456"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca"
[1] "raw alpha:  -0.149664039585181"
[1] "std alpha:  -0.0731581020231009"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp"
[1] "raw alpha:  -0.133787505887913"
[1] "std alpha:  -0.0607741579979384"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault"
[1] "raw alpha:  -0.111565332337974"
[1] "std alpha:  -0.0317839903991792"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca"
[1] "raw alpha:  -0.111744735245975"
[1] "std alpha:  -0.0173915027661669"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores"
[1] "raw alpha:  0.0179064119478229"
[1] "std alpha:  0.045849355026355"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault"
[1] "raw alpha:  0.0714666396083291"
[1] "std alpha:  0.127997128191837"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito"
[1] "raw alpha:  0.072347100109016"
[1] "std alpha:  0.124823931615072"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD"
[1] "raw alpha:  0.113808481409458"
[1] "std alpha:  0.163315923689259"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura"
[1] "raw alpha:  0.194444737885368"
[1] "std alpha:  0.220634368441669"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito"
[1] "raw alpha:  0.212287611526037"
[1] "std alpha:  0.233845634266119"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao"
[1] "raw alpha:  0.297104420307871"
[1] "std alpha:  0.306789565630783"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes"
[1] "raw alpha:  0.311182084357418"
[1] "std alpha:  0.318216645715585"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem"
[1] "raw alpha:  0.324326950077926"
[1] "std alpha:  0.33512820778957"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem imagensDeFundo"
[1] "raw alpha:  0.320398630600304"
[1] "std alpha:  0.347058018333279"
[1] "Colunas:  dimensionamentoResponsivo capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem imagensDeFundo"
[1] "raw alpha:  0.321360656012127"
[1] "std alpha:  0.325445194847637"
[1] "Colunas:  dimensionamentoResponsivo caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem"
[1] "raw alpha:  0.335049336294745"
[1] "std alpha:  0.343963004741125"
[1] "Colunas:  dimensionamentoResponsivo caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem imagensDeFundo"
[1] "raw alpha:  0.340174978837508"
[1] "std alpha:  0.33615733505791"
[1] "Colunas:  dimensionamentoResponsivo tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes"
[1] "raw alpha:  0.351509589833052"
[1] "std alpha:  0.347122082797248"
[1] "Colunas:  dimensionamentoResponsivo tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem"
[1] "raw alpha:  0.363682088733592"
[1] "std alpha:  0.36385620312054"
[1] "Colunas:  dimensionamentoResponsivo tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem imagensDeFundo"
[1] "raw alpha:  0.359487058964982"
[1] "std alpha:  0.36643654062154"
[1] "Colunas:  dimensionamentoResponsivo doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem imagensDeFundo"
[1] "raw alpha:  0.364909218921313"
[1] "std alpha:  0.373907811019014"
[1] "Colunas:  dimensionamentoResponsivo pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem"
[1] "raw alpha:  0.378094202879965"
[1] "std alpha:  0.391399117732011"
[1] "Colunas:  dimensionamentoResponsivo pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem imagensDeFundo"
[1] "raw alpha:  0.378325305720251"
[1] "std alpha:  0.396284836736388"
[1] "Colunas:  dimensionamentoResponsivo telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem imagensDeFundo"
[1] "raw alpha:  0.388287919396453"
[1] "std alpha:  0.381370638951911"
[1] "Colunas:  dimensionamentoResponsivo dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes"
[1] "raw alpha:  0.394184399133974"
[1] "std alpha:  0.388462062274516"
[1] "Colunas:  dimensionamentoResponsivo dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem"
[1] "raw alpha:  0.399737588258808"
[1] "std alpha:  0.396648045857311"
[1] "Colunas:  dimensionamentoResponsivo dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem imagensDeFundo"
[1] "raw alpha:  0.417085399478347"
[1] "std alpha:  0.382210176612748"
[1] "Colunas:  dimensionamentoResponsivo sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes"
[1] "raw alpha:  0.423099880874393"
[1] "std alpha:  0.392197096121297"
[1] "Colunas:  dimensionamentoResponsivo sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem"
[1] "raw alpha:  0.422251703280932"
[1] "std alpha:  0.394998384288569"
[1] "Colunas:  dimensionamentoResponsivo sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem imagensDeFundo"
[1] "raw alpha:  0.249310463449864"
[1] "std alpha:  0.401776800917247"
[1] "Colunas:  dimensionamentoResponsivo imagensDeFundo alturaComponenteAlvoToque larguraComponenteAlvoToque"
[1] "raw alpha:  0.297104420307871"
[1] "std alpha:  0.306789565630783"
[1] "Colunas:  familiaFonte dimensionamentoResponsivo tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes"
[1] "raw alpha:  0.311182084357418"
[1] "std alpha:  0.318216645715585"
[1] "Colunas:  familiaFonte dimensionamentoResponsivo tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem"
[1] "raw alpha:  0.324326950077926"
[1] "std alpha:  0.33512820778957"
[1] "Colunas:  familiaFonte dimensionamentoResponsivo tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem imagensDeFundo"
[1] "raw alpha:  0.327182369663629"
[1] "std alpha:  0.341212376129184"
[1] "Colunas:  familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem"
[1] "raw alpha:  0.338210122605096"
[1] "std alpha:  0.353627062002729"
[1] "Colunas:  familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem imagensDeFundo"
[1] "raw alpha:  0.330996790391367"
[1] "std alpha:  0.361583281899121"
[1] "Colunas:  familiaFonte capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem"
[1] "raw alpha:  0.341662608862626"
[1] "std alpha:  0.372094328932249"
[1] "Colunas:  familiaFonte capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem imagensDeFundo"
[1] "raw alpha:  0.341920033563724"
[1] "std alpha:  0.35294827586994"
[1] "Colunas:  familiaFonte caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem"
[1] "raw alpha:  0.352653714808659"
[1] "std alpha:  0.364734998286104"
[1] "Colunas:  familiaFonte caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem imagensDeFundo"
[1] "raw alpha:  0.360271350507272"
[1] "std alpha:  0.363884265555727"
[1] "Colunas:  familiaFonte tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes"
[1] "raw alpha:  0.374765039954222"
[1] "std alpha:  0.378806041559701"
[1] "Colunas:  familiaFonte tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem"
[1] "raw alpha:  0.38381630680245"
[1] "std alpha:  0.388409215200391"
[1] "Colunas:  familiaFonte tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem imagensDeFundo"
[1] "raw alpha:  0.376144519696166"
[1] "std alpha:  0.385424427301135"
[1] "Colunas:  familiaFonte doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem imagensDeFundo"
[1] "raw alpha:  0.385463295148576"
[1] "std alpha:  0.401123686563584"
[1] "Colunas:  familiaFonte pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem"
[1] "raw alpha:  0.39522241545885"
[1] "std alpha:  0.410896699262417"
[1] "Colunas:  familiaFonte pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem imagensDeFundo"
[1] "raw alpha:  0.39472900891242"
[1] "std alpha:  0.41212345010278"
[1] "Colunas:  familiaFonte telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem imagensDeFundo"
[1] "raw alpha:  0.40659099682033"
[1] "std alpha:  0.400320705677115"
[1] "Colunas:  familiaFonte dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes"
[1] "raw alpha:  0.417108358786844"
[1] "std alpha:  0.413959993508707"
[1] "Colunas:  familiaFonte dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem"
[1] "raw alpha:  0.418930387969527"
[1] "std alpha:  0.414246699046766"
[1] "Colunas:  familiaFonte dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem imagensDeFundo"
[1] "raw alpha:  0.432515417402305"
[1] "std alpha:  0.334501353815823"
[1] "Colunas:  familiaFonte sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito"
[1] "raw alpha:  0.448489953998188"
[1] "std alpha:  0.416540223657119"
[1] "Colunas:  familiaFonte sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes"
[1] "raw alpha:  0.457139399035366"
[1] "std alpha:  0.430973223695069"
[1] "Colunas:  familiaFonte sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem"
[1] "raw alpha:  0.444968700867576"
[1] "std alpha:  0.436167028462779"
[1] "Colunas:  capitalizacaoSentenca sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem"
[1] "raw alpha:  0.426267438463477"
[1] "std alpha:  0.439236627438429"
[1] "Colunas:  tamanhoFonteComp dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem"
[1] "raw alpha:  0.427985776107967"
[1] "std alpha:  0.438598004192479"
[1] "Colunas:  tamanhoFonteComp dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem imagensDeFundo"
[1] "raw alpha:  0.463510152943074"
[1] "std alpha:  0.392354426194109"
[1] "Colunas:  tamanhoFonteComp sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito"
[1] "raw alpha:  0.465354843717809"
[1] "std alpha:  0.451604172950652"
[1] "Colunas:  tamanhoFonteComp sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes"
[1] "raw alpha:  0.47028074579887"
[1] "std alpha:  0.46032020478824"
[1] "Colunas:  tamanhoFonteComp sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes componenteImagem"
[1] "raw alpha:  0.482994746522583"
[1] "std alpha:  0.384458927400961"
[1] "Colunas:  contrasteEntreTextoFundoNegrito sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD"
[1] "raw alpha:  0.452204694079017"
[1] "std alpha:  0.427815040913875"
[1] "Colunas:  contrasteEntreTextoFundoNegrito sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura pixelizacao imagensEmBotoes"
[1] "raw alpha:  0.459229886718156"
[1] "std alpha:  0.440328122793487"
[1] "Colunas:  contrasteEntreTextoFundoNegrito sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura pixelizacao imagensEmBotoes componenteImagem"
[1] "raw alpha:  0.455391435618233"
[1] "std alpha:  0.395249776477128"
[1] "Colunas:  pixelizacao imagensEmBotoes componenteImagem imagensDeFundo"
[1] "raw alpha:  0.459229886718156"
[1] "std alpha:  0.440328122793487"
[1] "Colunas:  imagensEmBotoes sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao componenteImagem"
[1] "raw alpha:  0.455391435618233"
[1] "std alpha:  0.395249776477128"
[1] "Colunas:  imagensEmBotoes pixelizacao componenteImagem imagensDeFundo"
[1] "raw alpha:  0.459229886718156"
[1] "std alpha:  0.440328122793487"
[1] "Colunas:  componenteImagem sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito pixelizacao imagensEmBotoes"
[1] "raw alpha:  0.455391435618233"
[1] "std alpha:  0.395249776477128"
[1] "Colunas:  componenteImagem pixelizacao imagensEmBotoes imagensDeFundo"	


----------------------------------------------------------------------------------------------
TESTE 02: TODAS AS COMBINAÇÕES POSSÍVEIS DE ITENS EM CONJUNTOS DE 4 A 21 COM "check.keys=TRUE"
----------------------------------------------------------------------------------------------

alpha_comp <- function(){
	alphas <- array(dim=c(1000000,4))
	max_alpha = -100
	max_std_alpha = -100
	colnames_max_alpha = array(dim=c(22))
	num_alphas = 0
	for (c1 in 1:22) {
		df_wc <- df
		df_col_alvo <- df_wc[,c1:c1]
		df_wc[c1] <- NULL
		for(ini_jan in 1:21){
			for(tam in 2:20){
				df_alpha <- df_col_alvo
				if((ini_jan+tam) <= length(df_wc)){
					for (col in ini_jan:(ini_jan+tam)){
						df_alpha <- cbind(df_alpha, df_wc[,col:col])
					}
					alpha_ <- suppressWarnings(alpha(df_alpha, check.keys=TRUE))
					if ((alpha_$total$std.alpha > max_std_alpha) || (alpha_$total$raw_alpha > max_alpha) || (alpha_$total$raw_alpha > .6) ){
						num_alphas = (num_alphas+1)
						print("-")
						print(paste("!!!!!!!!!! NOVO MAX ALPHA: ", alpha_$total$raw_alpha))
						print(colnames(df_alpha))
						if (alpha_$total$std.alpha > max_std_alpha){
							max_std_alpha <- alpha_$total$std.alpha
						}
						if (alpha_$total$raw_alpha > max_alpha){
							max_alpha <- alpha_$total$raw_alpha
						}
						colnames_max_alpha <- colnames(df_alpha)
						alphas[num_alphas,1] <- alpha_$total$raw_alpha
						alphas[num_alphas,2] <- alpha_$total$std.alpha
						alphas[num_alphas,3] <- alpha_$total$G6
						alphas[num_alphas,4] <- paste(colnames(df_alpha),collapse=" ")
					}
				}
			}
		}
	}
	return(alphas)
}


[1] "raw alpha:  0.149038139272653"
[1] "std alpha:  0.170211901231746"
[1] "G6:  0.134645656832644"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca"
[1] "raw alpha:  0.23122436987947"
[1] "std alpha:  0.223616731774388"
[1] "G6:  0.191608115219355"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao"
[1] "raw alpha:  0.271515824407961"
[1] "std alpha:  0.274695641258551"
[1] "G6:  0.245011208493217"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp"
[1] "raw alpha:  0.27577483293939"
[1] "std alpha:  0.273379193355538"
[1] "G6:  0.251260268891659"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault"
[1] "raw alpha:  0.296221853333866"
[1] "std alpha:  0.29252444050802"
[1] "G6:  0.281054823089656"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca"
[1] "raw alpha:  0.292136929779848"
[1] "std alpha:  0.302327548295781"
[1] "G6:  0.300586707399654"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores"
[1] "raw alpha:  0.324534998859446"
[1] "std alpha:  0.334569126014373"
[1] "G6:  0.339695875259984"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault"
[1] "raw alpha:  0.362138055154794"
[1] "std alpha:  0.368147573874696"
[1] "G6:  0.373349602531565"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores"
[1] "raw alpha:  0.425811504626904"
[1] "std alpha:  0.423371808917866"
[1] "G6:  0.43728816390718"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito"
[1] "raw alpha:  0.430205936442333"
[1] "std alpha:  0.427062654790837"
[1] "G6:  0.441049823257986"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD"
[1] "raw alpha:  0.442939503710724"
[1] "std alpha:  0.445019868751639"
[1] "G6:  0.460065992278836"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura"
[1] "raw alpha:  0.480111673092193"
[1] "std alpha:  0.477223315381546"
[1] "G6:  0.493574002893579"
[1] "Colunas:  dimensionamentoResponsivo familiaFonte tamanhoFonteBotoes capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito"
[1] "raw alpha:  0.475389168221924"
[1] "std alpha:  0.479789801942754"
[1] "G6:  0.496439276041331"
[1] "Colunas:  dimensionamentoResponsivo capitalizacaoSentenca caixaAltaBotao tamanhoFonteComp textoDefault doisPontosLegenda pontoFinalSentenca telasComOrganizadores dicaPreenchidaEDiferenteDefault sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito"
[1] "raw alpha:  0.485222151447592"
[1] "std alpha:  0.455904897706857"
[1] "G6:  0.444565006463084"
[1] "Colunas:  tamanhoFonteComp sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD tamanhoFonteSenhaCaixaTextoPintura contrasteEntreTextoFundoNegrito"
[1] "raw alpha:  0.508959491627164"
[1] "std alpha:  0.468376041451853"
[1] "G6:  0.434637284011105"
[1] "Colunas:  contrasteEntreTextoFundoNegrito sistemaDeCores contrasteEntreTextoFundoNaoNegrito iconesMD"
[1] "raw alpha:  0.465147676030119"
[1] "std alpha:  0.487650752057866"
[1] "G6:  0.541875425425788"
[1] "Colunas:  pixelizacao imagensEmBotoes componenteImagem imagensDeFundo alturaComponenteAlvoToque larguraComponenteAlvoToque"


----------------------------------------------------------------------------------------------
RESULTADO ALPHA GERAL 
----------------------------------------------------------------------------------------------


Call: alpha(x = df)

  raw_alpha std.alpha G6(smc) average_r  S/N    ase mean   sd median_r
      0.31      0.26    0.35     0.016 0.35 0.0033  1.6 0.28    0.011

 lower alpha upper     95% confidence boundaries
0.3 0.31 0.32 

 Reliability if an item is dropped:
                                   raw_alpha std.alpha G6(smc) average_r  S/N alpha se  var.r  med.r
dimensionamentoResponsivo               0.32      0.28    0.36     0.018 0.39   0.0032 0.0080 0.0127
familiaFonte                            0.30      0.26    0.35     0.016 0.35   0.0033 0.0082 0.0112
tamanhoFonteBotoes                      0.31      0.27    0.36     0.018 0.38   0.0032 0.0083 0.0121
capitalizacaoSentenca                   0.32      0.26    0.34     0.016 0.35   0.0032 0.0078 0.0109
caixaAltaBotao                          0.34      0.28    0.37     0.018 0.39   0.0031 0.0080 0.0127
tamanhoFonteComp                        0.30      0.25    0.33     0.015 0.33   0.0033 0.0078 0.0109
textoDefault                            0.31      0.27    0.36     0.018 0.38   0.0032 0.0081 0.0134
doisPontosLegenda                       0.33      0.28    0.36     0.018 0.39   0.0032 0.0081 0.0141
pontoFinalSentenca                      0.30      0.25    0.34     0.015 0.33   0.0033 0.0080 0.0109
telasComOrganizadores                   0.33      0.26    0.34     0.017 0.36   0.0032 0.0075 0.0105
dicaPreenchidaEDiferenteDefault         0.29      0.24    0.33     0.015 0.32   0.0033 0.0080 0.0110
sistemaDeCores                          0.27      0.22    0.30     0.013 0.28   0.0034 0.0070 0.0101
contrasteEntreTextoFundoNaoNegrito      0.26      0.22    0.30     0.013 0.28   0.0035 0.0069 0.0116
iconesMD                                0.31      0.27    0.36     0.017 0.37   0.0033 0.0082 0.0127
tamanhoFonteSenhaCaixaTextoPintura      0.30      0.25    0.34     0.015 0.33   0.0033 0.0080 0.0112
contrasteEntreTextoFundoNegrito         0.27      0.23    0.32     0.014 0.30   0.0034 0.0075 0.0116
pixelizacao                             0.23      0.21    0.29     0.012 0.26   0.0036 0.0072 0.0099
imagensEmBotoes                         0.25      0.24    0.31     0.015 0.31   0.0036 0.0064 0.0090
componenteImagem                        0.30      0.25    0.34     0.016 0.34   0.0033 0.0081 0.0108
imagensDeFundo                          0.29      0.24    0.33     0.015 0.31   0.0033 0.0082 0.0105
alturaComponenteAlvoToque               0.32      0.25    0.32     0.016 0.34   0.0032 0.0062 0.0129
larguraComponenteAlvoToque              0.31      0.25    0.32     0.016 0.33   0.0032 0.0063 0.0119

 Item statistics 
                                       n raw.r std.r   r.cor  r.drop  mean   sd
dimensionamentoResponsivo          88861 0.099  0.16 -0.0328 -0.0439 0.145 0.64
familiaFonte                       78549 0.174  0.22  0.0726  0.0661 2.865 0.56
tamanhoFonteBotoes                 67514 0.082  0.18 -0.0119 -0.0034 0.053 0.35
capitalizacaoSentenca              88402 0.186  0.22  0.0950  0.0116 1.383 1.14
caixaAltaBotao                     67514 0.203  0.15 -0.0500 -0.0657 0.552 1.06
tamanhoFonteComp                   57829 0.342  0.26  0.1597  0.0889 2.137 1.15
textoDefault                       77735 0.182  0.18 -0.0011  0.0150 2.766 0.66
doisPontosLegenda                  53747 0.130  0.15 -0.0424 -0.0356 2.602 0.79
pontoFinalSentenca                 88589 0.104  0.26  0.1480  0.0855 2.864 0.37
telasComOrganizadores              88861 0.354  0.20  0.0801  0.0190 1.561 1.41
dicaPreenchidaEDiferenteDefault    28294 0.405  0.27  0.1792  0.0977 0.946 1.33
sistemaDeCores                     88861 0.279  0.35  0.3547  0.2024 2.609 0.87
contrasteEntreTextoFundoNaoNegrito 70729 0.427  0.35  0.3500  0.1882 2.054 1.33
iconesMD                           17953 0.083  0.19  0.0249  0.0236 0.042 0.32
tamanhoFonteSenhaCaixaTextoPintura 52912 0.263  0.26  0.1529  0.1015 2.797 0.69
contrasteEntreTextoFundoNegrito    31994 0.415  0.31  0.2563  0.1562 2.234 1.24
pixelizacao                        45940 0.449  0.38  0.4109  0.2534 1.986 1.26
imagensEmBotoes                    22188 0.464  0.29  0.2833  0.2085 1.383 1.39
componenteImagem                   23597 0.310  0.24  0.1126  0.0844 2.406 1.11
imagensDeFundo                     20321 0.255  0.29  0.1929  0.1060 0.479 0.69
alturaComponenteAlvoToque          85863 0.130  0.24  0.2139 -0.0352 0.050 0.37
larguraComponenteAlvoToque         85863 0.143  0.26  0.2418 -0.0162 0.055 0.40

Non missing response frequency for each item
                                      0    1    2    3 miss
dimensionamentoResponsivo          0.95 0.00 0.00 0.05 0.00
familiaFonte                       0.03 0.01 0.03 0.93 0.12
tamanhoFonteBotoes                 0.97 0.01 0.01 0.01 0.24
capitalizacaoSentenca              0.28 0.32 0.14 0.26 0.01
caixaAltaBotao                     0.76 0.05 0.06 0.13 0.24
tamanhoFonteComp                   0.17 0.10 0.17 0.57 0.35
textoDefault                       0.04 0.02 0.08 0.86 0.13
doisPontosLegenda                  0.04 0.07 0.13 0.76 0.40
pontoFinalSentenca                 0.00 0.01 0.12 0.87 0.00
telasComOrganizadores              0.41 0.08 0.05 0.46 0.00
dicaPreenchidaEDiferenteDefault    0.64 0.05 0.03 0.28 0.68
sistemaDeCores                     0.07 0.05 0.09 0.80 0.00
contrasteEntreTextoFundoNaoNegrito 0.29 0.00 0.09 0.63 0.20
iconesMD                           0.98 0.01 0.00 0.01 0.80
tamanhoFonteSenhaCaixaTextoPintura 0.05 0.02 0.03 0.91 0.40
contrasteEntreTextoFundoNegrito    0.23 0.00 0.08 0.69 0.64
pixelizacao                        0.24 0.08 0.13 0.55 0.48
imagensEmBotoes                    0.46 0.08 0.08 0.38 0.75
componenteImagem                   0.15 0.04 0.06 0.75 0.73
imagensDeFundo                     0.61 0.34 0.03 0.03 0.77
alturaComponenteAlvoToque          0.98 0.00 0.00 0.01 0.03
larguraComponenteAlvoToque         0.98 0.00 0.00 0.02 0.03


--------------------------------------------------------------------------------

alpha(df, check.keys=TRUE)

Reliability analysis   
Call: alpha(x = df, check.keys = TRUE)

  raw_alpha std.alpha G6(smc) average_r  S/N    ase mean   sd median_r
      0.46      0.47    0.52     0.039 0.88 0.0025  2.3 0.36    0.024

 lower alpha upper     95% confidence boundaries
0.46 0.46 0.47 

 Reliability if an item is dropped:
                                   raw_alpha std.alpha G6(smc) average_r  S/N alpha se  var.r med.r
dimensionamentoResponsivo-              0.45      0.45    0.51     0.038 0.82   0.0026 0.0069 0.022
familiaFonte                            0.46      0.46    0.52     0.039 0.86   0.0026 0.0069 0.022
tamanhoFonteBotoes-                     0.46      0.47    0.52     0.041 0.89   0.0026 0.0069 0.025
capitalizacaoSentenca                   0.45      0.45    0.50     0.038 0.83   0.0026 0.0066 0.022
caixaAltaBotao-                         0.45      0.45    0.51     0.038 0.83   0.0026 0.0069 0.022
tamanhoFonteComp                        0.44      0.44    0.50     0.036 0.79   0.0027 0.0067 0.022
textoDefault-                           0.46      0.47    0.52     0.040 0.88   0.0026 0.0068 0.025
doisPontosLegenda-                      0.47      0.48    0.53     0.043 0.93   0.0025 0.0067 0.026
pontoFinalSentenca                      0.45      0.45    0.51     0.038 0.83   0.0026 0.0068 0.022
telasComOrganizadores-                  0.46      0.46    0.51     0.039 0.85   0.0026 0.0063 0.023
dicaPreenchidaEDiferenteDefault-        0.47      0.46    0.51     0.039 0.86   0.0025 0.0066 0.024
sistemaDeCores                          0.42      0.44    0.49     0.036 0.78   0.0027 0.0059 0.023
contrasteEntreTextoFundoNaoNegrito      0.40      0.43    0.48     0.034 0.75   0.0028 0.0059 0.022
iconesMD-                               0.46      0.47    0.52     0.041 0.89   0.0026 0.0068 0.024
tamanhoFonteSenhaCaixaTextoPintura      0.45      0.46    0.51     0.038 0.84   0.0026 0.0067 0.022
contrasteEntreTextoFundoNegrito         0.43      0.44    0.50     0.036 0.79   0.0027 0.0064 0.022
pixelizacao                             0.46      0.47    0.51     0.041 0.89   0.0025 0.0057 0.026
imagensEmBotoes                         0.44      0.45    0.49     0.037 0.81   0.0026 0.0053 0.023
componenteImagem                        0.45      0.46    0.51     0.039 0.85   0.0026 0.0068 0.025
imagensDeFundo-                         0.47      0.48    0.53     0.042 0.92   0.0025 0.0067 0.025
alturaComponenteAlvoToque-              0.46      0.45    0.49     0.038 0.83   0.0026 0.0050 0.026
larguraComponenteAlvoToque-             0.46      0.45    0.49     0.038 0.82   0.0026 0.0051 0.025

 Item statistics 
                                       n raw.r std.r  r.cor  r.drop mean   sd
dimensionamentoResponsivo-         88861 0.220  0.31 0.2266  0.1521 2.85 0.64
familiaFonte                       78549 0.200  0.27 0.1543  0.1040 2.86 0.56
tamanhoFonteBotoes-                67514 0.137  0.22 0.0877  0.0579 2.95 0.35
capitalizacaoSentenca              88402 0.505  0.30 0.2268  0.1427 1.38 1.14
caixaAltaBotao-                    67514 0.364  0.31 0.2153  0.1541 2.45 1.06
tamanhoFonteComp                   57829 0.443  0.36 0.3014  0.1920 2.14 1.15
textoDefault-                      77735 0.249  0.24 0.1187  0.0943 0.23 0.66
doisPontosLegenda-                 53747 0.196  0.16 0.0057 -0.0128 0.40 0.79
pontoFinalSentenca                 88589 0.249  0.31 0.2272  0.1744 2.86 0.37
telasComOrganizadores-             88861 0.496  0.27 0.1896  0.1238 1.44 1.41
dicaPreenchidaEDiferenteDefault-   28294 0.380  0.26 0.1581  0.0762 2.05 1.33
sistemaDeCores                     88861 0.391  0.38 0.3523  0.2967 2.61 0.87
contrasteEntreTextoFundoNaoNegrito 70729 0.514  0.43 0.4228  0.3044 2.05 1.33
iconesMD-                          17953 0.092  0.22 0.0873  0.0614 2.96 0.32
tamanhoFonteSenhaCaixaTextoPintura 52912 0.241  0.30 0.2064  0.1206 2.80 0.69
contrasteEntreTextoFundoNegrito    31994 0.441  0.36 0.3055  0.2208 2.23 1.24
pixelizacao                        45940 0.285  0.22 0.1443  0.1065 1.99 1.26
imagensEmBotoes                    22188 0.450  0.34 0.3248  0.1810 1.38 1.39
componenteImagem                   23597 0.316  0.27 0.1704  0.1255 2.41 1.11
imagensDeFundo-                    20321 0.141  0.18 0.0341  0.0089 2.52 0.69
alturaComponenteAlvoToque-         85863 0.160  0.31 0.2890  0.1143 2.95 0.37
larguraComponenteAlvoToque-        85863 0.172  0.32 0.3013  0.1208 2.95 0.40

Non missing response frequency for each item
                                      0    1    2    3 miss
dimensionamentoResponsivo          0.95 0.00 0.00 0.05 0.00
familiaFonte                       0.03 0.01 0.03 0.93 0.12
tamanhoFonteBotoes                 0.97 0.01 0.01 0.01 0.24
capitalizacaoSentenca              0.28 0.32 0.14 0.26 0.01
caixaAltaBotao                     0.76 0.05 0.06 0.13 0.24
tamanhoFonteComp                   0.17 0.10 0.17 0.57 0.35
textoDefault                       0.04 0.02 0.08 0.86 0.13
doisPontosLegenda                  0.04 0.07 0.13 0.76 0.40
pontoFinalSentenca                 0.00 0.01 0.12 0.87 0.00
telasComOrganizadores              0.41 0.08 0.05 0.46 0.00
dicaPreenchidaEDiferenteDefault    0.64 0.05 0.03 0.28 0.68
sistemaDeCores                     0.07 0.05 0.09 0.80 0.00
contrasteEntreTextoFundoNaoNegrito 0.29 0.00 0.09 0.63 0.20
iconesMD                           0.98 0.01 0.00 0.01 0.80
tamanhoFonteSenhaCaixaTextoPintura 0.05 0.02 0.03 0.91 0.40
contrasteEntreTextoFundoNegrito    0.23 0.00 0.08 0.69 0.64
pixelizacao                        0.24 0.08 0.13 0.55 0.48
imagensEmBotoes                    0.46 0.08 0.08 0.38 0.75
componenteImagem                   0.15 0.04 0.06 0.75 0.73
imagensDeFundo                     0.61 0.34 0.03 0.03 0.77
alturaComponenteAlvoToque          0.98 0.00 0.00 0.01 0.03
larguraComponenteAlvoToque         0.98 0.00 0.00 0.02 0.03
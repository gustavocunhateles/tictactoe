var TIC = {}; //crio o objeto que contera tudo sobre o sistema.

TIC.Functions = {} //crio o objeto que irá conter todas as funcções
TIC.Handlers = {} // crio o objeto que irá conter todas as funções de eventos. Exe: Click, MouseDown e etc;
TIC.Variables = {}// crio o objeto que irá armazenar Variables globais.
TIC.Prototypes = {}// Armazena prototipos de estrutura de dados...como a estrutura de dados de uma casa, porr exemplo.
TIC.Helpers = {}
TIC.HTML = {};//Irá armazenaar conteudos \HTML que irei necessitar para o funcionamento do programa.
TIC.HTML.Elements = {} //Armazena os elementos HTML que irei utilizar, como: Cenário, peças e tudo mais.
TIC.HTML.Templates = {}// Armazena string que são transformadas em HTML e exibidas na interface.

// ------------------------------------------------FUNÇÕES------------------------------------------

// Algoritmo AlfaBeta --------- :)
TIC.Functions.AlfaBeta = function()
{

	/*
		PRIMEIRO LEIA ISSO:

		AlfaBetaPruning - JavaScript - TIC TAC TOE
		O Algoritmo foi construido seguindo os seguintes passos:

		1 - Pegar a arvore que corresponde ao estado atual
		2 - Chamo a Função : AlfaBetaBrain, passando como parametro a arvore do esado atual, 0 como depth, -9999 como alfa e 9999 como beta
		3 - AlfaBetaBrain:
			3.1 - Verificar se a arovre/nó recebido possui filhos: Se não possuir, retorna a pontuação desta arvore;
			3.2 - Verificar se a profundidade da arvore recebida é igual a profudidade máxima: Se for, retorna a pontuação desta arvore
			3.3 - Se o jogador da tree atual for o jogador min:

				3.3.1 - pBeta recebe infinito e para cada nó filho, AlfaBetaBrain será envocada recebendo como parametro cada um dos nó filhos e depth + 1.
				3.3.2 - Se o retorno de AlfaBetaBrain for menor do que o pBeta atual, o pBeta recebe o valor de retorno;
				3.3.3 - Se o pBeta for menor do que beta, beta recebe Pbeta;
				3.3.4 - Se alfa for maior ou iggual a beta, retorne alfa;
				3.3.4 - Após o for, return pBeta;

			3.4 - Se o jogador da tree atual for o jogador Max:

				3.4.1 - pAlfa recebe infinito e para cada nó filho, AlfaBetaBrain será envocada recebendo como parametro cada um dos nó filhos e depth + 1.
				3.4.2 - Se o retorno de AlfaBetaBrain for maior do que o pAlfa atual, o pAlfa recebe o valor de retorno;
				3.4.3 - Se o pAlfa for maior do que alfa, alfa recebe Palfa;
				3.4.4 - Se depth == 0 e a condição 3.4.3, best recebe o filho enviado como parametro para AlfaBetaBrain;
				3.4.4 - Se alfa for maior ou iggual a beta, retorne beta;
				3.4.4 - Após o for, return pAlfa;

			OBS.: Para cada nó expandido existe um somador que incrementa toda a vez que um nó é expandido	
		4 - Após o retorno de AlfaBetaBrain, a melhor jogada vai estar armazenada em best.	

	*/

	var root = TIC.Functions.GenerateTree();
	var best = null;
	var bestID = null;
	var nodesPath = 0;

	this.AlfaBetaBrain =  function(tree, depth, alfa, beta)	
	{
		if(tree.nodes.length == 0 || (TIC.Variables.MaxDepth != "Max" && depth > TIC.Variables.MaxDepth))
		{	
			return tree.pontuation
		}
		else if(tree.nextPlayer == TIC.Variables.Min)	
		{
			var pBeta = 9999;
			for(var i = 0; i < tree.nodes.length; i++)
			{
				pBeta =  Math.min(this.AlfaBetaBrain(tree.nodes[i], depth+1, alfa, beta));
				if(pBeta < beta)
				{
					beta = pBeta;
				}
				nodesPath++;
				if(alfa >= beta)
					return alfa;
			}
			return pBeta;
		}
		else if(tree.nextPlayer == TIC.Variables.Max)
		{
			var pAlfa = -9999;
			for(var i = 0; i < tree.nodes.length; i++)
			{
				pAlfa = Math.max(this.AlfaBetaBrain(tree.nodes[i], depth+1, alfa, beta));
				if(pAlfa > alfa)
				{
					if(depth == 0)
					{
						best = tree.nodes[i];
					}
					alfa = pAlfa;
				}
				nodesPath++;
				if(alfa >= beta)
					return beta;
			}
			return pAlfa;
		}
	}

	this.AlfaBetaBrain(root, 0, -9999, 9999)
	TIC.HTML.Templates.NodesPath(nodesPath);
	TIC.HTML.Templates.Ramification(root.nodes.length);

	bestID  = TIC.Variables.Board.houses[ best.played[ best.played.length -1 ]].id;
	TIC.Functions.NewMove(bestID);
}

// Está função é responsavél por analisar se o jogo acabou ou não.
TIC.Functions.CheckMatch = function(house, Board, simulation)
{
	var indice = house.i;//pega o indice da ultima casa jogada. Este indice é a posição da casa no Board.houses.
	var houses = Board.houses;// pega todas as casas da BOARD
	var winHouses = [];//Armazena as trÊs casas que contemplam a vitória
	var player = house.player;// armazena qual é o jogador que fez a ultima jogada
	var end = false;// será true se uma das possíveis jogadas vitoriosas tiver aocntecido


	//Nos IF's abaixo, apenas comparo as possíveis jogadas virotiasas verificando se nas casas está o jogador que fez a ultima jogada. Se estiver, winHouses recebera as 3 casas;

	if(houses[0].player == house.player)
	{
		if(houses[1].player == player && houses[2].player == player && houses[3].player == player && houses[4].player == player)
		{
			winHouses.push(houses[0]);
			winHouses.push(houses[1]);
			winHouses.push(houses[2]);
			winHouses.push(houses[3]);
			winHouses.push(houses[4]);
			end = true;
		}
		else if(houses[5].player == player && houses[10].player == player && houses[15].player == player && houses[20].player == player)
		{
			winHouses.push(houses[0]);
			winHouses.push(houses[5]);
			winHouses.push(houses[10]);
			winHouses.push(houses[15]);
			winHouses.push(houses[20]);
			end = true;
		}
		else if(houses[6].player == player && houses[12].player == player && houses[18].player == player && houses[24].player == player)
		{
			winHouses.push(houses[0]);
			winHouses.push(houses[6]);
			winHouses.push(houses[12]);
			winHouses.push(houses[18]);
			winHouses.push(houses[24]);
			end = true;
		}
	}
	if(houses[5].player == house.player && !end)
	{
		if(houses[6].player == player && houses[7].player == player && houses[8].player == player && houses[9].player == player)
		{
			winHouses.push(houses[5]);
			winHouses.push(houses[6]);
			winHouses.push(houses[7]);
			winHouses.push(houses[8]);
			winHouses.push(houses[9]);
			end = true;
		}
	}
	if(houses[10].player == house.player && !end)
	{
		if(houses[11].player == player && houses[12].player == player && houses[13].player == player && houses[14].player == player)
		{
			winHouses.push(houses[10]);
			winHouses.push(houses[11]);
			winHouses.push(houses[12]);
			winHouses.push(houses[13]);
			winHouses.push(houses[14]);
			end = true;
		}
	}
	if(houses[15].player == house.player && !end)
	{
		if(houses[16].player == player && houses[17].player == player && houses[18].player == player && houses[19].player == player)
		{
			winHouses.push(houses[15]);
			winHouses.push(houses[16]);
			winHouses.push(houses[17]);
			winHouses.push(houses[18]);
			winHouses.push(houses[19]);
			end = true;
		}
	}
	if(houses[20].player == house.player && !end)
	{
		if(houses[21].player == player && houses[22].player == player && houses[23].player == player && houses[24].player == player)
		{
			winHouses.push(houses[20]);
			winHouses.push(houses[21]);
			winHouses.push(houses[22]);
			winHouses.push(houses[23]);
			winHouses.push(houses[24]);
			end = true;
		}
		else if(houses[16].player == player && houses[12].player == player && houses[8].player == player && houses[4].player == player)
		{
			winHouses.push(houses[20]);
			winHouses.push(houses[16]);
			winHouses.push(houses[12]);
			winHouses.push(houses[8]);
			winHouses.push(houses[4]);
			end = true;
		}
	}
 	if(houses[1].player == house.player && !end)
	{
		if(houses[6].player == player && houses[11].player == player && houses[16].player == player && houses[21].player == player)
		{
			winHouses.push(houses[1]);
			winHouses.push(houses[6]);
			winHouses.push(houses[11]);
			winHouses.push(houses[16]);
			winHouses.push(houses[21]);
			end = true;
		}
	}
	if(houses[2].player == house.player && !end)
	{
		if(houses[7].player == player && houses[12].player == player && houses[17].player == player && houses[22].player == player)
		{
			winHouses.push(houses[2]);
			winHouses.push(houses[7]);
			winHouses.push(houses[12]);
			winHouses.push(houses[17]);
			winHouses.push(houses[22]);
			end = true;
		}
	}
	if(houses[3].player == house.player && !end)
	{
		if(houses[8].player == player && houses[13].player == player && houses[18].player == player && houses[23].player == player)
		{
			winHouses.push(houses[3]);
			winHouses.push(houses[8]);
			winHouses.push(houses[13]);
			winHouses.push(houses[18]);
			winHouses.push(houses[23]);
			end = true;
		}
	}
	if(houses[4].player == house.player && !end)
	{
		if(houses[9].player == player && houses[14].player == player && houses[19].player == player && houses[24].player == player)
		{

			winHouses.push(houses[4]);
			winHouses.push(houses[9]);
			winHouses.push(houses[14]);
			winHouses.push(houses[19]);
			winHouses.push(houses[24]);
			end = true;
		}
	}

	if(end)//se o jogo tiver acabado
	{
		if(!simulation)// se não for uma simulação - DEPRECATED - simulation nunca é utilizado. Portanto, sempre entrara no if se o jogo tiver acabado.
		{
			for(var i = 0; i < winHouses.length; i++)//para cada uma das casas vitorias, estas casas serão pintadas.
				$("#"+ winHouses[i].id).css("background", "rgba(0,0,0,0.5)");
		}
		return "WIN";//retorna WIN para a função que chamou
	}
	else if(TIC.Variables.Board.free == 0)
		return "EMPATE"
	else
		return undefined;
}

//Função responsavél pro cria toda a arovre de jogadas do jogo
TIC.Functions.GenerateTree = function()
{
	/*
		Na função abaixo é realizado a criação da arvore de jogadas. O Estado inicial(root), não possui jogador, porém, todos os nodes de root são jogadas possíveis para MAX.
	*/
	var root = new TIC.Prototypes.Tree(); //instÂncia um novo objeto Tree.
	var generateNodes = function(treeRoot, depth)// Está função é responsavel por gerar os nodes filhos de uma dada arvore. FUNÇÃo Dentro de Uma Função BABY!!!!
	{		
		if(TIC.Variables.MaxDepth == "MAX" || depth < TIC.Variables.MaxDepth)
		{
			for(var i = 0; i < treeRoot.houses.length; i++)// para cada uma das casas livres que a arvore possui
			{
				var node =  new TIC.Prototypes.Tree();//instâncio uma nova tree
				var house = treeRoot.houses[i];// armazendo a casa

				for(var j = 0; j < treeRoot.houses.length; j++)//para cada uma das casas restantes, para o estado NODE, serão jogadas possíveis.
				{
					var nHouse = treeRoot.houses[j];
					if(house != nHouse)
						node.houses.push(nHouse);//adiciono ao node se for diferente de si mesma. Afinal, o node é uma nova arvore contendo novos possiveis estados.
				}
			
				if(treeRoot.player == null)//se o jogador for null (Só ocorre no primeiro estado possível do jogo que é: todas as casas vazias)
					node.nextPlayer = "King";//O proximo jogador será o REI, porque, sempre inicia-se pela rainha
				else
					node.nextPlayer = treeRoot.player;//O proximo jogador do estado NODE será o jogador do estado atual.

				node.player = treeRoot.nextPlayer;//O jogador atual do estado NODE será o jogador NEXT do estado atual;
				node.free = treeRoot.free - 1;// O número de casas livres no estado NODE é igual ao número de estados atuais livres -1.
				
				for(var j = 0; j < treeRoot.played.length; j++)//adiciono ao estado NODE todas as jogadas já feitas em estados PAIS de node.
					node.played.push(treeRoot.played[j]);

				node.played.push(house);//Adiciono a jogadas já feitas o estado node, a jogada que node representa, pois, estamos simulando que a jogada feita foi node.(Node é o estado resultante da ultima jogada feita.)

				node.depth = treeRoot.depth + 1;//A profundidade de node recebe +1;
				node.pontuation = TIC.Helpers.GetNodePontuation(node);// A função HELPER GetNodePontuation, pega a pontuação do estado node uma vez que a ultima jogada feita tenha sido node.
				node.parent = treeRoot;//Adiciono a referencia a node de que seu pai é o estado atual.
				treeRoot.nodes.push(node);//Adiciono node como um filho do estado atual(note que os filhos sao possiveis jogadas, e cada possivel joogada é uma nova arvore).

				if(node.pontuation == 0 && node.houses.length > 0 && node.free > 0 && (TIC.Variables.MaxDepth == "MAX" || depth < TIC.Variables.MaxDepth))// se a pontuação do estado node for 0 e ainda tiver casas livres, o estado NODE possiu filhos.
					generateNodes(node, depth + 1);// Reinvoco generateNodes para processar e salvar as arvores filhas de node.
				else
				{
					//caso o jogo tenha acabado com 1 ou -1
					var aux = node;
					node.end = true;
					while(aux != null)//Somo aos pais a pontuação deste estado node.
					{
						if(aux.parent == null)
							aux = null
						else
						{
							aux.parent.pontuation += aux.pontuation;
							aux = aux.parent;
						}
					}

				}
			}
			return treeRoot;// este retorno não está sendo utilizado.
		}
		else 
			return;
	}

	for(var i = 0; i < TIC.Variables.Board.houses.length; i++) // como o estado ROOT é o estado inicial, as casas disponiveis são TODAS.
	{
		var house = TIC.Variables.Board.houses[i];
		if(!house.ocupada)
			root.houses.push(house.i);
	}

	root.played = TIC.Variables.Moves;
	root.free = TIC.Variables.Board.free//Número de casas disponiveis
	root.nextPlayer = "Queen"//proximo jogador
	root.pontuation = 0;//pontuação do estado atual
	generateNodes(root, 0);//chamo a função responsavel por criar as arvores filho do estado atual. É a primeira iteração de generateNodes.;

	return root;//Nó cabeça da arvore
}

//Função que dado um ID acha e retorna  Objeto casa que possuia este ID;
TIC.Functions.GetHouseByID = function(ID)
{
	//COm base em um ID, eu busco entre todas as casas está casa. Se eu achar, retorno a casa;
	for(var i = 0; i < TIC.Variables.Board.houses.length; i++)
	{
		var house = TIC.Variables.Board.houses[i];
		if(house.id == ID)
			return house;
	}
}

//Função responsavél por iniciar e reinicar as variaveis do jogo
TIC.Functions.Init = function( newM )
{
	TIC.Handlers.GlobalProperties();
	TIC.Handlers.LoadBoard(newM); //Chama a função responsavél por gerenciar a interface e exibir corretamente os elementos que ela compõem.
	TIC.Functions.RandomPlayer();// Randomiza um jogador inicial. Porém, para titulo de teste, o jogador inicial sempre será a rainha.
	TIC.Functions.UpdateHouses();// A função LoadBoard, cria as casas, e está função e responsavél por adicionar as adjacencias de uma dada casa.

	if(!newM)// newM só é passado caso seja para reiniciar a partida
	{
		TIC.Handlers.Events();//Declara aos objetos da interface a quais eventos responderam e qqual função irá tratar este evento.
		TIC.HTML.Templates.NodesNumber();//Utilizado apeans para contar o número total de nós da arvore total de decições criadas acima.
		TIC.HTML.Templates.TreeDepth();//Utilizado apeans para contar o número total de nós da arvore total de decições criadas acima.
	}
}

//-----------Algoritmo MiniMax
TIC.Functions.MiniMax = function()
{
	/*
		PRIMEIRO LEIA ISSO:

		MiniMax - JavaScript - TIC TAC TOE
		O Algoritmo foi construido seguindo os seguintes passos:

		1 - Pegar a arvore que corresponde ao estado atual
		2 - Chamo a Função : MiniMaxBrain, passando como parametro a arvore do esado atual e 0 como depth
		3 - MiniMaxBrain:
			3.1 - Verificar se a arovre/nó recebido possui filhos: Se não possuir, retorna a pontuação desta arvore;
			3.2 - Verificar se a profundidade da arvore recebida é igual a profudidade máxima: Se for, retorna a pontuação desta arvore
			3.3 - Se o jogador da tree atual for o jogador min:

				3.3.1 - Alfa recebe infinito e para cada nó filho, MiniMaxBrain será envocada recebendo como parametro cada um dos nó filhos e depth + 1.
				3.3.2 - Se o retorno de MiniMaxBrain for menor do que o alfa atual, o alfa recebe o valor de retorno;
				3.3.3 - Retorna Alfa;
				
			3.4 - Se o jogador da tree atual for o jogador Max:

				3.4.1 - Alfa recebe -9999 e para cada nó filho, MiniMaxBrain será envocada recebendo como parametro cada um dos nó filhos e depth +1;
				3.4.2 - Se o retorno de MiniMaxBrain for mair do que o alfa atua, o alfa recebe o valor de retorno;
				3.4.3 - Se o depth == 0 e a condição 3.4.2, a melhor jogada é o filho passado para MiniMaxBrain na interação atual;
				3.4.4 retorna alfa;

			OBS.: Para cada nó expandido existe um somador que incrementa toda a vez que um nó é expandido	
		4 - Após o retorno de MiniMaxBrain, a melhor jogada vai estar armazenada em best.	

	*/

	// Para gerar o estado do tabuleiro corrente e pegar a arvore de resultados.
	var root = TIC.Functions.GenerateTree();
	var best = null;
	var bestID = null;
	var nodesPath = 0;

	this.MiniMaxBrain = function(tree, depth)
	{
		if(tree.nodes.length == 0 || (TIC.Variables.MaxDepth != "Max" && depth > TIC.Variables.MaxDepth))
			return tree.pontuation;
		else if(tree.nextPlayer == TIC.Variables.Min)
		{
			var alfa = 9999;
			for(var i = 0; i < tree.nodes.length; i++)
			{
				var nAlfa = this.MiniMaxBrain(tree.nodes[i], depth + 1);
				if(nAlfa < alfa)
				{
					alfa = nAlfa;
				}
				nodesPath++;
			}
			return alfa;
		}
		else
		{
			var alfa = -9999;

			for(var i = 0; i < tree.nodes.length; i++)
			{
				var nAlfa = this.MiniMaxBrain(tree.nodes[i], depth + 1);
				if(nAlfa > alfa)
				{
					if(depth == 0)
						best = tree.nodes[i];
					alfa = nAlfa;
				}
				nodesPath++;
			}
			return alfa;
		}
	}
	this.MiniMaxBrain(root, 0)
	TIC.HTML.Templates.NodesPath(nodesPath);
	TIC.HTML.Templates.Ramification(root.nodes.length);

	bestID  = TIC.Variables.Board.houses[ best.played[ best.played.length -1 ]].id;// BestID recebe o ID corresponde da jogada BEST e passa para a função NewMove realizar a jogada;
	TIC.Functions.NewMove(bestID);
}

//Está função é utilizada para reiniciar o jogo
TIC.Functions.NewMatch = function()
{
	//Interface
	$(".house-TIC").each(function(i)
	{
		$(this).empty();
		$(this).css("background", "rgba(178,34,34,0.6)");
	})

	TIC.Variables.endGame = false;//Como é um jogo novo, o ENdGAME é false
	TIC.Variables.Moves = [];//As jogadas já feitas para o novo jogo é []
	
	//INTERFACE
	if(TIC.Variables.lastMove != null)
		TIC.Variables.lastMove.elem.css("text-shadow", "none");

	delete TIC.Variables.Board;// removo da memória o objeto que contia as casas e seus respectivos estados
	TIC.Functions.Init(true);	//Reinicio o programa
	if(TIC.Functions.CPUPlay)// Se o CPU for fazer jogada, já realizo o primeiro movimento.
		TIC.Functions.PlayAs();
}

//Recebe o ID da ultima jogada feita e processa algumas informações...
TIC.Functions.NewMove = function( ID )
{
	var house = TIC.Functions.GetHouseByID(ID);//Pega com base no ID, o objeto que representa está casa/jogada
	if(!house.ocupada)//Se a casa não estiver ocupada
	{
		//INterface
		if(TIC.Variables.lastMove != null)// se já tiver havido alguma jogada
			TIC.Variables.lastMove.elem.css("text-shadow", "none");

		house.ocupada = true;//A casa agora está ocupada
		house.player = TIC.Variables.Player;//E o jogador é o jogador da vez
		house.elem.css("text-shadow", "0px 0px 10px rgb(0,255,255)");//Interface
		TIC.Variables.lastMove = house;//Armazena a ultima jogada feita
		TIC.HTML.Templates.Icon(house);//INTERFACE

		TIC.Variables.Moves.push(house.i);//Armazena no array de jogadas feitas a jogada atual
		TIC.Variables.Board.free--;//O número de possíveis jogadas é decrementado

		var endGame = TIC.Functions.CheckMatch(house, TIC.Variables.Board);//Faz avaliação da jogada atual afim de verificar se a mesma resulta em uma vitória, empate ou nada
		TIC.Variables.endGame = endGame;//armazena o ultimo status da partida

		if(endGame && endGame != "EMPATE")//Se tiver havido vitóia
			alert("O Jogo acabou. O jogador: "+ house.player +", ganhou!")
		else if(endGame == "EMPATE")//se tiver ocorrido um empate
			alert("Não houve vencedores :(!");

		//O jogador da vez muda.
		if(TIC.Variables.Player == "King")
			TIC.Variables.Player = "Queen";
		else
			TIC.Variables.Player = "King"
	}
	else
		alert("Essa casa já tem peça!");
}

//Chama e faz uma jogada com o algoritmo selecioando.
TIC.Functions.PlayAs = function()
{
	if(TIC.Variables.Player == TIC.Variables.Max)//Se o jogador atual for o CPU
	{
		if(TIC.Variables.Algorithm == "MiniMax")//Se o algoritmo selecionado for minimax, a jogada será feita com MINI MAX
			TIC.Functions.MiniMax();
		else if(TIC.Variables.Algorithm == "AlfaBeta")//Se não se o algorimo for alfabtea, a jogada será feita com o ALFA BETA
			TIC.Functions.AlfaBeta();
	}
}

//Apenas setá que o jogador inicial é a Queen
TIC.Functions.RandomPlayer = function()
{
	// var rand =  Math.floor(Math.random*2 +1);
	// if(rand == 1)
	// 	TIC.Variables.Player = "King";
	// else
		TIC.Variables.Player = "Queen";
}

//Função para setar o algoritmo a ser usado pelo CPU - INTERFACE APENAS
TIC.Functions.SetAlgorithm = function(algorithm, elem)
{
	TIC.Variables.Algorithm =  algorithm;//Armazeno o algoritmo selecionado

	if(!TIC.Variables.AlgorithmElem)//Se não haver algum algoritmo selecionado
		TIC.Variables.AlgorithmElem = elem;//O algoritmo selecionado é o atual
	else // se não
		TIC.Variables.AlgorithmElem.css("background", "none");

	if(elem)
	{
		TIC.Variables.AlgorithmElem = elem;	
		elem.css("background", "rgba(75,0,130,0.8)");
		TIC.HTML.Templates.AlgorithmName();
	}
	else
		TIC.Variables.Algorithm = "";
}

//Função responsvél por vincular uma a estrura casa ao elemento casa do HTML correspondente;
TIC.Functions.UpdateHouses = function()
{
	for(var i = 0; i < TIC.Variables.Board.length; i++)
	{
		var house = TIC.Variables.Board[i];
		house.elem = $("#"+ house.id);
	}
}

//-----------------------------------------------Handler---------------------------------------------

//Função responsavél por controlar e tercerizar tratamentos de eventos da interface
TIC.Handlers.Events = function()
{
	//Quando um CLICK for feito na casa(INterface)
	$(".house-TIC").on("click", function(ev)
	{
		ev.preventDefault();
		if(!TIC.Variables.endGame && TIC.Variables.Player == "King")// Se o jogo não tiver acabado e o jogador da vez for o Jogador Humano
		{
			TIC.Functions.NewMove($(this).attr("id"));// Mando para função responsavél o ID da casa que o jogador clicou

			setTimeout(function()
			{
				if(!TIC.Variables.endGame)	//após a jogada verifico se o jogo acabou, se não tiver acabado o computador irá fazer uma jogada com base no algoritmo selecioando
					TIC.Functions.PlayAs();
			}, 300)
		}
		else if(TIC.Variables.Player != "King")
		{
			alert('Por gentileza, selecione um algoritmo e aperte "Play"')
		}
	})


	//Apenas para efeitos visuais na interface.
	$(".house-TIC").on("mouseenter", function(ev)
	{
		ev.preventDefault();
		if(!TIC.Variables.endGame)
		{
			$(this).css({
				"background": "rgba(178,34,34,0.8)",	
			})

			if(TIC.Variables.lastMove && TIC.Variables.lastMove.id != $(this).attr("id"))
			{
				$(this).css({
					"text-shadow": "0px 0px 5px white"
				})
			}


			$(this).unbind("mouseleave").on("mouseleave", function(ev)
			{
				if(!TIC.Variables.endGame)
				{
					$(this).css({
						"background": "rgba(178,34,34,0.6)"
					})

					if(TIC.Variables.lastMove && TIC.Variables.lastMove.id != $(this).attr("id"))
					{
						$(this).css({
							"text-shadow": "none"	
						})
					}
				}
				else
					$(this).unbind("mouseleave");

			})
		}
		else
		{
			$(this).unbind("mouseleave");
		}
	})


	//Função controladora dos eventos de click em elementos da nav-bar
	$(".nav-bar-TIC-Mid-Tool").on("click", function(ev)
	{
		ev.preventDefault();
		var type = $(this).attr("data-type");//Pego o tipo do objeto clicado na interface
		if(type == "new-match")//se for para reinicar o jogo
			TIC.Functions.NewMatch();
		else if(type == "AlfaBeta")//se for para selecionar  algoritimo alfabeta
			TIC.Functions.SetAlgorithm("AlfaBeta", $(this))
		else if(type == "MiniMax")//se for para selecionar o algoritimo minimax
			TIC.Functions.SetAlgorithm("MiniMax", $(this))
		else if(type == "play")// se for para inicar as jogadas do coputador
		{
			if(!TIC.Variables.Algorithm || TIC.Variables.Algorithm == "")// Para inicar jgoadas do computador, antes, o usuário deve ter selecionado um algoritmo
				alert("Primeiro Selecione o Algoritmo!")
			else
			{
				$(this).hide();//Escondo o botão de PLAY
				$("#TIC-Stop").show();//Exibo o botão de STOP
				TIC.Functions.CPUPlay = true;//Como apertei play, o CPU é o adversário
				TIC.Functions.PlayAs();//Mando o CPU fazer a jogada
			}
		}
		else if(type == "stop")//se for para parar a execução de jogadas por parte da CPU
		{
			TIC.Functions.SetAlgorithm(null);//Seto que o algoritmo escolhido agora é NULL
			TIC.Functions.CPUPlay = false;//O CPU não está mais jogadno
			$(this).hide();//Escondo o botão de STOP
			$("#TIC-Play").show();//Exibo o botão PLAY;
		}
	})
}

//Função que define variavéis globais e propriedades comuns :)
TIC.Handlers.GlobalProperties = function()
{
	TIC.Variables.Board = new TIC.Prototypes.Board(); // Instância um objeto do tipo BOARD.
	TIC.Variables.Tree = new TIC.Prototypes.Tree();// InstÂncia o Objeto Tree.
	TIC.Variables.Max = "Queen";//O jogador MAX será a Queen
	TIC.Variables.Min = "King";//O Jogador Min será o King
	TIC.Variables.Moves = [];//Armazena as jogadas realizadas no estado atual
	TIC.Variables.MaxDepth = 3;//Tamanho máximo que será gerado em arvore para cada estado qualquer
	TIC.Variables.BoardSize = 5;//Tamanho da mexa TIC.Variables.BoardSize x TIC.Variables.BoardSize
	TIC.Variables.NodesCount = Math.Fact( Math.pow(TIC.Variables.BoardSize, 2));//Apenas para exibir na interface qual é o número total de nós da arvore.

	// Armazena os Objetos Html como um todo;
	TIC.HTML.Elements.Scene = $(".scene-TIC");
	TIC.HTML.Elements.Board = $(".board-TIC");
	TIC.HTML.Elements.BoardInfo = $(".board-TIC-info");
	TIC.HTML.Elements.Houses = $(".houses-TIC");
}

//Função responsavél por setar margens dos elementos e inicar algumas variaveis
TIC.Handlers.LoadBoard = function(newM)
{
 
 	//Seta o tamanho da interface, bem como, tabuleiro e informações.
	TIC.HTML.Elements.Scene.css("margin-top", (TIC.HTML.Elements.Scene.parent().height() - TIC.HTML.Elements.Scene.height())/2)
	TIC.HTML.Elements.Scene.css("margin-left", (TIC.HTML.Elements.Scene.parent().width() - TIC.HTML.Elements.Scene.width())/2);
	
	//Função responsavél em desenhar as casas com base no tamanho mesa;
	if(!newM)
		TIC.HTML.Templates.Houses();

	//Para cada casa existente na interface
	$(".house-TIC").each(function(i)
	{
		var house = new TIC.Prototypes.House();//Instância um objeto do tipo casa

		house.id = "House-"+ i;//O id dos elementos da interface são formados por: House-i, onde i, é a posíção no array
		house.i = i; //O indice de I
		house.elem = $(this); // Salva no Objeto casa, a referência do objeto na interface.

		TIC.Variables.Board.houses.push( house );//adiciono na Board a casa e incremento o número de casas disponíveis
		TIC.Variables.Board.free++;

		$(this).attr("id", house.id);//Seto o ID da casa na interface.
	})
}


//----------------------------------------Funções HELPERS - Helpers são funções que auxilian funções principais. Elas existem apenas para não ficar muito carregado de código uma dada função.

//Função serve apenas para fazer a pontuação do nó. Ela simula como se fosse o CHECKMATCH
TIC.Helpers.GetNodePontuation = function(node)
{
	var ret = 0;//Valor a ser retornado -1, 0, 1

	var index = node.played[node.played.length -1];//Indice da ultima jogada feita (jogada que gerou este nó)
	var simBoard = [];//Contém uma BOARD simulada para fazer a pontuação do NÒ.
	var player = null;
	var end = false;// Se tiver havido um ganhador

	for(var i = 0; i < 25; i++)//crio 25 casas na board de simulação.
		simBoard.push({player:null})

	if(node.played.length > 4) // para haver um vitorioso, o minimo de jogadas que devem ter sido feitas é 5.
	{
		for(var i = 0; i < node.played.length; i++)// para cada jogada feita, é simulado na simBoard.
		{
			var tPlayer = null;
			var tPlayed = node.played[i];
			if(i % 2 == 0)//se a posição for par, é o jogador Queen, uma vez que ele iniciou o estado 0.
				tPlayer = "Queen"
			else
				tPlayer = "King"

			simBoard[ tPlayed ].player = tPlayer;
		}

		player = simBoard[index].player;//pego o ultimo jogador que realizou a jogada. --- Poderia ser substituido por: node.player

		//faço as comparações para ver se existem três casas que descrevam uma vitória.
		if(simBoard[0].player == player )
		{
			if(simBoard[6].player == player && simBoard[12].player == player && simBoard[18].player == player && simBoard[24].player == player)
			{
				ret = 1;
				end = true;
			}
			else if(simBoard[1].player == player && simBoard[2].player == player && simBoard[3].player == player && simBoard[4].player == player)
			{
				ret = 1;
				end = true;
			}
			else if(simBoard[5].player == player && simBoard[10].player == player && simBoard[15].player == player && simBoard[20].player == player)
			{
				ret = 1;
				end = true;
			}
		}
		if(simBoard[5].player == player && !end)
		{
			if(simBoard[6].player == player && simBoard[7].player == player && simBoard[8].player == player && simBoard[9].player == player)
			{
				ret = 1;
				end = true;
			}
		}
		if(simBoard[10].player == player && !end)
		{
			if(simBoard[11].player == player && simBoard[12].player == player && simBoard[13].player == player && simBoard[14].player == player)
			{
				ret = 1;
				end = true;
			}
		}
		if(simBoard[15].player == player && !end)
		{
			if(simBoard[16].player == player && simBoard[17].player == player && simBoard[18].player == player && simBoard[19].player == player)
			{
				ret = 1;
				end = true;
			}
		}
		if(simBoard[20].player == player && !end)
		{
			if(simBoard[16].player == player && simBoard[12].player == player && simBoard[8].player == player && simBoard[4].player == player)
			{
				ret = 1;
				end = true;
			}
			else if(simBoard[21].player == player && simBoard[22].player == player && simBoard[23].player == player && simBoard[24].player == player)
			{
				ret = 1;
				end = true;
			}
		}
		if(simBoard[1].player == player && !end)
		{
			if(simBoard[6].player == player && simBoard[11].player == player && simBoard[16].player == player && simBoard[21].player == player)
			{
				ret = 1;
				end = true;
			}
		}
		if(simBoard[2].player == player && !end)
		{
			if(simBoard[7].player == player && simBoard[12].player == player && simBoard[17].player == player && simBoard[22].player == player)
			{
				ret = 1;
				end = true;
			}
		}
		if(simBoard[3].player == player && !end)
		{
			if(simBoard[8].player == player && simBoard[13].player == player && simBoard[18].player == player && simBoard[23].player == player)
			{
				ret = 1;
				end = true;
			}
		}
		if(simBoard[4].player == player && !end)
		{
			if(simBoard[9].player == player && simBoard[14].player == player && simBoard[19].player == player && simBoard[24].player == player)
			{
				ret = 1;
				end = true;
			}
		}

		//se o vencedor for o oponente, o retorno deverá ser -1, pois foi uma derrota de MAX.
		if(player != TIC.Variables.Max && ret != 0)
		{
			ret = -1;
		}

	}
	else
		ret = 0;

	delete simBoard;
	return ret;
}

//---------------------------------------------------------------------HTML TEMPLATES INTERFACE ------------------

//função que insere o nome do algoritmo selecioado
TIC.HTML.Templates.AlgorithmName = function()
{

	//Insere na interface o algoritmo selecionado.
	if(TIC.Variables.Algorithm && TIC.Variables.Algorithm != "")
	{
		var template = "";
		template += "<b>Algoritmo:</b><br>";
		template += TIC.Variables.Algorithm;
		$(".board-TIC-Info-Algorithm").empty().append(template).show();
	}
	else
		$(".board-TIC-Info-Algorithm").empty().hide();	
}

//Função que desenha as casas no tabuleiro com base no tamanho do tabuleiro :)
TIC.HTML.Templates.Houses = function()
{
	var template = '';
	for(var i = 0; i < TIC.Variables.BoardSize; i++)
	{
		for(var j = 0; j < TIC.Variables.BoardSize; j++)
		{
			if(i == 0 && j == 0)
				template += '<div class = "house-TIC" style = "border-left:none; border-top:none;"></div>';
			else if(i == 0 && j == TIC.Variables.BoardSize -1)
				template += '<div class = "house-TIC" style = "border-top:none; border-right:none;"></div>';
			else if(i == 0)
				template += '<div class = "house-TIC" style = "border-top:none;"></div>';
			else if(j == 0 && i < TIC.Variables.BoardSize -1)
				template += '<div class = "house-TIC" style = "border-left:none;"></div>';
			else if(j < TIC.Variables.BoardSize -1 && i < TIC.Variables.BoardSize -1)
				template += '<div class = "house-TIC"></div>';
			else if( j == TIC.Variables.BoardSize -1 && i < TIC.Variables.BoardSize -1)
				template += '<div class = "house-TIC" style = "border-top:none; border-right:none;"></div>'
			else if(i == TIC.Variables.BoardSize -1 && j == 0)
				template += '<div class = "house-TIC" style = "border-bottom:none; border-left:none;"></div>'
			else if(i == TIC.Variables.BoardSize -1 && j < TIC.Variables.BoardSize -1)
				template += '<div class = "house-TIC" style = "border-bottom:none;"></div>'
			else
				template += '<div class = "house-TIC" style = "border-bottom:none; border-right:none;"></div>'
		}
	}
    $(".houses-TIC").empty().append(template);
    $(".house-TIC").css({
    	width: (700/TIC.Variables.BoardSize) +"px",
    	height: (700/TIC.Variables.BoardSize) +"px"
    })
}

//Função que insere no tabuleiro e casa o icone do jogador que fez a jogada;
TIC.HTML.Templates.Icon = function(house)
{

	//Insere no tabuleiro e na casa certa, o ìcone correspondente ao jogador que fez a jogada;
	var template = '';
	var color = "";
	if(house.player == "King")
		color = "rgba(25,25,112,1)";
	else if(house.player == "Queen")
		color = "rgba(255,255,255,0.9)"

	template += '<span class = "glyphicon glyphicon-'+ house.player.toLowerCase() +'" style = "color:'+ color +';">'
	template += '</span>'
	house.elem.append( template );
}

//Função que insere o número de nós da arovre de possíveis jogadas
TIC.HTML.Templates.NodesNumber = function()
{
	//INsere na interface o número de nós da arvore de possíveis jogadaas.

	var template = "";
	template += "<b>Número total de nós:</b><br>";
	template += TIC.Variables.NodesCount;
	$(".board-TIC-Info-Nodes-Number").empty().append(template).show();
}

//Função que insere o número de nós expandidos do ultimo estad
TIC.HTML.Templates.NodesPath = function(nodesPath)
{

	//INsere na interface o número de nós percorridos na ultiam jogada feita pelo computador

	var template = "";
	template += "<b>O Número de nós percorridos para o estado inicial foi:</b> ";
	template += nodesPath;
	$(".board-TIC-Info-Nodes-Path").show().empty().append( template );
}

//Função que insere o farot de ramificação do ultimo estado
TIC.HTML.Templates.Ramification = function(length)
{
	//INsere na interface a ramificação da ultiam jogada feita pelo computador
	var template = "";
	template += "<b>O fator de ramificação do estado inicial é de:</b> ";
	template += length;
	$(".board-TIC-Info-Nodes-Ramification").show().empty().append( template );
}

//Insere na itnerface a profundidade máxima da arvore
TIC.HTML.Templates.TreeDepth = function()
{
	//INsere na interface o número de nós da arvore de possíveis jogadaas.

	var template = "";
	template += "<b>Profundidade Máxima:</b><br>";
	template += TIC.Variables.MaxDepth;
	$(".board-TIC-Info-Tree-Depth").empty().append(template).show();
}

//-----------------------------------------------Prototypes----------------------------------

TIC.Prototypes.Board = function()
{
	this.free = 0;// Contém o número de casas que aida estão disponíveis na mesa
	this.houses = [];// Contém as casas da mesa;
}

TIC.Prototypes.House = function()
{
	this.id = null;//Armazena o ID corresponde ao elemento HTML
	this.i = null;//Contém o INDICE no TIC.Variables.Board.houses.
	this.elem = null;//Contém o elemento HTML atrelada a está casa
	this.ocupada = false;//Se a casa tiver sido ocupada, será true;
	this.edges = [];//Contém as casas adjacencias da casa atual. _ Não utilizado
	this.player = null;//Contém o jogador que está nesta casa
}

//Prototypo do Objeto TREE
TIC.Prototypes.Tree = function()
{
	this.free = 0;//irá armazenar o número de jogas possíveis com base no estado que está arvore representa
	this.played = [];//contera todas as jogas já jogadas no estado que a tree ira representar
	this.houses = [];//Contera as casas que podem ser jogadas
	this.player = null;//Irá contem qual é o jogador que realizou a jogada atual;
	this.nextPlayer = null;//Irá armazenar  o jogador da proxima rodada
	this.nodes = [];//Irá conter todas as possíveis arvores oriundas do estado atual
	this.parent = null;//irá armazenar o nó pai do estado que a tree irá representar
	this.depth = 0;//Irá conter a profundidade em que está arvore está
	this.pontuation = 0;// Irá conter a pontua~ção do estado atual;
	this.end = false;// se a arvore for um estado termianl, end será true;
}

//Como o MATH do JavaScript não possui fatorial.... 
Math.Fact = function(number)
{
	var result = 0;
	for(var i  = number; i > 0; i--)
	{
		if(i != number)
			result = result * i;
		else
		{
			result = i * (i-1);
			i--;
		}
			
	}
	console.log(number)
	console.log(result)
	return result;
}

TIC.Functions.Init();// INICIO O PROGRAMA
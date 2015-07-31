% Узел дерева: {Key, Color, ParentPid, LeftPid, RightPid}
% Лист дерева: {nil, black, nil,       nil,     nil}

-module(rb).
-compile(export_all).
%-export([start/0, stop/0, search/1, insert/1, showTree/0]).
-include_lib("eunit/include/eunit.hrl").	% Тесты




%-------------------------------------------------Запуск и остановка------------------------------------------------------------
start() ->
	register(userPid, spawn(fun() -> userLoop() end)),
	register(root,    spawn(fun() -> nodeLoop({nil, black, nil, nil, nil}) end)).
	
stop() ->
 	userPid ! stop.






%-----------------------------------------------Функции пользователя----------------------------------------------
insert(Key) ->
	userPid ! {insert, Key},
	{ok, insert}.
	
search(Key) ->
	userPid ! {search, Key},
	{ok, search}.



showTree() ->									% Вывод дерева
	RootPid = whereis(root),
	printTree(RootPid).
	
printTree(nil) ->								% Если дерево пусто - не печатаем
	nil;			
		
printTree(Pid) ->								% Если дерево не пусто
	printSubTree(left,  Pid),							% Печатаем левое поддерево
	printRoot(Pid),									% Печатаем корень дерева
	printSubTree(right, Pid).							% Печатаем правое поддерево
	
printRoot(Pid) ->								% Печать корня
	Key = getNodeKey(Pid),								% Получим значение корня
	Color = getNodeColor(Pid),							% Получим цвет корня
	io:format("Key: ~p  Color: ~p  ID: ~p ~n", [Key, Color, Pid]).			% Печатаем
	
printSubTree(left, Pid) ->							% Печать левого поддерева
	LeftPid = getNodeLeftPid(Pid),							% Получим ID корня левого поддерева
	printTree(LeftPid);								% Рачпечатаем левое поддерево
	
printSubTree(right, Pid) ->							% Печать правого поддерева
	RightPid = getNodeRightPid(Pid),						% Получим ID корня правого поддерева 
	printTree(RightPid).								% Распечатаем правое поддерево





%-----------------------------------------Функции запросов к узлу--------------------------------------------------------------
getNodeKey(nil) -> nil;
getNodeKey(Pid) ->							% Получить значение узла
	SelfPid = self(),
	Pid ! {getKey, SelfPid},						
	receive
		{key, Key} -> Key
	end.
	
getNodeColor(nil) ->	black;	
getNodeColor(Pid) ->							% Получить цвет узла
	SelfPid = self(),
	Pid ! {getColor, SelfPid},						
	receive
		{color, Color} -> Color
	end.	
	
getNodeParentPid(nil) -> nil;	
getNodeParentPid(Pid) ->						% Получить ID предка узла
	SelfPid = self(),
	Pid ! {getParentPid, SelfPid},
	receive
		{parentPid, ParentPid} -> ParentPid
	end.

getNodeLeftPid(nil)  ->	nil;	
getNodeLeftPid(root) ->	nil;
getNodeLeftPid(Pid)  ->							% Получить ID левого дочернего узла
	SelfPid = self(),
	Pid ! {getLeftPid, SelfPid},
	receive
		{leftPid, LeftPid} -> LeftPid
	end.

getNodeRightPid(nil) ->	nil;	
getNodeRightPid(Pid) ->							% Получить ID правого дочернего узла
	SelfPid = self(),
	Pid ! {getRightPid, SelfPid},
	receive
		{rightPid, RightPid} -> RightPid
	end.	

getNode(nil) ->	{nil, black, nil, nil, nil};
getNode(Pid) ->
	Key       = getNodeKey(Pid),
	Color     = getNodeColor(Pid),
	ParentPid = getNodeParentPid(Pid),
	LeftPid   = getNodeLeftPid(Pid),
	RightPid  = getNodeRightPid(Pid),
	{Key, Color, ParentPid, LeftPid, RightPid}.


changeNodeKey(_, nil)   -> {ok, changeNodeKey};
changeNodeKey(Key, Pid) ->						% Заменить в узле с ID Pid значение на Key
	SelfPid = self(),
	Pid ! {changeKey, {Key, SelfPid}},
	receive
		{ok, changeKey} -> {ok, changeNodeKey}
	end.

changeNodeColor(_, nil)     -> {ok, changeNodeColor};
changeNodeColor(Color, Pid) ->						% Заменить в узле с ID Pid цвет на Color
	SelfPid = self(),
	Pid ! {changeColor, {Color, SelfPid}},
	receive
		{ok, changeColor} -> {ok, changeNodeColor}
	end.

changeNodeParentPid(_, nil)         -> {ok, changeNodeParentPid};
changeNodeParentPid(ParentPid, Pid) ->					% Заменить в узле с ID Pid предка на ParentPid
	SelfPid = self(),
	Pid ! {changeParentPid, {ParentPid, SelfPid}},
	receive
		{ok, changeParentPid} -> {ok, changeNodeParentPid}
	end.
	
changeNodeLeftPid(_, nil)       -> {ok, changeNodeLeftPid};	
changeNodeLeftPid(LeftPid, Pid) ->					% Заменить в узле с ID Pid левого потомка на LeftPid
	SelfPid = self(),
	Pid ! {changeLeftPid, {LeftPid, SelfPid}},
	receive
		{ok, changeLeftPid} -> {ok, changeNodeLeftPid}
	end.
	
changeNodeRightPid(_, nil)        -> {ok, changeNodeRightPid};	
changeNodeRightPid(RightPid, Pid) ->					% Заменить в узле с ID Pid правого потомка на RightPid
	SelfPid = self(),
	Pid ! {changeRightPid, {RightPid, SelfPid}},
	receive
		{ok, changeRightPid} -> {ok, changeNodeRightPid}
	end.
	













	
%------------------------------------Процесс обработки сообщений пользователя--------------------------------------------------
userLoop() ->
	RootPid = whereis(root),
	receive
		{search, Key} -> 
			RootPid ! {search, Key},
			receive
				{search, exist, Color, Pid} ->
					io:format("Key: ~p  Color: ~p  Process: ~p ~n", [Key, Color, Pid]);
				{search, parent, _} ->
					io:format("Node doesn't exist. ~n")
			end,
			userLoop();
		{insert, Key} -> 
			RootPid ! {insert, Key},
			userLoop();
		stop -> 
			RootPid ! stop			
	end.
			
			
%-------------------------------------------------------Процесс узла-----------------------------------------------------------
nodeLoop(Node = {Key, Color, ParentPid, LeftPid, RightPid}) ->
	receive
		{getKey, Pid} ->							% Поучить значение узла
			Pid ! {key, Key}, 
			nodeLoop(Node);
		{getColor, Pid} ->							% Получить цвет узла
			Pid ! {color, Color},
			nodeLoop(Node);
		{getParentPid, Pid} ->							% Получить ID предка узла
			Pid ! {parentPid, ParentPid},
			nodeLoop(Node);
		{getLeftPid, Pid} ->							% Получить ID левого потомка узла
			Pid ! {leftPid, LeftPid},
			nodeLoop(Node);
		{getRightPid, Pid} ->							% Получить ID правого потомка узла
			Pid ! {rightPid, RightPid},
			nodeLoop(Node);
		
		{changeKey,      {NewKey, Pid}}      ->					% Изменить значение узла
			Pid ! {ok, changeKey},
			nodeLoop({NewKey, Color, ParentPid, LeftPid, RightPid});
		{changeColor,    {NewColor, Pid}}    ->					% Изменить цвет узла
			Pid ! {ok, changeColor},
			nodeLoop({Key, NewColor, ParentPid, LeftPid, RightPid});
		{changeParentPid, {NewParentPid, Pid}} ->				% Изменить ID предка узла
			Pid ! {ok, changeParentPid},
			nodeLoop({Key, Color, NewParentPid, LeftPid, RightPid});
		{changeLeftPid,  {NewLeftPid, Pid}}  ->					% Изменить ID левого потомка узла
			Pid ! {ok, changeLeftPid},
			nodeLoop({Key, Color, ParentPid, NewLeftPid, RightPid});
		{changeRightPid, {NewRightPid, Pid}} ->					% Изменить ID правого потмка узла
			Pid ! {ok, changeRightPid},
			nodeLoop({Key, Color, ParentPid, LeftPid, NewRightPid});			
			
			
		{search, SearchedKey} ->						% Поиск в дереве по значению
			SelfPid = self(),
			RootPid = whereis(root),
			
			if SelfPid =:= RootPid , RightPid =:= nil ->
				userPid  ! {search, parent, SelfPid};
			   
			   SelfPid =:= RootPid ->
				RightPid ! {search, SearchedKey};	
			
			   SearchedKey  =:=  Key                       -> 		% Если искомый узел 
				userPid  ! {search, exist, Color, SelfPid};
			   
			   SearchedKey   <   Key , LeftPid  =/= nil -> 			% Если узел в левом поддереве
			   	LeftPid  ! {search, SearchedKey};
			   
			   SearchedKey   >   Key , RightPid =/= nil -> 			% Если узел в правом поддереве
			   	RightPid ! {search, SearchedKey};
			   
			   SearchedKey   <   Key , LeftPid  =:= nil -> 			% Если узла нет - потенциальный родитель
			   	userPid  ! {search, parent, SelfPid};
			   
			   SearchedKey   >   Key , RightPid =:= nil -> 			% Если узла нет - потенциальный родитель
			   	userPid  ! {search, parent, SelfPid}
			
			end,
			nodeLoop(Node);
				
		{insert, InsertedKey} ->						% Вставка в дерево
			SelfPid = self(),
			RootPid = whereis(root),
									
			if SelfPid =:= RootPid , RightPid  =:= nil ->
				NewPid = spawn(fun() -> nodeLoop({InsertedKey, black, RootPid, nil, nil}) end),
                		nodeLoop({Key, Color, nil, nil, NewPid});
				
			   SelfPid =:= RootPid ->
				RightPid ! {insert, InsertedKey},
				nodeLoop(Node);	
				
			   InsertedKey =:= Key ->
			   	nodeLoop(Node);
			   	
			   InsertedKey  <  Key , LeftPid  =:= nil ->			
			   	NewLeftPid  = spawn(fun() -> nodeLoop({InsertedKey, red, SelfPid, nil, nil}) end), 
			   	RealRootPid = getNodeRightPid(RootPid),
			   	RealRootPid ! insertBalance,
			   	nodeLoop({Key, Color, ParentPid, NewLeftPid, RightPid});
			   	
			   InsertedKey  >  Key , RightPid =:= nil ->
			   	NewRightPid = spawn(fun() -> nodeLoop({InsertedKey, red, SelfPid, nil, nil}) end), 
			   	RealRootPid = getNodeRightPid(RootPid),
			   	RealRootPid ! insertBalance,
			   	nodeLoop({Key, Color, ParentPid, LeftPid, NewRightPid});
			   
			   InsertedKey  <  Key , LeftPid  =/= nil -> 			
			   	LeftPid  ! {insert, InsertedKey},
			   	nodeLoop(Node);
			   
			   InsertedKey  >  Key , RightPid =/= nil -> 			
			   	RightPid ! {insert, InsertedKey},
			   	nodeLoop(Node)
			
			end;
			
		insertBalance ->							% Балансировка дерева после вставки
			SelfPid = self(),
			RootPid = whereis(root),
			RealRootPid = getNodeRightPid(RootPid),
		
			{NewKey, NewColor, NewParentPid, NewLeftPid, NewRightPid} = insertBalanceCases(Node),
			
			sendIfNotNil(insertBalance, LeftPid),
			sendIfNotNil(insertBalance, RightPid),
			
			if SelfPid =:= RealRootPid ->
				nodeLoop({NewKey, black, NewParentPid, NewLeftPid, NewRightPid});
				
			   true ->
			   	nodeLoop({NewKey, NewColor, NewParentPid, NewLeftPid, NewRightPid})
			end;
			
		stop ->
			{ok, sendStop} = sendStop(LeftPid),
			{ok, sendStop} = sendStop(RightPid)
	end.


sendStop(nil) -> {ok, sendStop};
sendStop(Pid) ->
	Pid ! stop,
	{ok, sendStop}.


sendIfNotNil(_, nil) -> {ok, sendIfNotNil};
sendIfNotNil(Message, Pid) ->
	Pid ! Message,
	{ok, sendIfNotNil}.
	


insertBalanceCases(Node = {Key, Color, ParentPid, LeftPid, RightPid}) ->
	{_, LeftPidColor,  _, LeftPidLeft,  LeftPidRight} = getNode(LeftPid),
	LeftPidLeftColor   = getNodeColor(LeftPidLeft),
	LeftPidRightColor  = getNodeColor(LeftPidRight),
	
	{_, RightPidColor, _, RightPidLeft, RightPidRight} = getNode(RightPid),
	RightPidLeftColor  = getNodeColor(RightPidLeft),
	RightPidRightColor = getNodeColor(RightPidRight),
	
	if     
	    RightPidColor =:= red , RightPidRightColor =:= red  -> insertBalance1stCase(Node); 
	    RightPidColor =:= red , RightPidLeftColor  =:= red  -> insertBalance2ndCase(Node);
	    LeftPidColor  =:= red , LeftPidLeftColor   =:= red  -> insertBalance3rdCase(Node);
	    LeftPidColor  =:= red , LeftPidRightColor  =:= red  -> insertBalance4thCase(Node);
	    true						-> Node
	end.
	
insertBalance1stCase(Node = {Key, Color, ParentPid, LeftPid, RightPid}) ->
	SelfPid = self(), 
	
	{_, _, _, RightPidLeft, RightPidRight} 		 = getNode(RightPid),
	{_, _, _, RightPidRightLeft, RightPidRightRight} = getNode(RightPidRight),
	{_, _, _, ParentPidLeft, ParentPidRight} 	 = getNode(ParentPid),
	
	{ok, changeNodeColor}     = changeNodeColor(black, RightPidRight),
	{ok, changeNodeLeftPid}   = changeNodeLeftPid(SelfPid, RightPid),
	{ok, changeNodeParentPid} = changeNodeParentPid(SelfPid, RightPidLeft),
	{ok, changeNodeParentPid} = changeNodeParentPid(ParentPid, RightPid),
	
	
	if 
		 SelfPid =:= ParentPidLeft  -> 
			{ok, changeNodeLeftPid}  = changeNodeLeftPid(RightPid, ParentPid);
			
		 SelfPid =:= ParentPidRight ->
			{ok, changeNodeRightPid} = changeNodeRightPid(RightPid, ParentPid)
	end,
	
	{Key, black, RightPid, LeftPid, RightPidLeft}.

insertBalance2ndCase(Node = {Key, Color, ParentPid, LeftPid, RightPid}) ->
	SelfPid = self(),
		
	{_, _, _, RightPidLeft, RightPidRight} 		= getNode(RightPid),
	{_, _, _, RightPidLeftLeft, RightPidLeftRight}  = getNode(RightPidLeft),
	{_, _, _, ParentPidLeft, ParentPidRight} 	= getNode(ParentPid),
	
	{ok, changeNodeColor}     = changeNodeColor(black, RightPid),
	{ok, changeNodeLeftPid}   = changeNodeLeftPid(SelfPid, RightPidLeft),
	{ok, changeNodeRightPid}  = changeNodeRightPid(RightPid, RightPidLeft),
	{ok, changeNodeLeftPid}   = changeNodeLeftPid(RightPidLeftRight, RightPid),
	{ok, changeNodeParentPid} = changeNodeParentPid(SelfPid, RightPidLeftLeft),
	{ok, changeNodeParentPid} = changeNodeParentPid(RightPid, RightPidLeftRight),
	{ok, changeNodeParentPid} = changeNodeParentPid(RightPidLeft, RightPid),
	{ok, changeNodeParentPid} = changeNodeParentPid(ParentPid, RightPidLeft),
	
	if 
		 SelfPid =:= ParentPidLeft  -> 
			{ok, changeNodeLeftPid}  = changeNodeLeftPid(RightPidLeft, ParentPid);
			
		 SelfPid =:= ParentPidRight ->
			{ok, changeNodeRightPid} = changeNodeRightPid(RightPidLeft, ParentPid)
	end,
	
	{Key, Color, RightPidLeft, LeftPid, RightPidLeftLeft}.
	
insertBalance3rdCase(Node = {Key, Color, ParentPid, LeftPid, RightPid}) ->
	SelfPid = self(),
	
	{_, _, _, LeftPidLeft, LeftPidRight} 		= getNode(LeftPid),
	{_, _, _, LeftPidLeftLeft, LeftPidLeftRight} 	= getNode(LeftPidLeft),
	{_, _, _, ParentPidLeft, ParentPidRight} 	= getNode(ParentPid),
	
	{ok, changeNodeColor}     = changeNodeColor(black, LeftPidLeft),
	{ok, changeNodeRightPid}  = changeNodeRightPid(SelfPid, LeftPid),
	{ok, changeNodeParentPid} = changeNodeParentPid(SelfPid, LeftPidRight),
	{ok, changeNodeParentPid} = changeNodeParentPid(ParentPid, LeftPid),
	
	if 
		 SelfPid =:= ParentPidLeft  -> 
			{ok, changeNodeLeftPid}  = changeNodeLeftPid(LeftPid, ParentPid);
			
		 SelfPid =:= ParentPidRight ->
			{ok, changeNodeRightPid} = changeNodeRightPid(LeftPid, ParentPid)
	end,
	
	{Key, Color, ParentPid, LeftPidRight, RightPid}.
	
insertBalance4thCase(Node = {Key, Color, ParentPid, LeftPid, RightPid}) ->
	SelfPid = self(),
		
	{_, _, _, LeftPidLeft, LeftPidRight} 		= getNode(LeftPid),
	{_, _, _, LeftPidRightLeft, LeftPidRightRight} 	= getNode(LeftPidRight),
	{_, _, _, ParentPidLeft, ParentPidRight} 	= getNode(ParentPid),
	
	{ok, changeNodeColor}     = changeNodeColor(black, LeftPid),
	{ok, changeNodeLeftPid}   = changeNodeLeftPid(LeftPid, LeftPidRight),
	{ok, changeNodeRightPid}  = changeNodeRightPid(SelfPid, LeftPidRight),
	{ok, changeNodeRightPid}  = changeNodeRightPid(LeftPidRightLeft, LeftPid),
	{ok, changeNodeParentPid} = changeNodeParentPid(LeftPid, LeftPidRightLeft),
	{ok, changeNodeParentPid} = changeNodeParentPid(SelfPid, LeftPidRightRight),
	{ok, changeNodeParentPid} = changeNodeParentPid(LeftPidRight, LeftPid),
	{ok, changeNodeParentPid} = changeNodeParentPid(ParentPid, LeftPidRight),
	
	if 
		 SelfPid =:= ParentPidLeft  -> 
			{ok, changeNodeLeftPid}  = changeNodeLeftPid(LeftPidRight, ParentPid);
			
		 SelfPid =:= ParentPidRight ->
			{ok, changeNodeRightPid} = changeNodeRightPid(LeftPidRight, ParentPid)
	end,
	
	{Key, Color, LeftPidLeft, LeftPidRightRight, RightPid}.
	
	


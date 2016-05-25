-module (rss_parse).

-include_lib("xmerl/include/xmerl.hrl").
-export ([is_rss2_feed/1]).
-export ([get_feed_items/1]).
-export ([get_item_time/1]).
-export ([compare_feed_items/2]).

is_rss2_feed(XML) ->
	VerXPath = "@version",
	case XML#xmlElement.name of
		rss -> case (lists:nth(1, xmerl_xpath:string(VerXPath, XML)))#xmlAttribute.value of
			"2.0" -> ok;
			_ -> false
		end; 
		_ -> false
	end.

get_feed_items(RSS2Feed) ->
	ItemsXPath = "//item",
	xmerl_xpath:string(ItemsXPath, RSS2Feed).

get_item_time(Item) ->
	PubDateXPath = "//pubDate",
	[PubDateNode | _] = xmerl_xpath:string(PubDateXPath, Item),
	[PubDate | _] = PubDateNode#xmlElement.content,
	DateTime = httpd_util:convert_request_date(PubDate#xmlText.value), % bad_time if wrong
	calendar:datetime_to_gregorian_seconds(DateTime).

% @private
% @doc Эта вспомогательная функция просматривает заданный XML элемент
%	  и удаляет из него сведения о других XML элементах, например содержащиеся в полях
%	  "parents" или "pos".
%
% @spec extract_xml(Node::xmlAny()) -> xmlAny()
%
extract_xml(Elem = #xmlElement{}) ->
	Elem#xmlElement{parents=[], pos=0,
		content=lists:map(fun extract_xml/1, Elem#xmlElement.content),
		attributes=lists:map(fun extract_xml/1, Elem#xmlElement.attributes)};
extract_xml(Attr = #xmlAttribute{}) ->
	Attr#xmlAttribute{parents=[], pos=0};
extract_xml(Text = #xmlText{}) ->
	Text#xmlText{parents=[], pos=0};
extract_xml(Comment = #xmlComment{}) ->
	Comment#xmlComment{parents=[], pos=0};
extract_xml(Other) ->
	Other.

compare_feed_items(OldItemRow, NewItemRow) ->
	OldItem = extract_xml(OldItemRow),
	NewItem = extract_xml(NewItemRow),

	case OldItem =:= NewItem of
		true -> same;
		_ ->
			Handler = fun(E, Acc) ->
				Acc or is_updated(OldItem, NewItem, E)
			end,
			Acc = false,
			ListXPathAttrs = ["//guid", "//title", "//link"],

			case lists:foldl(Handler, Acc, ListXPathAttrs) of
				true -> updated;
				_ -> different
			end
	end.

% @private
% @doc Эта вспомогательная функция проверяет на равенство двух нод 
% по заданному XPath селектору
%
is_updated(OldItem, NewItem, XPathAttr) ->
	OldAttrValue = xmerl_xpath:string(XPathAttr, OldItem),
	case OldAttrValue of
		[] -> false;
		_ -> NewAttrValue = xmerl_xpath:string(XPathAttr, NewItem),
			case NewAttrValue of
				[] -> false ;
				_ ->
					[OldGUID] = OldAttrValue
					,[NewGUID] = NewAttrValue
					,OldGUID == NewGUID
			end
	end.

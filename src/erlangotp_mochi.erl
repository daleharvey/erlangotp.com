-module(erlangotp_mochi).
-export([handle/1, regen_html/0]).

handle(Mochi) ->
    try
        case Mochi:get(path) of
            
            "/www/"++File ->
                Mochi:serve_file(File, docroot(), []);
            
            "/"++Path ->
                
                Name   = path_to_name(Path),
                Action = request_type(Mochi, Name),
                
                case Action of
                    save_page ->       
                        Body = Mochi:recv_body(),
                        {struct, [{<<"html">>, Html},
                                  {<<"markdown">>, Markdown}]}
                            = mochijson2:decode(Body),
                        
                        ok = erlangotp_srv:save_page(Name, Html, Markdown),
                        
                        Mochi:ok({"application/json", [],
                                  encode({struct, [{"response","ok"}]})});
                    
                    view_markdown ->
                        io:format("~p ~p~n",[wikiroot(), Name]),                       
                        Mochi:serve_file(Name++".markdown", wikiroot(), []);
                    view_html ->
                        Out = [docroot(), "/wiki_out"],
                        Mochi:serve_file(Name++".html", Out, []);
                    create_page ->
                        Mochi:serve_file("newpage.html", docroot(), [])
                end
        end
    catch
        exit:normal ->
            exit(normal);
        Type:What ->
            Report = ["web request failed",
                      {path, Mochi:get(path)},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            Mochi:respond({500, [], []})
    end.


request_type(Mochi, Name) ->
    case Mochi:get(method) of
        'POST' -> save_page;
        'GET'  ->
            Get = Mochi:parse_qs(),
            case lists:keymember("markdown", 1, Get) of
                true  -> view_markdown;
                false ->
                    Path = [docroot(), "/wiki_out/", Name, ".html"],
                    io:format("~p~n",[Path]),
                    case filelib:is_regular( Path ) of
                        true  -> view_html;
                        false -> create_page
                    end
            end
    end.

encode(Json) ->
    (mochijson:encoder([{input_encoding, utf8}]))(Json).

path_to_name([]) ->
    "index";
path_to_name(Name) ->
    re:replace(Name, "/", "", [{return, list}, global]).
   
regen_html() ->
    
    Content = "There is no page here, <a class=\"wiki-create\">make one?</a>",
    {ok, Data} = erlangotp_srv:render_template(Content),
    ok = file:write_file([docroot(), "/", "newpage.html"], Data),
    
    [ begin Name = filename:basename(X, ".html"),
            {ok, File} = file:read_file(X),
            erlangotp_srv:save_output(Name, File)
      end || X <- filelib:wildcard(wikiroot() ++ "/*.html") ].

docroot() ->
    {ok, Path} = application:get_env(erlangotp, docroot),
    Path.

wikiroot() ->
    {ok, Path} = application:get_env(erlangotp, wikiroot),
    Path.


-module(erlangotp_srv).
-behaviour(gen_server).

-export([
         save_page/3,
         save_output/2,
         render_template/1
        ]).

%% gen_server callbacks
-export([ start_link/0, init/1, handle_call/3, handle_cast/2,
          handle_info/2, terminate/2, code_change/3]).

-record(state,  { }).


-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->    
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({save_page, Name, Html, Markdown}, {_From, _Tag}, State) ->
    ok = do_save_page(Name, Html, Markdown),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Else, State) ->    
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% Api Calls
save_page(Name, Html, Markdown) ->
    gen_server:call(?MODULE, {save_page, Name, Html, Markdown}).

%% Worker function
do_save_page(Name, Html, Markdown) ->
    
    {ok, Dir} = file:get_cwd(),
    ok = file:write_file([wikiroot(), "/", Name, ".markdown"], Markdown),
    ok = file:write_file([wikiroot(), "/", Name, ".html"], Html),
    
    file:set_cwd(wikiroot()),
    Output = os:cmd("git add --all && git commit -a -m \"testing\""),
    error_logger:info_msg("git output ~p~n",[Output]),
    file:set_cwd(Dir),
    
    ok = save_output(Name, Html).

save_output(Name, Content) ->
    {ok, Out} = render_template(Content),
    ok = file:write_file([docroot(),"/wiki_out/", Name, ".html"], Out).

render_template(Content) ->
    Path = [docroot(),"/","template.html"],
    erlydtl:compile(Path, def_template),
    def_template:render([{content, Content}]).

docroot() ->
    {ok, Path} = application:get_env(erlangotp, docroot),
    Path.

wikiroot() ->
    {ok, Path} = application:get_env(erlangotp, wikiroot),
    Path.



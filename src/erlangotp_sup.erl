
-module(erlangotp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->

    ok = start_mochiweb(),

    ErlangOtp = {erlangotp_srv, {erlangotp_srv, start_link, []},
             permanent, 2000, worker, [erlangotp_srv]},

    
    {ok, { {one_for_one, 5, 10}, [ ErlangOtp ]} }.

start_mochiweb() ->
    {ok, Host} = application:get_env(erlangotp, host),
    ok = start_instance(Host).

start_instance({IP, Port}) ->
    StrIp = inet_parse:ntoa(IP),
    Opts = [{port, Port}, 
            {ip,   StrIp},
            {name, StrIp ++ "&" ++ integer_to_list(Port)},
            {loop, {erlangotp_mochi, handle}}],
    {ok, _Pid} = mochiweb_http:start(Opts),
    ok.




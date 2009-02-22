%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

%% @doc Template handling, compiles and renders django compatible templates using an extended version of erlydtl

-module(zp_template).
-behaviour(gen_server).

-author("Marc Worrell <marc@worrell.nl>").

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

-include_lib("zophrenic.hrl").

%% External exports
-export([compile/1, render/2, reader/1]).


start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @spec render(File, Context) -> iolist()
%% @doc Render a template.  First requests the template module from the template server, then renders the template.
%%      We do not let the template server render the template to prevent that the whole context is copied whilst passing
%%      The message to the template server. The context might contain a lot of variables set by the resource.
render(File, Context) ->
    case gen_server:call(?MODULE, {compile_if_modified, File}) of
        {ok, Module} ->
            case Module:render(Context) of
                {ok, Output}   -> Output;
                {error, Reason} -> "<strong>Error rendering template: "++ File ++ " ("++Reason++")</strong>"
             end;
        {error, Error} ->
            io:format("<strong>Error compiling template: ~s (~p)</strong>", [File, Error])
    end.


%% @spec compile(File) -> {ok, atom()} | {error, Reason}
%% @doc Compile a template, retun the module name.
compile(File) ->
    gen_server:call(?MODULE, {compile, File}).


%% @spec reader(File) -> {ok, binary()} | {error, code} 
%% @doc Read the template designated by the file, check project and default directory
reader(File) ->
    case find_template(File) of
        {ok, Filename} -> file:read_file(Filename);
        _ -> {error, enoent}
    end.


%% @spec init([]) -> {ok, [])}
%% @doc Initialize the template server, handles template compiles and rendering
init(Options) ->
    {ok, Options}.


%% @spec compile_if_modified(File) -> {ok, Module} | {error, Reason}
%% @doc Compile the template if its has been modified, return the template module for rendering.
handle_call({compile_if_modified, File}, _From, State) ->
    ModuleName = filename_to_modulename(File),
    Module     = list_to_atom(ModuleName),
    ErlyResult = case template_is_modified(File, ModuleName) of
                    true  -> 
                        case erlydtl:compile(File, Module, [{reader, {?MODULE,reader}}]) of
                            ok -> {ok, Module};
                            Error -> Error
                        end;
                    false -> 
                        {ok, Module}
                 end,
    {reply, ErlyResult, State};


%% @doc Compile the template, creates a beam file in the ebin directory.  Make sure that we only compile
%%      one template at a time to prevent overwriting the beam file with two processes.
handle_call({compile, File}, _From, State) ->
    ModuleName = filename_to_modulename(File),
    Module     = list_to_atom(ModuleName),
    ErlyResult = erlydtl:compile(File, Module, [{reader, {?MODULE,reader}}]),
    {reply, ErlyResult, State}.


handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


%% @doc Translate a filename to a module name
filename_to_modulename(File) ->
    filename_to_modulename(File, []).

filename_to_modulename([], Acc) ->
    "template_" ++ lists:reverse(Acc);
filename_to_modulename([C|T], Acc) ->
    filename_to_modulename(T, [savechar(C)|Acc]).

savechar(C) when C >= $0 andalso C =< $9 ->
    C;
savechar(C) when C >= $a andalso C =< $z ->
    C;
savechar(C) when C >= $A andalso C =< $Z ->
    C-32;
savechar(_C) ->
    $_.


%% Check if the template or one of the by the template included files is modified with respect to the beam file
template_is_modified(File, ModuleName) ->
    BeamFile = filename:join("ebin", ModuleName ++ ".beam"),
    case filelib:is_file(BeamFile) of
        true ->
            Module  = list_to_atom(ModuleName),
            BeamMod = filelib:last_modified(BeamFile),
            Deps    = Module:dependencies(),
            Files   = lists:map(fun({F,_CheckSum}) -> F end, Deps),
            is_modified([File|Files], BeamMod);
        false ->
            true
    end.
    

%% Check if one of the template files is newer than the given datetime
is_modified([], _DateTime) ->
    false;
is_modified([File|Rest], DateTime) ->
    case find_template(File) of
        {ok, Filename} -> 
                FileMod = filelib:last_modified(Filename),
                case FileMod > DateTime of
                    true -> true;
                    _    -> is_modified(Rest, DateTime)
                end;
        _ ->
            true
    end.


find_template(File) ->
    Dirs =  [
                "priv/templates",
                "default/templates"
            ],
    find_template(File, Dirs).
    
find_template(_File, []) ->
    {error, enoent};
find_template(File, [Dir|Rest]) ->
    Filename = filename:join(Dir, File),
    case filelib:is_file(Filename) of
        true  -> {ok, Filename};
        false -> find_template(File, Rest)
    end.


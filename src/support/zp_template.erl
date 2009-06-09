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
-export([compile/2, render/3, find_template/2]).


start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @spec render(File, Variables, Context) -> iolist()
%% @doc Render a template.  First requests the template module from the template server, then renders the template.
render(File, Variables, Context) ->
    case find_template(File, Context) of
        {ok, FoundFile} ->
            Result = case gen_server:call(?MODULE, {check_modified, FoundFile}) of
                modified -> compile(File, Context);
                Other -> Other
            end,

            case Result of
                {ok, Module} ->
                    case Module:render(Variables, Context) of
                        {ok, Output}   -> 
                            Output;
                        {error, Reason} ->
                            ?ERROR("Error rendering template: ~p (~p)", [File, Reason]),
                            <<>>
                     end;
                {error, Reason} ->
                    ?ERROR("Error compiling template: ~s (~p)", [File, Reason]),
                    <<>>
            end;
        {error, Reason} ->
            ?LOG("Could not find template: ~s (~p)", [File, Reason]),
            <<>>
    end.
            


%% @spec compile(File, Context) -> {ok, atom()} | {error, Reason}
%% @doc Compile a template, retun the module name.
compile(File, Context) ->
    case find_template(File, Context) of
        {ok, FoundFile} ->
            gen_server:call(?MODULE, {compile, FoundFile, Context});
        {error, Reason} ->
            {error, Reason}
    end.


%% @spec find_template(File, Context) -> {ok, binary()} | {error, code} 
%% @doc Finds the template designated by the file, check modules, project and default directory
find_template(File, Context) ->
    case zp_module_indexer:find(template, File, Context) of
        {ok, FoundFile} ->
            {ok, FoundFile};
        {error, enoent} ->
            Dirs =  [
                        "priv/templates",
                        "default/templates"
                    ],
            find_template_dirs(File, Dirs)
    end.


%% @spec init([]) -> {ok, [])}
%% @doc Initialize the template server, handles template compiles and rendering
init(Options) ->
    {ok, Options}.


%% @spec check_modified(File) -> {ok, Module} | {error, Reason}
%% @doc Compile the template if its has been modified, return the template module for rendering.
handle_call({check_modified, File}, _From, State) ->
    ModuleName = filename_to_modulename(File),
    Module = list_to_atom(ModuleName),
    Result = case template_is_modified(File, ModuleName) of
                true  -> modified;
                false -> {ok, Module}
             end,
    {reply, Result, State};


%% @doc Compile the template, creates a beam file in the ebin directory.  Make sure that we only compile
%%      one template at a time to prevent overwriting the beam file with two processes.
handle_call({compile, File, Context}, _From, State) ->
    FinderFun  = fun(FinderFile) ->
        ?MODULE:find_template(FinderFile, Context)
    end,
    ModuleName = filename_to_modulename(File),
    Module     = list_to_atom(ModuleName),
    ErlyResult = case erlydtl:compile(File, Module, [{finder, FinderFun}]) of
                    ok -> {ok, Module};
                    Other -> Other
                 end,
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
    C+32;
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
    FileMod = filelib:last_modified(File),
    case FileMod > DateTime of
        true -> true;
        _    -> is_modified(Rest, DateTime)
    end.

   
find_template_dirs(_File, []) ->
    {error, enoent};
find_template_dirs(File, [Dir|Rest]) ->
    Filename = filename:join(Dir, File),
    case filelib:is_file(Filename) of
        true  -> {ok, Filename};
        false -> find_template(File, Rest)
    end.


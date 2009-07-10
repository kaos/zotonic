%% @author Marc Worrell <marc@worrell.nl>
%% @author Rusty Klophaus
%% @copyright 2009 Marc Worrell
%%
%% Based on Nitrogen, which is copyright (c) 2008-2009 Rusty Klophaus
%% See MIT-LICENSE for licensing information.

-module(zp_render).
-author("Marc Worrell <marc@worrell.nl>").
-author("Rusty Klophaus").

-include("zophrenic.hrl").

-export ([
	render/2,
	render_actions/4,
	render_to_iolist/2,
	
	validator/4,
	render_validator/4,

	update/3,
	insert_top/3,
	insert_bottom/3,
	set_value/3,
	
	dialog/4,
	growl/2,
	growl_error/2,
	
	make_postback/6,
	make_postback_info/6,
	make_validation_postback/1,
	make_validation_postback/2,
	
	wire/2, wire/3, wire/4
]).


%% @doc Render adds output to the render field of the context state, makes sure that the added output is an iolist
render(undefined, Context) -> 
    Context;
render(<<>>, Context) -> 
    Context;
render([], Context) -> 
    Context;
render(#context{} = C, Context) ->
    C1 = render(C#context.render, Context),
    C2 = zp_context:merge_scripts(C, C1),
    C2;
render(B, Context) when is_integer(B) orelse is_binary(B) -> 
    Context#context{render=[Context#context.render, B]};
render(A, Context) when is_atom(A) -> 
    Context#context{render=[Context#context.render, atom_to_list(A)]};
render(List=[H|_], Context) when is_integer(H) orelse is_binary(H) ->
    %% Optimization for rendering lists of characters, aka strings
    F = fun (C) ->
            is_integer(C) orelse is_binary(C)
        end,
    {String,Rest} = lists:splitwith(F,List),
    Context1 = Context#context{render=[Context#context.render, String]},
    render(Rest, Context1);
render([H|T], Context) ->
    Context1 = render(H, Context),
    render(T, Context1).


%% @doc Render adds output to the render field of the context state. Do update the context for
%%      possible changes in scripts etc.
render_to_iolist(Ts, Context) ->
    Context1 = Context#context{render=[]},
    Context2 = render(Ts, Context1),
    {Context2#context.render, Context2#context{render=Context#context.render}}.


%%% RENDER ACTIONS %%%

render_actions(_, _, undefined, Context) ->
    {[], Context};
render_actions(_, _, [], Context) ->
    {[], Context};
render_actions(TriggerId, TargetId, [H|T], Context) -> 
    {Script1, Context1} = render_actions(TriggerId, TargetId, H, Context),
    {Script2, Context2} = render_actions(TriggerId, TargetId, T, Context1),
    {[Script1,Script2], Context2};
render_actions(TriggerId, TargetId, {Action, Args}, Context) ->
	case zp_utils:is_true(proplists:get_value(show_if, Args, true)) of 
		true -> 
            Trigger      = proplists:get_value(trigger, Args, TriggerId),
	        Target       = proplists:get_value(target,  Args, TargetId),
	        case zp_module_indexer:find(action, Action, Context) of
	            {ok, ActionModule} ->
			        ActionModule:render_action(Trigger, Target, Args, Context);
			    {error, enoent} ->
			        ?LOG("No action enabled for \"~p\"", [Action]),
			        {[], Context}
			end;
		false -> 
			{[],Context}
	end.


%% @spec validator(TriggerID::string(), TargetID::string(), Validator::#validator, Context::#context) -> #context
%% @doc Add an input validator to the list of known validators, used when rendering custom validators
validator(TriggerId, TargetId, Validator, Context) ->
    V = {TriggerId, TargetId, Validator},
    case lists:member(V, Context#context.validators) of
        true ->
            Context;
        false ->
            Context#context{validators=[V|Context#context.validators]}
    end.


%% @doc Render a validator to the correct javascript.  Args are all arguments of the validator scomp.
%%      This renders an allocation of the initial validator and then appends all validations.
%%      'type' holds multiple validations.  Validations are of the form:  {validator, [Args]}
render_validator(TriggerId, TargetId, Args, Context) ->
    Validations = proplists:get_all_values(type, Args),
    Trigger     = proplists:get_value(trigger, Args, TriggerId),
    Target      = proplists:get_value(target,  Args, TargetId),

    % The validator object, can have parameters for failureMessage.
    VldOptions  = zp_utils:js_object(Args, [type,trigger,id,target]),
    VldScript   = [<<"zp_init_validator(\"">>,Trigger,<<"\", ">>,VldOptions,<<");\n">>],
    
    % Now render and append all individual validations
    % The Postback contains all information to perform a server side validation
    % The Script is the script that ties the client side validation to the element
    RValidation = fun({VType,VArgs}, {PostbackAcc,ScriptAcc,Ctx}) ->
                    VMod = case proplists:get_value(delegate, VArgs) of
                                undefined -> 
                                    case zp_module_indexer:find(validator, VType, Context) of
                                        {ok, Mod} ->
                                            {ok, Mod};
                                        {error, enoent} ->
                                            ?LOG("No validator found for \"~p\"", [VType])
                                    end;
                                Delegate  -> 
                                    {ok, Delegate}
                             end,
                     case VMod of
                        {ok, ValidatorModule} ->
                            {VPostback,VScript,VCtx} = ValidatorModule:render_validator(VType, Trigger, Target, VArgs, Ctx),
                            {[{VType,ValidatorModule,VPostback}|PostbackAcc],[VScript|ScriptAcc],VCtx};
                        _ ->
                            {PostbackAcc, ScriptAcc, Ctx}
                     end
                 end,

    {Postback,Append,Context1} = lists:foldr(RValidation, {[],[],Context}, Validations),
    case Postback of
        [] ->
            {[VldScript|Append], Context1};
        _ ->
            Pickled  = zp_utils:pickle({Trigger,Postback}),
            PbScript = [<<"zp_set_validator_postback('">>,Trigger,<<"', '">>, Pickled, <<"');\n">>],
            {[PbScript,VldScript|Append], Context1}
    end.



%%% AJAX UPDATES %%%

%% @doc Set the contents of an element to the the html fragment	
update(TargetId, undefined, Context) -> 
    Script = "$('#~s').html(\"~s\");",
    add_update(TargetId, "", Script, Context);
update(TargetId, Html, Context) -> 
    Script = "$('#~s').html(\"~s\");",
    add_update(TargetId, Html, Script, Context).
    
%% @doc Insert a html fragment at the top of the contents of an element
insert_top(_TargetId, undefined, Context) -> 
    Context;
insert_top(TargetId, Html, Context) -> 
    Script = "$('#~s').prepend(\"~s\");",
    add_update(TargetId, Html, Script, Context).

%% @doc Append a html fragment at the bottom of the contents of an element
insert_bottom(_TargetId, undefined, Context) -> 
    Context;
insert_bottom(TargetId, Html, Context) -> 
    Script = "$('#~s').append(\"~s\");",
    add_update(TargetId, Html, Script, Context).

%% @doc Set the value of an input element.
set_value(TargetId, undefined, Context) ->
    Script = "$('#~s').val(\"~s\");",
    add_update(TargetId, "", Script, Context);
set_value(TargetId, Value, Context) ->
    Value1 = zp_html:escape(Value),
    Script = "$('#~s').val(\"~s\");",
    add_update(TargetId, Value1, Script, Context).


%% @doc Internal function to add updates to the update queue for rendering
add_update(TargetId, Html, JSFormatString, Context) ->
	Context#context{updates=[{TargetId, Html, JSFormatString}|Context#context.updates]}.


%%% SIMPLE FUNCTION TO SHOW DIALOG OR GROWL (uses the dialog and growl actions) %%%

dialog(Title, Template, Vars, Context) ->
    {Html, Context1} = zp_template:render_to_iolist(Template, Vars, Context),
    zp_render:wire({dialog, [{title, Title}, {text, Html}]}, Context1).

growl(Text, Context) ->
    zp_render:wire({growl, [{text, Text}]}, Context).

growl_error(Text, Context) ->
    zp_render:wire({growl, [{text, Text}, {type, "error"}]}, Context).


%%% POSTBACK ENCODING %%%

%% @doc Make an encoded string containing information which module and function to call.
make_postback_info(Tag, EventType, TriggerId, TargetId, Delegate, Context) ->
	Delegate1 = case Delegate of
            		undefined -> zp_context:get_resource_module(Context);
            		_         -> zp_convert:to_atom(Delegate)
            	end,
	PostbackInfo = {EventType, TriggerId, TargetId, Tag, Delegate1},
	zp_utils:pickle(PostbackInfo).


%% @doc Make a javascript to call the postback, posting an encoded string containing callback information. 
%% The PostbackTag is send to the server, EventType is normally the atom 'postback'.
%% @spec make_postback(PostbackTag, EventType, TriggerId, TargetId, Deletegate, Context) -> {JavascriptString, PickledPostback}
make_postback(undefined, _EventType, _TriggerId, _TargetId, _Delegate, _Context) ->
    {[],[]};
make_postback(PostbackTag, EventType, TriggerId, TargetId, Delegate, Context) ->
	PickledPostbackInfo = make_postback_info(PostbackTag, EventType, TriggerId, TargetId, Delegate, Context),
	{[<<"zp_queue_postback('">>,TriggerId,<<"', '">>,PickledPostbackInfo,<<"');">>], PickledPostbackInfo}.


make_validation_postback(Validator) ->
    make_validation_postback(Validator,{}).
make_validation_postback(Validator, Args) ->
    zp_utils:pickle({Validator, Args}).


%%% ACTION WIRING %%%

%% Add to the queue of wired actions. These will be rendered in get_script().

wire(Actions, Context) -> 
	wire(<<>>, <<>>, Actions, Context).

wire(undefined, Actions, Context) ->	
	wire(<<>>, <<>>, Actions, Context);
wire(TriggerId, Actions, Context) ->	
	wire(TriggerId, TriggerId, Actions, Context).

wire(undefined, TargetId, Actions, Context) ->
    wire(<<>>, TargetId, Actions, Context);
wire(TriggerId, undefined, Actions, Context) ->
    wire(TriggerId, <<>>, Actions, Context);
wire(_TriggerId, _TargetId, [], Context) ->
    Context;
wire(TriggerId, TargetId, Actions, Context) ->
    Context#context{actions=[{TriggerId, TargetId, flatten_list(Actions)}|Context#context.actions]}.

    flatten_list(L) when is_list(L) ->
        lists:flatten(L);
    flatten_list(Other) ->
        Other.

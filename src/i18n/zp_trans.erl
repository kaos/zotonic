%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @package Zophrenic - i18n
%% @doc Translate english sentences into other languages
%% @seealso GNU gettext

-module(zp_trans).
-author("Marc Worrell <marc@worrell.nl>").

-export([trans/2, default_language/1, is_language/1, lc2/1, lc2descr/1]).

-include_lib("zophrenic.hrl").

%% @doc translate a string or trans record into another language
%% @spec trans(From, Language) -> String
%%   From = #trans{} | String
%%   Language = atom()
trans(Text, #context{} = Context) ->
    trans(Text, Context#context.language);
trans({trans, Trans}, Language) ->
	case proplists:get_value(Language, Trans) of
		undefined -> 
			% @todo Do the lookup in the gettext tables of the english string, for now 
			% we return just the english original.
			proplists:get_value(en, Trans);
		String -> 
			String
	end;
trans(String, Language) ->
	trans({trans, [{en,String}]}, Language).

%% @doc Return the configured default language for this server
default_language(Context) ->
    zp_convert:to_atom(m_config:get_value(i18n, language, en, Context)).


%% @doc check if the two letter code is a valid language
%% @spec is_language(LanguageString) -> bool()
%%   LanguageString = string()
is_language(LanguageString) ->
	Language = iso639:lc2lang(LanguageString),
	Language /= "".
	

%% @doc Translate a language to an atom, fail when unknown language
%% @spec lc2(LanguageString) -> Language
%%  LanguageString = string()
%%  Language = atom()
lc2(LanguageString) ->
	true = is_language(iso639:lc2lang(LanguageString)),
	list_to_atom(LanguageString).


%% @doc Return a descriptive (english) string for the language
%% @spec lc2descr(Language) -> Descr
%%  Language = atom()
%%  Descr = list()
lc2descr(Language) ->
	iso639:lc2lang(atom_to_list(Language)).
